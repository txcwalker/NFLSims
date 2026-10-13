"""Tournament-pool evaluation (2026-10-08): grade EVERY lineup the Optimizer
generated for a week -- played or not -- against the week's real results.

Why: Cam iterates constantly (inputs, objective, settings) and cannot hand-
evaluate each build, so every Optimize run is treated as a paper trade. Once a
week settles we ask, over the whole generated pool: did the predicted ITM / top-
1% / EV numbers match what happened, were we generating the winning lineup (or
close) and just not playing it, which input versions did better, how much of
the pool was the same lineup regenerated, and were projections / ownership
right. It tests the math, process, optimizer and inputs at volume with no
entries submitted.

Data flow
    builds   -- data/optimizer/{year}/week_NN/builds/*.json via optimizer_store
                (pruning is OFF so throwaway autosaves survive, see
                optimizer_store.PRUNE_ENABLED)
    field    -- the week's DEFAULT grading contest's standings CSV in
                data/dfs_ownership/{year}/week_NN/main_slate/ : DraftKings $1
                "First Down" (firstdown_1_20max, ~119K entries, $100K pool,
                $5K to 1st). Every real entry is RESCORED with the latest per-
                player actuals (cash_lineup_eval.load_dk_actuals) rather than
                trusting the CSV's Points column, which is frozen at download
                time (week 1's file was grabbed mid-slate).
    payouts  -- that contest's saved real table (payouts.json) when archived
                (week 4 so far). Earlier weeks borrow the nearest saved table
                with ranks AND dollars scaled by field size (DK's pool tracks
                entries); `payout_source` says which was used.

Calibration is judged against the BUILD'S OWN contest settings where it can be:
a build's predicted ITM% refers to its own paying-positions / field-size, so
"realized ITM" for it is "finished inside that same top fraction of the real
field". Dollar results (payout, ROI) always use the default contest's table.

Identity: a lineup is its sorted set of player keys (slot-agnostic, so FLEX
reshuffles collapse). `n_generated` counts every time any build produced it;
grading is per UNIQUE lineup so a lineup regenerated 40 times is one trade, not
40, with the repeat count reported separately.

Stale lineups (Cam, 2026-10-09): a build made Tuesday can be wrecked by a Friday-
night scratch, and a scratch also re-prices OTHER players (Smith out -> Wicks
becomes the value). A lineup is "stale" if it holds a player who was
unavailable by lock, defined source-agnostically so no injury feed is needed:
  * "ruled out"       -- the week's override sheet has him dfs_status=out, or
  * "field abandoned" -- the real field rostered him <1% although our pool's
                         ownership model expected >=5%, AND he did not play
                         (no score / <=2 pts): news the pool never saw. A
                         dropped player who then scored is an ownership-model
                         miss, not a scratch, and stays valid.
Headline roll-ups, calibration and the page use VALID lineups only; stale ones
are reported separately (how many, which players, and a by-build-age table
showing how fast early builds decay). `exposure_gaps` lists the other half of
the Wicks effect: players the field flocked to that the pool barely held.
Not modeled: re-optimizing a stale lineup with final information (a repair
pass); the hindsight-optimal "ceiling" lineup.
"""
import datetime
import json
import os
from collections import Counter, defaultdict
from functools import lru_cache

import numpy as np
import pandas as pd

from src.api import optimizer_store, week_freeze
from src.evaluation.cash_lineup_eval import load_dk_actuals, load_fallback_actuals, main_slate_dir
from src.evaluation.line_history import BASE_DIR
from src.scrapers.dk_scraper import normalize_player_name
from scripts.dfs_ownership.standings_parser import CLASSIC_SLOT_RE, player_key, resolve_lineup_name

# The default grading field (Cam, 2026-10-08): DK $1 First Down. These numbers
# are only the fallback description; the real table comes from payouts.json.
DEFAULT_CONTEST = {"stem": "firstdown_1_20max", "label": "$1 First Down", "entry_fee": 1.0, "prize_pool": 100000.0}
TOP1_PCT, TOP01_PCT = 1.0, 0.1
FIELD_ABANDON_OWN = 1.0     # field ownership (%) below which a player counts as abandoned ...
POOL_EXPECTED_OWN = 5.0     # ... if our pool's ownership model expected at least this (%)
PLAYED_PTS = 2.0            # a "field abandoned" player who scored more than this actually played
AGE_BUCKETS = [(0, 6, "<6h before lock"), (6, 24, "6-24h"), (24, 72, "1-3 days"), (72, 1e9, "3+ days")]


# --------------------------------------------------------------------------
# Field (the real contest) -- parse, rescore, payouts
# --------------------------------------------------------------------------
@lru_cache(maxsize=8192)
def _resolve(raw):
    """Lineup-string token -> player key, memoized (a slate has only a few
    hundred distinct names but the field has ~1M tokens)."""
    return resolve_lineup_name(raw.strip(), normalize_player_name)


def _comp_key(lineup_str):
    """Inputs: one standings 'Lineup' string. Output: tuple of its sorted player
    keys (slot-agnostic composition identity), () if it doesn't parse."""
    return tuple(sorted(_resolve(raw) for _slot, raw in CLASSIC_SLOT_RE.findall(lineup_str)))


def load_field(year, week, stem=DEFAULT_CONTEST["stem"], base_dir=BASE_DIR):
    """Parse the default contest's standings CSV.

    Output: dict or None (no CSV) -- {"path", "n_rows", "comps": Counter of
    composition-key -> number of entries, "csv_points": {comp: Points of one
    such entry} (only used as a sanity check against our rescore)}.
    """
    path = os.path.join(main_slate_dir(year, week, base_dir), f"{stem}.csv")
    if not os.path.exists(path):
        return None
    df = pd.read_csv(path, dtype=str, keep_default_na=False, encoding="utf-8-sig",
                     usecols=lambda c: c.strip().lstrip("﻿").lower() in ("points", "lineup"))
    cols = {c.strip().lstrip("﻿").lower(): c for c in df.columns}
    df = df[df[cols["lineup"]].str.strip() != ""]
    comps, csv_points = Counter(), {}
    for lu, pts in zip(df[cols["lineup"]], df[cols["points"]]):
        k = _comp_key(lu)
        if len(k) != 9:
            continue
        comps[k] += 1
        if k not in csv_points:
            try:
                csv_points[k] = float(pts)
            except ValueError:
                pass
    return {"path": path, "n_rows": int(sum(comps.values())), "comps": comps, "csv_points": csv_points}


def rescore_field(field, actuals, fallback):
    """Re-score every distinct field composition with the latest actuals.

    Inputs: field (load_field), actuals / fallback ({player_key: DK pts}).
    Output: dict -- "scores" ascending np.ndarray of distinct comp scores,
      "cum" cumulative entry counts aligned to it, "total" entries,
      "n_missing_rows" entries containing a player with no real score (scored
      0 for them), "max_csv_diff" largest |rescore - CSV Points| over comps
      with no missing player (a freshness / parsing sanity check).
    """
    comp_scores, counts, missing_rows, diffs = [], [], 0, []
    for comp, n in field["comps"].items():
        s, miss = 0.0, False
        for k in comp:
            v = actuals.get(k, fallback.get(k))
            if v is None:
                miss = True
            else:
                s += v
        if miss:
            missing_rows += n
        elif comp in field["csv_points"]:
            diffs.append(abs(s - field["csv_points"][comp]))
        comp_scores.append(round(s, 2))
        counts.append(n)
    scores, counts = np.array(comp_scores), np.array(counts)
    order = np.argsort(scores)
    return {"scores": scores[order], "cum": np.cumsum(counts[order]), "total": int(counts.sum()),
            "n_missing_rows": int(missing_rows), "max_csv_diff": round(float(max(diffs)), 2) if diffs else None}


def n_field_above(rescored, score):
    """Number of real entries scoring strictly more than `score`."""
    idx = int(np.searchsorted(rescored["scores"], round(score, 2), side="right"))
    n_le = int(rescored["cum"][idx - 1]) if idx > 0 else 0
    return rescored["total"] - n_le


def field_ownership(field):
    """{player_key: % of field entries rostering him} from the parsed lineups
    (exact; not the CSV summary block, which splits a player by slot)."""
    cnt = Counter()
    for comp, n in field["comps"].items():
        for k in comp:
            cnt[k] += n
    total = sum(field["comps"].values())
    return {k: 100.0 * v / total for k, v in cnt.items()}


def default_payout_tiers(year, week, field_size, base_dir=BASE_DIR):
    """The default contest's payout table for a week.

    Output: (tiers [{rank_start, rank_end, payout}], source str). Uses the
    week's own payouts.json entry when archived; otherwise the nearest week's
    saved table with ranks and dollars both scaled by field_size / that
    week's max_entries ("scaled from week N"). ([], "none") if no table
    exists anywhere.
    """
    stem = DEFAULT_CONTEST["stem"]
    weeks = sorted(
        (int(d.split("_")[1]) for d in os.listdir(os.path.join(base_dir, "data", "dfs_ownership", str(year)))
         if d.startswith("week_")), key=lambda w: abs(w - int(week)))
    for w in weeks:
        path = os.path.join(main_slate_dir(year, w, base_dir), "payouts.json")
        if not os.path.exists(path):
            continue
        with open(path, encoding="utf-8") as f:
            cands = [c for c in (json.load(f) or {}).get("contests", []) if c.get("stem") == stem]
        if not cands:
            continue
        c = min(cands, key=lambda c: abs(int(c.get("max_entries") or 0) - field_size))
        if w == int(week):
            return c["tiers"], f"saved week {w}"
        f_ = field_size / float(c["max_entries"])
        tiers = [{"rank_start": max(1, int(round(t["rank_start"] * f_))),
                  "rank_end": max(1, int(round(t["rank_end"] * f_))),
                  "payout": round(t["payout"] * f_, 2)} for t in c["tiers"]]
        return tiers, f"scaled from week {w} (x{f_:.2f})"
    return [], "none"


def payout_for_rank(tiers, rank):
    """Dollar payout at a finishing rank (0.0 outside every tier)."""
    for t in tiers:
        if t["rank_start"] <= rank <= t["rank_end"]:
            return float(t["payout"])
    return 0.0


# --------------------------------------------------------------------------
# Builds -> unique lineups
# --------------------------------------------------------------------------
def _lineup_key(players):
    return tuple(sorted(player_key(p["name"], p["team"], p["pos"], normalize_player_name) for p in players))


def _paying_frac(settings):
    """Fraction of the build's own contest that cashes (paying / field). None if
    the build recorded neither."""
    c = (settings or {}).get("contest") or {}
    paying = c.get("paying_positions") or (settings or {}).get("payingPositions")
    size = c.get("field_size") or (settings or {}).get("contestSize")
    return float(paying) / float(size) if paying and size else None


def _iso_to_ts(iso):
    """'2026-10-07T17:43:39Z' -> unix seconds (None if unparseable)."""
    try:
        return datetime.datetime.strptime(iso, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc).timestamp()
    except (TypeError, ValueError):
        return None


def collect_pool(season, week, lock_ts=None):
    """Read every saved build for a week and fold lineups into unique ones.
    Builds created AFTER `lock_ts` (unix s, the main slate's first kickoff) are
    skipped and returned separately: they were made with results possibly in
    view and would flatter the grades.

    Output: (pool, builds, player_inputs, excluded)
      pool          -- {lineup_key: record} with the lineup's players, n_generated
                       (every appearance across builds), build_ids, first/last
                       build id, inputs_hashes, and per-prediction lists
                       (itm/top1/top01/ev/projected_score/paying_frac).
      builds        -- [{build_id, created_at, label, inputs_hash, n_lineups,
                       portfolio_ev_pct, lineup_keys:[...]}] oldest first.
      player_inputs -- {player_key: {name,team,pos, projection:[...], ownership:[...]}},
                       one sample per build the player appears in.
      excluded      -- [{build_id, created_at, n_lineups}] post-lock builds skipped.
    """
    pool, builds, player_inputs, excluded = {}, [], {}, []
    summaries = sorted(optimizer_store.list_builds(season, week), key=lambda b: b.get("created_at") or "")
    for s in summaries:
        b = optimizer_store.read_build(season, week, s["build_id"])
        if not b:
            continue
        ts = _iso_to_ts(b.get("created_at"))
        if lock_ts is not None and ts is not None and ts > lock_ts:
            excluded.append({"build_id": b["build_id"], "created_at": b.get("created_at"),
                             "n_lineups": len(b.get("lineups") or [])})
            continue
        pf = _paying_frac(b.get("settings"))
        keys, seen_players = [], set()
        for lu in b.get("lineups") or []:
            players = lu.get("players") or []
            if len(players) != 9:
                continue
            key = _lineup_key(players)
            keys.append(key)
            rec = pool.get(key)
            if rec is None:
                rec = pool[key] = {
                    "players": [{k: p.get(k) for k in ("slot", "name", "team", "pos", "salary", "projection", "ownership_pct")}
                                for p in players],
                    "total_salary": lu.get("total_salary"), "over_salary_cap": bool(lu.get("over_salary_cap")),
                    "n_generated": 0, "build_ids": [], "inputs_hashes": set(),
                    "itm": [], "top1": [], "top01": [], "ev": [], "proj": [], "paying_frac": [],
                }
            rec["n_generated"] += 1
            if b["build_id"] not in rec["build_ids"]:
                rec["build_ids"].append(b["build_id"])
            if b.get("inputs_hash"):
                rec["inputs_hashes"].add(b["inputs_hash"])
            for src, dst in (("itm_pct", "itm"), ("top1_pct", "top1"), ("top01_pct", "top01"),
                             ("ev_pct", "ev"), ("projected_score", "proj")):
                if lu.get(src) is not None:
                    rec[dst].append(float(lu[src]))
            if pf is not None:
                rec["paying_frac"].append(pf)
            rec["players"] = [{k: p.get(k) for k in ("slot", "name", "team", "pos", "salary", "projection", "ownership_pct")}
                              for p in players]   # keep the latest generation's snapshot
            for p in players:
                pk = player_key(p["name"], p["team"], p["pos"], normalize_player_name)
                if pk in seen_players:
                    continue
                seen_players.add(pk)
                pi = player_inputs.setdefault(pk, {"name": p["name"], "team": p["team"], "pos": p["pos"],
                                                   "projection": [], "ownership": []})
                if p.get("projection") is not None:
                    pi["projection"].append(float(p["projection"]))
                if p.get("ownership_pct") is not None:
                    pi["ownership"].append(float(p["ownership_pct"]))
        builds.append({
            "build_id": b["build_id"], "created_at": b.get("created_at"), "label": b.get("label"),
            "inputs_hash": b.get("inputs_hash"), "n_lineups": len(keys),
            "portfolio_ev_pct": (b.get("portfolio") or {}).get("total_ev_pct"),
            "paying_frac": pf, "lineup_keys": keys,
        })
    for rec in pool.values():
        rec["first_build"], rec["last_build"] = rec["build_ids"][0], rec["build_ids"][-1]
    return pool, builds, player_inputs, excluded


# --------------------------------------------------------------------------
# Grading
# --------------------------------------------------------------------------
def _mean(xs):
    return float(np.mean(xs)) if xs else None


def _r(x, nd=2):
    return None if x is None else round(float(x), nd)


def _corr(a, b):
    if len(a) < 3 or np.std(a) == 0 or np.std(b) == 0:
        return None
    return round(float(np.corrcoef(a, b)[0, 1]), 3)


def _calibration_bins(pred, realized, n_bins=5):
    """Quantile bins of a predicted probability (percent) vs the realized hit
    rate (percent). Output: [{n, pred_mean, realized}] low -> high prediction."""
    pred, realized = np.asarray(pred, float), np.asarray(realized, float)
    if len(pred) < n_bins * 5:
        return []
    order = np.argsort(pred)
    out = []
    for chunk in np.array_split(order, n_bins):
        out.append({"n": int(len(chunk)), "pred_mean": _r(pred[chunk].mean(), 3),
                    "realized": _r(100.0 * realized[chunk].mean(), 3)})
    return out


def grade_pool(pool, rescored, tiers, actuals, fallback, field_own, entry_fee):
    """Grade each unique lineup against the rescored real field.

    Output: list of rows (one per unique lineup), each with n_generated, builds,
    predictions (means over generations), actual score, rank, percentile finish
    (rank / (field+1), %), default-contest payout, hit flags (paid / top1% /
    top0.1% and `itm_own` = inside the build's own paying fraction), real
    field dupes, and actual vs predicted ownership sums.
    """
    total = rescored["total"]
    rows = []
    for key, rec in pool.items():
        score, missing = 0.0, 0
        for k in key:
            v = actuals.get(k, fallback.get(k))
            if v is None:
                missing += 1
            else:
                score += v
        score = round(score, 2)
        rank = n_field_above(rescored, score) + 1
        pct = 100.0 * rank / (total + 1)
        payout = payout_for_rank(tiers, rank)
        own_actual = [field_own.get(k) for k in key]
        pred_own = [p.get("ownership_pct") for p in rec["players"]]
        pf = _mean(rec["paying_frac"])
        rows.append({
            "key": [list(k) for k in key],
            "players": rec["players"], "total_salary": rec["total_salary"], "over_salary_cap": rec["over_salary_cap"],
            "n_generated": rec["n_generated"], "n_builds": len(rec["build_ids"]),
            "first_build": rec["first_build"], "last_build": rec["last_build"],
            "inputs_hashes": sorted(rec["inputs_hashes"]),
            "pred_itm": _r(_mean(rec["itm"])), "pred_top1": _r(_mean(rec["top1"])), "pred_top01": _r(_mean(rec["top01"])),
            "pred_ev": _r(_mean(rec["ev"])), "pred_score": _r(_mean(rec["proj"])), "paying_frac": _r(pf, 5),
            "actual_score": score, "n_missing_players": missing,
            "rank": int(rank), "pct_finish": _r(pct, 3),
            "payout": payout, "roi_pct": _r(100.0 * (payout - entry_fee) / entry_fee),
            "paid": payout > 0, "top1": pct <= TOP1_PCT, "top01": pct <= TOP01_PCT,
            "itm_own": (pct <= 100.0 * pf) if pf else None,
            "field_dupes": int(field_dupes(rescored, key)),
            "own_actual_sum": _r(sum(o for o in own_actual if o is not None)),
            "own_pred_sum": _r(sum(pred_own)) if all(o is not None for o in pred_own) else None,
        })
    return rows


def field_dupes(rescored, key):
    """Real field entries with exactly this composition."""
    return rescored.get("_comps", {}).get(key, 0)


def _roll(rows, weights=None):
    """Roll-up of graded rows: counts, hit rates (%), ROI under the default
    payout, mean percentile. `weights` (n_generated) gives the generation-
    weighted view; None = one vote per unique lineup."""
    if not rows:
        return {}
    w = np.array([r["n_generated"] for r in rows], float) if weights == "gen" else np.ones(len(rows))
    def wmean(vals):
        return float(np.average(np.asarray(vals, float), weights=w))
    return {
        "n": int(w.sum()),
        "paid_pct": _r(100 * wmean([r["paid"] for r in rows]), 2),
        "top1_pct": _r(100 * wmean([r["top1"] for r in rows]), 2),
        "top01_pct": _r(100 * wmean([r["top01"] for r in rows]), 3),
        "mean_pct_finish": _r(wmean([r["pct_finish"] for r in rows]), 2),
        "roi_pct": _r(wmean([r["roi_pct"] for r in rows]), 1),
        "best_rank": int(min(r["rank"] for r in rows)),
        "best_score": max(r["actual_score"] for r in rows),
    }


def sheet_outs(year, week, base_dir=BASE_DIR):
    """Players the week's override sheets mark dfs_status=out.
    Output: {player_key: display name}. Empty if the week has no sheets."""
    folder = os.path.join(base_dir, "data", "overrides", str(year), f"week_{int(week):02d}")
    out = {}
    if not os.path.isdir(folder):
        return out
    for path in sorted(os.listdir(folder)):
        if not path.endswith(".csv"):
            continue
        df = pd.read_csv(os.path.join(folder, path), dtype=str, keep_default_na=False, encoding_errors="replace")
        if "dfs_status" not in df.columns:
            continue
        for r in df[df["dfs_status"].str.strip().str.lower() == "out"].itertuples(index=False):
            out[player_key(r.player_name, r.team, r.pos, normalize_player_name)] = r.player_name
    return out


def classify_stale(player_inputs, field_own, outs, actuals=None):
    """Which pool players were unavailable by lock, and why.

    Inputs: player_inputs (collect_pool), field_own ({key: real field %}),
    outs (sheet_outs), actuals ({key: real DK pts}, optional). Output:
    {player_key: "ruled out" | "field abandoned"}.

    "field abandoned" needs ALL of: pool expected >= POOL_EXPECTED_OWN %, field
    held < FIELD_ABANDON_OWN %, and (when actuals are given) he did not play
    (no score, or <= PLAYED_PTS). A player the field dropped who then scored
    (Herbert 12.6 with 0.3% field) is an ownership-model miss, not a scratch,
    so he stays valid and shows up under ownership misses instead. DSTs are
    never "abandoned" (0 DK points is a legitimate DST result).
    """
    stale = {}
    for pk, pi in player_inputs.items():
        if pk in outs:
            stale[pk] = "ruled out"
            continue
        if not pi["ownership"] or pi["pos"] == "DST":
            continue
        if _mean(pi["ownership"]) >= POOL_EXPECTED_OWN and field_own.get(pk, 0.0) < FIELD_ABANDON_OWN:
            if actuals is not None and (actuals.get(pk) or 0.0) > PLAYED_PTS:
                continue
            stale[pk] = "field abandoned"
    return stale


def _label(key, names=None):
    """Display label for a player key."""
    if key[0] == "__dst__":
        return f"{key[1]} DST"
    return (names or {}).get(key) or key[0].title()


def age_table(rows):
    """Lineup validity and results by how long before lock the lineup's LAST
    build was made. Output: [{label, n, pct_stale, + _roll of valid rows}]."""
    out = []
    for lo, hi, label in AGE_BUCKETS:
        sel = [r for r in rows if r.get("hours_before_lock") is not None and lo <= r["hours_before_lock"] < hi]
        if not sel:
            continue
        valid = [r for r in sel if r["valid"]]
        # _roll's own "n" is the valid count; the table's "n" is every lineup in the bucket
        out.append({**_roll(valid), "label": label, "n": len(sel),
                    "pct_stale": _r(100.0 * (len(sel) - len(valid)) / len(sel), 1)})
    return out


def exposure_gaps(rows, field_own, stale, actuals, fallback, names, min_field_own=3.0, top=12):
    """Pool exposure (% of unique lineups holding the player) vs real field
    ownership. Output: {"underused": [...], "overused": [...]} sorted by gap --
    "underused" is the Wicks effect: who the field flocked to that we barely held."""
    n = len(rows)
    held = Counter()
    for r in rows:
        for k in r["key"]:
            held[tuple(k)] += 1
    recs = []
    for pk in set(held) | {k for k, v in field_own.items() if v >= min_field_own}:
        exp, fo = 100.0 * held.get(pk, 0) / n, field_own.get(pk, 0.0)
        if max(exp, fo) < min_field_own:
            continue
        recs.append({"name": _label(pk, names), "pool_exposure": _r(exp, 1), "field_own": _r(fo, 1), "gap": _r(fo - exp, 1),
                     "actual": actuals.get(pk, fallback.get(pk)), "stale": stale.get(pk)})
    return {"underused": sorted(recs, key=lambda x: -x["gap"])[:top], "overused": sorted(recs, key=lambda x: x["gap"])[:top]}


def finish_hist(rows, n_bins=20):
    """Counts of percentile-finish (0-100, low = good) in equal bins."""
    return np.histogram([r["pct_finish"] for r in rows], bins=n_bins, range=(0, 100))[0].tolist() if rows else []


def build_week_eval(year, week, base_dir=BASE_DIR):
    """Full tournament-pool evaluation for one settled week.

    Output: {"gradable": False, "reason": ...} if the default contest's
    standings aren't archived or no pre-lock builds exist, else a dict with
      field, summary (valid / all / generation_weighted roll-ups, top-N counts,
      repeat stats), stale (counts + players), by_age, calibration (valid
      lineups only), ownership, projection, exposure_gaps, finish_hist,
      by_build, by_inputs_hash, top_lineups, most_regenerated, and "lineups"
      (every unique graded lineup, valid or not).
    """
    field = load_field(year, week, base_dir=base_dir)
    if field is None:
        return {"gradable": False, "reason": f"{DEFAULT_CONTEST['stem']}.csv not archived for week {week}"}
    actuals = load_dk_actuals(year, week, base_dir)
    fallback = load_fallback_actuals(year, week, base_dir)
    lock = week_freeze.lock_ts(week, year, os.path.join(base_dir, "data", "external", f"schedule_{year}.csv"))
    pool, builds, player_inputs, excluded = collect_pool(year, week, lock)
    if not pool:
        return {"gradable": False, "reason": f"no saved pre-lock builds for week {week} ({len(excluded)} post-lock excluded)"}

    rescored = rescore_field(field, actuals, fallback)
    rescored["_comps"] = field["comps"]
    tiers, payout_source = default_payout_tiers(year, week, rescored["total"], base_dir)
    own = field_ownership(field)
    fee = DEFAULT_CONTEST["entry_fee"]
    rows = grade_pool(pool, rescored, tiers, actuals, fallback, own, fee)

    # ---- stale lineups: unavailable-by-lock players ------------------------
    names = {pk: pi["name"] for pk, pi in player_inputs.items()}
    stale = classify_stale(player_inputs, own, sheet_outs(year, week, base_dir), {**fallback, **actuals})
    created = {b["build_id"]: _iso_to_ts(b["created_at"]) for b in builds}
    for r in rows:
        keys = [tuple(k) for k in r["key"]]
        r["stale_players"] = [{"name": _label(k, names), "reason": stale[k]} for k in keys if k in stale]
        r["valid"] = not r["stale_players"]
        ts = created.get(r["last_build"])
        r["hours_before_lock"] = _r((lock - ts) / 3600.0, 1) if lock and ts else None
    vrows = [r for r in rows if r["valid"]]
    by_key = {tuple(map(tuple, r["key"])): r for r in rows}
    n_gen = sum(r["n_generated"] for r in rows)
    stale_players = []
    for pk, reason in stale.items():
        n_l = sum(1 for r in rows if any(tuple(k) == pk for k in r["key"]))
        if n_l:
            stale_players.append({"name": _label(pk, names), "team": player_inputs[pk]["team"], "pos": player_inputs[pk]["pos"],
                                  "reason": reason, "n_lineups": n_l, "pool_pred_own": _r(_mean(player_inputs[pk]["ownership"]), 1),
                                  "field_own": _r(own.get(pk, 0.0), 1), "actual": actuals.get(pk, fallback.get(pk))})
    stale_players.sort(key=lambda x: -x["n_lineups"])
    stale_info = {"n_stale": len(rows) - len(vrows), "n_valid": len(vrows),
                  "pct_stale": _r(100.0 * (len(rows) - len(vrows)) / len(rows), 1), "players": stale_players,
                  "stale_roll": _roll([r for r in rows if not r["valid"]])}

    summary = {
        "valid": _roll(vrows), "all": _roll(rows), "generation_weighted": _roll(vrows, "gen"),
        "n_builds": len(builds), "n_generated": n_gen, "n_unique": len(rows),
        "mean_regen": _r(n_gen / len(rows), 2), "max_regen": max(r["n_generated"] for r in rows),
        "share_gen_from_repeats_5plus": _r(100.0 * sum(r["n_generated"] for r in rows if r["n_generated"] >= 5) / n_gen, 1),
        "n_top10": sum(r["rank"] <= 10 for r in vrows), "n_top100": sum(r["rank"] <= 100 for r in vrows),
        "n_over_cap": sum(r["over_salary_cap"] for r in rows),
        "n_lineups_missing_score": sum(r["n_missing_players"] > 0 for r in rows),
        "post_lock_builds_excluded": excluded,
    }

    # ---- calibration of the optimizer's own predictions (valid lineups) ----
    cal = {}
    for label, pred_f, hit_f in (("itm", "pred_itm", "itm_own"), ("top1", "pred_top1", "top1"), ("top01", "pred_top01", "top01")):
        sel = [r for r in vrows if r[pred_f] is not None and r[hit_f] is not None]
        if sel:
            pred = [r[pred_f] for r in sel]
            hit = [float(r[hit_f]) for r in sel]
            cal[label] = {"n": len(sel), "pred_mean": _r(np.mean(pred), 3), "realized": _r(100 * np.mean(hit), 3),
                          "bins": _calibration_bins(pred, hit)}
    sel = [r for r in vrows if r["pred_ev"] is not None]
    if sel:
        cal["ev"] = {"n": len(sel), "pred_mean_roi_pct": _r(np.mean([r["pred_ev"] for r in sel]), 1),
                     "realized_roi_pct": _r(np.mean([r["roi_pct"] for r in sel]), 1),
                     "note": "predicted EV is vs each build's own contest/payout; realized is vs the default $1 First Down table"}
    sel = [r for r in vrows if r["pred_score"] is not None]
    if sel:
        cal["score"] = {"pred_mean": _r(np.mean([r["pred_score"] for r in sel])),
                        "actual_mean": _r(np.mean([r["actual_score"] for r in sel])),
                        "corr": _corr([r["pred_score"] for r in sel], [r["actual_score"] for r in sel])}

    # ---- player-level ownership + projection accuracy (available players) --
    own_rows, proj_rows = [], []
    for pk, pi in player_inputs.items():
        if pk in stale:
            continue    # a scratched player's miss is the staleness story, not model error
        act = actuals.get(pk, fallback.get(pk))
        if pi["ownership"] and pk in own:
            own_rows.append((pi["name"], pi["team"], pi["pos"], _mean(pi["ownership"]), own[pk]))
        if pi["projection"] and act is not None:
            proj_rows.append((pi["name"], pi["team"], pi["pos"], _mean(pi["projection"]), act))
    ownership = {"n_players_in_pool": len(player_inputs), "n_with_prediction": len(own_rows), "excludes_stale_players": True}
    if own_rows:
        p, a = np.array([r[3] for r in own_rows]), np.array([r[4] for r in own_rows])
        ownership.update({"bias_pred_minus_actual": _r(np.mean(p - a), 2), "mae": _r(np.mean(np.abs(p - a)), 2),
                          "corr": _corr(p, a),
                          "biggest_misses": [{"name": r[0], "team": r[1], "pos": r[2], "pred": _r(r[3], 1), "actual": _r(r[4], 1)}
                                             for r in sorted(own_rows, key=lambda r: -abs(r[3] - r[4]))[:10]]})
    sel = [r for r in vrows if r["own_pred_sum"] is not None]
    if sel:
        ownership["lineup_own_sum"] = {"n": len(sel), "pred_mean": _r(np.mean([r["own_pred_sum"] for r in sel])),
                                       "actual_mean": _r(np.mean([r["own_actual_sum"] for r in sel]))}
    if vrows:
        ownership["field_dupes"] = {
            "mean": _r(np.mean([r["field_dupes"] for r in vrows]), 2),
            "pct_lineups_with_any_dupe": _r(100 * np.mean([r["field_dupes"] > 0 for r in vrows]), 1),
            "corr_with_actual_own_sum": _corr([r["field_dupes"] for r in vrows], [r["own_actual_sum"] for r in vrows])}
    projection = {"n": len(proj_rows)}
    if proj_rows:
        p, a = np.array([r[3] for r in proj_rows]), np.array([r[4] for r in proj_rows])
        projection.update({"bias_pred_minus_actual": _r(np.mean(p - a), 2), "mae": _r(np.mean(np.abs(p - a)), 2), "corr": _corr(p, a)})

    # ---- per build / per inputs-hash roll-ups ----------------------------
    by_build = []
    for b in builds:
        brows = [by_key[k] for k in b["lineup_keys"] if k in by_key]
        if brows:
            ts = created.get(b["build_id"])
            valid = [r for r in brows if r["valid"]]
            by_build.append({"build_id": b["build_id"], "created_at": b["created_at"], "label": b["label"],
                             "inputs_hash": b["inputs_hash"], "n_lineups": b["n_lineups"],
                             "hours_before_lock": _r((lock - ts) / 3600.0, 1) if lock and ts else None,
                             "pct_stale": _r(100.0 * (len(brows) - len(valid)) / len(brows), 1),
                             "portfolio_ev_pct": b["portfolio_ev_pct"], **_roll(valid)})
    hash_rows = defaultdict(list)
    for b in builds:
        hash_rows[b["inputs_hash"]].extend(by_key[k] for k in b["lineup_keys"] if k in by_key and by_key[k]["valid"])
    by_hash = [{"inputs_hash": h, "n_builds": sum(1 for b in builds if b["inputs_hash"] == h), **_roll(rs)}
               for h, rs in hash_rows.items() if rs]

    return {
        "gradable": True, "year": year, "week": week,
        "field": {"contest": DEFAULT_CONTEST["label"], "size": rescored["total"], "winner_score": float(rescored["scores"][-1]),
                  "payout_source": payout_source, "first_prize": payout_for_rank(tiers, 1),
                  "n_missing_rows": rescored["n_missing_rows"], "max_rescore_vs_csv_diff": rescored["max_csv_diff"]},
        "summary": summary, "stale": stale_info, "by_age": age_table(rows), "calibration": cal,
        "ownership": ownership, "projection": projection,
        "exposure_gaps": exposure_gaps(rows, own, stale, actuals, fallback, names),
        "finish_hist": finish_hist(vrows),
        "by_build": by_build, "by_inputs_hash": by_hash,
        "top_lineups": sorted(vrows, key=lambda r: r["rank"])[:100],
        "most_regenerated": sorted(rows, key=lambda r: -r["n_generated"])[:20],
        "lineups": rows,
    }


def output_path(year, week, base_dir=BASE_DIR):
    return os.path.join(base_dir, "data", "eval", str(year), "tournament_pool", f"week_{int(week):02d}.json")


def _json_default(o):
    return sorted(o) if isinstance(o, set) else str(o)


def write_week_eval(year, week, base_dir=BASE_DIR):
    """Run build_week_eval and persist it (regenerable output, not committed).
    Output: the result dict."""
    res = build_week_eval(year, week, base_dir)
    if res.get("gradable"):
        p = output_path(year, week, base_dir)
        os.makedirs(os.path.dirname(p), exist_ok=True)
        with open(p, "w", encoding="utf-8") as f:
            json.dump(res, f, default=_json_default)
    return res


def available_weeks(year, base_dir=BASE_DIR):
    """Weeks with at least one saved build. Output: sorted list[int]."""
    root = os.path.join(base_dir, "data", "optimizer", str(year))
    if not os.path.isdir(root):
        return []
    out = []
    for d in os.listdir(root):
        b = os.path.join(root, d, "builds")
        if d.startswith("week_") and os.path.isdir(b) and any(f.endswith(".json") for f in os.listdir(b)):
            out.append(int(d.split("_")[1]))
    return sorted(out)


def input_token(year, week, base_dir=BASE_DIR):
    """Cache signature: mtimes of everything the evaluation reads (build files,
    the week's archived standings, override sheets, schedule)."""
    paths = []
    for sub in (("data", "optimizer", str(year), f"week_{int(week):02d}", "builds"),
                ("data", "dfs_ownership", str(year), f"week_{int(week):02d}", "main_slate"),
                ("data", "overrides", str(year), f"week_{int(week):02d}")):
        d = os.path.join(base_dir, *sub)
        if os.path.isdir(d):
            paths += [os.path.join(d, f) for f in os.listdir(d)]
    paths.append(os.path.join(base_dir, "data", "external", f"schedule_{year}.csv"))
    return tuple(sorted((p, os.path.getmtime(p)) for p in paths if os.path.exists(p)))


_CACHE = {}


def web_payload(year, week, base_dir=BASE_DIR):
    """What the page needs: build_week_eval without the full `lineups` list
    (kept on disk via write_week_eval), cached on input_token. The ~20-40s
    compute only reruns when a build / standings / sheet file changes."""
    token = input_token(year, week, base_dir)
    hit = _CACHE.get((year, week, base_dir))
    if hit and hit[0] == token:
        return hit[1]
    res = write_week_eval(year, week, base_dir)
    payload = {k: v for k, v in res.items() if k != "lineups"}
    payload["week"], payload["available_weeks"] = week, available_weeks(year, base_dir)
    for key in ("top_lineups", "most_regenerated"):
        for r in payload.get(key, []):
            r.pop("key", None)
    payload = json.loads(json.dumps(payload, default=_json_default))
    _CACHE[(year, week, base_dir)] = (token, payload)
    return payload
