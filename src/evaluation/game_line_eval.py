"""Grade the sim's game lines (spread / total / winner) against Vegas and reality.

Roadmap: Evaluation tab, "Game Lines" section (2026-09-25). Player projections
and props are a later layer on the same page -- not in here.

Three comparisons per game, each against BOTH the opening and closing line
(see line_history.py for how open/close are resolved):
  1. Absolute accuracy   -- sim vs. actual, Vegas vs. actual (margin/total error)
  2. Relative to Vegas   -- the side the sim would take at that number, and
                            whether it won (W/L/P, units at -110)
  3. Market movement     -- open->close move relative to the sim's side (CLV):
                            positive = market moved toward us after we'd have bet
  4. Agreement tiers     -- (2026-09-25) each market classified by how far OUR
                            line is from Vegas's: agree / disagree / strong
                            (spread & total: <=1 / <=2.5 / >2.5 pts; moneyline:
                            <=3 / <=7 / >7 win-prob pts). Per tier: record,
                            units, and "who was closer" (spread/total) or
                            Brier sim vs. Vegas (moneyline).
  5. Moneyline value     -- bet the side whose sim win prob beats the price's
                            vig-inclusive break-even; paid at the real odds
                            (+210 winner = +2.1u). No edge over the vig = no bet.

Sim "line" = MEAN of the 10K sim runs (matches what the Game Explorer shows);
the side pick uses the full distribution (P(cover) vs. P(opponent covers),
pushes excluded), so key numbers are handled correctly -- a mean of -2.6 vs.
a -3 line isn't automatically "take the dog" if the sims pile up on exactly 3.

Conventions (home-perspective everywhere, same as nflverse):
  margin = home_score - away_score;  spread_line > 0 = home favored
  home covers if margin > spread_line;  over if total > total_line

Calibration tools:
  PIT (probability integral transform) -- where the actual result landed in
  the sim's own distribution (0..1). If the sims are calibrated, PITs across
  games are uniform. PITs piling up near 1 on totals = reality kept going over
  our distribution (Week 1); a U-shape = distributions too narrow.
"""
import os

import numpy as np
import pandas as pd

from src.evaluation.line_history import (BASE_DIR, load_ledger, load_overrides,
                                         resolve_open_close)

STD_ODDS = -110
BREAKEVEN_110 = 110 / 210          # 52.38% -- win rate needed at -110
LINE_KINDS = ("open", "close")

# Agreement tiers (Cam, 2026-09-25): how far OUR line is from Vegas's line,
# classified separately per market. Spread/total in points (sim mean vs. the
# Vegas number); moneyline in win-probability points (sim P(home wins) vs.
# Vegas's vig-free P(home wins)). Upper bounds are inclusive.
TIERS = ("agree", "disagree", "strong")
TIER_CUTS_PTS = (1.0, 2.5)         # <=1 agree, (1, 2.5] disagree, >2.5 strong
TIER_CUTS_ML = (0.03, 0.07)        # <=3pp agree, (3, 7]pp disagree, >7pp strong


def tier(diff, cuts):
    """Inputs: diff (sim minus Vegas, any sign; NaN ok), cuts ((agree_max, disagree_max)).
    Output: 'agree' | 'disagree' | 'strong' | None (no line)."""
    if diff is None or pd.isna(diff):
        return None
    a = abs(diff)
    return "agree" if a <= cuts[0] else "disagree" if a <= cuts[1] else "strong"


def implied_prob(ml):
    """Inputs: American odds. Output: implied win probability INCLUDING the vig
    (what the price itself says you need to win to break even), or NaN."""
    if ml is None or pd.isna(ml):
        return np.nan
    return 100.0 / (ml + 100.0) if ml > 0 else -ml / (-ml + 100.0)


# ── Small pure helpers ────────────────────────────────────────────────────────

def payout(result, odds=STD_ODDS):
    """Inputs: result ('W'|'L'|'P'|None), odds (American). Output: units won on
    a 1-unit risk (float), or NaN for an ungraded bet."""
    if result == "W":
        return 100.0 / abs(odds) if odds < 0 else odds / 100.0
    if result == "L":
        return -1.0
    if result == "P":
        return 0.0
    return np.nan


def grade_side(actual_value, line, pick_over):
    """Inputs: actual_value (actual margin or total; NaN = not played),
    line (float), pick_over (bool -- True if the pick wins when actual > line,
    i.e. 'home' on a spread or 'over' on a total).
    Output: 'W' | 'L' | 'P' | None."""
    if actual_value is None or pd.isna(actual_value) or line is None or pd.isna(line):
        return None
    if actual_value == line:
        return "P"
    return "W" if (actual_value > line) == pick_over else "L"


def pit(samples, actual_value):
    """Inputs: samples (np.ndarray of sim outcomes), actual_value (float|NaN).
    Output: mid-rank PIT in [0,1] (NaN if not played). Mid-rank = P(sim < x)
    + 0.5*P(sim == x), which keeps discrete football scores from biasing it."""
    if actual_value is None or pd.isna(actual_value):
        return np.nan
    return float((samples < actual_value).mean() + 0.5 * (samples == actual_value).mean())


def novig_home_prob(home_ml, away_ml):
    """Inputs: American moneylines. Output: vig-free P(home wins) or NaN.
    Purpose: fair market win probability to compare against the sim's."""
    if home_ml is None or away_ml is None or pd.isna(home_ml) or pd.isna(away_ml):
        return np.nan
    h, a = implied_prob(home_ml), implied_prob(away_ml)
    return h / (h + a)


def ml_value_bet(p_sim_home, home_ml, away_ml):
    """Moneyline value pick: bet the side whose sim win probability beats the
    price's own break-even (implied prob WITH vig), if either does.

    Inputs: p_sim_home (sim P(home wins), ties split), home_ml/away_ml (American).
    Output: (side: 'home'|'away'|None, edge: float) -- edge = sim prob minus the
    vig-inclusive implied prob of the chosen side (NaN if no line). If both
    sides clear the vig (rare), the bigger edge wins. No bet -> (None, best edge).
    Why vig-inclusive (Cam's call, 2026-09-25): otherwise tiny edges on big
    favorites (-400) count as "bets" and dominate the record without meaning much.
    """
    ih, ia = implied_prob(home_ml), implied_prob(away_ml)
    if pd.isna(ih) or pd.isna(ia):
        return None, np.nan
    eh, ea = p_sim_home - ih, (1.0 - p_sim_home) - ia
    side, edge = ("home", eh) if eh >= ea else ("away", ea)
    return (side if edge > 0 else None), edge


def pick_at_line(samples, line):
    """Which side the sim takes at a given line.
    Inputs: samples (sim margins or totals), line (float).
    Output: (pick_over: bool, pick_prob: float, p_push: float) -- pick_prob is
    P(pick side wins | no push). pick_over=True means home/over."""
    p_over = float((samples > line).mean())
    p_under = float((samples < line).mean())
    p_push = 1.0 - p_over - p_under
    decided = p_over + p_under
    if decided == 0:
        return True, 0.5, p_push
    pick_over = p_over >= p_under
    return pick_over, (p_over if pick_over else p_under) / decided, p_push


# ── Per-game build ────────────────────────────────────────────────────────────

def _kickoff_ts(row):
    try:
        return pd.Timestamp(f"{row['gameday']} {row['gametime']}").tz_localize("America/New_York").timestamp()
    except (KeyError, ValueError, TypeError):
        return None


def grade_game(margins, totals, sched_row, lines, sim_run_at=None):
    """Grade one game.

    Inputs:
      margins, totals -- np.ndarray, per-sim home margin and total (the 10K runs)
      sched_row       -- schedule row (game_id, week, teams, result, total, roof, div_game, gameday/gametime)
      lines           -- dict from resolve_open_close() for this game (may be empty)
      sim_run_at      -- unix s the sim was run (None = unknown)
    Output: flat dict, one row of the evaluation table (see build_eval for columns).
    """
    home, away = sched_row["home_team"], sched_row["away_team"]
    act_margin = sched_row.get("result")
    act_total = sched_row.get("total")
    played = not pd.isna(act_margin) and not pd.isna(act_total)
    ko = _kickoff_ts(sched_row)

    rec = {
        "game_id": sched_row["game_id"], "week": int(sched_row["week"]),
        "away_team": away, "home_team": home,
        "roof": sched_row.get("roof"), "div_game": int(sched_row.get("div_game") or 0),
        "kickoff_ts": ko, "sim_run_at": sim_run_at,
        # 'pregame' = provably simmed before kickoff; 'post_kickoff' = not;
        # 'unknown' = no timestamp (pre-lock sims, graded per Cam's call)
        "sim_timing": ("unknown" if sim_run_at is None or ko is None
                       else "pregame" if sim_run_at < ko else "post_kickoff"),
        "played": played,
        "n_sims": int(len(margins)),
        "sim_margin": float(margins.mean()), "sim_total": float(totals.mean()),
        "sim_margin_median": float(np.median(margins)), "sim_total_median": float(np.median(totals)),
        "sim_margin_sd": float(margins.std()), "sim_total_sd": float(totals.std()),
        "sim_home_win": float((margins > 0).mean() + 0.5 * (margins == 0).mean()),
        "actual_margin": float(act_margin) if played else np.nan,
        "actual_total": float(act_total) if played else np.nan,
        "vegas_home_win": novig_home_prob(lines.get("close_home_ml"), lines.get("close_away_ml")),
    }
    rec["pit_margin"] = pit(margins, rec["actual_margin"])
    rec["pit_total"] = pit(totals, rec["actual_total"])
    rec["sim_margin_err"] = rec["actual_margin"] - rec["sim_margin"]      # + = home did better than sim
    rec["sim_total_err"] = rec["actual_total"] - rec["sim_total"]         # + = went over the sim
    rec["home_won"] = (np.nan if not played else
                       1.0 if act_margin > 0 else 0.0 if act_margin < 0 else 0.5)

    for kind in LINE_KINDS:
        for mkt, samples, actual, sim_pt in (("spread", margins, rec["actual_margin"], rec["sim_margin"]),
                                             ("total", totals, rec["actual_total"], rec["sim_total"])):
            line = lines.get(f"{kind}_{mkt}")
            p = f"{kind}_{mkt}"
            rec[p] = line
            rec[f"{p}_src"] = lines.get(f"{kind}_{mkt}_src")
            if line is None or pd.isna(line):
                for s in ("edge", "pick", "pick_prob", "p_push", "result", "units", "line_err", "tier", "sim_closer"):
                    rec[f"{p}_{s}"] = None if s in ("pick", "result", "tier") else np.nan
                continue
            pick_over, pick_prob, p_push = pick_at_line(samples, line)
            res = grade_side(actual, line, pick_over)
            rec[f"{p}_edge"] = sim_pt - line          # + = sim higher than the line (home / over)
            rec[f"{p}_pick"] = (home if pick_over else away) if mkt == "spread" else ("over" if pick_over else "under")
            rec[f"{p}_pick_over"] = pick_over
            rec[f"{p}_pick_prob"] = pick_prob
            rec[f"{p}_p_push"] = p_push
            rec[f"{p}_result"] = res
            rec[f"{p}_units"] = payout(res)
            rec[f"{p}_line_err"] = actual - line if played else np.nan   # Vegas's own error
            rec[f"{p}_tier"] = tier(sim_pt - line, TIER_CUTS_PTS)
            # "Who was closer?" 1 = actual landed nearer our number, 0 = nearer
            # Vegas's, 0.5 = dead even. The cleanest "when we disagree, are we right?"
            if played:
                ds, dv = abs(actual - sim_pt), abs(actual - line)
                rec[f"{p}_sim_closer"] = 1.0 if ds < dv else 0.0 if ds > dv else 0.5
            else:
                rec[f"{p}_sim_closer"] = np.nan

        # Moneyline value bet at this line kind (see ml_value_bet).
        hml, aml = lines.get(f"{kind}_home_ml"), lines.get(f"{kind}_away_ml")
        p = f"{kind}_ml"
        nv = novig_home_prob(hml, aml)
        side, ml_edge = ml_value_bet(rec["sim_home_win"], hml, aml)
        rec[f"{p}_home"], rec[f"{p}_away"] = hml, aml
        rec[f"{p}_novig_home"] = nv
        rec[f"{p}_diff"] = rec["sim_home_win"] - nv if not pd.isna(nv) else np.nan   # + = we like home more than Vegas
        rec[f"{p}_tier"] = tier(rec[f"{p}_diff"], TIER_CUTS_ML)
        rec[f"{p}_edge"] = ml_edge
        rec[f"{p}_pick"] = None if side is None else (home if side == "home" else away)
        rec[f"{p}_pick_home"] = None if side is None else side == "home"
        rec[f"{p}_pick_odds"] = None if side is None else (hml if side == "home" else aml)
        rec[f"{p}_pick_prob"] = (np.nan if side is None else
                                 rec["sim_home_win"] if side == "home" else 1.0 - rec["sim_home_win"])
        rec[f"{p}_pick_dog"] = None if side is None else rec[f"{p}_pick_odds"] > 0
        if side is None or not played:
            res = None
        elif act_margin == 0:
            res = "P"                                   # tie (OT tie) -> moneyline push
        else:
            res = "W" if (act_margin > 0) == (side == "home") else "L"
        rec[f"{p}_result"] = res
        rec[f"{p}_units"] = np.nan if side is None else payout(res, rec[f"{p}_pick_odds"])
        # Brier per line kind, for the tier breakdown (sim vs. this line's market)
        rec[f"{p}_vegas_brier"] = (rec["home_won"] - nv) ** 2 if played and not pd.isna(nv) else np.nan
    rec["sim_brier"] = (rec["home_won"] - rec["sim_home_win"]) ** 2 if played else np.nan

    # Market movement vs. the side we'd have taken at the OPEN (CLV, in points).
    for mkt in ("spread", "total"):
        o, c = rec.get(f"open_{mkt}"), rec.get(f"close_{mkt}")
        if o is None or c is None or pd.isna(o) or pd.isna(c):
            rec[f"{mkt}_move"] = np.nan
            rec[f"{mkt}_clv"] = np.nan
        else:
            rec[f"{mkt}_move"] = c - o
            rec[f"{mkt}_clv"] = (c - o) * (1 if rec[f"open_{mkt}_pick_over"] else -1)
    return rec


def load_week_sims(week, base_dir=BASE_DIR):
    """Inputs: week (int). Output: (games DataFrame | None, parquet mtime | None)
    from data/interim/dfs_week_{week}_games.parquet (the latest sim run)."""
    p = os.path.join(base_dir, "data", "interim", f"dfs_week_{week}_games.parquet")
    if not os.path.exists(p):
        return None, None
    cols = ["game_id", "away_score", "home_score", "total"]
    df = pd.read_parquet(p)
    keep = cols + (["sim_run_at"] if "sim_run_at" in df.columns else [])
    return df[keep], os.path.getmtime(p)


def build_eval(year=2026, weeks=None, base_dir=BASE_DIR):
    """Build the per-game evaluation table.

    Inputs: year (int), weeks (iterable of int | None = every REG week with a
    sim file), base_dir (repo root).
    Output: DataFrame, one row per simmed game (played or not). Key columns:
      sim_margin/sim_total (mean), actual_*, sim_*_err, pit_*, sim_home_win,
      vegas_home_win, and per {open,close} x {spread,total}: the line, _src,
      _edge, _pick, _pick_prob, _result, _units, _line_err; plus spread/total
      _move and _clv (open->close move relative to our open-line side).
    """
    sched = pd.read_csv(os.path.join(base_dir, "data", "external", f"schedule_{year}.csv"))
    sched = sched[sched["game_type"] == "REG"]
    if weeks is None:
        weeks = sorted(int(w) for w in sched["week"].unique())
    kickoffs = {r["game_id"]: _kickoff_ts(r) for _, r in sched.iterrows()}
    lines = resolve_open_close(load_ledger(year, base_dir), load_overrides(year, base_dir), kickoffs)

    rows = []
    for wk in weeks:
        sims, mtime = load_week_sims(wk, base_dir)
        if sims is None:
            continue
        for gid, g in sims.groupby("game_id"):
            srow = sched[sched["game_id"] == gid]
            if srow.empty:
                continue
            margins = (g["home_score"] - g["away_score"]).to_numpy(dtype=float)
            totals = g["total"].to_numpy(dtype=float)
            # Pre-lock parquets have no sim_run_at; the file mtime is only an
            # upper bound (a later resim of ANOTHER game bumps it), so only
            # trust it when it proves "pregame".
            if "sim_run_at" in g and g["sim_run_at"].notna().any():
                run_at = float(g["sim_run_at"].max())
            else:
                ko = kickoffs.get(gid)
                run_at = mtime if (ko is not None and mtime < ko) else None
            ln = lines.loc[gid].to_dict() if gid in lines.index else {}
            rows.append(grade_game(margins, totals, srow.iloc[0], ln, run_at))
    return pd.DataFrame(rows)


# ── Aggregates for the Evaluation tab ────────────────────────────────────────

def _record(df, col_result, col_units):
    r = df[col_result].dropna()
    w, l, p = int((r == "W").sum()), int((r == "L").sum()), int((r == "P").sum())
    units = float(df[col_units].sum(skipna=True))
    return {"w": w, "l": l, "p": p, "n": w + l + p,
            "win_pct": (w / (w + l)) if (w + l) else None,
            "units": round(units, 2),
            "roi": round(units / (w + l + p), 4) if (w + l + p) else None}


def _mae(s):
    s = s.dropna()
    return round(float(s.abs().mean()), 2) if len(s) else None


def _bias(s):
    s = s.dropna()
    return round(float(s.mean()), 2) if len(s) else None


def _calibration(pred, hit, edges):
    """Bin predicted probabilities, compare to realized hit rate.
    Output: list of {lo, hi, n, mean_pred, hit_rate}."""
    out = []
    for lo, hi in zip(edges[:-1], edges[1:]):
        m = (pred >= lo) & (pred < hi) & hit.notna()
        if m.sum() == 0:
            out.append({"lo": lo, "hi": hi, "n": 0, "mean_pred": None, "hit_rate": None})
            continue
        out.append({"lo": lo, "hi": hi, "n": int(m.sum()),
                    "mean_pred": round(float(pred[m].mean()), 4),
                    "hit_rate": round(float(hit[m].mean()), 4)})
    return out


_TIER_ORDER = {t: i for i, t in enumerate(TIERS)}


def _slice_rows(frame, dim, col_result, col_units):
    """Group `frame` by `dim` and return one _record() per group, in a readable
    order (agreement tiers agree->strong, everything else alphabetical).
    Division flag is relabeled 1/0 -> 'yes'/'no'."""
    rows = [{"key": ("yes" if key == 1 else "no") if dim == "div_game" else str(key),
             **_record(g, col_result, col_units)} for key, g in frame.groupby(dim)]
    return sorted(rows, key=lambda r: (_TIER_ORDER.get(r["key"], 99), r["key"]))


def tier_breakdown(played, kind):
    """Agree / disagree / strong-disagree breakdown for one line kind.
    Inputs: played games (build_eval rows with played=True), kind ('open'|'close').
    Output: {spread|total|ml: [{tier, n, <record fields>, ...}]} where
      spread/total rows add sim_closer_pct -- share of games where the actual
        result landed nearer our number than Vegas's (the direct "when we
        disagree, who's right?" answer; ties count half);
      ml rows add sim_brier / vegas_brier over that tier's games (lower =
        better probabilities) plus the record of the value bets placed in it.
    Every tier is listed even when empty so the UI grid stays stable."""
    out = {}
    for m in ("spread", "total", "ml"):
        rows = []
        for t in TIERS:
            g = played[played[f"{kind}_{m}_tier"] == t]
            row = {"tier": t, "games": int(len(g)), **_record(g, f"{kind}_{m}_result", f"{kind}_{m}_units")}
            if m == "ml":
                row["sim_brier"] = round(float(g["sim_brier"].mean()), 4) if len(g) else None
                row["vegas_brier"] = round(float(g[f"{kind}_ml_vegas_brier"].mean()), 4) if len(g) else None
            else:
                sc = g[f"{kind}_{m}_sim_closer"].dropna()
                row["sim_closer_pct"] = round(float(sc.mean()), 4) if len(sc) else None
            rows.append(row)
        out[m] = rows
    return out


def running_by_week(df):
    """Cumulative season-to-date metrics after each week -- the "is it
    getting better or worse" line chart on the Evaluation tab.
    Inputs: build_eval() DataFrame. Output: list of dicts, one per week with
    played games: {week, n, sim_margin_mae, close_margin_mae, sim_total_mae,
    close_total_mae, sim_brier, vegas_brier, units_{open,close}_{spread,total}}
    -- all cumulative through that week."""
    played = df[df["played"]]
    out = []
    for wk in sorted(played["week"].unique()):
        g = played[played["week"] <= wk]
        hw = g["home_won"]
        vv = g["vegas_home_win"].notna()
        row = {"week": int(wk), "n": int(len(g)),
               "sim_margin_mae": _mae(g["sim_margin_err"]),
               "close_margin_mae": _mae(g["close_spread_line_err"]),
               "sim_total_mae": _mae(g["sim_total_err"]),
               "close_total_mae": _mae(g["close_total_line_err"]),
               "sim_brier": round(float(((g["sim_home_win"] - hw) ** 2).mean()), 4),
               "vegas_brier": (round(float(((g.loc[vv, "vegas_home_win"] - hw[vv]) ** 2).mean()), 4)
                               if vv.any() else None)}
        for k in LINE_KINDS:
            for m in ("spread", "total", "ml"):
                row[f"units_{k}_{m}"] = round(float(g[f"{k}_{m}_units"].sum(skipna=True)), 2)
        out.append(row)
    return out


def summarize(df):
    """Season/filtered summary of a build_eval() table (played games only for
    anything graded). Output: JSON-able dict with sections:
      accuracy     -- MAE + bias of sim vs. open vs. close, margin and total
      records      -- W/L/P, win%, units, ROI for {open,close}x{spread,total}
      clv          -- avg points of market movement toward our open-line side
      win_prob     -- Brier score, sim vs. vig-free closing moneyline
      calibration  -- pick-probability bins vs. hit rate; sim win-prob bins;
                      PIT histograms (10 bins) for margin and total
      slices       -- where we win/lose: by week, edge size, fav/dog,
                      over/under, roof, division game
    """
    played = df[df["played"]].copy()
    out = {"n_games": int(len(df)), "n_played": int(len(played)),
           "n_pregame_verified": int((played["sim_timing"] == "pregame").sum()) if len(played) else 0}
    if played.empty:
        return out

    out["accuracy"] = {
        "margin": {"sim_mae": _mae(played["sim_margin_err"]),
                   "open_mae": _mae(played["open_spread_line_err"]),
                   "close_mae": _mae(played["close_spread_line_err"]),
                   "sim_bias": _bias(played["sim_margin_err"])},
        "total": {"sim_mae": _mae(played["sim_total_err"]),
                  "open_mae": _mae(played["open_total_line_err"]),
                  "close_mae": _mae(played["close_total_line_err"]),
                  "sim_bias": _bias(played["sim_total_err"]),
                  "open_bias": _bias(played["open_total_line_err"]),
                  "close_bias": _bias(played["close_total_line_err"])},
    }
    out["records"] = {f"{k}_{m}": _record(played, f"{k}_{m}_result", f"{k}_{m}_units")
                      for k in LINE_KINDS for m in ("spread", "total", "ml")}
    out["tiers"] = {k: tier_breakdown(played, k) for k in LINE_KINDS}
    out["tier_cuts"] = {"pts": list(TIER_CUTS_PTS), "ml_pct_pts": [c * 100 for c in TIER_CUTS_ML]}
    out["breakeven_win_pct"] = round(BREAKEVEN_110, 4)
    out["clv"] = {m: {"avg_pts": _bias(df[f"{m}_clv"]),
                      "n": int(df[f"{m}_clv"].notna().sum()),
                      "moved_toward_us": int((df[f"{m}_clv"] > 0).sum()),
                      "moved_against_us": int((df[f"{m}_clv"] < 0).sum())}
                  for m in ("spread", "total")}

    hw = played["home_won"]
    brier_sim = float(((played["sim_home_win"] - hw) ** 2).mean())
    vv = played["vegas_home_win"].notna()
    out["win_prob"] = {
        "sim_brier": round(brier_sim, 4),
        "vegas_brier": round(float(((played.loc[vv, "vegas_home_win"] - hw[vv]) ** 2).mean()), 4) if vv.any() else None,
        "sim_brier_same_games": round(float(((played.loc[vv, "sim_home_win"] - hw[vv]) ** 2).mean()), 4) if vv.any() else None,
        "n_vegas": int(vv.sum()),
    }

    pick_edges = [0.5, 0.55, 0.6, 0.65, 0.7, 1.0001]
    cal = {}
    for k in LINE_KINDS:
        for m in ("spread", "total"):
            res = played[f"{k}_{m}_result"]
            hit = res.map({"W": 1.0, "L": 0.0})           # pushes excluded
            cal[f"{k}_{m}"] = _calibration(played[f"{k}_{m}_pick_prob"], hit, pick_edges)
    cal["home_win"] = _calibration(played["sim_home_win"], hw.where(hw != 0.5),
                                   [0, 0.2, 0.35, 0.5, 0.65, 0.8, 1.0001])
    for m in ("margin", "total"):
        counts, _ = np.histogram(played[f"pit_{m}"].dropna(), bins=np.linspace(0, 1, 11))
        cal[f"pit_{m}"] = [int(c) for c in counts]
    out["calibration"] = cal

    slices = {}
    by_week = []
    for wk, g in played.groupby("week"):
        by_week.append({"week": int(wk), "n": int(len(g)),
                        "sim_total_bias": _bias(g["sim_total_err"]),
                        "close_total_bias": _bias(g["close_total_line_err"]),
                        "sim_margin_mae": _mae(g["sim_margin_err"]),
                        "close_margin_mae": _mae(g["close_spread_line_err"]),
                        "overs_hit": int((g["actual_total"] > g["close_total"]).sum()),
                        **{f"{k}_{m}": _record(g, f"{k}_{m}_result", f"{k}_{m}_units")
                           for k in LINE_KINDS for m in ("spread", "total", "ml")}})
    slices["week"] = by_week
    for k in LINE_KINDS:
        sp = played[played[f"{k}_spread"].notna()].copy()
        if len(sp):
            # favorite side = home if spread_line > 0; pick_over = picked home
            sp["fav_dog"] = np.where((sp[f"{k}_spread"] > 0) == sp[f"{k}_spread_pick_over"].astype(bool),
                                     "favorite", "underdog")
            sp.loc[sp[f"{k}_spread"] == 0, "fav_dog"] = "pick'em"
            sp["home_away"] = np.where(sp[f"{k}_spread_pick_over"].astype(bool), "home", "away")
            for dim in ("fav_dog", "home_away", "roof", "div_game"):
                slices[f"{k}_spread_by_{dim}"] = _slice_rows(sp, dim, f"{k}_spread_result", f"{k}_spread_units")
        tt = played[played[f"{k}_total"].notna()].copy()
        if len(tt):
            tt["side"] = tt[f"{k}_total_pick"]
            for dim in ("side", "roof"):
                slices[f"{k}_total_by_{dim}"] = _slice_rows(tt, dim, f"{k}_total_result", f"{k}_total_units")
        # Moneyline value bets only (games where a bet cleared the vig).
        ml = played[played[f"{k}_ml_pick"].notna()].copy()
        if len(ml):
            ml["fav_dog"] = np.where(ml[f"{k}_ml_pick_dog"].astype(bool), "underdog", "favorite")
            ml["home_away"] = np.where(ml[f"{k}_ml_pick_home"].astype(bool), "home", "away")
            for dim in ("fav_dog", "home_away"):
                slices[f"{k}_ml_by_{dim}"] = _slice_rows(ml, dim, f"{k}_ml_result", f"{k}_ml_units")
    out["slices"] = slices
    return out
