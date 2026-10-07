"""Backtest of the 5-input window blend (src/data_pipeline/window_blend_v_0_1_0.py)
against the current blend, on real 2024 and 2025 play-by-play.

Inputs : data/interim/backtest_pbp_{year-1,year}.parquet (scripts/evaluation/cache_backtest_pbp.py).
Outputs: docs/eda_outputs/window_blend_backtest/{results.md, flat_field_scores.csv,
         zone_scores.csv} -- prediction ERROR OF THE INPUTS, not DFS points.
Purpose: pick a weighting scheme / fill rule / min-volume rule without running sims.

Method: for target week t = 5..18 and every player-field with a real value in
week t, predict it from weeks < t only and score |pred - actual|, weighted by
the real volume behind the actual (ratio stats) or equally (shares, for players
with a real role). "Preseason" = the player's own prior-season value (proxy for
the real preseason inputs). Rows are scored on the COMMON set of rows every
method can predict, so methods are compared like for like.

Methods: preseason, season_only, current_l4 (what the engine does from game 5:
plain mean of the last 4 played games), steady_2_3 (2/3 L4 + 1/3 season, the
zone/team path), {flat, ramp_a, ramp_b} x {mean, median} fill with min-volume,
flat_mean_nomin (control: no minimum), flat_pre50 (flat blended 50/50 with
preseason, "does anchoring help flat fields too?").
Usage: venv\\Scripts\\python.exe scripts/evaluation/backtest_window_blend_v_0_1_0.py
"""
import os
import sys
from collections import defaultdict

import numpy as np
import pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
sys.path.insert(0, ROOT)
from src.data_pipeline import window_blend_v_0_1_0 as wb  # noqa: E402
from src.data_pipeline.rolling_stats_v_0_1_0 import (  # noqa: E402
    build_player_game_log, build_team_game_log, build_team_week_totals, classify_zone,
)

OUT_DIR = os.path.join(ROOT, "docs", "eda_outputs", "window_blend_backtest")
FIRST_TARGET_WEEK = 5
MIN_ROLE = 8          # shares scored only for players with >= this many prior targets/carries
BOOT = 300
rng = np.random.default_rng(2026)


def load(year):
    return pd.read_parquet(os.path.join(ROOT, "data", "interim", f"backtest_pbp_{year}.parquet"))


# --------------------------------------------------------------------------- entities
def player_entities(pbp):
    """Inputs: one season's pass/run PBP. Output: list of dicts, one per
    (player, field): log/volume {week: x}, team_weeks, active_weeks, is_share."""
    team_weeks = defaultdict(set)
    for col in ("posteam", "defteam"):
        for team, wk in pbp[[col, "week"]].drop_duplicates().itertuples(index=False):
            team_weeks[team].add(int(wk))
    team_totals = build_team_week_totals(pbp)

    ids = set()
    for col in ("receiver_player_id", "rusher_player_id", "passer_player_id"):
        ids |= set(pbp[col].dropna().unique())
    att = pbp[pbp["play_type"] == "pass"].groupby("passer_player_id").size()
    active_by = defaultdict(set)
    team_of = {}
    for col in ("receiver_player_id", "rusher_player_id", "passer_player_id"):
        g = pbp.dropna(subset=[col])[[col, "week", "posteam"]].drop_duplicates()
        for pid, wk, tm in g.itertuples(index=False):
            active_by[pid].add(int(wk))
    for col in ("receiver_player_id", "rusher_player_id", "passer_player_id"):
        for pid, tm in pbp.dropna(subset=[col]).groupby(col)["posteam"].agg(lambda s: s.mode().iat[0]).items():
            team_of.setdefault(pid, tm)

    ents = []
    for pid in ids:
        pos = "QB" if att.get(pid, 0) >= 15 else "WR"
        log, vol = build_player_game_log(pbp, pid, pos, team_totals=team_totals)
        tw = team_weeks[team_of[pid]]
        for f, lg in log.items():
            ents.append(dict(pid=pid, field=f, log={int(k): float(v) for k, v in lg.items() if v == v and v is not None},
                             vol={int(k): float(v) for k, v in vol.get(f, {}).items()},
                             team_weeks=tw, active=active_by[pid], is_share=f in wb.SHARE_FIELDS))
    return ents


def team_entities(pbp):
    """Team defense/offense fields with pass-dropback volume per week."""
    ents = []
    pp = pbp[pbp["play_type"] == "pass"]
    for team in sorted(set(pbp["posteam"].dropna())):
        log = build_team_game_log(pbp, team)
        tw = set(pbp[pbp["posteam"] == team]["week"].astype(int)) | set(pbp[pbp["defteam"] == team]["week"].astype(int))
        off_db = pp[pp["posteam"] == team].groupby("week").size().to_dict()
        def_db = pp[pp["defteam"] == team].groupby("week").size().to_dict()
        for f, lg in log.items():
            vol = off_db if f == "sack_rate_allowed" else def_db
            ents.append(dict(pid=team, field=f, log={int(k): float(v) for k, v in lg.items() if v == v},
                             vol={int(k): float(v) for k, v in vol.items()},
                             team_weeks=tw, active=tw, is_share=False))
    return ents


def preseason_map(prev_ents):
    """(pid, field) -> prior-season value via the same qualifying-game logic."""
    out = {}
    for e in prev_ents:
        mv = wb.MIN_VOLUME.get(e["field"], 0)
        v = wb.season_value(e["log"], e["vol"], e["team_weeks"], e["active"], 99, e["is_share"], mv)
        if v is not None:
            out[(e["pid"], e["field"])] = v
    return out


# --------------------------------------------------------------------------- methods
def mean_window(log, through, n):
    ws = sorted(w for w in log if w <= through)[-n:]
    return float(np.mean([log[w] for w in ws])) if ws else None


def methods_for(e, through, pre):
    """Returns {method: prediction or None} for one entity at `through`."""
    f, mv = e["field"], wb.MIN_VOLUME.get(e["field"], 0)
    kw = dict(game_values=e["log"], volume=e["vol"], team_weeks=e["team_weeks"], active_weeks=e["active"],
              through_week=through, preseason=pre, is_share=e["is_share"])
    out = {"preseason": pre}
    season = wb.season_value(e["log"], e["vol"], e["team_weeks"], e["active"], through, e["is_share"], mv)
    out["season_only"] = season if season is not None else pre
    l4 = mean_window(e["log"], through, 4)
    sn_plain = mean_window(e["log"], through, 99)
    out["current_l4"] = l4 if l4 is not None else pre
    out["steady_2_3"] = (2 / 3 * l4 + 1 / 3 * sn_plain) if l4 is not None else pre
    for s in ("flat", "ramp_a", "ramp_b"):
        for fill in ("mean", "median"):
            out[f"{s}_{fill}"] = wb.window_blend(scheme=s, min_volume=mv, fill=fill, **kw)
    out["flat_mean_nomin"] = wb.window_blend(scheme="flat", min_volume=0, fill="mean", **kw)
    fm = out["flat_mean"]
    out["flat_pre50"] = None if fm is None or pre is None else 0.5 * fm + 0.5 * pre
    return out


def score_entities(ents, pre_map, label):
    """Returns DataFrame rows: label, pid, field, week, actual, weight, <method preds>."""
    rows = []
    for e in ents:
        pre = pre_map.get((e["pid"], e["field"]))
        if pre is None:
            continue  # no preseason proxy (rookie/new) -> can't compare like-for-like
        for t in range(FIRST_TARGET_WEEK, 19):
            if t not in e["team_weeks"]:
                continue
            if e["is_share"]:
                if t not in e["active"]:
                    continue
                prior = sum(e["vol"].get(w, 0) for w in e["vol"] if w < t)
                if prior < MIN_ROLE:
                    continue
                actual, weight = e["log"].get(t, 0.0), 1.0
            else:
                if t not in e["log"] or e["vol"].get(t, 0) <= 0:
                    continue
                actual, weight = e["log"][t], e["vol"][t]
            preds = methods_for(e, t - 1, pre)
            if any(v is None for v in preds.values()):
                continue
            rows.append(dict(label=label, pid=e["pid"], field=e["field"], week=t, actual=actual, weight=weight, **preds))
    return pd.DataFrame(rows)


# --------------------------------------------------------------------------- reporting
def md_table(df, floatfmt=".4f"):
    """Minimal DataFrame -> GitHub markdown table (avoids a tabulate dependency)."""
    cols = list(df.columns)
    fmt = lambda v: format(v, floatfmt) if isinstance(v, float) else str(v)
    lines = ["| " + " | ".join(cols) + " |", "|" + "|".join("---" for _ in cols) + "|"]
    lines += ["| " + " | ".join(fmt(v) for v in row) + " |" for row in df.itertuples(index=False)]
    return chr(10).join(lines)


METHODS = ["preseason", "season_only", "current_l4", "steady_2_3", "flat_mean", "flat_median", "ramp_a_mean",
           "ramp_a_median", "ramp_b_mean", "ramp_b_median", "flat_mean_nomin", "flat_pre50"]


def wmae(df, m):
    return float((df["weight"] * (df[m] - df["actual"]).abs()).sum() / df["weight"].sum())


def field_table(df):
    recs = []
    for f, g in df.groupby("field"):
        rec = {"field": f, "n": len(g)}
        rec.update({m: wmae(g, m) for m in METHODS})
        recs.append(rec)
    return pd.DataFrame(recs)


def aggregate_vs_current(df, boot=BOOT):
    """Per method: mean over fields of (wMAE_method / wMAE_current_l4 - 1), with a
    player-cluster bootstrap 95% interval. Negative = better than current."""
    fields = sorted(df["field"].unique())
    cur = {f: wmae(df[df["field"] == f], "current_l4") for f in fields}
    pids = df["pid"].unique()
    # per (player, field) sums for fast resampling
    parts = {}
    for (pid, f), g in df.groupby(["pid", "field"]):
        w = g["weight"].to_numpy()
        parts[(pid, f)] = {m: float((w * np.abs(g[m].to_numpy() - g["actual"].to_numpy())).sum()) for m in METHODS}
        parts[(pid, f)]["w"] = float(w.sum())
    by_pid = defaultdict(list)
    for (pid, f), p in parts.items():
        by_pid[pid].append((f, p))

    def agg(pid_list):
        tot = {f: {m: 0.0 for m in METHODS + ["w"]} for f in fields}
        for pid in pid_list:
            for f, p in by_pid[pid]:
                for k, v in p.items():
                    tot[f][k] += v
        res = {}
        for m in METHODS:
            rel = [(tot[f][m] / tot[f]["w"]) / cur[f] - 1 for f in fields if tot[f]["w"] > 0]
            res[m] = float(np.mean(rel))
        return res

    point = agg(list(pids))
    draws = [agg(rng.choice(pids, size=len(pids), replace=True)) for _ in range(boot)]
    out = []
    for m in METHODS:
        d = np.array([x[m] for x in draws])
        out.append(dict(method=m, rel_vs_current=point[m], lo=float(np.percentile(d, 2.5)), hi=float(np.percentile(d, 97.5))))
    return pd.DataFrame(out)


# --------------------------------------------------------------------------- zones
def zone_scores(pbp_prev, pbp):
    """Red-zone / goal-line target & carry share: preseason-only vs pooled last-8
    (no anchor) vs anchored blends. Truth = zone share in week t, weighted by
    team zone plays that week."""
    def counts(p):
        d = p.copy()
        d["zone"] = d["yardline_100"].apply(classify_zone)
        d = d[d["zone"].isin(["redzone", "goalline"])]
        out = {}
        for kind, idcol, ptype in (("target_share", "receiver_player_id", "pass"), ("carry_share", "rusher_player_id", "run")):
            x = d[d["play_type"] == ptype]
            team = x.groupby(["posteam", "week", "zone"]).size()
            pl = x.dropna(subset=[idcol]).groupby([idcol, "posteam", "week", "zone"]).size()
            out[kind] = (team, pl)
        return out
    prev, cur = counts(pbp_prev), counts(pbp)
    active = defaultdict(set)  # pid -> weeks he appeared in ANY play (rusher/receiver/passer)
    for col in ("receiver_player_id", "rusher_player_id", "passer_player_id"):
        for pid, wk in pbp.dropna(subset=[col])[[col, "week"]].drop_duplicates().itertuples(index=False):
            active[pid].add(int(wk))
    tw = defaultdict(set)
    for team, wk in pbp[["posteam", "week"]].drop_duplicates().itertuples(index=False):
        tw[team].add(int(wk))
    rows = []
    for kind in ("target_share", "carry_share"):
        team_p, pl_p = prev[kind]
        team_c, pl_c = cur[kind]
        for zone in ("redzone", "goalline"):
            pre_num = defaultdict(float)
            pre_den = defaultdict(float)
            for (pid, tm, wk, z), n in pl_p.items():
                if z == zone:
                    pre_num[pid] += n
            tot_team_prev = {tm: team_p[tm].xs(zone, level="zone").sum() for tm in team_p.index.get_level_values(0).unique()}
            pid_team_prev = {}
            for (pid, tm, wk, z), n in pl_p.items():
                pid_team_prev.setdefault(pid, tm)
            for (pid, tm, wk, z), n in pl_c.items():
                pass
            by_pid = defaultdict(dict)
            for (pid, tm, wk, z), n in pl_c.items():
                if z == zone:
                    by_pid[(pid, tm)][int(wk)] = n
            for (pid, tm), wkn in by_pid.items():
                if pid not in pre_num or pid not in pid_team_prev:
                    continue
                pre = pre_num[pid] / max(tot_team_prev.get(pid_team_prev[pid], 0), 1)
                for t in range(FIRST_TARGET_WEEK, 19):
                    if t not in tw[tm]:
                        continue
                    try:
                        team_t = team_c.loc[(tm, t, zone)]
                    except KeyError:
                        continue
                    actual = wkn.get(t, 0) / team_t
                    win = sorted(w for w in tw[tm] if w < t)[-8:]
                    num = sum(wkn.get(w, 0) for w in win)
                    den = 0
                    act_win = [w for w in win if w in active[pid]]
                    num_a = sum(wkn.get(w, 0) for w in act_win)
                    den_a = 0
                    for w in win:
                        try:
                            tz = team_c.loc[(tm, w, zone)]
                        except KeyError:
                            continue
                        den += tz
                        if w in active[pid]:
                            den_a += tz
                    if den == 0:
                        continue
                    pooled = num / den
                    rec = dict(kind=kind, zone=zone, pid=pid, week=t, actual=actual, weight=float(team_t), n_win=num,
                               n_active=len(act_win), active_t=int(t in active[pid]), preseason=pre, pooled8=pooled,
                               pooled_active=(num_a / den_a) if den_a > 0 else pre, n_win_active=num_a)
                    rows.append(rec)
    return pd.DataFrame(rows)


GATE_KS = (4, 5, 6, 7)
ANCHORS = (0.0, 0.15, 0.3)


def zone_variants(z):
    """Adds prediction columns to the zone rows. Gate variants (all pooled over the
    last 8 TEAM games):
      pooled8_ungated   no fallback at all
      play_gate5        old rule: preseason unless >= 5 of his zone plays in the window
      avail_K           pooled8 if active in >= K of the 8 games else preseason
      availpool_K       same gate, but pooled over only the games he was ACTIVE in
      avail6_aX         avail_6 with a fixed X share of preseason kept
    """
    z = z.copy()
    z["play_gate5"] = np.where(z["n_win"] >= 5, z["pooled8"], z["preseason"])
    for k in GATE_KS:
        z[f"avail_{k}"] = np.where(z["n_active"] >= k, z["pooled8"], z["preseason"])
        z[f"availpool_{k}"] = np.where(z["n_active"] >= k, z["pooled_active"], z["preseason"])
    for a in ANCHORS[1:]:
        z[f"avail6_a{a}"] = np.where(z["n_active"] >= 6, a * z["preseason"] + (1 - a) * z["pooled8"], z["preseason"])
    return z


ZONE_METHODS = (["preseason", "pooled8", "play_gate5"] + [f"avail_{k}" for k in GATE_KS]
                + [f"availpool_{k}" for k in GATE_KS] + [f"avail6_a{a}" for a in ANCHORS[1:]])


def zone_table(z, methods=ZONE_METHODS):
    recs = []
    for (kind, zone), g in z.groupby(["kind", "zone"]):
        r = {"kind": kind, "zone": zone, "n": len(g)}
        r.update({c: float((g["weight"] * (g[c] - g["actual"]).abs()).sum() / g["weight"].sum()) for c in methods})
        recs.append(r)
    return pd.DataFrame(recs)


def zone_aggregate(z, ref="pooled8", boot=BOOT):
    """Mean over the 4 kind x zone cells of (method wMAE / ref wMAE - 1), with a
    player-cluster bootstrap interval. Negative = better than `ref`."""
    cells = sorted({(k, zn) for k, zn in zip(z["kind"], z["zone"])})
    pids = z["pid"].unique()
    per = {}
    for (kind, zone, pid), g in z.groupby(["kind", "zone", "pid"]):
        w = g["weight"].to_numpy()
        per[(kind, zone, pid)] = {m: float((w * np.abs(g[m].to_numpy() - g["actual"].to_numpy())).sum()) for m in ZONE_METHODS}
        per[(kind, zone, pid)]["w"] = float(w.sum())
    by_pid = defaultdict(list)
    for (kind, zone, pid), p in per.items():
        by_pid[pid].append(((kind, zone), p))

    def agg(pid_list):
        tot = {c: {m: 0.0 for m in ZONE_METHODS + ["w"]} for c in cells}
        for pid in pid_list:
            for c, p in by_pid[pid]:
                for k, v in p.items():
                    tot[c][k] += v
        return {m: float(np.mean([(tot[c][m] / tot[c]["w"]) / (tot[c][ref] / tot[c]["w"]) - 1 for c in cells if tot[c]["w"] > 0]))
                for m in ZONE_METHODS}

    point = agg(list(pids))
    draws = [agg(rng.choice(pids, size=len(pids), replace=True)) for _ in range(boot)]
    return pd.DataFrame([dict(method=m, rel_vs_ref=point[m],
                              lo=float(np.percentile([d[m] for d in draws], 2.5)),
                              hi=float(np.percentile([d[m] for d in draws], 97.5))) for m in ZONE_METHODS])


def zone_main():
    """Zone-only run: red-zone/goal-line share with availability gates (Cam, 2026-10-07).
    Scored two ways: active-in-week-t rows only (fair -- an inactive player has no
    share to predict) and ALL rows (comparable to the first zone run)."""
    os.makedirs(OUT_DIR, exist_ok=True)
    zs = []
    for year in (2024, 2025):
        z = zone_scores(load(year - 1), load(year))
        z["year"] = year
        zs.append(z)
    z = zone_variants(pd.concat(zs, ignore_index=True))
    z.to_csv(os.path.join(OUT_DIR, "zone_rows_availability.csv.gz"), index=False, compression="gzip")
    md = ["# Zone-share availability-gate backtest (2024 + 2025)" + chr(10),
          "Red-zone / goal-line target_share + carry_share. Pooled over the last 8 TEAM games. `n_active` = games of those 8 he appeared in. "
          "Weighted by team zone plays in the target week. Preseason = his prior-season zone share." + chr(10)]
    for title, sub in (("ACTIVE in the target week (primary)", z[z["active_t"] == 1]), ("ALL rows (as in the first zone run)", z)):
        md.append(chr(10) + f"## {title} - rows={len(sub)}" + chr(10))
        md.append(md_table(zone_table(sub)))
        md.append(chr(10) + "### aggregate vs pooled8_ungated (mean of 4 cells; negative = better)" + chr(10))
        md.append(md_table(zone_aggregate(sub, ref="pooled8").sort_values("rel_vs_ref")))
    sub = z[(z["active_t"] == 1)]
    full = sub[sub["n_active"] == 8]
    miss = sub[sub["n_active"] < 8]
    for title, part in (("full attendance (active 8 of 8)", full), ("missed >=1 of the last 8", miss)):
        md.append(chr(10) + f"## Active in week t, {title} - rows={len(part)}" + chr(10))
        md.append(md_table(zone_table(part, ["preseason", "pooled8", "play_gate5", "avail_6", "availpool_6", "availpool_4"])))
    with open(os.path.join(OUT_DIR, "zone_availability_results.md"), "w") as f:
        f.write(chr(10).join(md))
    print("wrote zone_availability_results.md")


# --------------------------------------------------------------------------- main
def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    all_flat, zone_all, md = [], [], []
    for year in (2024, 2025):
        print(f"== {year}: loading + building logs")
        prev, cur = load(year - 1), load(year)
        prev_p, cur_p = player_entities(prev), player_entities(cur)
        prev_t, cur_t = team_entities(prev), team_entities(cur)
        d_p = score_entities(cur_p, preseason_map(prev_p), f"{year} players")
        d_t = score_entities(cur_t, preseason_map(prev_t), f"{year} teams")
        z = zone_scores(prev, cur)
        z["year"] = year
        zone_all.append(z)
        all_flat += [d_p, d_t]
        print(f"   player rows {len(d_p)}, team rows {len(d_t)}, zone rows {len(z)}")

    df = pd.concat(all_flat, ignore_index=True)
    df["year"] = df["label"].str[:4]
    df["group"] = df["label"].str[5:]
    df.to_csv(os.path.join(OUT_DIR, "flat_field_rows.csv.gz"), index=False, compression="gzip")

    md.append("# Window-blend backtest (2024 + 2025)\n")
    md.append("Error of the INPUT predictions (weighted MAE per field), target weeks 5-18, scored on rows every method can predict. "
              "Preseason proxy = same player's prior-season value. `rel` = mean over fields of (method wMAE / current_l4 wMAE - 1); "
              "negative = better than today's blend. 95% interval is a player-cluster bootstrap.\n")
    for grp in ("players", "teams"):
        for yr in ("2024", "2025", "both"):
            sub = df[(df["group"] == grp) & ((df["year"] == yr) | (yr == "both"))]
            if sub.empty:
                continue
            agg = aggregate_vs_current(sub)
            md.append(f"\n## {grp} - {yr} (rows={len(sub)})\n")
            md.append(md_table(agg.sort_values("rel_vs_current")))
            ft = field_table(sub)
            ft.to_csv(os.path.join(OUT_DIR, f"fields_{grp}_{yr}.csv"), index=False)
            if yr == "both":
                md.append(f"\n### per-field wMAE ({grp}, both years)\n")
                md.append(md_table(ft))
    z = pd.concat(zone_all, ignore_index=True)
    z.to_csv(os.path.join(OUT_DIR, "zone_rows.csv.gz"), index=False, compression="gzip")
    md.append("\n## Zone shares (red zone / goal line), weighted by team zone plays\n")
    z = zone_variants(z)
    md.append(md_table(zone_table(z)))
    with open(os.path.join(OUT_DIR, "results.md"), "w") as f:
        f.write("\n".join(md))
    print("wrote", os.path.join(OUT_DIR, "results.md"))


if __name__ == "__main__":
    zone_main() if "zone" in sys.argv[1:] else main()
