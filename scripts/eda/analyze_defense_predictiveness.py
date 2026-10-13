"""How well does early-season defensive performance predict LATER defensive performance?

Purpose: pick Stage 1 inputs for the sim's team-defense component (docs/implementation_plans/
defense_components_plan.md). Answers: (1) does opponent (schedule) adjustment help? (2) do longer
windows help (weeks 1-8 -> 9-12 / 9-17)? (3) is simple points / yards allowed as good as EPA?
(4) does defense vs a specific offensive position (EPA per target to WR / TE / RB) carry signal?
(5) how much does the prior season add?

Method: for each season 2021-2025 and each (early window, late window) pair, build per-team ratings
in each window from game-level data, raw (plain mean) and opponent-adjusted (weighted ridge on
offense + defense effects per game). Correlate each early-window rating with the late-window
OPPONENT-ADJUSTED rating of the target (points allowed per game, EPA/play allowed), across the 32
teams; report the mean over seasons. 'ceiling' = Spearman-Brown reliability of the target window
itself (odd vs even weeks), i.e. roughly the best any predictor can do on that target.
Plays: regular season pass/run with epa and win probability 5-95% (garbage time out).

Usage: venv\\Scripts\\python.exe scripts/eda/analyze_defense_predictiveness.py
Outputs: docs/eda_outputs/defense/predictiveness.{json,md}
"""
import json
import os

import nfl_data_py as nfl
import numpy as np
import pandas as pd

OUT = os.path.join("docs", "eda_outputs", "defense")
SEASONS = [2021, 2022, 2023, 2024, 2025]
COLS = ["season", "season_type", "week", "game_id", "play_type", "posteam", "defteam", "home_team", "away_team",
        "home_score", "away_score", "epa", "success", "pass", "rush", "yards_gained", "complete_pass", "cpoe",
        "receiver_player_id", "wp", "sack", "qb_scramble"]
LAMBDAS = {"adj_light": 150.0, "adj_mod": 600.0}   # ridge strength, in 'plays'
WINDOWS = {"wk1-4 -> wk5-8": ((1, 4), (5, 8)), "wk1-8 -> wk9-12": ((1, 8), (9, 12)),
           "wk1-8 -> wk9-17": ((1, 8), (9, 17)), "wk1-12 -> wk13-17": ((1, 12), (13, 17))}


def load():
    p = nfl.import_pbp_data(SEASONS, columns=COLS, include_participation=False)
    p = p[(p["season_type"] == "REG") & p["play_type"].isin(["pass", "run"]) & p["epa"].notna()
          & p["wp"].between(0.05, 0.95)].copy()
    ros = nfl.import_seasonal_rosters(SEASONS)[["season", "player_id", "position"]].drop_duplicates(["season", "player_id"])
    p = p.merge(ros, left_on=["season", "receiver_player_id"], right_on=["season", "player_id"], how="left")
    p["rec_pos"] = p["position"].where(p["pass"] == 1)
    return p


def game_rows(p, kind):
    """Per (game, offense, defense) rows with a value and a weight. kind selects the metric."""
    g = p.groupby(["season", "week", "game_id", "posteam", "defteam"])
    if kind == "points":
        h = p.drop_duplicates("game_id")
        rows = pd.concat([
            h.assign(posteam=h.home_team, defteam=h.away_team, y=h.home_score.astype(float)),
            h.assign(posteam=h.away_team, defteam=h.home_team, y=h.away_score.astype(float))])
        rows["w"] = 1.0
        return rows[["season", "week", "game_id", "posteam", "defteam", "y", "w"]]
    if kind == "epa":
        r = g["epa"].agg(["mean", "size"])
    elif kind == "success":
        r = g["success"].agg(["mean", "size"])
    elif kind == "yards":
        r = g["yards_gained"].agg(["mean", "size"])
    elif kind == "pass_epa":
        r = p[p["pass"] == 1].groupby(["season", "week", "game_id", "posteam", "defteam"])["epa"].agg(["mean", "size"])
    elif kind == "rush_epa":
        r = p[p["rush"] == 1].groupby(["season", "week", "game_id", "posteam", "defteam"])["epa"].agg(["mean", "size"])
    elif kind == "cpoe":
        r = p[p["pass"] == 1].dropna(subset=["cpoe"]).groupby(["season", "week", "game_id", "posteam", "defteam"])["cpoe"].agg(["mean", "size"])
    elif kind.startswith("pos_"):
        pos = kind[4:]
        r = p[(p["pass"] == 1) & (p["rec_pos"] == pos)].groupby(["season", "week", "game_id", "posteam", "defteam"])["epa"].agg(["mean", "size"])
    else:
        raise ValueError(kind)
    r = r.reset_index().rename(columns={"mean": "y", "size": "w"})
    return r


def ratings(rows, lam=None):
    """Team defensive rating (+ = allows MORE) from game rows. lam None -> raw weighted mean allowed;
    else weighted ridge on offense + defense effects. Returns Series indexed by defending team."""
    if rows.empty:
        return pd.Series(dtype=float)
    if lam is None:
        return rows.groupby("defteam").apply(lambda t: np.average(t["y"], weights=t["w"]))
    teams = sorted(set(rows["posteam"]) | set(rows["defteam"]))
    ix = {t: i for i, t in enumerate(teams)}
    n, k = len(rows), len(teams)
    X = np.zeros((n, 2 * k))
    X[np.arange(n), rows["posteam"].map(ix).to_numpy()] = 1.0
    X[np.arange(n), k + rows["defteam"].map(ix).to_numpy()] = 1.0
    w = rows["w"].to_numpy(float)
    mu = np.average(rows["y"], weights=w)
    y = rows["y"].to_numpy(float) - mu
    A = X.T @ (X * w[:, None]) + lam * np.eye(2 * k) * (w.mean())
    beta = np.linalg.solve(A, X.T @ (w * y))
    return pd.Series(beta[k:], index=teams)


def window(rows, lo, hi):
    return rows[(rows["week"] >= lo) & (rows["week"] <= hi)]


def season_corr(rows_by_kind, season, early, late, pred_specs, target_kind, target_lam):
    out = {}
    tgt = ratings(window(rows_by_kind[target_kind][rows_by_kind[target_kind]["season"] == season], *late), target_lam)
    for name, (kind, lam) in pred_specs.items():
        r = rows_by_kind[kind]
        pr = ratings(window(r[r["season"] == season], *early), lam)
        j = pd.concat([pr.rename("p"), tgt.rename("t")], axis=1).dropna()
        out[name] = float(j["p"].corr(j["t"])) if len(j) > 10 else np.nan
    return out


def reliability(rows, season, late, lam):
    """Spearman-Brown reliability of a window's rating: odd weeks vs even weeks."""
    r = window(rows[rows["season"] == season], *late)
    a, b = ratings(r[r["week"] % 2 == 1], lam), ratings(r[r["week"] % 2 == 0], lam)
    c = pd.concat([a, b], axis=1).dropna().corr().iloc[0, 1]
    return float(2 * c / (1 + c))


def main():
    os.makedirs(OUT, exist_ok=True)
    p = load()
    kinds = ["points", "epa", "success", "yards", "pass_epa", "rush_epa", "cpoe", "pos_WR", "pos_TE", "pos_RB"]
    rows = {k: game_rows(p, k) for k in kinds}
    preds = {"points allowed, raw": ("points", None), "points allowed, opp-adjusted": ("points", LAMBDAS["adj_mod"]),
             "yards/play allowed, raw": ("yards", None), "yards/play allowed, opp-adj": ("yards", LAMBDAS["adj_mod"]),
             "EPA/play allowed, raw": ("epa", None), "EPA/play allowed, opp-adj (light)": ("epa", LAMBDAS["adj_light"]),
             "EPA/play allowed, opp-adj": ("epa", LAMBDAS["adj_mod"]),
             "success rate allowed, opp-adj": ("success", LAMBDAS["adj_mod"]),
             "pass EPA allowed, opp-adj": ("pass_epa", LAMBDAS["adj_mod"]), "rush EPA allowed, opp-adj": ("rush_epa", LAMBDAS["adj_mod"]),
             "CPOE allowed, opp-adj": ("cpoe", LAMBDAS["adj_mod"]),
             "EPA/target to WR, opp-adj": ("pos_WR", LAMBDAS["adj_mod"]), "EPA/target to TE, opp-adj": ("pos_TE", LAMBDAS["adj_mod"]),
             "EPA/target to RB, opp-adj": ("pos_RB", LAMBDAS["adj_mod"])}
    targets = {"later points allowed (opp-adj)": ("points", LAMBDAS["adj_mod"]), "later EPA/play allowed (opp-adj)": ("epa", LAMBDAS["adj_mod"])}
    res = {}
    for tname, (tk, tl) in targets.items():
        res[tname] = {}
        for wname, (early, late) in WINDOWS.items():
            per = [season_corr(rows, s, early, late, preds, tk, tl) for s in SEASONS]
            df = pd.DataFrame(per)
            rel = np.mean([reliability(rows[tk], s, late, tl) for s in SEASONS])
            res[tname][wname] = {"corr": df.mean().round(3).to_dict(), "target_ceiling": round(float(rel), 3)}
    # prior season as a predictor, and prior + early combined (pooled standardized regression)
    prior = {}
    for tname, (tk, tl) in targets.items():
        prior[tname] = {}
        for wname, (early, late) in {"wk5-8": ((1, 4), (5, 8)), "wk9-17": ((1, 8), (9, 17))}.items():
            xs = []
            for s in SEASONS[1:]:
                tg = ratings(window(rows[tk][rows[tk]["season"] == s], *late), tl)
                pr = ratings(rows["epa"][rows["epa"]["season"] == s - 1], LAMBDAS["adj_mod"])
                er = ratings(window(rows["epa"][rows["epa"]["season"] == s], *early), LAMBDAS["adj_mod"])
                j = pd.concat([pr.rename("prior"), er.rename("early"), tg.rename("t")], axis=1).dropna()
                z = (j - j.mean()) / j.std()
                xs.append(z.assign(season=s))
            d = pd.concat(xs)
            A = np.c_[np.ones(len(d)), d["prior"], d["early"]]
            beta, *_ = np.linalg.lstsq(A, d["t"], rcond=None)
            r2 = 1 - ((d["t"] - A @ beta) ** 2).sum() / ((d["t"] - d["t"].mean()) ** 2).sum()
            prior[tname][wname] = {"corr_prior_alone": round(float(d["prior"].corr(d["t"])), 3),
                                   "corr_early_alone": round(float(d["early"].corr(d["t"])), 3),
                                   "beta_prior": round(float(beta[1]), 3), "beta_early": round(float(beta[2]), 3), "R2": round(float(r2), 3)}
    res["prior_season_EPA_allowed_combo"] = prior
    json.dump(res, open(os.path.join(OUT, "predictiveness.json"), "w"), indent=1)
    lines = ["# Defensive predictiveness (2021-2025, mean across seasons)\n"]
    for tname in targets:
        lines.append(f"\n## Target: {tname}\n")
        wn = list(WINDOWS)
        lines.append("| predictor (early window) | " + " | ".join(wn) + " |")
        lines.append("|---|" + "---|" * len(wn))
        for pn in preds:
            lines.append(f"| {pn} | " + " | ".join(f"{res[tname][w]['corr'][pn]:.2f}" for w in wn) + " |")
        lines.append("| *target ceiling (reliability)* | " + " | ".join(f"*{res[tname][w]['target_ceiling']:.2f}*" for w in wn) + " |")
    lines.append("\n## Prior season + early window (pooled, standardized)\n")
    lines.append("```json\n" + json.dumps(prior, indent=1) + "\n```")
    open(os.path.join(OUT, "predictiveness.md"), "w", encoding="utf-8").write("\n".join(lines))
    print("\n".join(lines))


if __name__ == "__main__":
    main()
