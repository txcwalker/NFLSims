"""Team pace + no-huddle tendencies for the sim (experimental, 2026-10-08).

Reads the slim pbp cache built by scripts/eda/analyze_team_pace.py and writes
data/dna/team_pace_{year}.json:
    teams[T] = {
      "pace_neutral|trail2|lead2": shrunk, calibrated log-multiplier on snap-to-snap runoff, HUDDLE
                      snaps in that game state, relative to league. exp(x) = factor.
      "nh_neutral" / "nh_trail2" / "nh_lead2": shrunk no-huddle rate by game state
    }
    _metadata: league no-huddle rates, nh_log_effect (no-huddle vs huddle runoff, in the same
    cell-normalised log units), n's, shrinkage constants.
Stability (analyze_team_pace.py, 2021-26, snaps <15s trimmed): year-over-year 0.41 neutral,
0.27 trailing 2+, 0.73 leading 2+ (n=26 team pairs, noisy).

Estimator: pool the team's prior-season plays at weight 0.5 with this season's at weight 1,
then empirical-Bayes shrink to the league mean: B = n_eff / (n_eff + sigma2/tau2), where sigma2 =
within-team per-play variance and tau2 = method-of-moments true between-team variance.
Usage:
    venv\\Scripts\\python.exe scripts/roster_management/refresh_team_pace_v_0_1_0.py <year> [--backtest]
(--backtest: estimate 2025 using ONLY 2024 data and report corr with actual 2025 values.)
"""
import json
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "eda")))
import analyze_team_pace as atp  # noqa: E402

PRIOR_WEIGHT = 0.5
# Backtest (estimate 2025 from <= 2024 data only, 31 teams): empirical-Bayes deviations were ~2x too
# wide -- regression slope of actual on predicted = 0.52 -- so pace deviations are scaled by this.
PACE_CALIBRATION = 0.52
SITS = ["neutral", "trail2", "lead2"]


def _eb(values, sigma2):
    """values: DataFrame[team, x, n_eff]. Returns {team: shrunk deviation from league mean}."""
    mu = np.average(values["x"], weights=values["n_eff"])
    obs_var = np.average((values["x"] - mu) ** 2, weights=values["n_eff"])
    tau2 = max(obs_var - np.average(sigma2 / values["n_eff"]), 1e-6)
    out = {}
    for _, r in values.iterrows():
        b = r["n_eff"] / (r["n_eff"] + sigma2 / tau2)
        out[r["team"]] = float(b * (r["x"] - mu))
    return out, float(mu), float(tau2)


def _pool(d, col, year, min_n=1):
    """Weighted team mean of col over `year` (weight 1) and year-1 (weight PRIOR_WEIGHT)."""
    d = d.assign(w=np.where(d["season"] == year, 1.0, PRIOR_WEIGHT))[lambda x: x["season"].isin([year, year - 1])]
    g = d.groupby("posteam").apply(lambda t: pd.Series({"x": np.average(t[col], weights=t["w"]),
                                                       "n_eff": t["w"].sum()}))
    return g[g["n_eff"] >= min_n].reset_index().rename(columns={"posteam": "team"})


def estimate(s, year):
    """Inputs: s (build_samples output), year. Output: (teams dict, metadata dict)."""
    teams, meta = {}, {"year": year, "prior_weight": PRIOR_WEIGHT, "pace_calibration": PACE_CALIBRATION,
                       "pace_tau2": {}, "league_nh_rate": {}}
    for sit in SITS:
        hud = s[(s["nh"] == 0) & (s["sit"] == sit)]
        sigma2 = float(hud["lr"].var())
        dev, _, tau2 = _eb(_pool(hud, "lr", year), sigma2)
        meta["pace_tau2"][sit] = round(tau2, 6)
        for t, v in dev.items():
            teams.setdefault(t, {})[f"pace_{sit}"] = round(PACE_CALIBRATION * v, 5)
    for sit in SITS:
        d = s[s["sit"] == sit]
        vals = _pool(d, "nh", year)
        p = float(d["nh"].mean())
        dev, mu, _ = _eb(vals, p * (1 - p))
        meta["league_nh_rate"][sit] = round(mu, 4)
        for t, v in dev.items():
            teams.setdefault(t, {})[f"nh_{sit}"] = round(mu + v, 4)
    # effect of a no-huddle snap vs a huddle snap, same cell-normalised log units
    meta["nh_log_effect"] = round(float(s[s["nh"] == 1]["lr"].mean() - s[s["nh"] == 0]["lr"].mean()), 4)
    return teams, meta


def write_team_pace(year, completed_week):
    """Inputs: year, completed_week. Output: path of data/dna/team_pace_{year}.json (re-pulling the
    pbp cache for `year` if it is behind completed_week). Called by refresh_weekly_dna_v_0_1_0.py."""
    s = atp.build_samples(atp.load(refresh_year=year, completed_week=completed_week))
    s = s[(s["season"] < year) | (s["week"] <= completed_week)]
    teams, meta = estimate(s[s["season"] <= year], year)
    meta["completed_week"] = int(completed_week)
    path = os.path.join("data", "dna", f"team_pace_{year}.json")
    json.dump({"_metadata": meta, "teams": teams}, open(path, "w"), indent=1, sort_keys=True)
    return path


def main():
    year = int(sys.argv[1])
    if "--backtest" not in sys.argv and len(sys.argv) > 2 and sys.argv[2].isdigit():
        print(f"wrote {write_team_pace(year, int(sys.argv[2]))}")
        return
    s = atp.build_samples(atp.load())
    if "--backtest" in sys.argv:
        past = s[s["season"] <= year - 1]
        teams, _ = estimate(past, year - 1)
        act = s[(s["season"] == year) & (s["nh"] == 0) & (s["sit"] == "neutral")].groupby("posteam")["lr"].agg(["mean", "size"])
        act = act[act["size"] >= 60]
        pred = pd.Series({t: v["pace_neutral"] for t, v in teams.items()}) / PACE_CALIBRATION
        j = act.join(pred.rename("pred"), how="inner")
        print(f"backtest {year}: n={len(j)} corr(pred from <= {year-1}, actual)={j['mean'].corr(j['pred']):.3f}; "
              f"slope={np.polyfit(j['pred'], j['mean'], 1)[0]:.2f} (1.0 = shrinkage well calibrated); "
              f"pred sd={j['pred'].std():.4f}, actual sd={j['mean'].std():.4f}")
        return
    teams, meta = estimate(s[s["season"] <= year], year)
    path = os.path.join("data", "dna", f"team_pace_{year}.json")
    json.dump({"_metadata": meta, "teams": teams}, open(path, "w"), indent=1, sort_keys=True)
    print(f"wrote {path}  meta={meta}")
    tt = pd.DataFrame(teams).T
    print(tt.sort_values("pace_neutral").round(3).iloc[[0, 1, 2, 3, -4, -3, -2, -1]].to_string())


if __name__ == "__main__":
    main()
