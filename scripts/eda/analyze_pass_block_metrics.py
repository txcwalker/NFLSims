"""
analyze_pass_block_metrics.py
===============================
EDA for the pass-blocking (offense) and pass-rush (defense) cells of the
trench-tier rebuild. This is EDA to pick good real metrics for the eventual
trench multiplier -- NOT a training script, no model involved.

Source: nfl_data_py.import_weekly_pfr(s_type='pass'), 2018-2024. This table
already has both 'team' (the passer's team) and 'opponent' columns per row,
so no separate schedule join is needed (unlike the rushing case).

IMPORTANT: def_times_blitzed / def_times_hurried / def_times_hitqb are
confirmed 100% NULL across every season checked (2020-2024) -- unusable
despite existing as column names. Defense-side pass-rush numbers here are
instead DERIVED by opponent-joining the real, populated offense-side columns
(times_pressured, times_hurried, times_hit) -- same technique already proven
for run-defense metrics.

Offense (pass-blocking) candidates:
  - pressured_pct_allowed : times_pressured_pct, team-season weighted avg
  - hurry_rate_allowed    : times_hurried / implied_dropbacks
  - hit_rate_allowed      : times_hit / implied_dropbacks

Defense (pass-rush) candidates:
  - pressure_rate_forced  : opponent-joined times_pressured / dropbacks faced
  - hurry_rate_forced     : opponent-joined times_hurried / dropbacks faced
  - hit_rate_forced       : opponent-joined times_hit / dropbacks faced
  (cross-checked against the already-live trench_dna.json def_pressure_rate/def_sack_rate)

Run from repo root: python scripts/eda/analyze_pass_block_metrics.py
"""

import json
import numpy as np
import pandas as pd
import nfl_data_py as nfl

YEARS = list(range(2018, 2026))


def load_pfr_pass():
    print("Pulling PFR pass data...")
    df = nfl.import_weekly_pfr(s_type="pass", years=YEARS)
    df = df[df["times_pressured"].notna() & (df["times_pressured_pct"] > 0)].copy()
    df["implied_dropbacks"] = df["times_pressured"] / df["times_pressured_pct"]
    return df


def build_offense_team_season(df):
    def wavg(g, col):
        w = g["implied_dropbacks"].sum()
        return (g[col] * g["implied_dropbacks"]).sum() / w if w > 0 else np.nan

    rows = []
    for (team, season), g in df.groupby(["team", "season"]):
        dropbacks = g["implied_dropbacks"].sum()
        rows.append({
            "team": team, "season": season,
            "pressured_pct_allowed": wavg(g, "times_pressured_pct"),
            "hurry_rate_allowed": g["times_hurried"].sum() / dropbacks if dropbacks > 0 else np.nan,
            "hit_rate_allowed": g["times_hit"].sum() / dropbacks if dropbacks > 0 else np.nan,
        })
    return pd.DataFrame(rows)


def build_defense_team_season(df):
    """Opponent-join: this row's times_pressured/hurried/hit were inflicted BY
    the 'opponent' team's defense ON the 'team' passer. Group by opponent."""
    rows = []
    for (opp, season), g in df.groupby(["opponent", "season"]):
        dropbacks = g["implied_dropbacks"].sum()
        rows.append({
            "team": opp, "season": season,
            "pressure_rate_forced": g["times_pressured"].sum() / dropbacks if dropbacks > 0 else np.nan,
            "hurry_rate_forced": g["times_hurried"].sum() / dropbacks if dropbacks > 0 else np.nan,
            "hit_rate_forced": g["times_hit"].sum() / dropbacks if dropbacks > 0 else np.nan,
        })
    return pd.DataFrame(rows)


def main():
    df = load_pfr_pass()
    off = build_offense_team_season(df)
    defn = build_defense_team_season(df)

    print(f"\nOffense team-seasons: {len(off)} | Defense team-seasons: {len(defn)}")

    off_metrics = ["pressured_pct_allowed", "hurry_rate_allowed", "hit_rate_allowed"]
    def_metrics = ["pressure_rate_forced", "hurry_rate_forced", "hit_rate_forced"]

    print("\n=== Offense (pass-blocking) correlation matrix ===\n")
    print(off[off_metrics].corr().round(2).to_string())

    print("\n=== Defense (pass-rush, new candidates) correlation matrix ===\n")
    print(defn[def_metrics].corr().round(2).to_string())

    # Cross-check new defense candidates against the already-live trench_dna.json metrics
    trench = json.load(open("data/dna/trench_dna.json"))
    trench_2024 = trench.get("2024", {})
    trench_df = pd.DataFrame([
        {"team": t, "def_pressure_rate": v.get("def_pressure_rate"), "def_sack_rate": v.get("def_sack_rate")}
        for t, v in trench_2024.items()
    ])
    defn_2024 = defn[defn["season"] == 2024].merge(trench_df, on="team", how="inner")
    print(f"\n=== New pass-rush candidates vs. existing def_pressure_rate (2024, {len(defn_2024)} teams) ===\n")
    print(defn_2024[def_metrics + ["def_pressure_rate"]].corr()["def_pressure_rate"].drop("def_pressure_rate").round(3).to_string())

    off.to_csv("docs/eda_outputs/pass_block_offense_team_season.csv", index=False)
    defn.to_csv("docs/eda_outputs/pass_block_defense_team_season.csv", index=False)
    print("\nSaved offense + defense team-season CSVs to docs/eda_outputs/")


if __name__ == "__main__":
    main()
