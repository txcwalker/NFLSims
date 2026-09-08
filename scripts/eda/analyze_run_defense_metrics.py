"""
analyze_run_defense_metrics.py
===============================
Defense-side mirror of analyze_run_block_metric_correlations.py. There is no
native "defense" stat_type anywhere in the pipeline for rushing (import_seasonal_pfr
only supports pass/rec/rush; standard seasonal_data has no run-stop columns) --
every run-defense metric here is DERIVED by opponent-joining the same offense-side
rushing data against schedule data, i.e. "how much X did this defense allow,
aggregated across every rusher who faced them."

stuff_rate_forced / aly_allowed don't need a join -- raw PBP already tags every
play with defteam directly.

ybc_allowed_per_att / rush_pct_over_expected_allowed / pct_stacked_box_forced
DO need the join -- PFR/NGS data is keyed by the rusher's own team, not the
opponent, so team-week -> opponent comes from schedules_2015_2024.csv.

Run from repo root: python scripts/eda/analyze_run_defense_metrics.py
(Run analyze_run_block_metric_correlations.py first if its output CSV is missing.)
"""

import numpy as np
import pandas as pd
import nfl_data_py as nfl

YEARS = list(range(2018, 2026))


def build_opponent_map():
    sched = pd.read_csv("data/external/schedules_2015_2024.csv")
    sched = sched[sched["season"].isin(YEARS)]
    a = sched[["season", "week", "away_team", "home_team"]].rename(
        columns={"away_team": "team", "home_team": "opponent"})
    h = sched[["season", "week", "home_team", "away_team"]].rename(
        columns={"home_team": "team", "away_team": "opponent"})
    return pd.concat([a, h], ignore_index=True)


def aly_value(yards):
    if yards < 0:
        return yards * 1.2
    elif yards <= 4:
        return yards
    elif yards <= 10:
        return 4 + (yards - 4) * 0.5
    else:
        return 4 + 6 * 0.5


def build_pbp_defense_team_season():
    """stuff_rate_forced / aly_allowed -- no join needed, PBP has defteam directly."""
    print("Pulling PBP data...")
    pbp = nfl.import_pbp_data(YEARS, downcast=True)
    cols = ["defteam", "season", "rush_attempt", "qb_scramble", "qb_kneel", "yards_gained"]
    pbp = pbp[cols].copy()
    runs = pbp[
        (pbp["rush_attempt"] == 1) & (pbp["qb_scramble"].fillna(0) == 0)
        & (pbp["qb_kneel"].fillna(0) == 0) & pbp["defteam"].notna()
    ].copy()
    runs["is_stuffed"] = (runs["yards_gained"] <= 0).astype(int)
    runs["aly_val"] = runs["yards_gained"].apply(aly_value)
    grp = runs.groupby(["defteam", "season"]).agg(
        stuff_rate_forced=("is_stuffed", "mean"),
        aly_allowed=("aly_val", "mean"),
    ).reset_index().rename(columns={"defteam": "team"})
    return grp


def build_opponent_joined_metrics(opp_map):
    """ybc_allowed_per_att, rush_pct_over_expected_allowed, pct_stacked_box_forced --
    all need the opponent join since PFR/NGS data is keyed by the rusher's team."""
    print("Loading PFR ybc data + joining opponent...")
    pfr = pd.read_csv("data/external/pfr_adv_rushing_stats.csv")
    pfr["implied_att"] = np.where(pfr["ybc_avg"] > 0, pfr["ybc"] / pfr["ybc_avg"], np.nan)
    pfr = pfr.merge(opp_map, on=["season", "week", "team"], how="inner")

    ybc_def = pfr.groupby(["opponent", "season"]).apply(
        lambda g: g["ybc"].sum() / g["implied_att"].sum() if g["implied_att"].sum(skipna=True) > 0 else np.nan,
        include_groups=False
    ).reset_index(name="ybc_allowed_per_att").rename(columns={"opponent": "team"})

    print("Pulling NGS rushing data + joining opponent...")
    ngs = nfl.import_ngs_data(stat_type="rushing", years=YEARS)
    ngs = ngs[ngs["week"] > 0].copy()
    for c in ["rush_pct_over_expected", "percent_attempts_gte_eight_defenders", "rush_attempts"]:
        ngs[c] = pd.to_numeric(ngs[c], errors="coerce")
    ngs = ngs.rename(columns={"team_abbr": "team"}).merge(opp_map, on=["season", "week", "team"], how="inner")

    def wavg(g, col, w="rush_attempts"):
        wsum = g[w].sum()
        return (g[col] * g[w]).sum() / wsum if wsum > 0 else np.nan

    rows = []
    for (opp, season), g in ngs.groupby(["opponent", "season"]):
        rows.append({
            "team": opp, "season": season,
            "rush_pct_over_expected_allowed": wavg(g, "rush_pct_over_expected"),
            "pct_stacked_box_forced": wavg(g, "percent_attempts_gte_eight_defenders"),
        })
    ngs_def = pd.DataFrame(rows)

    return ybc_def.merge(ngs_def, on=["team", "season"], how="inner")


def main():
    opp_map = build_opponent_map()
    pbp_def = build_pbp_defense_team_season()
    joined_def = build_opponent_joined_metrics(opp_map)

    df = pbp_def.merge(joined_def, on=["team", "season"], how="inner")
    print(f"\nTeam-seasons in combined defense dataset: {len(df)}")

    metrics = ["ybc_allowed_per_att", "rush_pct_over_expected_allowed",
               "stuff_rate_forced", "aly_allowed", "pct_stacked_box_forced"]

    corr = df[metrics].corr(method="pearson")
    print("\n=== Run-defense metric correlation matrix (team-season, 2018-2024) ===\n")
    print(corr.round(2).to_string())

    # Cross-check against existing trench_dna.json pass-rush metrics -- are these
    # run-D metrics redundant with the ALREADY-LIVE def_pressure_rate/def_sack_rate,
    # or a genuinely distinct skill (run-stopping vs. pass-rushing)?
    import json
    trench = json.load(open("data/dna/trench_dna.json"))
    trench_rows = [{"team": t, "def_pressure_rate": v.get("def_pressure_rate"),
                    "def_sack_rate": v.get("def_sack_rate")}
                   for t, v in trench.items() if t != "_metadata"]
    trench_df = pd.DataFrame(trench_rows)
    df_2024 = df[df["season"] == 2024].merge(trench_df, on="team", how="inner")
    cross_corr = df_2024[metrics + ["def_pressure_rate", "def_sack_rate"]].corr()["def_pressure_rate"].drop(
        ["def_pressure_rate", "def_sack_rate"])
    print("\n=== Run-D metrics vs. existing pass-rush def_pressure_rate (2024, is run-D a distinct skill?) ===\n")
    print(cross_corr.round(3).to_string())

    df.to_csv("docs/eda_outputs/run_defense_metrics_team_season.csv", index=False)
    corr.to_csv("docs/eda_outputs/run_defense_metrics_correlation.csv")
    print("\nSaved: docs/eda_outputs/run_defense_metrics_team_season.csv")
    print("Saved: docs/eda_outputs/run_defense_metrics_correlation.csv")


if __name__ == "__main__":
    main()
