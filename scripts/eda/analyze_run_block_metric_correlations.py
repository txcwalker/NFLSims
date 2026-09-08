"""
analyze_run_block_metric_correlations.py
=========================================
EDA for the v0.4.0 trench-tier migration (run-blocking side) -- OFFENSE ONLY.
Cam wants a 4-metric run-blocking feature set to mirror the existing 4-metric
pass-blocking set in trench_dna.json (sack_rate_allowed, def_pressure_rate,
def_sack_rate, avg_air_yards). YBC (yards before contact) is already chosen;
this script checks pairwise correlation among the remaining 6 candidates plus
YBC, all aggregated to team-season level (the grain trench_dna.json actually
uses), 2018-2024 (matching pfr_adv_rushing_stats.csv's coverage).

Decision (2026-07-16): this feeds a pure OFFENSE run-blocking multiplier, not
a matchup differential against opponent run-defense quality -- mirrors the
precedent set by the deployed air_yards_v_0_1_1 model, which also has zero
defense-side inputs. A run-blocking team gets the same multiplier regardless
of the opponent's run defense until real defense-side data is built later
(deliberate, accepted short-term accuracy gap -- see AGENTS.md).

Candidates (offense side, i.e. run-BLOCKING quality):
  - ybc_per_att              : yards before contact per attempt (PFR)
  - ryoe_per_att             : rush yards over expected per attempt (NGS)
  - rush_pct_over_expected   : % over expected (NGS)
  - avg_time_to_los          : avg time to line of scrimmage, seconds (NGS)
  - efficiency               : NGS "efficiency" (distance traveled / yards gained)
  - pct_stacked_box          : % of attempts facing 8+ defenders (NGS) -- note this
                               is a *defense-faced* signal, kept in for completeness
  - stuff_rate               : % of attempts at or behind the LOS (derived from PBP)
  - aly                      : Adjusted Line Yards (derived from PBP, standard
                               Football Outsiders weighting)

Excludes QB scrambles/kneels from all "run" attempt counts -- these aren't
designed runs and don't reflect O-line blocking.

Run from repo root: python scripts/eda/analyze_run_block_metric_correlations.py
"""

import numpy as np
import pandas as pd
import nfl_data_py as nfl

YEARS = list(range(2018, 2026))


def aly_value(yards):
    """Football Outsiders Adjusted Line Yards weighting, per play."""
    if yards < 0:
        return yards * 1.2
    elif yards <= 4:
        return yards
    elif yards <= 10:
        return 4 + (yards - 4) * 0.5
    else:
        return 4 + 6 * 0.5  # yards 11+ capped at the 5-10 boundary value


def build_pbp_team_season():
    print("Pulling PBP data (this is the slow part)...")
    pbp = nfl.import_pbp_data(YEARS, downcast=True)
    cols = ["posteam", "season", "rush_attempt", "qb_scramble", "qb_kneel", "yards_gained"]
    pbp = pbp[cols].copy()

    runs = pbp[
        (pbp["rush_attempt"] == 1)
        & (pbp["qb_scramble"].fillna(0) == 0)
        & (pbp["qb_kneel"].fillna(0) == 0)
        & pbp["posteam"].notna()
    ].copy()

    runs["is_stuffed"] = (runs["yards_gained"] <= 0).astype(int)
    runs["aly_val"] = runs["yards_gained"].apply(aly_value)

    grp = runs.groupby(["posteam", "season"]).agg(
        attempts=("yards_gained", "count"),
        stuff_rate=("is_stuffed", "mean"),
        aly=("aly_val", "mean"),
    ).reset_index()
    grp = grp.rename(columns={"posteam": "team"})
    return grp


def build_ngs_team_season():
    print("Pulling NGS rushing data...")
    ngs = nfl.import_ngs_data(stat_type="rushing", years=YEARS)
    ngs = ngs[ngs["week"] > 0].copy()  # drop season-total rows (week==0)

    for c in ["efficiency", "avg_time_to_los", "rush_yards_over_expected_per_att",
              "rush_pct_over_expected", "percent_attempts_gte_eight_defenders",
              "rush_attempts"]:
        ngs[c] = pd.to_numeric(ngs[c], errors="coerce")

    def wavg(df, col, w="rush_attempts"):
        wsum = df[w].sum()
        return (df[col] * df[w]).sum() / wsum if wsum > 0 else np.nan

    rows = []
    for (team, season), g in ngs.groupby(["team_abbr", "season"]):
        rows.append({
            "team": team, "season": season,
            "ngs_attempts": g["rush_attempts"].sum(),
            "ryoe_per_att": wavg(g, "rush_yards_over_expected_per_att"),
            "rush_pct_over_expected": wavg(g, "rush_pct_over_expected"),
            "avg_time_to_los": wavg(g, "avg_time_to_los"),
            "efficiency": wavg(g, "efficiency"),
            "pct_stacked_box": wavg(g, "percent_attempts_gte_eight_defenders"),
        })
    return pd.DataFrame(rows)


def build_pfr_ybc_team_season():
    print("Loading existing PFR ybc data...")
    pfr = pd.read_csv("data/external/pfr_adv_rushing_stats.csv")
    pfr["implied_att"] = np.where(pfr["ybc_avg"] > 0, pfr["ybc"] / pfr["ybc_avg"], np.nan)

    rows = []
    for (team, season), g in pfr.groupby(["team", "season"]):
        total_ybc = g["ybc"].sum()
        total_att = g["implied_att"].sum(skipna=True)
        rows.append({
            "team": team, "season": season,
            "ybc_per_att": total_ybc / total_att if total_att > 0 else np.nan,
        })
    return pd.DataFrame(rows)


def main():
    pbp_agg = build_pbp_team_season()
    ngs_agg = build_ngs_team_season()
    pfr_agg = build_pfr_ybc_team_season()

    df = pfr_agg.merge(ngs_agg, on=["team", "season"], how="inner")
    df = df.merge(pbp_agg[["team", "season", "stuff_rate", "aly"]], on=["team", "season"], how="inner")

    print(f"\nTeam-seasons in combined dataset: {len(df)}")

    metrics = ["ybc_per_att", "ryoe_per_att", "rush_pct_over_expected",
               "avg_time_to_los", "efficiency", "pct_stacked_box",
               "stuff_rate", "aly"]

    corr = df[metrics].corr(method="pearson")
    print("\n=== Pearson correlation matrix (team-season level, 2018-2024) ===\n")
    print(corr.round(2).to_string())

    print("\n=== Correlation with ybc_per_att specifically ===\n")
    print(corr["ybc_per_att"].drop("ybc_per_att").sort_values(key=abs, ascending=False).round(3).to_string())

    df.to_csv("docs/eda_outputs/run_block_metrics_team_season.csv", index=False)
    corr.to_csv("docs/eda_outputs/run_block_metrics_correlation.csv")
    print("\nSaved: docs/eda_outputs/run_block_metrics_team_season.csv")
    print("Saved: docs/eda_outputs/run_block_metrics_correlation.csv")


if __name__ == "__main__":
    main()
