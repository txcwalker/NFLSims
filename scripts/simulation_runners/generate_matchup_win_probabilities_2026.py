"""Generates a per-matchup win-probability breakdown for every one of the 272
real 2026 regular-season games, organized by week, from the games parquet
cache written by run_full_season_sim_2026.py.

Unlike the season standings (which sample 11 outcomes per matchup to produce
discrete win/loss records for the 100-season Monte Carlo), this reports the
RAW win rate across all 1,000 pre-simulated iterations for each matchup
directly -- i.e. "what percentage of the time did each team win this
specific game," not a discrete season outcome.

Usage: python generate_matchup_win_probabilities_2026.py
"""
import os
import json
import pandas as pd

SIM_YEAR = 2026
GAMES_CACHE = f"data/interim/sim_results_{SIM_YEAR}_games.parquet"
SCHEDULE_CSV = f"data/external/schedule_{SIM_YEAR}.csv"
OUTPUT_DIR = "docs/reports"


def generate():
    if not os.path.exists(GAMES_CACHE):
        print(f"Error: {GAMES_CACHE} not found -- run run_full_season_sim_{SIM_YEAR}.py first.")
        return

    games_df = pd.read_parquet(GAMES_CACHE)
    sched = pd.read_csv(SCHEDULE_CSV)
    sched = sched[sched["game_type"] == "REG"][[
        "game_id", "week", "gameday", "spread_line", "total_line",
        "away_moneyline", "home_moneyline",
    ]]

    win_pct = games_df.groupby("game_id").apply(
        lambda g: pd.Series({
            "away_team": g["away_team"].iloc[0],
            "home_team": g["home_team"].iloc[0],
            "away_win_pct": (g["away_score"] > g["home_score"]).mean() * 100,
            "home_win_pct": (g["home_score"] > g["away_score"]).mean() * 100,
            "tie_pct": (g["away_score"] == g["home_score"]).mean() * 100,
            "avg_away_score": g["away_score"].mean(),
            "avg_home_score": g["home_score"].mean(),
        }),
        include_groups=False,
    ).reset_index()

    merged = win_pct.merge(sched, on="game_id", how="left").sort_values(["week", "game_id"])

    # Moneylines are whole numbers (e.g. -148, +124). Cast to pandas' nullable
    # Int so missing odds stay <NA> (later mapped to JSON null) instead of
    # becoming 124.0 floats in the output.
    for col in ("away_moneyline", "home_moneyline"):
        merged[col] = merged[col].astype("Int64")

    md = f"# NFL {SIM_YEAR} Matchup Win Probabilities\n"
    md += "**Raw win rate across the 1,000 pre-simulated iterations for each of the 272 real matchups.**\n"
    md += "*(Different from the season standings, which use an 11-game sample per matchup to produce discrete W/L records.)*\n\n"

    for week in sorted(merged["week"].dropna().unique()):
        wk = merged[merged["week"] == week]
        md += f"## Week {int(week)}\n\n"
        md += "| Away | Win % | Avg Score | | Home | Win % | Avg Score |\n"
        md += "| :--- | :---: | :---: | :---: | :--- | :---: | :---: |\n"
        for _, r in wk.iterrows():
            fav_away = "**" if r["away_win_pct"] >= r["home_win_pct"] else ""
            fav_home = "**" if r["home_win_pct"] > r["away_win_pct"] else ""
            md += (f"| {fav_away}{r['away_team']}{fav_away} | {fav_away}{r['away_win_pct']:.1f}%{fav_away} | "
                   f"{r['avg_away_score']:.1f} | @ | {fav_home}{r['home_team']}{fav_home} | "
                   f"{fav_home}{r['home_win_pct']:.1f}%{fav_home} | {r['avg_home_score']:.1f} |\n")
        md += "\n"

    out_path = os.path.join(OUTPUT_DIR, f"matchup_win_probabilities_{SIM_YEAR}.md")
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(md)
    print(f"Wrote {out_path} ({len(merged)} games across {merged['week'].nunique()} weeks)")

    # Structured JSON sibling for the analytics dev site (frontend_analysis
    # Season2026 page), grouped by week -- same data as the markdown above.
    weeks_json = {}
    for week in sorted(merged["week"].dropna().unique()):
        wk = merged[merged["week"] == week].round(2)
        # Most weeks this far out have no posted line yet -- NaN isn't valid
        # JSON, so convert to null (object dtype) rather than leaving the
        # literal NaN token, which would break JSON.parse on the frontend.
        wk = wk.astype(object).where(wk.notna(), None)
        weeks_json[str(int(week))] = wk.drop(columns=["gameday"]).to_dict("records")
    json_path = os.path.join(OUTPUT_DIR, f"matchup_win_probabilities_{SIM_YEAR}.json")
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump({"weeks": weeks_json}, f)
    print(f"Wrote {json_path}")


if __name__ == "__main__":
    generate()
