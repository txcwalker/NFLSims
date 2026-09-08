"""Generates a dedicated playoff round-by-round odds summary for the 2026
simulated season, from docs/reports/season_summaries_2026.csv (written by
run_full_season_sim_2026.py's 100-season Monte Carlo).

The per-division standings tables already carry these same percentages, but
buried one team at a time inside 8 separate division tables -- this pulls
them into one league-wide view, ranked by Super Bowl odds, so the actual
playoff picture (who's really in the mix at each round) reads at a glance
instead of needing to be reconstructed by scanning 8 tables.

Usage: python generate_playoff_summary_2026.py
"""
import os
import pandas as pd

SIM_YEAR = 2026
SUMMARY_CSV = f"docs/reports/season_summaries_{SIM_YEAR}.csv"
OUTPUT_DIR = "docs/reports"


def bar(pct, width=20):
    filled = round(pct / 100 * width)
    return "█" * filled + "░" * (width - filled)


def generate():
    if not os.path.exists(SUMMARY_CSV):
        print(f"Error: {SUMMARY_CSV} not found -- run run_full_season_sim_{SIM_YEAR}.py first.")
        return

    df = pd.read_csv(SUMMARY_CSV)
    df = df.sort_values("Champion_%", ascending=False)

    md = f"# NFL {SIM_YEAR} Playoff Odds Summary\n"
    md += f"**Probability of reaching each round, across 100 simulated seasons. Ranked by Super Bowl win odds.**\n\n"

    md += "## Full League\n\n"
    md += "| Rank | Team | Conf | Div | Made Playoffs | Won Wild Card | Won Divisional | Won Conference | Won Super Bowl |\n"
    md += "| :---: | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |\n"
    for idx, (_, r) in enumerate(df.iterrows()):
        md += (f"| {idx+1} | **{r['Team']}** | {r['Conference']} | {r['Division']} | "
               f"{r['Playoffs_%']:.0f}% | {r['Divional_%']:.0f}% | {r['Conference_%']:.0f}% | "
               f"{r['SuperBowl_%']:.0f}% | **{r['Champion_%']:.1f}%** |\n")
    md += "\n"

    md += "## Super Bowl Odds (visual)\n\n"
    md += "| Team | Champion % |\n"
    md += "| :--- | :--- |\n"
    for _, r in df[df["Champion_%"] > 0].iterrows():
        md += f"| **{r['Team']}** | `{bar(r['Champion_%'])}` {r['Champion_%']:.1f}% |\n"
    md += "\n"

    md += "## By Conference\n\n"
    for conf in ["AFC", "NFC"]:
        conf_df = df[df["Conference"] == conf].sort_values("Playoffs_%", ascending=False)
        md += f"### {conf}\n\n"
        md += "| Team | Div | Made Playoffs | Won Wild Card | Won Divisional | Won Conference | Won Super Bowl |\n"
        md += "| :--- | :---: | :---: | :---: | :---: | :---: | :---: |\n"
        for _, r in conf_df.iterrows():
            md += (f"| **{r['Team']}** | {r['Division']} | {r['Playoffs_%']:.0f}% | {r['Divional_%']:.0f}% | "
                   f"{r['Conference_%']:.0f}% | {r['SuperBowl_%']:.0f}% | {r['Champion_%']:.1f}% |\n")
        md += "\n"

    md += "## Longshots and Locks\n\n"
    locks = df[df["Playoffs_%"] >= 95].sort_values("Playoffs_%", ascending=False)
    bubble = df[(df["Playoffs_%"] > 5) & (df["Playoffs_%"] < 60)].sort_values("Playoffs_%", ascending=False)
    dead = df[df["Playoffs_%"] <= 1].sort_values("PF_Avg", ascending=False)

    md += f"**Locks (>=95% playoff odds):** {', '.join(locks['Team']) if len(locks) else 'None'}\n\n"
    md += f"**Bubble (5-60% playoff odds):** {', '.join(bubble['Team']) if len(bubble) else 'None'}\n\n"
    md += f"**Eliminated (<=1% playoff odds):** {', '.join(dead['Team']) if len(dead) else 'None'}\n\n"

    out_path = os.path.join(OUTPUT_DIR, f"playoff_summary_{SIM_YEAR}.md")
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(md)
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    generate()
