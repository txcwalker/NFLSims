"""Real pass rate by game script (score differential x time remaining), 2021-2025.

Feeds the play-selection review: is the sim too run-heavy overall, or only
failing to pass enough when trailing badly late? Compares against the sim if
--sim is passed (needs a fresh slate run).

Pass = play_type in ('pass','run') and it's a pass (dropbacks incl. sacks/
scrambles, matching how the engine counts). REG only, excludes garbage-time
kneels/spikes and 4th down.

Usage: venv\\Scripts\\python.exe scripts/eda/analyze_pass_rate_by_script.py
"""
import numpy as np
import pandas as pd
import nfl_data_py as nfl

SEASONS = list(range(2021, 2026))


def score_band(sd):
    if sd >= 17: return "lead 17+"
    if sd >= 9: return "lead 9-16"
    if sd >= 1: return "lead 1-8"
    if sd == 0: return "tied"
    if sd >= -8: return "trail 1-8"
    if sd >= -16: return "trail 9-16"
    return "trail 17+"


BANDS = ["lead 17+", "lead 9-16", "lead 1-8", "tied", "trail 1-8", "trail 9-16", "trail 17+"]
TIME = [("Q1-Q2", 1800, 3600), ("Q3", 900, 1800), ("Q4 15-4", 240, 900), ("Q4 <4", 0, 240)]


def main():
    print(f"loading PBP {SEASONS} ...")
    df = nfl.import_pbp_data(SEASONS, downcast=True, cache=False)
    df = df[df["season_type"] == "REG"]
    p = df[
        (df["play_type"].isin(["pass", "run"]))
        & (df["down"].isin([1, 2, 3]))
        & (df["qb_kneel"] != 1) & (df["qb_spike"] != 1)
        & df["score_differential"].notna()
        & df["game_seconds_remaining"].notna()
    ].copy()
    p["is_pass"] = (p["play_type"] == "pass").astype(float)
    p["band"] = p["score_differential"].apply(score_band)

    print(f"\n{len(p):,} plays. Pass rate % by score band x time:\n")
    hdr = f"{'band':<12}" + "".join(f"{t[0]:>10}" for t in TIME) + f"{'  ALL':>8}"
    print(hdr); print("-" * len(hdr))
    for band in BANDS:
        row = f"{band:<12}"
        b = p[p["band"] == band]
        for _, lo, hi in TIME:
            sub = b[(b["game_seconds_remaining"] > lo) & (b["game_seconds_remaining"] <= hi)]
            row += f"{100*sub['is_pass'].mean():>10.1f}" if len(sub) > 200 else f"{'--':>10}"
        row += f"{100*b['is_pass'].mean():>8.1f}"
        print(row)
    print("-" * len(hdr))
    print(f"{'LEAGUE ALL':<12}" + " " * (10 * len(TIME)) + f"{100*p['is_pass'].mean():>8.1f}")

    # 1st-down only (closest to 'neutral' script) for the base-rate anchor
    d1 = p[(p["down"] == 1) & (p["game_seconds_remaining"] > 900)]
    print(f"\n1st down, pre-Q4, within one score: "
          f"{100*d1[d1['score_differential'].abs() <= 8]['is_pass'].mean():.1f}% pass "
          f"(the 'neutral game script' base rate)")


if __name__ == "__main__":
    main()
