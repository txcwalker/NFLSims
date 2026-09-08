"""Generates the skeleton for data/dna/rookie_projections_2026.json --
placeholder preseason projections for every rookie (entry_year == 2026)
flagged by build_2026_rosters_v_0_1_0.py, sorted by draft round so the
highest-capital picks are easiest to find and prioritize.

This is a STARTING POINT for Cam to hand-edit, not a real projection --
every early_value/late_value below is a rough position-baseline guess, not
derived from any actual scouting/analytics process. Values only exist so
the file has a sensible, obviously-editable shape rather than being empty.

Schema (see docs/sims/inputs/README.md's 2026 rollover section):
  usage_curve / efficiency_curve: {field: {start_week, steady_week,
    early_value, late_value}}, resolved per-game by
    rookie_curves_v_0_1_0.resolve_rookie_curves(). Interpolated linearly
    between start_week and steady_week.
  static_projection: {field: value} -- flat number, no curve, used at every
    taper step (same treatment as a veteran's career-DNA fallback). These
    are fields rolling_stats_v_0_1_0 doesn't track a live L4/season signal
    for (see its PLAYER_RATE_FIELDS), so they never get blended toward real
    in-season data regardless -- setting them once here is the whole story.

QB usage_curve is intentionally omitted: a QB's snap share isn't a gradual
ramp, it's a status flip (backup -> starter) driven by real evidence, which
Phase 5's weekly refresh handles directly on the roster's `status` field --
see the 2026 rollover plan's Phase 5 addendum. A QB rookie's
efficiency_curve still matters once they ARE playing, so that part is kept.
"""
import json
import os
import glob

ROSTERS_DIR = "data/current_rosters"
OUTPUT_PATH = "data/dna/rookie_projections_2026.json"

# start_week=1, steady_week=5 matches the taper's own boundary -- by game 6
# the blend has already fully handed off to real data regardless of this
# curve, so ramping to "true" value by week 5 is the natural default shape.
DEFAULT_START_WEEK = 1
DEFAULT_STEADY_WEEK = 5


def curve(early, late):
    return {"start_week": DEFAULT_START_WEEK, "steady_week": DEFAULT_STEADY_WEEK,
            "early_value": early, "late_value": late}


# Position-baseline placeholder values -- deliberately modest (rookies
# underperform veterans on average), NOT a real per-player projection.
POSITION_DEFAULTS = {
    "QB": {
        "efficiency_curve": {
            "cpoe": curve(-2.0, 0.0),
            "sack_rate": curve(0.08, 0.06),
            "scramble_rate": curve(0.08, 0.05),
        },
        "static_projection": {
            "avg_air_yards_per_att": 7.5, "deep_ball_rate": 0.10,
            "play_action_rate": 0.20, "under_pressure_cpoe": -4.0,
            "pressure_rate": 0.22, "avg_time_to_throw_sec": 2.8,
            "ngs_aggressiveness_index": 14.0,
        },
    },
    "RB": {
        "usage_curve": {
            "target_share": curve(0.03, 0.08),
            "carry_share": curve(0.15, 0.35),
        },
        "efficiency_curve": {
            "ypc": curve(3.5, 4.3),
            "catch_rate": curve(0.65, 0.75),
            "yac_per_rec": curve(4.0, 5.0),
        },
        "static_projection": {
            "elusiveness": 0.0, "broken_tackle_rate": 0.15, "efficiency": 3.5,
            "percent_attempts_gte_eight_defenders": 0.22, "avg_time_to_los": 2.8,
            "rush_yards_over_expected_per_att": 0.0, "rush_pct_over_expected": 0.30,
            "top_speed_mph": 20.3, "contested_catch_rate": 0.35,
            "avg_separation_yds": 3.2, "deep_target_rate": 0.04,
        },
    },
    "WR": {
        "usage_curve": {"target_share": curve(0.03, 0.12)},
        "efficiency_curve": {
            "catch_rate": curve(0.55, 0.62),
            "yac_per_rec": curve(3.5, 4.2),
            "adot": curve(8.0, 11.5),
        },
        "static_projection": {
            "elusiveness": 0.0, "broken_tackle_rate": 0.15,
            "avg_separation_yds": 2.6, "avg_cushion_yds": 5.8,
            "route_profile": "intermediate", "top_speed_mph": 21.0,
            "deep_target_rate": 0.13, "contested_catch_rate": 0.45,
        },
    },
    "TE": {
        "usage_curve": {"target_share": curve(0.02, 0.08)},
        "efficiency_curve": {
            "catch_rate": curve(0.60, 0.68),
            "yac_per_rec": curve(3.8, 4.5),
            "adot": curve(5.5, 7.5),
        },
        "static_projection": {
            "elusiveness": 0.0, "broken_tackle_rate": 0.15,
            "avg_separation_yds": 2.8, "avg_cushion_yds": 5.6,
            "route_profile": "short", "top_speed_mph": 19.8,
            "deep_target_rate": 0.05, "contested_catch_rate": 0.50,
        },
    },
}


def collect_rookies():
    """Scans Phase 3's output for every rookie: True entry."""
    rookies = []
    for path in glob.glob(os.path.join(ROSTERS_DIR, "*_traits_2026.json")):
        data = json.load(open(path))
        team = data["team"]
        for name, traits in data.get("traits", {}).items():
            if traits.get("rookie"):
                rookies.append({
                    "name": name, "team": team, "pos": traits["pos"],
                    "draft_capital": traits.get("draft_capital"),
                })
    # Drafted picks first (by round, then pick), UDFAs last.
    rookies.sort(key=lambda r: (
        r["draft_capital"] is None,
        r["draft_capital"]["round"] if r["draft_capital"] else 999,
        r["draft_capital"]["pick"] if r["draft_capital"] else 999,
    ))
    return rookies


def build_skeleton(rookies):
    out = {
        "_metadata": {
            "version": "V.0.1.0",
            "created": "2026-07-22",
            "note": (
                "Skeleton generated by build_rookie_projections_v_0_1_0.py from "
                "Phase 3's rookie flags. Every value below is a rough position-"
                "baseline placeholder, NOT a real per-player projection -- edit "
                "before relying on this for a live sim. Sorted by draft round "
                "(UDFAs last)."
            ),
        }
    }
    for r in rookies:
        pos_defaults = POSITION_DEFAULTS[r["pos"]]
        entry = {"pos": r["pos"], "team": r["team"]}
        if r["draft_capital"]:
            entry["draft_capital"] = r["draft_capital"]
        if "usage_curve" in pos_defaults:
            entry["usage_curve"] = json.loads(json.dumps(pos_defaults["usage_curve"]))
        entry["efficiency_curve"] = json.loads(json.dumps(pos_defaults["efficiency_curve"]))
        entry["static_projection"] = dict(pos_defaults["static_projection"])
        out[r["name"]] = entry
    return out


def main():
    rookies = collect_rookies()
    skeleton = build_skeleton(rookies)
    with open(OUTPUT_PATH, "w") as f:
        json.dump(skeleton, f, indent=2)
    n_drafted = sum(1 for r in rookies if r["draft_capital"])
    print(f"Wrote {OUTPUT_PATH}: {len(rookies)} rookies ({n_drafted} drafted, {len(rookies) - n_drafted} UDFA).")


if __name__ == "__main__":
    main()
