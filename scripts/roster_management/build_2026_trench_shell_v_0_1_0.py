"""Establishes data/dna/trench_dna.json's "2026" season entry -- the team-
defense equivalent of build_2026_rosters_v_0_1_0.py's player-side shell.

Carries forward each team's real 2025 values as the 2026 starting point
(all fields, including the v0.4.0 composite z-scores like run_block_off_z --
those aren't rebuilt here, see the scoping note below) and freezes a
`preseason_projection` sub-dict containing just the fields
rolling_stats_v_0_1_0.TEAM_RATE_FIELDS actually tracks a live in-season
signal for (def_pressure_rate, def_sack_rate, sack_rate_allowed) -- the
stable baseline refresh_weekly_dna_v_0_1_0.py's refresh_team_defense()
blends against all season, same pattern as players'
preseason_projection.

Scoping note: the composite z-score fields (run_block_off_z, run_def_z,
pass_block_off_z, pass_def_z) are NOT part of the weekly blend -- they're
z-scored composites built by scripts/eda/build_trench_dna_composites*.py
from multiple underlying metrics, normalized against the full league
distribution for that season. Recomputing them on a rolling weekly basis
would need to re-run that whole composite-scoring methodology against a
partial-season sample each week, which is materially more work than a
simple rolling average and wasn't in Phase 6's scope. They stay frozen at
each team's real 2025 value for the whole 2026 season unless a future
phase builds that out.
"""
import json
import os

DNA_DIR = "data/dna"
TRENCH_PATH = os.path.join(DNA_DIR, "trench_dna.json")
LIVE_RATE_FIELDS = ["def_pressure_rate", "def_sack_rate", "sack_rate_allowed"]


def main():
    trench = json.load(open(TRENCH_PATH))
    prior_season = trench["2025"]

    trench["2026"] = {}
    for team, fields in prior_season.items():
        entry = dict(fields)  # carries forward composites unchanged, see scoping note
        entry["preseason_projection"] = {f: fields[f] for f in LIVE_RATE_FIELDS if f in fields}
        trench["2026"][team] = entry

    trench["_metadata"]["2026_shell_note"] = (
        "2026 entry added 2026-07-22 by build_2026_trench_shell_v_0_1_0.py: "
        "carries forward each team's real 2025 values as the starting point "
        "(including composite z-scores, which are NOT weekly-refreshed -- see "
        "script docstring). `preseason_projection` per team is the frozen "
        "baseline refresh_weekly_dna_v_0_1_0.py's refresh_team_defense() "
        "blends real in-season data against."
    )

    with open(TRENCH_PATH, "w") as f:
        json.dump(trench, f, indent=2)
    print(f"Wrote trench_dna.json['2026'] for {len(trench['2026'])} teams.")


if __name__ == "__main__":
    main()
