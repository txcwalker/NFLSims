"""Merges R/scripts/build_dna_registry_v_0_1_0.R's staged legacy trench
fields (sack_rate_allowed, avg_air_yards, def_pressure_rate, def_sack_rate,
blitz_rate, avg_ay_allowed) into data/dna/trench_dna.json per season/team,
without disturbing the composite fields (run_block_off_z, pass_def_z, etc.)
a separate v0.4.0 pipeline already wrote there for 2025+.

Run only after the R script has produced data/dna/_trench_dna_legacy_staging.json.
"""
import json
import os

DNA_DIR = "data/dna"
STAGING_PATH = os.path.join(DNA_DIR, "_trench_dna_legacy_staging.json")
TARGET_PATH = os.path.join(DNA_DIR, "trench_dna.json")

LEGACY_FIELDS = [
    "sack_rate_allowed", "avg_air_yards", "def_pressure_rate",
    "def_sack_rate", "blitz_rate", "avg_ay_allowed",
    "off_pass_block_win_rate", "times_to_pressure_sec",
]


def main():
    with open(STAGING_PATH, "r") as f:
        staged = json.load(f)
    with open(TARGET_PATH, "r") as f:
        target = json.load(f)

    seasons_touched = []
    for season, teams in staged.items():
        if season == "_metadata":
            continue
        target.setdefault(season, {})
        seasons_touched.append(season)
        for team, legacy_fields in teams.items():
            existing = target[season].setdefault(team, {})
            for field in LEGACY_FIELDS:
                if field in legacy_fields:
                    existing[field] = legacy_fields[field]

    target["_metadata"]["merged_legacy_fields"] = (
        f"Legacy fields ({', '.join(LEGACY_FIELDS)}) refreshed for seasons "
        f"{min(seasons_touched)}-{max(seasons_touched)} via "
        f"R/scripts/build_dna_registry_v_0_1_0.R + this merge script, "
        f"2026-07-22. Composite fields (run_block_off_z etc.) from the "
        f"separate v0.4.0 pipeline were not touched."
    )

    with open(TARGET_PATH, "w") as f:
        json.dump(target, f, indent=2)

    # Verify: spot-check a season/team that should now have both legacy and
    # composite fields present before trusting the merge.
    with open(TARGET_PATH, "r") as f:
        written = json.load(f)
    check_season = max(seasons_touched)
    check = written[check_season]["KC"]
    has_legacy = all(field in check for field in ["def_pressure_rate", "def_sack_rate", "sack_rate_allowed"])
    has_composite = "run_block_off_z" in check
    print(f"Merged seasons: {seasons_touched}")
    print(f"Verify {check_season}/KC -- legacy fields present: {has_legacy}, composite fields present: {has_composite}")
    if not (has_legacy and has_composite):
        raise SystemExit("Merge verification failed -- NOT deleting staging file, investigate before rerunning.")

    os.remove(STAGING_PATH)
    print(f"Verified OK. Removed staging file: {STAGING_PATH}")


if __name__ == "__main__":
    main()
