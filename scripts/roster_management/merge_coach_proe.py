"""Merges each coach's off_proe value into data/dna/coach_dna.json's "proe"
field.

Regression fix (2026-07-22, found while verifying Phase 7e): coach_dna.json
originally had "proe" merged in from the now-retired data/dna/
coordinator_atlas.json on 2026-07-16 (see docs/sims/inputs/README.md's bug
#3). But R/scripts/build_dna_registry_v_0_1_0.R's build_coach_dna() never
produced a "proe" field itself -- that was always a separate, one-time
merge step. When Phase 1 of this session's 2026 rollover reran the R script
to extend coach_dna.json through 2025, it silently wiped every coach's
"proe" field back out, since a full rebuild only writes what
build_coach_dna() itself computes. Not caught until Phase 7e actually
exercised get_coach_proe() and found every team returning 0.0 instead of
real values (e.g. Andy Reid should be ~+6.4, not 0.0).

coordinator_atlas.json itself is deleted from disk (deliberately retired,
per the above) but was never actually committed as deleted -- still fully
recoverable from git history. This script pulls it from a pinned commit SHA
(stable regardless of what HEAD becomes later) rather than requiring the
file to exist on disk or restoring it (which would undo the deliberate
retirement). Safe to rerun any time coach_dna.json gets rebuilt from
scratch in the future.
"""
import json
import subprocess

DNA_DIR = "data/dna"
COACH_DNA_PATH = f"{DNA_DIR}/coach_dna.json"
# Commit confirmed to still have coordinator_atlas.json's off_proe data,
# pinned so this script keeps working even as HEAD moves forward.
SOURCE_COMMIT = "17e8a69871c115040586b6298b5c6dd6a934562f"
SOURCE_PATH = "data/dna/coordinator_atlas.json"


def load_off_proe():
    raw = subprocess.run(
        ["git", "show", f"{SOURCE_COMMIT}:{SOURCE_PATH}"],
        capture_output=True, text=True, check=True,
    ).stdout
    return json.loads(raw)["off_proe"]


def main():
    off_proe = load_off_proe()
    coach_dna = json.load(open(COACH_DNA_PATH))

    updated, missing = 0, []
    for name in coach_dna:
        if name == "_metadata":
            continue
        if name in off_proe:
            coach_dna[name]["proe"] = off_proe[name]
            updated += 1
        else:
            missing.append(name)

    with open(COACH_DNA_PATH, "w") as f:
        json.dump(coach_dna, f, indent=2)

    print(f"Merged proe for {updated} coaches.")
    if missing:
        print(f"No off_proe data for {len(missing)} coaches (no field added, falls back to league-average 0.0 at lookup time): {missing}")


if __name__ == "__main__":
    main()
