"""Regenerates every DNA/roster file that's gitignored for repo-size
reasons (see .gitignore's "regeneratable data" section) rather than
version-controlled -- run this after a fresh clone, or any time you want a
from-scratch rebuild instead of trusting what's on disk.

Deliberately does NOT touch trench_dna.json -- its composite fields
(run_block_off_z etc., built by scripts/eda/build_trench_dna_composites*.py)
depend on docs/eda_outputs/*.csv files that are themselves not fully
regeneratable by a verified, run-today script chain. trench_dna.json stays
version-controlled precisely because of that gap -- see WORKLOG.md's
2026-07-22 entry.

Order matters: qb/rb/wr/te/skill_dna.json before the 2026 roster shell
(which joins against them); coach_dna.json has no dependents here.

Usage: python regenerate_dna_v_0_1_0.py
"""
import subprocess
import sys
import os

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))

STEPS = [
    ("Python: qb/rb/wr/te/skill_dna.json (2021-2025)",
     [sys.executable, "scripts/roster_management/build_full_name_dna.py"]),
    ("R: coach_dna.json (2015-2025)",
     ["Rscript", "R/scripts/build_dna_registry_v_0_1_0.R"]),
    ("Python: 2026 roster shell (data/current_rosters/*_traits_2026.json)",
     [sys.executable, "scripts/roster_management/build_2026_rosters_v_0_1_0.py"]),
]


def main():
    for label, cmd in STEPS:
        print(f"\n=== {label} ===")
        result = subprocess.run(cmd, cwd=REPO_ROOT)
        if result.returncode != 0:
            raise SystemExit(f"FAILED: {label} (exit {result.returncode}) -- stopping, later steps may depend on this one.")
    print("\n=== All gitignored DNA/roster files regenerated. ===")
    print("NOTE: trench_dna.json was NOT touched -- it stays version-controlled, see this script's docstring.")


if __name__ == "__main__":
    main()
