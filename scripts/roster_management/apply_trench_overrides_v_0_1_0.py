"""Applies hand-edited values from data/dna/trench_overrides_<year>.csv back
into data/dna/trench_dna.json[<year>][<team>] -- writes each non-blank CSV
value into the raw metric field, then RECOMPUTES all 4 composite z-scores
(run_block_off_z, run_def_z, pass_block_off_z, pass_def_z) from the
(possibly-overridden) raw values across all 32 teams, using the identical
z-scoring formula scripts/eda/build_trench_dna_composites.py and
build_trench_dna_pass_composites.py use for real historical seasons: z-score
each metric within the season (i.e. relative to these same 32 teams' values,
not history), sign-corrected so higher is always better, then equal-weighted
average. This is what actually drives game_engine.py's run-gate and Gate 2b
matchup mechanics -- the raw fields alone don't do anything until this step
recomputes the composites.

Blank cells are skipped (no override, leaves the current value alone). Only
touches the 14 raw fields the CSV carries (see export_trench_overrides_v_0_1_0.py)
-- run_block_off_z/run_def_z/pass_block_off_z/pass_def_z are never read from
the CSV directly, only ever recomputed from the raw fields.

Safe to re-run: idempotent, only reports/writes fields that actually changed.

Usage: python apply_trench_overrides_v_0_1_0.py <year>
"""
import sys
import os
import csv
import json
import numpy as np

DNA_PATH = "data/dna/trench_dna.json"
DNA_DIR = "data/dna"

RUN_BLOCK_OFF_METRICS = {"ybc_per_att": 1, "rush_pct_over_expected": 1, "stuff_rate": -1, "avg_time_to_los": -1}
RUN_DEF_METRICS = {"stuff_rate_forced": 1, "aly_allowed": -1, "ybc_allowed_per_att": -1, "rush_pct_over_expected_allowed": -1}
PASS_BLOCK_OFF_METRICS = {"pressured_pct_allowed": -1, "hurry_rate_allowed": -1, "hit_rate_allowed": -1}
PASS_DEF_METRICS = {"pressure_rate_forced": 1, "hurry_rate_forced": 1, "hit_rate_forced": 1}

ALL_FIELDS = (list(RUN_BLOCK_OFF_METRICS) + list(PASS_BLOCK_OFF_METRICS)
              + list(RUN_DEF_METRICS) + list(PASS_DEF_METRICS))


def load_overrides(year):
    path = os.path.join(DNA_DIR, f"trench_overrides_{year}.csv")
    if not os.path.exists(path):
        raise SystemExit(f"{path} not found -- run export_trench_overrides_v_0_1_0.py {year} first.")
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def compute_composite(teams_data, metrics, out_col):
    """z-score each metric across these teams (within this one season's
    population), sign-corrected, then equal-weighted average -- same formula
    build_trench_dna_composites.py/build_trench_dna_pass_composites.py use
    for real historical seasons."""
    teams = list(teams_data.keys())
    z_components = []
    for metric, sign in metrics.items():
        vals = np.array([teams_data[t].get(metric, np.nan) for t in teams], dtype=np.float64)
        mean = np.nanmean(vals)
        std = np.nanstd(vals)
        z = np.where(std > 0, (vals - mean) / max(std, 1e-9), 0.0) * sign
        z_components.append(z)
    composite = np.mean(np.stack(z_components, axis=0), axis=0)
    for i, t in enumerate(teams):
        teams_data[t][out_col] = round(float(composite[i]), 4)


def apply(year):
    rows = load_overrides(year)
    dna = json.load(open(DNA_PATH))
    season_key = str(year)
    season = dna.setdefault(season_key, {})

    changes = []
    for row in rows:
        team = row["team"]
        t = season.setdefault(team, {})
        for field in ALL_FIELDS:
            raw = row.get(field, "")
            if raw is None or str(raw).strip() == "":
                continue
            try:
                value = float(raw)
            except ValueError:
                print(f"  {team}: unparseable value for {field}: {raw!r}, skipped")
                continue
            old = t.get(field)
            if old is not None and abs(old - value) < 1e-9:
                continue
            t[field] = value
            changes.append((team, field, old, value))

    compute_composite(season, RUN_BLOCK_OFF_METRICS, "run_block_off_z")
    compute_composite(season, RUN_DEF_METRICS, "run_def_z")
    compute_composite(season, PASS_BLOCK_OFF_METRICS, "pass_block_off_z")
    compute_composite(season, PASS_DEF_METRICS, "pass_def_z")

    with open(DNA_PATH, "w") as f:
        json.dump(dna, f, indent=4)

    if changes:
        print(f"Applied {len(changes)} field change(s), recomputed all 4 composites for {year}:")
        for team, field, old, new in changes:
            print(f"  {team} {field}: {old} -> {new}")
    else:
        print(f"No raw-field changes -- recomputed all 4 composites for {year} anyway (cheap, always safe).")


def main():
    if len(sys.argv) != 2:
        print("Usage: python apply_trench_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    apply(int(sys.argv[1]))


if __name__ == "__main__":
    main()
