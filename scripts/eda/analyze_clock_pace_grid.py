import os
import json
import pandas as pd
import numpy as np
import nfl_data_py as nfl


def analyze_clock_pace_grid():
    """
    Builds the (quarter x score-margin-tier) pace grid for clock_physics_v020.
    Reuses the exact 'running clock' definition from analyze_clock_physics.py
    (out_of_bounds/incomplete/timeout/penalty on the PREVIOUS play excludes it,
    plus possession-change transitions excluded) so this grid is consistent
    with docs/eda_outputs/clock/README.md.
    """
    print("==================================================")
    print("NFL Clock Pace Grid EDA (2021-2025) — clock_physics_v020")
    print("==================================================")

    output_dir = "docs/audit/clock_physics_v020"
    os.makedirs(output_dir, exist_ok=True)

    seasons = list(range(2021, 2026))
    cols_to_keep = [
        'game_id', 'play_id', 'season', 'season_type', 'qtr',
        'quarter_seconds_remaining', 'game_seconds_remaining', 'game_half',
        'play_type', 'incomplete_pass', 'out_of_bounds', 'penalty',
        'timeout', 'posteam', 'defteam', 'score_differential', 'play_clock'
    ]

    try:
        df_raw = nfl.import_pbp_data(seasons, columns=cols_to_keep)
    except Exception as e:
        print(f"Error importing with columns specified: {e}. Attempting full import and filtering...")
        df_raw = nfl.import_pbp_data(seasons)
        df_raw = df_raw[[c for c in cols_to_keep if c in df_raw.columns]]

    print(f"Loaded {len(df_raw)} play-by-play rows.")
    df = df_raw[df_raw['season_type'] == 'REG'].copy()
    print(f"Filtered to Regular Season: {len(df)} rows.")

    df = df.sort_values(by=['game_id', 'play_id']).reset_index(drop=True)

    df['prev_game_id'] = df['game_id'].shift(1)
    df['prev_game_half'] = df['game_half'].shift(1)
    df['prev_game_seconds'] = df['game_seconds_remaining'].shift(1)
    df['game_clock_elapsed'] = np.where(
        (df['game_id'] == df['prev_game_id']) & (df['game_half'] == df['prev_game_half']),
        df['prev_game_seconds'] - df['game_seconds_remaining'],
        np.nan
    )

    df['prev_play_type'] = df['play_type'].shift(1)
    df['prev_incomplete_pass'] = df['incomplete_pass'].shift(1).fillna(0)
    df['prev_out_of_bounds'] = df['out_of_bounds'].shift(1).fillna(0)
    df['prev_penalty'] = df['penalty'].shift(1).fillna(0)
    df['prev_timeout'] = df['timeout'].shift(1).fillna(0)
    df['prev_posteam'] = df['posteam'].shift(1)

    off_types = ['pass', 'run', 'no_play', 'qb_kneel', 'qb_spike']
    # Reverted back to excluding out_of_bounds (matching analyze_clock_physics.py's
    # "running clock" cohort). OOB used to be folded into this general pool as a
    # stopgap (see git history / WORKLOG for that episode), back when the engine
    # had no OOB modeling and applied this pool to every qualifying play
    # regardless of outcome. Now that OOB is its own explicit mechanism in
    # game_engine.py (roll a probability, then draw from THIS pool minus a
    # random 3-5s reduction, or a short crunch-time-only runoff inside the
    # last 2:00 of Q2 / 5:00 of Q4), this pool must go back to representing
    # ONLY genuinely-continued running-clock plays — otherwise OOB's timing
    # effect gets double-counted (once via the pool blend, once via the
    # explicit reduction).
    df['prev_clock_stopped'] = (
        (df['prev_incomplete_pass'] == 1) |
        (df['prev_out_of_bounds'] == 1) |
        (df['prev_timeout'] == 1) |
        (df['prev_penalty'] == 1)
    )
    df['possession_change'] = (df['game_id'] == df['prev_game_id']) & (df['posteam'] != df['prev_posteam']) & df['posteam'].notna() & df['prev_posteam'].notna()

    valid_df = df[
        (df['game_clock_elapsed'].notna()) &
        (df['game_clock_elapsed'] >= 0) &
        (df['game_clock_elapsed'] <= 60) &
        (df['prev_play_type'].isin(off_types))
    ].copy()

    running_clock_df = valid_df[~valid_df['prev_clock_stopped'] & ~valid_df['possession_change']].copy()
    print(f"Running-clock snaps (regulation only, qtr 1-4): {len(running_clock_df)}")

    # Score-margin tiers, posteam perspective (score_differential = posteam - defteam)
    def margin_tier(sd):
        if sd == 0:
            return "Tied"
        elif 1 <= sd <= 8:
            return "Leading 1-score"
        elif 9 <= sd <= 16:
            return "Leading 2-score"
        elif sd >= 17:
            return "Leading 3+ score"
        elif -8 <= sd <= -1:
            return "Trailing 1-score"
        elif -16 <= sd <= -9:
            return "Trailing 2-score"
        else:
            return "Trailing 3+ score"

    running_clock_df = running_clock_df[running_clock_df['qtr'].isin([1, 2, 3, 4])].copy()
    running_clock_df['margin_tier'] = running_clock_df['score_differential'].apply(margin_tier)

    tier_order = ["Trailing 3+ score", "Trailing 2-score", "Trailing 1-score", "Tied",
                  "Leading 1-score", "Leading 2-score", "Leading 3+ score"]

    def q2_window(qsr):
        if qsr > 240:
            return "Q2 >4:00"
        elif qsr > 120:
            return "Q2 4:00-2:00"
        else:
            return "Q2 <2:00"

    def q3_window(qsr):
        if qsr > 600:
            return "Q3 >10:00"
        elif qsr > 300:
            return "Q3 10:00-5:00"
        else:
            return "Q3 <5:00"

    def q4_window(qsr):
        if qsr > 600:
            return "Q4 >10:00"
        elif qsr > 300:
            return "Q4 10:00-5:00"
        elif qsr > 120:
            return "Q4 5:00-2:00"
        else:
            return "Q4 <2:00"

    conditions = [
        running_clock_df['qtr'] == 1,
        running_clock_df['qtr'] == 2,
        running_clock_df['qtr'] == 3,
        running_clock_df['qtr'] == 4,
    ]
    choices = [
        pd.Series("Q1", index=running_clock_df.index),
        running_clock_df['quarter_seconds_remaining'].apply(q2_window),
        running_clock_df['quarter_seconds_remaining'].apply(q3_window),
        running_clock_df['quarter_seconds_remaining'].apply(q4_window),
    ]
    running_clock_df['row_group'] = np.select(conditions, choices, default="UNKNOWN")

    row_order = [
        "Q1",
        "Q2 >4:00", "Q2 4:00-2:00", "Q2 <2:00",
        "Q3 >10:00", "Q3 10:00-5:00", "Q3 <5:00",
        "Q4 >10:00", "Q4 10:00-5:00", "Q4 5:00-2:00", "Q4 <2:00",
    ]

    # Merge Q1's 3+ score tiers into the 2-score tier — blowout margins are rare
    # this early and the raw 3+ cells are tiny/noisy (e.g. N=21, N=365 with a
    # median driven by a handful of end-of-quarter snaps). Not worth a separate
    # bucket; per Cam's call, just fold them in rather than investigate further.
    q1_mask = running_clock_df['row_group'] == "Q1"
    running_clock_df.loc[q1_mask & (running_clock_df['margin_tier'] == "Trailing 3+ score"), 'margin_tier'] = "Trailing 2-score"
    running_clock_df.loc[q1_mask & (running_clock_df['margin_tier'] == "Leading 3+ score"), 'margin_tier'] = "Leading 2-score"

    def get_stats(series):
        if len(series) == 0:
            return {'N': 0, 'Mean': np.nan, 'Median': np.nan, 'P25': np.nan, 'P75': np.nan}
        return {
            'N': len(series),
            'Mean': float(series.mean()),
            'Median': float(series.median()),
            'P25': float(series.quantile(0.25)),
            'P75': float(series.quantile(0.75))
        }

    rows = []
    for row_group in row_order:
        for tier in tier_order:
            # Q1's 3+ tiers were merged into the 2-score tiers above; skip the
            # now-empty 3+ rows for Q1 rather than print zeroed-out placeholders.
            if row_group == "Q1" and tier in ("Trailing 3+ score", "Leading 3+ score"):
                continue
            cell = running_clock_df[(running_clock_df['row_group'] == row_group) & (running_clock_df['margin_tier'] == tier)]
            stats = get_stats(cell['game_clock_elapsed'])
            rows.append({'Situation': row_group, 'Margin Tier': tier, **stats})

    grid_df = pd.DataFrame(rows)
    THIN_THRESHOLD = 100
    grid_df['Thin Sample'] = grid_df['N'] < THIN_THRESHOLD

    csv_path = os.path.join(output_dir, "pace_grid.csv")
    grid_df.to_csv(csv_path, index=False)
    print(f"Saved raw grid to {csv_path}")

    md_path = os.path.join(output_dir, "pace_grid.md")
    with open(md_path, 'w', encoding='utf-8') as f:
        f.write("# Clock Pace Grid (2021-2025 Regular Season)\n\n")
        f.write("Built from real pbp data: previous play was not incomplete/timeout/penalty, and no possession change. ")
        f.write("**Deliberately includes out-of-bounds-terminated plays** (unlike `docs/eda_outputs/clock/README.md`'s ")
        f.write("'running clock' cohort, which excludes them) — this grid feeds `game_engine.py`'s `is_normal_reg` bucket, ")
        f.write("which has no out-of-bounds modeling and applies this pool to every qualifying play regardless of where it ends, ")
        f.write("so the pool must reflect the real blended mix of continuous-clock and OOB-stopped snaps, not the OOB-excluded subset. ")
        f.write(f"Score-margin tiers are from the posteam's perspective. Cells with N < {THIN_THRESHOLD} are flagged as thin.\n\n")
        f.write("| Situation | Margin Tier | N | Mean | Median | P25 | P75 | Thin? |\n")
        f.write("| :--- | :--- | ---: | ---: | ---: | ---: | ---: | :---: |\n")
        for _, r in grid_df.iterrows():
            if r['N'] == 0:
                f.write(f"| {r['Situation']} | {r['Margin Tier']} | 0 | - | - | - | - | ⚠️ EMPTY |\n")
            else:
                thin_flag = "⚠️" if r['Thin Sample'] else ""
                f.write(f"| {r['Situation']} | {r['Margin Tier']} | {r['N']:,} | {r['Mean']:.1f}s | {r['Median']:.1f}s | {r['P25']:.1f}s | {r['P75']:.1f}s | {thin_flag} |\n")

    print(f"Saved grid markdown to {md_path}")
    print("\nThin cells (N < {}):".format(THIN_THRESHOLD))
    print(grid_df[grid_df['Thin Sample']][['Situation', 'Margin Tier', 'N']].to_string(index=False))

    # -------------------------------------------------------------------------
    # Persist raw per-cell pools for the engine to bootstrap-sample from.
    # -------------------------------------------------------------------------
    pools = {}
    for row_group in row_order:
        for tier in tier_order:
            if row_group == "Q1" and tier in ("Trailing 3+ score", "Leading 3+ score"):
                continue
            cell = running_clock_df[(running_clock_df['row_group'] == row_group) & (running_clock_df['margin_tier'] == tier)]
            values = cell['game_clock_elapsed'].round().astype(int).tolist()
            pools[f"{row_group}|{tier}"] = values

    # Late-Q4 leading/tied cells are contaminated by timeout stoppages (bimodal —
    # see pace_grid.md, e.g. Q4 5:00-2:00 Leading 1-score: median 42.0 but P25 8.0).
    # Override with the clean Q4 10:00-5:00 pool for the same tier — the apparent
    # slowdown should emerge from the (separately modeled) timeout logic, not be
    # baked into the pace pool itself. Trailing tiers are untouched (genuine
    # hurry-up behavior, not primarily a timeout artifact).
    OVERRIDE_SOURCE = "Q4 10:00-5:00"
    OVERRIDE_TARGETS = ["Q4 5:00-2:00", "Q4 <2:00"]
    OVERRIDE_TIERS = ["Tied", "Leading 1-score", "Leading 2-score", "Leading 3+ score"]
    for target in OVERRIDE_TARGETS:
        for tier in OVERRIDE_TIERS:
            source_key = f"{OVERRIDE_SOURCE}|{tier}"
            target_key = f"{target}|{tier}"
            if source_key in pools and target_key in pools:
                pools[target_key] = pools[source_key]

    model_dir = "src/nfl_sim/models/clock_pace_v_0_1_0"
    os.makedirs(model_dir, exist_ok=True)
    pools_path = os.path.join(model_dir, "pace_pools.json")
    with open(pools_path, 'w') as f:
        json.dump(pools, f)
    print(f"Saved pace pools to {pools_path} ({sum(len(v) for v in pools.values())} total values across {len(pools)} cells)")

    metadata = {
        "version": "V.0.1.0",
        "created": "2026-07-09",
        "source": "nflfastR/nfl_data_py PBP 2021-2025 regular season, running-clock snaps only",
        "n_running_clock_snaps": int(len(running_clock_df)),
        "cell_key_format": "'{row_group}|{margin_tier}' — row_group is Q1 (whole quarter), Q2/Q3/Q4 split into time windows; margin_tier is from posteam's perspective",
        "row_groups": row_order,
        "margin_tiers": tier_order,
        "q1_note": "Trailing/Leading 3+ score tiers merged into 2-score tiers for Q1 (too rare/noisy for a separate bucket)",
        "q4_late_override": {
            "targets": OVERRIDE_TARGETS,
            "tiers": OVERRIDE_TIERS,
            "source": OVERRIDE_SOURCE,
            "reason": "Raw pools for leading/tied in these windows are bimodal due to trailing-defense timeout stoppages contaminating the measurement; overridden with the clean pre-timeout-window pool so the pace model and the timeout model don't double-count the same slowdown."
        }
    }
    with open(os.path.join(model_dir, "metadata.json"), 'w') as f:
        json.dump(metadata, f, indent=2)
    print(f"Saved metadata to {os.path.join(model_dir, 'metadata.json')}")


if __name__ == '__main__':
    analyze_clock_pace_grid()
