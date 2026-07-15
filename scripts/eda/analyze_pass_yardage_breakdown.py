"""
clock_physics_v020 audit, Round 8: yards-per-completion diagnostic.

Cam noticed sim yards/completion running ~11% short (9.70 vs. real 10.93) even
after completion rate landed almost exactly on target (Round 7). This script
isolates WHERE the shortfall lives: air yards (depth of target) vs. YAC
(after the catch), broken down by zone (primary/redzone/goalline) and by
depth-of-target bucket (screen/standard/deep, same thresholds as
air_yards_v_0_1_1's metadata: screen_max=0, deep_min=20).

Real side: pulls the same 2021-2025 real PBP nflfastR data used by
run_historical_eda.py and computes mean air_yards / yards_after_catch /
yards_gained on completed passes, by zone and by depth bucket.

Sim side: drives NFLGameEngine directly (not via BatchSimulator, which doesn't
expose per-play air_yards/yac) across a sample of real Week 1-4 2025 matchups,
reading the additive last_play_* hook fields added to game_engine.py this
round after every simulate_play_step() call.

Output: printed comparison table + JSON dump to
docs/audit/v_0_2_0_audit/pass_yardage_breakdown.json (diagnostic only, not
wired into the main audit's sim_post_cal_metrics.json).
"""
import sys
import os
import json
import numpy as np
import pandas as pd

sys.path.append(os.getcwd())

from src.nfl_sim.game_engine import NFLGameEngine
from src.nfl_sim.batch import BatchSimulator

SCREEN_MAX = 0
DEEP_MIN = 20


def zone_of(yardline_100):
    return np.where(yardline_100 <= 5, 'goalline', np.where(yardline_100 <= 20, 'redzone', 'primary'))


def depth_bucket_of(air_yards):
    return np.where(air_yards <= SCREEN_MAX, 'screen', np.where(air_yards >= DEEP_MIN, 'deep', 'standard'))


def real_breakdown():
    import nfl_data_py as nfl
    print("Loading real 2021-2025 PBP for pass-yardage breakdown...")
    df_raw = nfl.import_pbp_data(list(range(2021, 2026)))
    if 'season_type' in df_raw.columns:
        df = df_raw[df_raw['season_type'] == 'REG'].copy()
    else:
        df = df_raw[df_raw['game_type'] == 'REG'].copy()

    df = df[(df['play_type'] == 'pass') & (df['complete_pass'] == 1)].copy()
    df = df.dropna(subset=['air_yards', 'yards_after_catch', 'yardline_100'])

    df['zone'] = zone_of(df['yardline_100'].values)
    df['depth_bucket'] = depth_bucket_of(df['air_yards'].values)

    overall = {
        'n': int(len(df)),
        'air_yards_mean': float(df['air_yards'].mean()),
        'yac_mean': float(df['yards_after_catch'].mean()),
        'yards_gained_mean': float(df['yards_gained'].mean()),
    }

    by_zone = {}
    for zone, g in df.groupby('zone'):
        by_zone[zone] = {
            'n': int(len(g)),
            'air_yards_mean': float(g['air_yards'].mean()),
            'yac_mean': float(g['yards_after_catch'].mean()),
            'yards_gained_mean': float(g['yards_gained'].mean()),
            'yardline_100_mean': float(g['yardline_100'].mean()),
            'yardline_100_median': float(g['yardline_100'].median()),
            'room_to_goal_after_air_mean': float((g['yardline_100'] - g['air_yards']).mean()),
        }

    by_depth = {}
    for bucket, g in df.groupby('depth_bucket'):
        by_depth[bucket] = {
            'n': int(len(g)),
            'air_yards_mean': float(g['air_yards'].mean()),
            'yac_mean': float(g['yards_after_catch'].mean()),
            'yards_gained_mean': float(g['yards_gained'].mean()),
        }

    return {'overall': overall, 'by_zone': by_zone, 'by_depth': by_depth}


def sim_breakdown(n_per_matchup=300, max_matchups=16):
    sched_df = pd.read_csv("data/external/schedule_2025.csv")
    week_games = sched_df[(sched_df["week"].isin([1, 2, 3, 4])) & (sched_df["game_type"] == "REG")]
    week_games = week_games.head(max_matchups)

    records = []

    for idx, row in week_games.iterrows():
        away, home = row["away_team"], row["home_team"]
        print(f"Simulating {away} @ {home} (N={n_per_matchup}) for pass-yardage sample...")
        batch = BatchSimulator(away, home, year=2025)
        engine = NFLGameEngine(
            away, home, year=2025,
            dna=batch.dna, team_coaches=batch.team_coaches,
            rosters=batch.rosters, trench_tiers=batch.trench_tiers,
            N=n_per_matchup
        )

        while not np.all(engine.game_over):
            engine.simulate_play_step()
            mask = engine.last_play_is_complete_pass
            if not np.any(mask):
                continue
            ay = engine.last_play_air_yards[mask]
            yac = engine.last_play_yac[mask]
            gain = engine.last_play_gain[mask]
            pre_yd = engine.last_play_pre_yardline_100[mask]
            for a, y, g, yd in zip(ay, yac, gain, pre_yd):
                records.append((int(a), int(y), int(g), int(yd)))

    df = pd.DataFrame(records, columns=['air_yards', 'yac', 'yards_gained', 'yardline_100'])
    df['zone'] = zone_of(df['yardline_100'].values)
    df['depth_bucket'] = depth_bucket_of(df['air_yards'].values)

    overall = {
        'n': int(len(df)),
        'air_yards_mean': float(df['air_yards'].mean()),
        'yac_mean': float(df['yac'].mean()),
        'yards_gained_mean': float(df['yards_gained'].mean()),
    }

    by_zone = {}
    for zone, g in df.groupby('zone'):
        by_zone[zone] = {
            'n': int(len(g)),
            'air_yards_mean': float(g['air_yards'].mean()),
            'yac_mean': float(g['yac'].mean()),
            'yards_gained_mean': float(g['yards_gained'].mean()),
            'yardline_100_mean': float(g['yardline_100'].mean()),
            'yardline_100_median': float(g['yardline_100'].median()),
            'room_to_goal_after_air_mean': float((g['yardline_100'] - g['air_yards']).mean()),
        }

    by_depth = {}
    for bucket, g in df.groupby('depth_bucket'):
        by_depth[bucket] = {
            'n': int(len(g)),
            'air_yards_mean': float(g['air_yards'].mean()),
            'yac_mean': float(g['yac'].mean()),
            'yards_gained_mean': float(g['yards_gained'].mean()),
        }

    return {'overall': overall, 'by_zone': by_zone, 'by_depth': by_depth}


def print_comparison(real, sim):
    def pct_delta(s, r):
        return (s - r) / r * 100 if r else float('nan')

    print("\n" + "=" * 70)
    print("OVERALL (completed passes)")
    print("=" * 70)
    for metric in ['air_yards_mean', 'yac_mean', 'yards_gained_mean']:
        r, s = real['overall'][metric], sim['overall'][metric]
        print(f"{metric:22s}  sim={s:7.2f}  real={r:7.2f}  delta={pct_delta(s, r):+.1f}%")
    print(f"n: sim={sim['overall']['n']}  real={real['overall']['n']}")

    for section in ['by_zone', 'by_depth']:
        print("\n" + "=" * 70)
        print(section.upper())
        print("=" * 70)
        keys = sorted(set(real[section].keys()) | set(sim[section].keys()))
        for k in keys:
            r = real[section].get(k)
            s = sim[section].get(k)
            if not r or not s:
                print(f"[{k}] missing in one side (real={bool(r)}, sim={bool(s)})")
                continue
            print(f"[{k}]  n_sim={s['n']}  n_real={r['n']}")
            metrics = ['air_yards_mean', 'yac_mean', 'yards_gained_mean']
            if section == 'by_zone':
                metrics += ['yardline_100_mean', 'yardline_100_median', 'room_to_goal_after_air_mean']
            for metric in metrics:
                print(f"   {metric:28s}  sim={s[metric]:7.2f}  real={r[metric]:7.2f}  delta={pct_delta(s[metric], r[metric]):+.1f}%")


if __name__ == "__main__":
    real = real_breakdown()
    sim = sim_breakdown()

    print_comparison(real, sim)

    os.makedirs("docs/audit/v_0_2_0_audit", exist_ok=True)
    with open("docs/audit/v_0_2_0_audit/pass_yardage_breakdown.json", "w") as f:
        json.dump({'real': real, 'sim': sim}, f, indent=2)
    print("\nSaved docs/audit/v_0_2_0_audit/pass_yardage_breakdown.json")
