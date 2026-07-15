"""
Audit: does field position always move by exactly the recorded gain on a
"normal" play (no sack/interception/lost-fumble/accepted-penalty), when
possession doesn't change hands?

Triggered by Cam spotting repeated identical down/distance/yardline lines in
a single-game play-by-play (clock_physics_v020 session) with different
players/gains attached — e.g. "PHI 1 & 10 at DAL 29" appearing twice in a
row with two different rushers. This checks whether that's a genuine
engine bug (skipped or duplicated state update) rather than a clock-pacing
question, which is why it's being audited directly instead of guessed at.

Invariant checked, every step, every active lane:
  if last_play_is_normal_play AND possession_is_away unchanged this step:
      post_yardline_100 should equal pre_yardline_100 - last_play_gain
      (pre_yardline_100 is the last_play_pre_yardline_100 hook, captured
      before any mutation this step)

Possession-unchanged filtering deliberately excludes touchdowns and
turnovers-on-downs (both switch possession via _switch_possession, which
resets/flips yardline_100 in a way this simple check doesn't model) — so
any violation on a possession-unchanged normal play is not explained by
scoring or turnovers, and needs a real explanation.
"""
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

import numpy as np
from src.nfl_sim.game_engine import NFLGameEngine


def audit(away_team="DAL", home_team="PHI", year=2025, n_games=200, max_steps=600):
    engine = NFLGameEngine(away_team, home_team, year=year, N=n_games)

    violations = []
    step = 0
    while not np.all(engine.game_over) and step < max_steps:
        pre_poss_away = engine.possession_is_away.copy()
        pre_down = engine.down.copy()
        pre_dist = engine.distance.copy()
        pre_yl_before_call = engine.yardline_100.copy()
        pre_quarter = engine.quarter.copy()

        engine.simulate_play_step()
        step += 1

        post_quarter = engine.quarter

        post_poss_away = engine.possession_is_away
        post_yl = engine.yardline_100
        pre_yl = engine.last_play_pre_yardline_100  # snapshot taken inside this step, before mutation

        check_mask = (
            engine.last_play_is_normal_play
            & (post_poss_away == pre_poss_away)
            & ~engine.game_over  # a lane that just ended (final kneel etc.) isn't a useful check
        )

        if np.any(check_mask):
            expected_yl = pre_yl[check_mask] - engine.last_play_gain[check_mask]
            actual_yl = post_yl[check_mask]
            bad = expected_yl != actual_yl
            if np.any(bad):
                idxs = np.where(check_mask)[0][bad]
                for i in idxs:
                    violations.append({
                        "step": step,
                        "lane": int(i),
                        "pre_quarter": int(pre_quarter[i]),
                        "post_quarter": int(post_quarter[i]),
                        "pre_down": int(pre_down[i]),
                        "pre_dist": int(pre_dist[i]),
                        "pre_yl_hook": int(pre_yl[i]),
                        "pre_yl_before_call": int(pre_yl_before_call[i]),
                        "gain": int(engine.last_play_gain[i]),
                        "expected_yl": int(pre_yl[i] - engine.last_play_gain[i]),
                        "actual_yl": int(post_yl[i]),
                        "is_run": bool(engine.last_play_is_run[i]),
                        "is_pass": bool(engine.last_play_is_pass[i]),
                        "is_complete": bool(engine.last_play_is_complete_pass[i]),
                        "rusher": engine.last_play_rusher_name[i],
                        "target": engine.last_play_target_name[i],
                        "is_sack": bool(engine.last_play_is_sack[i]),
                        "is_scramble": bool(engine.last_play_is_scramble[i]),
                    })

    total_active_lane_steps = step * n_games
    print(f"Ran {n_games} parallel games for up to {step} steps each ({away_team} @ {home_team}, {year}).")
    print(f"Violations found: {len(violations)}")
    if violations:
        quarter_transitions = sum(1 for v in violations if v["pre_quarter"] != v["post_quarter"])
        actual_yl_values = sorted(set(v["actual_yl"] for v in violations))
        print(f"Violations that coincide with a quarter transition (pre_quarter != post_quarter): {quarter_transitions} / {len(violations)}")
        print(f"Distinct actual_yl values across all violations: {actual_yl_values}")
        print("\nFirst 20 violations:")
        for v in violations[:20]:
            print(f"  step={v['step']:4d} lane={v['lane']:3d} Q{v['pre_quarter']}->Q{v['post_quarter']}  "
                  f"pre: {v['pre_down']} & {v['pre_dist']} at yl={v['pre_yl_hook']} (pre_call snapshot={v['pre_yl_before_call']})  "
                  f"gain={v['gain']:4d}  expected_yl={v['expected_yl']:4d}  actual_yl={v['actual_yl']:4d}  "
                  f"is_run={v['is_run']} is_pass={v['is_pass']} is_complete={v['is_complete']} "
                  f"is_sack={v['is_sack']} is_scramble={v['is_scramble']}  "
                  f"rusher={v['rusher']} target={v['target']}")
    else:
        print("No violations — field position always moved by exactly the recorded gain on possession-unchanged normal plays.")

    return violations


if __name__ == "__main__":
    audit()
