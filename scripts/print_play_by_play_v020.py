"""
Prints a full play-by-play log of a single simulated game (N=1) using the
current vectorized NFLGameEngine, for manual clock-physics verification
(clock_physics_v020). Unlike scripts/play_by_play_runner.py (which drives the
legacy sequential engine and is interactive), this runs the real engine
non-interactively and dumps every play_step's state diff to a text file.
"""
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.nfl_sim.game_engine import NFLGameEngine


def fmt_clock(seconds):
    seconds = max(0, int(seconds))
    return f"{seconds // 60:02d}:{seconds % 60:02d}"


def describe_yardline(yardline_100, possession_team, other_team):
    if yardline_100 <= 50:
        return f"{other_team} {yardline_100}"
    return f"{possession_team} {100 - yardline_100}"


def print_play_by_play(away_team="KC", home_team="BUF", year=2025, out_path=None):
    engine = NFLGameEngine(away_team, home_team, year=year, N=1)

    lines = []
    lines.append(f"Play-by-play sim check: {away_team} @ {home_team} ({year})")
    lines.append("=" * 90)

    play_num = 0
    while not engine.game_over[0]:
        pre_quarter = int(engine.quarter[0])
        pre_time = int(engine.time_remaining[0])
        pre_down = int(engine.down[0])
        pre_dist = int(engine.distance[0])
        pre_yl = int(engine.yardline_100[0])
        pre_poss_away = bool(engine.possession_is_away[0])
        pre_score_away = int(engine.score_away[0])
        pre_score_home = int(engine.score_home[0])
        pre_needs_ko = bool(engine.needs_kickoff[0])
        pre_clock_stopped = bool(engine.clock_stopped[0])
        pre_to_away = int(engine.timeouts_away[0])
        pre_to_home = int(engine.timeouts_home[0])

        poss_team = away_team if pre_poss_away else home_team
        other_team = home_team if pre_poss_away else away_team

        engine.simulate_play_step()
        play_num += 1

        post_quarter = int(engine.quarter[0])
        post_time = int(engine.time_remaining[0])
        post_down = int(engine.down[0])
        post_dist = int(engine.distance[0])
        post_yl = int(engine.yardline_100[0])
        post_poss_away = bool(engine.possession_is_away[0])
        post_score_away = int(engine.score_away[0])
        post_score_home = int(engine.score_home[0])
        post_needs_ko = bool(engine.needs_kickoff[0])
        post_clock_stopped = bool(engine.clock_stopped[0])
        post_to_away = int(engine.timeouts_away[0])
        post_to_home = int(engine.timeouts_home[0])

        # Elapsed only meaningful within the same quarter; quarter transitions
        # reset time_remaining to 900, so don't report a bogus negative/huge delta.
        if pre_quarter == post_quarter:
            elapsed = pre_time - post_time
        else:
            elapsed = pre_time  # ran out the rest of the old quarter

        tags = []
        if pre_needs_ko:
            tags.append("KICKOFF")
        if pre_down == 4 and not pre_needs_ko:
            tags.append("4TH-DOWN-DECISION")
        if post_score_away != pre_score_away or post_score_home != pre_score_home:
            tags.append("SCORE")
        if post_poss_away != pre_poss_away and not pre_needs_ko:
            tags.append("POSSESSION-CHANGE")
        if post_quarter != pre_quarter:
            tags.append("QUARTER-END")
        if post_to_away != pre_to_away:
            tags.append(f"{away_team}-TIMEOUT")
        if post_to_home != pre_to_home:
            tags.append(f"{home_team}-TIMEOUT")

        tag_str = f" [{', '.join(tags)}]" if tags else ""

        situation = f"{poss_team} {pre_down} & {pre_dist} at {describe_yardline(pre_yl, poss_team, other_team)}"
        line = (
            f"Play {play_num:3d} | Q{pre_quarter} {fmt_clock(pre_time)} -> Q{post_quarter} {fmt_clock(post_time)} "
            f"(elapsed {elapsed:2d}s) | clock_stopped(after)={post_clock_stopped} | {situation}{tag_str}\n"
            f"           Score after: {away_team} {post_score_away} - {home_team} {post_score_home} | "
            f"TOs after: {away_team} {post_to_away} - {home_team} {post_to_home}"
        )
        lines.append(line)

        if play_num > 500:
            lines.append("SAFETY STOP: exceeded 500 play-step calls, aborting.")
            break

    lines.append("=" * 90)
    lines.append(f"FINAL: {away_team} {int(engine.score_away[0])} - {home_team} {int(engine.score_home[0])}")
    lines.append(f"Total play-step calls: {play_num}")
    lines.append(f"Offensive snaps (play_count): {int(engine.play_count[0])}")
    lines.append(f"Total snaps (offense+ST+presnap-penalty): "
                 f"{int(engine.play_count[0] + engine.punts_run[0] + engine.fg_attempts_away[0] + engine.fg_attempts_home[0] + engine.kickoffs_run[0] + engine.presnap_penalty_snaps[0])}")

    output = "\n".join(lines)
    print(output)

    if out_path:
        os.makedirs(os.path.dirname(out_path), exist_ok=True)
        with open(out_path, 'w', encoding='utf-8') as f:
            f.write(output)
        print(f"\nSaved to {out_path}")


if __name__ == "__main__":
    print_play_by_play(
        away_team="KC", home_team="BUF", year=2025,
        out_path="docs/audit/clock_physics_v020/sim_check/kc_at_buf_2025_pbp.txt"
    )
