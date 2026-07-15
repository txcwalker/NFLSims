"""
Prints a human-readable, football-style play-by-play narrative for a single
simulated game (N=1) — "Mahomes complete to Kelce for 12 yards" rather than
the raw state-diff log in print_play_by_play_v020.py (which is built for
clock-mechanic verification, not readability). Uses the same additive
last_play_* hooks on NFLGameEngine (game_engine.py), including the
target/rusher name fields added for this purpose.
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
    if yardline_100 == 50:
        return "midfield"
    return f"{possession_team} {100 - yardline_100}"


def describe_play(engine, poss_team, other_team, pre_down, pre_dist, pre_yl, pre_score_away, pre_score_home,
                   post_score_away, post_score_home, away_team, home_team):
    is_pass = bool(engine.last_play_is_pass[0])
    is_run = bool(engine.last_play_is_run[0])
    is_sack = bool(engine.last_play_is_sack[0])
    is_scramble = bool(engine.last_play_is_scramble[0])
    is_complete = bool(engine.last_play_is_complete_pass[0])
    gain = int(engine.last_play_gain[0])
    target = engine.last_play_target_name[0]
    rusher = engine.last_play_rusher_name[0]
    scored = (post_score_away != pre_score_away) or (post_score_home != pre_score_home)

    if is_sack:
        return f"{poss_team} QB sacked for a loss of {abs(gain)}"
    if is_scramble:
        return f"{poss_team} QB scrambles for {gain} yards"
    if is_pass:
        who = f" to {target}" if target and target != "Unknown" else ""
        if is_complete:
            verb = "TOUCHDOWN pass" if scored else "Complete"
            return f"{poss_team} pass{who} - {verb}, {gain} yards"
        return f"{poss_team} pass{who} - INCOMPLETE"
    if is_run:
        who = f" ({rusher})" if rusher and rusher != "Unknown" else ""
        verb = "TOUCHDOWN run" if scored else "Run"
        return f"{poss_team} {verb}{who} for {gain} yards"
    return None  # non-scrimmage step (kickoff, punt, FG, kneel, etc. — described via tags below)


def print_game(away_team="DAL", home_team="PHI", year=2025, out_path=None):
    engine = NFLGameEngine(away_team, home_team, year=year, N=1)

    lines = []
    lines.append(f"{away_team} @ {home_team} vs {year} (simulated)")
    lines.append("=" * 80)

    play_num = 0
    quarter_printed = 0
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

        poss_team = away_team if pre_poss_away else home_team
        other_team = home_team if pre_poss_away else away_team

        if pre_quarter != quarter_printed:
            lines.append(f"\n--- Quarter {pre_quarter} ---")
            quarter_printed = pre_quarter

        engine.simulate_play_step()
        play_num += 1

        post_score_away = int(engine.score_away[0])
        post_score_home = int(engine.score_home[0])
        post_poss_away = bool(engine.possession_is_away[0])

        desc = None
        if pre_needs_ko:
            desc = "Kickoff"
        elif not bool(engine.last_play_scrimmage_mask[0]):
            # Punt / field goal / kneel / spike / other non-scrimmage resolution —
            # not covered by the pass/run hooks, infer from score/possession change.
            if post_score_away != pre_score_away or post_score_home != pre_score_home:
                desc = "Score (special teams / kick)"
            elif post_poss_away != pre_poss_away:
                desc = "Punt / turnover on downs"
            else:
                desc = "Clock / administrative"
        else:
            desc = describe_play(engine, poss_team, other_team, pre_down, pre_dist, pre_yl,
                                  pre_score_away, pre_score_home, post_score_away, post_score_home,
                                  away_team, home_team)
            if desc is None:
                desc = "Play"

        situation = f"{poss_team} {pre_down} & {pre_dist} at {describe_yardline(pre_yl, poss_team, other_team)}"
        score_str = f"{away_team} {post_score_away} - {home_team} {post_score_home}"
        lines.append(f"Q{pre_quarter} {fmt_clock(pre_time)}  {situation:38s}  {desc}   [{score_str}]")

        if play_num > 400:
            lines.append("SAFETY STOP: exceeded 400 play-step calls, aborting.")
            break

    total_snaps = int(engine.play_count[0] + engine.punts_run[0] + engine.fg_attempts_away[0]
                       + engine.fg_attempts_home[0] + engine.kickoffs_run[0] + engine.presnap_penalty_snaps[0])
    lines.append("\n" + "=" * 80)
    lines.append(f"FINAL: {away_team} {int(engine.score_away[0])} - {home_team} {int(engine.score_home[0])}")
    lines.append(f"Engine steps printed above: {play_num}  |  Offensive snaps (play_count): {int(engine.play_count[0])}"
                 f"  |  Total snaps (off+ST+presnap-penalty): {total_snaps}")

    output = "\n".join(lines)
    print(output)

    if out_path:
        os.makedirs(os.path.dirname(out_path), exist_ok=True)
        with open(out_path, 'w', encoding='utf-8') as f:
            f.write(output)
        print(f"\nSaved to {out_path}")


if __name__ == "__main__":
    away = sys.argv[1] if len(sys.argv) > 1 else "DAL"
    home = sys.argv[2] if len(sys.argv) > 2 else "PHI"
    print_game(away_team=away, home_team=home, year=2025)
