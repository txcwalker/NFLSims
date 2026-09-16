"""Marks a hand-verified real starting QB on the current_rosters/{TEAM}_traits_{year}.json
file via a new `starter_override: true` flag, read by game_engine.py's
_get_starter_static() before it falls back to its default heuristic (highest
career total_attempts in qb_dna.json).

That heuristic is right for most teams but silently wrong whenever the real
current starter has a shorter track record than a veteran backup on the same
roster -- found 2026-08-12 for 4 teams (a new/rookie-ish starter losing to a
journeyman backup with more career volume). Confirmed via two independent
sources (see WORKLOG.md's 2026-08-12 entry) -- NOT a blanket fix, only these
4 specific teams are known-wrong; do not add a team here without independently
confirming its real 2026 starter first, the same standard used for
team_to_coach_2026.json.

Usage: python set_qb_starter_overrides_v_0_1_0.py <year>
"""
import sys
import os
import json

ROSTERS_DIR = "data/current_rosters"

# team -> confirmed real starting QB, only for teams where the engine's
# default career-total_attempts heuristic picks the wrong player.
STARTER_OVERRIDES = {
    "NYG": "Jaxson Dart",
    "NO": "Tyler Shough",
    "TEN": "Cam Ward",
    "SF": "Brock Purdy",
    # 2026-09-13, Cam confirmed both Michael Penix Jr. (knee) and Tua
    # Tagovailoa have been ruled out for ATL's Week 1 opener -- Cooper Rush
    # (617 career attempts) is the real starter despite having far fewer
    # career attempts than Tua (2,296), which is exactly the heuristic
    # failure mode this file exists to patch.
    "ATL": "Cooper Rush",
}


def apply(year):
    changes = []
    for team, starter_name in STARTER_OVERRIDES.items():
        path = os.path.join(ROSTERS_DIR, f"{team}_traits_{year}.json")
        if not os.path.exists(path):
            print(f"  {team}: {path} not found, skipped")
            continue
        data = json.load(open(path))
        traits = data["traits"].get(starter_name)
        if traits is None:
            print(f"  {team}: {starter_name!r} not found on roster, skipped")
            continue

        # Clear any stale override on a teammate first (e.g. re-running after
        # a roster rebuild that reset the flag on the wrong player).
        for name, t in data["traits"].items():
            if t.get("pos") == "QB" and t.get("starter_override"):
                del t["starter_override"]

        traits["starter_override"] = True
        changes.append((team, starter_name))

        with open(path, "w") as f:
            json.dump(data, f, indent=4)

    if changes:
        print(f"Applied {len(changes)} starter override(s):")
        for team, name in changes:
            print(f"  {team}: {name}")
    else:
        print("No changes applied.")


def main():
    if len(sys.argv) != 2:
        print("Usage: python set_qb_starter_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    apply(int(sys.argv[1]))


if __name__ == "__main__":
    main()
