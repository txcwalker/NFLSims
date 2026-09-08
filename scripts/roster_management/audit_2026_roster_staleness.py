"""Audits data/dna/preseason_overrides_2026.csv against nfl_data_py's live
2026 seasonal roster feed to catch two kinds of drift in the opposite
direction from add_2026_missing_skill_players.py (which adds players the
pipeline is MISSING): players our sheet still lists who have actually left
their team (departed/cut/retired), and players who moved to a different
team without our roster files being updated.

Context (2026-08-20): after hand-adding Najee Harris/Diggs/Samuel/Allen
because the live feed didn't have them yet, Cam asked the natural follow-up
-- can we also check the other direction, so we're not carrying stale
players who are no longer actually on the team. Confirmed the live feed can
have real quirks (a team-code mismatch, ARI vs AZ, and a position-code
mismatch, Barion Brown/NO tagged "KR" not "WR" in one particular table) that
look like false departures if taken at face value -- this script cross-
checks BOTH active/reserve status AND any-status-on-any-team before
concluding a player is really gone, and hardcodes the one confirmed team-
code fix so it doesn't re-surface as noise every run.

Three buckets reported:
  - MOVED, active elsewhere: real signal, a trade/signing our roster files
    haven't caught up to yet (the mirror image of what
    add_2026_missing_skill_players.py handles for brand-new signings).
  - MOVED, only inactive elsewhere: weaker signal (worth a look, could be
    real or could be a data quirk) -- surfaced separately so it doesn't get
    mixed in with confident matches.
  - NOT FOUND anywhere in the live feed: could be a real departure
    (retired, cut, out of the league) OR the live feed simply hasn't caught
    up to a signing WE already hand-added this same way (check
    add_2026_missing_skill_players.py's NEW_SIGNINGS dict first) OR a name/
    position-code quirk in that one nfl_data_py table -- always verify
    before removing anyone, this script only flags candidates, never edits
    files itself.

Usage: python audit_2026_roster_staleness.py
"""
import csv
import os
import sys

import nfl_data_py as nfl

DNA_DIR = "data/dna"
OVERRIDES_CSV = os.path.join(DNA_DIR, "preseason_overrides_2026.csv")
SKILL_POSITIONS = ["QB", "RB", "WR", "TE"]
ACTIVE_STATUSES = {"ACT", "RES"}

# Confirmed 2026-08-20: nfl.import_seasonal_rosters([2026]) uses "AZ" for
# the Cardinals where every other file/table in this repo (and
# build_2026_rosters_v_0_1_0.py's own DRAFT_TEAM_CODE_FIX) uses "ARI".
# Extend this if another mismatch turns up on a future run.
LIVE_TEAM_CODE_FIX = {"AZ": "ARI"}


def clean_name(name):
    if not isinstance(name, str):
        return ""
    name = name.strip()
    for s in [" Jr.", " Sr.", " III", " II", " IV", " V"]:
        if name.endswith(s):
            name = name[: -len(s)]
    return name.replace(".", "")


def load_live_roster():
    roster = nfl.import_seasonal_rosters([2026])
    roster = roster[roster["position"].isin(SKILL_POSITIONS)]
    roster = roster.dropna(subset=["player_name", "team"])
    roster["team"] = roster["team"].map(lambda t: LIVE_TEAM_CODE_FIX.get(t, t))

    active = roster[roster["status"].isin(ACTIVE_STATUSES)]
    active_by_name = {clean_name(r["player_name"]): r["team"] for _, r in active.iterrows()}

    any_status = {}
    for _, r in roster.iterrows():
        any_status.setdefault(clean_name(r["player_name"]), set()).add(r["team"])

    return active_by_name, any_status


def audit():
    active_by_name, any_status = load_live_roster()

    with open(OVERRIDES_CSV, newline="") as f:
        ours = list(csv.DictReader(f))

    moved_active, moved_inactive, not_found, matched = [], [], [], 0

    for row in ours:
        name, team, pos = row["player_name"], row["team"], row["pos"]
        if name in active_by_name:
            if active_by_name[name] == team:
                matched += 1
            else:
                moved_active.append((name, pos, team, active_by_name[name]))
        elif name in any_status:
            teams = any_status[name]
            if team in teams:
                matched += 1
            else:
                moved_inactive.append((name, pos, team, sorted(teams)))
        else:
            not_found.append((name, pos, team))

    print(f"Audited {len(ours)} rows in {OVERRIDES_CSV}.")
    print(f"  Matches live roster (active or otherwise, same team): {matched}")

    print(f"\n  MOVED, active on a different team ({len(moved_active)}):")
    for name, pos, our_team, their_team in moved_active:
        print(f"    {name} ({pos}): ours={our_team} -> live={their_team}")

    print(f"\n  MOVED, only inactive status on a different team ({len(moved_inactive)}):")
    for name, pos, our_team, their_teams in moved_inactive:
        print(f"    {name} ({pos}): ours={our_team} -> live={their_teams}")

    print(f"\n  NOT FOUND anywhere in the live feed ({len(not_found)}) -- "
          f"verify each before removing (could be real departures, could be "
          f"a signing we already hand-added that the feed hasn't caught up "
          f"to, could be a name/position-code quirk):")
    for name, pos, team in not_found:
        print(f"    {name} ({pos}, {team})")


if __name__ == "__main__":
    audit()
