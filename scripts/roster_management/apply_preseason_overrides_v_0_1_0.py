"""Applies hand-edited values from data/dna/preseason_overrides_{year}.csv
back into data/current_rosters/{TEAM}_traits_{year}.json -- writes each
non-blank CSV value into BOTH the player's top-level field and their
preseason_projection[field], keeping the two in sync (top-level is what a
week-0 sim reads directly; preseason_projection is the frozen anchor the
weekly refresh tapers away from once real games start).

Only touches the ~14 tunable fields the CSV carries (see
export_preseason_overrides_v_0_1_0.py) -- static career-DNA fields like
pressure_rate/ngs_aggressiveness_index aren't in the CSV at all and are
untouched by this script.

Blank cells are skipped (no override, leaves the current value alone) --
you don't need to fill in every column, only the ones you're hand-tuning.

Safe to re-run: idempotent, only reports/writes fields that actually
changed. Run export_preseason_overrides_v_0_1_0.py first if the CSV doesn't
exist yet or you want a fresh baseline.

Usage: python apply_preseason_overrides_v_0_1_0.py <year>
"""
import sys
import os
import csv
import glob
import json

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS

ROSTERS_DIR = "data/current_rosters"
DNA_DIR = "data/dna"
TUNABLE_FIELDS = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS

# target_share/carry_share are duplicated into traits['splits'][zone][field]
# for RB/WR/TE at roster-build time (see build_2026_rosters_v_0_1_0.py) so
# the game engine can sample redzone/goalline shares independently of the
# overall/primary share. game_engine.py's zone-based lookup always prefers
# splits[zone][field] over the flat field whenever that key is present --
# so without this sync, a hand-tuned override here updates the flat field
# (what every report/leaderboard reads) but the actual sim keeps using
# whatever value was baked into splits at build time, silently ignoring the
# override. Confirmed bug (2026-08-16): a rookie given a real carry_share
# here still got a build-time splits stub of 0.0 in all three zones and
# never touched the ball in any simulated game; a WR's target_share cut
# from ~9% to 6% here still simulated at ~9% because splits.primary never
# moved off its stale pre-override value.
ZONE_SHARE_FIELDS = ("target_share", "carry_share")
ZONES = ("primary", "redzone", "goalline")

# catch_rate has the same silent-desync problem as the share fields (2026-09-06,
# "A1"): game_engine.py's completion model anchors on splits[zone]['catch_rate']
# (falling back to the flat field only if the split is 0/missing), so a
# hand-tuned flat catch_rate in the sheet never reached the sim for any player
# whose zone splits were populated -- which is almost all of them. The sheet
# carries ONE catch_rate column (no rz_/gl_ catch-rate columns yet), so:
#   - splits.primary.catch_rate  <- set to the flat value (primary zone == the
#     overall anchor; the model's own air-yards depth-decay term handles
#     within-zone target-depth variation).
#   - splits.redzone/goalline.catch_rate  <- shifted by the SAME delta as
#     primary moved, so each player's real red-zone / goal-line discount vs.
#     their primary-zone rate is preserved, just re-centred on the new value.
# Both traits['splits'] (week-0 sim) and preseason_projection['splits'] (the
# frozen anchor the weekly blend tapers from) are synced, so the override
# doesn't evaporate over weeks 1-5 of an in-season refresh.
ZONE_RATE_FIELDS = ("catch_rate",)
ZONE_RATE_CLIP = (0.05, 0.99)
# Only sync catch_rate into the splits for players with a real receiving role.
# Below this target share the flat catch_rate is almost always a tiny-sample
# artifact (a backup RB with 3 career catches reads 1.00; a 4th WR reads off a
# dozen targets) and forcing it into the zone splits is worse than leaving the
# build-time default -- and these players have no measurable effect on a sim
# anyway. Real receivers and pass-catching backs clear this easily.
MIN_TARGET_SHARE_FOR_RATE_SYNC = 0.04


def load_overrides(year):
    path = os.path.join(DNA_DIR, f"preseason_overrides_{year}.csv")
    if not os.path.exists(path):
        raise SystemExit(f"{path} not found -- run export_preseason_overrides_v_0_1_0.py {year} first.")
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def apply(year):
    rows = load_overrides(year)

    rosters = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, f"*_traits_{year}.json")):
        data = json.load(open(path))
        rosters[data["team"]] = (path, data)

    changes = []
    unmatched = []

    for row in rows:
        name, team = row["player_name"], row["team"]
        if team not in rosters:
            unmatched.append((name, team, "team file not found"))
            continue
        _, data = rosters[team]
        traits = data["traits"].get(name)
        if traits is None:
            unmatched.append((name, team, "player not found in roster file"))
            continue

        pp = traits.setdefault("preseason_projection", {})
        for field in TUNABLE_FIELDS:
            raw = row.get(field, "")
            if raw is None or str(raw).strip() == "":
                continue
            try:
                value = float(raw)
            except ValueError:
                unmatched.append((name, team, f"unparseable value for {field}: {raw!r}"))
                continue
            old = traits.get(field)
            if old is None or abs(old - value) >= 1e-9:
                traits[field] = value
                pp[field] = value
                changes.append((team, name, field, old, value))

            # Splits sync runs every time this field has a CSV value, not just
            # when the flat field itself is changing this run -- otherwise a
            # player whose flat value was already applied in an earlier run
            # (so `old == value` here) but whose splits never got synced back
            # then stays permanently desynced. Re-checking unconditionally
            # makes this pass a full repair, not just a going-forward guard.
            if field in ZONE_SHARE_FIELDS:
                splits = traits.get("splits")
                if splits and field in splits.get("primary", {}):
                    primary_before = splits["primary"].get(field)
                    for zone in ZONES:
                        zdict = splits.get(zone)
                        if zdict is None or field not in zdict:
                            continue
                        # primary always tracks the flat/overall share.
                        # redzone/goalline only follow along if they were
                        # still mirroring primary (i.e. never hand-
                        # differentiated) -- a real redzone/inside-the-5
                        # split set separately from primary is left alone.
                        in_lockstep = zone == "primary" or (
                            primary_before is not None and abs(zdict[field] - primary_before) < 1e-9
                        )
                        zone_old = zdict[field]
                        if in_lockstep and abs(zone_old - value) > 1e-9:
                            zdict[field] = value
                            changes.append((team, name, f"splits.{zone}.{field}", zone_old, value))

            # catch_rate: primary <- flat value, redzone/goalline shift by the
            # same delta (keep each player's zone shape). Synced into both
            # traits['splits'] and preseason_projection['splits']. See the
            # ZONE_RATE_FIELDS note above.
            if field in ZONE_RATE_FIELDS:
                lo, hi = ZONE_RATE_CLIP
                try:
                    csv_ts = float(row.get("target_share") or 0)
                except ValueError:
                    csv_ts = 0.0
                role_ts = max(csv_ts, float(traits.get("target_share") or 0))
                if role_ts < MIN_TARGET_SHARE_FOR_RATE_SYNC:
                    continue
                for scope, container in (("", traits), ("pp.", pp)):
                    splits = container.get("splits")
                    if not splits or field not in splits.get("primary", {}):
                        continue
                    old_primary = splits["primary"].get(field)
                    delta = (value - old_primary) if old_primary is not None else 0.0
                    for zone in ZONES:
                        zdict = splits.get(zone)
                        if zdict is None or field not in zdict:
                            continue
                        zone_old = zdict[field]
                        new_zone = value if zone == "primary" else max(lo, min(hi, zone_old + delta))
                        if abs(zone_old - new_zone) > 1e-9:
                            zdict[field] = new_zone
                            changes.append((team, name, f"{scope}splits.{zone}.{field}", zone_old, new_zone))

    for path, data in rosters.values():
        with open(path, "w") as f:
            json.dump(data, f, indent=4)

    if changes:
        print(f"Applied {len(changes)} field change(s):")
        for team, name, field, old, new in changes:
            print(f"  {team} {name}: {field} {old} -> {new}")
    else:
        print("No changes -- CSV matches what's already in the roster files.")

    if unmatched:
        print(f"\n{len(unmatched)} row/field issue(s) skipped:")
        for name, team, reason in unmatched:
            print(f"  {name} ({team}): {reason}")


def main():
    if len(sys.argv) != 2:
        print("Usage: python apply_preseason_overrides_v_0_1_0.py <year>")
        sys.exit(1)
    apply(int(sys.argv[1]))


if __name__ == "__main__":
    main()
