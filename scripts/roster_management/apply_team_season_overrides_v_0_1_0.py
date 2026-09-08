"""Apply the 32 per-team season-long override sheets
(data/overrides/2026/season_long/{TEAM}.csv) back into the pipeline.

Steps:
  1. Concatenate every sheet's player_name/team/pos + the 15 flat tunable
     fields -> regenerate data/dna/preseason_overrides_2026.csv, then run
     apply_preseason_overrides(2026) -> traits JSON (top-level +
     preseason_projection + primary splits).
  2. Update the 4 zone-share columns (rz_target_share -> rz_target_share,
     rz_carry_share -> rz_carry_share, gl_target_share -> five_target_share,
     gl_carry_share -> five_carry_share) into
     data/dna/zone_usage_overrides_2026.csv -- preserving that file's
     hist_*/median_* research columns -- then run
     apply_zone_usage_overrides(2026) -> traits.splits.redzone/goalline.
  3. sync_roster_membership() -- steps 1-2 only ever UPDATE an existing
     traits.json entry; they never add or remove one. Without this step two
     real gaps open up: (a) a player deleted from a sheet (cut/traded away)
     keeps whatever nonzero share was last written to traits.json forever --
     the sim would still give them touches; (b) a player added to a sheet
     (a trade/signing from the nfl.com feed, or a hand-added row) who isn't
     already a traits.json key can never receive a hand-assigned share --
     apply_preseason_overrides/apply_zone_usage_overrides silently skip
     ("player not found") every single run. This step zeroes (a) and
     creates a full schema entry via enrich_player for (b), then applies
     the CSV's field values on top -- so traits.json's player SET always
     matches the sheets', not just the values of whoever was already there.

The per-team sheets are the source of truth; the two league-wide CSVs are
build artifacts the existing apply steps read. `roster_slot=cut/left_team/
retired` rows are excluded (a hand-flagged cut whose share you haven't
reassigned yet just doesn't get applied).

Idempotent. Run after editing the per-team sheets.

Usage: venv\\Scripts\\python.exe scripts/roster_management/apply_team_season_overrides_v_0_1_0.py
"""
import os
import sys
import json
import glob

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS, PLAYER_NGS_FIELDS  # noqa: E402
from src.data_pipeline.week_roster_v_0_1_0 import is_player_row  # noqa: E402
from roster_feed_v_0_1_0 import read_rows, write_rows, match_key  # noqa: E402
from build_2026_rosters_v_0_1_0 import enrich_player, load_skill_dna  # noqa: E402
import apply_preseason_overrides_v_0_1_0 as apply_pre  # noqa: E402
import apply_zone_usage_overrides_v_0_1_0 as apply_zone  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")
ROSTERS_DIR = os.path.join("data", "current_rosters")
PRESEASON_CSV = os.path.join("data", "dna", "preseason_overrides_2026.csv")
ZONE_CSV = os.path.join("data", "dna", "zone_usage_overrides_2026.csv")
TUNABLE = PLAYER_RATE_FIELDS + PLAYER_NGS_FIELDS
FLAT_COLS = ["player_name", "team", "pos"] + TUNABLE
RZ_COLS = ["rz_target_share", "rz_carry_share", "gl_target_share", "gl_carry_share"]
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}
GONE = ("left_team", "cut", "retired")
SHARE_FIELDS = ("target_share", "carry_share")
ZONES = ("primary", "redzone", "goalline")
# sheet zone column -> zone_usage_overrides_2026.csv column
SHEET_TO_ZONE = {"rz_target_share": "rz_target_share", "rz_carry_share": "rz_carry_share",
                 "gl_target_share": "five_target_share", "gl_carry_share": "five_carry_share"}


def _f(v, default=0.0):
    try:
        return float(v)
    except (TypeError, ValueError):
        return default


def _zero_shares(player):
    for field in SHARE_FIELDS:
        if field in player:
            player[field] = 0.0
        for zone in player.get("splits", {}).values():
            if field in zone:
                zone[field] = 0.0
        pp = player.get("preseason_projection", {})
        if field in pp:
            pp[field] = 0.0
        for zone in pp.get("splits", {}).values():
            if field in zone:
                zone[field] = 0.0


def sync_roster_membership(sheet_rows, skill_dna):
    """traits.json's player SET, not just values, follows the sheets:
      - a traits.json player no longer present in ANY sheet -> shares zeroed
      - a sheet player not yet in traits.json -> a full entry is created
        (enrich_player for the schema/defaults) then the sheet's field
        values (incl. rz_/gl_ zone shares) are applied on top.
    Returns (zeroed, created) name lists for reporting."""
    by_team = {}
    for r in sheet_rows:
        by_team.setdefault(r["team"], {})[match_key(r["player_name"])] = r

    zeroed, created = [], []
    for team, rows_by_key in by_team.items():
        path = os.path.join(ROSTERS_DIR, f"{team}_traits_2026.json")
        if not os.path.exists(path):
            continue
        data = json.load(open(path))
        traits = data["traits"]

        traits_keys = {match_key(n): n for n in traits}
        for key, name in traits_keys.items():
            p = traits[name]
            if key not in rows_by_key and (_f(p.get("target_share")) > 0
                                           or _f(p.get("carry_share")) > 0):
                _zero_shares(p)
                zeroed.append((team, name))

        team_created = False
        for key, r in rows_by_key.items():
            if key in traits_keys:
                continue
            team_created = True
            name = r["player_name"]
            pos = r["pos"]
            pid = (r.get("player_id") or "").strip() or None
            entry = enrich_player(name, pos, _f(r.get("target_share")), _f(r.get("carry_share")),
                                  skill_dna, False, None, player_id=pid)
            for f in TUNABLE:
                v = r.get(f, "")
                if str(v).strip() != "":
                    entry[f] = _f(v, entry.get(f))
            for sheet_col, field in (("rz_target_share", "target_share"),
                                     ("rz_carry_share", "carry_share")):
                v = str(r.get(sheet_col, "")).strip()
                if v != "":
                    entry.setdefault("splits", {}).setdefault("redzone", {})[field] = _f(v)
            for sheet_col, field in (("gl_target_share", "target_share"),
                                     ("gl_carry_share", "carry_share")):
                v = str(r.get(sheet_col, "")).strip()
                if v != "":
                    entry.setdefault("splits", {}).setdefault("goalline", {})[field] = _f(v)
            traits[name] = entry
            created.append((team, name))

        if team_created:  # re-sort by position so validate_rosters.py's ordering check stays clean
            data["traits"] = dict(sorted(
                traits.items(), key=lambda kv: POS_ORDER.get(kv[1].get("pos"), 4)))
        json.dump(data, open(path, "w"), indent=4)
    return zeroed, created


def main():
    paths = sorted(glob.glob(os.path.join(SEASON_DIR, "*.csv")))
    if not paths:
        print(f"No sheets in {SEASON_DIR} -- run export_team_season_overrides first.")
        sys.exit(1)

    sheet_rows = []           # every real, non-gone player row (skips 'Totals' rows)
    for p in paths:
        _, rs = read_rows(p)
        for r in rs:
            if is_player_row(r) and (r.get("roster_slot") or "").strip().lower() not in GONE:
                sheet_rows.append(r)

    # --- 1. flat preseason_overrides ---------------------------------
    flat = [{c: r.get(c, "") for c in FLAT_COLS} for r in sheet_rows]
    flat.sort(key=lambda r: (r["team"], POS_ORDER.get(r["pos"], 4), r["player_name"]))
    write_rows(PRESEASON_CSV, FLAT_COLS, flat)
    print(f"Regenerated {PRESEASON_CSV} ({len(flat)} rows).")

    # --- 2. zone_usage_overrides (preserve hist_*/median_* columns) --
    zcols, zrows = ([], [])
    if os.path.exists(ZONE_CSV):
        zcols, zrows = read_rows(ZONE_CSV)
    zcols = zcols or (["player_name", "team", "pos"] + list(SHEET_TO_ZONE.values()))
    zindex = {(r["team"], match_key(r["player_name"])): r for r in zrows}

    out_z, seen = [], set()
    for r in sheet_rows:
        k = (r["team"], match_key(r["player_name"]))
        seen.add(k)
        base = zindex.get(k) or {c: "" for c in zcols}
        base.update({"player_name": r.get("player_name"), "team": r.get("team"),
                     "pos": r.get("pos")})
        for sheet_col, zone_col in SHEET_TO_ZONE.items():
            v = str(r.get(sheet_col, "")).strip()
            if v != "":
                base[zone_col] = v
        out_z.append({c: base.get(c, "") for c in zcols})
    out_z.sort(key=lambda r: (r.get("team", ""), POS_ORDER.get(r.get("pos"), 4),
                              r.get("player_name", "")))
    write_rows(ZONE_CSV, zcols, out_z)
    print(f"Regenerated {ZONE_CSV} ({len(out_z)} rows, research columns preserved).")

    # --- apply both -------------------------------------------------
    print("\nApplying flat overrides to data/current_rosters/*_traits_2026.json ...")
    apply_pre.apply(2026)
    print("\nApplying zone (redzone/goalline) overrides ...")
    apply_zone.apply(2026)

    # --- 3. make traits.json's player SET match the sheets ------------
    print("\nSyncing roster membership (zero removed players, create new ones) ...")
    skill_dna = load_skill_dna()
    zeroed, created = sync_roster_membership(sheet_rows, skill_dna)
    if zeroed:
        print(f"  zeroed {len(zeroed)} player(s) no longer in any sheet:")
        for t, n in sorted(zeroed):
            print(f"    {t}  {n}")
    if created:
        print(f"  created {len(created)} new traits.json entr{'y' if len(created)==1 else 'ies'}:")
        for t, n in sorted(created):
            print(f"    {t}  {n}")
    if not zeroed and not created:
        print("  traits.json player set already matched the sheets.")


if __name__ == "__main__":
    main()
