"""One-time (idempotent) migration: add a `hand_edit` column to the 32
season-long sheets (data/overrides/2026/season_long/{TEAM}.csv).

Why (window-blend rollout plan, Phase 1a): a sheet's share columns used to be
overlaid on the DFS tree whenever they differed from the live blend -- and the
sheets are ~99.9% identical to the FROZEN PRESEASON values, so for about half
the players the sim silently got the preseason share instead of the blend.
Now an unmarked row FOLLOWS THE BLEND; only rows with hand_edit=TRUE keep the
sheet's own share values.

Pre-marking rule (conservative = status quo for anything that might be a real
hand-tune):  hand_edit=TRUE when ANY of
  - a share column differs from the player's frozen preseason_projection value,
  - the player has no preseason_projection at all (new signing; the sheet is the
    only source of his share),
  - the row has a hand-written note (anything except the auto-generated
    "new to team" / "NOT on nfl.com roster" notes).
Cam clears the flag on rows that should follow the blend. A review list is
written to data/overrides/2026/_hand_edit_migration.md.

Usage: python scripts/roster_management/add_hand_edit_column_v_0_1_0.py [--dry-run]
"""
import glob
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from roster_feed_v_0_1_0 import match_key, read_rows, write_rows  # noqa: E402
from apply_sheet_helpers_v_0_1_0 import ZONE_COL_MAP  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")
ROSTERS_DIR = os.path.join("data", "current_rosters")
SHARE_COLS = ("target_share", "carry_share", "rz_target_share", "rz_carry_share", "gl_target_share", "gl_carry_share")
AUTO_NOTES = ("new to team", "not on nfl.com roster")
TOL = 5e-4


def _f(v):
    try:
        return float(v)
    except (TypeError, ValueError):
        return None


def preseason_share(traits, col):
    """The frozen preseason value behind one share column, or None."""
    pp = traits.get("preseason_projection") or {}
    if col in ZONE_COL_MAP:
        zone, field = ZONE_COL_MAP[col]
        return (pp.get("splits") or {}).get(zone, {}).get(field)
    return pp.get(col)


def classify_row(row, traits):
    """Returns the reason string if this row should be pre-marked hand_edit, else ''."""
    if traits is None:
        return ""  # not in the live traits yet (nothing to follow, nothing to protect)
    if not traits.get("preseason_projection"):
        return "no frozen preseason value (new signing)"
    diffs = []
    for col in SHARE_COLS:
        sv, pv = _f(row.get(col)), preseason_share(traits, col)
        if sv is not None and pv is not None and abs(sv - pv) > TOL:
            diffs.append(f"{col} {sv:g} vs preseason {pv:g}")
    if diffs:
        return "; ".join(diffs)
    note = (row.get("note") or "").strip()
    if note and not note.lower().startswith(AUTO_NOTES):
        return f"note: {note[:80]}"
    return ""


def main():
    dry = "--dry-run" in sys.argv
    lines, n_rows, n_marked = [], 0, 0
    for path in sorted(glob.glob(os.path.join(SEASON_DIR, "*.csv"))):
        team = os.path.basename(path)[:-4]
        cols, rows = read_rows(path)
        live_path = os.path.join(ROSTERS_DIR, f"{team}_traits_2026.json")
        live = json.load(open(live_path, encoding="utf-8"))["traits"] if os.path.exists(live_path) else {}
        by_key = {match_key(n): t for n, t in live.items()}
        for r in rows:
            if not r.get("player_name"):
                continue
            n_rows += 1
            if (r.get("hand_edit") or "").strip().upper() == "TRUE":
                continue  # idempotent: never clear a flag
            reason = classify_row(r, by_key.get(match_key(r["player_name"])))
            r["hand_edit"] = "TRUE" if reason else ""
            if reason:
                n_marked += 1
                lines.append(f"- {team} {r['player_name']} ({r.get('roster_slot','')}): {reason}")
        if not dry:
            write_rows(path, list(cols) + (["hand_edit"] if "hand_edit" not in cols else []), rows)
    report = ["# hand_edit migration (2026-10-07)", "",
              f"{n_marked} of {n_rows} season-long rows pre-marked hand_edit=TRUE (sheet value wins over the blend). "
              "Clear the flag in the sheet for any row that should FOLLOW the blend.", ""] + lines
    if not dry:
        with open(os.path.join("data", "overrides", "2026", "_hand_edit_migration.md"), "w", encoding="utf-8") as f:
            f.write("\n".join(report))
    print(f"{'DRY RUN: ' if dry else ''}{n_marked}/{n_rows} rows marked hand_edit")


if __name__ == "__main__":
    main()
