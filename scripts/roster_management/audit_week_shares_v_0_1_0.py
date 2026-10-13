"""Hand-check a week's compiled share sheets (data/overrides/2026/week_NN/{TEAM}.csv).

Read-only. Never writes to the override sheets or rosters.

Inputs : week number (+ optional team abbr), the week sheets, the matching
         season_long sheets (for the "sheet" vs "week" comparison).
Outputs: a per-team table on stdout (every player holding any share: target,
         carry, rz/gl, hand_edit flag, slot/status, and the change vs. the
         season_long sheet), team sum rows, and a FLAGS list.
Purpose: Cam wants to eyeball that the share pipeline (live-blend fill ->
         out-player redistribution -> normalize to 1.0) is not doing anything
         egregious before sims run.

Flags raised:
  SUM       a share column's active-player total is not 1.000 (+/-0.002)
  GHOST     a player who is IR/PUP/NFI/suspended/PS/cut/gone, or dfs_status=out,
            still carries share
  SPIKE     a player's week value is > 2x his season_long sheet value (and
            moved by >= 0.10) -- a redistribution or live-blend surprise
  BIGSHARE  a single player holds > 0.45 target or > 0.75 carry
  NOBACKS   a team has no active RB/WR/TE with share (shares would be lost)

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/audit_week_shares_v_0_1_0.py 6
    venv\\Scripts\\python.exe scripts/roster_management/audit_week_shares_v_0_1_0.py 6 MIN
    venv\\Scripts\\python.exe scripts/roster_management/audit_week_shares_v_0_1_0.py 6 --flags-only
"""
import os
import sys
import glob

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.week_roster_v_0_1_0 import is_player_row, SHARE_FIELDS, RESERVE_SLOTS, parse_return_week  # noqa: E402
from roster_feed_v_0_1_0 import read_rows, match_key  # noqa: E402

SEASON_DIR = os.path.join("data", "overrides", "2026", "season_long")
SHORT = {"target_share": "tgt", "carry_share": "car", "rz_target_share": "rzT",
         "rz_carry_share": "rzC", "gl_target_share": "glT", "gl_carry_share": "glC"}
POS_ORDER = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}


def _f(v):
    """str/None -> float, blank or junk -> 0.0."""
    try:
        return float(str(v).strip())
    except (TypeError, ValueError):
        return 0.0


def audit_team(team, week_rows, season_rows, week):
    """Inputs: team abbr, that team's week-sheet rows, its season_long rows (dicts), week (int).
    Output: (lines, flags) -- printable table lines and a list of flag strings.
    Purpose: build the per-team view described in the module docstring."""
    season = {match_key(r["player_name"]): r for r in season_rows if r.get("player_name")}
    players = [r for r in week_rows if is_player_row(r)]
    lines, flags = [], []
    hdr = f"{'player':24s} {'pos':3s} {'slot':14s} {'stat':6s} {'HE':2s} " + \
          " ".join(f"{SHORT[c]:>6s}" for c in SHARE_FIELDS) + "   vs season_long (tgt/car)"
    lines += [f"=== {team} ===", hdr]

    def on_field(r):
        """Active slot, or a reserve player whose return_week has arrived (or a
        force_active toggle); never practice squad / gone; never dfs_status=out."""
        slot = (r.get("roster_slot") or "active").strip().lower()
        stat = (r.get("dfs_status") or "active").strip().lower()
        if stat == "out":
            return False
        if slot == "active":
            return True
        if slot in RESERVE_SLOTS:
            return stat == "force_active" or parse_return_week(r.get("return_week")) <= week
        return False

    shown = [r for r in players if any(_f(r.get(c)) > 0 for c in SHARE_FIELDS)
             or not on_field(r)]
    shown.sort(key=lambda r: (POS_ORDER.get(r["pos"].upper(), 9), -_f(r.get("target_share")) - _f(r.get("carry_share"))))
    for r in shown:
        s = season.get(match_key(r["player_name"]), {})
        slot = (r.get("roster_slot") or "active").strip().lower()
        stat = (r.get("dfs_status") or "active").strip().lower()
        he = "Y" if (r.get("hand_edit") or "").strip().upper() == "TRUE" else ""
        vals = " ".join(f"{_f(r.get(c)):6.3f}" for c in SHARE_FIELDS)
        cmp_ = f"{_f(s.get('target_share')):.3f}->{_f(r.get('target_share')):.3f} / " \
               f"{_f(s.get('carry_share')):.3f}->{_f(r.get('carry_share')):.3f}"
        lines.append(f"{r['player_name'][:24]:24s} {r['pos']:3s} {slot:14s} {stat:6s} {he:2s} {vals}   {cmp_}")
        if not on_field(r) and any(_f(r.get(c)) > 0 for c in SHARE_FIELDS):
            flags.append(f"{team} GHOST {r['player_name']} ({slot}/{stat}) holds share")
        for c in ("target_share", "carry_share"):
            new, old = _f(r.get(c)), _f(s.get(c))
            if new - old >= 0.10 and new > 2 * old and on_field(r):
                flags.append(f"{team} SPIKE {r['player_name']} {c} {old:.3f}->{new:.3f}")
        if _f(r.get("target_share")) > 0.45 or _f(r.get("carry_share")) > 0.75:
            flags.append(f"{team} BIGSHARE {r['player_name']} tgt={_f(r.get('target_share')):.3f} car={_f(r.get('carry_share')):.3f}")

    act = [r for r in players if on_field(r)]
    sums = " ".join(f"{sum(_f(r.get(c)) for r in act):6.3f}" for c in SHARE_FIELDS)
    lines.append(f"{'ACTIVE SUM':24s} {'':3s} {'':14s} {'':6s} {'':2s} {sums}")
    for c in SHARE_FIELDS:
        tot = sum(_f(r.get(c)) for r in act)
        if abs(tot - 1.0) > 0.002:
            flags.append(f"{team} SUM {c}={tot:.3f}")
    if not any(_f(r.get("target_share")) > 0 for r in act if r["pos"].upper() in ("RB", "WR", "TE")):
        flags.append(f"{team} NOBACKS no active skill player with target share")
    return lines, flags


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    flags_only = "--flags-only" in sys.argv
    if not args or not args[0].isdigit():
        print(__doc__)
        sys.exit(1)
    week = int(args[0])
    only = args[1].upper() if len(args) > 1 else None
    wk_dir = os.path.join("data", "overrides", "2026", f"week_{week:02d}")
    all_flags = []
    for path in sorted(glob.glob(os.path.join(wk_dir, "*.csv"))):
        team = os.path.basename(path)[:-4]
        if team.endswith("_live") or (only and team != only):
            continue
        _, week_rows = read_rows(path)
        sp = os.path.join(SEASON_DIR, f"{team}.csv")
        season_rows = read_rows(sp)[1] if os.path.exists(sp) else []
        lines, flags = audit_team(team, week_rows, season_rows, week)
        all_flags += flags
        if not flags_only:
            print("\n".join(lines) + "\n")
    print(f"FLAGS ({len(all_flags)}):")
    print("\n".join(all_flags) if all_flags else "  none")


if __name__ == "__main__":
    main()
