"""Window-blend rollout Phase 1: week sheets must FOLLOW the live blend unless a row is
hand_edit=TRUE, and OUT-redistribution must work on the live (blended) numbers."""
import os
import sys

import pytest

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)
sys.path.insert(0, os.path.join(ROOT, "scripts", "roster_management"))

import apply_team_week_overrides_v_0_1_0 as apply_wk  # noqa: E402
import build_week_overrides_v_0_1_0 as bwo  # noqa: E402


def _row(name, pos, tgt, car, slot="active", ret="", hand=""):
    r = {c: "" for c in ("player_name", "team", "pos", "player_id", "roster_slot", "return_week", "note",
                         "target_share", "carry_share", "rz_target_share", "rz_carry_share",
                         "gl_target_share", "gl_carry_share", "hand_edit")}
    r.update(player_name=name, team="AAA", pos=pos, roster_slot=slot, return_week=ret, target_share=str(tgt),
             carry_share=str(car), rz_target_share=str(tgt), rz_carry_share=str(car),
             gl_target_share=str(tgt), gl_carry_share=str(car), hand_edit=hand)
    return r


def _live(tgt, car, rz_t=None, rz_c=None):
    return {"target_share": tgt, "carry_share": car,
            "splits": {"redzone": {"target_share": rz_t if rz_t is not None else tgt, "carry_share": rz_c if rz_c is not None else car},
                       "goalline": {"target_share": tgt, "carry_share": car}}}


def test_unmarked_row_takes_live_blend_and_hand_edit_keeps_sheet():
    rows = [_row("A WR", "WR", 0.30, 0.0), _row("B WR", "WR", 0.30, 0.0, hand="TRUE")]
    live = {"A WR": _live(0.18, 0.0), "B WR": _live(0.18, 0.0)}
    out = {r["player_name"]: r for r in bwo.fill_live_shares(rows, live)}
    assert float(out["A WR"]["target_share"]) == pytest.approx(0.18)
    assert float(out["A WR"]["rz_target_share"]) == pytest.approx(0.18)
    assert float(out["B WR"]["target_share"]) == pytest.approx(0.30)  # hand edit untouched


def test_player_missing_from_live_is_untouched():
    out = bwo.fill_live_shares([_row("New Guy", "WR", 0.05, 0.0)], {"A WR": _live(0.18, 0.0)})
    assert float(out[0]["target_share"]) == pytest.approx(0.05)


def test_apply_overlays_nothing_for_unmarked_player_but_keeps_hand_edit():
    live = {"A WR": _live(0.18, 0.0), "B WR": _live(0.18, 0.0)}
    rows = bwo.fill_live_shares([_row("A WR", "WR", 0.30, 0.0), _row("B WR", "WR", 0.30, 0.0, hand="TRUE")], live)
    a = apply_wk._redistribution_only_row(rows[0], live["A WR"])
    b = apply_wk._redistribution_only_row(rows[1], live["B WR"])
    assert not any(c in a for c in apply_wk.SHARE_COLS)       # nothing to overlay -> live blend survives
    assert float(b["target_share"]) == pytest.approx(0.30)      # deliberate edit still applied


def test_out_player_share_redistributes_from_live_values():
    sheet = [_row("Star WR", "WR", 0.40, 0.0, slot="ir", ret="99"),
             _row("WR2", "WR", 0.30, 0.0), _row("WR3", "WR", 0.30, 0.0)]
    live = {"Star WR": _live(0.20, 0.0), "WR2": _live(0.10, 0.0), "WR3": _live(0.30, 0.0)}
    rows, _ = bwo.build_team_week_rows(sheet, week=5, team=None, live_traits=live)
    by = {r["player_name"]: float(r["target_share"]) for r in rows}
    assert by["Star WR"] == 0.0
    # Star's LIVE 0.20 goes pro-rata to WR2 (0.10) / WR3 (0.30): 0.15 / 0.45 (total 0.60),
    # then the team is normalized to 1.0 (2026-10-13) with the same 1:3 ratio.
    assert by["WR2"] == pytest.approx(0.25)
    assert by["WR3"] == pytest.approx(0.75)
    assert sum(by.values()) == pytest.approx(1.0)
