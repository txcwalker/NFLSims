"""Tests for src/api/week_freeze.py: lock time = main slate's first kickoff,
snapshots are write-once, and reads return exactly what was saved."""
import pandas as pd
import pytest

from src.api import week_freeze


def _sched(tmp_path, rows):
    p = tmp_path / "sched.csv"
    pd.DataFrame(rows).to_csv(p, index=False)
    return str(p)


def _row(gameday, weekday, gametime, week=2):
    return {"season": 2026, "week": week, "game_type": "REG",
            "gameday": gameday, "weekday": weekday, "gametime": gametime}


def test_lock_is_first_sunday_1pm_not_thursday_or_london(tmp_path):
    sched = _sched(tmp_path, [
        _row("2026-09-17", "Thursday", "20:15"),   # Thursday night: not main slate
        _row("2026-09-20", "Sunday", "09:30"),     # London early game: not main slate
        _row("2026-09-20", "Sunday", "13:00"),     # <- first main-slate kickoff
        _row("2026-09-20", "Sunday", "16:25"),
        _row("2026-09-21", "Monday", "20:15"),
    ])
    ts = week_freeze.lock_ts(2, 2026, sched)
    expected = pd.Timestamp("2026-09-20 13:00").tz_localize("America/New_York").timestamp()
    assert ts == expected
    assert not week_freeze.is_locked(2, 2026, sched, now=expected - 1)
    assert week_freeze.is_locked(2, 2026, sched, now=expected)


def test_lock_none_for_unknown_week_never_locks(tmp_path):
    sched = _sched(tmp_path, [_row("2026-09-20", "Sunday", "13:00")])
    assert week_freeze.lock_ts(9, 2026, sched) is None
    assert not week_freeze.is_locked(9, 2026, sched, now=9e12)


def test_save_is_write_once_and_load_roundtrips(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    assert week_freeze.load(2026, 2, "projections") is None
    first = {"week": 2, "players": [{"name": "Lamar Jackson"}]}
    assert week_freeze.save(2026, 2, "projections", first, source="live", draft_group_id=7)
    # A later (different) payload must NOT replace the frozen one...
    assert not week_freeze.save(2026, 2, "projections",
                                {"week": 2, "players": [{"name": "Tyler Huntley"}]}, source="live")
    assert week_freeze.load(2026, 2, "projections") == first
    # ...unless explicitly forced.
    assert week_freeze.save(2026, 2, "projections", {"week": 2, "players": []},
                            source="backfill", force=True)
    assert week_freeze.load(2026, 2, "projections")["players"] == []
    meta = week_freeze.load_meta(2026, 2)
    assert meta["draft_group_id"] == 7 and meta["kinds"]["projections"]["source"] == "backfill"


# ---------------------------------------------------------------------------
# Per-game freeze (2026-10-12)
# ---------------------------------------------------------------------------

def _wk_sched(tmp_path):
    """Week 2: THU (kicks first), SUN (main slate), MON."""
    rows = []
    for gid, day, wd, t in (("g_thu", "2026-09-17", "Thursday", "20:15"),
                            ("g_sun", "2026-09-20", "Sunday", "13:00"),
                            ("g_mon", "2026-09-21", "Monday", "20:15")):
        r = _row(day, wd, t)
        r["game_id"] = gid
        rows.append(r)
    return _sched(tmp_path, rows)


def _ts(day, t):
    return pd.Timestamp(f"{day} {t}").tz_localize("America/New_York").timestamp()


def _proj(**salary_by_game):
    """projections payload with one player per game, tagged by `salary` so a
    test can tell which version of a game it got back."""
    return {"week": 2,
            "players": [{"name": f"p_{g}", "game_id": g, "salary": s} for g, s in salary_by_game.items()],
            "games": [{"game_id": g} for g in salary_by_game]}


def _sims(**tag_by_game):
    return {"week": 2, "games": {g: {"game_id": g, "tag": t} for g, t in tag_by_game.items()},
            "cash_consensus_lineups": ["live-cash"]}


def test_kicked_off_game_keeps_stored_entries_unkicked_updates(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    sched = _wk_sched(tmp_path)
    after_thu = _ts("2026-09-18", "12:00")
    # first request, Thursday game already played: nothing stored -> takes live
    out = week_freeze.serve(2026, 2, "projections", lambda: _proj(g_thu=1, g_sun=1, g_mon=1), sched, now=after_thu)
    assert {p["name"]: p["salary"] for p in out["players"]} == {"p_g_thu": 1, "p_g_sun": 1, "p_g_mon": 1}
    # roster change: live now says 2 everywhere -- Thursday must stay at 1, the rest move
    out = week_freeze.serve(2026, 2, "projections", lambda: _proj(g_thu=2, g_sun=2, g_mon=2), sched, now=after_thu)
    assert {p["name"]: p["salary"] for p in out["players"]} == {"p_g_thu": 1, "p_g_sun": 2, "p_g_mon": 2}
    assert [g["game_id"] for g in out["games"]] == ["g_thu", "g_sun", "g_mon"]
    # persisted: a later process sees the same thing on disk
    assert {p["name"]: p["salary"] for p in week_freeze.load(2026, 2, "projections")["players"]}["p_g_thu"] == 1
    assert week_freeze.load_meta(2026, 2)["kinds"]["projections"]["per_game"] is True


def test_monday_game_still_updates_after_the_sunday_lock(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    sched = _wk_sched(tmp_path)
    sun_eve = _ts("2026-09-20", "20:00")           # main slate locked, Monday not kicked
    week_freeze.serve(2026, 2, "projections", lambda: _proj(g_thu=1, g_sun=1, g_mon=1), sched, now=_ts("2026-09-19", "9:00"))
    out = week_freeze.serve(2026, 2, "projections", lambda: _proj(g_thu=9, g_sun=9, g_mon=9), sched, now=sun_eve)
    assert {p["name"]: p["salary"] for p in out["players"]} == {"p_g_thu": 1, "p_g_sun": 1, "p_g_mon": 9}


def test_game_that_vanishes_from_live_after_kickoff_is_still_served(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    sched = _wk_sched(tmp_path)
    week_freeze.serve(2026, 2, "sim_results", lambda: _sims(g_thu="pre", g_sun="a", g_mon="a"), sched, now=_ts("2026-09-17", "10:00"))
    # after kickoff the live path degrades / drops the played game
    out = week_freeze.serve(2026, 2, "sim_results", lambda: _sims(g_sun="b", g_mon="b"), sched, now=_ts("2026-09-18", "10:00"))
    assert out["games"]["g_thu"]["tag"] == "pre" and out["games"]["g_sun"]["tag"] == "b"


def test_cash_lineups_freeze_at_main_slate_lock_not_before(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    sched = _wk_sched(tmp_path)
    live1 = _sims(g_thu="a", g_sun="a", g_mon="a")
    out = week_freeze.serve(2026, 2, "sim_results", lambda: live1, sched, now=_ts("2026-09-19", "10:00"))
    live2 = _sims(g_thu="a", g_sun="a", g_mon="a")
    live2["cash_consensus_lineups"] = ["new-cash"]
    # pre-lock: follows live
    assert week_freeze.serve(2026, 2, "sim_results", lambda: live2, sched, now=_ts("2026-09-20", "12:00"))["cash_consensus_lineups"] == ["new-cash"]
    live3 = _sims(g_thu="a", g_sun="a", g_mon="a")
    live3["cash_consensus_lineups"] = ["post-lock"]
    # post-lock: stays at the last pre-lock copy
    assert week_freeze.serve(2026, 2, "sim_results", lambda: live3, sched, now=_ts("2026-09-20", "20:00"))["cash_consensus_lineups"] == ["new-cash"]


def test_legacy_whole_week_snapshot_served_verbatim_and_all_kicked_skips_live(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    sched = _wk_sched(tmp_path)

    def boom():
        raise AssertionError("live_fn must not run")

    legacy = _proj(g_thu=7, g_sun=7, g_mon=7)
    week_freeze.save(2026, 2, "projections", legacy, source="live")        # no per_game flag
    assert week_freeze.serve(2026, 2, "projections", boom, sched, now=_ts("2026-09-18", "9:00")) == legacy
    # per-game snapshot with every game kicked off and stored -> served without recompute
    week_freeze.save(2026, 2, "sim_results", _sims(g_thu="x", g_sun="x", g_mon="x"), source="live", per_game=True)
    assert week_freeze.serve(2026, 2, "sim_results", boom, sched, now=_ts("2026-09-23", "9:00"))["games"]["g_mon"]["tag"] == "x"


def test_empty_live_payload_never_wipes_the_store(tmp_path, monkeypatch):
    monkeypatch.setattr(week_freeze, "BASE_DIR", str(tmp_path))
    sched = _wk_sched(tmp_path)
    good = _proj(g_thu=1, g_sun=1, g_mon=1)
    week_freeze.serve(2026, 2, "projections", lambda: good, sched, now=_ts("2026-09-16", "9:00"))
    out = week_freeze.serve(2026, 2, "projections", lambda: {"week": 2, "players": [], "games": []}, sched, now=_ts("2026-09-17", "9:00"))
    assert out == good and week_freeze.load(2026, 2, "projections") == good
