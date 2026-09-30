"""Tests for src/evaluation/cash_lineup_eval.py -- Cash Lineups grading.

Pure functions against synthetic inputs (no real sims / network): the
latest-download staleness rule for DK standings CSVs, per-lineup grading
(projection sources, actual-score fallbacks, sim percentile) and the
benchmark comparison summary.
"""
import json
import os

import numpy as np

from src.evaluation import cash_lineup_eval as ce

KEY = lambda name: (name, None)  # noqa: E731 -- player_key for an already-normalized skill name
DST = lambda team: ("__dst__", team)  # noqa: E731


def _write_standings(folder, fname, rows, mtime):
    """A minimal DK standings export: lineup columns blank, Player/FPTS summary rows."""
    os.makedirs(folder, exist_ok=True)
    path = os.path.join(folder, fname)
    with open(path, "w", encoding="utf-8") as f:
        f.write("Rank,EntryId,EntryName,Points,Lineup,,Player,Roster Position,%Drafted,FPTS\n")
        for name, pos, fpts in rows:
            f.write(f",,,,,,{name},{pos},1.00%,{fpts}\n")
    os.utime(path, (mtime, mtime))
    return path


def test_load_dk_actuals_latest_download_wins(tmp_path):
    folder = ce.main_slate_dir(2026, 1, str(tmp_path))
    # Early (mid-slate) file has stale scores; a later file has the finals.
    _write_standings(folder, "slant_9_150max.csv", [("Justin Jefferson", "WR", "12.0"), ("Jets", "DST", "3")], 1000)
    _write_standings(folder, "screenpass_15_5max.csv", [("Justin Jefferson", "WR", "31.2")], 2000)
    # salaries_prelock.csv is not a standings file and must be ignored
    with open(os.path.join(folder, "salaries_prelock.csv"), "w") as f:
        f.write("name,team,pos,salary\njustin jefferson,MIN,WR,8000\n")
    got = ce.load_dk_actuals(2026, 1, str(tmp_path))
    assert got[KEY("justin jefferson")] == 31.2      # later download wins
    assert got[DST("NYJ")] == 3.0                    # only-in-older-file player kept
    assert ce.load_dk_actuals(2026, 2, str(tmp_path)) == {}   # no archive -> not gradable


def _players():
    return [
        {"slot": "QB", "name": "Jared Goff", "team": "DET", "pos": "QB", "projection": 20.0, "salary": 6000},
        {"slot": "WR", "name": "Devaughn Vele", "team": "NO", "pos": "WR", "salary": 3500},   # no own projection
        {"slot": "DST", "name": "Jets", "team": "NYJ", "pos": "DST", "projection": 5.0, "salary": 2500},
    ]


def test_grade_lineup_sources_totals_and_percentile():
    proj_lookup = {KEY("devaughn vele"): 7.0}
    actuals = {KEY("jared goff"): 16.0, DST("NYJ"): 9.0}
    fallback = {KEY("devaughn vele"): 19.9}
    sims = {KEY("jared goff"): np.array([10.0, 20.0, 30.0, 40.0]),
            KEY("devaughn vele"): np.array([0.0, 5.0, 10.0, 15.0]),
            DST("NYJ"): np.array([0.0, 5.0, 10.0, 15.0])}
    g = ce.grade_lineup(_players(), proj_lookup, actuals, fallback, sims, 4)

    by = {p["name"]: p for p in g["players"]}
    assert by["Jared Goff"]["source"] == "dk" and by["Jared Goff"]["diff"] == -4.0
    assert by["Devaughn Vele"]["projection"] == 7.0 and by["Devaughn Vele"]["source"] == "nflverse"
    assert g["projected"] == 32.0 and g["actual"] == 44.9 and g["diff"] == 12.9
    assert g["salary"] == 12000 and g["n_beat"] == 2 and g["missing"] == []
    # lineup sims per iteration: 10, 30, 50, 70 -> 44.9 beats 2 of 4
    assert g["sim"]["actual_percentile"] == 50.0
    assert g["sim"]["p50"] == 40.0


def test_grade_lineup_missing_player_counts_zero_and_no_sims():
    g = ce.grade_lineup(_players(), {}, {KEY("jared goff"): 16.0}, {}, {}, 0)
    assert g["actual"] == 16.0
    assert set(g["missing"]) == {"Devaughn Vele", "NYJ DST"}
    assert g["sim"] is None
    assert next(p for p in g["players"] if p["name"] == "Devaughn Vele")["projection"] is None


def _graded(label, actual, players):
    return {"label": label, "projected": 150.0, "actual": actual, "diff": actual - 150.0,
            "players": [{"name": n, "team": t, "pos": pos, "actual": a} for n, t, pos, a in players]}


def test_summarize_counts_and_overlap():
    ours = [
        _graded("Build 1", 140.0, [("Jahmyr Gibbs", "DET", "RB", 37.6), ("Jared Goff", "DET", "QB", 16.4),
                                   ("Defense", "NYJ", "DST", 9.0)]),
        _graded("Build 2", 170.0, []),
    ]
    bench = [_graded("Adam Levitan", 160.0, [("jahmyr gibbs", "DET", "RB", 37.6), ("Jonathan Taylor", "IND", "RB", 26.1),
                                            ("Jets", "NYJ", "DST", 9.0)])]
    s = ce.summarize(ours, bench)
    assert s["avg_actual"] == 155.0 and s["n_beat_projection"] == 1 and s["best_actual"] == 170.0
    b = s["benchmarks"][0]
    assert b["n_ours_beat_it"] == 1 and b["top_build_minus_benchmark"] == -20.0
    # case-insensitive name match; DST matched on team regardless of display name
    assert [p["name"] for p in b["shared"]] == ["Jahmyr Gibbs", "Defense"]
    assert [p["name"] for p in b["only_ours"]] == ["Jared Goff"]
    assert [p["name"] for p in b["only_theirs"]] == ["Jonathan Taylor"]


def test_load_benchmarks_filters_week(tmp_path):
    p = ce.benchmark_path(2026, str(tmp_path))
    os.makedirs(os.path.dirname(p))
    with open(p, "w") as f:
        json.dump({"lineups": [{"week": 1, "label": "A", "players": []}, {"week": 2, "label": "B", "players": []}]}, f)
    assert [b["label"] for b in ce.load_benchmarks(2026, 2, str(tmp_path))] == ["B"]
    assert ce.load_benchmarks(2027, 1, str(tmp_path)) == []


def test_build_cash_eval_not_gradable_without_standings(tmp_path):
    assert ce.build_cash_eval(2026, 9, [], [], str(tmp_path)) == {
        "gradable": False, "ours": [], "benchmarks": [], "summary": {}}
