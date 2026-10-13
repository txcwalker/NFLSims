"""Tests for src/evaluation/tournament_pool_eval.py -- grading every generated lineup.

Everything runs on synthetic data in tmp_path (optimizer builds, a tiny
"First Down" standings CSV, payouts.json, override sheets, schedule) -- no real
sims, network, or repo data. Covers: field rescoring + rank lookup, payout
tables (saved vs scaled), build folding / repeat counting / post-lock
exclusion, stale-player classification, grading math, calibration bins, the
age table, exposure gaps, and one end-to-end build_week_eval.
"""
import datetime
import json
import os

import numpy as np
import pytest

from src.api import optimizer_store
from src.evaluation import tournament_pool_eval as tp
from src.scrapers.dk_scraper import normalize_player_name
from scripts.dfs_ownership.standings_parser import player_key

SLOTS = ["QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DST"]
POS = {"QB": "QB", "RB": "RB", "WR": "WR", "TE": "TE", "FLEX": "WR", "DST": "DST"}
BASE_PLAYERS = ["Quinn Back", "Rhett Runner", "Ross Rush", "Wade Wide", "Walt Wing", "Wes Wall", "Tate End"]


def K(name, pos="WR", team="AAA"):
    """player_key exactly as the grader builds it."""
    return player_key(name, team, pos, normalize_player_name)


def lineup_players(flex, dst_team="SEA", own=None):
    """9 builds-format players: 7 fixed + a FLEX + a DST."""
    names = BASE_PLAYERS + [flex]
    out = []
    for slot, name in zip(SLOTS[:8], names):
        out.append({"name": name, "team": "AAA", "pos": POS[slot], "slot": slot, "salary": 5000,
                    "projection": 10.0, "ownership_pct": own})
    out.append({"name": "Defense", "team": dst_team, "pos": "DST", "slot": "DST", "salary": 3000,
                "projection": 5.0, "ownership_pct": own})
    return out


def iso(ts):
    return datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


# ---------------------------------------------------------------- pure helpers
def test_paying_frac_and_regen_key_is_slot_agnostic():
    assert tp._paying_frac({"contest": {"paying_positions": 20, "field_size": 100}}) == pytest.approx(0.2)
    assert tp._paying_frac({"payingPositions": 10, "contestSize": 200}) == pytest.approx(0.05)
    assert tp._paying_frac({}) is None
    a, b = lineup_players("Flex One"), lineup_players("Flex One")
    b[1], b[2] = b[2], b[1]                    # swap two RB slots
    assert tp._lineup_key(a) == tp._lineup_key(b)


def test_payout_for_rank_and_tier_edges():
    tiers = [{"rank_start": 1, "rank_end": 1, "payout": 100.0}, {"rank_start": 2, "rank_end": 5, "payout": 10.0}]
    assert tp.payout_for_rank(tiers, 1) == 100.0
    assert tp.payout_for_rank(tiers, 5) == 10.0
    assert tp.payout_for_rank(tiers, 6) == 0.0


def test_rescore_and_rank_lookup_ties_and_missing():
    a, b, c = K("a"), K("b"), K("c")
    field = {"comps": {(a, b): 3, (a, c): 1, (b, c): 2}, "csv_points": {(a, b): 30.0, (a, c): 99.0}}
    actuals = {a: 10.0, b: 20.0, c: 5.0}
    res = tp.rescore_field(field, actuals, {})
    assert res["total"] == 6 and res["n_missing_rows"] == 0
    # scores: ab=30 (x3), ac=15 (x1), bc=25 (x2)
    assert tp.n_field_above(res, 30.0) == 0       # ties share the best rank
    assert tp.n_field_above(res, 29.99) == 3
    assert tp.n_field_above(res, 20.0) == 5
    assert tp.n_field_above(res, 1.0) == 6
    assert res["max_csv_diff"] == 84.0            # ac rescored 15 vs CSV 99
    # a player with no real score counts 0 and is flagged
    res2 = tp.rescore_field(field, {a: 10.0, b: 20.0}, {})
    assert res2["n_missing_rows"] == 3            # (a,c) x1 + (b,c) x2


def test_field_ownership_is_share_of_entries():
    a, b = K("a"), K("b")
    own = tp.field_ownership({"comps": {(a, b): 1, (a,): 3}})
    assert own[a] == 100.0 and own[b] == 25.0


def test_calibration_bins_and_small_sample_guard():
    pred = list(np.linspace(1, 10, 50))
    realized = [1.0 if p > 5 else 0.0 for p in pred]
    bins = tp._calibration_bins(pred, realized, n_bins=5)
    assert [b["n"] for b in bins] == [10] * 5
    assert bins[0]["realized"] == 0.0 and bins[-1]["realized"] == 100.0
    assert bins[0]["pred_mean"] < bins[-1]["pred_mean"]
    assert tp._calibration_bins([1, 2, 3], [0, 1, 0]) == []        # too few to bin


def test_classify_stale_reasons_and_thresholds():
    pi = lambda own: {"name": "x", "team": "T", "pos": "WR", "projection": [], "ownership": own}  # noqa: E731
    out, abandoned, fine, lowexp, popular = K("out"), K("abandoned"), K("fine"), K("lowexp"), K("popular")
    stale = tp.classify_stale(
        {out: pi([2.0]), abandoned: pi([20.0, 30.0]), fine: pi([10.0]), lowexp: pi([3.0]), popular: pi([25.0])},
        {out: 0.0, abandoned: 0.4, fine: 1.0, lowexp: 0.2, popular: 24.0},
        {out: "Out Guy"})
    assert stale[out] == "ruled out"                 # sheet wins even with low expectation
    assert stale[abandoned] == "field abandoned"      # expected 25%, field 0.4%
    assert fine not in stale                          # field has exactly 1.0% -> not abandoned
    assert lowexp not in stale                        # pool never expected much -> not news
    assert popular not in stale


def test_classify_stale_requires_not_playing_and_skips_dst():
    pi = lambda pos: {"name": "x", "team": "T", "pos": pos, "projection": [], "ownership": [20.0]}  # noqa: E731
    scratched, played, noscore, dst = K("scratched"), K("played"), K("noscore"), ("__dst__", "TEN")
    inputs = {scratched: pi("WR"), played: pi("WR"), noscore: pi("WR"), dst: pi("DST")}
    field = {scratched: 0.2, played: 0.3, noscore: 0.0, dst: 0.5}
    stale = tp.classify_stale(inputs, field, {}, {scratched: 0.0, played: 12.6})
    assert stale == {scratched: "field abandoned", noscore: "field abandoned"}   # played + DST stay valid
    # sheet "out" still wins even if the player somehow scored
    assert tp.classify_stale(inputs, field, {played: "x"}, {played: 12.6})[played] == "ruled out"


def test_age_table_buckets_and_stale_share():
    rows = [
        {"hours_before_lock": 2, "valid": True, "paid": True, "top1": False, "top01": False, "pct_finish": 10.0, "roi_pct": 100.0,
         "rank": 5, "actual_score": 1.0, "n_generated": 1},
        {"hours_before_lock": 3, "valid": False, "paid": False, "top1": False, "top01": False, "pct_finish": 90.0, "roi_pct": -100.0,
         "rank": 9, "actual_score": 1.0, "n_generated": 1},
        {"hours_before_lock": 100, "valid": True, "paid": False, "top1": False, "top01": False, "pct_finish": 60.0, "roi_pct": -100.0,
         "rank": 7, "actual_score": 1.0, "n_generated": 1},
        {"hours_before_lock": None, "valid": True},
    ]
    t = {r["label"]: r for r in tp.age_table(rows)}
    assert t["<6h before lock"]["n"] == 2 and t["<6h before lock"]["pct_stale"] == 50.0
    assert t["<6h before lock"]["paid_pct"] == 100.0           # roll-up uses valid rows only
    assert t["3+ days"]["pct_stale"] == 0.0
    assert "1-3 days" not in t                                  # empty buckets are skipped


def test_exposure_gaps_flags_underused_player():
    wicks, smith = K("wicks"), K("smith")
    rows = [{"key": [list(smith)]} for _ in range(8)] + [{"key": [list(K("other"))]} for _ in range(2)]
    g = tp.exposure_gaps(rows, {wicks: 30.0, smith: 1.0, K("other"): 5.0}, {smith: "ruled out"},
                         {wicks: 28.0}, {}, {wicks: "Dontayvion Wicks"})
    top = g["underused"][0]
    assert top["name"] == "Dontayvion Wicks" and top["pool_exposure"] == 0.0 and top["gap"] == 30.0 and top["actual"] == 28.0
    over = g["overused"][0]
    assert over["stale"] == "ruled out" and over["pool_exposure"] == 80.0


# ------------------------------------------------------------ files on disk
def write_payouts(base, week, field):
    folder = tp.main_slate_dir(2026, week, str(base))
    os.makedirs(folder, exist_ok=True)
    tiers = [{"rank_start": 1, "rank_end": 1, "payout": 5000.0}, {"rank_start": 2, "rank_end": 10, "payout": 500.0}]
    with open(os.path.join(folder, "payouts.json"), "w") as f:
        json.dump({"contests": [{"stem": "firstdown_1_20max", "max_entries": field, "tiers": tiers}]}, f)


def test_default_payout_saved_scaled_and_missing(tmp_path):
    base = str(tmp_path)
    assert tp.default_payout_tiers(2026, 4, 1000, base) == ([], "none") if os.path.isdir(os.path.join(base, "data", "dfs_ownership", "2026")) else True
    write_payouts(tmp_path, 4, 1000)
    os.makedirs(tp.main_slate_dir(2026, 2, base), exist_ok=True)
    tiers, src = tp.default_payout_tiers(2026, 4, 1000, base)
    assert src == "saved week 4" and tiers[0]["payout"] == 5000.0
    tiers, src = tp.default_payout_tiers(2026, 2, 1500, base)         # week 2 borrows week 4, x1.5
    assert src.startswith("scaled from week 4")
    assert tiers[0]["payout"] == 7500.0 and tiers[1]["rank_end"] == 15


@pytest.fixture
def builds_dir(tmp_path, monkeypatch):
    """Point optimizer_store at tmp_path/data/optimizer."""
    monkeypatch.setattr(optimizer_store, "_BASE", str(tmp_path / "data" / "optimizer"))
    return tmp_path


def save_build(week, build_id, created_ts, lineups, inputs_hash="h1", contest=None):
    optimizer_store.write_build(2026, week, {
        "build_id": build_id, "created_at": iso(created_ts), "inputs_hash": inputs_hash, "source": "autosave",
        "settings": {"contest": contest or {"paying_positions": 2, "field_size": 10}},
        "portfolio": {"total_ev_pct": 12.0},
        "lineups": [{"players": p, "itm_pct": 30.0, "top1_pct": 2.0, "top01_pct": 0.2, "ev_pct": 5.0,
                     "projected_score": 100.0, "total_salary": 49000, "over_salary_cap": False} for p in lineups],
    })


def test_collect_pool_counts_repeats_and_excludes_post_lock(builds_dir):
    lock = 2_000_000_000.0
    save_build(4, "20260101T000001Z", lock - 7200, [lineup_players("Flex One"), lineup_players("Flex Two")])
    save_build(4, "20260101T000002Z", lock - 3600, [lineup_players("Flex One")], inputs_hash="h2")
    save_build(4, "20260101T000003Z", lock + 60, [lineup_players("Flex Three")])      # after lock
    pool, builds, pinputs, excluded = tp.collect_pool(2026, 4, lock)
    assert [b["build_id"] for b in builds] == ["20260101T000001Z", "20260101T000002Z"]
    assert [e["build_id"] for e in excluded] == ["20260101T000003Z"]
    assert len(pool) == 2
    one = pool[tp._lineup_key(lineup_players("Flex One"))]
    assert one["n_generated"] == 2 and len(one["build_ids"]) == 2
    assert one["first_build"] == "20260101T000001Z" and one["last_build"] == "20260101T000002Z"
    assert one["inputs_hashes"] == {"h1", "h2"}
    assert K("Quinn Back", "QB") in pinputs and len(pinputs[K("Quinn Back", "QB")]["projection"]) == 2   # one sample per build
    # no lock supplied -> nothing is excluded
    assert len(tp.collect_pool(2026, 4, None)[0]) == 3


def test_available_weeks(builds_dir):
    save_build(2, "20260101T000001Z", 1.0, [lineup_players("Flex One")])
    save_build(5, "20260101T000001Z", 1.0, [lineup_players("Flex One")])
    assert tp.available_weeks(2026, str(builds_dir)) == [2, 5]


def test_prune_is_disabled(builds_dir):
    save_build(4, "20260101T000001Z", 1.0, [lineup_players("Flex One")])
    assert optimizer_store.PRUNE_ENABLED is False
    assert optimizer_store.prune_builds(2026, 4) == []
    assert optimizer_store.read_build(2026, 4, "20260101T000001Z") is not None


# --------------------------------------------------------------- end to end
def write_week(base):
    """A 4-entry field and the files around it. Scores: base 7 players x10 = 70,
    DST 5; FLEX points decide: field flex = 5/15/25/35 -> totals 80/90/100/110."""
    folder = tp.main_slate_dir(2026, 4, str(base))
    os.makedirs(folder, exist_ok=True)
    dst = "Seahawks"
    field_flex = ["Field Five", "Field Fifteen", "Field TwentyFive", "Field ThirtyFive"]
    flex_pts = {"Field Five": 5, "Field Fifteen": 15, "Field TwentyFive": 25, "Field ThirtyFive": 35,
                "Mid Twenty": 20, "Scratched Sam": 0, "Wicks Wade": 30}
    entries = []
    for i, f in enumerate(field_flex):
        lu = f"DST {dst}  FLEX {f} QB Quinn Back RB Rhett Runner RB Ross Rush TE Tate End WR Wade Wide WR Walt Wing WR Wes Wall"
        entries.append((4 - i, 100 + i, f"user{i}", 80 + 10 * i, lu))
    summary = [(n, "WR", "1.00%", str(p)) for n, p in flex_pts.items()] + [(n, "WR", "1.00%", "10") for n in BASE_PLAYERS] + [(dst, "DST", "1.00%", "5")]
    with open(os.path.join(folder, "firstdown_1_20max.csv"), "w", encoding="utf-8") as f:
        f.write("Rank,EntryId,EntryName,TimeRemaining,Points,Lineup,,Player,Roster Position,%Drafted,FPTS\n")
        for i in range(max(len(entries), len(summary))):
            e = entries[i] if i < len(entries) else ("", "", "", "", "")
            s = summary[i] if i < len(summary) else ("", "", "", "")
            f.write(f"{e[0]},{e[1]},{e[2]},0,{e[3]},{e[4]},,{s[0]},{s[1]},{s[2]},{s[3]}\n")
    write_payouts(base, 4, 4)
    # override sheet: Scratched Sam is out
    sheets = os.path.join(str(base), "data", "overrides", "2026", "week_04")
    os.makedirs(sheets, exist_ok=True)
    with open(os.path.join(sheets, "AAA.csv"), "w", encoding="utf-8") as f:
        f.write("player_name,team,pos,dfs_status\nScratched Sam,AAA,WR,out\nMid Twenty,AAA,WR,active\n")
    # schedule: Sunday 13:00 ET Oct 4 2026 -> lock = 17:00 UTC
    ext = os.path.join(str(base), "data", "external")
    os.makedirs(ext, exist_ok=True)
    with open(os.path.join(ext, "schedule_2026.csv"), "w") as f:
        f.write("season,week,game_type,weekday,gameday,gametime\n2026,4,REG,Sunday,2026-10-04,13:00\n")
    return datetime.datetime(2026, 10, 4, 17, 0, tzinfo=datetime.timezone.utc).timestamp()


def test_build_week_eval_end_to_end(builds_dir):
    lock = write_week(builds_dir)
    ownership = lambda pct: pct  # noqa: E731
    # Sam is expected at 10% but ruled out per the sheet; Mid/Wicks are modest (<5%) so the
    # "field abandoned" rule (expected >=5%, field <1%) correctly leaves them alone.
    mid, scr = lineup_players("Mid Twenty", own=ownership(2.0)), lineup_players("Scratched Sam", own=ownership(10.0))
    # a "Wicks" lineup: scores 70+5+30 = 105 -> beats 100 and below, loses to 110
    wicks = lineup_players("Wicks Wade", own=ownership(2.0))
    save_build(4, "20261002T000001Z", lock - 2 * 86400, [mid, scr])                    # Friday-ish: Sam still looked fine
    save_build(4, "20261004T120001Z", lock - 3600, [mid, wicks], inputs_hash="h2")     # final build
    save_build(4, "20261005T000001Z", lock + 3600, [wicks])                             # post-lock: must be ignored

    res = tp.build_week_eval(2026, 4, str(builds_dir))
    assert res["gradable"]
    assert res["field"]["size"] == 4 and res["field"]["winner_score"] == 110.0
    assert res["field"]["max_rescore_vs_csv_diff"] == 0.0           # rescoring reproduces the CSV
    s = res["summary"]
    assert (s["n_builds"], s["n_generated"], s["n_unique"]) == (2, 4, 3)
    assert s["mean_regen"] == pytest.approx(4 / 3, abs=0.01) and s["max_regen"] == 2
    assert len(s["post_lock_builds_excluded"]) == 1

    by_flex = {next(p["name"] for p in r["players"] if p["slot"] == "FLEX"): r for r in res["lineups"]}
    # Mid Twenty: 70+5+20 = 95 -> beaten by 100 and 110 -> rank 3 of 4; pays the 500 tier
    assert by_flex["Mid Twenty"]["actual_score"] == 95.0 and by_flex["Mid Twenty"]["rank"] == 3
    assert by_flex["Mid Twenty"]["payout"] == 500.0 and by_flex["Mid Twenty"]["n_generated"] == 2
    assert by_flex["Mid Twenty"]["valid"]
    # Wicks: 105 -> rank 2
    assert by_flex["Wicks Wade"]["rank"] == 2 and by_flex["Wicks Wade"]["payout"] == 500.0
    # Scratched Sam is ruled out per the sheet -> stale, excluded from valid roll-ups
    sam = by_flex["Scratched Sam"]
    assert not sam["valid"] and sam["stale_players"][0]["reason"] == "ruled out"
    assert res["stale"]["n_stale"] == 1 and res["stale"]["n_valid"] == 2
    assert res["summary"]["valid"]["n"] == 2 and res["summary"]["all"]["n"] == 3
    assert res["summary"]["valid"]["best_rank"] == 2
    # age is keyed by a lineup's LAST build: Sam's only build is ~48h out (1-3 days) and he is
    # stale; Mid and Wicks were both regenerated/built in the final <6h build
    ages = {a["label"]: a for a in res["by_age"]}
    assert ages["1-3 days"]["n"] == 1 and ages["1-3 days"]["pct_stale"] == 100.0
    assert ages["<6h before lock"]["n"] == 2 and ages["<6h before lock"]["pct_stale"] == 0.0
    # the field held Field ThirtyFive/TwentyFive etc. that the pool never did -> shows as underused
    assert any(g["pool_exposure"] == 0.0 for g in res["exposure_gaps"]["underused"])
    # per-build roll-up covers both pre-lock builds; the stale build reports its stale share
    by_b = {b["build_id"]: b for b in res["by_build"]}
    assert set(by_b) == {"20261002T000001Z", "20261004T120001Z"}
    assert by_b["20261002T000001Z"]["pct_stale"] == 50.0 and by_b["20261004T120001Z"]["pct_stale"] == 0.0


def test_build_week_eval_not_gradable_reasons(builds_dir):
    r = tp.build_week_eval(2026, 4, str(builds_dir))
    assert not r["gradable"] and "not archived" in r["reason"]
    lock = write_week(builds_dir)
    save_build(4, "20261005T000001Z", lock + 60, [lineup_players("Mid Twenty")])      # only a post-lock build
    r = tp.build_week_eval(2026, 4, str(builds_dir))
    assert not r["gradable"] and "post-lock" in r["reason"]


def test_web_payload_drops_full_lineups_and_caches(builds_dir):
    lock = write_week(builds_dir)
    save_build(4, "20261004T120001Z", lock - 3600, [lineup_players("Mid Twenty")])
    p1 = tp.web_payload(2026, 4, str(builds_dir))
    assert "lineups" not in p1 and p1["available_weeks"] == [4] and p1["week"] == 4
    assert all("key" not in r for r in p1["top_lineups"])
    assert os.path.exists(tp.output_path(2026, 4, str(builds_dir)))
    assert tp.web_payload(2026, 4, str(builds_dir)) is p1            # unchanged inputs -> cached object
