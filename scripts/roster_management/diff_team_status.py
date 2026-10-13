"""Phase 2 of injury/depth-chart monitoring: diff the last two scrape_team_status.py
snapshots and report which not-yet-kicked-off games need a re-sim.

Reads  data/overrides/2026/_team_status/{previous,latest}/{TEAM}.json  (written by
scrape_team_status.py), manual_overrides.json (hand corrections, e.g. retirements the
sites haven't caught up with) and data/external/schedule_2026.csv.
Writes data/overrides/2026/_team_status/changes_latest.json (full report incl. the
per-team availability "leans") and appends changes_log.jsonl.
It does NOT run any sim -- it prints the resim_games_2026.py command to run. (Phase 5
wires the trigger.)

What counts as a change worth a re-sim ("material"): only positions the sim models
(QB/RB/WR/TE, OL, DL/edge) -- a CB limping changes nothing in the engine, so it is
listed as info only. Material =
  * a modeled player's out-probability changed,
  * a modeled depth-chart starter (first name in a slot) changed,
  * a modeled player entered/left the Active roster (IR, cut, retirement...).

Out-probability rules (Cam, 2026-10-07):
  Out / Doubtful ........................ 1.00
  Questionable, final practice day DNP .. 0.75  ("didn't practice Friday" -- really higher)
  Questionable otherwise ................ 0.50
  Status not published yet (mid-week) ... None  (no number invented; practice shown as the lean)

Usage:
    venv\\Scripts\\python.exe scripts/roster_management/diff_team_status.py [--week N]
"""
import os
import re
import sys
import json
import datetime

import pandas as pd

sys.path.insert(0, os.path.join("scripts", "simulation_runners"))
from sim_run_status import has_kicked_off  # noqa: E402

STATUS_DIR = os.path.join("data", "overrides", "2026", "_team_status")
SCHEDULE = os.path.join("data", "external", "schedule_2026.csv")
OVERRIDES = os.path.join(STATUS_DIR, "manual_overrides.json")

# Positions the engine actually uses (skill + trench); anything else is info-only.
MODELED_POS = {"QB", "RB", "HB", "FB", "WR", "TE", "T", "OT", "G", "OG", "C", "OL", "IOL",
               "DE", "DT", "NT", "DL", "EDGE", "OLB"}
# Depth-chart slot names (after stripping a trailing tier digit) that are modeled.
MODELED_SLOTS = MODELED_POS | {"LT", "LG", "RG", "RT", "LOT", "ROT", "LDE", "RDE", "LDT", "RDT"}
UNSET = ("", "(-)", "UNSPECIFIED")


def out_probability(status, practice):
    """Inputs: status (str, report 'Game Status'), practice (list[str], Mon/Tue/Wed cells).
    Output: float in [0,1], or None when no game status has been published yet.
    Purpose: Cam's availability rules (module docstring). 'Final practice day' = the
    last non-blank practice cell, which on a Sunday game is Friday."""
    st = (status or "").strip().upper()
    if st in UNSET:
        return None
    if st in ("OUT", "DOUBTFUL", "IR", "INJURED RESERVE"):
        return 1.0
    if st == "QUESTIONABLE":
        days = [p for p in practice if p not in UNSET]
        return 0.75 if days and days[-1].upper() == "DNP" else 0.5
    return 0.0                                   # e.g. a future 'Probable'/'Full' designation


def apply_overrides(snap, overrides):
    """Inputs: snap (one team snapshot dict), overrides (manual_overrides.json dict).
    Output: a copy with that team's retired players removed and depth slots replaced.
    Purpose: let hand knowledge win over sites that lag (e.g. a retirement)."""
    ov = overrides.get(snap["team"])
    if not ov:
        return snap
    s = json.loads(json.dumps(snap))
    gone = set(ov.get("retired", []))
    s["roster"] = [r for r in s["roster"] if r["player"] not in gone]
    for slots in s["depth"].values():
        for k in slots:
            slots[k] = [n for n in slots[k] if n not in gone]
    for slot, names in ov.get("depth", {}).items():
        for slots in s["depth"].values():
            if slot in slots:
                slots[slot] = names
    return s


def _modeled(pos):
    return re.sub(r"\d+$", "", pos).upper() in MODELED_SLOTS


def _starters(snap):
    """Depth chart -> {'Unit:SLOT': first-listed name} for the offense and defense units."""
    return {f"{u}:{k}": v[0] for u in ("Offense", "Defense")
            for k, v in snap["depth"].get(u, {}).items() if v}


def team_changes(prev, cur):
    """Inputs: prev/cur snapshots of one team (overrides already applied).
    Output: list[dict(kind, player, pos, detail, material)].  See module docstring for rules."""
    out = []
    # --- injuries (own report only; the opponent copy is covered by their own page) ---
    pi = {r["player"]: r for r in prev["injuries"]["team"]}
    ci = {r["player"]: r for r in cur["injuries"]["team"]}
    sig = lambda r: (r["status"], tuple(r["practice"])) if r else None   # noqa: E731
    for name in sorted(set(pi) | set(ci)):
        a, b = pi.get(name), ci.get(name)
        if sig(a) == sig(b):
            continue
        pa = out_probability(a["status"], a["practice"]) if a else 0.0
        pb = out_probability(b["status"], b["practice"]) if b else 0.0
        r = b or a
        out.append({"kind": "injury", "player": name, "pos": r["pos"],
                    "material": _modeled(r["pos"]) and (pa or 0.0) != (pb or 0.0),
                    "detail": f"{r['injury']}: {sig(a)} -> {sig(b)}  (out-prob {pa} -> {pb})"})
    # --- depth-chart starters (skip if the source changed team_site<->espn: slot names differ) ---
    if prev.get("depth_source") == cur.get("depth_source"):
        ps, cs = _starters(prev), _starters(cur)
        for slot in sorted(set(ps) | set(cs)):
            if ps.get(slot) != cs.get(slot):
                pos = slot.split(":")[1]
                out.append({"kind": "depth", "player": cs.get(slot), "pos": pos,
                            "material": _modeled(pos),
                            "detail": f"{slot}: {ps.get(slot)} -> {cs.get(slot)}"})
    # --- Active-roster membership ---
    pa = {r["player"]: r for r in prev["roster"] if r["section"] == "Active"}
    ca = {r["player"]: r for r in cur["roster"] if r["section"] == "Active"}
    for name in sorted(set(pa) ^ set(ca)):
        r = ca.get(name) or pa.get(name)
        out.append({"kind": "roster", "player": name, "pos": r["pos"], "material": _modeled(r["pos"]),
                    "detail": "joined Active" if name in ca else "left Active"})
    return out


def leans(cur):
    """Inputs: cur snapshot. Output: list of modeled players with any injury-report entry,
    each with out_prob (None = status unpublished) and practice cells -- the pre-inactives lean."""
    return [{"player": r["player"], "pos": r["pos"], "injury": r["injury"],
             "practice": r["practice"], "status": r["status"],
             "out_prob": out_probability(r["status"], r["practice"])}
            for r in cur["injuries"]["team"] if _modeled(r["pos"])]


def current_week(sched, forced=None):
    """Earliest regular-season week that still has an unscored game (or --week)."""
    if forced:
        return forced
    reg = sched[sched.game_type == "REG"]
    return int(reg[reg.away_score.isna()].week.min())


def main():
    forced = int(sys.argv[sys.argv.index("--week") + 1]) if "--week" in sys.argv else None
    sched = pd.read_csv(SCHEDULE)
    week = current_week(sched, forced)
    games = {}
    for _, g in sched[(sched.week == week) & (sched.game_type == "REG")].iterrows():
        games[g.away_team] = games[g.home_team] = g
    overrides = {}
    if os.path.exists(OVERRIDES):
        overrides = json.load(open(OVERRIDES, encoding="utf-8"))

    def load(sub, fn):
        with open(os.path.join(STATUS_DIR, sub, fn), encoding="utf-8") as f:
            return apply_overrides(json.load(f), overrides)

    report = {"generated_at": datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds"),
              "week": week, "teams": {}, "games_to_resim": {}, "leans": {}}
    for fn in sorted(os.listdir(os.path.join(STATUS_DIR, "latest"))):
        code = fn[:-5]
        cur = load("latest", fn)
        report["leans"][code] = leans(cur)
        if not os.path.exists(os.path.join(STATUS_DIR, "previous", fn)):
            report["teams"][code] = {"baseline": True, "changes": []}
            continue
        ch = team_changes(load("previous", fn), cur)
        report["teams"][code] = {"baseline": False, "changes": ch}
        g = games.get(code)
        if g is not None and any(c["material"] for c in ch) and not has_kicked_off(g):
            report["games_to_resim"].setdefault(f"{g.away_team}_{g.home_team}", []).append(code)

    with open(os.path.join(STATUS_DIR, "changes_latest.json"), "w", encoding="utf-8") as f:
        json.dump(report, f, indent=1)
    changed = {c: t["changes"] for c, t in report["teams"].items() if t["changes"]}
    if changed:
        entry = {k: report[k] for k in ("generated_at", "week", "games_to_resim")}
        entry["changes"] = changed
        with open(os.path.join(STATUS_DIR, "changes_log.jsonl"), "a", encoding="utf-8") as f:
            f.write(json.dumps(entry) + "\n")

    print(f"Week {week}")
    base = [c for c, t in report["teams"].items() if t["baseline"]]
    if base:
        print(f"  baseline only (no previous snapshot): {len(base)} teams")
    for code, chs in changed.items():
        for c in chs:
            print(f"  {'**' if c['material'] else '  '} {code:3} {c['kind']:7} {c['player']} ({c['pos']}): {c['detail']}")
    if report["games_to_resim"]:
        print("\nRe-sim needed (not yet kicked off):")
        for key in report["games_to_resim"]:
            print(f"  venv\\Scripts\\python.exe scripts/simulation_runners/resim_games_2026.py {week} {key}")
    else:
        print("\nNo material changes in unplayed games.")
    n = sum(1 for v in report["leans"].values() for p in v if p["out_prob"] not in (None, 0.0))
    print(f"Leans: {n} modeled players with out-prob > 0 (see changes_latest.json)")


if __name__ == "__main__":
    main()
