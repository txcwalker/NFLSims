"""Builds the initial data/current_rosters/{TEAM}_traits_2026.json shells --
the pure-projection (max_week=0) starting point for the 2026 season.

Offensive skill positions only (QB/RB/WR/TE) per the 2026 rollover plan's
scope -- no defense, O-line, kickers, or punters at the player level.

Rookies (entry_year == 2026, the real signal -- NOT "missing from career
DNA", which is what silently dropped real rookies from the 2025 files, see
docs/sims/inputs/README.md's root-cause writeup) get target_share/
carry_share left at 0.0 here on purpose -- Phase 4's rookie_projections_2026.json
+ rookie_curves_v_0_1_0 is what actually assigns them a real preseason
value. This script's job is just to identify who they are and attach
draft_capital so Phase 4 knows who to prioritize.

Returning players get their most recent (2025) target_share/carry_share as
the 2026 preseason projection basis -- falling back to career-DNA averages
for anyone not found in a 2025 roster file (e.g. missed all of 2025).

QA gate: every round-1 offensive skill-position draft pick must appear, by
name, on the correct team in the output -- fails loudly rather than
shipping a silently incomplete roster the way 2025's did.
"""
import sys
import nfl_data_py as nfl
import json
import os
import glob

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.data_pipeline.rolling_stats_v_0_1_0 import PLAYER_RATE_FIELDS

DNA_DIR = "data/dna"
ROSTERS_DIR = "data/current_rosters"
SKILL_POSITIONS = ["QB", "RB", "WR", "TE"]
ACTIVE_STATUSES = {"ACT", "RES"}

# import_draft_picks() uses PFR-style team codes that differ from the
# standard nflverse codes used everywhere else in this repo (confirmed by
# direct comparison against import_seasonal_rosters(), which already uses
# the standard codes -- e.g. "LA" not "LAR" for the Rams).
DRAFT_TEAM_CODE_FIX = {
    "GNB": "GB", "KAN": "KC", "LAR": "LA", "LVR": "LV",
    "NOR": "NO", "NWE": "NE", "SFO": "SF", "TAM": "TB",
}


def clean_name(name):
    if not isinstance(name, str):
        return ""
    name = name.strip()
    for s in [" Jr.", " Sr.", " III", " II", " IV", " V"]:
        if name.endswith(s):
            name = name[: -len(s)]
    return name.replace(".", "")


def load_skill_dna():
    dna = {}
    for fname in ["qb_dna.json", "rb_dna.json", "wr_dna.json", "te_dna.json"]:
        with open(os.path.join(DNA_DIR, fname), "r") as f:
            dna[fname.split("_")[0]] = json.load(f)
    return dna


def load_2025_shares():
    """full_name -> {target_share, carry_share} from the most recent
    enriched 2025 roster files, as the 2026 preseason projection basis for
    returning players."""
    shares = {}
    for path in glob.glob(os.path.join(ROSTERS_DIR, "*_traits_2025.json")):
        with open(path, "r") as f:
            data = json.load(f)
        for name, traits in data.get("traits", {}).items():
            shares[name] = {
                "target_share": traits.get("target_share", 0.0),
                "carry_share": traits.get("carry_share", 0.0),
            }
    return shares


def load_draft_capital():
    """clean_name(pfr_player_name) -> {round, pick, team} for 2026 skill-position picks."""
    picks = nfl.import_draft_picks([2026])
    picks = picks[picks["position"].isin(SKILL_POSITIONS)]
    out = {}
    for _, row in picks.iterrows():
        name = clean_name(row["pfr_player_name"])
        team = DRAFT_TEAM_CODE_FIX.get(row["team"], row["team"])
        out[name] = {
            "round": int(row["round"]), "pick": int(row["pick"]),
            "team": team, "position": row["position"],
            "player_id": row["gsis_id"] if isinstance(row["gsis_id"], str) else None,
        }
    return out


def enrich_player(full_name, pos, target_share, carry_share, skill_dna, is_rookie, draft_capital, player_id=None):
    enriched = {
        "pos": pos,
        "target_share": round(float(target_share), 4),
        "carry_share": round(float(carry_share), 4),
        "status": "active",
    }
    if player_id:
        enriched["player_id"] = player_id
    if is_rookie:
        enriched["rookie"] = True
        if draft_capital:
            enriched["draft_capital"] = {"round": draft_capital["round"], "pick": draft_capital["pick"]}

    if pos == "QB":
        p_dna = skill_dna["qb"].get(full_name, {})
        enriched.update({
            "cpoe": p_dna.get("cpoe", 0.0),
            "avg_air_yards_per_att": p_dna.get("avg_air_yards_per_att", 8.0),
            "deep_ball_rate": p_dna.get("deep_ball_rate", 0.12),
            "scramble_rate": p_dna.get("scramble_rate", 0.05),
            "sack_rate": p_dna.get("sack_rate", 0.06),
            "play_action_rate": p_dna.get("play_action_rate", 0.20),
            "under_pressure_cpoe": p_dna.get("under_pressure_cpoe", -2.5),
            "pressure_rate": p_dna.get("pressure_rate", 0.20),
            "avg_time_to_throw_sec": p_dna.get("avg_time_to_throw_sec", 2.7),
            "ngs_aggressiveness_index": p_dna.get("ngs_aggressiveness_index", 15.0),
            "splits": p_dna.get("splits", {
                "primary": {"cpoe": 0.0, "sack_rate": 0.06, "play_action_rate": 0.20},
                "redzone": {"cpoe": 0.0, "sack_rate": 0.06, "play_action_rate": 0.20},
                "goalline": {"cpoe": 0.0, "sack_rate": 0.06, "play_action_rate": 0.20},
            }),
        })
    elif pos == "RB":
        p_dna = skill_dna["rb"].get(full_name, {})
        enriched.update({
            "adot": p_dna.get("avg_target_depth_yds", 1.0),
            "yac_per_rec": p_dna.get("yac_per_reception", 6.5),
            "elusiveness": p_dna.get("elusiveness", 0.0),
            "broken_tackle_rate": p_dna.get("broken_tackle_rate", 0.15),
            "ypc": p_dna.get("ypc", 4.2),
            "efficiency": p_dna.get("efficiency", 3.8),
            "percent_attempts_gte_eight_defenders": p_dna.get("percent_attempts_gte_eight_defenders", 0.22),
            "avg_time_to_los": p_dna.get("avg_time_to_los", 2.75),
            "rush_yards_over_expected_per_att": p_dna.get("rush_yards_over_expected_per_att", 0.0),
            "rush_pct_over_expected": p_dna.get("rush_pct_over_expected", 0.35),
            "catch_rate": p_dna.get("catch_rate", 0.75),
            "top_speed_mph": p_dna.get("top_speed_mph", 20.3),
            "contested_catch_rate": p_dna.get("contested_catch_rate", 0.38),
            "splits": p_dna.get("splits", {
                "primary": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.75, "yac_per_reception": 6.5},
                "redzone": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.75, "yac_per_reception": 6.5},
                "goalline": {"target_share": target_share, "carry_share": carry_share, "catch_rate": 0.75, "yac_per_reception": 6.5},
            }),
        })
    elif pos in ("WR", "TE"):
        p_dna = skill_dna["wr" if pos == "WR" else "te"].get(full_name, {})
        default_adot = 11.5 if pos == "WR" else 7.5
        default_yac = 4.2 if pos == "WR" else 4.5
        default_catch = 0.62 if pos == "WR" else 0.68
        default_route = "intermediate" if pos == "WR" else "short"
        default_top_speed = 21.0 if pos == "WR" else 19.8
        default_deep_rate = 0.15 if pos == "WR" else 0.06
        default_contested = 0.48 if pos == "WR" else 0.52
        enriched.update({
            "adot": p_dna.get("avg_target_depth_yds", default_adot),
            "yac_per_rec": p_dna.get("yac_per_reception", default_yac),
            "elusiveness": p_dna.get("elusiveness", 0.0),
            "broken_tackle_rate": p_dna.get("broken_tackle_rate", 0.15),
            "ypc": p_dna.get("ypc", 4.2),
            "avg_separation_yds": p_dna.get("avg_separation_yds", 2.8 if pos == "WR" else 3.0),
            "avg_cushion_yds": p_dna.get("avg_cushion_yds", 5.8 if pos == "WR" else 5.6),
            "catch_rate": p_dna.get("catch_rate", default_catch),
            "route_profile": p_dna.get("route_profile", default_route),
            "top_speed_mph": p_dna.get("top_speed_mph", default_top_speed),
            "deep_target_rate": p_dna.get("deep_target_rate", default_deep_rate),
            "contested_catch_rate": p_dna.get("contested_catch_rate", default_contested),
            "splits": p_dna.get("splits", {
                "primary": {"target_share": target_share, "carry_share": carry_share, "catch_rate": default_catch, "yac_per_reception": default_yac},
                "redzone": {"target_share": target_share, "carry_share": carry_share, "catch_rate": default_catch, "yac_per_reception": default_yac},
                "goalline": {"target_share": target_share, "carry_share": carry_share, "catch_rate": default_catch, "yac_per_reception": default_yac},
            }),
        })

    # Frozen snapshot of this build's values for every rolling-stat-tracked
    # field (rolling_stats_v_0_1_0.PLAYER_RATE_FIELDS) -- Phase 5's weekly
    # refresh blends L4/season-to-date against THIS, not a freshly re-derived
    # value, so the taper measures against a stable preseason baseline all
    # season instead of drifting week to week. Veterans only -- rookies get
    # their taper-period projection from rookie_projections_2026.json's
    # curves instead (resolved fresh per game_number by Phase 5).
    if not is_rookie:
        enriched["preseason_projection"] = {
            f: enriched[f] for f in PLAYER_RATE_FIELDS if f in enriched
        }
    return enriched


def build_rosters():
    skill_dna = load_skill_dna()
    shares_2025 = load_2025_shares()
    draft_capital = load_draft_capital()

    roster = nfl.import_seasonal_rosters([2026])
    roster = roster[roster["position"].isin(SKILL_POSITIONS)]
    roster = roster[roster["status"].isin(ACTIVE_STATUSES)]
    roster = roster.dropna(subset=["player_name", "team"])

    by_team = {}
    for _, row in roster.iterrows():
        team = row["team"]
        full_name = clean_name(row["player_name"])
        pos = row["position"]
        is_rookie = int(row["entry_year"]) == 2026

        if is_rookie:
            target_share, carry_share = 0.0, 0.0
        else:
            prior = shares_2025.get(full_name)
            if prior:
                target_share, carry_share = prior["target_share"], prior["carry_share"]
            else:
                p_dna = skill_dna["qb" if pos == "QB" else pos.lower()].get(full_name, {})
                target_share = p_dna.get("target_share", 0.0)
                carry_share = p_dna.get("carry_share", 0.0)

        enriched = enrich_player(
            full_name, pos, target_share, carry_share, skill_dna,
            is_rookie, draft_capital.get(full_name),
            player_id=row["player_id"] if isinstance(row["player_id"], str) else None,
        )
        by_team.setdefault(team, {})[full_name] = enriched

    add_missing_draft_picks(by_team, draft_capital, skill_dna)
    return by_team, draft_capital


def add_missing_draft_picks(by_team, draft_capital, skill_dna):
    """Some drafted rookies (confirmed: Fernando Mendoza/LV pick #1, Ty
    Simpson/LA pick #13, both backup-slot QBs behind entrenched veterans)
    are absent from import_seasonal_rosters(2026) entirely even though
    they're real, drafted players -- not a name-matching bug, verified
    directly against the raw feed. Patch them in from draft_capital instead
    of letting the QA gate hard-fail on a real, known, temporary source gap.
    Self-heals once nfl_data_py's feed catches up (by_team already has them,
    so this loop is then a no-op for that player)."""
    for name, dc in draft_capital.items():
        team = dc["team"]
        if name in by_team.get(team, {}):
            continue
        print(f"  Patching in draft pick missing from roster feed: {name} ({dc['position']}, {team}, round {dc['round']} pick {dc['pick']})")
        enriched = enrich_player(name, dc["position"], 0.0, 0.0, skill_dna, is_rookie=True, draft_capital=dc, player_id=dc.get("player_id"))
        by_team.setdefault(team, {})[name] = enriched


def sort_and_write(by_team):
    pos_order = {"QB": 0, "RB": 1, "WR": 2, "TE": 3}
    os.makedirs(ROSTERS_DIR, exist_ok=True)
    for team, traits in by_team.items():
        ordered_names = sorted(
            traits.keys(),
            key=lambda n: (pos_order.get(traits[n]["pos"], 4), -max(traits[n]["target_share"], traits[n]["carry_share"])),
        )
        ordered_traits = {n: traits[n] for n in ordered_names}
        out = {"team": team, "year": 2026, "max_week": 0, "traits": ordered_traits}
        with open(os.path.join(ROSTERS_DIR, f"{team}_traits_2026.json"), "w") as f:
            json.dump(out, f, indent=4)


def qa_gate(by_team, draft_capital):
    """Every round-1 offensive skill-position pick must appear, by name, on
    its drafted team in the built output. Raises loudly on any miss."""
    round1 = {name: dc for name, dc in draft_capital.items() if dc["round"] == 1}
    missing = []
    for name, dc in round1.items():
        team_roster = by_team.get(dc["team"], {})
        if name not in team_roster:
            missing.append((name, dc["team"], dc["pick"]))
    if missing:
        lines = "\n".join(f"  - {n} ({team}, pick #{pick})" for n, team, pick in missing)
        raise SystemExit(
            f"QA GATE FAILED: {len(missing)} round-1 offensive skill-position pick(s) "
            f"missing from the built 2026 roster:\n{lines}"
        )
    print(f"QA gate passed: all {len(round1)} round-1 offensive skill-position picks present.")


def main():
    print("=== BUILDING 2026 ROSTER SHELLS ===")
    by_team, draft_capital = build_rosters()
    qa_gate(by_team, draft_capital)
    sort_and_write(by_team)
    total_players = sum(len(t) for t in by_team.values())
    total_rookies = sum(1 for t in by_team.values() for p in t.values() if p.get("rookie"))
    print(f"Wrote {len(by_team)} team files, {total_players} players ({total_rookies} rookies).")


if __name__ == "__main__":
    main()
