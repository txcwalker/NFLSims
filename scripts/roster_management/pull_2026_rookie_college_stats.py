"""Replaces the position-average placeholder stats on every DRAFTED 2026
skill-position rookie (the 80 with real draft_capital, not the 145
undrafted-free-agent rookies also promoted this session -- see
add_2026_missing_skill_players.py) with their real 2024+2025 college
production, pulled from the CollegeFootballData.com (CFBD) API.

Context (2026-08-20): Cam asked whether college stats could be automated
instead of hand-sourcing pages like Sports Reference's CFB site (which
explicitly prohibits scraping/bots in its Terms of Use). CFBD is the right
tool -- free API, official Python client (`cfbd` on PyPI), and critically
its own /draft/picks endpoint returns each pick's `college_athlete_id`
directly, so this script can join our roster's (round, pick) draft_capital
straight to CFBD's player IDs -- no fuzzy name matching needed at all,
which sidesteps every Jr./Sr./nickname-formatting headache that's bitten
other scripts in this pipeline.

IMPORTANT CAVEAT, read before running -- confirmed by inspecting the `cfbd`
package's response models directly (no API key needed for that, just to
install and introspect):  CFBD's season-overview endpoint gives real
COUNTING production (rushing yards/carries/TDs, receiving yards/
receptions/TDs, passing yards/attempts/completions/TDs) plus `usage` (share
of team PLAYS the player was involved in, by down/situation) and `ppa`
(predicted points added, college's native EPA-equivalent). It does NOT
expose the NFL Next Gen Stats-style tracking data our preseason_projection
schema mostly wants (avg_separation_yds, yac_per_rec, adot/air-yards depth,
elusiveness, broken_tackle_rate) -- those come from player-tracking
cameras the public college data ecosystem doesn't have an equivalent for.
Realistically computable from real college data: `ypc` for RB (rushing
yards / carries). Everything else stays on its current position-average
default -- this script only overwrites a field when it has a real number
for it, same "partial fill, don't fabricate" philosophy as
add_2026_missing_skill_players.py's avg_separation_yds fallback. `usage`
and `ppa` are stored in `college_stats_2026.json` for reference even though
they don't map to any current TUNABLE_FIELD -- may be useful input to a
real rookie-grading model later.

STAT-NAME MAPPING IS UNVERIFIED AGAINST A LIVE RESPONSE -- built from CFBD's
well-documented standard category/stat-name conventions (rushing:
CAR/YDS/TD/YPC/LONG; receiving: REC/YDS/TD/YPR/LONG), but no live call has
actually been made yet (no API key available while writing this). The
script prints every raw category+stat name it receives for the first 3
matched players specifically so a real run immediately surfaces whether
STAT_NAME_MAP below needs adjusting -- check that output before trusting
the results at scale.

Requires: an isolated virtualenv (venv_cfbd/, gitignored) -- the `cfbd`
package pins pydantic<2, which conflicts with this repo's main venv/ (needs
pydantic 2.x for the FastAPI backend). Set up once:
    python -m venv venv_cfbd
    venv_cfbd/Scripts/python.exe -m pip install cfbd python-dotenv
Then add your free key (https://collegefootballdata.com/key) to the
repo-root .env file (gitignored, same convention as src/api/app.py):
    CFBD_API_KEY=your_key_here

Usage (from venv_cfbd, NOT the main venv -- this script never touches
nfl_data_py/pandas, only reads local JSON files + the CFBD API):
    venv_cfbd/Scripts/python.exe scripts/roster_management/pull_2026_rookie_college_stats.py

After it runs, regenerate both flat CSVs with the MAIN Python env (needs
nfl_data_py, not installed in venv_cfbd):
    python scripts/roster_management/export_preseason_overrides_v_0_1_0.py 2026
    python scripts/roster_management/export_zone_usage_overrides_v_0_1_0.py

~161 CFBD API calls total (1 for /draft/picks + up to 2 per matched player)
-- well inside the free tier's 1,000/month.
"""
import glob
import json
import os

from dotenv import load_dotenv

try:
    import cfbd
except ImportError:
    raise SystemExit(
        "cfbd not installed in this Python environment. This script must run "
        "under venv_cfbd/ (see this file's docstring), not the main venv -- "
        "cfbd pins pydantic<2, which conflicts with the main venv's FastAPI "
        "backend (needs pydantic 2.x) if installed there."
    )

DNA_DIR = "data/dna"
ROSTERS_DIR = "data/current_rosters"
DRAFT_YEAR = 2026
STAT_SEASONS = [2024, 2025]
OUT_PATH = os.path.join(DNA_DIR, "college_stats_2026.json")  # raw pull, for reference/debugging

# Best-known CFBD stat-name conventions -- VERIFY against the first-3-players
# debug dump this script prints on a real run, adjust here if they differ.
STAT_NAME_MAP = {
    "rushing": {"carries": "CAR", "yards": "YDS", "tds": "TD"},
    "receiving": {"receptions": "REC", "yards": "YDS", "tds": "TD"},
}


def load_api_client():
    load_dotenv()
    api_key = os.getenv("CFBD_API_KEY")
    if not api_key:
        raise SystemExit(
            "CFBD_API_KEY not found. Add it to the repo-root .env file:\n"
            "  CFBD_API_KEY=your_key_here\n"
            "Get a free key (no credit card) at https://collegefootballdata.com/key"
        )
    config = cfbd.Configuration(access_token=api_key)
    return cfbd.ApiClient(config)


def load_drafted_rookies():
    """overall pick number -> {name, team, pos} for every current_rosters
    rookie with real draft_capital -- the 80 actually-drafted skill-position
    picks, not the 145 UDFA rookies also promoted this session.

    NOTE (found 2026-08-20, first real run): our draft_capital["pick"] (from
    nfl_data_py's import_draft_picks(), see build_2026_rosters_v_0_1_0.py)
    is the OVERALL 1-257 draft slot. CFBD's DraftPick.pick is instead
    ROUND-RELATIVE (1-32/36/40/... within each round) -- only round 1
    happens to have identical relative and overall numbers, which is why an
    early version of this script's (round, pick) join only matched 10/80
    (every round-1 pick, nothing else). CFBD's DraftPick.overall is the
    field that actually matches our "pick" -- that's the real join key."""
    drafted = {}
    for path in sorted(glob.glob(os.path.join(ROSTERS_DIR, "*_traits_2026.json"))):
        data = json.load(open(path))
        for name, traits in data["traits"].items():
            dc = traits.get("draft_capital")
            if traits.get("rookie") and dc:
                drafted[dc["pick"]] = {
                    "name": name, "team": data["team"], "pos": traits["pos"], "round": dc["round"],
                }
    return drafted


def match_to_cfbd_ids(api_client, drafted):
    """One call: CFBD's own /draft/picks for 2026 gives college_athlete_id
    directly, keyed by overall pick number -- exact join against our own
    draft_capital["pick"], no name-matching fuzziness needed."""
    draft_api = cfbd.DraftApi(api_client)
    picks = draft_api.get_draft_picks(year=DRAFT_YEAR)
    cfbd_by_overall = {p.overall: p for p in picks}

    matched, unmatched = {}, []
    for overall, info in drafted.items():
        cfbd_pick = cfbd_by_overall.get(overall)
        if cfbd_pick is None:
            unmatched.append((info["name"], info["team"], info["round"], overall))
            continue
        matched[info["name"]] = {
            **info,
            "college_athlete_id": cfbd_pick.college_athlete_id,
            "cfbd_name": cfbd_pick.name,
            "college_team": cfbd_pick.college_team,
        }

    print(f"Matched {len(matched)}/{len(drafted)} drafted skill-position rookies to a CFBD college_athlete_id.")
    if unmatched:
        print(f"Unmatched ({len(unmatched)}) -- overall pick not found in CFBD's {DRAFT_YEAR} draft picks, skipping:")
        for name, team, rnd, overall in unmatched:
            print(f"  {name} ({team}), round {rnd}, overall #{overall}")
    return matched


def pull_season_overview(api_client, player_id, year):
    players_api = cfbd.PlayersApi(api_client)
    try:
        overview = players_api.get_player_season_overview(year=year, player_id=player_id)
    except cfbd.ApiException as e:
        if e.status == 404:
            return None
        raise
    categories = {}
    for cat in overview.box_score_stats.categories:
        categories[cat.name] = {stat.name: stat.value for stat in cat.stats}
    return {
        "games": overview.games,
        "categories": categories,
        "usage": overview.usage.to_dict() if overview.usage else None,
        "ppa": overview.ppa.to_dict() if overview.ppa else None,
    }


def pool_and_compute(pos, seasons_data):
    """seasons_data: list of per-year {categories: {...}} dicts (whichever
    years had real data). Pools raw counting stats across years (volume-
    weighted by construction, same convention as
    add_2026_missing_skill_players.py) then computes whichever
    TUNABLE_FIELDS are honestly derivable -- see this file's docstring for
    why that's a short list."""
    computed = {}

    def summed(category, stat_key):
        total = 0.0
        found = False
        for yr in seasons_data:
            cat = yr["categories"].get(category, {})
            raw = cat.get(STAT_NAME_MAP[category][stat_key])
            if raw is not None:
                try:
                    total += float(raw)
                    found = True
                except ValueError:
                    pass
        return total if found else None

    if pos == "RB":
        carries = summed("rushing", "carries")
        yards = summed("rushing", "yards")
        if carries and carries > 0 and yards is not None:
            computed["ypc"] = round(yards / carries, 3)

    return computed


def main():
    api_client = load_api_client()
    drafted = load_drafted_rookies()
    print(f"Found {len(drafted)} drafted skill-position rookies in current_rosters.")

    matched = match_to_cfbd_ids(api_client, drafted)

    raw_dump = {}
    no_data = []
    updates = {}  # name -> {team, computed fields}
    debug_printed = 0

    for name, info in matched.items():
        seasons_data = []
        for year in STAT_SEASONS:
            result = pull_season_overview(api_client, info["college_athlete_id"], year)
            if result is not None:
                seasons_data.append(result)

        raw_dump[name] = {"college_team": info["college_team"], "pos": info["pos"], "seasons": seasons_data}

        if debug_printed < 3 and seasons_data:
            print(f"\n[DEBUG] Raw categories/stats for {name} ({info['pos']}, {info['college_team']}):")
            for yr_data in seasons_data:
                print(f"  {yr_data['categories']}")
            debug_printed += 1

        if not seasons_data:
            no_data.append((name, info["team"], info["pos"]))
            continue

        computed = pool_and_compute(info["pos"], seasons_data)
        if computed:
            updates[name] = {"team": info["team"], "fields": computed}

    with open(OUT_PATH, "w") as f:
        json.dump(raw_dump, f, indent=2, default=str)
    print(f"\nWrote raw pull ({len(raw_dump)} players) to {OUT_PATH} for reference.")

    if no_data:
        print(f"\nNo 2024 or 2025 CFBD data found for {len(no_data)} players (true freshmen, JUCO/FCS transfers, etc.):")
        for name, team, pos in no_data:
            print(f"  {name} ({pos}, {team})")

    apply_updates(updates)


def apply_updates(updates):
    if not updates:
        print("\nNo computable field updates -- nothing written to current_rosters.")
        return

    by_team = {}
    for name, u in updates.items():
        by_team.setdefault(u["team"], []).append((name, u["fields"]))

    total_changed = 0
    for team, entries in by_team.items():
        path = os.path.join(ROSTERS_DIR, f"{team}_traits_2026.json")
        data = json.load(open(path))
        for name, fields in entries:
            traits = data["traits"].get(name)
            if traits is None:
                continue
            for field, value in fields.items():
                traits[field] = value
                traits.setdefault("preseason_projection", {})[field] = value
            total_changed += 1
        with open(path, "w") as f:
            json.dump(data, f, indent=4)

    print(f"\nApplied real college-stat fields to {total_changed} players across {len(by_team)} team files.")
    print("Next: regenerate both flat CSVs with the MAIN Python env (not venv_cfbd):")
    print("  python scripts/roster_management/export_preseason_overrides_v_0_1_0.py 2026")
    print("  python scripts/roster_management/export_zone_usage_overrides_v_0_1_0.py")


if __name__ == "__main__":
    main()
