"""How stacked were REAL winning DK Classic lineups? First look, weeks 1-2.

Why (2026-09-23): the optimizer A/Bs showed our sims' own correlations
build far fewer QB+2 stacks than the hand-set correlation table, and in our
own sim grading that cost almost nothing. Real GPP winners are widely
reported to be heavily stacked. Before adding any stacking rule (Cam wants
the sims, not operator rules, to drive lineups), this measures what real
fields actually did: stack structure by finish tier, small vs. large field.

For every entry in every settled main-slate standings CSV:
  qb_catchers  # of WR/TE on the QB's team        (0, 1, 2, 3+)
  qb_rb        RB on the QB's team                 (bool)
  bring_back   any non-DST player facing the QB    (bool)
  max_game     most players from any one game      (2..9)
  dst_vs_off   DST facing a rostered player        (bool)
Grouped by finish tier (top 0.1% / top 1% / top 10% / rest), per contest.

CAVEATS -- Cam's, and they're real: two weeks only; the same users enter
several of these contests with overlapping lineup sets, so rows are far
from independent; and "what winners did" is shaped by which games happened
to break open those two Sundays. Descriptive only -- not a rule generator.
Re-run mid-season / end of season when the sample means something.

Inputs: data/dfs_ownership/2026/week_{ww}/main_slate/*max.csv,
        salaries_prelock.csv (name -> team/pos), data/external/schedule_2026.csv
Output: printed tables + docs/eda_outputs/real_field_stacking/weeks_{...}.csv

Usage:
    venv\\Scripts\\python.exe scripts/eda/analyze_real_field_stacking.py 1 2
"""
import glob
import os
import sys
from collections import Counter

sys.path.insert(0, os.getcwd())

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402

from src.scrapers.dk_scraper import normalize_player_name as N  # noqa: E402
from scripts.dfs_ownership.standings_parser import (  # noqa: E402
    CLASSIC_SLOT_RE, parse_filename, read_standings, resolve_lineup_name,
)

YEAR = 2026
TIERS = [(0.001, "top0.1%"), (0.01, "top1%"), (0.10, "top10%"), (1.01, "rest")]


def size_bucket(field):
    """Cam's field-size classes (2026-09-24): small <= 2,000 entries,
    large >= 5,000, medium in between (plays closer to large than small).
    Provisional -- revisit with real research on where GPP dynamics shift."""
    return "small" if field <= 2000 else ("large" if field >= 5000 else "medium")


def team_lookup(week):
    """{normalized name: (team, pos)} from the week's DK salary snapshot.
    A name DK lists twice (different teams/pos) is ambiguous and dropped."""
    sal = pd.read_csv(os.path.join("data", "dfs_ownership", str(YEAR), f"week_{week:02d}",
                                   "main_slate", "salaries_prelock.csv"))
    seen, out = Counter(N(n) for n in sal["name"].drop_duplicates()), {}
    for r in sal.drop_duplicates(["name", "team", "pos"]).itertuples():
        if seen[N(r.name)] == 1:
            out[N(r.name)] = (r.team, r.pos)
    return out


def opponents(week):
    s = pd.read_csv(os.path.join("data", "external", f"schedule_{YEAR}.csv"))
    s = s[(s["week"] == week) & (s["game_type"] == "REG")]
    opp = {}
    for r in s.itertuples():
        opp[r.away_team], opp[r.home_team] = r.home_team, r.away_team
    return opp


def lineup_features(lineup, teams, opp):
    """Inputs: DK Lineup string, name lookup, opponent map.
    Output: feature dict, or None if any player can't be resolved (skipped,
    counted by the caller -- never guessed)."""
    players = []
    for slot, raw in CLASSIC_SLOT_RE.findall(lineup):
        key, dst_team = resolve_lineup_name(raw.strip(), N)
        if dst_team:
            players.append((dst_team, "DST"))
        elif key in teams:
            players.append(teams[key])
        else:
            return None
    if len(players) != 9:
        return None
    qb = next((t for t, p in players if p == "QB"), None)
    if qb is None:
        return None
    catchers = sum(1 for t, p in players if t == qb and p in ("WR", "TE"))
    games = Counter(frozenset((t, opp.get(t))) for t, p in players)
    dst = next(t for t, p in players if p == "DST")
    return {
        "qb_catchers": min(catchers, 3),
        "qb_rb": any(t == qb and p == "RB" for t, p in players),
        "bring_back": any(t == opp.get(qb) and p != "DST" for t, p in players),
        "max_game": max(games.values()),
        "dst_vs_off": any(opp.get(t) == dst for t, p in players if p != "DST"),
    }


def analyze(week):
    teams, opp = team_lookup(week), opponents(week)
    rows = []
    folder = os.path.join("data", "dfs_ownership", str(YEAR), f"week_{week:02d}", "main_slate")
    for f in sorted(glob.glob(os.path.join(folder, "*max.csv"))):
        meta = parse_filename(os.path.splitext(os.path.basename(f))[0])
        entries, _ = read_standings(f)
        entries = entries.dropna(subset=["rank"])
        field = int(entries["rank"].max())
        skipped = 0
        for rank, lineup in zip(entries["rank"].values, entries["lineup"].values):
            feat = lineup_features(lineup, teams, opp)
            if feat is None:
                skipped += 1
                continue
            pct = rank / field
            tier = next(label for cut, label in TIERS if pct <= cut)
            rows.append({"week": week, "contest": meta["contest_name"], "field": field,
                         "size": size_bucket(field), "tier": tier, **feat})
        print(f"  week {week} {meta['contest_name']:18s} field {field:>7,}  unresolved lineups {skipped:,}")
    return pd.DataFrame(rows)


def summarize(df):
    """Share of lineups with each stack feature, by field size x finish tier."""
    df = df.copy()
    for c in (0, 1, 2, 3):
        df[f"qb+{c}{'+' if c == 3 else ''}"] = df["qb_catchers"] == c
    df["qb+2 or more"] = df["qb_catchers"] >= 2
    df["game_stack_4+"] = df["max_game"] >= 4
    cols = ["qb+0", "qb+1", "qb+2", "qb+3+", "qb+2 or more", "qb_rb", "bring_back",
            "game_stack_4+", "dst_vs_off"]
    g = df.groupby(["size", "tier"])
    out = g[cols].mean().round(3)
    out.insert(0, "n", g.size())
    wanted = [(s, t) for s in ("small", "medium", "large") for _, t in TIERS]
    return out.reindex([k for k in wanted if k in out.index])


def main(weeks):
    df = pd.concat([analyze(w) for w in weeks], ignore_index=True)
    table = summarize(df)
    pd.set_option("display.width", 200)
    print("\nShare of lineups with each feature (rows: field size x finish tier), all weeks pooled")
    print(table.to_string())
    # Per-week split (Cam, 2026-09-24): each week has its own story -- wk1
    # scored big everywhere (stacks paid broadly), wk2 was low-scoring except
    # the chalky Dak/CeeDee DAL stack -- so a pooled number can be one
    # week's circumstance, not a general pattern.
    if len(weeks) > 1:
        for w in weeks:
            print(f"\n-- week {w} only")
            print(summarize(df[df["week"] == w])[["n", "qb+2 or more", "bring_back", "game_stack_4+"]].to_string())
    out_dir = os.path.join("docs", "eda_outputs", "real_field_stacking")
    os.makedirs(out_dir, exist_ok=True)
    table.to_csv(os.path.join(out_dir, f"weeks_{'_'.join(map(str, weeks))}.csv"))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    main([int(w) for w in sys.argv[1:]])
