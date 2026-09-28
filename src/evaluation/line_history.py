"""Append-only Vegas line ledger -> per-game opening and closing lines.

Why (2026-09-25): nflverse's schedule feed (data/external/schedule_{year}.csv,
refreshed by src/data_pipeline/vegas_lines_refresh.py) holds ONE line per game
and overwrites it on every pull, so the opener is lost the moment the line
moves. The Evaluation tab grades the sim against BOTH the opener (soft lines
are where the edge is) and the closer (the sharpest benchmark / CLV), so we
keep every line we've ever seen here.

Files (both committed -- a captured line can't be regenerated later):
  data/eval/{year}/line_history.csv    machine-written, append-only. One row
                                       per game each time its line CHANGES.
  data/eval/{year}/line_overrides.csv  hand-edited. True open/close numbers
                                       from outside nflverse (e.g. what you saw
                                       at the book). Always wins over the ledger.

Line convention (same as nflverse, used everywhere in src/evaluation/):
  spread_line > 0  -> HOME team favored by that many points (GB -4.5 = 4.5)
  home covers      -> (home_score - away_score) > spread_line
  over hits        -> (home_score + away_score) > total_line

Opening/closing resolution (resolve_open_close):
  open  = override tagged 'open', else the EARLIEST ledger capture before kickoff
  close = override tagged 'close', else the LATEST ledger capture at any time
          (nflverse keeps the closing number frozen after kickoff, so a
          post-game pull is a valid close; open must be pre-kickoff)
"""
import os
import time

import pandas as pd

BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

LINE_COLS = ["spread_line", "total_line", "home_moneyline", "away_moneyline",
             "home_spread_odds", "away_spread_odds", "over_odds", "under_odds"]
LEDGER_COLS = ["game_id", "week", "captured_at", "source"] + LINE_COLS
OVERRIDE_COLS = ["game_id", "tag", "spread_line", "total_line", "note"]


def ledger_path(year, base_dir=BASE_DIR):
    """Inputs: year (int), base_dir (repo root). Output: str path to line_history.csv."""
    return os.path.join(base_dir, "data", "eval", str(year), "line_history.csv")


def overrides_path(year, base_dir=BASE_DIR):
    """Inputs: year (int), base_dir (repo root). Output: str path to line_overrides.csv."""
    return os.path.join(base_dir, "data", "eval", str(year), "line_overrides.csv")


def load_ledger(year, base_dir=BASE_DIR):
    """Inputs: year, base_dir. Output: ledger DataFrame (LEDGER_COLS; empty if
    no file yet). captured_at is unix seconds (float)."""
    p = ledger_path(year, base_dir)
    if not os.path.exists(p):
        return pd.DataFrame(columns=LEDGER_COLS)
    return pd.read_csv(p)


def load_overrides(year, base_dir=BASE_DIR):
    """Inputs: year, base_dir. Output: overrides DataFrame (OVERRIDE_COLS; empty
    if no file). Blank spread/total cells mean "no override for that field"."""
    p = overrides_path(year, base_dir)
    if not os.path.exists(p):
        return pd.DataFrame(columns=OVERRIDE_COLS)
    df = pd.read_csv(p, comment="#")
    df["tag"] = df["tag"].astype(str).str.strip().str.lower()
    return df


def append_snapshot(sched_df, year, source="nflverse", captured_at=None, base_dir=BASE_DIR):
    """Record a schedule pull into the ledger, one row per game whose line
    differs from that game's most recent ledger row.

    Inputs:
      sched_df    -- schedule DataFrame (nflverse columns: game_id, week, LINE_COLS)
      year        -- season (selects the ledger file)
      source      -- label for provenance ('nflverse', 'nflverse_git', ...)
      captured_at -- unix seconds the lines were observed (default: now)
    Output: int, number of rows appended. Writes line_history.csv atomically.
    Purpose: dedupe-on-change keeps the ledger to real line moves, not one
    row per game per refresh. Games with no spread AND no total are skipped.
    Note: a snapshot is only appended if captured_at is >= that game's latest
    ledger capture -- back-filling OLDER snapshots goes through the same call
    but is compared against the latest row, so backfill oldest-first.
    """
    captured_at = time.time() if captured_at is None else float(captured_at)
    ledger = load_ledger(year, base_dir)
    last = ledger.sort_values("captured_at").groupby("game_id").tail(1).set_index("game_id")

    new_rows = []
    for _, r in sched_df.iterrows():
        if pd.isna(r.get("spread_line")) and pd.isna(r.get("total_line")):
            continue
        vals = {c: r.get(c) for c in LINE_COLS}
        gid = r["game_id"]
        if gid in last.index:
            prev = last.loc[gid]
            same = all((pd.isna(prev[c]) and pd.isna(vals[c])) or prev[c] == vals[c] for c in LINE_COLS)
            if same:
                continue
        new_rows.append({"game_id": gid, "week": int(r["week"]), "captured_at": captured_at,
                         "source": source, **vals})

    if not new_rows:
        return 0
    out = pd.concat([ledger, pd.DataFrame(new_rows)], ignore_index=True)[LEDGER_COLS]
    p = ledger_path(year, base_dir)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    tmp = p + ".tmp"
    out.to_csv(tmp, index=False)
    os.replace(tmp, p)
    return len(new_rows)


def resolve_open_close(ledger, overrides, kickoffs):
    """Collapse the ledger + overrides to one opening and one closing line per game.

    Inputs:
      ledger    -- load_ledger() DataFrame
      overrides -- load_overrides() DataFrame
      kickoffs  -- dict game_id -> kickoff unix seconds (None = unknown)
    Output: DataFrame indexed by game_id with columns
      open_spread, open_total, open_src, close_spread, close_total, close_src,
      close_home_ml, close_away_ml, close_home_spread_odds, close_away_spread_odds,
      close_over_odds, close_under_odds, open_home_ml, open_away_ml
      (*_src = 'override' | 'ledger' | None). Spread and total resolve
      independently, so an override can fix just one of them.
    Tricky bit: odds/moneylines only come from the ledger's closing row (the
    overrides file doesn't carry them); callers fall back to -110.
    """
    rows = {}
    game_ids = set(ledger["game_id"]) | set(overrides["game_id"])
    for gid in game_ids:
        g = ledger[ledger["game_id"] == gid].sort_values("captured_at")
        ko = kickoffs.get(gid)
        pre = g[g["captured_at"] < ko] if ko is not None else g
        first = pre.iloc[0] if len(pre) else None
        latest = g.iloc[-1] if len(g) else None
        rec = {}
        for kind, row in (("open", first), ("close", latest)):
            ov = overrides[(overrides["game_id"] == gid) & (overrides["tag"] == kind)]
            for fld in ("spread", "total"):
                v, src = None, None
                if len(ov) and not pd.isna(ov.iloc[-1][f"{fld}_line"]):
                    v, src = float(ov.iloc[-1][f"{fld}_line"]), "override"
                elif row is not None and not pd.isna(row[f"{fld}_line"]):
                    v, src = float(row[f"{fld}_line"]), "ledger"
                rec[f"{kind}_{fld}"] = v
                rec[f"{kind}_{fld}_src"] = src
        for c in ("home_moneyline", "away_moneyline", "home_spread_odds", "away_spread_odds",
                  "over_odds", "under_odds"):
            short = c.replace("moneyline", "ml")
            rec[f"close_{short}"] = None if latest is None or pd.isna(latest[c]) else float(latest[c])
        # Opening moneylines (2026-09-25) -- for grading moneyline value bets
        # at the open. Same "earliest pre-kickoff capture" rule as open spread.
        for c in ("home_moneyline", "away_moneyline"):
            rec[f"open_{c.replace('moneyline', 'ml')}"] = None if first is None or pd.isna(first[c]) else float(first[c])
        rows[gid] = rec
    return pd.DataFrame.from_dict(rows, orient="index")
