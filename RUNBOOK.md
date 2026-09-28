# RUNBOOK.md — Recurring Manual Operations

# Status: live | 2026-09-23

The chores that currently have to be run **by hand** to keep the NFL sims, DFS
projections, and ownership model current during the season. These are
operations, not roadmap work (roadmap → `PROJECT_ROADMAP.md` / `GOAL_TRACKER.md`).

**The NFL week starts Tuesday** (the day after MNF).

Every task has a **Mode**: `manual` (someone has to run it) or `automated`
(it runs on a schedule and is only listed so its health can be checked). The goal
is to move as many rows to `automated` as possible. This file is the list of
automation candidates. When a task gets automated, flip its Mode and write the
decision in [DECISIONS.md](DECISIONS.md).

All commands run from the repo root with `venv\Scripts\python.exe` (see AGENTS.md §8,
where a bare `python` can pick up the wrong interpreter). `<W>` = the week being
prepared, `<W-1>` = the week that just finished.

---

## Weekly — Tuesday (close out last week, build next week)

Run top to bottom. Order matters: each group feeds the next.

### A. Last week's real data

| # | Task | How | Mode | Depends on |
|---|---|---|---|---|
| A1 | Refresh player/team DNA from last week's real PBP + NGS | `scripts/roster_management/refresh_weekly_dna_v_0_1_0.py 2026 <W-1>` | manual | nflverse has published `<W-1>` data |
| A2 | Rebuild real season-to-date standings / stats / leaders (Current Season page) | `scripts/simulation_runners/build_actual_season_stats_2026.py` | manual | — |
| A3 | Rerun the rest-of-season sim + all season reports | `scripts/simulation_runners/regenerate_2026_reports.py` (slow; always use this, never `run_full_season_sim_2026.py` alone) | manual | A1 |

### B. Ownership model (retrain on the newest settled slates)

| # | Task | How | Mode | Depends on |
|---|---|---|---|---|
| B1 | Download DK **Export Full Standings** CSV for each contest to track, save it into its slate folder | draftkings.com → contest → Export → Full Standings. Naming: `data/dfs_ownership/README.md` step 2 | manual (browser) | contests settled |
| B2 | Rebuild the ownership dataset | `scripts/dfs_ownership/build_ownership_dataset.py` | manual | B1 |
| B3 | Settle paper trades / analyze the field / sim replay (Evaluation page) | `scripts/dfs_ownership/score_paper_entries.py --year 2026 --week <W-1>`, then `eval_field.py` and `sim_replay_field.py` with the same flags | manual | B2 (sim replay also needs the `<W-1>` week sim on disk) |
| B4 | Retrain both ownership models (classic + showdown) | `scripts/dfs_ownership/train_ownership_model.py` (always saves, prints a coverage caveat; see its docstring) | manual | B2 |
| B5 | *(optional)* Backtest the heuristic against actuals | `scripts/dfs_ownership/calibrate_ownership_model.py` | manual | B2 |

### C. Build next week's DFS projections

| # | Task | How | Mode | Depends on |
|---|---|---|---|---|
| C1 | Build the `<W>` override sheets from season_long + status ledger | `scripts/roster_management/build_week_overrides_v_0_1_0.py <W>` | manual | A1 |
| C2 | Compile the sheets into the DFS roster tree | `scripts/roster_management/apply_team_week_overrides_v_0_1_0.py <W>` | manual | C1 |
| C3 | Run the week sim (feeds optimizers + ownership inputs) | `scripts/simulation_runners/run_week_sim_2026.py <W>` (10k iterations default) | manual | C2 |

---

## Daily — Wednesday through Sunday (and Monday before MNF)

| # | Task | How | Mode | Depends on |
|---|---|---|---|---|
| D1 | **Injury news**: check practice reports / designations, then set Active/Inactive | Check the news (manual). Toggle in the DFS site's **Game Explorer** (writes the sticky `data/overrides/2026/dfs_status_ledger.json`). Then C1 → C2 for the week, and `resim_games_2026.py <W> <AWAY>_<HOME> ...` for affected games only (cheaper than a full C3) | manual | C3 done once this week |
| D2 | **Vegas lines**: pull the latest consensus spread / total / moneylines | DFS site navbar **Refresh Vegas** button (`POST /api/refresh_vegas_lines`), which overwrites `data/external/schedule_2026.csv` from nflverse | manual | DFS API (8002) running |
| D3 | **Pre-lock salary snapshot** (slate days only): freeze DK pool + Vegas as ownership features | `scripts/dfs_ownership/snapshot_slate_salaries.py --week <W>` (`--main` for the classic main slate; `--backfill` if a slate was missed) | manual | D2 (Vegas is read from the schedule CSV) |

**Game-day order (Thu / Sun / Mon):** D2 → D1 (final inactives ~90 min before kick) → resim affected games → D3 before lock.

---

## Monthly / as-needed

| # | Task | How | Mode | Depends on |
|---|---|---|---|---|
| M1 | Check roster sheets for players who left their team (trades / cuts) | `scripts/roster_management/audit_2026_roster_staleness.py` (report only) | manual | — |
| M2 | Refresh the nfl.com 53-man roster cache | `scripts/roster_management/scrape_nfl_rosters_v_0_1_0.py` | manual | — |
| M3 | Back up critical untracked assets | `scripts/data_utils/backup_critical_assets.py` (needs B2 credentials, see WORKLOG) | manual | — |

---

## Planned — prediction markets (priority: ASAP, see DECISIONS.md 2026-09-23)

| # | Task | How | Mode | Depends on |
|---|---|---|---|---|
| P1 | Pull Kalshi / Polymarket NFL game prices | **Not built.** Design options in `docs/implementation_plans/sim_vs_market_comparison_plan.md` (Option C) | automated (target: hourly, daily minimum) | — |

Must be **automated from day one**. A manual pull defeats the purpose for a price
that moves. If hourly isn't feasible, daily is the floor.

---

## Automation candidates (ranked by payoff ÷ effort; a suggestion, not a decision)

1. **D2 Vegas refresh.** One nflverse call, already a function (`src/data_pipeline/vegas_lines_refresh.py`). Easiest win. Doing it alongside P1 means one scheduler for all "market" data.
2. **P1 prediction markets.** Required automated anyway.
3. **A1 → A2 → C1 → C2 → C3 Tuesday chain.** Deterministic scripts, but A1 waits on nflverse publishing, so it needs a "data available yet?" check before running.
4. **B2 → B4 ownership retrain.** Scriptable, but B1 (DK export) is a manual browser download. Automating B1 is the real blocker (see `docs/todo/dk_contest_api.md`).
5. **D1 injury news.** Hardest. Reading news is judgment; an automated feed could *flag* designation changes, but the Active/Inactive call stays human.
