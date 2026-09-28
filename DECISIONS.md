# DECISIONS.md — Key Decision Log

# Status: live | 2026-09-23

One entry per decision that shapes how the project works: what was decided, why,
and what else was considered. Newest first. Keep entries short. The detail lives
in WORKLOG / implementation plans, so link to them rather than repeating them.

Seeded 2026-09-23 with decisions already recorded across AGENTS.md / WORKLOG.md /
script docstrings. Older decisions were back-filled only where they still shape
day-to-day work; this is not a complete history.

**Entry format:**
```
### YYYY-MM-DD — Title
- **Decision:** what was decided
- **Why:** the reason
- **Alternatives considered:** what was passed over (optional)
- **Status:** active | superseded by <entry> | revisit <when>
```

---

### 2026-09-23 — Prediction-market integration moved up to ASAP, must be automated
- **Decision:** Kalshi / Polymarket integration jumps ahead of its May 2027 target. It has to run automatically: hourly if possible, daily at minimum. Never a manual pull.
- **Why:** Market prices move continuously; a hand-run pull is stale almost immediately and adds another daily chore.
- **Alternatives considered:** Keep the May 2027 target; manual pulls.
- **Status:** active. `GOAL_TRACKER.md` still shows May 2027 and needs updating. Design options: `docs/implementation_plans/sim_vs_market_comparison_plan.md`.

### 2026-09-23 — Private admin site that reads existing docs
- **Decision:** Build a private admin/ops site (roadmap, recent activity, runbook, decisions, model summaries, offseason ideas). It **reads** existing repo docs and never becomes a second copy of them. Start as a private claude.ai Artifact; move to a local site if it proves useful, then extend to all projects via one hub with a project switcher.
- **Why:** Avoid doc-rot from writing things twice. The repo is public, so ops notes stay off any public surface. One hub answers "what do I need to do today" across projects.
- **Alternatives considered:** A separate admin site per project; a third frontend inside this repo (public, and tied to one project).
- **Status:** active. Created `RUNBOOK.md` and this file as the two missing sources.

### 2026-09-14 — Ship the ownership model on thin data
- **Decision:** Train and ship the classic + showdown ownership models on the few settled slates available, with no coverage minimum. Retrain weekly as slates accrue.
- **Why:** "Better than what we were doing" (the hand-tuned heuristic), even while coverage is thin.
- **Alternatives considered:** Wait for about 4–6 slates per bucket.
- **Status:** active. The training script prints a coverage caveat every run.

### 2026-09-04 — Season-long and DFS-weekly rosters kept in separate trees
- **Decision:** A DFS week's injury-adjusted usage compiles into `data/current_rosters/dfs/`, never into the season-long `data/current_rosters/`. Only the current week's DFS JSON is kept; the `week_NN/*.csv` sheets are the history.
- **Why:** A one-week injury adjustment must not corrupt season-long projections.
- **Status:** active.

### 2026-09 (audit S3-9) — Keep the 6 unused coach-tendency levers
- **Decision:** Keep `no_huddle_rate`, `sec_per_play`, etc. in the DNA files even though nothing in the sim reads them.
- **Why:** They may be wired in later; deleting them loses the computed values.
- **Status:** revisit off-season.

### 2026-07-22 — Weekly data refreshes run by hand, not scheduled
- **Decision:** `refresh_weekly_dna_v_0_1_0.py`, the actual-season-stats build, and the Vegas refresh run manually.
- **Why:** Eyeball the output for the first few weeks of a brand-new pipeline before trusting a cron job with it.
- **Status:** revisit now that the pipeline has run for multiple weeks. `RUNBOOK.md` ranks the automation candidates.

### 2026-07-22 — `contested_catch_rate` left static
- **Decision:** No weekly refresh for `contested_catch_rate`, and no proxy stat.
- **Why:** No real signal exists for it anywhere in `nfl_data_py` (PBP + all 3 NGS categories checked); a faked proxy is worse than an honest static value.
- **Status:** active.
