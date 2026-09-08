# 2026 Zone Usage Overrides — Notes

Companion to [`zone_usage_overrides_2026.csv`](zone_usage_overrides_2026.csv). This is the starting document for the gap [`preseason_overrides_2026_notes.md`](preseason_overrides_2026_notes.md) and `AGENTS.md` §0 both flag: *"red-zone/inside-the-5 usage-and-efficiency splits are not modeled anywhere — season-long shares stand in near the goal line too."* This file covers **target_share and carry_share only** (not efficiency fields like catch_rate/adot/ypc — those are a separate follow-on pass).

Built by [`scripts/roster_management/export_zone_usage_overrides_v_0_1_0.py`](../../scripts/roster_management/export_zone_usage_overrides_v_0_1_0.py). Re-running that script overwrites the CSV — only do so before you've started hand-editing, or intentionally to refresh the historical columns (e.g. once 2026 real data exists).

## Zone definitions

Matches `rolling_stats_v_0_1_0.py`'s `classify_zone()` exactly, since that's the function the live per-week rolling pipeline already uses and the eventual goal is for this hand-edited sheet and any future automated zone pipeline to speak the same language:

- **`five_*`** (goalline) = `yardline_100 <= 5` (inside the opponent's 5-yard line)
- **`rz_*`** (redzone) = `yardline_100` 6–20 (**mutually exclusive of goalline** — this is NOT the traditional "inside the 20" stat some sources report, which usually includes the goalline plays too. A player's true inside-the-20 share is `rz_*` and `five_*` combined, not `rz_*` alone.)

## Columns

- **`rz_target_share` / `rz_carry_share` / `five_target_share` / `five_carry_share`** — the 2026 hand-editable projection. **Prepopulated with each player's flat (whole-game) `target_share`/`carry_share` from `preseason_overrides_2026.csv`** as a placeholder, per Cam's request — nothing zone-specific has actually been projected yet, so these four columns currently just repeat the season-long number in both zones. Treat every value here as a placeholder until hand-edited against the historical columns.
- **`median_rz_target_share` / `median_rz_carry_share` / `median_five_target_share` / `median_five_carry_share`** — sit directly right of the four placeholder columns, one glance away while hand-editing. The median of the player's 2024 and 2025 real shares in that zone (i.e. of `hist_2024_*`/`hist_2025_*` below), with any season that had zero real zone plays excluded rather than counted as a 0% year. With only two real data points most rows, this is effectively their average, not a robust median in the statistical sense — but it collapses to a single number a season a player barely played (rookie year, injury year) would otherwise wrongly drag toward zero. Blank when neither season has real data.
- **`hist_2024_*` / `hist_2025_*`** — real zone usage pulled from actual 2024/2025 play-by-play (`nfl_data_py`). Share = the player's own zone targets (or carries) that season ÷ their team's total zone targets (or carries) that season. A raw count rides alongside every share (`hist_2024_rz_targets`, etc.) — **check the count before trusting a share.** Goalline volume is often single digits for a full season; a "100% share" built on 1-for-1 is noise, not a real role. `rolling_stats_v_0_1_0.py`'s own live pipeline won't trust a zone value under `MIN_ZONE_SAMPLES = 5` real plays for exactly this reason — apply the same skepticism by eye here.
- If a player was traded mid-season, their historical share is computed against whichever team they saw the most zone volume with that season (mode team), not split across both — same approach the live pipeline already uses for the flat (non-zone) case.
- Players with no `player_id` in `data/current_rosters/{TEAM}_traits_2026.json` (a handful of deep-roster/inactive filler rows) have blank historical columns — no real PBP to match against.
- Rookies and any player with zero 2024/2025 NFL snaps will also show blank historical columns for that season — expected, not a bug.

## Known gaps / not yet done

- **Efficiency fields** (catch_rate, adot, ypc, yac_per_rec, etc.) by zone are not covered here — `preseason_overrides_2026_notes.md`'s general note already flags carrying over real historical numbers for those rather than hand-projecting, but that hasn't been built yet either.
- **No apply/blend wiring.** This CSV is a standalone hand-editing document, same stage `preseason_overrides_2026.csv` started at before `apply_preseason_overrides_v_0_1_0.py` existed. Whether/how zone-specific `target_share`/`carry_share` actually feed `game_engine.py` (a new `_by_filter` split, a blend against the flat share, something else) is an open design decision — do not build that wiring without checking with Cam first, per the AGENTS.md §0 deferral note.
- **QB rows** are included in the CSV (matching `preseason_overrides_2026.csv`'s full roster) but a QB's own `target_share` is always 0 by definition — their `rz_carry_share`/`five_carry_share` (designed runs + scrambles near the goal line, e.g. QB sneaks) are the fields actually worth hand-tuning for that position. Historical QB carry shares should reflect this — sneak-heavy QBs will show elevated `five_carry_share` relative to their flat `carry_share`.
