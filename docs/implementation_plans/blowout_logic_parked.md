# Blowout / garbage-time RB redistribution — PARKED 2026-09-08

Status: **not implemented.** Built once, measured, reverted. This note is so the
next attempt starts from what we learned, not from scratch.

## The idea

When a game is far out of hand, RB1 sits and the backups finish. Model it with a
per-play "garbage time" flag (a leverage score on score margin × time left); on
flagged plays, draw the rusher from a redistributed carry-share table (RB1
shrunk, backups renormalised up).

## What was built (and reverted)

- `_garbage_margin_threshold(game_sec_left)` — linear ramp: **40** at the Q4 gun
  (900 s left) → **29** floor at 0:00, slope `(40-29)/900` per second. Pre-Q4 it
  yields 45–62, which a real margin never reaches (self-limiting, no quarter gate).
- `rusher_cache_garbage` — per-zone twin of `rusher_cache`; RB1 (max share)
  multiplied by `GARBAGE_RB1_KEEP = 0.15`, then renormalised.
- Run-block rusher selection split each zone-mask into normal / garbage sub-masks.
- `tests/test_garbage_time.py` — 9 tests, all green (106 total).

Reverted in 4 edits to `game_engine.py` + deletion of the test file. Clean.

## Why it was parked

1. **It's inert at Cam's threshold.** Measured Gibbs across 6 DET blowout
   matchups, feature on vs off: **−0.18 carries/game (~−3 per season).** Even in
   DET's best matchups only 4–10 % of games *reach* margin ≥ 40, and that state
   exists for only part of Q4. The mechanism works; the (deliberately
   conservative) threshold makes it do almost nothing.
2. **No real evidence base.** We don't have data on when teams actually pull
   starters, and it clearly differs by team/coach. Guessing a threshold is not
   better than not having one.
3. **Implementation cost vs benefit.** ~40 lines + a second cache + a constant
   block, in an engine that already carries PROE-fade, Q4-trailing-blend, and
   dual throwaway paths. Not worth the maintenance rent for a 3-carry effect.

## What actually inflates top-RB carries (the real finding)

Not blowouts. Two things:

- **Carry share.** Every flagged RB has a fat primary `carry_share` on a thin
  room: Gibbs 0.75, J.Taylor 0.74, CMC 0.72, Judkins 0.68, Bijan 0.69. Cam's
  call (2026-09-08): these are **correct** — the backups genuinely have no NFL
  production, so the workhorse *should* carry that load. Gibbs at 0.75 "feels low
  if anything." Leave them.
- **Pass-volume shortfall (~9 %, benchmark open issue #1).** Run plays are
  over-represented league-wide, so every workhorse's raw carry count is inflated
  ~8–12 %. This is the lever that matters, and it's the run/pass-tendency work,
  not garbage time.

## If we revisit

Only worth it if (a) the pass-volume fix lands and blowouts get more common, or
(b) we get real "starter pulled" data (snap-share cliffs by score state from
nflverse PBP — win-probability > ~0.95 buckets). Until then, don't.
