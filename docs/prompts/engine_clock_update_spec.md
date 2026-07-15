# Specification: NFL Engine Clock & Pacing Updates

Goal: Update the NFL Game Engine's clock-management and play-runoff logic in `src/nfl_sim/game_engine.py` using empirical distributions from the 2016-2025 NFLFastR EDA and new strategic features.

Requirements:

1. Clock State Checked Before Runoff
   - Differentiate play runoffs based on the state of the clock (`self.clock_stopped`) before the play starts.
   - If the clock is currently stopped (e.g., from an incomplete pass, timeout, penalty, or turnover on the previous play):
     - The runoff on the next play represents only the active play duration.
     - Model this runoff using the empirical Stopped Clock distribution: Median of 6.0 seconds (Range: 4 to 8 seconds).
   - If the clock is running (running clock snap):
     - The runoff represents the play duration PLUS pre-snap play clock consumption.
     - Model this runoff using the empirical Running Clock distribution: Median of 38.0 seconds (Range: 28 to 42 seconds).
     - **Constraint:** Since the play clock is 40 seconds max, the maximum allowable runoff for a running clock play is capped at 46.0 seconds (6s average play duration + 40s play clock max).

2. Pre-Snap Penalties Elapse Zero Time
   - If a pre-snap penalty (e.g., False Start, Offsides) is called:
     - The game clock must freeze immediately.
     - **Constraint:** Zero (0) seconds should elapse on the game clock for that play step.

3. Surprise Hurry-Up Drive Mechanic
   - At the start of any new drive, introduce a **5% probability** that the possession team elects to use a "surprise hurry-up" (no-huddle) tempo to catch the opposing defense off guard.
   - Under this tempo, the play-to-play game-clock runoff is set to a flat 15.0 seconds (or sampled tightly around 13 to 17 seconds) for all running clock plays on that drive.
   - This state persists until the drive ends or a timeout is called.

4. Out of Bounds Logic
   - Add a rule verifying if the ball carrier went out of bounds.
   - If a play ends out of bounds, check if the time remaining in the half is <= 300 seconds (5 minutes remaining in Q2 or Q4).
     - If <= 5 minutes remain in the half: Stop the clock (`self.clock_stopped` = True).
     - Otherwise: The clock restarts when spotted (treated as a standard Running Clock play).

5. Leverage Timing Scenarios (Standard 2-Min and 4-Min Offenses)
   - Ensure the existing hurry-up (losing team in Q4 last 5 mins) and clock-bleeding (winning team in Q4 last 5 mins) tempos continue to override standard pacing:
     - Hurry-up: Paced at median 14.0 seconds runoff (inferred 8s play-clock consumption).
     - Clock-bleeding: Paced at median 39.0 seconds runoff (inferred 33s play-clock consumption).

Deliverables:
- Implement these updates vectorially across the batch array (`self.N` games simulated concurrently) inside `NFLGameEngine.simulate_play_step()`.
- Validate that the average total play count per simulated game shifts from ~123 plays to the realistic NFL average of ~145-155 total snaps (representing ~60-65 offensive snaps per team).
