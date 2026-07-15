# Specification: NFL Engine Overtime (OT) Logic Update

Goal: Implement robust regular-season overtime (OT) logic in `src/nfl_sim/game_engine.py` for vectorized game simulation across `self.N` concurrent games.

## Requirements

1. **Overtime Transition (Quarter 5)**
   - If scores are tied (`self.score_away == self.score_home`) at the end of the 4th quarter (when Q4 game clock hits 0), transition the game into Quarter 5 (Overtime).
   - Set the overtime clock (`self.time_remaining`) to a regular-season limit of **10 minutes (600 seconds)**.
   - Set `self.clock_stopped = True` to initiate the period.

2. **OT Possession Coin Toss**
   - For all games entering overtime, simulate a coin toss (50% probability) to determine possession.
   - Set the possession to either the away team or home team vectorially based on the toss.
   - Position the ball at the 30-yard line (70 yards to go, representing a post-kickoff touchback situation) and set `self.needs_kickoff = True` to restart with a kickoff.
   - Reset downs and distance (`self.down = 1`, `self.distance = 10`).

3. **NFL Overtime Possession and Scoring Rules**
   - Check score differences and possession states in Q5 on *every* play step.
   - The team receiving the overtime kickoff has the first possession.
   - If the first team to possess the ball scores a touchdown or a defensive safety, that team wins immediately and the game is over.
   - If the first team scores a field goal on their possession:
     - The opponent gets a possession to respond.
     - If the opponent scores a touchdown, the opponent wins immediately and the game is over.
     - If the opponent scores a field goal, the game continues under sudden-death rules (next score wins).
     - If the opponent's possession ends without scoring, the first team wins.
   - If the first team fails to score on their possession (punted, turned over, failed on downs):
     - The game enters sudden-death rules immediately (next score of any kind wins).
   - If the 10-minute overtime clock expires and the score is still tied, the game ends immediately in a tie. If one team is leading when the clock expires (e.g., first team scored FG, and time ran out during second team's possession), that team wins.

