# espn_adapter.py — Scoreboard & Play-by-Play Parser

### Why do we need it
Live ESPN play-by-play API payloads contain nested metadata and unstructured strings. To evaluate fourth downs in real time, we need a parser that cleans these payloads and translates them into a standardized, flat schema (e.g. down, distance, yardline_100, scores) that our simulation engine can ingest.

### What is it doing
* **Possession Midfield Parsing**: Extracts the team abbreviation and yardline from strings like "MID 50", "MIDFIELD", and "50", handling cases where team prefixes are missing.
* **Yardline Translation**: Converts standard field spots (e.g. "KC 40" or "BUF 40") into absolute distance from the opponent's endzone (`yardline_100`).
* **Game Seconds Derivation**: Converts display clock times (e.g. "12:30" in the 3rd quarter) into total remaining game seconds.
* **Action Inference**: Reads play-type text labels to determine the actual called action on fourth down (Punt, Field Goal, or Go).

### Subject matter expertise utilized
* **String Tokenization**: Parses unstructured display text as a fallback when standard JSON metadata fields are incomplete.
* **Geographic Field Mapping**: Uses possession context to correctly determine if a play is on the offense's or defense's side of the 50-yard line, ensuring `yardline_100` calculations are accurate.

### Decisions made & metrics/reasoning used
* **Midfield Edge Case Handling**: Explicitly assumes that midfield plays represent the 50-yard line (`yardline_100 = 50`), assigning a default possession team when abbreviations are omitted in the feed.
* **PBP Filtering**: Quickly discards all non-4th down plays in the loop to keep the parsing process efficient.

### Why this metric vs alternatives
* **Fast Regex Tokenizer vs Full String Matching**: Selected lightweight, regex-based tokenization to parse possession strings rather than large natural language processing (NLP) models, ensuring real-time performance on high-speed game-day loops.

### What project goal does this accomplish
Acts as the translator for live ESPN feeds, feeding clean, structured play data directly into the simulation and posting pipelines.
