# utils.py — Personnel Name Standardizer

### Why do we need it
Ingesting rosters and statistics from different data feeds (ESPN, nflreadr, sports databases) introduces naming variations (e.g. "Patrick Mahomes II" vs "P. Mahomes" vs "Patrick Mahomes"). To map stats and profiles accurately, we need a standardizer that aligns names.

### What is it doing
* **Suffix Removal**: Automatically removes generational suffixes (e.g. Jr., Sr., III, II, IV, V) from players and coaches.
* **Player Name Centralization**: Converts player names into standard `F.LastName` shorthand (e.g. "Patrick Mahomes" becomes "P.Mahomes", "Amon-Ra St. Brown" becomes "A.St. Brown").
* **Coach Name Centralization**: Standardizes coach names into clean, capitalized full names (e.g. "andy reid" becomes "Andy Reid").
* **Case and Dot Normalization**: Removes periods and normalizes whitespace and capitalization.

### Subject matter expertise utilized
* **NFL Roster Alignment**: Handles complex last names (such as "St. Brown" or "Van Ness") to preserve hyphens and capitalization.
* **Name Shorthand Mapping**: Uses first-initial abbreviation formats to match the naming standards of nflreadr and ESPN APIs.

### Decisions made & metrics/reasoning used
* **Suffix Filtration**: Checks for suffix patterns at the end of name strings using case-insensitive regex sweeps to prevent string slicing errors.

### Why this metric vs alternatives
* **F.LastName Shorthand vs Full Names**: Standardizing player names to `F.LastName` is chosen because it matches the data formats used in live play-by-play feeds, preventing target and carry mapping errors during simulation runs.

### What project goal does this accomplish
Aligns player and coach names across different data feeds, ensuring accurate database lookups and trait mappings.
