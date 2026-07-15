# TODO: DraftKings / FanDuel Contest API & Slate Scraping

## Context
The DFS Optimizer currently uses manually entered contest settings (contest size,
entry fee, paying positions, etc.). The goal is to eventually pull live contest
data directly from DK/FD so the optimizer can auto-populate these fields and
use real payout structures for EV calculations.

## DraftKings

### Slate Data
- DK exposes a public contests page at:
  `https://www.draftkings.com/lineup/getavailableplayers?draftGroupId=<ID>`
- The draft group ID changes each week and by slate type (Main, Early, Late, TNF, Showdown).
- Need to identify the current week's draft group IDs, likely by scraping:
  `https://www.draftkings.com/lobby#/NFL`

### Contest Data & Payout Structure
- Individual contest pages include a payout table, e.g.:
  `https://www.draftkings.com/contest/detailspop?contestId=<ID>`
- Key fields to capture:
  - `1st_place_pct` — percentage of prize pool going to 1st place
  - `top10_pct` — percentage going to top 10
  - `min_cash_pct` — minimum cash threshold rank
  - `prize_pool` — total prize pool
  - `entry_fee` — entry fee per lineup
  - `max_entries` — max entries per user
  - `total_entries` / `max_entries_total`
- Some of this data is embedded in the page as JSON (`__NEXT_DATA__` or similar).

### Salary Data
- Player salaries are included in the `getavailableplayers` response.
- Fields: `DisplayName`, `TeamAbbreviation`, `Salary`, `PlayerGameHash`, `Status`
- This is the highest-priority field to capture — it removes the need for manual
  salary entry and unlocks real value/pts-per-dollar calculations.

## FanDuel

### Slate Data
- FD uses a different internal API; the public lobby is:
  `https://api.fanduel.com/fixture-lists`
- Player salaries are at:
  `https://api.fanduel.com/fixture-lists/<fixture_list_id>/players`

### Contest Payout
- FD contest details at:
  `https://api.fanduel.com/contests/<contest_id>`

## Implementation Plan (When Ready)

1. **Auth** — Both DK and FD require authentication for most APIs. Investigate
   whether any endpoints remain public (they historically have).
2. **Scraper** — Build a `src/scrapers/dk_scraper.py` that:
   - Fetches the current week's draft groups
   - Fetches player salaries per slate
   - Fetches payout structure for a given contest ID or URL
3. **Backend endpoint** — Expose `/api/dk/players?week=<n>&slate=<main|early|...>`
   and `/api/dk/contest?url=<contest_url>` from the FastAPI backend.
4. **Frontend wiring** — In Optimizer Settings, add a "Paste Contest URL" field
   that calls the backend endpoint and auto-populates:
   - Entry fee, contest size, paying positions, total entries
   - Full payout structure tiers for EV calculation
   - Player salary overrides

## Status
- [ ] Confirm which endpoints remain public without auth
- [ ] Confirm weekly draft group discovery method
- [ ] Build scraper
- [ ] Build backend endpoint
- [ ] Wire frontend
