# 2026 Preseason Overrides — Notes

Companion to [`preseason_overrides_2026.csv`](preseason_overrides_2026.csv) (and [`handcuff_overrides_2026.csv`](handcuff_overrides_2026.csv)) — qualitative reasoning, known unknowns, and things to revisit that don't fit in a spreadsheet cell. Covers manual edits to usage shares (target_share/carry_share) *and* to the "actual stat" efficiency fields (adot, ypc, catch_rate, etc.) — anything hand-tuned away from the rebuilt 2024-2025 baseline gets a line here, not just usage. Organized by team, alphabetical; only teams with an actual note get a section, added to as each team gets a hand-editing pass.

## General / Cross-Team

- **QB scramble_rate** manually capped/adjusted down for any QB projected to actually play this year. Reasoning: double-digit scramble rates are rare league-wide — even Lamar Jackson, the clearest outlier case, normally sits around 7-8%, not double digits. The rebuilt 2024-2025 real-data baseline is trusted as a starting point, but any QB showing an unrealistically high rate gets pulled back toward that range by hand.
- **To revisit later:** pull real historical scramble_rate distributions (league-wide, by QB archetype) to replace this hand-applied ceiling with an actual data-backed bound instead of a rule of thumb.
- **Inside-the-5 / red-zone split** — not yet modeled for any team. `target_share` and `carry_share` need a dedicated inside-the-5 (and likely inside-the-20) split, since goal-line roles can diverge meaningfully from overall-game roles — see [[JAX]] (Rodriguez vs. Tuten) for the case that surfaced this. Plan for the other fields (catch_rate, adot, etc.) that also shift near the goal line: carry over last year's (or last active year's) real numbers rather than hand-projecting them. General modeling task, not something to solve per-team.

## ARI

- **Jacoby Brissett** — `cpoe` shifted from the rebuilt baseline (+1.03, his 2021-2025 career average) down to -1.2324. Two reasons: (1) a deliberate lever against his absurd 2025 pass-attempt volume (~55 attempts/game) — that workload isn't repeating, and (2) genuine skepticism he can sustain the positive CPOE he's posted the last couple of years at a lower, more normal workload. `sack_rate` (→ 0.0738) and `avg_time_to_throw_sec` (→ 2.9293) adjusted for the same regression-to-the-mean reasoning tied to that same attempt-volume normalization.
- **Marvin Harrison Jr.** — `catch_rate` raised to 0.5774. Projecting a breakout season; set to what's judged to be league-average catch rate for his adot (depth-adjusted benchmark) rather than his rebuilt career-average number.
- **Michael Wilson** — `catch_rate` raised to 0.6049 alongside Harrison's, same league-average-for-adot approach.

## ATL

Both Tua Tagovailoa and Michael Penix are listed on ATL in the current 2026 roster data (confirmed — not a lookup bug, `MIA`'s QB room shows Cam Miller/Malik Willis/Quinn Ewers with no Tua at all). Worth a sanity-check against the real news if that doesn't match what's expected.

- Both QBs' `sack_rate` and `avg_time_to_throw_sec` nudged up a bit — neither is viewed as a great pass-protection/pocket-management QB. Tua specifically: coming out of Miami's system, which prioritized getting the ball out quickly — he should still post one of the league's fastest (if not the fastest) `avg_time_to_throw_sec` figures even after the bump, just not quite as extreme as his Miami-system number implied.

## BAL

WR corps is thin behind Zay Flowers (24.97% target share) and Rashod Bateman (13.51%) — Devontez Walker (6.16%) and the rest of the depth chart (Cornelius Johnson, Dayton Wade, LaJohntay Wester, Xavier Guillory) are marginal/unproven. The two drafted rookie WRs, Ja'Kobi Lane (R3, pick 80) and Elijah Sarratt (R4, pick 115), aren't in `preseason_overrides_2026.csv` yet — still on the curve-based rookie track (`rookie_projections_2026.json`), not promoted to the flat CSV like the round-1 skill rookies.

- Reserving ~14% combined target share for those two rookies rather than assigning it to the existing WR corps, split evenly as a placeholder for now.
- Known placeholder, not a real projection: expect the rookies to earn a bigger and less even split than the flat placeholder implies, especially later in the season once one of them separates from the other in real usage.
- Revisit once BAL's real WR2/3 picture is clearer — a real offseason addition, real 2026 practice/preseason signal, or once the season starts and real usage data exists to react to (see `refresh_weekly_dna_v_0_1_0.py`).
- The two rookies (Lane, Sarratt) are being tracked in a separate rookie spreadsheet rather than promoted into `preseason_overrides_2026.csv` — not doing the flat-track promotion `promote_rookie_to_flat_v_0_1_0.py` did for the round-1 skill rookies.

## BUF

- **DJ Moore** — `adot` manually raised 9.546 → 9.746. Reasoning: traded from CHI to a better/more accurate QB situation, and Buffalo is expected to use his vertical skill set more than Chicago did the last couple of years. Not expecting a route-tree overhaul (he won't suddenly be running go routes constantly) — just a handful more deep shots per season pulling his average target depth up slightly.

## CAR

- **Roster size** — currently carrying 5 RBs, 10 WRs, and 6 TEs on the depth chart. Needs to come down to a more reasonable number of realistic contributors before target/carry shares are finalized; too many bodies splitting shares right now.
- **Tetairoa McMillan** — `catch_rate` at 55.5%, on the lower side for a receiver with his large adot. Leaving as-is for now — flagged to revisit, not yet convinced it needs a correction.
- **Bryce Young** — projecting a step forward. `sack_rate` held to a modest 5.55% (not pushed down further), while `cpoe` raised to 1.842 — nearly 50% higher than his previous-year figure. If more optimism needs to be levered in later, prefer lowering `sack_rate` further while raising `avg_time_to_throw_sec` in tandem, rather than pushing `cpoe` any higher — wary of overstating his accuracy improvement.

## CHI

One of the more ambiguous receiving corps in the league. `target_share` for Burden, Odunze, and Loveland projected to be nearly identical.

- **Rome Odunze** — has more than double Luther Burden's `adot`. Given a slightly smaller `target_share` than Burden/Loveland to reflect that, but both players' adots will get a closer look on the second pass before this split is trusted. `catch_rate` was just above 50%, nudged up ~3 percentage points — willing to go higher but holding there for now.
- **Luther Burden III** — `catch_rate` was extremely high, over 80%. Nudged down to just under 79%; still seems outrageous for the role, flagged to revisit.
- Given Ben Johnson's history and scheme innovation, most non-RBs likely warrant a real `carry_share` (jet sweeps, gadget touches, etc.) rather than zero. Not yet built in — needs a closer look.
- **Caleb Williams** — projecting another big step forward. `sack_rate` moved down nearly a full percentage point, reflecting a real, coached improvement (his sack rate reportedly went from ~10% to ~4.5% under Ben Johnson on his prior stop). `cpoe` kept negative but only barely now. Combined with a high `avg_time_to_throw_sec` (projected highest in the league) alongside that low, below-average sack rate, this profile is meant to put him in real MVP-conversation territory this year.
- **Backfield split (D'Andre Swift / Kyle Monangai)** — exact RB1/RB2 split still unresolved, but combined `carry_share` between the two feels right at roughly 80%+ of the backfield.

## CIN

- **Joe Burrow** — elite `cpoe` is real but partly a product of throwing to two top-15 (arguably top-1) receivers, not purely his own accuracy. `sack_rate` nudged up — he has a tendency to hold the ball a little too long on some plays — and may go up again on a later pass. `carry_share`/rush rate significantly lowered (not much of a designed-run QB, basically just the occasional 4th-and-short/goal-line sneak), but given a `scramble_rate` of 4%, which is on the higher side — not confident that's above or below league average for a QB of his mobility profile, worth checking against real scramble-rate distributions (see general cross-team note above).
- **Backfield (Chase Brown / Samaje Perine / Chase Brooks)** — this should be Brown's backfield; his `target_share`/`carry_share` may need to come up further. Perine and Brooks split behind him: if Brown were to go down, Brooks projects as the primary back, not Perine — Brooks is younger and better between the tackles. Perine's current role is sized the way it is because he's historically been excellent in pass protection; he's a worse receiver than Brown but still well above average, and so is Brooks.
- **Ja'Marr Chase** and **Tee Higgins** — both are legitimate elite, top-15-ish receivers; their projected stats look like outliers/mistakes but are correct. Chase is still the clear WR1 — Higgins will have individual games where he leads in yards/targets/catches/TDs, but not the season-long edge. WR3 in the passing-game pecking order is actually Chase Brown out of the backfield, then Iosivas and the tight end rotation.
- **Tee Higgins** — `adot` of 12.34 paired with a `catch_rate` of 63.1% looked high enough to sanity-check. Pulled every player league-wide with `target_share` > 15% and compared: at his own adot/catch_rate combo, he's squarely in the pack, not an outlier. Leaving as-is; not revisiting on the second pass unless new info surfaces.

  Comparable players by `catch_rate` (closest to 63%):

  | Player | Team | target_share | adot | catch_rate |
  |---|---|---|---|---|
  | Tank Dell | HOU | 15.8% | 12.77 | 62.8% |
  | Garrett Wilson | NYJ | 25.1% | 9.34 | 62.8% |
  | **Tee Higgins** | **CIN** | **19.4%** | **12.34** | **63.1%** |
  | Justin Jefferson | MIN | 25.9% | 10.42 | 63.4% |
  | Jaylen Waddle | DEN | 18.1% | 11.54 | 63.5% |
  | DK Metcalf | PIT | 21.2% | 11.91 | 63.6% |
  | Romeo Doubs | NE | 17.7% | 13.45 | 63.7% |
  | Jameson Williams | DET | 16.8% | 12.22 | 63.8% |
  | DJ Moore | BUF | 18.4% | 9.55 | 62.1% |

  Comparable players by `adot` (closest to 12.3):

  | Player | Team | target_share | adot | catch_rate |
  |---|---|---|---|---|
  | Nico Collins | HOU | 21.7% | 12.29 | 66.4% |
  | Jerry Jeudy | CLE | 18.9% | 12.28 | 52.3% |
  | George Pickens | DAL | 20.1% | 12.33 | 61.9% |
  | **Tee Higgins** | **CIN** | **19.4%** | **12.34** | **63.1%** |
  | Rashid Shaheed | SEA | 16.5% | 12.25 | 59.6% |
  | AJ Brown | NE | 26.6% | 12.22 | 70.2% |
  | Jameson Williams | DET | 16.8% | 12.22 | 63.8% |
  | Tetairoa McMillan | CAR | 22.3% | 12.18 | 55.5% |
  | Courtland Sutton | DEN | 21.6% | 12.50 | 58.5% |

  Both cuts pulled from `preseason_overrides_2026.csv`, filtered to `target_share` > 15%. Note the spread is wide (Jeudy at 52.3% catch_rate vs. Nico Collins at 66.4%, both near a 12.3 adot) — catch_rate at a given adot isn't tightly determined by adot alone, so this is a sanity check, not a precise benchmark.
- **Tight ends** — CIN has historically used the full TE room heavily; needs a closer look nearer the start of the season once roles are clearer.

## CLE

This team is going to be bad, roster-wide. Rebuilding around a QB not on the roster yet (playing for Arch Manning next year).

- **QBs (Deshaun Watson / Dillon Gabriel / Shedeur Sanders)** — whoever starts, it's going to be ugly. Modeling as a straight 50/50 split between Watson and Sanders for now; Gabriel was bad enough last year that it's hard to believe he gets another real chance, though a small "other" allocation might get added later. Both Watson and Sanders given a 5% design `carry_share` but a big `scramble_rate` — genuinely unclear how mobile Watson still is coming off two torn Achilles and no game action since. **Watson's** `cpoe` crushed further, from just under -1 down to around -3. Not touching any other QB fields for either player for now, but may revisit.
- **Backfield (Quinshon Judkins / Raheim Sanders)** — this is the Judkins show: given him nearly 70% of the rushing work plus a not-insignificant 8% `target_share`. RB2 is a guess — going with Raheim Sanders, but there isn't much proven talent behind Judkins. Expect this offense to get its best skill players the ball by any means necessary, so Harold Fannin and Isaiah Bond are also given non-trivial `carry_share` (jet sweeps/gadget touches) — could see that number come up further.
- **Receivers** — didn't realize how bad this room is until digging in. Worst group looked at so far; would be talked about as historically bad if the Dolphins' room didn't exist. Plan (not yet executed — second pass): add 2nd-round rookie **Denzel Boston** to the roster and assume he starts Day 1, pulling a percent or two of `target_share` from most of the existing players to land him around 10%.
- **Jerry Jeudy** — has never played with a good QB and, by the numbers, effectively quit on plays last year. Viewed as genuinely talented — if he actually tries this year, he has a real shot at soaking up the leftover/empty target share on this roster, same as the rookies.
- **Harold Fannin** — the best receiver on the roster; projected to lead the team in `target_share`. Also carries a relatively modest `adot` — his routes are typically complete before the ball needs to come out, which means easier, quicker throws for whoever's at QB.
- **Isaiah Bond** — special watch flag, not a stat override: an athletic freak who's going to be running deep routes ("wind sprints") most plays and is excellent with the ball in his hands. Worth tracking his deep `TD` rate week-to-week once sims are running — when it spikes, he becomes an interesting DFS play.

## DAL

- **Dak Prescott** — leaving his stats alone for now other than `carry_share`, coming way down. Hasn't run much since the broken ankle a few years back; still mobile in and around the pocket, so given a lower-than-average `scramble_rate` rather than cutting it entirely.
- **Backfield (Javonte Williams / Jaydon Blue / Miles Sanders / Malik Davis / Deuce Mafah)** — this is Williams' room with a reasonable leash, but he hasn't been a great pass-catcher since his injury, and Blue is allegedly taking over that receiving-back role. A bit nervous about giving the RB position a combined 15% `target_share`, but think all three (Williams, Blue, and whoever else touches the field) end up catching passes. If Williams goes down, either Davis or Mafah takes over as the primary early-down runner, and Blue's role grows in both the passing and rushing games — the resulting split would land a lot closer to even than the current allocation.
- **Receivers (CeeDee Lamb / George Pickens / Ryan Flournoy / KaVontae Turpin)** — Lamb and Pickens should dominate the WR target share, with Flournoy a clear WR3 — he showed real promise last year when Lamb was out. Turpin is used as a gadget player and should touch the ball in just about every way (rush, target, return) this year.
- **George Pickens** — `adot` and `catch_rate` land close to Tee Higgins' (see [[CIN]] comparison), both a bit lower than Higgins' numbers. Viewed as roughly equally talented receivers, so this reads as a good sanity check on both players' profiles — still want to sit down and actually review the Higgins comparison table on the second pass.
- **Jake Ferguson** — knocked his `target_share` down a bit, but he's still 3rd-highest on the team. Used primarily in the red zone and on 3rd down; still has a real, relevant role this year even with the reduction.

## DEN

- **Bo Nix** — expecting continued improvement. `cpoe` moved from negative to positive, a shift of a little over 0.5 total percentage points. `sack_rate` also looked historically low, so bumped up two-tenths of a percentage point. Given a better-than-average `scramble_rate`, and expecting him to pick up a couple of designed runs (`carry_share`) per game.
- **Backfield (JK Dobbins / Jonah Coleman / RJ Harvey)** — the strange room on this roster. Rookie Jonah Coleman isn't in the overrides yet — not expecting much from him early, if anything. Dobbins carries a well-known injury history, so of any room looked at so far, this is the one most likely to look completely different by Week 4, let alone Week 8. Sticking with Dobbins as the lead back until he actually goes down. Expecting at least 3 players to get meaningful carries at some point this season — a fantasy nightmare to plan around.
- **Receivers (Jaylen Waddle / Courtland Sutton)** — Denver traded for Waddle this offseason. Waddle and Sutton are viewed as comparable, with Waddle the slightly better talent, but Sutton has real chemistry with Nix and has been in Denver his whole career, most of it under Sean Payton — he knows the system cold. Similar situation to the backfield: a huge WR room, and most of them likely to have a major role at some point this season. A nightmare to project past the top two, and not confident in the ordering of anyone behind Waddle/Sutton. Will need more massaging as the season approaches.
- **Evan Engram** — `target_share` at 12% might be a touch high. Leaving as-is for now.

## DET

One of the most condensed offenses in the league — and it somehow still feels like there isn't enough volume to go around for everyone who deserves some.

- **Jared Goff** — not really touching his values. Given a 1% `carry_share` for the occasional sneak, and a 2% `scramble_rate` — he'll run when he has to, but doesn't love doing it.
- **Backfield (Jahmyr Gibbs / Isiah Pacheco)** — Gibbs projects to what's probably the highest `carry_share` in the league at 75%, with Pacheco still kind of significant behind him around 20% (worth a note: Pacheco shows up on DET's roster in this data, not KC — same kind of surprising-but-confirmed team assignment as the Tua/ATL situation, see [[ATL]]). There's simply no one else on the roster to run the ball. Worth watching the depth chart as the season approaches, but expecting this to hold roughly as-is at kickoff.
- **Receivers (Amon-Ra St. Brown / Jameson Williams)** — a two-man game. Expecting a step forward from Williams this year, but his existing stats already look strong, so nervous about bumping his `catch_rate` or other peripherals further — might raise `deep_target_rate` a bit, but otherwise leaving him as-is for now.
- **Jahmyr Gibbs (receiving)** — want to give him more `target_share`, but not at the expense of either St. Brown, Williams, or LaPorta — doing so would effectively mean zeroing out one of the tertiary pass-catchers, which doesn't feel right when those guys still need real rest/rotation. Stuck on how to free up the volume; flagged for the second pass.
- **Sam LaPorta** — TE room combining for over 20% `target_share` feels high at first glance, but Detroit genuinely likes using its TE2/TE3/TE4 as well. Want to bump LaPorta up specifically, but same problem as Gibbs — can't take from St. Brown or Williams, and don't want to eliminate anyone else's share outright. Stuck here too for now.

## GB

- **Jordan Love** — a rather mobile QB, given basically a designed run a game (`carry_share` ~3%). Also given an above-average `scramble_rate` to match that mobility and his feel for pressure. Already carries a really low `sack_rate` and typically gets rid of the ball on time/quickly. Could see an MVP-esque season from him this year if one of his pass-catchers emerges as a true WR1.
- **Backfield (Josh Jacobs / Chris Brooks / MarShawn Lloyd)** — a little light. Jacobs is getting older and dealing with a bit of legal trouble. Brooks given some work in the projection, but expected to mostly be a special-teamer in reality. Expecting the team to work in a real third option behind Jacobs and Lloyd at some point.
- **Receivers** — one of the strangest rooms in the league; no true WR1. The most talented player in the room is viewed as Jayden Reed, but he mostly plays the slot, so he's only on the field in 3-WR sets. If no one steps up as "the guy," target share ends up massively spread out and Love suffers for it.
- **Matthew Golden** — projecting a huge step forward this year: bigger `target_share`, better `catch_rate`. His after-the-catch numbers (yac/elusiveness/broken-tackle profile) still look bad — not willing to move those yet, but they need to improve for the bigger role to make sense. `catch_rate` moved to just below 60%, `adot` bumped up just a tad.
- **Jayden Reed** — currently modeled as the team's `target_share` leader at 22.5%, but not comfortable with that at all. His snap rate projects to only around 60%. May need to flip Reed and Golden at the top of the target share order — but handing 22.5% to whichever of the two was largely invisible last year is a big ask either way. Needs another look.
- **Tucker Kraft** — the one genuinely straightforward part of this offense. Has been excellent the last two years and expected to repeat that. His after-catch profile (`yac_per_rec`, `elusiveness`, `broken_tackle_rate`) is disturbingly high, but his other peripherals back it up as real skill, not noise — moved all three down by about 25% each to rein in the outlier risk, and even after that cut he's still far ahead of the field (see comparison below).

  Comparable WR/TE by after-catch profile (`target_share` > 15%, `yac_per_rec` >= 5), sorted by `yac_per_rec`:

  | Player | Team | Pos | target_share | yac_per_rec | elusiveness | broken_tackle_rate |
  |---|---|---|---|---|---|---|
  | **Tucker Kraft** | **GB** | **TE** | **14.9%** | **9.15** | **4.05** | **40.8%** |
  | Rashee Rice | KC | WR | 26.1% | 7.95 | 1.71 | 22.9% |
  | Jameson Williams | DET | WR | 16.8% | 7.57 | 2.29 | 27.1% |
  | Khalil Shakir | BUF | WR | 20.1% | 6.85 | 0.83 | 25.2% |
  | Dalton Kincaid | BUF | TE | 16.0% | 6.66 | 1.55 | 35.5% |
  | Travis Hunter | JAX | WR | 16.0% | 6.51 | 0.08 | 26.2% |
  | Sam LaPorta | DET | TE | 15.8% | 6.47 | 1.50 | 24.5% |
  | Zay Flowers | BAL | WR | 25.0% | 6.34 | 0.81 | 26.0% |
  | Brock Bowers | LV | TE | 21.4% | 5.57 | 0.95 | 21.6% |
  | George Kittle | SF | TE | 19.0% | 5.35 | 0.97 | 18.1% |

  Kraft's `target_share` is technically just under the 15% cutoff (14.9%), so he's shown as a reference row rather than a true pool member — but the point stands regardless: even after a 25% haircut on all three after-catch fields, he's not close to the next-best player in the league on any of `yac_per_rec`, `elusiveness`, or `broken_tackle_rate`. Worth keeping an eye on whether that's a real generational after-the-catch profile or still needs to come down further.

## HOU

- **C.J. Stroud** — has looked awful the last couple of seasons, specifically the back half of last year. Giving him the benefit of the doubt given all the turnover, injuries, and a poor offensive coordinator last season. Tempted to drop `cpoe` down to just below -1, but leaving everything alone for now. Like most QBs in the league he isn't really a rushing threat, but he's mobile when needed — given an average-ish `scramble_rate`.
- **Backfield (David Montgomery / Woody Marks / Jawhar Jordan)** — this really looks like a two-RB room on paper, but felt uncomfortable modeling it that cleanly, so gave Jawhar Jordan a small role too. Montgomery ("Monty") is still the bell cow, with Marks playing a meaningful but clearly minority role behind him. Depending on how the numbers shake out, might widen that gap further — nothing more than 62-63% `carry_share` for Montgomery, though. Neither Montgomery nor Marks is a standout pass-catcher, but both are perfectly capable if targeted.
- **Receivers** — an interesting room; will need adjustments once final roster decisions are announced. Currently 6 players carry a real `target_share`. Nico Collins projects among the league leaders in `target_share`. The real question is the battle for the WR2 spot — hard to see it being anyone but **Jayden Higgins**, who showed plenty last year.
- **Tank Dell** — coming back from injury; was excellent as a rookie before fading in step with Stroud's decline. Expecting a limited role early in the offense, but wouldn't be shocked (just surprised) if he's clawed his way back to the WR2 role by the end of the season.
- **Xavier Hutchinson / Jaylin Noel** — worth keeping an eye on, but likely not fantasy-relevant behind Collins/Higgins/Dell. Not planning to adjust either of their stats for now — Collins looks like an animal, and Higgins already has some interesting-looking numbers on his own.
- **Tight ends (Dalton Schultz / Foster Moreau / Brevin Jordan)** — this should be Schultz's role again. Moreau and Jordan will both see the field but carry low `target_share` to match. Could see going lower across all three, or even cutting Moreau or Jordan out of the target mix entirely, but not making that move for now.

## IND

Relatively straightforward team.

- **Daniel Jones** — back from the Achilles tear and has been improving each year since leaving the Giants. Leaving his passing numbers as-is. Lowered `carry_share` and `scramble_rate` to reflect a less rush-intensive style post-injury — wouldn't be shocked if he can still run effectively, but not projecting that level yet.
- **Backfield (Jonathan Taylor / DJ Giddens / Ulysses Bentley)** — only 3 RBs on the roster right now, which is fine. Giddens is the clear #2, with the lion's share of the work going to Taylor. Gave Taylor a bit of an uptick in his receiving role specifically because there are currently very few NFL-ready WRs on this roster — between Taylor and Giddens, hard to imagine many other backs carrying as high a `carry_share` as Taylor does here.
- **Receivers** — a shaky room, especially for a team that's supposed to be contending this year.
- **Alec Pierce** — excellent at getting down the field, but probably too high at a `target_share` near 15% unless he starts running more than just 20+-yard routes. His `adot` of 21.17 is the single highest in the league among all players with a double-digit `target_share` (next closest is Tory Horton at 18.95) — not just high, an outlier by a wide margin. It's also curious that Pierce's `catch_rate` is significantly better than Westbrook-Ikhine's despite Pierce's far deeper average depth of target. If Pierce takes a real step forward, it probably means a genuine profile rework — shorter `adot` (something like 18), higher `catch_rate`, maybe a bit more `yac_per_rec` — though this is still hand-wavy and not yet modeled. Someone on this offense needs to take that step; maybe it ends up being Warren instead.
- **Josh Downs** — expected to have the biggest `target_share` in the offense, contingent on whether he's on the field in 2-WR sets. If the 2-WR look is Pierce and Westbrook-Ikhine instead, their respective target shares will need to be pulled closer together.
- **Tyler Warren** — this team loves the TE position, and realistically Warren ends up being the most-targeted receiver on the roster (see also the Pierce note above — Warren may be the one who ends up as the true #1 target instead of a WR). Mo Alie-Cox and Andrew Ogletree get what amount to courtesy target shares for being on the field a lot, but expect several games where neither actually gets a target.

## JAX

Expecting a huge year from this team — they showed a lot at the end of last year, including in the playoffs against the Bills.

- **Trevor Lawrence** — `sack_rate` moved up a small amount, `cpoe` increased significantly. Betting that his receiving corps makes him better, plus another year in Liam Coen's offense should make things click. Given a small `carry_share` and a league-average `scramble_rate` (or at least what's believed to be league average — see the general cross-team scramble-rate note above).
- **Backfield (Chris Rodriguez / Bhayshul Tuten / Ameer Abdullah / DeeJay Dallas)** — a brand-new room. Unclear yet whether Dallas or Abdullah ends up as the third back; leaning Dallas for now, with adjustments likely as the season approaches — both have been better-than-average pass-catchers for their careers. Split between Rodriguez and Tuten currently favors Tuten, just barely, but Rodriguez is expected to get more of the goal-line/inside-the-5 work.
- **Red zone / inside-the-5 modeling gap** — flagged here but applies league-wide, not just JAX: usage (target share and carry share specifically) needs a dedicated inside-the-5 split, since the goal-line role can diverge meaningfully from the overall-game role (e.g. Rodriguez vs. Tuten above). Other fields (catch_rate, adot, etc.) also shift in the red zone/inside-the-20, but the plan is to just carry over last year's (or last active year's) real numbers for those rather than hand-project them. Not yet built for any team — worth adding as a general modeling task, not something to solve per-team.
- **Receivers** — the most up-in-the-air WR room in the league right now.
- **Jakobi Meyers** — projected to lead the team in `target_share`, owning the middle of the field and short/short-intermediate routes.
- **Parker Washington** and **Brian Thomas Jr.** — guessing similar target shares between the two, with Thomas working further down the field.
- **Brian Thomas Jr.** — `catch_rate` bumped up 3 percentage points to adjust for a down year. Could see pushing it up further, especially with another year in the offense and a bounce-back toward his rookie-year breakout numbers.
- **Travis Hunter** — the odd man out at only 13% `target_share`, despite easily being someone who could lead the team in targets — still a respectable share regardless. Expect the offense to get him the ball in ways that let him gain yards through sheer dynamism rather than through pure target volume. If his projection needs a boost, it should come via `yac_per_rec` rather than `target_share`.
- **Brenton Strange** — also projects for a decent target share, set at about 1 percentage point below Hunter's. Nothing more notable to flag here.

Overall: very little real information to go on for this roster yet — will need to come back to this team specifically once more is known.

## KC

- **Patrick Mahomes** — coming back from injury; expecting a bounce-back to his 2024-and-earlier level. `cpoe` boosted from roughly 1.4 up to roughly 2.4 — still workshopping that exact number, but he was uncharacteristically bad last year. `sack_rate` bumped up slightly to help hold the projection in check and to account for him being somewhat less mobile, though by all accounts his knee didn't actually implode. Given a standard `scramble_rate`, which he leaned on more last year than in previous seasons.
- **Kenneth Walker** — backfield looks straightforward; this is Walker's role. There's a rookie who eventually needs to be accounted for separately, not yet built in. Open question whether they let Walker catch passes much this year — might need to come down on his `target_share` depending on how that shakes out.
- **Receivers** — a genuinely bad room outside the top two.
- **Tyquan Thornton** — somehow projects as the team's #3 target, playing the Alec Pierce-style deep-role (see [[IND]]). Was fairly successful in that role last year; tempted to move his `catch_rate` up a bit from its current 41%.
- **Xavier Worthy** — if Mahomes bounces back, it likely means Worthy takes a step forward too (or gets dragged forward with him). `adot` taken down slightly to just below 11, `catch_rate` boosted to near 60%.
- **Rashee Rice** — will be great again; no changes needed.
- **WR4/WR5** — likely ends up being a free-agent signing or a rookie (if KC even has a relevant one) making noise in camp, since the names currently behind Thornton are mostly unknowns. Placeholder shares in for now. Notes on each of the current depth names, pulled from real 2026 camp/roster reporting:
  - **Andrew Armstrong** — UDFA (originally Dolphins 2025, brief Detroit practice-squad stint, signed by KC in January 2026); big-bodied at 6'4"/202, led the SEC in catches and receiving yards at Arkansas in 2024. Drawing early camp buzz and comparisons to former Chiefs WR Demarcus Robinson, and reportedly making a real case for a 53-man roster spot.
  - **Jalen Royals** — 2025 4th-round pick out of Utah State, quiet as a rookie. Getting real second-year run in 2026 camp — Andy Reid has praised his hands specifically — and picked up extra reps with Rice sidelined this offseason. The most likely of this group to actually earn a real role.
  - **Jason Brownlee** — journeyman UDFA (originally Jets, out of Southern Miss); has seesawed on and off active rosters at past stops. Re-signed by KC on a reserve/futures deal. Camp/preseason standout type more than a proven regular-season contributor.
  - **Jimmy Holiday** — 2025 UDFA (Tennessee/Western Kentucky/Louisiana Tech); had a rocky rookie year — released, re-signed, released again. KC kept him around for another offseason of development, so there's some real internal belief, but still fully unproven.
  - **Nikko Remigio** — entering his 4th season with KC, but his real role is special teams, not receiving — primarily a kick/punt returner (741 kick-return yards, 191 punt-return yards in 2025), expected to compete for the lead returner job again in 2026. His `target_share`/`adot`/`catch_rate` in the projection sheet shouldn't be read as a real receiving role.

  Sources: [Chiefs UDFA Andrew Armstrong roster case](https://kckingdom.com/chiefs-udfa-andrew-armstrong-is-making-an-early-case-for-a-53-man-roster-spot-01kz426tmf04), [Andrew Armstrong bio — Chiefs.com](https://www.chiefs.com/team/players-roster/andrew-armstrong/), [Jalen Royals minicamp performance](https://heavy.com/sports/nfl/kansas-city-chiefs/jalen-royals-big-performance-mandatory-minicamp/), [2026 Outlook: Jalen Royals — CBS Sports](https://www.cbssports.com/fantasy/football/news/2026-outlook-jalen-royals/), [Jason Brownlee bio — Wikipedia](https://en.wikipedia.org/wiki/Jason_Brownlee), [Chiefs re-sign Jason Brownlee](https://www.arrowheadpride.com/kansas-city-chiefs-news/193200/1-27-chiefs-re-sign-wide-receiver-jason-brownlee), [Jimmy Holiday bio — Spotrac](https://www.spotrac.com/nfl/player/_/id/98376/jimmy-holiday), [Chiefs re-sign Jimmy Holiday](https://heavy.com/sports/nfl/kansas-city-chiefs/news-jimmy-holiday-reserve-future-contract-signings/), [Chiefs re-sign Nikko Remigio](https://sports.yahoo.com/articles/chiefs-sign-wr-nikko-remigio-021114701.html), [Chiefs double down on special teams: Remigio returns](https://www.yardbarker.com/nfl/articles/chiefs_double_down_on_special_teams_nikko_remigio_returns_for_2026/s1_17772_43585287).
- **Travis Kelce** — not a believer. Slowed down significantly last year and no reason to expect that reverses this year. `yac_per_rec` docked down to just over 4. Will still carry a huge `target_share` because the offense more or less requires it on this roster, but the profile overall should read as worse than last year, not better.

## LA (Rams)

- **Matthew Stafford** — coming off an MVP-caliber season (46 TD). Getting up there in age, but one of the few QBs still trained in the "old" style who can stay elite purely on mental processing of the position. Low `sack_rate` is fine as-is. `avg_air_yards_per_att` is maybe a touch high, but his `cpoe` is actually much lower than expected given that 50%+ of his throws go to Adams and Nacua — two of the best receivers in the league. No changes made yet, just flagged as worth a look.
- **Backfield (Kyren Williams / Blake Corum)** — the split should get closer to 50/50 this year, but Corum isn't great in the passing game and Kyren still holds the incumbent advantage. Should probably move a bit closer than the current split, but not drastically — capping Corum at 40% `carry_share` max for this year's projection.
- **Receivers** — a two-man show between Adams and Nacua.
- **Davante Adams** — used a ton around the goal line last year to great effect, but struggled down the field, especially late in the season. Want to dock him somewhat but not sure exactly where yet — probably `adot` — while his red-zone/inside-the-5 `target_share` (see the general cross-team red-zone note above) should actually be much higher than his overall number. Still has to be featured heavily; there's no one else on this roster to take the load. One possibility: the collective TE room `target_share` goes up instead of leaning further on Adams.
- **Puka Nacua** — arguably the best receiver in football. Stats and peripherals are high, but actually look reasonable given his skill level and the eye test. Leaving as-is for now; could even see his `adot` creep up a bit further.
- **Tight ends (Terrance Ferguson / Tyler Higbee / Colby Parkinson)** — the TE room's `target_share` could collectively rise, per the Adams note above. A lot of outside projections have Ferguson taking a big step forward and becoming the TE1, with the down-the-field targets specifically going to him from that position. Might need to adjust his numbers given the small sample size from his rookie year. Higbee and Parkinson are both professionals in every sense of the word — don't expect either of them to just go away even if Ferguson's role grows.

## LAC

- **Justin Herbert** — set up for a great year with Mike McDaniel coming in and an improved, healthier O-line. Known for a high `sack_rate`, but took it down about half a percentage point to reflect that improvement. Like most top-tier QBs he's mobile but doesn't get many designed runs, so given an about-average `scramble_rate`.
- **Backfield (Omarion Hampton / Keaton Mitchell / Kimani Vidal)** — a genuinely confusing room. Hampton is going in the first round of fantasy drafts despite two other backs who will see real time. Keaton Mitchell brought in as the passing-downs back — an excellent pass protector and receiver, a role he already had in Baltimore, expected to carry over here. Kimani Vidal saw a lot of usage last year due to injuries and graded out average-to-slightly-above; he isn't just going away, and while he won't have much receiving work, he meaningfully cuts into both Mitchell's and Hampton's shares. Hampton himself could go off, but the expectation is McDaniel — a running-game specialist — spreads work across the different back types to get the most out of each rather than feeding Hampton a bell-cow role.
- **Ladd McConkey** — expecting a bounce-back after a down sophomore year. Numbers left essentially untouched, though a slight bump to `yac_per_rec` (already at 5.1) and `catch_rate` could be worth it given Herbert should be throwing behind a more stable O-line this year.
- **Quentin Johnston** — has consistently improved and is compared to Davante Adams in the red zone — has earned a real reputation as a beast in and around the goal line. Considering bringing his `target_share` down some, especially since his real value is concentrated in the red zone/inside-the-5 (see the general cross-team red-zone note above) rather than spread across the whole game. If his overall share comes down, that volume would likely get redistributed between McConkey and the TE room.
- **Tre Harris** — a bit of a dart throw; the rest of the WR room beyond him is auxiliary at the moment.
- **Tight ends (David Njoku / Oronde Gadsden)** — 3 TEs on the roster, but only Njoku and Gadsden project for meaningful passing-game work. Njoku shows up on LAC in this dataset rather than Cleveland — a real, confirmed move worth a gut-check the same way the [[ATL]] Tua and [[DET]] Pacheco assignments were, since he's been a pass-game stud in Cleveland for years. Gadsden flashed real promise as a rookie; slightly favoring the younger Gadsden in `target_share` over Njoku, and it might be worth juicing his numbers further for a real Year 2 breakout — though he has plenty of competition around him, and this projects as a slower-paced offense overall (fewer total plays, lower pass rate) than some of the pass-funnel rooms looked at so far.

## LV

A tricky one.

- **QBs (Kirk Cousins / Fernando Mendoza)** — Cousins looks like the Week 1 starter, but Mendoza takes over at some point this season. Mendoza will carry significantly more designed rushing volume than Cousins (who should sit at ~0% `carry_share` outside of sneaks) — that split will need to be readjusted once Mendoza is actually named the starter, but that's an in-season adjustment, not something to solve now. As a rookie, Mendoza is projected with a highish `sack_rate` and a slow `avg_time_to_throw_sec` to start — his mobility should help him avoid some sacks, but the rate is still set on the higher side for a rookie learning curve.
- **Backfield (Ashton Jeanty / Chris Collier / Dylan Laube)** — basically empty behind Jeanty. There's a rookie who should be included here, and that's likely where Collier's current `target_share`/`carry_share` needs to go instead. Caught off guard by how thin this is. Generally uncomfortable giving any single back above 80% `carry_share`, and Jeanty could genuinely get there — leaving as-is pending a second look. Once Mendoza takes over at QB, his own designed-run volume should be enough to keep Jeanty under that 80% ceiling on its own.
- **Receivers (Tre Tucker / Jalen Nailor)** — one of several weak WR rooms across the league this year, and this is one of the weaker ones — Tucker and Nailor lead the way, which isn't saying much. Jeanty and Bowers should be the main beneficiaries of that weakness, but the actual outside receivers still have to catch some passes, so their shares stay in place for now. Not expecting anything great from this room, and considering lowering their stats across the board given the likelihood of below-average QB play for the season as a whole.
- **Brock Bowers** — should be demanding a huge `target_share`, on par with names like Puka Nacua, Ja'Marr Chase, or Jaxon Smith-Njigba. Considered moving his stats up further, but the expected QB play probably holds him back enough that leaving him as-is makes sense for now.
- **Michael Mayer** — don't sleep on him; genuinely a better receiving option than most of the actual wide receivers on this roster.

## MIA

A disaster of a team.

- **Malik Willis** — named the starter, but need to somehow account for the real possibility of a benching/injury/late-season rest given how many games this team is likely to lose. Expecting him to run a ton — currently modeled at a 15.5% `carry_share`, which could still be too low; realistically thinking 5-8 carries a game. He's thrown for 20+ completions only twice in his career, and this roster doesn't have the receivers to support consistent, high-volume passing anyway. `cpoe` moved way down to reflect a much worse team, scheme, and receiver corps than Green Bay. Given an elite `scramble_rate` near 10%. He does get the ball out relatively quickly, but added a full tenth of a second to `avg_time_to_throw_sec`; despite a high `sack_rate`, brought it down a bit from 9-and-change to 8-and-change percent.
- **De'Von Achane** — the one genuinely talented player on this roster and should get a ton of work. Tempted to hit his efficiency numbers given how bad this team will be overall, but left everything as-is for now. Will be spelled by Wright or Gordon, likely both, but still projects as the clear lead back in the rushing attack.
- **Receivers** — an embarrassing room that needs a real revisit; tempted to dock everyone and cap every player's `target_share` under 12.5%. Someone will have to emerge as a real #1, but no idea who yet. Currently have Tutu Atwell as the target-share leader, but the issue is he runs huge-`adot` routes as a down-the-field specialist — not the profile of a true possession/volume #1.
- **Greg Dulcich** — the team's premier pass-catcher in practice, even though Achane is the most talented receiving option overall (rushing-oriented QBs like Willis don't typically feed the ball to the RB much). So Dulcich ends up the guy by default. Also looking to dock his projection somehow given how bad this offense is shaping up to be overall — not yet resolved.

## MIN

- **Kyler Murray** — this is going to be Kyler's team; the notes here are focused almost entirely on him. He shows up on MIN in this dataset rather than Arizona — another real, confirmed move worth a gut-check the same way the [[ATL]] Tua, [[DET]] Pacheco, and [[LAC]] Njoku assignments were. If he goes down, the team is in real trouble, since JJ McCarthy isn't viewed as a viable answer behind him. Didn't touch Kyler's stats — could see boosting `avg_air_yards_per_att` given the system, the talent around him, and easily the best coaching staff he's ever played for, but holding off and revisiting closer to the start of the season.
- **Backfield (Aaron Jones / Jordan Mason)** — should end up the closest to a true 50/50 split of any backfield in the league. Slightly favoring Mason for early-down rushing work and in the red zone/inside-the-5 (see the general cross-team red-zone note above), while Jones sees more passing-down work than Mason. Considering shaving a few percentage points off Jones' efficiency given his late-season struggles and age. MIN also drafted a rookie RB late who could work his way into some passing-down usage later in the season.
- **Receivers** — one of the best 1-through-3 WR rooms in the league, with Jauan Jennings a genuinely good WR3 who handles a lot of the auxiliary/detail work well.
- **Justin Jefferson** — expected to get back to his dominant form after a down year; `catch_rate` and `yac_per_rec` both bumped up slightly.
- **Jordan Addison** — same story as Jefferson (last year was a disaster leaguewide, so everyone's getting something of a mulligan for it), and arguably more so since Addison generally works further down the field than Jefferson does. Leaving him as-is for now regardless.
- **TJ Hockenson** — hard to say what he'll actually be this year. Tempted to knock down `adot` and `yac_per_rec` given the injuries, age, and how the last two years have gone, but giving him a pass for now the same way most players are getting one for how bad last season was overall.

## NE

- **Drake Maye** — had an astronomical season last year, but the Patriots also had a historically easy schedule, so expecting some regression off that on top of general regression-to-the-mean. That said, the cat's out of the bag — he's legitimately good. `cpoe` taken down nearly a full point but still sits at 3.7813, which is elite. `sack_rate` knocked down and given an elite `scramble_rate`. Also projected for a small `carry_share` beyond just sneaks (real designed runs). With AJ Brown now in the offense, his efficiency should be excellent again, though tough to match last year's peak.
- **Backfield (Rhamondre Stevenson / TreVeyon Henderson)** — a contentious room this year, expecting a genuinely even split between two backs who do different things. Stevenson projects a slight edge in both `carry_share` and `target_share`, while Henderson is the far more explosive player. Both are elite pass-catchers and pass-protectors, but the staff trusts Stevenson a bit more in that role. Not yet executed, but planning to push Henderson's `elusiveness` up from its current slightly-negative value to a comfortably positive one (not above ~0.25) — just to reflect how fast and dynamic he is. Expecting him to be among the league leaders in 20+ yard carries this year.
- **AJ Brown** — the big addition. Should immediately dominate targets and is a real double-digit-TD threat this year.
- **Romeo Doubs** — also signed this offseason. Can never be a true WR1, but is a perfectly capable WR2 and would easily be the best WR3 in the league if used in that role instead.
- **Depth (Kyle Williams / Kayshon Boutte / Mack Hollins / DeMario Douglas)** — each fills a specific niche: Williams and Boutte get down the field, Hollins is an excellent blocker, and Douglas can fill in anywhere. All of them should get featured here and there. Shares at the bottom of this group might need to be readjusted relative to each other.
- **Hunter Henry** — the TE room is almost exclusively his, and deservedly so. Might be starting to fall off a bit, but not moving his stats at the moment — if anything would be a small knock to his `adot`.

## NO

- **Tyler Shough** — stepped up in a big way after half a season wasted on awful QB play. Passed the eye test well, and Kellen Moore's innovative offensive style seems to be getting the most out of him. His peripherals don't look great — mediocre `cpoe`, high `sack_rate` — but he does push the ball down the field, with a high `avg_air_yards_per_att` over 9 yards. Not moving anything yet; want to believe, but need to see or hear more out of camp/preseason first.
- **Backfield (Alvin Kamara / Travis Etienne)** — a bit of a mess. Kamara is still on the roster for now — while he's here, he's the premier pass-catcher in the RB room, picking up carries here and there but with Etienne expected to get the bulk of the work, north of 60% `carry_share`. There will also be a third back in the mix, but unclear which one yet; if Kamara does leave, two backs would split the remaining ~40% or less. Kamara's rushing efficiency should come down somewhat as a runner, but he remains a world-class receiver out of the backfield regardless.
- **Receivers** — this is where it gets tricky.
- **Chris Olave** — great, but has real injury issues, and even in his best, healthiest role as the clear #1 he rarely climbs over 25% `target_share`.
- **Jordyn Tyson** — the rookie, projected as the team's #2 in targets. Set to the league-wide WR average for now as a placeholder (see table below), to be adjusted once real college-profile-informed judgment and/or camp/preseason signal comes in.
- **Bub Means / Devaughn Vele** — the best guesses for the WR3/4 mix behind Olave and Tyson, but there are other interesting names in the room, plus an additional rookie who could emerge late in the season. Genuinely unclear past the top two.
- **Tight ends (Juwan Johnson / Noah Fant)** — should be Johnson over Fant, but Fant will get real targets and will have individual games where he out-targets Johnson. Fant's `catch_rate` and `yac_per_rec` probably need a slight nerf, but leaving as-is for now.

  League-wide WR averages (all WRs with `target_share` > 0 in `preseason_overrides_2026.csv`, n=192 — practice-squad/inactive placeholder rows excluded since many share an obviously generic filler value: `catch_rate` 0.62 / `adot` 11.5):

  | Stat | League avg |
  |---|---|
  | `catch_rate` | 60.8% |
  | `adot` | 11.24 |
  | `yac_per_rec` | 4.48 |
  | `ypc` | 4.69 |
  | `elusiveness` | -0.35 |
  | `broken_tackle_rate` | 15.8% |
  | `deep_target_rate` | 18.3% |
  | `avg_separation_yds` | 2.88 |

  Note this is the average across the *whole* WR pool (WR1s through WR5s), not scoped down to a "WR2-with-12%-target-share" tier specifically — if a narrower, role-matched comparison set would be more useful for Tyson than the full-league number, that's a quick follow-up query rather than a new pull.

## NYG

A weird team. Almost certainly bad again this year — the rookie core is only entering Year 2, and team chemistry/vibes aren't great heading into the season. If Jaxson Dart plays all 17 games, this team can maybe win 7, but he plays such a risky style with no guarantee he holds up that full-season durability seems unlikely.

- **Jaxson Dart** — not a believer in the talent here, but willing to leave his stats as-is for now. Given a high `carry_share` and `scramble_rate` since he likes to do both. His high `sack_rate` is a bit alarming but in line with a QB who holds onto the ball a lot; `avg_time_to_throw_sec` nudged up to help explain that and give him more time to work the ball down the field. Most of the receiving corps works down the field — will be interesting to see whether Mooney or Slayton ends up running more underneath routes, and who ultimately takes over the Wan'Dale Robinson-style role (see TE note below).
- **Backfield (Cam Skattebo / Tyrone Tracy / Devin Singletary / Patrick Ricard)** — Skattebo's backfield, but he's sharing real touches with at least two others. Both Singletary and Tracy get significant work. Ricard — reportedly one of John Harbaugh's favorite players in Baltimore, and for good reason — will see plenty of snaps, not necessarily to get the ball himself, but his presence on the field makes it harder for other skill players to also be out there, indirectly limiting their touches. If Skattebo goes down, this is close to a 50/50 split between Tracy and Singletary, with Tracy the more involved of the two.
- **Receivers** — a huge WR room; will need to follow roster news closely just to see who makes the team.
- **Malik Nabers** — injured or not, easily the best player on the roster. Even valued at only ~65% (health-adjusted confidence), he still commands the most targets on the team.
- **Darius Slayton / Darnell Mooney** — play very similar roles to each other, and similar to what Nabers does too, just less completely (Nabers is the more well-rounded receiver of the three).
- **Wan'Dale Robinson-style underneath role** — genuinely unclear who takes this over; none of the current WRs run these routes especially well. Beckham and JuJu Smith-Schuster are both viewed as washed at this point, with no guarantee either even makes the final roster.
- **Tight ends (Theo Johnson / Isaiah Likely)** — the most likely landing spot for those underneath routes is actually the TE position, split between Johnson and Likely. Likely shows up on NYG in this dataset rather than Baltimore — another real, confirmed move worth a gut-check the same way the [[ATL]] Tua, [[DET]] Pacheco, [[LAC]] Njoku, and [[MIN]] Kyler Murray assignments were. Tough to know the exact split, but expect it to be a lower-volume, closer-to-50/50 split than fantasy owners would want — if this were consolidated into one player instead of two, that player could be a top-10 TE.
- **General** — not looking to move much of anyone's efficiency metrics across the board. Nervous about Nabers coming back from injury, and the rest of the roster isn't good enough to justify giving anyone a significant improvement. Also not projecting a giant step forward for Dart, which would otherwise justify boosting the receiving corps as a group.

## NYJ

- **Geno Smith** — a huge upgrade even if he ends up worse than league-average, given how much of an unmitigated disaster Justin Fields was last year. If Geno is just league-average, this team can be really good. `cpoe` lowered to reflect working with a lot of rookies and deep-threat-type receivers; `avg_air_yards_per_att` bumped up a smidge to compensate. Expecting him to be serviceable this year, partly because of the weapons but mostly because of a genuinely solid Jets O-line — `sack_rate` pushed down a bit accordingly.
- **Backfield (Breece Hall / Braelon Allen / Isaiah Davis)** — if healthy, this stays Hall's room, with Allen and Davis both seeing significant work. Goal-line and pass-catching work should still concentrate with Hall. Allen and Davis project to roughly equal roles behind him, and it's still unclear who'd take over if Hall were to miss time — all three are solid across most phases of the game.
- **Adonai Mitchell** — looked good after being acquired in the trade; expect him to outpace Omar Cooper for most or all of the season, and certainly to start it. There's a real world where Mitchell ends up one of the most improved players not just on the Jets but in the league — not projecting a massive breakout outright, but wouldn't be surprised by one.
- **Garrett Wilson** — should continue to dominate targets, having played through genuinely poor QB play up to now. If Geno is even just league-average (let alone better), Wilson has real top-5 fantasy upside.
- **Omar Cooper** — expected to come on, especially late in the season, but starting slow. Want this reflected via a rookie workload curve rather than a flat target share — smaller `target_share` to start, with that volume spread out (not entirely) across the rest of the offense early on.
- **Kenyon Sadiq** — viewed as more interesting than Cooper. Expect Mason Taylor to open the season as the TE1, but to be supplanted relatively quickly (by rookie-TE standards) by Sadiq. Not saying Sadiq is a top-10 TE type right away, but his talent/athleticism should demand snaps, functioning almost like a WR in a lot of situations. His rookie workload curve should also start slow, but ramp up faster than Cooper's.
- **Rookie curve status** — neither Cooper nor Sadiq is currently in `rookie_projections_2026.json`; both are only in the flat `preseason_overrides_2026.csv` right now. Both also need their auxiliary stats (catch_rate/adot/yac_per_rec/etc.) adjusted — running with league averages for now (same approach as the [[NO]] Jordyn Tyson placeholder) and will revisit on a future pass. Cooper gets the WR average table from the Tyson note; Sadiq's TE equivalent:

  League-wide TE averages (all TEs with `target_share` > 0 in `preseason_overrides_2026.csv`, n=91, placeholder/inactive rows excluded):

  | Stat | League avg |
  |---|---|
  | `catch_rate` | 68.8% |
  | `adot` | 6.18 |
  | `yac_per_rec` | 4.69 |
  | `ypc` | 3.04 |
  | `elusiveness` | -0.09 |
  | `broken_tackle_rate` | 18.2% |
  | `deep_target_rate` | 6.4% |
  | `avg_separation_yds` | 3.53 |

## PHI

- **Jalen Hurts** — ran way less last year than in past seasons; expecting both his designed-run rate and `scramble_rate` to come back up. If he returns to his old rushing form, he's a top-3 fantasy QB, which matters enormously for the Eagles as a team. Not adjusting any of his stats yet, but losing AJ Brown is not ideal for any QB, so some downward adjustment may still be warranted — nothing changed for now.
- **Backfield (Saquon Barkley / Tank Bigsby / Will Shipley)** — a two-RB room in practice, with Shipley as the clear third option who might get a carry or target roughly every third game. Barkley's `target_share` may need to come up to reflect what game-flow looks like when games are close (i.e. not a blowout). Bigsby got 10 carries in the 4th quarter of blowouts on two separate occasions and looked excellent doing it — wouldn't be surprised if he's earned a bit more work for himself this year as a result.
- **DeVonta Smith** — the league is about to be reminded how good he is even without AJ Brown lined up opposite him. Expecting a big year.
- **Receivers** — PHI brought in Dontayvion Wicks, Elijah Moore, and Marquise Brown this offseason, plus drafted Makai Lemon.
- **Dontayvion Wicks** — projected as the WR2 to open the season.
- **Makai Lemon** — expect him to see real snaps quickly despite that. Plays mostly out of the slot right now, but his role could expand outside over the next couple of years as he learns the position at the NFL level. Expecting him to overtake Wicks for the WR2 role by around midseason — his rookie curve should be one of the more sudden/front-loaded of any rookie this year, and he could even reasonably start the season with a real workload rather than a token one. Already promoted to the flat `preseason_overrides_2026.csv` track (not in `rookie_projections_2026.json`) — consistent with that faster, flatter curve.
- **Eli Stowers** — PHI's 2nd-round TE pick (confirmed spelling — he's in `rookie_projections_2026.json`, not yet promoted to the flat CSV). Expect his rookie curve to be one of the more drawn-out of any rookie this year. He's more of a pure receiver and won't be expected to block much, if at all, especially early on — that's likely to make it a real challenge to get him meaningful snaps early in the season regardless of his receiving talent. Current curve file has him hitting a steady state by Week 5, which may be too aggressive given how drawn-out this ramp is expected to be — worth revisiting.
- **Dallas Goedert** — probably his last good year, or maybe next year is. His current role might be a bit too large at the moment, but this team is going to use several TEs in general and he should still carry a huge snap percentage regardless of target-share exact sizing.
- **To do** — Lemon's auxiliary stats (catch_rate/adot/yac_per_rec/etc.) still need to be filled in for real rather than left at rookie defaults. Might also be worth taking Hurts' `avg_air_yards_per_att` down a bit, and rebalancing target shares between the receivers and TEs given the Stowers/Lemon/Goedert overlap noted above.

## PIT

- **Aaron Rodgers** — his last year, and that framing shapes how this whole offense is being modeled. He's one of the fastest ball-out-of-hand QBs in the league, so he carries one of the lowest `avg_air_yards_per_att` and `avg_time_to_throw_sec` marks of any starter. Because of that, the RBs are getting bumped-up `target_share` league-wide relative to their raw talent — and it's also why, even though Metcalf will likely lead the team in yards and TDs, Pittman projects for the higher `target_share`.
- **Backfield (Rico Dowdle / Jaylen Warren)** — Dowdle likely gets the higher `carry_share` but the lower `target_share` of the two. That gap might currently be a bit too wide either way — willing to revisit on a future pass. Dowdle gets the goal-line and short-yardage work.
- **Receivers (DK Metcalf / Michael Pittman)** — roughly equal roles, with the deeper targets specifically going to Metcalf. This offense could get a bit out of hand in one direction — Pittman could end up with an almost absurd `target_share` simply because Rodgers is getting the ball out early (maybe too early), leaving Metcalf stranded down the field more than his talent would otherwise suggest.
- **Michael Pittman** — took a full yard off his `adot` and a few percentage points off `catch_rate` to reflect the quick-throw offense; could see going even further in that direction depending on just how quickly Rodgers ends up getting rid of the ball in practice.
- **Germie Bernard** (correct spelling — confirmed via `rookie_projections_2026.json`, 2nd-round pick 47) — this WR room looks thin after the top two, so expect Bernard to slot into a real WR3 role quickly, possibly immediately. Not yet added to the flat sheet; his intended ~10% `target_share` is temporarily parked on Cole Burgess as a placeholder until Bernard gets promoted properly. His curve file currently has him ramping to a 12% late-season target share by Week 5 steady-state — in the neighborhood of what's being planned here.
- **Darnell Washington** — should look similar to last year's TE room. Given a sizeable 8% `target_share`, with most of it expected to come in the red zone and on 3rd/4th-and-short-to-medium (see the general cross-team red-zone note above). Nothing too aggressive here otherwise.

## SEA

- **Sam Darnold** — not changing anything on his passing stats. Given a small `scramble_rate`.
- **Backfield (Zach Charbonnet / Jadarian Price / Emanuel Wilson / George Holani)** — a nightmare room. Kenneth Walker III has moved on to KC (confirmed real 2026 trade, not a data error), so the old Charbonnet/Walker split from last year is a historical reference point only, not this year's competition. Once Charbonnet is back from injury, expect the room to again look roughly like last year's two-man split, just with Charbonnet as the clear lead rather than half of a committee. Until then, Price gets the majority of the early-down work, with Wilson and Holani working in on 3rd down and passing situations. Price isn't great in the passing game or in pass protection, worth monitoring. While Charbonnet is out, splitting his vacated share evenly between Wilson and Holani as a placeholder, to revisit from there. Price's auxiliary stats (efficiency fields) still need adjustment on a future pass.
- **Jaxon Smith-Njigba** — this offense is all about him. Currently has the single highest `target_share` in the league at 31%, and there's a real case it should be even higher — this isn't an offense that wants to pass a lot or run many plays, so 31% only works out to about 9 targets on a 30-throw game; that could climb closer to 12-13 targets on the same 30 throws as the season goes on. His efficiency was outstanding last year, and while some regression is expected, he's talented enough that his peripherals are being left untouched.
- **Rashid Shaheed / Cooper Kupp** — the rest of the room is mostly an afterthought behind JSN. Shaheed currently gets the #2 target share, then Kupp, but these tertiary shares could need rearranging. Shaheed and Tory Horton play similar roles, so it'll be worth watching whether one of them (probably Shaheed) shifts toward more of a possession-receiver profile over time. Expect Kupp's role to shrink as the season progresses, which could open up a real fantasy-relevant WR3/4 role behind JSN and Shaheed.
- **AJ Barner** — should be most of the TE room's relevance; same as the WRs, everyone who isn't JSN is largely an afterthought in this offense.

## SF

- **Brock Purdy** — had one of the highest `scramble_rate`s in the league last year, and gets a designed run every now and again (roughly 1.5 per two games). `scramble_rate` taken down a bit from last year's level. His `sack_rate` looks unrealistically low given that scramble rate and his `avg_time_to_throw_sec` — not changing it for now, but may lower `scramble_rate` further and/or raise `sack_rate` if the combination keeps looking off.
- **Backfield (Christian McCaffrey / Isaac Guerendo)** — genuinely unclear. McCaffrey given one of the highest `carry_share`/`target_share` combinations of any RB in the league. Expect the team to lean on either Jordan James or Guerendo as the clear #2, guessing Guerendo but with real uncertainty. Not yet factored in, but worth flagging: McCaffrey had roughly 600 touches last year and doesn't have a strong track record of stringing together back-to-back fully healthy seasons. With Pearsall out (see below), there may not be much room to actively manage McCaffrey's touches down for health reasons either — as a result, he's one of the higher injury-risk players in the league this year for projection purposes.
- **Receivers** — officially a mess. Ricky Pearsall is out for the season before it even starts. There had been talk that Christian Kirk might not even make the roster given his injury history and poor performance the last several seasons — that no longer looks realistic given the state of the room. Mike Evans has never been a picture of health despite playing nearly every game most seasons (last year being the exception).
- **Deebo Samuel / De'Zhaun Stribling** — SF recently re-signed Deebo, and rookie Stribling now projects to play a bigger role, faster, than originally anticipated. For now, modeling this as: Jacob Cowing's current allocation stands in as a placeholder for Deebo's share (Deebo not yet added to the sheet), and Stribling pulls a bit of `target_share` from everyone else to land around the 10% mark. Stribling should be on the field a ton regardless of target volume — an excellent blocker, arguably the best blocking WR in this draft class including the first-round names. Confirmed via `rookie_projections_2026.json`: 2nd-round pick (33rd overall), currently ramping to a 12% target share by Week 5 steady-state — in the neighborhood of the ~10% being planned here.
- **George Kittle** — good news: should be back for real by Week 2, and is probably actually ready for Week 1, but likely sits that game out anyway since it's in Australia. Jake Tonges holds down Kittle's normal role in the meantime, projected for around 13% total `target_share` instead of the ~17% Kittle would normally command.

## TB

- **Baker Mayfield** — was off to a strong start last year before playing through injury for roughly the last 10 games. Expecting a return to that early-season level — he's basically been TB's QB1 every year he's been there and has shown why. Nothing moved yet, but tempted to boost `cpoe` and `avg_air_yards_per_att`.
- **Backfield (Bucky Irving / Sean Tucker / Kenneth Gainwell)** — a genuinely interesting room. Irving, with an assist from Tucker, effectively ran Rachaad White out of town. Tucker was scarily effective in the red zone last year — Irving should still get real chances inside the 5, just sharing that work rather than owning it outright (see the general cross-team red-zone note above). Gainwell might be the second-best pass-catching RB in the league behind CMC; he was brought in specifically for that strength, even though Irving himself is a good receiver too. Net effect: two specialists (Tucker in the red zone, Gainwell in the passing game) are each carving into what would otherwise be Irving's workload as the clear RB1 — even so, Irving should have a nice year. This room may need more auxiliary-stat rebalancing across the board than most other RB situations looked at so far.
- **Receivers** — really comes down to how well Chris Godwin bounces back.
- **Chris Godwin** — has played a full season exactly once in his career, and it's been two years since he's played more than half a team's games outside that one full year. Not projecting a new injury, but he projects comfortably behind Egbuka and the backfield as a group, and third in line for targets overall. Mayfield should still find him and make him look good on any play where he manages even a bit of separation.
- **Jalen McMillan / Tez Johnson** — both flashed real promise last year; a breakout from either could end up eating into both Egbuka's and Godwin's shares.
- **Ted Hurst** — TB's 3rd-round WR pick (round 3, pick 84 — confirmed via `rookie_projections_2026.json`, not yet on the flat sheet). Expected to work his way onto the field slowly, but should pick up real chances as the season progresses.
- **Tight ends** — a boring room, which is a nice change of pace. Maybe one of the depth names deserves something like a 1% `target_share`, but leaving the room as-is for now.

## TEN

- **Cam Ward** — projecting a step forward, nothing outrageous. `cpoe` boosted from -1.8 to -0.95, `sack_rate` and `avg_time_to_throw_sec` both lowered a bit. Still expecting a long developmental road ahead, but the trend should be positive. The team itself should still not be good this year — if the offense ends up looking too good under these settings, some of these can be dialed back, but this feels like the right starting point. He's mobile, but not given any extra designed-run volume — just an average `scramble_rate`.
- **Backfield (Tony Pollard / Tyjae Spears)** — mostly a two-man room, with a rookie the team drafted eventually working his way in. The split between Pollard and Spears (and eventually the rookie) may need to even out somewhat, but this is fine for a first pass.
- **Wan'Dale Robinson** — brought in this offseason; projected to lead the team in targets. His `adot` is very low, and given the overall lack of receiving talent on this roster, that leaves real room for others.
- **Carnell Tate** — the rookie, taken 4th overall for a reason; expect him to command real targets even in Week 1. In a sense, he ends up as the team's true fantasy WR1 despite Robinson leading in raw target share, since Tate plays much further down the field than Robinson does.
- **Elic Ayomanor / Chimere Dike** — the two younger returning receivers; both have looked good and improved as their careers have progressed.
- **Calvin Ridley** — there's a real world where, by the end of the season, he's been phased out to the WR5 role on this roster given the Robinson/Tate/Ayomanor/Dike mix ahead of him.
- **Tight ends (Gunnar Helm / Daniel Bellinger)** — hoping Helm is the featured TE all year, but Bellinger will still see the field and was a solid pass-catcher during his time with the Giants. If Helm can lock down the full 12% currently projected for the position, he might even be able to demand more than that — for now, though, his role is fairly subdued.

## WAS

- **Jayden Daniels** — returns, and a healthy season from him would be a huge boost for this team. Likes to call his own number, given an elite `scramble_rate`. May need to bring his called (non-scramble) `carry_share` down from its current 12%, but leaving it there for now. Might also bump up `avg_time_to_throw_sec` given his willingness to push the ball down the field and how much he scrambles, but not making that change yet either.
- **Backfield (Jacory Croskey-Merritt / Rachaad White / Jerome Ford / Jeremy McNichols)** — a disaster for fantasy purposes. All four could realistically be involved, though it would be a surprise if Ford isn't the one who ends up getting cut. WAS also has a 6th-round rookie, Kaytron Allen, who should make the roster barring disaster (confirmed via `rookie_projections_2026.json`, not yet on the flat sheet). JCM starts as the lead back. White is easily the best pass-catcher of the group, which carves out a real role for him even though he's not a great runner — expect him on the field enough to still pick up 4-5 carries a game. Ford plays a similar role to White, which is part of why he's the most at-risk of the group — if he sticks, he mainly takes work away from White. McNichols has been on this roster (and the Titans before that) seemingly forever, and excels specifically within the limited role he's given.
- **Receivers** — needed real help behind Terry McLaurin, and WAS just signed Stefon Diggs to address that (not yet added to the sheet — his eventual `target_share` is currently parked on Jaylin Lane as a placeholder).
- **Antonio Williams** — rookie, expected to see real usage later in the season (confirmed via `rookie_projections_2026.json`: round 3, pick 71, not yet on the flat sheet).
- **Van Jefferson** — purely a deep-decoy role — expect 1-2 targets a game 25+ yards downfield. Teams always seem to love this archetype of receiver despite a lack of actual production.
- **Depth (Dyami Brown / Treylon Burks / Luke McCaffrey)** — these tertiary names don't really separate from each other statistically, so there's a real chance any combination of them plays, or that one or two get surprisingly cut.
- **Tight ends (Chig Okonkwo / John Bates)** — should really be Chig's room, but Bates is such a good blocker that he'll be on the field for short-yardage and high-leverage situations and pick up some targets that way too — for a block-first TE, he's actually a solid receiver and runner after the catch. Could see Chig dominating the WR3-equivalent pass-catching role in this offense and climbing from his current 11.5% up toward 15% or so.
