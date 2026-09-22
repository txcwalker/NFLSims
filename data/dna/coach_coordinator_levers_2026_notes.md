# coach_coordinator_levers_2026.csv — which fields are live

<!-- Audit S3-9 (2026-09), Cam's call 2026-09-08: keep all 6 fields, may wire
them in later -- this file exists so nobody hand-tunes an inert column in
the CSV believing it affects the sim. Not injected as a CSV comment header
because build_2026_ooc_proxies.py reads the file with a plain
pd.read_csv(..., dtype=str) -- no comment= param -- so a raw comment row
would either corrupt the read or need script changes to tolerate. -->

`data/dna/coach_coordinator_levers_2026.csv` carries per-coach, per-zone
(primary/redzone/goalline) tendency fields. As of this note, confirmed by
grep of `src/nfl_sim/` + `src/api/`:

**Live — the sim actually reads these:**
- `proe_*` → `src/nfl_sim/proe_overlay_v_0_1_0.py` (`get_coach_proe`), applied
  as a post-model logit offset on pass probability.
- `deep_shot_rate_*` (via `coach_dna.json`, not this CSV directly) → `game_engine.py`'s
  `self.coach_aggression` (biases 4th-down GO probability).

**Staged but unconsumed — hand-tuning these has zero effect on any sim output:**
- `no_huddle_rate_*`
- `sec_per_play_*`
- `air_yards_tendency`, `screen_rate`, `play_action_rate`, `rpo_rate`,
  `conservative_score_bias` (these five live in `coach_dna.json` /
  `{TEAM}_traits_2026.json`, not in this CSV, but are part of the same
  "computed, stored, hand-editable, read by nothing" finding)

If you want one of these to actually influence the sim, it needs to be wired
into `game_engine.py` (or a submodel's feature set) first — editing the
number alone does nothing. See [docs/audit/2026_09_audit/phase_2_play_selection_dna.md](../../docs/audit/2026_09_audit/phase_2_play_selection_dna.md) §S3-9 for the full finding.
