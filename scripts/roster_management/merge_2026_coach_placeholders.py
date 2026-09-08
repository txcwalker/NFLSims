"""Adds placeholder/proxy entries to data/dna/coach_dna.json for 2026 first-
time HCs who have no real HC-level nflfastR history under their own name.

Context (2026-08-05): team_to_coach_2026.json lists 9 first-time 2026 head
coaches. Checking each against coach_dna.json showed 4 already had real
data (Schottenheimer/DAL, Coen/JAX, Moore/NO, Glenn/NYJ -- they were already
their team's credited HC in 2025). The remaining 5 have no entry, because
coach_dna.json is built from nflfastR's home_coach/away_coach columns, which
only ever credit the actual HC of record for a team-season -- an OC or DC's
individual playcalling never gets its own row, no matter how much of the
scheme was really theirs:

  - Klint Kubiak (LV)   -- real 2024-2025 Seahawks OC. Mike Macdonald's own
    entry happens to span exactly those 2 seasons (seasons_observed=2), so
    it's a clean, non-diluted proxy for Kubiak's actual offensive system.
    ALIASED, not a placeholder.
  - Jesse Minter (BAL)  -- real 2024-2025 Chargers DC. Defensive background;
    a team-offense proxy from LAC (Jim Harbaugh's system) wouldn't represent
    him at all. LEAGUE AVERAGE.
  - Jeff Hafley (MIA)   -- real 2024-2025 Packers DC. Same reasoning as
    Minter. LEAGUE AVERAGE.
  - Joe Brady (BUF)     -- real 2023-2025 Bills OC, actually called plays,
    but Sean McDermott's own entry spans 9 seasons (mostly pre-Brady) and
    would dilute his real tenure if reused directly. A correct fix needs a
    season-filtered rebuild of BUF's 2023-2025 PBP specifically -- not done
    yet. LEAGUE AVERAGE as an interim placeholder.
  - Todd Monken (CLE)   -- real 2023-2025 Ravens OC, same problem: John
    Harbaugh's entry spans 11 seasons. LEAGUE AVERAGE as an interim
    placeholder, pending the same kind of season-filtered BAL rebuild.

Every added/updated entry (except Kubiak's real alias) carries a "_note"
field flagging it as a placeholder so it's never mistaken for real personal
data. League-average values are computed fresh from whatever's currently in
coach_dna.json at merge time, not hardcoded, so they stay accurate if the
underlying coach pool changes on a future full rebuild.

Safe to rerun any time coach_dna.json gets rebuilt from scratch (same
pattern as merge_coach_proe.py, which should also be rerun after a rebuild --
run both).
"""
import json

DNA_DIR = "data/dna"
COACH_DNA_PATH = f"{DNA_DIR}/coach_dna.json"

LEAGUE_AVG_FIELDS = [
    "air_yards_tendency", "deep_shot_rate", "screen_rate", "play_action_rate",
    "no_huddle_rate", "rpo_rate", "conservative_score_bias", "proe",
]

ALIAS_SOURCE = "Mike Macdonald"
ALIAS_TARGET = "Klint Kubiak"
ALIAS_NOTE = (
    "ALIASED from Mike Macdonald's entry -- Macdonald's own seasons_observed=2 "
    "(2024-2025) maps exactly to Kubiak's real Seahawks OC tenure as the team's "
    "actual offensive play-caller under him, so this is a clean proxy, not a "
    "league-average placeholder. Added 2026-08-05."
)

PLACEHOLDER_NOTES = {
    "Jesse Minter": (
        "PLACEHOLDER: real 2024-2025 Chargers DC, not the nflfastR-credited "
        "play-caller (Jim Harbaugh). Defensive background -- a team-offense "
        "proxy from LAC wouldn't represent him anyway. Set to full "
        "league-average coach profile pending real 2026 BAL offensive data. "
        "Added 2026-08-05."
    ),
    "Jeff Hafley": (
        "PLACEHOLDER: real 2024-2025 Packers DC, not the nflfastR-credited "
        "play-caller (Matt LaFleur). Defensive background -- a team-offense "
        "proxy from GB wouldn't represent him anyway. Set to full "
        "league-average coach profile pending real 2026 MIA offensive data. "
        "Added 2026-08-05."
    ),
    "Joe Brady": (
        "PLACEHOLDER: real 2023-2025 Bills OC (interim from Nov 2023), not "
        "the nflfastR-credited play-caller (Sean McDermott, "
        "seasons_observed=9). McDermott's own entry spans years before Brady "
        "arrived and would dilute Brady's actual tenure if reused directly -- "
        "a clean tenure-filtered rebuild of BUF's 2023-2025 PBP is the real "
        "fix, not yet done. Set to full league-average coach profile in the "
        "meantime. Added 2026-08-05."
    ),
    "Todd Monken": (
        "PLACEHOLDER: real 2023-2025 Ravens OC, not the nflfastR-credited "
        "play-caller (John Harbaugh, seasons_observed=11). Harbaugh's own "
        "entry spans years before Monken arrived and would dilute Monken's "
        "actual tenure if reused directly -- a clean tenure-filtered rebuild "
        "of BAL's 2023-2025 PBP is the real fix, not yet done. Set to full "
        "league-average coach profile in the meantime. Added 2026-08-05."
    ),
}


def compute_league_average(coach_dna):
    sums = {f: 0.0 for f in LEAGUE_AVG_FIELDS}
    counts = {f: 0 for f in LEAGUE_AVG_FIELDS}
    for name, entry in coach_dna.items():
        if name == "_metadata":
            continue
        for f in LEAGUE_AVG_FIELDS:
            v = entry.get(f)
            if v is not None:
                sums[f] += v
                counts[f] += 1
    return {f: round(sums[f] / counts[f], 4) for f in LEAGUE_AVG_FIELDS if counts[f]}


def main():
    coach_dna = json.load(open(COACH_DNA_PATH, encoding="utf-8"))

    if ALIAS_SOURCE not in coach_dna:
        raise SystemExit(f"Alias source '{ALIAS_SOURCE}' not found in {COACH_DNA_PATH} -- can't build Kubiak's alias.")
    alias_entry = {k: v for k, v in coach_dna[ALIAS_SOURCE].items() if k != "_note"}
    alias_entry["_note"] = ALIAS_NOTE
    coach_dna[ALIAS_TARGET] = alias_entry

    league_avg = compute_league_average(coach_dna)
    for name, note in PLACEHOLDER_NOTES.items():
        entry = dict(league_avg)
        entry["seasons_observed"] = 0
        entry["total_pass_plays"] = 0
        entry["_note"] = note
        coach_dna[name] = entry

    metadata = coach_dna.pop("_metadata")
    ordered = {"_metadata": metadata}
    for name in sorted(coach_dna):
        ordered[name] = coach_dna[name]

    with open(COACH_DNA_PATH, "w", encoding="utf-8", newline="") as f:
        text = json.dumps(ordered, indent=2, ensure_ascii=False)
        f.write(text.replace("\n", "\r\n"))

    print(f"Aliased '{ALIAS_TARGET}' from '{ALIAS_SOURCE}'.")
    print(f"Set league-average placeholders for: {', '.join(PLACEHOLDER_NOTES)}.")


if __name__ == "__main__":
    main()
