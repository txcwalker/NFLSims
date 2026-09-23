"""Per-week injury/return resolution for the flat per-team override sheets
(data/overrides/2026/season_long/{TEAM}.csv -> week_NN/{TEAM}.csv).

The model (Cam, 2026-09-02):
  - The season_long sheet is the base: injured players sit in it at their
    FULL curated role, fill-ins already at their reduced curated share
    (a backup RB's 26% assumes the starter's 63% is being used).
  - Each row has `roster_slot` (active / practice_squad / ir / pup / nfi /
    suspended / exempt) and a single `return_week` (99 = out for the season).
  - For week W: a reserve player is BACK if return_week <= W -> keep their
    curated shares, fill-ins auto-revert. Otherwise OUT -> their
    target_share / carry_share is redistributed pro-rata to the remaining
    `active` SAME-POSITION rows (equal split when none holds any share).
    Team share totals are preserved.
  - Week sheets only (never season_long) also carry `dfs_status` (added
    2026-09-19 for the UI gameday-inactive toggle): "out" is a one-week-only
    OUT, independent of `roster_slot`/`return_week` and reset each time
    build_week_overrides regenerates the sheet from season_long -- a player
    who is perfectly healthy season-long-wise can still be marked out for
    just this game. Same pooling/redistribution treatment as an IR player.
  - 2026-09-22: `dfs_status` gained a third value, "force_active" -- a
    gameday override that puts a RESERVE-slot player (ir/pup/...) back on
    the field before their return_week, at their full curated season_long
    share (no fill-in reverts needed: season_long already holds injured
    players at full role). GONE slots (cut/left_team/retired) are never
    overridable. Toggles are now STICKY week to week via the persistent
    ledger (scripts/roster_management/dfs_status_ledger.py); this module
    only resolves a ledger entry into a per-week dfs_status value
    (effective_dfs_status below) -- the week sheet is still where it lands.

Flat schema only (no splits / preseason_projection nesting) -- these sheets
carry one value per field. No I/O: resolve_week_rows takes parsed rows.
"""

# overall + red-zone (6-20) + goal-line (<=5) target/carry shares -- all
# redistributed the same pro-rata way when a player is out.
SHARE_FIELDS = ("target_share", "carry_share",
                "rz_target_share", "rz_carry_share",
                "gl_target_share", "gl_carry_share")
PLAYER_POSITIONS = {"QB", "RB", "WR", "TE"}
ACTIVE_SLOTS = {"active"}
RESERVE_SLOTS = {"ir", "pup", "nfi", "suspended", "exempt"}
GONE_SLOTS = {"cut", "left_team", "retired"}     # off the roster, never coming back
_NEVER = 99
DFS_OUT = "out"
DFS_FORCE_ACTIVE = "force_active"


def is_player_row(row):
    """True for a real player row. A hand-added 'Totals' / summary row (blank
    pos, or player_name 'Totals') is passed through the pipelines untouched
    but never treated as a player."""
    return ((row.get("pos") or "").strip().upper() in PLAYER_POSITIONS
            and bool((row.get("player_name") or "").strip())
            and (row.get("player_name") or "").strip().lower() not in ("total", "totals", "sum"))


def _num(v, default=0.0):
    try:
        return float(str(v).strip())
    except (TypeError, ValueError):
        return default


def parse_return_week(v):
    """'' / None / non-numeric -> 99 (out all season). Otherwise int week."""
    try:
        return int(round(float(str(v).strip())))
    except (TypeError, ValueError):
        return _NEVER


def is_available(row, week):
    """A row counts as on the field in `week` if its slot is active, or it is
    a reserve slot whose return_week has arrived. practice_squad rows are
    carried through untouched (0 share) but are NOT redistribution targets."""
    slot = (row.get("roster_slot") or "active").strip().lower()
    if slot in ACTIVE_SLOTS:
        return True
    if slot in RESERVE_SLOTS:
        return parse_return_week(row.get("return_week")) <= week
    return slot == "practice_squad"      # carried, not a recipient


def reserve_signature(row, week):
    """Inputs: row (dict, a season_long sheet row -- needs roster_slot /
    return_week), week (int).
    Output: str like "ir|8" if the row is a reserve player still OUT in
    `week` (i.e. a gameday "active" toggle would be an IR override), else
    None.
    Purpose: stamped onto a sticky "active" ledger entry at record time so
    the override only keeps applying to THAT injury stint -- if Cam later
    moves the same player to a new IR stint in season_long (different
    slot/return_week), the old gameday override must not silently cancel
    the new, real injury move."""
    slot = (row.get("roster_slot") or "active").strip().lower()
    rw = parse_return_week(row.get("return_week"))
    if slot in RESERVE_SLOTS and rw > week:
        return f"{slot}|{rw}"
    return None


def effective_dfs_status(entries, week, row):
    """Inputs: entries (list of {"week": int, "status": "out"|"active",
    optional "reserve_sig": str} -- one player's sticky ledger history, from
    dfs_status_ledger.json), week (int, the week being built), row (dict, the
    player's CURRENT season_long row).
    Output: the dfs_status string to write into week `week`'s sheet --
    "out", "force_active", "active" -- or None if no ledger entry applies
    (caller keeps whatever the week sheet already had).
    Purpose: sticky toggles. The latest entry at or before `week` wins, so a
    week-3 "out" keeps the player out in weeks 4, 5, ... until an explicit
    later "active". An "active" only becomes force_active (overriding a
    reserve slot) while the player is still on the SAME reserve stint it was
    recorded against (see reserve_signature); otherwise it just means "no
    gameday override"."""
    applicable = [e for e in (entries or []) if int(e.get("week", 0)) <= week]
    if not applicable:
        return None
    latest = max(applicable, key=lambda e: int(e["week"]))
    if latest.get("status") == DFS_OUT:
        return DFS_OUT
    sig = latest.get("reserve_sig")
    if sig and reserve_signature(row, week) == sig:
        return DFS_FORCE_ACTIVE
    return "active"


def resolve_week_rows(rows, week):
    """rows: list of dicts (one per player) from a season_long/{TEAM}.csv.
    Returns (new_rows, report). new_rows is a fresh list, same schema:
      - players out this week: target_share / carry_share set to 0, a
        `note` marking them out
      - active same-position players: share scaled up pro-rata
      - everyone else: unchanged
    report: list of {field, pos, pool, method, recipients:[(name,old,new)]}."""
    out_rows, out, recipients_by_pos = [], [], {}
    for r in rows:
        if not is_player_row(r):        # 'Totals' / summary row -> pass through, recompute below
            out_rows.append(dict(r))
            continue
        nr = dict(r)
        for f in SHARE_FIELDS:          # normalise shares to float in every output row
            nr[f] = round(_num(nr.get(f)), 6)
        slot = (r.get("roster_slot") or "active").strip().lower()
        gone = slot in GONE_SLOTS
        dfs = (r.get("dfs_status") or "active").strip().lower()
        dfs_out = dfs == DFS_OUT
        forced = dfs == DFS_FORCE_ACTIVE     # gameday override of a reserve slot (never of GONE)
        hurt = (slot in RESERVE_SLOTS and parse_return_week(r.get("return_week")) > week
                and not forced)
        if gone or hurt or dfs_out:
            rec = {"player_name": r.get("player_name"), "pos": r.get("pos"), "slot": slot}
            for f in SHARE_FIELDS:
                rec[f] = _num(r.get(f))
                nr[f] = 0.0
            out.append(rec)
            if gone:
                tag = f"GONE ({slot})"
            elif hurt:
                tag = f"OUT wk{week} ({slot})"
            else:
                tag = f"OUT wk{week} (dfs_inactive)"
            nr["note"] = (f"{tag}; "
                          f"{(r.get('note') or '').strip()}").strip("; ").strip()
        out_rows.append(nr)
        # A force-activated reserve player is on the field, so it can also
        # absorb a same-position teammate's pooled share.
        if (slot in ACTIVE_SLOTS or (forced and slot in RESERVE_SLOTS)) and not dfs_out:
            recipients_by_pos.setdefault(r.get("pos"), []).append(nr)

    report = []
    for field in SHARE_FIELDS:
        pool = {}
        for o in out:
            if o[field] > 0:
                pool.setdefault(o["pos"], 0.0)
                pool[o["pos"]] += o[field]
        for pos, pool_total in pool.items():
            recips = recipients_by_pos.get(pos, [])
            if not recips:
                report.append({"field": field, "pos": pos, "pool": round(pool_total, 4),
                               "method": "LOST -- no active same-position player",
                               "recipients": []})
                continue
            base = {id(nr): _num(nr.get(field)) for nr in recips}
            base_total = sum(base.values())
            if base_total > 0:
                weights = {id(nr): base[id(nr)] / base_total for nr in recips}
                method = "pro-rata"
            else:
                weights = {id(nr): 1.0 / len(recips) for nr in recips}
                method = "equal split (no existing share)"
            recs = []
            for nr in recips:
                old = base[id(nr)]
                new = round(old + pool_total * weights[id(nr)], 6)
                nr[field] = new
                recs.append((nr.get("player_name"), round(old, 4), round(new, 4)))
            report.append({"field": field, "pos": pos, "pool": round(pool_total, 4),
                           "method": method, "recipients": recs})

    recompute_totals_rows(out_rows)
    return out_rows, report


def recompute_totals_rows(rows):
    """Update any 'Totals'/summary row's share fields to the column sum of the
    real player rows. In place."""
    players = [r for r in rows if is_player_row(r)]
    sums = {f: round(sum(_num(r.get(f)) for r in players), 6) for f in SHARE_FIELDS}
    for r in rows:
        if not is_player_row(r) and any((r.get(f) or "").strip() != "" for f in SHARE_FIELDS):
            for f in SHARE_FIELDS:
                r[f] = sums[f]
