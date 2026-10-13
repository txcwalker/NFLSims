// Shared by pages/Optimizer.jsx (Classic), components/ClassicLineupAnalysis.jsx and
// pages/Shortlist.jsx -- the colors, styles, player-pool builder and DK
// export helpers that used to live inside Optimizer.jsx. Moved verbatim (2026-10-08)
// when the results view was extracted so the Shortlist page could reuse it.
import { ALL_ROSTERS } from './allRosters';

// ─── Team Colors ─────────────────────────────────────────────────────────────
export const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB:  '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC:  '#E31837',
  LV:  '#888888', LAC: '#0080C6', LAR: '#003594', MIA: '#008E97',
  MIN: '#4F2683', NE:  '#002244', NO:  '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SF:  '#AA0000',
  SEA: '#002244', TB:  '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414'
};

export const POS_COLORS = {
  QB: '#ef4444', RB: '#22c55e', WR: '#3b82f6', TE: '#a855f7', DST: '#f97316'
};


// ─── Styles ───────────────────────────────────────────────────────────────────
export const cardStyle = {
  background: 'rgba(255,255,255,0.04)',
  border: '1px solid rgba(255,255,255,0.08)',
  borderRadius: '12px',
  padding: '16px',
};

export const inputStyle = {
  background: 'rgba(255,255,255,0.06)',
  border: '1px solid rgba(255,255,255,0.1)',
  color: 'white',
  borderRadius: '6px',
  padding: '5px 8px',
  width: '100%',
  fontSize: '0.85rem',
  outline: 'none',
};

export const labelStyle = {
  fontSize: '0.72rem',
  fontWeight: 600,
  color: 'var(--text-muted)',
  marginBottom: '3px',
  display: 'block',
  textTransform: 'uppercase',
  letterSpacing: '0.05em',
};

export const sectionTitleStyle = {
  fontSize: '0.68rem',
  fontWeight: 700,
  color: 'var(--accent-primary)',
  textTransform: 'uppercase',
  letterSpacing: '0.1em',
  marginBottom: '8px',
  marginTop: '2px',
  paddingBottom: '5px',
  borderBottom: '1px solid rgba(255,255,255,0.06)',
};

export const pillBtnBase = {
  border: '1px solid rgba(255,255,255,0.12)',
  borderRadius: '6px',
  padding: '3px 7px',
  fontSize: '0.72rem',
  fontWeight: 600,
  cursor: 'pointer',
  transition: 'all 0.15s',
};

// ─── Helpers ─────────────────────────────────────────────────────────────────
export function buildGameMap(games) {
  const map = {};
  if (!games || !games.length) return map;
  for (const g of games) {
    const label = `${g.away_team}@${g.home_team}`;
    map[g.away_team] = label;
    map[g.home_team] = label;
  }
  return map;
}

/** Build player pool from allSimResults (primary) → weekProjections (secondary) → ALL_ROSTERS fallback.
 * `slateTeams`, when non-empty, restricts the pool to those teams -- see the
 * note below on why this replaced the old "keep everyone" behavior. */
export function buildPlayerPool(weekProjections, allSimResults, games, slateTeams) {
  const gameMap = buildGameMap(games);
  const seen = new Set();
  const pool = [];
  const restrictToSlate = slateTeams && slateTeams.size > 0;

  // Previously every team was kept regardless of dk_main, on the theory that
  // a Thursday/Monday game just sits on its own separate slate rather than
  // being unpriced. In practice the Optimizer only ever builds lineups
  // against the single currently-selected slate (Main Slate today -- "Only
  // Main Slate is wired up today" per the DK Slate picker), so a player from
  // an off-slate game (e.g. IND@KC's Sunday Night game, or a Thu/Mon game)
  // can never actually be rostered here -- showing them with a blank "—"
  // salary just cluttered the pool (Cam flagged Jonathan Taylor/Jahmyr Gibbs
  // appearing on the Week 2 Main Slate pool, 2026-09-17). Filtered out below
  // via `slateTeams` (the teams on `games` whose `dk_main` flag is true).

  // Helper to standardize game labels as AWAY@HOME
  const getStandardGameLabel = (team, opponent) => {
    let label = gameMap[team];
    if (!label && opponent) {
      const oppClean = opponent.replace('@', '').replace('vs ', '');
      label = opponent.startsWith('@') ? `${team}@${oppClean}` : `${oppClean}@${team}`;
    }
    return label || team;
  };

  // DK's per-slate draftableId AND salary, keyed the same way as `seen`
  // below. Built up front from weekProjections -- the freshest source for
  // both (see get_week_projections()'s _overlay_live_salaries, which
  // re-resolves salary/dk_id on every read; allSimResults' projections are
  // baked once, whenever that game was last simmed, and never refreshed) --
  // so Tier 1, which wins the name/team dedup for virtually every player,
  // can still backfill either field when its own copy is missing or stale.
  // Without this, any player whose allSimResults entry predates a salary
  // resolution fix (or simply never had one) got stuck with Tier 1's null,
  // and Tier 2 below -- which has the real value -- never got a chance to
  // fill it in because `seen` already blocked it. This was the actual cause
  // of "Export for DK Upload" exporting names-only for nearly the whole pool
  // (dk_id case) and, identically, of specific players showing a blank
  // salary in the pool table despite /api/week_projections having a real
  // price for them (salary case).
  // optimal_pct specifically must ALWAYS come from /api/week_projections
  // (a true whole-slate Classic 9-slot lineup rate — precomputed exactly via
  // compute_optimal_pct_2026.py when available, else a live 50-iteration
  // solve_optimal_lineup_milp sample) and never from allSimResults' own
  // per-game optimal_pct, which is a DIFFERENT, showdown-scoped statistic
  // (how often a player is in the best 6-man captain+FLEX lineup drawn from
  // just its own 2 teams -- see run_simulation()'s solve_showdown_iteration
  // call in src/api/app.py, and Leverage.jsx's own comment making the same
  // distinction). A 2-team pool is trivially easier to dominate than the
  // full ~300-player slate, so that number runs 70-90%+ for most any decent
  // starter -- confirmed 2026-09-19 as the cause of "optimal % way too high"
  // in this pool once the showdown-side sample size got large enough to
  // converge on that (real, but wrong-context) rate instead of noise.
  // Because Tier 1 (allSimResults) wins the seen-key dedup for virtually
  // every player, this must be resolved up front and applied unconditionally
  // in both tiers below, not left as a per-tier fallback.
  const dkIdByKey = new Map();
  const dkNameByKey = new Map();
  const salaryByKey = new Map();
  const optimalPctByKey = new Map();
  (Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || [])).forEach(p => {
    if (p.name && p.team) {
      const key = `${p.name}_${p.team}`;
      dkIdByKey.set(key, p.dk_id ?? null);
      dkNameByKey.set(key, p.dk_name ?? null);
      salaryByKey.set(key, p.salary ?? null);
      optimalPctByKey.set(key, p.optimal_pct ?? null);
    }
  });

  // ── Tier 1: allSimResults (session simulations, custom user projections & stats)
  Object.values(allSimResults || {}).forEach(res => {
    if (!res?.projections) return;
    res.projections.forEach(p => {
      if (restrictToSlate && !slateTeams.has(p.team)) return;
      const key = `${p.name}_${p.team}`;
      if (seen.has(key)) return;
      seen.add(key);
      const pcts = p.dk_pcts_all || null;
      const median = pcts ? pcts[50] : (p.dk_points || 0);
      const gameLabel = getStandardGameLabel(p.team, p.opponent);
      pool.push({
        id: key,
        name: p.name,
        pos: p.pos,
        team: p.team,
        salary: p.salary ?? salaryByKey.get(key) ?? null,
        projection: parseFloat((median || 0).toFixed(1)),
        simProjection: parseFloat((median || 0).toFixed(1)),
        p25:  pcts ? parseFloat((pcts[25] ?? 0).toFixed(1)) : null,
        p40:  pcts ? parseFloat((pcts[40] ?? 0).toFixed(1)) : null,
        p50:  pcts ? parseFloat((pcts[50] ?? 0).toFixed(1)) : null,
        p75:  pcts ? parseFloat((pcts[75] ?? 0).toFixed(1)) : null,
        p95:  pcts ? parseFloat((pcts[95] ?? 0).toFixed(1)) : null,
        mean: parseFloat((p.dk_points || median || 0).toFixed(1)),
        hasPcts: !!pcts,
        locked: false,
        excluded: false,
        ownershipPct: p.ownership_proj ?? null,
        optimal_pct: optimalPctByKey.get(key) ?? null,
        game: gameLabel,
        dk_pcts_all: pcts,
        dk_id: dkIdByKey.get(key) ?? null,
        dk_name: dkNameByKey.get(key) ?? null,
      });
    });
  });

  // ── Tier 2: weekProjections from /api/week_projections (baseline projections)
  const wpList = Array.isArray(weekProjections)
    ? weekProjections
    : (weekProjections?.players || []);

  wpList.forEach(p => {
    if (!p.name || !p.pos) return;
    if (restrictToSlate && !slateTeams.has(p.team)) return;
    const pos = (p.pos || '').replace(/\d/g, '').toUpperCase();
    if (!['QB','RB','WR','TE','DST'].includes(pos)) return;
    const key = `${p.name}_${p.team}`;
    if (seen.has(key)) return;
    seen.add(key);
    const pcts = p.dk_pcts_all || null;
    const median = pcts ? pcts[50] : (p.dk_p50 ?? p.dk_score ?? 0);
    const gameLabel = getStandardGameLabel(p.team, p.opponent);
    pool.push({
      id: key,
      name: p.name,
      pos,
      team: p.team,
      salary: p.salary ?? null,
      projection: parseFloat((median || 0).toFixed(1)),
      simProjection: parseFloat((median || 0).toFixed(1)),
      // DK percentiles
      p25:  pcts ? parseFloat((pcts[25] ?? p.dk_p25 ?? 0).toFixed(1)) : (p.dk_p25 != null ? parseFloat(p.dk_p25.toFixed(1)) : null),
      p40:  pcts ? parseFloat((pcts[40] ?? 0).toFixed(1)) : null,
      p50:  pcts ? parseFloat((pcts[50] ?? p.dk_p50 ?? 0).toFixed(1)) : (p.dk_p50 != null ? parseFloat(p.dk_p50.toFixed(1)) : null),
      p75:  pcts ? parseFloat((pcts[75] ?? p.dk_p75 ?? 0).toFixed(1)) : (p.dk_p75 != null ? parseFloat(p.dk_p75.toFixed(1)) : null),
      p95:  pcts ? parseFloat((pcts[95] ?? p.dk_p95 ?? 0).toFixed(1)) : (p.dk_p95 != null ? parseFloat(p.dk_p95.toFixed(1)) : null),
      mean: parseFloat((p.dk_score ?? median ?? 0).toFixed(1)),
      hasPcts: !!(pcts || p.dk_p50 != null),
      // FD percentiles (used when platform === 'FD')
      fd_p25: p.fd_p25 != null ? parseFloat(p.fd_p25.toFixed(1)) : null,
      fd_p50: p.fd_p50 != null ? parseFloat(p.fd_p50.toFixed(1)) : null,
      fd_p75: p.fd_p75 != null ? parseFloat(p.fd_p75.toFixed(1)) : null,
      fd_p95: p.fd_p95 != null ? parseFloat(p.fd_p95.toFixed(1)) : null,
      fd_pcts_all: p.fd_pcts_all ?? null,
      locked: false,
      excluded: false,
      ownershipPct: p.ownership_proj ?? null,
      optimal_pct: optimalPctByKey.get(key) ?? null,
      game: gameLabel,
      dk_pcts_all: pcts,
      dk_id: p.dk_id ?? null,
      dk_name: dkNameByKey.get(key) ?? p.dk_name ?? null,
    });
  });

  // ── Tier 3: ALL_ROSTERS fallback (always available, flat projections)
  if (pool.length < 20) {
    Object.entries(ALL_ROSTERS).forEach(([team, data]) => {
      if (!data || !Array.isArray(data.roster)) return;
      data.roster.forEach(p => {
        const key = `${p.name}_${team}`;
        if (seen.has(key)) return;
        seen.add(key);
        const pos = (p.pos || 'WR').replace(/\d/g, '').toUpperCase();
        if (!['QB','RB','WR','TE','DST'].includes(pos)) return;
        const basePts = pos === 'QB' ? 18 : pos === 'RB' ? 10 : pos === 'WR' ? 9 : pos === 'TE' ? 7 : 6;
        pool.push({
          id: key,
          name: p.name, pos, team,
          salary: p.salary || 5000,
          projection: parseFloat(basePts.toFixed(1)),
          simProjection: parseFloat(basePts.toFixed(1)),
          p25: null, p40: null, p50: null, p75: null, p95: null,
          mean: parseFloat(basePts.toFixed(1)),
          hasPcts: false,
          fd_p25: null, fd_p50: null, fd_p75: null, fd_p95: null, fd_pcts_all: null,
          locked: false, excluded: false,
          ownershipPct: null,
          game: gameMap[team] || team,
          dk_pcts_all: null,
          dk_id: dkIdByKey.get(key) ?? null,
          dk_name: dkNameByKey.get(key) ?? null,
        });
      });
    });
  }

  return pool;
}


/** Players of one lineup in display/upload column order.
 * Input: lineup.players (each with .slot or .pos). Output: 9-element array, undefined where a slot is empty. */
export const getSlottedColumns = (players) => {
  const slots = { QB: [], RB: [], WR: [], TE: [], FLEX: [], DST: [] };
  players.forEach(p => { const s = p.slot || p.pos; if (slots[s]) slots[s].push(p); });
  return [slots.QB[0], slots.RB[0], slots.RB[1], slots.WR[0], slots.WR[1], slots.WR[2], slots.TE[0], slots.FLEX[0], slots.DST[0]];
};

const _download = (rows, header, filename) => {
  const blob = new Blob([[header, ...rows].join('\n')], { type: 'text/csv' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url; a.download = filename; a.click();
  URL.revokeObjectURL(url);
};

/** Human-readable summary CSV (last names + salary + projected score). NOT accepted by DK's upload form.
 * Input: lineups (array of optimizer lineup objects). Output: browser download. */
export const exportSummaryCSV = (lineups, filename = 'optimizer_lineups_summary.csv') => {
  const rows = lineups.map(lu => {
    const names = getSlottedColumns(lu.players).map(p => {
      if (!p) return '';
      if (p.pos === 'DST') return p.dk_name || `${p.team} DST`;
      return p.name.split(' ').slice(-1)[0];
    });
    return [...names, lu.total_salary, lu.projected_score].join(',');
  });
  _download(rows, 'QB,RB,RB,WR,WR,WR,TE,FLEX,DST,Salary,Projected Score', filename);
};

/** DK Bulk Upload CSV: header QB,RB,RB,WR,WR,WR,TE,FLEX,DST, each cell "Name (draftableId)".
 * draftableId is each player's dk_id from the live DK salary feed, so it is only valid for the
 * CURRENT main slate -- re-export if salaries refresh. A player with dk_id null exports by name only
 * (DK will reject it) and triggers the alert. DST cells use dk_name (e.g. "Buccaneers"), since the
 * sim's own DST name is the generic placeholder "Defense".
 * Input: lineups (array). Output: browser download. */
export const exportDkUploadCSV = (lineups, filename = 'dk_upload_lineups.csv') => {
  let missingIds = 0;
  const rows = lineups.map(lu => getSlottedColumns(lu.players).map(p => {
    if (!p) return '';
    const label = p.pos === 'DST' ? (p.dk_name || `${p.team} DST`) : p.name;
    if (p.dk_id == null) { missingIds++; return label; }
    return `${label} (${p.dk_id})`;
  }).join(','));
  if (missingIds > 0) {
    alert(`${missingIds} player slot(s) don't have a live DK ID (not matched to DK's current salary feed) and were exported by name only -- DK's upload will likely reject those rows. Refresh salaries or swap those players before uploading.`);
  }
  _download(rows, 'QB,RB,RB,WR,WR,WR,TE,FLEX,DST', filename);
};

/** Slim a lineup for storage in the shortlist: the /optimize response carries the full 101-point
 * dk_pcts_all array on every player (~130 KB/build of noise). Keeps dk_id so DK export still works.
 * Input: one optimizer lineup. Output: same lineup with players reduced to display + export fields. */
export const slimLineup = (lu) => ({
  ...lu,
  players: (lu.players || []).map(pl => ({
    name: pl.name, pos: pl.pos, team: pl.team, slot: pl.slot,
    salary: pl.salary, projection: pl.projection, ownership_pct: pl.ownership_pct,
    dk_id: pl.dk_id, dk_name: pl.dk_name,
  })),
});
