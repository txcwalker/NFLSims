import { useState, useMemo, useEffect, useCallback, useRef, Fragment } from 'react';
import { ApiService } from '../api';
import { ALL_ROSTERS } from '../allRosters';
import { fmtSpreadNum } from '../bettingLines';
import SlotSwitcher from '../components/SlotSwitcher';
import LineupHistogramModal from '../components/LineupHistogramModal';
import { useWorkspaceSlots } from '../hooks/useWorkspaceSlots';

// ─── Team Colors ─────────────────────────────────────────────────────────────
const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB:  '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC:  '#E31837',
  LV:  '#888888', LAC: '#0080C6', LAR: '#003594', MIA: '#008E97',
  MIN: '#4F2683', NE:  '#002244', NO:  '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SF:  '#AA0000',
  SEA: '#002244', TB:  '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414'
};

const POS_COLORS = {
  QB: '#ef4444', RB: '#22c55e', WR: '#3b82f6', TE: '#a855f7', DST: '#f97316'
};

// ─── GPP Projection Weights by Contest Type ───────────────────────────────────
// These weights tilt the ILP objective toward ceiling for GPP formats.
// Cash uses pure median; GPP types blend in P75/P95 proportionally to prize skew.
// To adjust: change weights here. They must sum to 1.0.
// See docs/todo/optimizer_ownership_leverage.md for the full design rationale.
const GPP_WEIGHTS_BY_TYPE = {
  cash:               { p25: 0,    p50: 1.00, p75: 0,    p95: 0    }, // pure median
  flat:               { p25: 0.10, p50: 0.30, p75: 0.40, p95: 0.20 }, // ~μ + 0.5σ
  top_heavy:          { p25: 0.05, p50: 0.20, p75: 0.35, p95: 0.40 }, // ~μ + 0.9σ
  extreme_top_heavy:  { p25: 0,    p50: 0.10, p75: 0.25, p95: 0.65 }, // ~μ + 1.4σ
};

function computeGppProj(p, weights) {
  // Falls back to median projection if percentile data is missing.
  if (!p.hasPcts) return parseFloat((p.projection || 0).toFixed(1));
  const p25 = p.p25 ?? p.projection ?? 0;
  const p50 = p.p50 ?? p.projection ?? 0;
  const p75 = p.p75 ?? p.projection ?? 0;
  const p95 = p.p95 ?? p.projection ?? 0;
  return parseFloat((
    (weights.p25 || 0) * p25 +
    (weights.p50 || 0) * p50 +
    (weights.p75 || 0) * p75 +
    (weights.p95 || 0) * p95
  ).toFixed(1));
}

const OPTIMIZER_SEASON = 2026;

// A fresh, empty optimizer overlay (the user's manual layer on top of the sim
// pool). Factory so each reset gets its own object.
const emptyOverlay = () => ({
  players: {}, excludedTeams: [], excludedGames: [], gameExclusions: {},
  ownershipFrozen: false, ownershipFrozenAt: null,
});

// Drop player-overlay entries that carry nothing but defaults, so the persisted
// state (and the diff that gates autosave) stays lean.
// Short "3m ago" / "Fri 2:14p" style stamp for a build row.
const relStamp = (iso) => {
  if (!iso) return '';
  const d = new Date(iso);
  const mins = Math.round((Date.now() - d.getTime()) / 60000);
  if (mins < 1) return 'just now';
  if (mins < 60) return `${mins}m ago`;
  if (mins < 24 * 60) return `${Math.round(mins / 60)}h ago`;
  return d.toLocaleDateString([], { weekday: 'short' }) + ' ' +
    d.toLocaleTimeString([], { hour: 'numeric', minute: '2-digit' });
};

const pruneOverlay = (ov) => {
  const players = {};
  for (const [id, p] of Object.entries(ov.players || {})) {
    const meaningful = (p.projAdjust != null && p.projAdjust !== 0)
      || p.projAbsolute != null || p.ownershipPct != null
      || p.ownershipFrozenValue != null || p.locked || p.excluded;
    if (meaningful) players[id] = p;
  }
  return { ...ov, players };
};

// ─── Styles ───────────────────────────────────────────────────────────────────
const cardStyle = {
  background: 'rgba(255,255,255,0.04)',
  border: '1px solid rgba(255,255,255,0.08)',
  borderRadius: '12px',
  padding: '16px',
};

const inputStyle = {
  background: 'rgba(255,255,255,0.06)',
  border: '1px solid rgba(255,255,255,0.1)',
  color: 'white',
  borderRadius: '6px',
  padding: '5px 8px',
  width: '100%',
  fontSize: '0.85rem',
  outline: 'none',
};

const labelStyle = {
  fontSize: '0.72rem',
  fontWeight: 600,
  color: 'var(--text-muted)',
  marginBottom: '3px',
  display: 'block',
  textTransform: 'uppercase',
  letterSpacing: '0.05em',
};

const sectionTitleStyle = {
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

const pillBtnBase = {
  border: '1px solid rgba(255,255,255,0.12)',
  borderRadius: '6px',
  padding: '3px 7px',
  fontSize: '0.72rem',
  fontWeight: 600,
  cursor: 'pointer',
  transition: 'all 0.15s',
};

// ─── Helpers ─────────────────────────────────────────────────────────────────
function buildGameMap(games) {
  const map = {};
  if (!games || !games.length) return map;
  for (const g of games) {
    const label = `${g.away_team}@${g.home_team}`;
    map[g.away_team] = label;
    map[g.home_team] = label;
  }
  return map;
}

/** Build player pool from allSimResults (primary) → weekProjections (secondary) → ALL_ROSTERS fallback */
function buildPlayerPool(weekProjections, allSimResults, games) {
  const gameMap = buildGameMap(games);
  const seen = new Set();
  const pool = [];

  // Teams on the live DK slate (dk_main, from /api/games -- itself sourced
  // from dk["main_slate_teams"]). A team missing from this set has no real
  // DK price yet (game not posted to the slate, not a bye), so its players
  // must not appear at all -- not with a real projection and a guessed
  // salary. Empty when games haven't loaded yet, in which case nothing is
  // filtered (better to show everyone than hide everyone on a slow load).
  const mainSlateTeams = new Set();
  (games || []).forEach(g => {
    if (g.dk_main) {
      mainSlateTeams.add(g.away_team);
      mainSlateTeams.add(g.home_team);
    }
  });
  const onSlate = team => mainSlateTeams.size === 0 || mainSlateTeams.has(team);

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
  const dkIdByKey = new Map();
  const salaryByKey = new Map();
  (Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || [])).forEach(p => {
    if (p.name && p.team) {
      const key = `${p.name}_${p.team}`;
      dkIdByKey.set(key, p.dk_id ?? null);
      salaryByKey.set(key, p.salary ?? null);
    }
  });

  // ── Tier 1: allSimResults (session simulations, custom user projections & stats)
  Object.values(allSimResults || {}).forEach(res => {
    if (!res?.projections) return;
    res.projections.forEach(p => {
      if (!onSlate(p.team)) return;
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
        optimal_pct: p.optimal_pct ?? null,
        game: gameLabel,
        dk_pcts_all: pcts,
        dk_id: dkIdByKey.get(key) ?? null,
      });
    });
  });

  // ── Tier 2: weekProjections from /api/week_projections (baseline projections)
  const wpList = Array.isArray(weekProjections)
    ? weekProjections
    : (weekProjections?.players || []);

  wpList.forEach(p => {
    if (!p.name || !p.pos) return;
    if (!onSlate(p.team)) return;
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
      optimal_pct: p.optimal_pct ?? null,
      game: gameLabel,
      dk_pcts_all: pcts,
      dk_id: p.dk_id ?? null,
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
        });
      });
    });
  }

  return pool;
}

// ─── Game Bar ─────────────────────────────────────────────────────────────────
function GameBar({ games, allSimResults, gameExclusions, onToggleGame }) {
  return (
    <div style={{
      display: 'flex', gap: '8px', overflowX: 'auto', paddingBottom: '8px',
      marginBottom: '4px',
    }}>
      {(games || []).map(g => {
        const gameLabel = `${g.away_team}@${g.home_team}`;
        const state = gameExclusions[gameLabel] || 0; // 0=none, 1=away excluded, 2=home excluded, 3=both excluded
        
        const simData = Object.values(allSimResults || {}).find(r =>
          (r.away_team === g.away_team && r.home_team === g.home_team) ||
          (r.projections && r.projections.some(p => p.team === g.away_team || p.team === g.home_team))
        );
        const vegasTotal = g.total_line;
        // Home-side spread (see bettingLines.js): nflverse spread_line is the
        // home margin, so the home-side line is its negative.
        const vegasSpread = g.spread_line != null ? -g.spread_line : null;

        const simTotal = simData?.summary
          ? (simData.summary.away_avg_score + simData.summary.home_avg_score).toFixed(1)
          : null;

        const simSpread = simData?.summary
          ? (simData.summary.away_avg_score - simData.summary.home_avg_score)
          : null;

        const awayColor = TEAM_COLORS[g.away_team] || '#888';
        const homeColor = TEAM_COLORS[g.home_team] || '#888';

        // Styling based on 4-state cycle
        const isAwayExcluded = state === 1 || state === 3;
        const isHomeExcluded = state === 2 || state === 3;

        let statusText = '';
        if (state === 1) statusText = 'AWAY EXCLUDED';
        else if (state === 2) statusText = 'HOME EXCLUDED';
        else if (state === 3) statusText = 'GAME EXCLUDED';

        return (
          <button
            key={g.game_id}
            onClick={() => onToggleGame(gameLabel)}
            title="Click to cycle: Exclude Away -> Exclude Home -> Exclude Game -> Reset"
            style={{
              flexShrink: 0,
              background: state === 3 
                ? 'rgba(239, 68, 68, 0.08)' 
                : state > 0 
                  ? 'rgba(245, 158, 11, 0.06)' 
                  : 'rgba(255, 255, 255, 0.04)',
              border: `1px solid ${
                state === 3 
                  ? 'rgba(239, 68, 68, 0.4)' 
                  : state > 0 
                    ? 'rgba(245, 158, 11, 0.3)' 
                    : 'rgba(255, 255, 255, 0.1)'
              }`,
              borderRadius: '12px',
              padding: '10px 14px',
              cursor: 'pointer',
              textAlign: 'left',
              transition: 'all 0.2s',
              minWidth: '180px',
              boxSizing: 'border-box',
            }}
          >
            {/* Teams row */}
            <div style={{ display: 'flex', alignItems: 'center', gap: '5px', marginBottom: '8px' }}>
              <span style={{
                display: 'inline-block', width: '8px', height: '8px',
                borderRadius: '50%', background: awayColor, flexShrink: 0,
              }} />
              <span style={{ 
                fontWeight: 700, 
                fontSize: '0.8rem', 
                color: isAwayExcluded ? '#ef4444' : 'var(--text-white)',
                textDecoration: isAwayExcluded ? 'line-through' : 'none',
                opacity: isAwayExcluded ? 0.4 : 1
              }}>{g.away_team}</span>
              
              <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>@</span>
              
              <span style={{
                display: 'inline-block', width: '8px', height: '8px',
                borderRadius: '50%', background: homeColor, flexShrink: 0,
              }} />
              <span style={{ 
                fontWeight: 700, 
                fontSize: '0.8rem', 
                color: isHomeExcluded ? '#ef4444' : 'var(--text-white)',
                textDecoration: isHomeExcluded ? 'line-through' : 'none',
                opacity: isHomeExcluded ? 0.4 : 1
              }}>{g.home_team}</span>
            </div>
            {/* Vegas Stats row */}
            <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginBottom: '2px' }}>
              Veg: <span style={{ color: 'var(--text-white)', fontWeight: 600 }}>{g.home_team} {fmtSpreadNum(vegasSpread)}</span> · <span style={{ color: 'var(--text-white)', fontWeight: 600 }}>{vegasTotal} O/U</span>
            </div>
            {/* Sim Stats row */}
            <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>
              Sim: {simTotal ? (
                <>
                  <span style={{ color: 'var(--accent-primary)', fontWeight: 600 }}>{g.home_team} {fmtSpreadNum(simSpread)}</span> · <span style={{ color: 'var(--accent-primary)', fontWeight: 600 }}>{simTotal} O/U</span>
                </>
              ) : (
                <span style={{ color: 'rgba(255, 255, 255, 0.2)' }}>—</span>
              )}
            </div>
            {statusText && (
              <div style={{ 
                fontSize: '0.62rem', 
                color: state === 3 ? '#ef4444' : '#f59e0b', 
                marginTop: '6px', 
                fontWeight: 700,
                letterSpacing: '0.05em'
              }}>{statusText}</div>
            )}
          </button>
        );
      })}
    </div>
  );
}

// ─── Settings Panel ───────────────────────────────────────────────────────────
// ─── Live DK contest search ────────────────────────────────────────────────
// Full contest list for the selected slate is already in memory (one lobby
// fetch covers every contest on it -- see dk_scraper.py), so filtering by
// name is a free client-side operation, not a new network call per keystroke.
function ContestPicker({ dkContests, setSettings }) {
  const [filter, setFilter] = useState('');
  const [selectedId, setSelectedId] = useState('');
  const [payoutStatus, setPayoutStatus] = useState(''); // '', 'loading', 'ok', 'error'

  const matches = filter.trim()
    ? dkContests.filter(c => c.name && c.name.toLowerCase().includes(filter.trim().toLowerCase()))
    : dkContests.slice(0, 100); // unfiltered default view: biggest contests first (dkContests is pre-sorted by entries)

  const applyContest = async (contestId) => {
    const c = dkContests.find(c => String(c.contest_id) === contestId);
    if (!c) return;
    setSelectedId(contestId);
    setSettings(s => ({
      ...s,
      contestSize: c.max_entries ?? s.contestSize,
      entryFee: c.entry_fee ?? s.entryFee,
      // Cleared until the real payout table below either lands or fails --
      // stops a stale tier table from a previous contest silently steering
      // this one's EV math.
      payoutStructure: null,
      // Identity of the contest this build targets -- carried into the saved
      // state and (Phase 3) frozen into each build for end-of-season review.
      contest: {
        dk_contest_id: String(c.contest_id),
        name: c.name ?? null,
        entry_fee: c.entry_fee ?? null,
        field_size: c.max_entries ?? null,
        prize_pool: c.prize_pool ?? null,
      },
    }));

    // Real rank-by-rank $ payout table, one call per contest (see
    // dk_scraper.get_dk_contest_payout docstring -- not cached, since a
    // contest's entry count changes constantly). Falls back to the
    // percentage-shaped default (src/api/app.py _get_default_payout_structure)
    // if this contest has no cash tiers yet or the request fails.
    setPayoutStatus('loading');
    const payout = await ApiService.getDkContestPayout(contestId);
    if (payout.tiers && payout.tiers.length > 0) {
      const payingPositions = Math.max(...payout.tiers.map(t => t.rank_end));
      setSettings(s => ({
        ...s, payingPositions, payoutStructure: payout.tiers,
        contest: { ...(s.contest || {}), paying_positions: payingPositions },
      }));
      setPayoutStatus('ok');
    } else {
      setPayoutStatus('error');
    }
  };

  return (
    <div style={{ marginBottom: '10px' }}>
      <label style={labelStyle}>Live DK Contest</label>
      <input
        type="text"
        placeholder="Search contests by name…"
        value={filter}
        onChange={e => setFilter(e.target.value)}
        style={{ ...inputStyle, marginBottom: '4px' }}
      />
      <select
        style={inputStyle}
        value={selectedId}
        onChange={e => applyContest(e.target.value)}
      >
        <option value="" disabled>
          {filter.trim() ? `${matches.length} match${matches.length === 1 ? '' : 'es'}...` : 'Select a live contest to auto-fill...'}
        </option>
        {matches.map(c => (
          <option key={c.contest_id} value={c.contest_id}>
            {c.name} — ${c.entry_fee} entry, ${Number(c.prize_pool).toLocaleString()} pool
          </option>
        ))}
      </select>
      <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: '3px' }}>
        {payoutStatus === 'loading' && 'Fetching exact payout table…'}
        {payoutStatus === 'ok' && 'Contest Size / Entry Fee / Payout Positions filled, and the EV math will use this contest\'s real rank-by-rank payout table.'}
        {payoutStatus === 'error' && 'Contest Size / Entry Fee filled. No cash payout table found for this contest — Payout Positions defaults to the estimate below; edit if you know the real figure.'}
        {payoutStatus === '' && 'Fills Contest Size / Entry Fee / Payout Positions from DraftKings\' live data.'}
      </div>
    </div>
  );
}

function SettingsPanel({ settings, setSettings, allTeams, allGames, excludedTeams, setExcludedTeams, excludedGames, setExcludedGames, dkContests = [] }) {
  const [advancedOpen, setAdvancedOpen] = useState(false);
  return (
    <div style={{
      ...cardStyle,
      borderColor: 'rgba(0,242,254,0.15)',
      marginBottom: '12px',
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fill, minmax(200px, 1fr))',
      gap: '16px 24px',
    }}>
      {/* ── Contest Settings ── */}
      <div>
        <div style={sectionTitleStyle}>Contest Settings</div>

        {/* Platform (DK/FD) now lives in the page's top Control Bar --
            it's an earlier, page-level choice (which slate/pool loads at
            all), not a per-optimize-run setting buried in here. */}

        <button
          onClick={() => setAdvancedOpen(o => !o)}
          style={{
            ...pillBtnBase, width: '100%', textAlign: 'left',
            background: 'rgba(255,255,255,0.04)', color: 'var(--text-muted)',
            borderColor: 'rgba(255,255,255,0.1)', padding: '6px 10px', cursor: 'pointer',
          }}
        >
          Advanced Settings {advancedOpen ? '▲' : '▼'}
        </button>

        {advancedOpen && (
          <>
            <label style={{ ...labelStyle, marginTop: '10px' }}>Contest Type</label>
            <div style={{ display: 'flex', flexDirection: 'column', gap: '4px', marginBottom: '10px' }}>
              {[
                { value: 'cash', label: 'Cash (50/50)' },
                { value: 'flat', label: 'Flat GPP' },
                { value: 'top_heavy', label: 'Top-Heavy GPP' },
                { value: 'extreme_top_heavy', label: 'Extreme Top-Heavy' },
              ].map(opt => (
                <label key={opt.value} style={{ display: 'flex', alignItems: 'center', gap: '8px', cursor: 'pointer', fontSize: '0.8rem', color: settings.contestType === opt.value ? 'var(--text-white)' : 'var(--text-muted)' }}>
                  <input type="radio" name="contestType" value={opt.value}
                    checked={settings.contestType === opt.value}
                    onChange={() => setSettings(s => ({ ...s, contestType: opt.value }))}
                    style={{ accentColor: 'var(--accent-primary)', cursor: 'pointer' }}
                  />
                  {opt.label}
                </label>
              ))}
            </div>
          </>
        )}
      </div>

      {/* ── Contest Numbers ── */}
      <div>
        <div style={sectionTitleStyle}>Contest Numbers</div>

        {settings.platform === 'DK' && dkContests.length > 0 && (
          <ContestPicker dkContests={dkContests} setSettings={setSettings} />
        )}

        {settings.payoutStructure && (
          <div style={{ fontSize: '0.7rem', color: 'var(--accent-primary)', marginBottom: '8px' }}>
            ✓ Using this contest's real payout table ({settings.payoutStructure.length} tiers). Editing a field below reverts to the estimated curve.
          </div>
        )}

        {!advancedOpen && (
          <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>
            Contest Size ${settings.contestSize.toLocaleString()} · Entry Fee ${settings.entryFee} · {settings.payingPositions.toLocaleString()} paid — open Advanced Settings to edit.
          </div>
        )}

        {advancedOpen && [
          { key: 'contestSize', label: 'Contest Size', type: 'int' },
          { key: 'entryFee', label: 'Entry Fee ($)', type: 'float' },
          { key: 'payingPositions', label: 'Payout Positions', type: 'int' },
        ].map(({ key, label, type }) => (
          <div key={key} style={{ marginBottom: '8px' }}>
            <label style={labelStyle}>{label}</label>
            <input type="number" style={inputStyle}
              value={settings[key]}
              onChange={e => setSettings(s => ({
                ...s,
                [key]: type === 'float' ? parseFloat(e.target.value) || 0 : parseInt(e.target.value) || 0,
                // A hand-edited number no longer matches the fetched contest's
                // real tiers, so drop back to the contest-type-shaped estimate
                // rather than silently feeding the solver a mismatched table.
                payoutStructure: null,
              }))}
            />
          </div>
        ))}
      </div>

      {/* ── Lineup Settings ── */}
      <div>
        <div style={sectionTitleStyle}>Lineup Settings</div>

        <label style={labelStyle}># Lineups (1–1000)</label>
        <input type="number" min={1} max={1000} style={{ ...inputStyle, marginBottom: '8px' }}
          value={settings.nLineups}
          onChange={e => setSettings(s => ({ ...s, nLineups: Math.min(1000, Math.max(1, parseInt(e.target.value) || 1)) }))}
        />

        <label style={labelStyle}>Min Unique Players</label>
        <input type="number" min={1} max={8} style={{ ...inputStyle, marginBottom: '8px' }}
          value={settings.minUnique}
          onChange={e => setSettings(s => ({ ...s, minUnique: Math.min(8, Math.max(1, parseInt(e.target.value) || 1)) }))}
        />

        <label style={{ ...labelStyle, display: 'flex', alignItems: 'center', gap: '8px', cursor: 'pointer', textTransform: 'none', letterSpacing: 0, marginBottom: '10px' }}>
          <input type="checkbox" checked={settings.includeDstUnique}
            onChange={() => setSettings(s => ({ ...s, includeDstUnique: !s.includeDstUnique }))}
            style={{ accentColor: 'var(--accent-primary)', cursor: 'pointer', width: '14px', height: '14px' }}
          />
          <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>Include DST in Unique Count</span>
        </label>

        <label style={labelStyle}>Max Exposure %</label>
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <input type="range" min={10} max={100} step={5}
            value={settings.maxExposure}
            onChange={e => setSettings(s => ({ ...s, maxExposure: parseInt(e.target.value) }))}
            style={{ flex: 1 }}
          />
          <span style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--accent-primary)', minWidth: '36px' }}>
            {settings.maxExposure}%
          </span>
        </div>
      </div>

      {/* ── Filters ── */}
      <div>
        <div style={sectionTitleStyle}>Filters</div>

        <label style={labelStyle}>Hide &amp; Exclude below (pts)</label>
        <input type="number" min={0} step={0.5} style={{ ...inputStyle, marginBottom: '8px' }}
          value={settings.projThreshold}
          onChange={e => setSettings(s => ({ ...s, projThreshold: parseFloat(e.target.value) || 0 }))}
        />

        <label style={labelStyle}>Exclude Teams</label>
        <select multiple value={[...excludedTeams]}
          onChange={e => setExcludedTeams(new Set([...e.target.selectedOptions].map(o => o.value)))}
          style={{ ...inputStyle, height: '80px', fontSize: '0.78rem', marginBottom: '6px' }}
        >
          {allTeams.map(t => <option key={t} value={t}>{t}</option>)}
        </select>
        {excludedTeams.size > 0 && (
          <button onClick={() => setExcludedTeams(new Set())}
            style={{ fontSize: '0.7rem', color: 'var(--accent-primary)', background: 'none', border: 'none', cursor: 'pointer', display: 'block', marginBottom: '6px' }}>
            Clear team exclusions
          </button>
        )}

        <label style={labelStyle}>Exclude Games</label>
        <select multiple value={[...excludedGames]}
          onChange={e => setExcludedGames(new Set([...e.target.selectedOptions].map(o => o.value)))}
          style={{ ...inputStyle, height: '80px', fontSize: '0.78rem', marginBottom: '4px' }}
        >
          {allGames.map(g => <option key={g} value={g}>{g}</option>)}
        </select>
        {excludedGames.size > 0 && (
          <button onClick={() => setExcludedGames(new Set())}
            style={{ fontSize: '0.7rem', color: 'var(--accent-primary)', background: 'none', border: 'none', cursor: 'pointer', display: 'block' }}>
            Clear game exclusions
          </button>
        )}
      </div>
    </div>
  );
}

// ─── Main Component ───────────────────────────────────────────────────────────
export default function Optimizer({
  allSimResults,
  simResults,
  weekProjections,
  games,
  weeks = [],
  selectedWeek = 1,
  setSelectedWeek = () => {},
  optimizerLineups,
  setOptimizerLineups,
  optimizerSettings,
  setOptimizerSettings,
  setCurrentPage,
  dkSlates = [],
  selectedDraftGroupId = null,
  setSelectedDraftGroupId = () => {},
}) {
  const [view, setView] = useState('pool');
  const [settingsOpen, setSettingsOpen] = useState(false);
  const [resultsMode, setResultsMode] = useState('optimize'); // 'optimize' | 'lab'
  const [labOpen, setLabOpen] = useState(false);
  const emptyLabRow = () => ({ label: '', qb: '', rb: ['', ''], wr: ['', '', ''], te: '', flex: '', dst: '' });
  const [labRows, setLabRows] = useState([emptyLabRow()]);
  const [labError, setLabError] = useState('');

  // ── Live DK contest list (entry fee / prize pool / size) for the selected
  // slate -- lets the Settings panel auto-populate Contest Numbers from a
  // real live contest instead of requiring every field hand-typed. Refetches
  // whenever the slate picker above changes.
  const [dkContests, setDkContests] = useState([]);
  useEffect(() => {
    ApiService.getDkContests(selectedDraftGroupId).then(res => setDkContests(res.contests || [])).catch(() => {});
  }, [selectedDraftGroupId]);

  // ── Player pool = PURE sim output. Never mutated by user actions. Rebuilt
  // whenever the week's projections / sims / games change. All of the user's
  // manual work lives in `overlay` below and is re-applied on top in
  // `enrichedPool`, so a mid-week sim refresh no longer wipes it.
  const [playerPool, setPlayerPool] = useState(() =>
    buildPlayerPool(weekProjections, allSimResults, games)
  );

  useEffect(() => {
    const wpList = Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || []);
    if (wpList.length > 0 || Object.keys(allSimResults || {}).length > 0) {
      setPlayerPool(buildPlayerPool(weekProjections, allSimResults, games));
    }
  }, [weekProjections, allSimResults, games]);

  // ── Overlay: the user's manual layer on top of the sim pool.
  //   players[id] = { projAdjust?, projAbsolute?, ownershipPct?, ownershipFrozenValue?, locked?, excluded? }
  //   projAdjust      additive points delta vs the sim median (see enrichedPool)
  //   ownershipPct    a genuine manual override (always wins)
  //   ownershipFrozenValue  snapshot taken by "Freeze ownership" (see below)
  // Hydrated from / persisted to the backend per week (see the persistence
  // effects further down).
  const [overlay, setOverlay] = useState(emptyOverlay);

  // Set-shaped views + setters so SettingsPanel / GameBar keep their current API
  const excludedTeams = useMemo(() => new Set(overlay.excludedTeams), [overlay.excludedTeams]);
  const excludedGames = useMemo(() => new Set(overlay.excludedGames), [overlay.excludedGames]);
  const setExcludedTeams = (nextSet) => setOverlay(o => ({ ...o, excludedTeams: [...nextSet] }));
  const setExcludedGames = (nextSet) => setOverlay(o => ({ ...o, excludedGames: [...nextSet] }));
  const gameExclusions = overlay.gameExclusions;
  const setGameExclusions = (updater) => setOverlay(o => ({
    ...o, gameExclusions: typeof updater === 'function' ? updater(o.gameExclusions) : updater,
  }));

  const _patchPlayer = (id, patch) => setOverlay(o => ({
    ...o, players: { ...o.players, [id]: { ...(o.players[id] || {}), ...patch } },
  }));

  // ── Sticky ownership: snapshot every player's current live ownership into the
  // overlay so the optimize payload always carries an explicit value and the
  // backend's _compute_ownership (seeded partly by projections) stops reshuffling
  // the portfolio math between runs. Manual Own% edits still win; unfreeze drops
  // the snapshot and reverts to the live weekly number.
  const ownershipHandEdits = useMemo(
    () => Object.values(overlay.players).filter(ov => ov.ownershipPct != null).length,
    [overlay.players]
  );
  const freezeOwnership = () => setOverlay(o => {
    const players = { ...o.players };
    playerPool.forEach(p => {
      players[p.id] = { ...(players[p.id] || {}), ownershipFrozenValue: (p.ownershipPct ?? 0.5) };
    });
    return { ...o, players, ownershipFrozen: true, ownershipFrozenAt: new Date().toISOString() };
  });
  const unfreezeOwnership = () => setOverlay(o => {
    const players = {};
    for (const [id, ov] of Object.entries(o.players)) {
      const rest = { ...ov };
      delete rest.ownershipFrozenValue;
      if (Object.keys(rest).length) players[id] = rest;
    }
    return { ...o, players, ownershipFrozen: false, ownershipFrozenAt: null };
  });

  const hasSimData = (Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || [])).length > 0
    || Object.keys(allSimResults || {}).length > 0;

  // ── Settings
  const defaultSettings = {
    platform: 'DK', contestType: 'top_heavy',
    contestSize: 11000, entryFee: 18,
    payingPositions: 2200,
    // No separate "total entries" input -- an NFL DK/FD contest is assumed
    // full by lock (see handleOptimize's payload build), so contestSize
    // doubles as the entry count fed to the backend's EV math.
    payoutStructure: null, // real rank-by-rank tiers from a picked live contest; null = use the contest-type-shaped estimate
    nLineups: 20, minUnique: 2,
    includeDstUnique: false, maxExposure: 40,
    projThreshold: 0,
  };
  const [settings, setSettings] = useState(() => optimizerSettings || defaultSettings);

  // ── Filters
  const [playerSearch, setPlayerSearch] = useState('');
  const [posFilter, setPosFilter] = useState('ALL');
  // Team/game filter for the pool table: 'ALL' | 'G:<AWAY@HOME>' | 'T:<TEAM>'.
  // View-only -- does not exclude anyone from the optimizer.
  const [teamGameFilter, setTeamGameFilter] = useState('ALL');
  // Projection cell currently being typed into: { id, projection, gppProjection }
  // snapshotted at focus. While set, that player is exempt from the "hide
  // below X pts" threshold and sorts by the snapshot, so a half-typed value
  // (12 -> 1 -> 13) neither hides the row nor jumps it around the table. Cleared
  // on blur, at which point the real value applies.
  const [editingProj, setEditingProj] = useState(null);

  // ── Sorting
  const [sortField, setSortField] = useState('projection');
  const [sortAsc, setSortAsc] = useState(false);

  // ── The GPP-blend column reads alarmingly next to the real projections
  // (ceiling blend inflates the top plays well past their median), so it's
  // collapsed by default -- toggle it on when you actually want to sort/inspect it.
  const [showGppCol, setShowGppCol] = useState(false);
  const [resSortField, setResSortField] = useState('ev_pct');
  const [resSortAsc, setResSortAsc] = useState(false);

  // ── Optimizer run state
  const [isOptimizing, setIsOptimizing] = useState(false);
  const [optimizeProgress, setOptimizeProgress] = useState('');
  const [portfolioStats, setPortfolioStats] = useState(null);
  const [expandedLineupIdx, setExpandedLineupIdx] = useState(null);
  const [histLineup, setHistLineup] = useState(null);

  // ── Portfolio Exposure state
  const [exposureSearch, setExposureSearch] = useState('');
  const [exposurePosFilter, setExposurePosFilter] = useState('ALL');
  const [exposureSortField, setExposureSortField] = useState('exposure');
  const [exposureSortAsc, setExposureSortAsc] = useState(false);
  // Exposure drill-down: keys (`name_team_pos`, same as playerExposures' key)
  // of players clicked in the exposure panel. Non-empty -> the lineups table
  // shows only lineups containing ALL (or ANY, per exposureMatchMode) of them.
  const [selectedExposureKeys, setSelectedExposureKeys] = useState(() => new Set());
  const [exposureMatchMode, setExposureMatchMode] = useState('all'); // 'all' | 'any'

  // ── Sync settings to parent
  useEffect(() => { setOptimizerSettings(settings); }, [settings]);

  // ── Per-week persistence (see src/api/optimizer_store.py). On week change we
  // load that week's saved settings + overlay; while working, a debounced PUT
  // keeps the file in sync. `hydratingRef` + a serialized-baseline compare stop
  // a freshly-loaded state from immediately echoing itself back to disk.
  const [saveStatus, setSaveStatus] = useState('idle'); // 'idle' | 'saving' | 'saved' | 'error'
  const [autosaveBuilds, setAutosaveBuilds] = useState(true);
  // Gates the (per-slate) workspace-slot system below until this legacy
  // (per-week, slate-agnostic) hydration has settled -- both would otherwise
  // race to set `settings`/`overlay` on mount, and the slot system should win
  // since it's the more specific, newer source of truth.
  const [weekHydrated, setWeekHydrated] = useState(false);
  const hydratingRef = useRef(false);
  const savedBaselineRef = useRef(null);
  const prevWeekRef = useRef(null);
  const canonState = (s, ov, slate, prefs) => JSON.stringify({ settings: s, overlay: pruneOverlay(ov), slate, prefs });
  // Compact, order-stable fingerprint of the optimize inputs -- used to gate
  // autosaved builds (a pure re-run has the same fingerprint).
  const cheapHash = (str) => {
    let h = 0;
    for (let i = 0; i < str.length; i++) h = (Math.imul(h, 31) + str.charCodeAt(i)) | 0;
    return (h >>> 0).toString(36);
  };
  const inputsFingerprint = (s, ov, slate, prefs) => cheapHash(canonState(s, ov, slate, prefs));

  // ── Saved builds (see Builds panel below)
  const [builds, setBuilds] = useState([]);          // summaries, newest first
  const [buildsOpen, setBuildsOpen] = useState(false);
  // Bankroll accounts (see account_store.py) -- global, not per-week, so
  // fetched once on mount rather than in the per-week hydration effect below.
  const [accounts, setAccounts] = useState([]);
  useEffect(() => { ApiService.getAccounts().then(setAccounts); }, []);
  const [selectedBuildIds, setSelectedBuildIds] = useState(() => new Set());
  const [buildToast, setBuildToast] = useState('');
  const lastBuildHashRef = useRef(null);
  // The full build record currently on screen (via "Load lineups" below), kept
  // around so the results page can offer a separate "restore settings/overlay"
  // checkpoint action without re-fetching -- loading lineups and reverting your
  // working settings are two different asks and shouldn't be bundled into one
  // irreversible click. Cleared whenever the lineups on screen stop being that
  // build's (a new Optimize/Lab run, or switching save slots).
  const [restoredBuild, setRestoredBuild] = useState(null);

  useEffect(() => {
    if (!Number.isInteger(selectedWeek) || selectedWeek < 1 || selectedWeek > 22) return;
    let cancelled = false;
    hydratingRef.current = true;
    setSaveStatus('idle');
    setLabRows([emptyLabRow()]); setLabOpen(false); setLabError('');

    // End-of-week tidy: silently drop the *previous* week's throwaway autosaves
    // (not pinned / labeled / submitted) when moving on to a new week.
    const leaving = prevWeekRef.current;
    if (leaving && leaving !== selectedWeek) {
      ApiService.pruneOptimizerBuilds(leaving, OPTIMIZER_SEASON).then(r => {
        const n = (r.removed || []).length;
        if (n && !cancelled) setBuildToast(`Pruned ${n} throwaway autosave${n > 1 ? 's' : ''} from Week ${leaving}`);
      });
    }
    prevWeekRef.current = selectedWeek;

    ApiService.getOptimizerState(selectedWeek, OPTIMIZER_SEASON).then(st => {
      if (cancelled) return;
      const nextOverlay = st && st.overlay ? st.overlay : emptyOverlay();
      const nextAutosave = st && st.prefs && st.prefs.autosaveBuilds != null ? !!st.prefs.autosaveBuilds : true;
      setOverlay(nextOverlay);
      setAutosaveBuilds(nextAutosave);
      setSettings(prev => {
        const merged = st && st.settings ? { ...prev, ...st.settings } : prev;
        const slate = { draft_group_id: selectedDraftGroupId, platform: merged.platform };
        savedBaselineRef.current = canonState(merged, nextOverlay, slate, { autosaveBuilds: nextAutosave });
        return merged;
      });
      setTimeout(() => { if (!cancelled) hydratingRef.current = false; }, 0);
      setWeekHydrated(true);
    });
    ApiService.listOptimizerBuilds(selectedWeek, OPTIMIZER_SEASON).then(bs => {
      if (cancelled) return;
      setBuilds(bs);
      setSelectedBuildIds(new Set());
      lastBuildHashRef.current = bs[0] ? bs[0].inputs_hash ?? null : null;
    });
    return () => { cancelled = true; };
  }, [selectedWeek]); // eslint-disable-line react-hooks/exhaustive-deps

  useEffect(() => {
    const slate = { draft_group_id: selectedDraftGroupId, platform: settings.platform };
    const prefs = { autosaveBuilds };
    const serial = canonState(settings, overlay, slate, prefs);
    if (hydratingRef.current) { savedBaselineRef.current = serial; return; }
    if (serial === savedBaselineRef.current) return;
    const t = setTimeout(async () => {
      setSaveStatus('saving');
      const state = {
        schema_version: 1,
        season: OPTIMIZER_SEASON,
        week: selectedWeek,
        updated_at: new Date().toISOString(),
        slate, settings, prefs,
        overlay: pruneOverlay(overlay),
      };
      const ok = await ApiService.putOptimizerState(selectedWeek, state, OPTIMIZER_SEASON);
      if (ok) { savedBaselineRef.current = serial; setSaveStatus('saved'); }
      else setSaveStatus('error');
    }, 1500);
    return () => clearTimeout(t);
  }, [settings, overlay, selectedDraftGroupId, selectedWeek, autosaveBuilds]);

  // ── GPP projection weights from contest type
  const contestWeights = useMemo(
    () => GPP_WEIGHTS_BY_TYPE[settings.contestType] || GPP_WEIGHTS_BY_TYPE.top_heavy,
    [settings.contestType]
  );

  // ── Salary cap by platform
  const salaryCap = settings.platform === 'FD' ? 60000 : 50000;

  // ── Save slots: 3 switchable, autosaved workspace snapshots for this slate
  // (see useWorkspaceSlots / ShowdownOptimizer.jsx's identical use of it) --
  // pool overlay, settings, lineups, and Lab rows survive a page/tab switch,
  // and up to 3 different takes on the slate can be kept side by side.
  // Additive to the per-week state.json/builds above (a different concept --
  // that's an ever-growing autosave history; this is 3 deliberate "save
  // files"), gated on `weekHydrated` so the two don't race to set
  // settings/overlay on mount -- the slot system (more specific: per-slate,
  // not just per-week) intentionally hydrates second and wins.
  const classicSlateKey = weekHydrated
    ? `classic_${settings.platform}_${selectedDraftGroupId ?? 'default'}` : null;
  const classicWorkspaceSnapshot = useMemo(() => ({
    settings, overlay, lineups: optimizerLineups, portfolio: portfolioStats, labRows, resultsMode, view,
  }), [settings, overlay, optimizerLineups, portfolioStats, labRows, resultsMode, view]);
  const onHydrateClassicWorkspace = (data) => {
    // Reset to defaults (not a same-key merge) when the slot has nothing
    // saved for that field -- an empty Slot 2 must actually look empty, not
    // silently keep whatever Slot 1 left in memory. `platform` is pinned to
    // its current value rather than reset: it's embedded in `classicSlateKey`
    // itself, so resetting it here would flip the slate key mid-hydration.
    setSettings(s => ({ ...defaultSettings, ...(data.settings || {}), platform: s.platform }));
    setOverlay(data.overlay || emptyOverlay());
    setOptimizerLineups(data.lineups || []);
    setPortfolioStats(data.portfolio || null);
    setLabRows(data.labRows?.length ? data.labRows : [emptyLabRow()]);
    setResultsMode(data.resultsMode || 'optimize');
    setView((data.lineups || []).length ? (data.view || 'pool') : 'pool');
    setExpandedLineupIdx(null);
    setRestoredBuild(null);
  };
  const classicWorkspace = useWorkspaceSlots(classicSlateKey, selectedWeek, OPTIMIZER_SEASON, classicWorkspaceSnapshot, onHydrateClassicWorkspace);

  // ── Platform-adjusted + GPP-enriched pool
  // When FD is selected, swap DK percentiles for FD percentiles so the ILP
  // objective and all displayed columns reflect FD scoring.
  const enrichedPool = useMemo(() => {
    const frozen = overlay.ownershipFrozen;
    return playerPool.map(p => {
      const ov = overlay.players[p.id] || {};
      const isFD = settings.platform === 'FD';
      // Sim (unshifted) percentile values — switch to FD if needed
      const s25 = isFD ? (p.fd_p25 ?? p.p25) : p.p25;
      const s50 = isFD ? (p.fd_p50 ?? p.p50) : p.p50;
      const s75 = isFD ? (p.fd_p75 ?? p.p75) : p.p75;
      const s95 = isFD ? (p.fd_p95 ?? p.p95) : p.p95;
      const basePcts = isFD ? (p.fd_pcts_all ?? p.dk_pcts_all) : p.dk_pcts_all;

      // Manual median override. `projAdjust` (overlay) is an additive points
      // delta vs the sim median, so the whole distribution slides left/right by
      // that amount -- shape, spread and correlation untouched. 0 = sim as-is.
      // `projAbsolute` is the fallback for players with no sim distribution.
      const adj = ov.projAdjust || 0;
      const shift1 = (v) => (v == null ? v : parseFloat(Math.max(0, v + adj).toFixed(1)));
      const ap25 = shift1(s25), ap50 = shift1(s50), ap75 = shift1(s75), ap95 = shift1(s95);
      const activePcts = (adj !== 0 && basePcts)
        ? basePcts.map(v => Math.max(0, v + adj))
        : basePcts;

      const baseProj = ap50 ?? ov.projAbsolute ?? p.projection;

      // Ownership resolution: manual override > frozen snapshot > live weekly.
      const liveOwn = p.ownershipPct;  // = ownership_proj from the weekly sim (may be null)
      const ownershipPct = ov.ownershipPct != null
        ? ov.ownershipPct
        : (frozen ? (ov.ownershipFrozenValue ?? 0.5) : liveOwn);

      const enriched  = {
        ...p,
        projection:   parseFloat((baseProj ?? 0).toFixed(1)),
        p25: ap25, p50: ap50, p75: ap75, p95: ap95,
        mean: (adj !== 0 && p.mean != null) ? parseFloat(Math.max(0, p.mean + adj).toFixed(1)) : p.mean,
        hasPcts: !!(ap50 != null),
        active_pcts_all: activePcts,
        projAdjust: adj,
        hasProjOverride: adj !== 0 || ov.projAbsolute != null,
        optimalPct: p.optimal_pct ?? null,
        ownershipPct,
        locked: !!ov.locked,
        excluded: !!ov.excluded,
        // Sim (pre-shift) percentiles, kept so the "you've projected this
        // player above their sim ceiling" tint stays meaningful after a shift.
        simP50: s50, simP75: s75, simP95: s95,
      };
      enriched.gppProjection = computeGppProj(enriched, contestWeights);
      return enriched;
    });
  }, [playerPool, overlay, settings.platform, contestWeights]);

  // ── Derived lists
  const allTeams = useMemo(() => [...new Set(enrichedPool.map(p => p.team))].sort(), [enrichedPool]);
  const allGames = useMemo(() => [...new Set(enrichedPool.map(p => p.game).filter(Boolean))].sort(), [enrichedPool]);

  // Main Slate-only games for the game strip -- dk_main comes straight from
  // /api/games (src/api/app.py get_games()), which prefers DK's live
  // main_slate_teams roster and falls back to a weekday/time heuristic
  // (Thu/Fri/Sat/Mon and Sun-night games excluded) when the live feed isn't
  // up yet. Keeps Thursday/Monday/international games out of a strip whose
  // whole point is "what's in this slate", not "every game this week".
  const mainSlateGames = useMemo(() => (games || []).filter(g => g.dk_main), [games]);

  const isPlayerExcluded = useCallback((p) => {
    if (p.excluded) return true;
    if (excludedTeams.has(p.team)) return true;
    if (excludedGames.has(p.game)) return true;
    // Hide & Exclude: players below proj threshold are also excluded from optimizer
    if (settings.projThreshold > 0 && p.projection < settings.projThreshold) return true;
    if (p.game) {
      const state = gameExclusions[p.game] || 0;
      const [awayTeam, homeTeam] = p.game.split('@');
      if (state === 1 && p.team === awayTeam) return true;
      if (state === 2 && p.team === homeTeam) return true;
      if (state === 3) return true;
    }
    return false;
  }, [excludedTeams, excludedGames, gameExclusions, settings.projThreshold]);

  // Options for the pool's team/game dropdown -- derived from the pool itself
  // (not `games`) so it only lists matchups that actually have players here.
  const poolGameOptions = useMemo(() => {
    const gs = new Set(), ts = new Set();
    enrichedPool.forEach(p => { if (p.game) gs.add(p.game); if (p.team) ts.add(p.team); });
    return { games: [...gs].sort(), teams: [...ts].sort() };
  }, [enrichedPool]);

  const visiblePool = useMemo(() => {
    const threshold = settings.projThreshold || 0;
    const editId = editingProj?.id;
    // Value used for sorting: the focus-time snapshot for the row being edited
    // (see editingProj), the live value for everyone else.
    const sortVal = (p, field) => (p.id === editId && field in editingProj ? editingProj[field] : p[field]);
    return enrichedPool
      .filter(p => {
        if (posFilter !== 'ALL' && p.pos !== posFilter) return false;
        if (playerSearch && !p.name.toLowerCase().includes(playerSearch.toLowerCase())) return false;
        if (teamGameFilter.startsWith('G:') && p.game !== teamGameFilter.slice(2)) return false;
        if (teamGameFilter.startsWith('T:') && p.team !== teamGameFilter.slice(2)) return false;
        if (threshold > 0 && p.projection < threshold && p.id !== editId) return false;
        return true;
      })
      .sort((a, b) => {
        const field = sortField === 'gppProjection' ? 'gppProjection' : sortField;
        let va = sortVal(a, field) ?? 0;
        let vb = sortVal(b, field) ?? 0;
        if (typeof va === 'string') return sortAsc ? va.localeCompare(vb) : vb.localeCompare(va);
        return sortAsc ? va - vb : vb - va;
      });
  }, [enrichedPool, posFilter, playerSearch, teamGameFilter, editingProj, settings.projThreshold, sortField, sortAsc]);

  const totalCount = visiblePool.length;
  const excludedCount = visiblePool.filter(p => isPlayerExcluded(p)).length;

  const handleSort = (field) => {
    if (sortField === field) setSortAsc(!sortAsc);
    else { setSortField(field); setSortAsc(false); }
  };
  const handleResSort = (field) => {
    if (resSortField === field) setResSortAsc(!resSortAsc);
    else { setResSortField(field); setResSortAsc(false); }
  };

  // ── Pool mutations -- all write to `overlay`, never to `playerPool`.
  const toggleLock    = (id) => _patchPlayer(id, { locked:   !(overlay.players[id]?.locked) });
  const toggleExclude = (id) => _patchPlayer(id, { excluded: !(overlay.players[id]?.excluded) });

  // Manual median edits are stored as `projAdjust` -- an additive delta vs the
  // sim median -- so enrichedPool can slide the player's whole distribution
  // (percentiles, GPP blend, the array sent to the optimizer) by that amount.
  // Clearing the field resets to the sim. Players with no sim distribution fall
  // back to an absolute override (`projAbsolute`).
  const _simMedian = (p) => (settings.platform === 'FD' ? (p.fd_p50 ?? p.p50) : p.p50);
  const setProjOverride = (id, absVal) => {
    const poolP = playerPool.find(p => p.id === id);
    const base = poolP ? _simMedian(poolP) : null;
    const raw = parseFloat(absVal);
    if (!isFinite(raw))     return _patchPlayer(id, { projAdjust: 0, projAbsolute: null });
    if (base == null)       return _patchPlayer(id, { projAdjust: 0, projAbsolute: raw });
    return _patchPlayer(id, { projAdjust: parseFloat((raw - base).toFixed(1)), projAbsolute: null });
  };
  const setProjection = (id, val) => setProjOverride(id, val);
  const setProjToPct  = (id, pctVal) => setProjOverride(id, pctVal);
  const setOwnership  = (id, val) => _patchPlayer(id, { ownershipPct: parseFloat(val) || null });

  // ── Game bar toggle (4-state click cycle: 0 -> 1 (away) -> 2 (home) -> 3 (both) -> 0)
  const toggleGame = (gameLabel) => {
    setGameExclusions(prev => ({ ...prev, [gameLabel]: ((prev[gameLabel] || 0) + 1) % 4 }));
  };

  // ── Projection tint -- flags how far a manual override has pushed the
  // player past where the SIM had them (compared to the unshifted sim
  // percentiles, so it still means something after the distribution slides).
  const getProjTint = (p) => {
    const s50 = p.simP50 ?? p.p50, s75 = p.simP75 ?? p.p75;
    if (!p.hasPcts || s75 == null || s50 == null) return {};
    const val = p.projection;
    if (val > s75) return { background: 'rgba(239,68,68,0.3)' };
    if (val > s50 * 1.15) return { background: 'rgba(245,158,11,0.25)' };
    if (val > s50 * 1.05) return { background: 'rgba(245,197,66,0.18)' };
    if (p.projAdjust < 0) return { background: 'rgba(59,130,246,0.14)' };
    return {};
  };

  // ── Optimize
  // Player pool payload shared by ⚡ Optimize and the Lineup Lab. Only
  // priced players are eligible either way (DK hasn't priced an unpriced
  // player, so there's no real salary/cap to score them against).
  const buildOptimizerPlayersPayload = () => {
    const isCash = settings.contestType === 'cash';
    return enrichedPool.filter(p => p.salary != null).map(p => ({
      name: p.name, team: p.team, pos: p.pos,
      salary: p.salary,
      projection: p.p50 ?? p.projection ?? p.simProjection,   // shifted P50 for evaluation
      gpp_projection: isCash ? null : (p.gppProjection ?? null), // blended for ILP only
      locked: p.locked,
      excluded: isPlayerExcluded(p),
      ownership_pct: p.ownershipPct,
      dk_pcts_all: p.active_pcts_all || p.dk_pcts_all || null,
      dk_id: p.dk_id ?? null,
    }));
  };

  const handleOptimize = async () => {
    setIsOptimizing(true);
    setOptimizeProgress('');
    if (settings.nLineups > 50) setOptimizeProgress(`Generating lineup 0 of ${settings.nLineups}…`);

    // Two separate projection values go to the backend:
    //   projection     = P50/median — used by the field simulation & EV evaluation.
    //                    Both our lineup AND the field are scored on this scale so
    //                    comparisons are apples-to-apples. This fixes the -98% EV bug
    //                    where sending gppProjection here inflated the field cutoff.
    //                    p.p50 here is already the manually-shifted median (see
    //                    enrichedPool / projAdjust), so overrides flow through.
    //   gpp_projection = blended ceiling value — used ONLY by the ILP objective to
    //                    steer the solver toward higher-ceiling players. null for cash.
    //                    Also computed off the shifted percentiles.
    const payload = {
      players: buildOptimizerPlayersPayload(),
      n_lineups: settings.nLineups,
      salary_cap: salaryCap,
      contest_type: settings.contestType,
      contest_size: settings.contestSize,
      min_unique_players: settings.minUnique,
      include_dst_in_unique: settings.includeDstUnique,
      max_exposure: settings.maxExposure / 100,
      entry_fee: settings.entryFee,
      // An NFL DK/FD contest is assumed full by game lock, so contest size
      // doubles as the entry count for the backend's EV math -- no separate
      // "total entries" input to keep in sync.
      total_entries: settings.contestSize,
      paying_positions: settings.payingPositions,
      // Real rank-by-rank tiers from a picked live contest, when we have
      // them -- otherwise omitted so the backend falls back to its
      // contest-type-shaped estimate (_get_default_payout_structure).
      payout_structure: settings.payoutStructure || undefined,
      // Lets the backend find this week's cached archetype-composed field
      // sample (see field_simulator.py) instead of falling back to a
      // uniform-random field.
      week: selectedWeek,
    };

    try {
      const result = await ApiService.optimizeLineups(payload);
      setOptimizerLineups(result.lineups || []);
      setPortfolioStats(result.portfolio || null);
      setResultsMode('optimize');
      setView('results');
      setLastResult(result);
      setRestoredBuild(null);
      // Auto-save this run as a build -- but only when the inputs actually
      // changed since the last saved build (a pure re-run of the same pool
      // just produces a new RNG draw and isn't worth its own record).
      const slate = { draft_group_id: selectedDraftGroupId, platform: settings.platform };
      const hash = inputsFingerprint(settings, overlay, slate, { autosaveBuilds });
      if (autosaveBuilds && hash !== lastBuildHashRef.current) {
        await saveBuild(result, hash, slate, 'autosave');
      }
    } catch (err) {
      console.error('Optimizer error:', err);
    } finally {
      setIsOptimizing(false);
      setOptimizeProgress('');
    }
  };

  // ── Lineup Lab: score hand-built lineups through the same field sim ─────────
  const labOptions = useMemo(
    () => enrichedPool.filter(p => p.salary != null).slice().sort((a, b) => (b.salary || 0) - (a.salary || 0)),
    [enrichedPool],
  );
  const optKey = p => `${p.name}|${p.team}`;
  const optLabel = p => `${p.pos === 'DST' ? `${p.team} DST` : p.name} · ${p.team} · $${(p.salary || 0).toLocaleString()}`;
  const labByPos = useMemo(() => ({
    QB: labOptions.filter(p => p.pos === 'QB'),
    RB: labOptions.filter(p => p.pos === 'RB'),
    WR: labOptions.filter(p => p.pos === 'WR'),
    TE: labOptions.filter(p => p.pos === 'TE'),
    DST: labOptions.filter(p => p.pos === 'DST'),
    FLEX: labOptions.filter(p => ['RB', 'WR', 'TE'].includes(p.pos)),
  }), [labOptions]);

  const scoreLab = async () => {
    setLabError('');
    const rows = labRows
      .map((r, i) => ({ ...r, i }))
      .filter(r => r.qb && r.rb.filter(Boolean).length === 2 && r.wr.filter(Boolean).length === 3 && r.te && r.flex && r.dst);
    if (!rows.length) { setLabError('Fill in QB, 2 RB, 3 WR, TE, FLEX and DST for at least one Lab lineup.'); return; }
    for (const r of rows) {
      const picks = [r.qb, ...r.rb, ...r.wr, r.te, r.flex, r.dst];
      if (new Set(picks).size !== 9) { setLabError(`Lab lineup ${r.i + 1}: a player is picked twice.`); return; }
    }
    setIsOptimizing(true);
    try {
      const res = await ApiService.optimizeLineups({
        players: buildOptimizerPlayersPayload(),
        salary_cap: salaryCap,
        contest_type: settings.contestType,
        entry_fee: settings.entryFee,
        total_entries: settings.contestSize,
        paying_positions: settings.payingPositions,
        payout_structure: settings.payoutStructure || undefined,
        week: selectedWeek,
        manual_lineups: rows.map(r => ({
          qb: r.qb, rb: r.rb, wr: r.wr, te: r.te, flex: r.flex, dst: r.dst,
          label: r.label?.trim() || `Lab lineup ${r.i + 1}`,
        })),
      });
      // ApiService.optimizeLineups falls back to a random mock lineup set if
      // the request fails (`!res.ok` after retries) -- mock responses never
      // carry `mode`, so this also catches "the backend rejected the lab
      // lineups" (e.g. a bad name) instead of silently showing fake ones.
      if (!res || !res.lineups || res.mode !== 'manual') {
        setLabError('Backend rejected the request or is unreachable — check the console / that it\'s running.');
        return;
      }
      setOptimizerLineups(res.lineups);
      setPortfolioStats(res.portfolio || null);
      setResultsMode('lab');
      setExpandedLineupIdx(0);
      setView('results');
      setRestoredBuild(null);
    } catch (e) {
      console.error(e);
      setLabError(String(e.message || e));
    } finally {
      setIsOptimizing(false);
    }
  };

  // ── Builds -------------------------------------------------------------
  const [lastResult, setLastResult] = useState(null); // most recent /optimize response, for a manual "Save build"

  // Freeze everything needed to reconstruct + later review one Optimize run.
  const assembleBuild = (result, hash, slate, source) => {
    const poolById = Object.fromEntries(enrichedPool.map(p => [p.id, p]));
    const players_used = {};
    // Slim the lineup player rows -- the /optimize response carries the full
    // 101-pt dk_pcts_all array per player, ~130 KB of noise in a saved build.
    // players_used already keeps p25/p50/p75/p95 for review.
    const lineups = (result.lineups || []).map(lu => ({
      ...lu,
      players: (lu.players || []).map(pl => ({
        name: pl.name, pos: pl.pos, team: pl.team, slot: pl.slot,
        salary: pl.salary, projection: pl.projection, ownership_pct: pl.ownership_pct,
        dk_id: pl.dk_id,
      })),
    }));
    lineups.forEach(lu => lu.players.forEach(pl => {
      const id = `${pl.name}_${pl.team}`;
      if (players_used[id]) return;
      const p = poolById[id];
      players_used[id] = p ? {
        name: p.name, team: p.team, pos: p.pos, salary: p.salary,
        proj: p.projection, gpp_proj: p.gppProjection,
        p25: p.p25, p50: p.p50, p75: p.p75, p95: p.p95,
        proj_adjust: p.projAdjust || 0, ownership_pct: p.ownershipPct,
      } : { name: pl.name, team: pl.team, pos: pl.pos, salary: pl.salary };
    }));
    return {
      schema_version: 1,
      source,
      inputs_hash: hash,
      slate,
      settings,
      overlay: pruneOverlay(overlay),
      players_used,
      lineups,
      portfolio: result.portfolio || null,
      label: null, pinned: false, submitted: false,
    };
  };

  const saveBuild = async (result, hash, slate, source) => {
    const build = assembleBuild(result, hash, slate, source);
    const saved = await ApiService.createOptimizerBuild(selectedWeek, build, OPTIMIZER_SEASON);
    if (saved) {
      lastBuildHashRef.current = hash;
      const bs = await ApiService.listOptimizerBuilds(selectedWeek, OPTIMIZER_SEASON);
      setBuilds(bs);
    }
    return saved;
  };

  const manualSaveBuild = async () => {
    if (!lastResult) return;
    const slate = { draft_group_id: selectedDraftGroupId, platform: settings.platform };
    const hash = inputsFingerprint(settings, overlay, slate, { autosaveBuilds });
    await saveBuild(lastResult, hash, slate, 'manual');
    setBuildToast('Build saved');
  };

  const patchBuildRow = async (buildId, patch) => {
    const saved = await ApiService.patchOptimizerBuild(selectedWeek, buildId, patch, OPTIMIZER_SEASON);
    if (saved) {
      setBuilds(bs => bs.map(b => b.build_id === buildId
        ? { ...b, label: saved.label, pinned: !!saved.pinned, submitted: !!saved.submitted, account_id: saved.account_id ?? null }
        : b));
    }
  };

  const deleteBuildRows = async (ids) => {
    for (const id of ids) await ApiService.deleteOptimizerBuild(selectedWeek, id, OPTIMIZER_SEASON);
    setBuilds(bs => bs.filter(b => !ids.includes(b.build_id)));
    setSelectedBuildIds(new Set());
  };

  // Load just the lineups a past build produced -- for reviewing/exporting an
  // old run without touching your current settings/overlay. Player rows were
  // slimmed for storage (see assembleBuild) but keep dk_id, so DK export still
  // works off a restored build as long as that ID hasn't gone stale.
  const restoreBuildLineups = async (buildId) => {
    const b = await ApiService.getOptimizerBuild(selectedWeek, buildId, OPTIMIZER_SEASON);
    if (!b) return;
    if (!window.confirm(`Load this build's ${b.lineups?.length || 0} lineup(s) into view? This replaces the lineups currently shown here (and will overwrite the active save slot once autosave runs).`)) return;
    setOptimizerLineups(b.lineups || []);
    setPortfolioStats(b.portfolio || null);
    setResultsMode('optimize');
    setView((b.lineups || []).length ? 'results' : 'pool');
    setExpandedLineupIdx(null);
    setRestoredBuild(b);
    setBuildToast('Lineups loaded — use "Restore settings" on the results page to also load this build\'s checkpoint');
  };

  // Separate, explicit checkpoint action: load the settings/overlay (and slate)
  // that produced the currently-viewed build's lineups. Uses the build already
  // fetched by restoreBuildLineups rather than re-fetching.
  const restoreBuildSettings = () => {
    const b = restoredBuild;
    if (!b) return;
    if (!window.confirm("Load this build's settings and projection adjustments into your working state? Your current settings/overlay will be replaced.")) return;
    if (b.overlay) setOverlay(b.overlay);
    if (b.settings) setSettings(s => ({ ...s, ...b.settings }));
    if (b.slate && b.slate.draft_group_id && b.slate.draft_group_id !== selectedDraftGroupId) {
      setSelectedDraftGroupId(b.slate.draft_group_id);
    }
    setBuildToast('Checkpoint restored — the debounced save will persist it as your working state');
  };

  const pruneBuildsNow = async () => {
    const r = await ApiService.pruneOptimizerBuilds(selectedWeek, OPTIMIZER_SEASON);
    const n = (r.removed || []).length;
    setBuildToast(n ? `Pruned ${n} throwaway autosave${n > 1 ? 's' : ''}` : 'Nothing to prune');
    const bs = await ApiService.listOptimizerBuilds(selectedWeek, OPTIMIZER_SEASON);
    setBuilds(bs);
  };

  useEffect(() => {
    if (!buildToast) return;
    const t = setTimeout(() => setBuildToast(''), 4000);
    return () => clearTimeout(t);
  }, [buildToast]);

  // ── Results helpers
  const sortedLineups = useMemo(() => {
    const ls = [...(optimizerLineups || [])];
    return ls.sort((a, b) => {
      let va = a[resSortField] ?? 0, vb = b[resSortField] ?? 0;
      if (typeof va === 'string') return resSortAsc ? va.localeCompare(vb) : vb.localeCompare(va);
      return resSortAsc ? va - vb : vb - va;
    });
  }, [optimizerLineups, resSortField, resSortAsc]);

  const playerExposures = useMemo(() => {
    if (!optimizerLineups || optimizerLineups.length === 0) return [];
    const counts = {};
    const poolMap = {};
    enrichedPool.forEach(p => {
      poolMap[p.id] = p;
      poolMap[p.name.toLowerCase()] = p;
    });

    optimizerLineups.forEach(lineup => {
      lineup.players.forEach(p => {
        const key = `${p.name}_${p.team}_${p.pos}`;
        if (!counts[key]) {
          const poolPlayer = poolMap[`${p.name}_${p.team}`] || poolMap[p.name.toLowerCase()];
          const projOwn = p.ownership_pct ?? poolPlayer?.ownershipPct ?? 0;
          counts[key] = {
            key,
            name: p.pos === 'DST' ? `${p.team} DST` : p.name,
            pos: p.pos,
            team: p.team,
            salary: p.salary || poolPlayer?.salary || 0,
            projection: p.projection || poolPlayer?.projection || 0,
            ownership: projOwn,
            count: 0
          };
        }
        counts[key].count++;
      });
    });

    const total = optimizerLineups.length;
    return Object.values(counts).map(p => {
      const exposure = (p.count / total) * 100;
      const leverage = p.ownership ? (exposure - p.ownership) : exposure;
      return {
        ...p,
        exposure,
        leverage
      };
    });
  }, [optimizerLineups, enrichedPool]);

  const filteredExposures = useMemo(() => {
    return playerExposures
      .filter(p => {
        const matchesSearch = p.name.toLowerCase().includes(exposureSearch.toLowerCase()) || p.team.toLowerCase().includes(exposureSearch.toLowerCase());
        const matchesPos = exposurePosFilter === 'ALL' || p.pos === exposurePosFilter;
        return matchesSearch && matchesPos;
      })
      .sort((a, b) => {
        let valA = a[exposureSortField];
        let valB = b[exposureSortField];
        
        if (exposureSortField === 'name') {
          return exposureSortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
        }
        
        if (valA == null) return exposureSortAsc ? -1 : 1;
        if (valB == null) return exposureSortAsc ? 1 : -1;
        
        return exposureSortAsc ? valA - valB : valB - valA;
      });
  }, [playerExposures, exposureSearch, exposurePosFilter, exposureSortField, exposureSortAsc]);

  // ── Exposure drill-down (click players in the exposure panel)
  const lineupKey = (p) => `${p.name}_${p.team}_${p.pos}`; // must match playerExposures' key

  const toggleExposureSelect = (key) => setSelectedExposureKeys(prev => {
    const next = new Set(prev);
    if (next.has(key)) next.delete(key); else next.add(key);
    return next;
  });

  // A fresh optimize / loaded build can drop players -- prune stale selections
  // and collapse any expanded row (its index no longer points at the same lineup).
  useEffect(() => {
    setSelectedExposureKeys(prev => {
      if (prev.size === 0) return prev;
      const live = new Set(playerExposures.map(p => p.key));
      const next = new Set([...prev].filter(k => live.has(k)));
      return next.size === prev.size ? prev : next;
    });
  }, [playerExposures]);
  useEffect(() => { setExpandedLineupIdx(null); }, [selectedExposureKeys, exposureMatchMode]);

  /** Lineups shown in the results table.
   * Inputs: sortedLineups (all lineups, current sort), selectedExposureKeys
   *   (Set<string> of player keys), exposureMatchMode ('all' | 'any').
   * Output: array of lineup objects -- sortedLineups unchanged when nothing is
   *   selected, else only lineups containing all/any of the selected players.
   * Exports keep using sortedLineups, so filtering never trims a DK upload. */
  const displayedLineups = useMemo(() => {
    if (selectedExposureKeys.size === 0) return sortedLineups;
    const sel = [...selectedExposureKeys];
    return sortedLineups.filter(lu => {
      const keys = new Set(lu.players.map(lineupKey));
      return exposureMatchMode === 'all' ? sel.every(k => keys.has(k)) : sel.some(k => keys.has(k));
    });
  }, [sortedLineups, selectedExposureKeys, exposureMatchMode]);

  /** Average per-lineup metrics over a lineup set.
   * Input: array of lineup objects. Output: { n, ev_pct, portfolio_score,
   *   itm_pct, top1_pct, top01_pct, p95, salary } (means; null if unavailable).
   * Used to compare the drilled-down subset against the whole portfolio. */
  const summarizeLineups = (ls) => {
    const avg = (f) => {
      const vals = ls.map(f).filter(v => v != null && isFinite(v));
      return vals.length ? vals.reduce((s, v) => s + v, 0) / vals.length : null;
    };
    return {
      n: ls.length,
      ev_pct: avg(l => l.ev_pct),
      portfolio_score: avg(l => l.portfolio_score),
      itm_pct: avg(l => l.itm_pct),
      top1_pct: avg(l => l.top1_pct),
      top01_pct: avg(l => l.top01_pct),
      p95: avg(l => l.lineup_p95 ?? l.projected_score),
      salary: avg(l => l.total_salary),
    };
  };
  const portfolioAvg = useMemo(() => summarizeLineups(sortedLineups), [sortedLineups]); // eslint-disable-line react-hooks/exhaustive-deps
  const selectionAvg = useMemo(
    () => (selectedExposureKeys.size ? summarizeLineups(displayedLineups) : null),
    [displayedLineups, selectedExposureKeys] // eslint-disable-line react-hooks/exhaustive-deps
  );
  const selectedExposurePlayers = useMemo(
    () => playerExposures.filter(p => selectedExposureKeys.has(p.key)),
    [playerExposures, selectedExposureKeys]
  );

  const evColor = (v) => v > 0 ? '#22c55e' : v < 0 ? '#ef4444' : 'var(--text-muted)';

  const getLineupRowTint = (lineup, all) => {
    if (!all.length) return {};
    const evs = all.map(l => l.ev_pct).sort((a, b) => a - b);
    const q1 = evs[Math.floor(evs.length * 0.25)];
    const q3 = evs[Math.floor(evs.length * 0.75)];
    if (lineup.ev_pct >= q3) return { background: 'rgba(34,197,94,0.08)' };
    if (lineup.ev_pct <= q1) return { background: 'rgba(239,68,68,0.08)' };
    return {};
  };

  const getSlottedColumns = (players) => {
    const slots = { QB: [], RB: [], WR: [], FLEX: [], TE: [], DST: [] };
    players.forEach(p => { const s = p.slot || p.pos; if (slots[s]) slots[s].push(p); });
    return [slots.QB[0], slots.RB[0], slots.RB[1], slots.WR[0], slots.WR[1], slots.WR[2], slots.FLEX[0], slots.TE[0], slots.DST[0]];
  };

  // Human-readable summary export (last-name only, salary + projected score) --
  // useful for reviewing lineups, but NOT accepted by DK's lineup-upload form
  // (no player IDs). See exportDkUploadCSV for the real upload format.
  const exportSummaryCSV = () => {
    const header = 'QB,RB,RB,WR,WR,WR,FLEX,TE,DST,Salary,Projected Score';
    const rows = sortedLineups.map(lu => {
      const cols = getSlottedColumns(lu.players);
      const names = cols.map(p => p ? p.name.split(' ').slice(-1)[0] : '');
      return [...names, lu.total_salary, lu.projected_score].join(',');
    });
    const blob = new Blob([[header, ...rows].join('\n')], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url; a.download = 'optimizer_lineups_summary.csv'; a.click();
    URL.revokeObjectURL(url);
  };

  // DK's actual lineup-upload format: header column order QB,RB,RB,WR,WR,WR,
  // TE,FLEX,DST (note TE before FLEX -- differs from getSlottedColumns' display
  // order), each cell "PlayerName (draftableId)" -- the exact format DK's own
  // "Bulk Upload" contest-entry tool expects. draftableId comes from the live
  // DK scraper (src/scrapers/dk_scraper.py) via each player's dk_id field, so
  // it's only correct for the CURRENT live Main Slate -- re-export if salaries
  // refresh. A player DK's feed didn't match (dk_id null, e.g. a name-matching
  // miss or someone outside the Main Slate pool) exports by name only, which
  // DK's upload will not resolve -- flagged via the alert below rather than
  // silently shipping a broken row.
  const exportDkUploadCSV = () => {
    const header = 'QB,RB,RB,WR,WR,WR,TE,FLEX,DST';
    let missingIds = 0;
    const rows = sortedLineups.map(lu => {
      const bySlot = { QB: [], RB: [], WR: [], TE: [], FLEX: [], DST: [] };
      lu.players.forEach(p => { const s = p.slot || p.pos; if (bySlot[s]) bySlot[s].push(p); });
      const ordered = [bySlot.QB[0], bySlot.RB[0], bySlot.RB[1], bySlot.WR[0], bySlot.WR[1], bySlot.WR[2], bySlot.TE[0], bySlot.FLEX[0], bySlot.DST[0]];
      return ordered.map(p => {
        if (!p) return '';
        if (p.dk_id == null) { missingIds++; return p.name; }
        return `${p.name} (${p.dk_id})`;
      }).join(',');
    });
    if (missingIds > 0) {
      alert(`${missingIds} player slot(s) don't have a live DK ID (not matched to DK's current salary feed) and were exported by name only -- DK's upload will likely reject those rows. Refresh salaries or swap those players before uploading.`);
    }
    const blob = new Blob([[header, ...rows].join('\n')], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url; a.download = 'dk_upload_lineups.csv'; a.click();
    URL.revokeObjectURL(url);
  };

  const elcColor = (elc, n) => {
    if (!n) return 'var(--text-muted)';
    const pct = elc / n;
    return pct >= 0.8 ? '#22c55e' : pct >= 0.6 ? '#f59e0b' : '#ef4444';
  };

  // ── Sort lineups by player slot for detail expand
  const sortLineupPlayers = (players) => {
    const order = ['QB', 'RB', 'WR', 'TE', 'FLEX', 'DST'];
    return [...players].sort((a, b) => order.indexOf(a.slot || a.pos) - order.indexOf(b.slot || b.pos));
  };

  // ─────────────────────────────────────────────────────────────────────────
  // RENDER
  // ─────────────────────────────────────────────────────────────────────────
  return (
    // ── Wide container: override App's 1280px max-width for the optimizer
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>

      {/* ── Control Bar */}
      <div className="glass-panel flex-between" style={{
        marginBottom: '20px',
        padding: '12px 20px',
        borderRadius: '12px',
        border: '1px solid var(--border-glass)',
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        flexWrap: 'wrap',
        gap: '15px'
      }}>
        {/* Slate selections */}
        <div style={{ display: 'flex', alignItems: 'center', gap: '15px', flexWrap: 'wrap' }}>
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <span style={{fontWeight: 700, color: 'var(--text-white)', marginRight: '10px'}}>Slate Focus:</span>
            <div className="slate-selector">
              <button className="slate-btn active">Traditional Multi-Game</button>
              <button className="slate-btn" style={{ cursor: 'pointer' }}
                onClick={() => setCurrentPage && setCurrentPage('showdown_optimizer')}
                title="Open the single-game Showdown optimizer">Showdown Single-Game ✧</button>
            </div>
          </div>
          {weeks.length > 0 && (
            <div style={{ display: 'flex', alignItems: 'center' }}>
              <span style={{fontWeight: 700, color: 'var(--text-white)', marginRight: '10px'}}>Week:</span>
              <select
                value={selectedWeek}
                onChange={e => setSelectedWeek(Number(e.target.value))}
                title="Each week has its own saved settings, projection adjustments and (Phase 3) builds"
                style={{ ...pillBtnBase, padding: '6px 10px', cursor: 'pointer', background: 'rgba(255,255,255,0.04)', color: 'var(--text-white)' }}
              >
                {weeks.map(w => <option key={w} value={w}>Week {w}</option>)}
              </select>
            </div>
          )}
        </div>

        {/* Platform -- an earlier, page-level choice than the in-Settings
            contest numbers: it decides which pool/salary cap/scoring loads
            at all, so it belongs up here rather than behind a Settings click. */}
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <span style={{fontWeight: 700, color: 'var(--text-white)', marginRight: '10px'}}>Platform:</span>
          <div style={{ display: 'flex', gap: '6px' }}>
            {['DK', 'FD'].map(plat => (
              <button key={plat}
                onClick={() => setSettings(s => ({ ...s, platform: plat }))}
                style={{
                  ...pillBtnBase,
                  background: settings.platform === plat ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.04)',
                  color: settings.platform === plat ? 'var(--accent-primary)' : 'var(--text-white)',
                  borderColor: settings.platform === plat ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.1)',
                  padding: '6px 14px', cursor: 'pointer',
                }}
              >{plat}</button>
            ))}
          </div>
        </div>
      </div>

      {/* ── Page Header */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: '12px', flexWrap: 'wrap', gap: '10px' }}>
        <div>
          <h1 style={{ marginBottom: '2px', fontSize: '1.6rem' }}>⚡ DFS Optimizer</h1>
          <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', margin: 0 }}>
            Multi-Game Slate · DraftKings Traditional
          </p>
          {dkSlates.length > 0 && (
            <div style={{ marginTop: '8px', maxWidth: '260px' }}>
              <label style={labelStyle}>DK Slate</label>
              <select
                style={inputStyle}
                value={selectedDraftGroupId ?? ''}
                onChange={e => setSelectedDraftGroupId(e.target.value ? Number(e.target.value) : null)}
              >
                {dkSlates.map(s => (
                  <option key={s.draft_group_id} value={s.draft_group_id} disabled={!s.is_default}>
                    {s.label}{s.contest_count != null ? ` (${s.contest_count} contest${s.contest_count === 1 ? '' : 's'})` : ''}{s.is_default ? '' : ' — not yet supported'}
                  </option>
                ))}
              </select>
              {dkSlates.length > 1 && (
                <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: '3px' }}>
                  Only Main Slate is wired up today — other live slates are visible but disabled until player-pool projections support them.
                </div>
              )}
            </div>
          )}
          {classicSlateKey && (
            <div style={{ marginTop: '8px' }}>
              <SlotSwitcher slots={classicWorkspace.slots} active={classicWorkspace.active} saveStatus={classicWorkspace.saveStatus}
                onSwitch={classicWorkspace.switchSlot} onRename={classicWorkspace.renameSlot} onClear={classicWorkspace.clearSlot} />
            </div>
          )}
        </div>
        <div style={{ display: 'flex', gap: '8px', alignItems: 'center', flexWrap: 'wrap' }}>
          {/* Per-week autosave status */}
          <span style={{ fontSize: '0.75rem', color: saveStatus === 'error' ? 'var(--accent-red)' : 'var(--text-muted)', minWidth: '52px' }}
            title={`Week ${selectedWeek} settings & adjustments are saved automatically`}>
            {saveStatus === 'saving' && '💾 Saving…'}
            {saveStatus === 'saved' && '✓ Saved'}
            {saveStatus === 'error' && '⚠ Save failed'}
          </span>
          {view === 'results' && (
            <button onClick={() => setView('pool')} style={{
              padding: '8px 14px', background: 'rgba(255,255,255,0.06)',
              color: 'var(--text-white)', border: '1px solid rgba(255,255,255,0.12)',
              borderRadius: '8px', cursor: 'pointer', fontWeight: 600, fontSize: '0.85rem',
            }}>← Back to Pool</button>
          )}
          <button
            onClick={() => setSettingsOpen(s => !s)}
            style={{
              padding: '8px 14px',
              background: settingsOpen ? 'rgba(0,242,254,0.12)' : 'rgba(255,255,255,0.06)',
              color: settingsOpen ? 'var(--accent-primary)' : 'var(--text-white)',
              border: `1px solid ${settingsOpen ? 'rgba(0,242,254,0.3)' : 'rgba(255,255,255,0.12)'}`,
              borderRadius: '8px', cursor: 'pointer', fontWeight: 600, fontSize: '0.85rem',
            }}
          >
            ⚙️ Settings {settingsOpen ? '▲' : '▼'}
          </button>
          <button
            onClick={() => { setLabOpen(o => !o); if (view !== 'pool') setView('pool'); }}
            title="Score a hand-built lineup through the same tournament field sim as the optimizer"
            style={{
              padding: '8px 14px',
              background: labOpen ? 'rgba(0,242,254,0.12)' : 'rgba(255,255,255,0.06)',
              color: labOpen ? 'var(--accent-primary)' : 'var(--text-white)',
              border: `1px solid ${labOpen ? 'rgba(0,242,254,0.3)' : 'rgba(255,255,255,0.12)'}`,
              borderRadius: '8px', cursor: 'pointer', fontWeight: 600, fontSize: '0.85rem',
            }}
          >
            ✍️ Lineup Lab
          </button>
          <button
            className="btn-primary"
            disabled={isOptimizing}
            onClick={handleOptimize}
            style={{ fontSize: '0.9rem', padding: '8px 20px', letterSpacing: '0.02em' }}
          >
            {isOptimizing ? (
              <><span className="spinner" style={{ width: '16px', height: '16px', borderWidth: '2px' }} /> Optimizing…</>
            ) : '⚡ Optimize'}
          </button>
          {optimizerLineups && optimizerLineups.length > 0 && view !== 'results' && (
            <button onClick={() => setView('results')} style={{
              padding: '8px 14px', background: 'rgba(34,197,94,0.1)', color: '#22c55e',
              border: '1px solid rgba(34,197,94,0.25)', borderRadius: '8px', cursor: 'pointer',
              fontWeight: 600, fontSize: '0.85rem',
            }}>
              View {optimizerLineups.length} Lineups →
            </button>
          )}
        </div>
      </div>

      {/* ── Progress text */}
      {optimizeProgress && (
        <div style={{ textAlign: 'center', fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '8px' }}>
          {optimizeProgress}
        </div>
      )}

      {/* ── No sim data banner */}
      {!hasSimData && (
        <div style={{
          background: 'rgba(245,197,66,0.1)', border: '1px solid rgba(245,197,66,0.3)',
          borderRadius: '10px', padding: '10px 16px', marginBottom: '12px',
          display: 'flex', alignItems: 'center', gap: '10px', fontSize: '0.82rem', color: '#f5c542',
        }}>
          ⚠️ Running on baseline projections — run simulations on the DFS Simulator page for enhanced percentile data.
        </div>
      )}

      {/* ── Builds panel (collapsible): every Optimize run, kept for reference and review */}
      <div style={{ border: '1px solid var(--border-glass)', borderRadius: '10px', marginBottom: '12px', overflow: 'hidden' }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px', padding: '8px 14px', background: 'rgba(255,255,255,0.03)', flexWrap: 'wrap' }}>
          <button onClick={() => setBuildsOpen(o => !o)} style={{ ...pillBtnBase, background: 'transparent', border: 'none', color: 'var(--text-white)', fontWeight: 700, padding: 0, cursor: 'pointer' }}>
            {buildsOpen ? '▾' : '▸'} Builds — Week {selectedWeek} <span style={{ color: 'var(--text-muted)', fontWeight: 400 }}>({builds.length})</span>
          </button>
          <label style={{ display: 'flex', alignItems: 'center', gap: '5px', fontSize: '0.75rem', color: 'var(--text-muted)', cursor: 'pointer', marginLeft: 'auto' }}>
            <input type="checkbox" checked={autosaveBuilds} onChange={e => setAutosaveBuilds(e.target.checked)} />
            Auto-save runs
          </label>
          {lastResult && (
            <button onClick={manualSaveBuild} style={{ ...pillBtnBase, fontSize: '0.72rem', background: 'rgba(0,242,254,0.1)', color: 'var(--accent-primary)', borderColor: 'rgba(0,242,254,0.25)' }}>
              Save current run
            </button>
          )}
          <button onClick={pruneBuildsNow} title="Delete this week's autosave builds that aren't pinned, labeled or submitted"
            style={{ ...pillBtnBase, fontSize: '0.72rem', background: 'rgba(255,255,255,0.04)', color: 'var(--text-muted)', borderColor: 'rgba(255,255,255,0.1)' }}>
            Prune autosaves
          </button>
          {buildToast && <span style={{ fontSize: '0.72rem', color: 'var(--accent-green)' }}>{buildToast}</span>}
        </div>

        {buildsOpen && (
          <div style={{ padding: '4px 6px 8px' }}>
            {builds.length === 0 ? (
              <div style={{ padding: '14px', textAlign: 'center', color: 'var(--text-muted)', fontSize: '0.8rem' }}>
                No builds yet this week. {autosaveBuilds ? 'Each Optimize run with changed inputs is saved here.' : 'Auto-save is off — use "Save current run".'}
              </div>
            ) : (
              <>
                {selectedBuildIds.size > 0 && (
                  <div style={{ padding: '4px 8px' }}>
                    <button onClick={() => deleteBuildRows([...selectedBuildIds])}
                      style={{ ...pillBtnBase, fontSize: '0.72rem', background: 'rgba(239,68,68,0.12)', color: '#ef4444', borderColor: 'rgba(239,68,68,0.3)' }}>
                      Delete selected ({selectedBuildIds.size})
                    </button>
                  </div>
                )}
                <div style={{ overflowX: 'auto' }}>
                  <table style={{ width: '100%', fontSize: '0.78rem', borderCollapse: 'collapse' }}>
                    <thead>
                      <tr style={{ color: 'var(--text-muted)', textAlign: 'left' }}>
                        <th style={{ padding: '4px 6px', width: '24px' }}></th>
                        <th style={{ padding: '4px 6px' }}>When</th>
                        <th style={{ padding: '4px 6px' }}>Label</th>
                        <th style={{ padding: '4px 6px' }}>Contest</th>
                        <th style={{ padding: '4px 6px' }}>Account</th>
                        <th style={{ padding: '4px 6px', textAlign: 'right' }}>Lineups</th>
                        <th style={{ padding: '4px 6px', textAlign: 'right' }}>Port. EV</th>
                        <th style={{ padding: '4px 6px', textAlign: 'right' }}>Actions</th>
                      </tr>
                    </thead>
                    <tbody>
                      {builds.map(b => (
                        <tr key={b.build_id} style={{ borderTop: '1px solid var(--border-glass)' }}>
                          <td style={{ padding: '4px 6px' }}>
                            <input type="checkbox" checked={selectedBuildIds.has(b.build_id)}
                              onChange={e => setSelectedBuildIds(prev => {
                                const n = new Set(prev);
                                e.target.checked ? n.add(b.build_id) : n.delete(b.build_id);
                                return n;
                              })} />
                          </td>
                          <td style={{ padding: '4px 6px', whiteSpace: 'nowrap', color: 'var(--text-secondary)' }}>
                            {relStamp(b.created_at)}
                            <span style={{ marginLeft: '5px', fontSize: '0.62rem', color: 'var(--text-muted)' }}>{b.source === 'manual' ? 'saved' : 'auto'}</span>
                          </td>
                          <td style={{ padding: '4px 6px' }}>
                            <input
                              defaultValue={b.label || ''}
                              placeholder="—"
                              onBlur={e => { const v = e.target.value.trim() || null; if (v !== (b.label || null)) patchBuildRow(b.build_id, { label: v }); }}
                              style={{ ...inputStyle, padding: '2px 6px', fontSize: '0.75rem', width: '110px' }}
                            />
                          </td>
                          <td style={{ padding: '4px 6px', color: 'var(--text-muted)', maxWidth: '160px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }} title={b.contest_name || ''}>
                            {b.contest_name || '—'}
                          </td>
                          <td style={{ padding: '4px 6px' }}>
                            <select
                              value={b.account_id || ''}
                              onChange={e => patchBuildRow(b.build_id, { account_id: e.target.value || null })}
                              title="Tag this build's lineups to a bankroll account -- every lineup gets registered as a paper entry so the Bankroll page can track it (see /bankroll)"
                              style={{ ...inputStyle, padding: '2px 4px', fontSize: '0.72rem', width: '110px' }}
                            >
                              <option value="">— untagged —</option>
                              {accounts.map(a => (
                                <option key={a.account_id} value={a.account_id}>{a.label}</option>
                              ))}
                            </select>
                          </td>
                          <td style={{ padding: '4px 6px', textAlign: 'right' }}>{b.n_lineups}</td>
                          <td style={{ padding: '4px 6px', textAlign: 'right', color: b.portfolio_ev > 0 ? 'var(--accent-green)' : 'var(--text-secondary)' }}>
                            {b.portfolio_ev != null ? `${b.portfolio_ev > 0 ? '+' : ''}${b.portfolio_ev}%` : '—'}
                          </td>
                          <td style={{ padding: '4px 6px', textAlign: 'right', whiteSpace: 'nowrap' }}>
                            <button onClick={() => patchBuildRow(b.build_id, { pinned: !b.pinned })} title={b.pinned ? 'Unpin' : 'Pin (kept on prune)'}
                              style={{ background: 'none', border: 'none', cursor: 'pointer', fontSize: '0.9rem', opacity: b.pinned ? 1 : 0.35 }}>★</button>
                            <button onClick={() => patchBuildRow(b.build_id, { submitted: !b.submitted })} title={b.submitted ? 'Unmark submitted' : 'Mark as submitted to a contest'}
                              style={{ background: 'none', border: 'none', cursor: 'pointer', fontSize: '0.85rem', opacity: b.submitted ? 1 : 0.35 }}>✓</button>
                            <button onClick={() => restoreBuildLineups(b.build_id)} title="Load this build's lineups into view (settings/overlay stay as they are)"
                              style={{ background: 'none', border: 'none', cursor: 'pointer', fontSize: '0.85rem' }}>⟲</button>
                            <button onClick={() => deleteBuildRows([b.build_id])} title="Delete this build"
                              style={{ background: 'none', border: 'none', cursor: 'pointer', fontSize: '0.8rem', opacity: 0.5 }}>🗑</button>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </>
            )}
          </div>
        )}
      </div>

      {/* ── Settings panel (collapsible) */}
      {settingsOpen && (
        <SettingsPanel
          settings={settings} setSettings={setSettings}
          allTeams={allTeams} allGames={allGames}
          excludedTeams={excludedTeams} setExcludedTeams={setExcludedTeams}
          excludedGames={excludedGames} setExcludedGames={setExcludedGames}
          dkContests={dkContests}
        />
      )}

      {/* ── Game Bar */}
      {mainSlateGames.length > 0 && (
        <div style={{ marginBottom: '14px' }}>
          <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', fontWeight: 600, textTransform: 'uppercase', letterSpacing: '0.08em', marginBottom: '6px' }}>
            Main Slate Games — Click to Exclude
          </div>
          <GameBar
            games={mainSlateGames}
            allSimResults={allSimResults}
            gameExclusions={gameExclusions}
            onToggleGame={toggleGame}
          />
        </div>
      )}

      {/* ── Main content area */}
      {view === 'pool' ? (
        /* ════ Player Pool Table ════ */
        <div style={cardStyle}>
          {/* Top bar */}
          <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '10px', flexWrap: 'wrap', gap: '10px' }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
              <h2 style={{ margin: 0, fontSize: '1rem' }}>Player Pool</h2>
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', background: 'rgba(255,255,255,0.05)', padding: '2px 8px', borderRadius: '20px' }}>
                {totalCount} players
              </span>
              {excludedCount > 0 && (
                <span style={{ fontSize: '0.75rem', color: '#ef4444', background: 'rgba(239,68,68,0.1)', padding: '2px 8px', borderRadius: '20px', border: '1px solid rgba(239,68,68,0.2)' }}>
                  {excludedCount} excluded
                </span>
              )}
              {/* Sticky-ownership control: freeze so the portfolio math stops
                  drifting between runs; hand-edit the few players that move. */}
              {overlay.ownershipFrozen ? (
                <span style={{ display: 'inline-flex', alignItems: 'center', gap: '6px', fontSize: '0.72rem', color: 'var(--accent-gold)', background: 'rgba(255,170,0,0.1)', padding: '2px 4px 2px 8px', borderRadius: '20px', border: '1px solid rgba(255,170,0,0.25)' }}>
                  Ownership FROZEN
                  {overlay.ownershipFrozenAt && (
                    <span style={{ color: 'var(--text-muted)' }}>
                      {new Date(overlay.ownershipFrozenAt).toLocaleDateString([], { weekday: 'short' })} {new Date(overlay.ownershipFrozenAt).toLocaleTimeString([], { hour: 'numeric', minute: '2-digit' })}
                    </span>
                  )}
                  {ownershipHandEdits > 0 && <span style={{ color: 'var(--text-muted)' }}>· {ownershipHandEdits} edit{ownershipHandEdits > 1 ? 's' : ''}</span>}
                  <button onClick={unfreezeOwnership} title="Revert to the live weekly ownership from the sim"
                    style={{ ...pillBtnBase, padding: '1px 7px', fontSize: '0.68rem', background: 'rgba(255,255,255,0.06)', color: 'var(--text-secondary)', borderColor: 'rgba(255,255,255,0.12)' }}>
                    Unfreeze
                  </button>
                </span>
              ) : (
                <button onClick={freezeOwnership}
                  title="Snapshot every player's current ownership for the week so the portfolio/EV math is stable across optimize runs. Hand-edit individuals as news breaks."
                  style={{ ...pillBtnBase, fontSize: '0.72rem', background: 'rgba(255,255,255,0.04)', color: 'var(--text-muted)', borderColor: 'rgba(255,255,255,0.08)', whiteSpace: 'nowrap' }}>
                  Freeze ownership
                </button>
              )}
            </div>
            <div style={{ display: 'flex', gap: '8px', alignItems: 'center' }}>
              {/* Position filter pills */}
              <div style={{ display: 'flex', gap: '4px' }}>
                {['ALL', 'QB', 'RB', 'WR', 'TE', 'DST'].map(pos => (
                  <button key={pos} onClick={() => setPosFilter(pos)} style={{
                    ...pillBtnBase,
                    background: posFilter === pos ? (POS_COLORS[pos] ? POS_COLORS[pos] + '22' : 'rgba(0,242,254,0.15)') : 'rgba(255,255,255,0.04)',
                    color: posFilter === pos ? (POS_COLORS[pos] || 'var(--accent-primary)') : 'var(--text-muted)',
                    borderColor: posFilter === pos ? (POS_COLORS[pos] || 'rgba(0,242,254,0.4)') + '88' : 'rgba(255,255,255,0.08)',
                  }}>{pos}</button>
                ))}
              </div>
              <button
                onClick={() => setShowGppCol(v => {
                  const next = !v;
                  if (!next && sortField === 'gppProjection') { setSortField('projection'); setSortAsc(false); }
                  return next;
                })}
                title="Show/hide the blended ceiling projection the optimizer uses for GPP contest types"
                style={{
                  ...pillBtnBase,
                  background: showGppCol ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.04)',
                  color: showGppCol ? 'var(--accent-primary)' : 'var(--text-muted)',
                  borderColor: showGppCol ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.08)',
                  whiteSpace: 'nowrap',
                }}
              >{showGppCol ? 'Hide' : 'Show'} GPP blend</button>
              <select
                value={teamGameFilter} onChange={e => setTeamGameFilter(e.target.value)}
                title="Show only one game or team (view filter -- doesn't exclude anyone from the optimizer)"
                style={{
                  ...inputStyle, padding: '5px 8px', fontSize: '0.78rem',
                  color: teamGameFilter !== 'ALL' ? 'var(--accent-primary)' : undefined,
                  borderColor: teamGameFilter !== 'ALL' ? 'rgba(0,242,254,0.4)' : undefined,
                }}
              >
                <option value="ALL">All games / teams</option>
                <optgroup label="Games">
                  {poolGameOptions.games.map(g => <option key={g} value={`G:${g}`}>{g.replace('@', ' @ ')}</option>)}
                </optgroup>
                <optgroup label="Teams">
                  {poolGameOptions.teams.map(t => <option key={t} value={`T:${t}`}>{t}</option>)}
                </optgroup>
              </select>
              <input
                type="text" placeholder="Search player…"
                value={playerSearch} onChange={e => setPlayerSearch(e.target.value)}
                style={{ ...inputStyle, width: '180px', padding: '5px 10px' }}
              />
            </div>
          </div>

          {labOpen && (
            <div style={{ ...cardStyle, padding: '10px', marginBottom: '10px' }}>
              <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginBottom: '6px' }}>
                Pick a full lineup and score it through the same <strong>{settings.contestType.replace(/_/g, ' ')}</strong>{' '}
                field sim the optimizer uses — EV%, ITM%, Top 1% / 0.1%, portfolio score.
                Uses the pool's current projections &amp; ownership (freeze them first to pin).
                {labOptions.length < 9 && <span style={{ color: '#f59e0b' }}> Load salaries for this slate first.</span>}
              </div>
              {labRows.map((row, ri) => {
                const setRow = patch => setLabRows(rs => rs.map((r, i) => (i === ri ? { ...r, ...patch } : r)));
                const setRb = (fi, v) => setRow({ rb: row.rb.map((x, i) => (i === fi ? v : x)) });
                const setWr = (fi, v) => setRow({ wr: row.wr.map((x, i) => (i === fi ? v : x)) });
                const chosen = new Set([row.qb, ...row.rb, ...row.wr, row.te, row.flex, row.dst].filter(Boolean));
                const sel = (val, onCh, placeholder, posKey) => (
                  <select value={val} onChange={e => onCh(e.target.value)}
                    style={{ ...inputStyle, minWidth: '128px', flex: '1 1 128px', fontSize: '0.74rem' }}>
                    <option value="">{placeholder}</option>
                    {labByPos[posKey].map(p => {
                      const k = optKey(p);
                      return <option key={k} value={k} disabled={k !== val && chosen.has(k)}>{optLabel(p)}</option>;
                    })}
                  </select>
                );
                return (
                  <div key={ri} style={{ display: 'flex', flexWrap: 'wrap', gap: '5px', alignItems: 'center', marginBottom: '6px' }}>
                    <input value={row.label} onChange={e => setRow({ label: e.target.value })} placeholder={`Lab ${ri + 1}`}
                      style={{ ...inputStyle, width: '80px', fontSize: '0.74rem' }} />
                    {sel(row.qb, v => setRow({ qb: v }), 'QB…', 'QB')}
                    {sel(row.rb[0], v => setRb(0, v), 'RB…', 'RB')}
                    {sel(row.rb[1], v => setRb(1, v), 'RB…', 'RB')}
                    {sel(row.wr[0], v => setWr(0, v), 'WR…', 'WR')}
                    {sel(row.wr[1], v => setWr(1, v), 'WR…', 'WR')}
                    {sel(row.wr[2], v => setWr(2, v), 'WR…', 'WR')}
                    {sel(row.te, v => setRow({ te: v }), 'TE…', 'TE')}
                    {sel(row.flex, v => setRow({ flex: v }), 'FLEX…', 'FLEX')}
                    {sel(row.dst, v => setRow({ dst: v }), 'DST…', 'DST')}
                    {labRows.length > 1 && (
                      <button onClick={() => setLabRows(rs => rs.filter((_, i) => i !== ri))}
                        style={{ padding: '4px 8px', borderRadius: '6px', border: '1px solid rgba(255,255,255,0.12)', background: 'rgba(255,255,255,0.04)', color: '#ef4444', cursor: 'pointer', fontSize: '0.78rem' }}>✕</button>
                    )}
                  </div>
                );
              })}
              <div style={{ display: 'flex', gap: '8px', alignItems: 'center', marginTop: '4px' }}>
                <button onClick={() => setLabRows(rs => [...rs, emptyLabRow()])}
                  style={{ padding: '6px 10px', borderRadius: '7px', border: '1px solid rgba(255,255,255,0.12)', background: 'rgba(255,255,255,0.04)', color: 'var(--text-white)', cursor: 'pointer', fontSize: '0.78rem' }}>
                  + add lineup
                </button>
                <button onClick={scoreLab} disabled={isOptimizing}
                  style={{ padding: '6px 16px', borderRadius: '7px', border: '1px solid rgba(0,242,254,0.3)', background: 'rgba(0,242,254,0.14)', color: 'var(--accent-primary)', fontWeight: 700, cursor: isOptimizing ? 'wait' : 'pointer', fontSize: '0.8rem' }}>
                  {isOptimizing ? 'Scoring…' : 'Score lineup(s) →'}
                </button>
                {labError && <span style={{ color: '#ef4444', fontSize: '0.78rem' }}>{labError}</span>}
              </div>
            </div>
          )}

          {/* Table */}
          <div className="table-container" style={{ maxHeight: '70vh', overflowY: 'auto' }}>
            <table style={{ fontSize: '0.8rem', width: '100%' }}>
              <thead>
                <tr>
                  <th style={{ width: '30px', textAlign: 'center', padding: '7px 4px', userSelect: 'none' }}>🔒</th>
                  <th style={{ width: '30px', textAlign: 'center', padding: '7px 4px', userSelect: 'none' }}>✕</th>
                  {[
                    ['name', 'Player'], ['pos', 'Pos'], ['team', 'Team'], ['game', 'Game'],
                    ['salary', 'Salary'], ['projection', 'Proj ✎'],
                  ].map(([field, label]) => (
                    <th key={field} style={{ cursor: 'pointer', padding: '7px 8px', whiteSpace: 'nowrap' }} onClick={() => handleSort(field)}>
                      {label} {sortField === field ? (sortAsc ? '↑' : '↓') : ''}
                    </th>
                  ))}
                  {showGppCol && (
                    <th style={{ cursor: 'pointer', padding: '7px 8px', whiteSpace: 'nowrap', color: 'rgba(0,242,254,0.7)', fontSize: '0.72rem' }}
                      onClick={() => handleSort('gppProjection')} title="Blended ceiling projection used by optimizer for GPP types">
                      GPP Proj {sortField === 'gppProjection' ? (sortAsc ? '↑' : '↓') : '⬍'}
                    </th>
                  )}
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P25</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P50</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P75</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P95</th>
                  <th style={{ cursor: 'pointer', padding: '7px 8px' }} onClick={() => handleSort('mean')}>
                    Mean {sortField === 'mean' ? (sortAsc ? '↑' : '↓') : ''}
                  </th>
                  <th style={{ padding: '7px 8px', minWidth: '68px', whiteSpace: 'nowrap' }}>Own%</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}
                    title="Sim's optimal-lineup rate — how often this player is the optimal play across sim iterations">Opt%</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}
                    title="Opt% minus Own% — positive means the sim likes this player more than the field will roster them (a leverage play)">Field Lev.</th>
                </tr>
              </thead>
              <tbody>
                {visiblePool.length === 0 ? (
                  <tr><td colSpan={showGppCol ? 16 : 15} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>No players match your filters.</td></tr>
                ) : visiblePool.map(p => {
                  // Don't dim the row being typed into just because a half-typed
                  // value dipped under the threshold (other exclusions still apply).
                  const excluded = editingProj?.id === p.id
                    ? isPlayerExcluded({ ...p, projection: Infinity })
                    : isPlayerExcluded(p);
                  const tint = getProjTint(p);
                  const isAboveP75 = p.hasPcts && (p.simP75 ?? p.p75) != null && p.projection > (p.simP75 ?? p.p75);
                  return (
                    <tr key={p.id} style={{
                      opacity: excluded ? 0.35 : 1,
                      background: p.locked ? 'rgba(234,179,8,0.06)' : 'transparent',
                      transition: 'opacity 0.15s',
                    }}>
                      {/* Lock */}
                      <td style={{ textAlign: 'center', padding: '5px 4px' }}>
                        <button onClick={() => toggleLock(p.id)} title={p.locked ? 'Unlock' : 'Lock'} style={{
                          background: p.locked ? 'rgba(234,179,8,0.2)' : 'rgba(255,255,255,0.04)',
                          border: `1px solid ${p.locked ? 'rgba(234,179,8,0.5)' : 'rgba(255,255,255,0.08)'}`,
                          color: p.locked ? '#eab308' : 'var(--text-muted)',
                          borderRadius: '4px', padding: '2px 5px', cursor: 'pointer', fontSize: '0.68rem',
                        }}>🔒</button>
                      </td>
                      {/* Exclude */}
                      <td style={{ textAlign: 'center', padding: '5px 4px' }}>
                        <button onClick={() => toggleExclude(p.id)} title={p.excluded ? 'Re-include' : 'Exclude'} style={{
                          background: excluded ? 'rgba(239,68,68,0.15)' : 'rgba(255,255,255,0.04)',
                          border: `1px solid ${excluded ? 'rgba(239,68,68,0.4)' : 'rgba(255,255,255,0.08)'}`,
                          color: excluded ? '#ef4444' : 'var(--text-muted)',
                          borderRadius: '4px', padding: '2px 5px', cursor: 'pointer', fontSize: '0.68rem',
                        }}>✕</button>
                      </td>
                      {/* Player Name — DST shows "TEAM DST" */}
                      <td style={{ fontWeight: 600, padding: '5px 8px', whiteSpace: 'nowrap' }}>
                        <span style={{ display: 'inline-block', width: '7px', height: '7px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '6px', verticalAlign: 'middle' }} />
                        {p.pos === 'DST' ? `${p.team} DST` : p.name}
                      </td>
                      {/* Pos badge */}
                      <td style={{ padding: '5px 6px' }}>
                        <span style={{
                          padding: '2px 5px', borderRadius: '4px', fontSize: '0.68rem', fontWeight: 700,
                          background: (POS_COLORS[p.pos] || '#888') + '22',
                          color: POS_COLORS[p.pos] || '#888',
                          border: `1px solid ${POS_COLORS[p.pos] || '#888'}44`,
                        }}>{p.pos}</span>
                      </td>
                      {/* Team */}
                      <td style={{ padding: '5px 6px', fontWeight: 600 }}>{p.team}</td>
                      {/* Game */}
                      <td style={{ padding: '5px 6px', fontSize: '0.75rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>{p.game}</td>
                      {/* Salary -- "—" for a player DK hasn't priced yet, not a guessed number */}
                      <td style={{ padding: '5px 6px', fontWeight: 600, color: p.salary == null ? 'var(--text-muted)' : undefined }}>
                        {p.salary == null ? '—' : `$${p.salary.toLocaleString()}`}
                      </td>
                      {/* Proj editable (median / platform projection) */}
                      <td style={{ padding: '3px 5px' }}>
                        <div title={isAboveP75 ? 'Above p75 — aggressive projection' : undefined} style={{ display: 'flex', alignItems: 'center', gap: '3px' }}>
                          <input
                            type="number" step={0.1}
                            value={p.projection}
                            onChange={e => setProjection(p.id, e.target.value)}
                            onFocus={() => setEditingProj({ id: p.id, projection: p.projection, gppProjection: p.gppProjection })}
                            onBlur={() => setEditingProj(null)}
                            onKeyDown={e => { if (e.key === 'Enter') e.currentTarget.blur(); }}
                            style={{
                              ...inputStyle, width: '60px', padding: '3px 5px', fontWeight: 700,
                              color: isAboveP75 ? '#ef4444' : 'var(--text-white)',
                              ...tint,
                            }}
                          />
                          {p.hasProjOverride && (
                            <button
                              onClick={() => _patchPlayer(p.id, { projAdjust: 0, projAbsolute: null })}
                              title={`Reset to sim projection (${p.simProjection ?? '—'})`}
                              style={{
                                border: 'none', cursor: 'pointer', borderRadius: '4px', padding: '2px 4px',
                                background: 'rgba(255,255,255,0.06)', color: 'var(--text-muted)', fontSize: '0.7rem',
                              }}>↺</button>
                          )}
                        </div>
                      </td>
                      {/* GPP Proj — blended ceiling projection (read-only display, click to use as proj) */}
                      {showGppCol && (
                        <td style={{ padding: '3px 5px' }}>
                          {p.hasPcts ? (
                            <button
                              onClick={() => setProjection(p.id, p.gppProjection)}
                              title={`Set projection to GPP blend: ${p.gppProjection}`}
                              style={{
                                ...pillBtnBase, padding: '2px 7px', fontSize: '0.72rem', fontWeight: 700,
                                background: 'rgba(0,242,254,0.08)',
                                color: 'rgba(0,242,254,0.85)',
                                borderColor: 'rgba(0,242,254,0.2)',
                              }}
                            >{p.gppProjection}</button>
                          ) : (
                            <span style={{ fontSize: '0.68rem', color: 'rgba(255,255,255,0.18)' }}>—</span>
                          )}
                        </td>
                      )}
                      {/* Percentile pills: P25, P50, P75, P95 */}
                      {['p25', 'p50', 'p75', 'p95'].map(pctKey => (
                        <td key={pctKey} style={{ padding: '3px 3px', textAlign: 'center' }}>
                          {p.hasPcts && p[pctKey] != null ? (
                            <button
                              onClick={() => setProjToPct(p.id, p[pctKey])}
                              title={`Set proj to ${pctKey.toUpperCase()}: ${p[pctKey]}`}
                              style={{
                                ...pillBtnBase, padding: '2px 5px', fontSize: '0.65rem',
                                background: 'rgba(0,242,254,0.07)',
                                color: 'var(--accent-primary)',
                                borderColor: 'rgba(0,242,254,0.15)',
                              }}
                            >{p[pctKey]}</button>
                          ) : (
                            <span style={{ fontSize: '0.68rem', color: 'rgba(255,255,255,0.18)' }}>—</span>
                          )}
                        </td>
                      ))}
                      {/* Mean */}
                      <td style={{ padding: '5px 6px', color: 'var(--accent-green)', fontSize: '0.8rem' }}>{p.mean}</td>
                      {/* Own% */}
                      <td style={{ padding: '3px 5px' }}>
                        <input
                          type="number" step={0.1} min={0} max={100}
                          value={p.ownershipPct ?? ''}
                          placeholder="—"
                          onChange={e => setOwnership(p.id, e.target.value)}
                          style={{ ...inputStyle, width: '58px', padding: '3px 5px', fontSize: '0.75rem' }}
                        />
                      </td>
                      {/* Opt% — sim's optimal-lineup rate */}
                      <td style={{ padding: '5px 5px', color: 'var(--text-muted)', fontSize: '0.75rem' }}>
                        {p.optimalPct != null ? `${p.optimalPct.toFixed(1)}%` : '—'}
                      </td>
                      {/* Field Leverage — Opt% minus Own% */}
                      <td style={{ padding: '5px 5px', fontSize: '0.75rem', fontWeight: 600 }}>
                        {p.optimalPct != null && p.ownershipPct != null ? (
                          <span style={{ color: (p.optimalPct - p.ownershipPct) > 0 ? 'var(--accent-green)' : '#ef4444' }}>
                            {(p.optimalPct - p.ownershipPct) > 0 ? '+' : ''}{(p.optimalPct - p.ownershipPct).toFixed(1)}%
                          </span>
                        ) : '—'}
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>

      ) : (
        /* ════ Results View ════ */
        <div style={{ display: 'flex', flexDirection: 'column', gap: '14px' }}>

          {/* Portfolio Summary Card */}
          {portfolioStats && (
            <div style={{ ...cardStyle, borderColor: 'rgba(0,242,254,0.15)' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '12px', flexWrap: 'wrap', gap: '10px' }}>
                <div>
                  <h2 style={{ margin: 0, fontSize: '1rem' }}>
                    {resultsMode === 'lab' ? '✍️ Lineup Lab' : 'Portfolio Summary'}
                  </h2>
                  <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginTop: '2px' }}>
                    {resultsMode === 'lab'
                      ? `${portfolioStats.n_generated} hand-built lineup${portfolioStats.n_generated === 1 ? '' : 's'} scored`
                      : `${portfolioStats.n_generated} lineups generated${portfolioStats.n_requested && portfolioStats.n_generated < portfolioStats.n_requested ? ` (${portfolioStats.n_requested} requested)` : ''}`}
                    {restoredBuild && (
                      <span style={{ marginLeft: '8px', color: 'var(--accent-primary)' }}>
                        · viewing a saved build{restoredBuild.label ? ` "${restoredBuild.label}"` : ''}
                      </span>
                    )}
                  </div>
                </div>
                <div style={{ display: 'flex', gap: '8px', flexWrap: 'wrap' }}>
                  {restoredBuild && (
                    <button onClick={restoreBuildSettings} title="Load this build's settings & projection adjustments as a checkpoint -- your current settings/overlay will be replaced" style={{
                      padding: '7px 14px', background: 'rgba(245,197,66,0.1)', color: '#f5c542',
                      border: '1px solid rgba(245,197,66,0.3)', borderRadius: '8px', cursor: 'pointer',
                      fontWeight: 600, fontSize: '0.82rem',
                    }}>↺ Restore Settings/Overlay</button>
                  )}
                  <button onClick={exportSummaryCSV} title="Human-readable summary -- not accepted by DK's upload form" style={{
                    padding: '7px 14px', background: 'rgba(255,255,255,0.04)', color: 'var(--text-muted)',
                    border: '1px solid rgba(255,255,255,0.1)', borderRadius: '8px', cursor: 'pointer',
                    fontWeight: 600, fontSize: '0.82rem',
                  }}>↓ Export Summary</button>
                  <button onClick={exportDkUploadCSV} title="DK's real Bulk Upload format (player IDs from the live salary feed)" style={{
                    padding: '7px 14px', background: 'rgba(0,242,254,0.1)', color: 'var(--accent-primary)',
                    border: '1px solid rgba(0,242,254,0.25)', borderRadius: '8px', cursor: 'pointer',
                    fontWeight: 600, fontSize: '0.82rem',
                  }}>↓ Export for DK Upload</button>
                </div>
              </div>
              <div style={{ display: 'grid', gridTemplateColumns: 'repeat(4, 1fr)', gap: '10px' }}>
                {[
                  { label: 'Portfolio EV%', value: `${portfolioStats.total_ev_pct > 0 ? '+' : ''}${portfolioStats.total_ev_pct}%`, color: evColor(portfolioStats.total_ev_pct) },
                  { label: 'Eff. Lineup Count', value: `${portfolioStats.effective_lineup_count}/${portfolioStats.n_generated}`, color: elcColor(portfolioStats.effective_lineup_count, portfolioStats.n_generated) },
                  { label: 'Avg Correlation', value: portfolioStats.avg_correlation?.toFixed(3), color: 'var(--text-white)' },
                  { label: 'Coverage Score', value: portfolioStats.coverage_score?.toFixed(2), color: 'var(--accent-primary)' },
                ].map(({ label, value, color }) => (
                  <div key={label} style={{ background: 'rgba(255,255,255,0.03)', borderRadius: '8px', padding: '10px', textAlign: 'center' }}>
                    <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', marginBottom: '4px', textTransform: 'uppercase', letterSpacing: '0.05em' }}>{label}</div>
                    <div style={{ fontSize: '1.3rem', fontWeight: 700, color }}>{value}</div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Grid Layout for Lineups and Portfolio Exposure */}
          <div className="optimizer-results-layout">
            {/* Lineups Table */}
            <div style={cardStyle}>
              <h2 style={{ margin: '0 0 10px 0', fontSize: '1rem' }}>
                Generated Lineups ({selectionAvg ? `${displayedLineups.length} of ${sortedLineups.length}` : sortedLineups.length})
              </h2>

              {/* Exposure drill-down: selected players + subset-vs-portfolio averages */}
              {selectionAvg && (
                <div style={{ background: 'rgba(0,242,254,0.04)', border: '1px solid rgba(0,242,254,0.18)', borderRadius: '8px', padding: '10px', marginBottom: '10px' }}>
                  <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '6px', marginBottom: '8px' }}>
                    <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>Lineups with</span>
                    {selectedExposurePlayers.length > 1 && (
                      <div style={{ display: 'flex', gap: '2px' }}>
                        {['all', 'any'].map(m => (
                          <button key={m} onClick={() => setExposureMatchMode(m)} style={{
                            ...pillBtnBase, padding: '1px 7px', fontSize: '0.68rem',
                            background: exposureMatchMode === m ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.03)',
                            color: exposureMatchMode === m ? 'var(--accent-primary)' : 'var(--text-muted)',
                            borderColor: exposureMatchMode === m ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.08)',
                          }}>{m.toUpperCase()} of</button>
                        ))}
                      </div>
                    )}
                    {selectedExposurePlayers.map(p => (
                      <span key={p.key} style={{ display: 'inline-flex', alignItems: 'center', gap: '5px', fontSize: '0.74rem', fontWeight: 600, padding: '2px 4px 2px 8px', borderRadius: '20px', background: (POS_COLORS[p.pos] || '#888') + '1f', border: `1px solid ${(POS_COLORS[p.pos] || '#888')}55`, color: 'var(--text-white)' }}>
                        {p.name}
                        <span style={{ color: 'var(--text-muted)', fontWeight: 500 }}>{p.exposure.toFixed(1)}%</span>
                        <button onClick={() => toggleExposureSelect(p.key)} title="Remove"
                          style={{ border: 'none', background: 'transparent', color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.72rem', padding: '0 3px' }}>✕</button>
                      </span>
                    ))}
                    <button onClick={() => setSelectedExposureKeys(new Set())}
                      style={{ ...pillBtnBase, marginLeft: 'auto', padding: '2px 9px', fontSize: '0.7rem', background: 'rgba(255,255,255,0.04)', color: 'var(--text-secondary)', borderColor: 'rgba(255,255,255,0.12)' }}>
                      Clear filter
                    </button>
                  </div>
                  {selectionAvg.n === 0 ? (
                    <div style={{ fontSize: '0.76rem', color: 'var(--text-muted)' }}>No lineups contain all of these players together.</div>
                  ) : (
                    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(92px, 1fr))', gap: '6px' }}>
                      {[
                        { label: 'Lineups', v: selectionAvg.n, base: portfolioAvg.n, fmt: v => `${v}`, baseFmt: v => `of ${v}` },
                        { label: 'Avg EV%', v: selectionAvg.ev_pct, base: portfolioAvg.ev_pct, fmt: v => `${v > 0 ? '+' : ''}${v.toFixed(2)}%`, color: evColor },
                        { label: 'Avg Port.Score', v: selectionAvg.portfolio_score, base: portfolioAvg.portfolio_score, fmt: v => v.toFixed(2) },
                        { label: 'Avg ITM%', v: selectionAvg.itm_pct, base: portfolioAvg.itm_pct, fmt: v => `${v.toFixed(2)}%` },
                        { label: 'Avg Top1%', v: selectionAvg.top1_pct, base: portfolioAvg.top1_pct, fmt: v => `${v.toFixed(2)}%` },
                        { label: 'Avg Top.1%', v: selectionAvg.top01_pct, base: portfolioAvg.top01_pct, fmt: v => `${v.toFixed(2)}%` },
                        { label: 'Avg P95', v: selectionAvg.p95, base: portfolioAvg.p95, fmt: v => v.toFixed(1) },
                        { label: 'Avg Salary', v: selectionAvg.salary, base: portfolioAvg.salary, fmt: v => `$${Math.round(v).toLocaleString()}` },
                      ].filter(s => s.v != null).map(s => (
                        <div key={s.label} style={{ background: 'rgba(255,255,255,0.03)', borderRadius: '6px', padding: '6px 8px', textAlign: 'center' }}>
                          <div style={{ fontSize: '0.62rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em' }}>{s.label}</div>
                          <div style={{ fontSize: '0.95rem', fontWeight: 700, color: s.color ? s.color(s.v) : 'var(--text-white)' }}>{s.fmt(s.v)}</div>
                          {s.base != null && (
                            <div style={{ fontSize: '0.62rem', color: 'var(--text-muted)' }} title="Whole-portfolio average">
                              {s.baseFmt ? s.baseFmt(s.base) : `port. ${s.fmt(s.base)}`}
                            </div>
                          )}
                        </div>
                      ))}
                    </div>
                  )}
                </div>
              )}
              <div className="table-container" style={{ maxHeight: '70vh', overflowY: 'auto' }}>
                <table style={{ fontSize: '0.78rem', width: '100%' }}>
                  <thead>
                    <tr>
                      <th style={{ padding: '7px 6px' }}>#</th>
                      {[['ev_pct','EV%'],['portfolio_score','Port.Score'],['itm_pct','ITM%'],['top1_pct','Top1%'],['top01_pct','Top.1%']].map(([f, l]) => (
                        <th key={f} style={{ padding: '7px 6px', cursor: 'pointer', whiteSpace: 'nowrap' }} onClick={() => handleResSort(f)}>
                          {l} {resSortField === f ? (resSortAsc ? '↑' : '↓') : ''}
                        </th>
                      ))}
                      {['QB','RB','RB','WR','WR','WR','FLEX','TE','DST'].map((slot, i) => (
                        <th key={`${slot}${i}`} style={{ padding: '7px 6px', fontSize: '0.68rem', color: POS_COLORS[slot] || 'var(--text-muted)' }}>{slot}</th>
                      ))}
                      <th style={{ padding: '7px 6px', cursor: 'pointer' }} onClick={() => handleResSort(sortedLineups[0]?.lineup_p95 !== undefined ? 'lineup_p95' : 'projected_score')}>
                        Score (P95) {(resSortField === 'lineup_p95' || resSortField === 'projected_score') ? (resSortAsc ? '↑' : '↓') : ''}
                      </th>
                      <th style={{ padding: '7px 6px', cursor: 'pointer', whiteSpace: 'nowrap' }} onClick={() => handleResSort('total_salary')}>
                        Salary {resSortField === 'total_salary' ? (resSortAsc ? '↑' : '↓') : ''}
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    {displayedLineups.length === 0 ? (
                      <tr><td colSpan={18} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>
                        {sortedLineups.length === 0 ? 'No lineups yet. Click ⚡ Optimize to generate.' : 'No lineups match the selected players.'}
                      </td></tr>
                    ) : displayedLineups.map((lu, idx) => {
                      const rowTint = getLineupRowTint(lu, sortedLineups);
                      const slotted = getSlottedColumns(lu.players);
                      const isExpanded = expandedLineupIdx === idx;
                      const remaining = salaryCap - lu.total_salary;
                      const hasLocked = lu.players.some(p => overlay.players[`${p.name}_${p.team}`]?.locked);

                      const renderSlot = (player) => {
                        if (!player) return <td style={{ padding: '5px 6px', color: 'rgba(255,255,255,0.2)' }}>—</td>;
                        const displayName = player.pos === 'DST' ? `${player.team} DST` : player.name.split(' ').slice(-1)[0];
                        const isSelected = selectedExposureKeys.has(lineupKey(player));
                        return (
                          <td style={{ padding: '5px 6px', whiteSpace: 'nowrap', ...(isSelected ? { boxShadow: 'inset 0 -2px 0 var(--accent-primary)' } : {}) }}>
                            <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[player.team] || '#888', marginRight: '4px', verticalAlign: 'middle' }} />
                            <span style={{ fontWeight: 600 }}>{displayName}</span>
                          </td>
                        );
                      };

                      return (
                        <Fragment key={idx}>
                          <tr onClick={() => setExpandedLineupIdx(isExpanded ? null : idx)}
                              style={{
                                ...rowTint,
                                cursor: 'pointer',
                                borderBottom: '1px solid rgba(255,255,255,0.04)',
                                transition: 'background-color 0.15s'
                              }}
                              className="lineup-row">
                            <td style={{ padding: '7px 6px', fontWeight: 'bold', color: 'var(--text-muted)', maxWidth: '110px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}
                              title={lu.label || undefined}>
                              {resultsMode === 'lab' && lu.label ? lu.label : idx + 1}
                            </td>
                            <td style={{ padding: '7px 6px', color: evColor(lu.ev_pct), fontWeight: 700 }}>{lu.ev_pct > 0 ? '+' : ''}{lu.ev_pct}%</td>
                            <td style={{ padding: '7px 6px', fontWeight: 600 }}>{lu.portfolio_score?.toFixed(2)}</td>
                            <td style={{ padding: '7px 6px', color: 'var(--text-main)' }}>{lu.itm_pct}%</td>
                            <td style={{ padding: '7px 6px', color: 'var(--accent-primary)', fontWeight: 600 }}>{lu.top1_pct}%</td>
                            <td style={{ padding: '7px 6px', color: 'var(--accent-gold)', fontWeight: 600 }}>{lu.top01_pct}%</td>
                            {slotted.map((p, pi) => renderSlot(p))}
                            <td style={{ padding: '7px 6px', fontWeight: 700, color: 'var(--accent-green)' }}>
                              {lu.lineup_p95 !== undefined ? lu.lineup_p95?.toFixed(1) : lu.projected_score?.toFixed(1)}
                            </td>
                            <td style={{ padding: '7px 6px', fontWeight: 600, color: remaining < 0 ? 'var(--accent-red)' : 'var(--text-main)' }}
                              title={lu.over_salary_cap ? `$${Math.abs(remaining).toLocaleString()} over the $${salaryCap.toLocaleString()} cap — DK would reject this lineup` : undefined}>
                              ${lu.total_salary?.toLocaleString()}{lu.over_salary_cap ? ' ⚠' : ''}
                            </td>
                          </tr>
                          {isExpanded && (
                            <tr>
                              <td colSpan={18} style={{ padding: '8px', background: 'rgba(0,0,0,0.2)' }}>
                                <div style={{ ...cardStyle, borderColor: 'rgba(0,242,254,0.1)', padding: '10px' }}>
                                  <div style={{ display: 'flex', gap: '20px', marginBottom: '10px', fontSize: '0.76rem', color: 'var(--text-muted)', borderBottom: '1px solid rgba(255,255,255,0.06)', paddingBottom: '8px' }}>
                                    <span>P50: <strong style={{ color: 'var(--text-white)' }}>{lu.lineup_p50?.toFixed(1) || '—'}</strong></span>
                                    <span>P75: <strong style={{ color: 'var(--text-white)' }}>{lu.lineup_p75?.toFixed(1) || '—'}</strong></span>
                                    <span>P95: <strong style={{ color: 'var(--accent-gold)', fontWeight: 800 }}>{lu.lineup_p95?.toFixed(1) || '—'}</strong></span>
                                    <span>Volatility (Std): <strong style={{ color: 'var(--accent-primary)' }}>{lu.lineup_std?.toFixed(1) || '—'}</strong></span>
                                    <span>Proj Median: <strong style={{ color: 'var(--accent-green)' }}>{lu.projected_score?.toFixed(1) || '—'}</strong></span>
                                    {lu.histogram && (
                                      <button onClick={e => { e.stopPropagation(); setHistLineup(lu); }}
                                        title="Show this lineup's range of outcomes across all sim runs"
                                        style={{ background: 'rgba(0,242,254,0.1)', border: '1px solid rgba(0,242,254,0.25)', borderRadius: '6px', color: 'var(--accent-primary)', cursor: 'pointer', fontSize: '0.72rem', padding: '2px 8px', fontWeight: 600, marginLeft: 'auto' }}>
                                        📊 Range of Outcomes
                                      </button>
                                    )}
                                  </div>
                                  {hasLocked && <div style={{ fontSize: '0.72rem', color: '#eab308', marginBottom: '8px' }}>🔒 Built with locked player(s)</div>}
                                  <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(170px, 1fr))', gap: '6px' }}>
                                    {sortLineupPlayers(lu.players).map((p, pi) => (
                                      <div key={pi} style={{
                                        background: 'rgba(255,255,255,0.03)', borderRadius: '8px', padding: '8px 10px',
                                        border: `1px solid ${(POS_COLORS[p.pos] || '#888')}22`,
                                      }}>
                                        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '4px' }}>
                                          <span style={{
                                            fontSize: '0.62rem', fontWeight: 700, padding: '1px 5px', borderRadius: '3px',
                                            background: (POS_COLORS[p.slot || p.pos] || '#888') + '22',
                                            color: POS_COLORS[p.slot || p.pos] || '#888',
                                          }}>{p.slot || p.pos}</span>
                                          <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)' }}>${(p.salary || 0).toLocaleString()}</span>
                                        </div>
                                        <div style={{ fontWeight: 700, fontSize: '0.82rem', color: 'var(--text-white)' }}>
                                          <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '5px', verticalAlign: 'middle' }} />
                                          {p.pos === 'DST' ? `${p.team} DST` : p.name}
                                        </div>
                                        <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.72rem', marginTop: '3px' }}>
                                          <span style={{ color: 'var(--accent-primary)' }}>Proj: <strong>{p.projection}</strong></span>
                                          {p.ownership_pct != null && <span style={{ color: 'var(--text-muted)' }}>Own: {p.ownership_pct}%</span>}
                                        </div>
                                      </div>
                                    ))}
                                  </div>
                                </div>
                              </td>
                            </tr>
                          )}
                        </Fragment>
                      );
                  })}
                </tbody>
              </table>
            </div>
            </div>

            {/* Portfolio Exposure */}
            <div style={{ ...cardStyle, display: 'flex', flexDirection: 'column', gap: '10px' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <h2 style={{ margin: 0, fontSize: '1rem' }}>Portfolio Exposure</h2>
                <span style={{ fontSize: '0.75rem', color: 'var(--accent-primary)', fontWeight: 600 }}>
                  {playerExposures.length} Players
                </span>
              </div>
              
              {/* Search & Pos Filter Controls */}
              <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                <input
                  type="text"
                  placeholder="Search portfolio..."
                  value={exposureSearch}
                  onChange={e => setExposureSearch(e.target.value)}
                  style={inputStyle}
                />
                
                <div style={{ display: 'flex', gap: '4px', overflowX: 'auto', paddingBottom: '4px' }}>
                  {['ALL', 'QB', 'RB', 'WR', 'TE', 'DST'].map(pos => {
                    const active = exposurePosFilter === pos;
                    return (
                      <button
                        key={pos}
                        onClick={() => setExposurePosFilter(pos)}
                        style={{
                          ...pillBtnBase,
                          background: active ? 'rgba(0, 242, 254, 0.15)' : 'rgba(255,255,255,0.03)',
                          color: active ? 'var(--accent-primary)' : 'var(--text-muted)',
                          borderColor: active ? 'var(--accent-primary)' : 'rgba(255,255,255,0.08)',
                          padding: '2px 6px',
                          fontSize: '0.7rem'
                        }}
                      >
                        {pos}
                      </button>
                    );
                  })}
                </div>
              </div>

              {/* Sort Buttons */}
              <div style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.7rem', color: 'var(--text-muted)' }}>
                <span>Sort:</span>
                {[
                  { field: 'exposure', label: 'Exposure' },
                  { field: 'leverage', label: 'Leverage' },
                  { field: 'salary', label: 'Salary' },
                  { field: 'projection', label: 'Proj' }
                ].map(({ field, label }) => {
                  const active = exposureSortField === field;
                  return (
                    <button
                      key={field}
                      onClick={() => {
                        if (exposureSortField === field) {
                          setExposureSortAsc(!exposureSortAsc);
                        } else {
                          setExposureSortField(field);
                          setExposureSortAsc(field === 'name'); // default asc for name, desc for others
                        }
                      }}
                      style={{
                        background: active ? 'rgba(255, 255, 255, 0.08)' : 'transparent',
                        border: 'none',
                        color: active ? 'var(--text-white)' : 'var(--text-muted)',
                        cursor: 'pointer',
                        padding: '2px 6px',
                        borderRadius: '4px',
                        fontSize: '0.7rem',
                        fontWeight: active ? 700 : 500
                      }}
                    >
                      {label} {active ? (exposureSortAsc ? '↑' : '↓') : ''}
                    </button>
                  );
                })}
              </div>

              {/* List of exposures */}
              <div style={{ maxHeight: '60vh', overflowY: 'auto', display: 'flex', flexDirection: 'column', gap: '8px', paddingRight: '4px' }}>
                {filteredExposures.length === 0 ? (
                  <div style={{ textAlign: 'center', color: 'var(--text-muted)', padding: '20px 0', fontSize: '0.8rem' }}>
                    No players match filters
                  </div>
                ) : (
                  filteredExposures.map(p => {
                    const teamColor = TEAM_COLORS[p.team] || '#888';
                    const posColor = POS_COLORS[p.pos] || '#888';
                    const isPositiveLev = p.leverage > 0;
                    const levText = p.ownership > 0 ? `${isPositiveLev ? '+' : ''}${p.leverage.toFixed(1)}%` : '—';
                    const isSelected = selectedExposureKeys.has(p.key);
                    const baseBg = isSelected ? 'rgba(0,242,254,0.08)' : 'rgba(255,255,255,0.02)';

                    return (
                      <div
                        key={p.key}
                        onClick={() => toggleExposureSelect(p.key)}
                        title={isSelected ? 'Click to remove from the lineup filter' : 'Click to show only lineups with this player (click more to combine)'}
                        style={{
                          background: baseBg,
                          border: `1px solid ${isSelected ? 'rgba(0,242,254,0.45)' : 'rgba(255,255,255,0.04)'}`,
                          borderRadius: '8px',
                          padding: '8px 10px',
                          display: 'flex',
                          flexDirection: 'column',
                          gap: '6px',
                          cursor: 'pointer',
                          transition: 'background 0.2s',
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = isSelected ? 'rgba(0,242,254,0.12)' : 'rgba(255,255,255,0.04)'}
                        onMouseLeave={e => e.currentTarget.style.background = baseBg}
                      >
                        {/* Player Info Row */}
                        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                          <div style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                            <span style={{
                              fontSize: '0.62rem',
                              fontWeight: 700,
                              padding: '1px 4px',
                              borderRadius: '3px',
                              background: `${posColor}15`,
                              color: posColor
                            }}>{p.pos}</span>
                            <span style={{
                              display: 'inline-block',
                              width: '6px',
                              height: '6px',
                              borderRadius: '50%',
                              background: teamColor
                            }} />
                            <span style={{ fontWeight: 700, fontSize: '0.82rem', color: 'var(--text-white)' }}>
                              {p.name}
                            </span>
                            <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>
                              {p.team}
                            </span>
                          </div>
                          <div style={{ fontSize: '0.75rem', fontWeight: 600, color: 'var(--text-muted)' }}>
                            ${p.salary.toLocaleString()}
                          </div>
                        </div>

                        {/* Stats Sub-row */}
                        <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.7rem', color: 'var(--text-muted)' }}>
                          <span>Proj: <strong style={{ color: 'var(--accent-primary)' }}>{p.projection}</strong></span>
                          {p.ownership > 0 ? (
                            <>
                              <span>Field Own: <strong>{p.ownership}%</strong></span>
                              <span>Leverage: <strong style={{ color: isPositiveLev ? 'var(--accent-green)' : 'var(--accent-red)' }}>{levText}</strong></span>
                            </>
                          ) : (
                            <span>No ownership data</span>
                          )}
                        </div>

                        {/* Progress Bar Container */}
                        <div style={{ position: 'relative', height: '16px', background: 'rgba(255,255,255,0.04)', borderRadius: '4px', overflow: 'hidden' }}>
                          {/* Exposure Bar */}
                          <div style={{
                            position: 'absolute',
                            left: 0,
                            top: 0,
                            bottom: 0,
                            width: `${p.exposure}%`,
                            background: `linear-gradient(90deg, ${posColor}88, ${posColor})`,
                            borderRadius: '4px',
                            transition: 'width 0.4s ease'
                          }} />
                          
                          {/* Ownership Tick Marker */}
                          {p.ownership > 0 && p.ownership <= 100 && (
                            <div
                              title={`Field Ownership: ${p.ownership}%`}
                              style={{
                                position: 'absolute',
                                left: `${p.ownership}%`,
                                top: 0,
                                bottom: 0,
                                width: '2px',
                                backgroundColor: 'rgba(255,255,255,0.7)',
                                boxShadow: '0 0 4px #fff',
                                zIndex: 2
                              }}
                            />
                          )}
                          
                          {/* Percentage text overlay */}
                          <div style={{
                            position: 'absolute',
                            top: 0,
                            left: 0,
                            right: 0,
                            bottom: 0,
                            display: 'flex',
                            alignItems: 'center',
                            justifyContent: 'center',
                            fontSize: '0.72rem',
                            fontWeight: 700,
                            color: 'var(--text-white)',
                            zIndex: 3,
                            textShadow: '0 1px 2px rgba(0,0,0,0.8)'
                          }}>
                            {p.exposure.toFixed(1)}% ({p.count}/{optimizerLineups.length})
                          </div>
                        </div>
                      </div>
                    );
                  })
                )}
              </div>
            </div>
          </div>
        </div>
      )}
      {histLineup && <LineupHistogramModal lineup={histLineup} onClose={() => setHistLineup(null)} />}
    </div>
  );
}
