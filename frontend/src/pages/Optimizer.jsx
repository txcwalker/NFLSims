import { useState, useMemo, useEffect, useCallback, Fragment } from 'react';
import { ApiService } from '../api';
import { ALL_ROSTERS } from '../allRosters';

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

  // Helper to standardize game labels as AWAY@HOME
  const getStandardGameLabel = (team, opponent) => {
    let label = gameMap[team];
    if (!label && opponent) {
      const oppClean = opponent.replace('@', '').replace('vs ', '');
      label = opponent.startsWith('@') ? `${team}@${oppClean}` : `${oppClean}@${team}`;
    }
    return label || team;
  };

  // ── Tier 1: allSimResults (session simulations, custom user projections & stats)
  Object.values(allSimResults || {}).forEach(res => {
    if (!res?.projections) return;
    res.projections.forEach(p => {
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
        salary: p.salary || 5000,
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
        game: gameLabel,
        dk_pcts_all: pcts,
      });
    });
  });

  // ── Tier 2: weekProjections from /api/week_projections (baseline projections)
  const wpList = Array.isArray(weekProjections)
    ? weekProjections
    : (weekProjections?.players || []);

  wpList.forEach(p => {
    if (!p.name || !p.pos) return;
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
      salary: p.salary || 5000,
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
      game: gameLabel,
      dk_pcts_all: pcts,
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
        const vegasSpread = g.spread_line;
        
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
              minWidth: '160px',
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
              Veg: <span style={{ color: 'var(--text-white)', fontWeight: 600 }}>{vegasSpread > 0 ? '+' : ''}{vegasSpread}</span> · <span style={{ color: 'var(--text-white)', fontWeight: 600 }}>{vegasTotal} O/U</span>
            </div>
            {/* Sim Stats row */}
            <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>
              Sim: {simTotal ? (
                <>
                  <span style={{ color: 'var(--accent-primary)', fontWeight: 600 }}>{simSpread > 0 ? '+' : ''}{simSpread.toFixed(1)}</span> · <span style={{ color: 'var(--accent-primary)', fontWeight: 600 }}>{simTotal} O/U</span>
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
function SettingsPanel({ settings, setSettings, allTeams, allGames, excludedTeams, setExcludedTeams, excludedGames, setExcludedGames }) {
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

        <label style={labelStyle}>Platform</label>
        <div style={{ display: 'flex', gap: '6px', marginBottom: '10px' }}>
          {['DK', 'FD'].map(plat => (
            <button key={plat}
              onClick={() => setSettings(s => ({ ...s, platform: plat }))}
              style={{
                ...pillBtnBase, flex: 1,
                background: settings.platform === plat ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.04)',
                color: settings.platform === plat ? 'var(--accent-primary)' : 'var(--text-white)',
                borderColor: settings.platform === plat ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.1)',
                padding: '6px 10px', cursor: 'pointer',
              }}
            >{plat}</button>
          ))}
        </div>

        <label style={labelStyle}>Contest Type</label>
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
      </div>

      {/* ── Contest Numbers ── */}
      <div>
        <div style={sectionTitleStyle}>Contest Numbers</div>
        {[
          { key: 'contestSize', label: 'Contest Size', type: 'int' },
          { key: 'entryFee', label: 'Entry Fee ($)', type: 'float' },
          { key: 'payingPositions', label: 'Payout Positions', type: 'int' },
          { key: 'totalEntries', label: 'Total Entries', type: 'int' },
        ].map(({ key, label, type }) => (
          <div key={key} style={{ marginBottom: '8px' }}>
            <label style={labelStyle}>{label}</label>
            <input type="number" style={inputStyle}
              value={settings[key]}
              onChange={e => setSettings(s => ({ ...s, [key]: type === 'float' ? parseFloat(e.target.value) || 0 : parseInt(e.target.value) || 0 }))}
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
  weeks,
  selectedWeek,
  setSelectedWeek,
  optimizerLineups,
  setOptimizerLineups,
  optimizerSettings,
  setOptimizerSettings,
  setCurrentPage,
}) {
  const [view, setView] = useState('pool');
  const [settingsOpen, setSettingsOpen] = useState(false);

  // ── Build player pool
  const [playerPool, setPlayerPool] = useState(() =>
    buildPlayerPool(weekProjections, allSimResults, games)
  );

  // ── Re-init player pool when week projections, simulation results, or games update (e.g. week change or sim runs)
  useEffect(() => {
    const wpList = Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || []);
    if (wpList.length > 0 || Object.keys(allSimResults || {}).length > 0) {
      setPlayerPool(buildPlayerPool(weekProjections, allSimResults, games));
    }
  }, [weekProjections, allSimResults, games]);

  const hasSimData = (Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || [])).length > 0
    || Object.keys(allSimResults || {}).length > 0;

  // ── Settings
  const defaultSettings = {
    platform: 'DK', contestType: 'top_heavy',
    contestSize: 11000, entryFee: 18,
    payingPositions: 2200, totalEntries: 11000,
    nLineups: 20, minUnique: 2,
    includeDstUnique: false, maxExposure: 40,
    projThreshold: 0,
  };
  const [settings, setSettings] = useState(() => optimizerSettings || defaultSettings);

  // ── Filters
  const [playerSearch, setPlayerSearch] = useState('');
  const [posFilter, setPosFilter] = useState('ALL');
  const [excludedTeams, setExcludedTeams] = useState(new Set());
  const [excludedGames, setExcludedGames] = useState(new Set());
  const [gameExclusions, setGameExclusions] = useState({});

  // ── Sorting
  const [sortField, setSortField] = useState('projection');
  const [sortAsc, setSortAsc] = useState(false);
  const [resSortField, setResSortField] = useState('ev_pct');
  const [resSortAsc, setResSortAsc] = useState(false);

  // ── Optimizer run state
  const [isOptimizing, setIsOptimizing] = useState(false);
  const [optimizeProgress, setOptimizeProgress] = useState('');
  const [portfolioStats, setPortfolioStats] = useState(null);
  const [expandedLineupIdx, setExpandedLineupIdx] = useState(null);

  // ── Portfolio Exposure state
  const [exposureSearch, setExposureSearch] = useState('');
  const [exposurePosFilter, setExposurePosFilter] = useState('ALL');
  const [exposureSortField, setExposureSortField] = useState('exposure');
  const [exposureSortAsc, setExposureSortAsc] = useState(false);

  // ── Sync settings to parent
  useEffect(() => { setOptimizerSettings(settings); }, [settings]);

  // ── GPP projection weights from contest type
  const contestWeights = useMemo(
    () => GPP_WEIGHTS_BY_TYPE[settings.contestType] || GPP_WEIGHTS_BY_TYPE.top_heavy,
    [settings.contestType]
  );

  // ── Salary cap by platform
  const salaryCap = settings.platform === 'FD' ? 60000 : 50000;

  // ── Platform-adjusted + GPP-enriched pool
  // When FD is selected, swap DK percentiles for FD percentiles so the ILP
  // objective and all displayed columns reflect FD scoring.
  const enrichedPool = useMemo(() => {
    return playerPool.map(p => {
      const isFD = settings.platform === 'FD';
      // Active percentile values — switch to FD if needed
      const ap25 = isFD ? (p.fd_p25 ?? p.p25) : p.p25;
      const ap50 = isFD ? (p.fd_p50 ?? p.p50) : p.p50;
      const ap75 = isFD ? (p.fd_p75 ?? p.p75) : p.p75;
      const ap95 = isFD ? (p.fd_p95 ?? p.p95) : p.p95;
      const activePcts = isFD ? (p.fd_pcts_all ?? p.dk_pcts_all) : p.dk_pcts_all;
      const baseProj  = isFD ? (ap50 ?? p.projection) : p.projection;
      const enriched  = {
        ...p,
        projection:   parseFloat((baseProj ?? 0).toFixed(1)),
        p25: ap25, p50: ap50, p75: ap75, p95: ap95,
        hasPcts: !!(ap50 != null),
        active_pcts_all: activePcts,
      };
      enriched.gppProjection = computeGppProj(enriched, contestWeights);
      return enriched;
    });
  }, [playerPool, settings.platform, contestWeights]);

  // ── Derived lists
  const allTeams = useMemo(() => [...new Set(enrichedPool.map(p => p.team))].sort(), [enrichedPool]);
  const allGames = useMemo(() => [...new Set(enrichedPool.map(p => p.game).filter(Boolean))].sort(), [enrichedPool]);

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

  const visiblePool = useMemo(() => {
    const threshold = settings.projThreshold || 0;
    return enrichedPool
      .filter(p => {
        if (posFilter !== 'ALL' && p.pos !== posFilter) return false;
        if (playerSearch && !p.name.toLowerCase().includes(playerSearch.toLowerCase())) return false;
        if (threshold > 0 && p.projection < threshold) return false;
        return true;
      })
      .sort((a, b) => {
        const field = sortField === 'gppProjection' ? 'gppProjection' : sortField;
        let va = a[field] ?? 0;
        let vb = b[field] ?? 0;
        if (typeof va === 'string') return sortAsc ? va.localeCompare(vb) : vb.localeCompare(va);
        return sortAsc ? va - vb : vb - va;
      });
  }, [enrichedPool, posFilter, playerSearch, settings.projThreshold, sortField, sortAsc]);

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

  // ── Pool mutations
  const toggleLock    = (id) => setPlayerPool(prev => prev.map(p => p.id === id ? { ...p, locked: !p.locked } : p));
  const toggleExclude = (id) => setPlayerPool(prev => prev.map(p => p.id === id ? { ...p, excluded: !p.excluded } : p));
  const setProjection = (id, val) => setPlayerPool(prev => prev.map(p => p.id === id ? { ...p, projection: parseFloat(val) || 0 } : p));
  const setOwnership  = (id, val) => setPlayerPool(prev => prev.map(p => p.id === id ? { ...p, ownershipPct: parseFloat(val) || null } : p));
  const setProjToPct  = (id, pctVal) => setPlayerPool(prev => prev.map(p => p.id === id ? { ...p, projection: pctVal } : p));

  // ── Game bar toggle (4-state click cycle: 0 -> 1 (away) -> 2 (home) -> 3 (both) -> 0)
  const toggleGame = (gameLabel) => {
    setGameExclusions(prev => {
      const current = prev[gameLabel] || 0;
      const next = (current + 1) % 4;
      return {
        ...prev,
        [gameLabel]: next
      };
    });
  };

  // ── Projection tint
  const getProjTint = (p) => {
    if (!p.hasPcts || p.p75 == null || p.p50 == null) return {};
    const val = p.projection;
    if (val > p.p75) return { background: 'rgba(239,68,68,0.3)' };
    if (val > p.p50 * 1.15) return { background: 'rgba(245,158,11,0.25)' };
    if (val > p.p50 * 1.05) return { background: 'rgba(245,197,66,0.18)' };
    return {};
  };

  // ── Optimize
  const handleOptimize = async () => {
    setIsOptimizing(true);
    setOptimizeProgress('');
    if (settings.nLineups > 50) setOptimizeProgress(`Generating lineup 0 of ${settings.nLineups}…`);

    // Two separate projection values go to the backend:
    //   projection     = P50/median — used by the field simulation & EV evaluation.
    //                    Both our lineup AND the field are scored on this scale so
    //                    comparisons are apples-to-apples. This fixes the -98% EV bug
    //                    where sending gppProjection here inflated the field cutoff.
    //   gpp_projection = blended ceiling value — used ONLY by the ILP objective to
    //                    steer the solver toward higher-ceiling players. null for cash.
    const isCash = settings.contestType === 'cash';
    const payload = {
      players: enrichedPool.map(p => ({
        name: p.name, team: p.team, pos: p.pos,
        salary: p.salary,
        projection: p.p50 ?? p.simProjection ?? p.projection,   // always P50 for evaluation
        gpp_projection: isCash ? null : (p.gppProjection ?? null), // blended for ILP only
        locked: p.locked,
        excluded: isPlayerExcluded(p),
        ownership_pct: p.ownershipPct,
        dk_pcts_all: p.active_pcts_all || p.dk_pcts_all || null,
      })),
      n_lineups: settings.nLineups,
      salary_cap: salaryCap,
      contest_type: settings.contestType,
      contest_size: settings.contestSize,
      min_unique_players: settings.minUnique,
      include_dst_in_unique: settings.includeDstUnique,
      max_exposure: settings.maxExposure / 100,
      entry_fee: settings.entryFee,
      total_entries: settings.totalEntries,
      paying_positions: settings.payingPositions,
    };

    try {
      const result = await ApiService.optimizeLineups(payload);
      setOptimizerLineups(result.lineups || []);
      setPortfolioStats(result.portfolio || null);
      setView('results');
    } catch (err) {
      console.error('Optimizer error:', err);
    } finally {
      setIsOptimizing(false);
      setOptimizeProgress('');
    }
  };

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
    playerPool.forEach(p => {
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
  }, [optimizerLineups, playerPool]);

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

  const exportCSV = () => {
    const header = 'QB,RB,RB,WR,WR,WR,FLEX,TE,DST,Salary,Projected Score';
    const rows = sortedLineups.map(lu => {
      const cols = getSlottedColumns(lu.players);
      const names = cols.map(p => p ? p.name.split(' ').slice(-1)[0] : '');
      return [...names, lu.total_salary, lu.projected_score].join(',');
    });
    const blob = new Blob([[header, ...rows].join('\n')], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url; a.download = 'optimizer_lineups.csv'; a.click();
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
        <div style={{ display: 'flex', alignItems: 'center', gap: '15px' }}>
          <div style={{ display: 'flex', alignItems: 'center' }}>
            <span style={{fontWeight: 700, color: 'var(--text-white)', marginRight: '10px'}}>Slate Focus:</span>
            <div className="slate-selector">
              <button className="slate-btn active">Traditional Multi-Game</button>
              <button className="slate-btn" disabled style={{ opacity: 0.5, cursor: 'not-allowed' }} title="Showdown Single-Game Optimizer coming soon">Showdown Single-Game ✧</button>
            </div>
          </div>
        </div>

        {/* Week selection */}
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <span style={{fontWeight: 600, color: 'var(--text-white)', marginRight: '10px'}}>Week:</span>
          <select 
            value={selectedWeek} 
            onChange={(e) => {
              if (setSelectedWeek) setSelectedWeek(parseInt(e.target.value));
            }}
            style={{
              background: 'rgba(11, 17, 38, 0.6)',
              border: '1px solid var(--border-glass)',
              color: 'var(--text-white)',
              borderRadius: '8px',
              padding: '6px 12px',
              fontWeight: 600,
              outline: 'none',
              cursor: 'pointer'
            }}
          >
            {(weeks && weeks.length > 0 ? weeks : Array.from({length: 18}, (_, i) => i + 1)).map(w => (
              <option key={w} value={w}>{w}</option>
            ))}
          </select>
        </div>
      </div>

      {/* ── Page Header */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: '12px', flexWrap: 'wrap', gap: '10px' }}>
        <div>
          <h1 style={{ marginBottom: '2px', fontSize: '1.6rem' }}>⚡ DFS Optimizer</h1>
          <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', margin: 0 }}>
            Multi-Game Slate · DraftKings Traditional
          </p>
        </div>
        <div style={{ display: 'flex', gap: '8px', alignItems: 'center', flexWrap: 'wrap' }}>
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

      {/* ── Settings panel (collapsible) */}
      {settingsOpen && (
        <SettingsPanel
          settings={settings} setSettings={setSettings}
          allTeams={allTeams} allGames={allGames}
          excludedTeams={excludedTeams} setExcludedTeams={setExcludedTeams}
          excludedGames={excludedGames} setExcludedGames={setExcludedGames}
        />
      )}

      {/* ── Game Bar */}
      {games && games.length > 0 && (
        <div style={{ marginBottom: '14px' }}>
          <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', fontWeight: 600, textTransform: 'uppercase', letterSpacing: '0.08em', marginBottom: '6px' }}>
            This Week's Games — Click to Exclude
          </div>
          <GameBar
            games={games}
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
              <input
                type="text" placeholder="Search player…"
                value={playerSearch} onChange={e => setPlayerSearch(e.target.value)}
                style={{ ...inputStyle, width: '180px', padding: '5px 10px' }}
              />
            </div>
          </div>

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
                  <th style={{ cursor: 'pointer', padding: '7px 8px', whiteSpace: 'nowrap', color: 'rgba(0,242,254,0.7)', fontSize: '0.72rem' }}
                    onClick={() => handleSort('gppProjection')} title="Blended ceiling projection used by optimizer for GPP types">
                    GPP Proj {sortField === 'gppProjection' ? (sortAsc ? '↑' : '↓') : '⬍'}
                  </th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P25</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P50</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P75</th>
                  <th style={{ padding: '7px 5px', fontSize: '0.68rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>P95</th>
                  <th style={{ cursor: 'pointer', padding: '7px 8px' }} onClick={() => handleSort('mean')}>
                    Mean {sortField === 'mean' ? (sortAsc ? '↑' : '↓') : ''}
                  </th>
                  <th style={{ padding: '7px 8px', minWidth: '55px' }}>Own%</th>
                </tr>
              </thead>
              <tbody>
                {visiblePool.length === 0 ? (
                  <tr><td colSpan={14} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>No players match your filters.</td></tr>
                ) : visiblePool.map(p => {
                  const excluded = isPlayerExcluded(p);
                  const tint = getProjTint(p);
                  const isAboveP75 = p.hasPcts && p.p75 != null && p.projection > p.p75;
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
                      {/* Salary */}
                      <td style={{ padding: '5px 6px', fontWeight: 600 }}>${(p.salary || 0).toLocaleString()}</td>
                      {/* Proj editable (median / platform projection) */}
                      <td style={{ padding: '3px 5px' }}>
                        <div title={isAboveP75 ? 'Above p75 — aggressive projection' : undefined}>
                          <input
                            type="number" step={0.1}
                            value={p.projection}
                            onChange={e => setProjection(p.id, e.target.value)}
                            style={{
                              ...inputStyle, width: '60px', padding: '3px 5px', fontWeight: 700,
                              color: isAboveP75 ? '#ef4444' : 'var(--text-white)',
                              ...tint,
                            }}
                          />
                        </div>
                      </td>
                      {/* GPP Proj — blended ceiling projection (read-only display, click to use as proj) */}
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
                          style={{ ...inputStyle, width: '52px', padding: '3px 5px', fontSize: '0.75rem' }}
                        />
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
                  <h2 style={{ margin: 0, fontSize: '1rem' }}>Portfolio Summary</h2>
                  <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginTop: '2px' }}>
                    {portfolioStats.n_generated} lineups generated
                    {portfolioStats.n_requested && portfolioStats.n_generated < portfolioStats.n_requested
                      ? ` (${portfolioStats.n_requested} requested)` : ''}
                  </div>
                </div>
                <button onClick={exportCSV} style={{
                  padding: '7px 14px', background: 'rgba(0,242,254,0.1)', color: 'var(--accent-primary)',
                  border: '1px solid rgba(0,242,254,0.25)', borderRadius: '8px', cursor: 'pointer',
                  fontWeight: 600, fontSize: '0.82rem',
                }}>↓ Export CSV</button>
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
              <h2 style={{ margin: '0 0 10px 0', fontSize: '1rem' }}>Generated Lineups ({sortedLineups.length})</h2>
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
                    {sortedLineups.length === 0 ? (
                      <tr><td colSpan={18} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>
                        No lineups yet. Click ⚡ Optimize to generate.
                      </td></tr>
                    ) : sortedLineups.map((lu, idx) => {
                      const rowTint = getLineupRowTint(lu, sortedLineups);
                      const slotted = getSlottedColumns(lu.players);
                      const isExpanded = expandedLineupIdx === idx;
                      const remaining = salaryCap - lu.total_salary;
                      const hasLocked = lu.players.some(p => playerPool.find(pp => pp.name === p.name)?.locked);

                      const renderSlot = (player) => {
                        if (!player) return <td style={{ padding: '5px 6px', color: 'rgba(255,255,255,0.2)' }}>—</td>;
                        const displayName = player.pos === 'DST' ? `${player.team} DST` : player.name.split(' ').slice(-1)[0];
                        return (
                          <td style={{ padding: '5px 6px', whiteSpace: 'nowrap' }}>
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
                            <td style={{ padding: '7px 6px', fontWeight: 'bold', color: 'var(--text-muted)' }}>{idx + 1}</td>
                            <td style={{ padding: '7px 6px', color: evColor(lu.ev_pct), fontWeight: 700 }}>{lu.ev_pct > 0 ? '+' : ''}{lu.ev_pct}%</td>
                            <td style={{ padding: '7px 6px', fontWeight: 600 }}>{lu.portfolio_score?.toFixed(2)}</td>
                            <td style={{ padding: '7px 6px', color: 'var(--text-main)' }}>{lu.itm_pct}%</td>
                            <td style={{ padding: '7px 6px', color: 'var(--accent-primary)', fontWeight: 600 }}>{lu.top1_pct}%</td>
                            <td style={{ padding: '7px 6px', color: 'var(--accent-gold)', fontWeight: 600 }}>{lu.top01_pct}%</td>
                            {slotted.map((p, pi) => renderSlot(p))}
                            <td style={{ padding: '7px 6px', fontWeight: 700, color: 'var(--accent-green)' }}>
                              {lu.lineup_p95 !== undefined ? lu.lineup_p95?.toFixed(1) : lu.projected_score?.toFixed(1)}
                            </td>
                            <td style={{ padding: '7px 6px', fontWeight: 600, color: remaining < 0 ? 'var(--accent-red)' : 'var(--text-main)' }}>
                              ${lu.total_salary?.toLocaleString()}
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
                    
                    return (
                      <div
                        key={`${p.name}_${p.pos}`}
                        style={{
                          background: 'rgba(255,255,255,0.02)',
                          border: '1px solid rgba(255,255,255,0.04)',
                          borderRadius: '8px',
                          padding: '8px 10px',
                          display: 'flex',
                          flexDirection: 'column',
                          gap: '6px',
                          transition: 'background 0.2s',
                        }}
                        onMouseEnter={e => e.currentTarget.style.background = 'rgba(255,255,255,0.04)'}
                        onMouseLeave={e => e.currentTarget.style.background = 'rgba(255,255,255,0.02)'}
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
    </div>
  );
}
