import { useState, useMemo, useEffect, useCallback, useRef } from 'react';
import { ApiService } from '../api';
import { fmtSpreadNum } from '../bettingLines';
import SlotSwitcher from '../components/SlotSwitcher';
import ClassicLineupAnalysis from '../components/ClassicLineupAnalysis';
import {
  TEAM_COLORS, POS_COLORS, cardStyle, inputStyle, labelStyle, sectionTitleStyle, pillBtnBase,
  buildPlayerPool, exportSummaryCSV, exportDkUploadCSV, slimLineup,
} from '../optimizerShared';
import { useWorkspaceSlots } from '../hooks/useWorkspaceSlots';

// ─── GPP Projection Weights by Contest Type ───────────────────────────────────
// Weights for the 'blend' Solver Objective (see OBJECTIVE_MODES below), the
// default for GPP types. Cash is pure median; GPP types blend in P75/P95
// proportionally to prize skew.
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

// ─── Solver objective ────────────────────────────────────────────────────────
// What per-player score the ILP maximises, independent of contest type
// (contest type still drives the payout/EV math and the draw width). Sent to
// the backend as `gpp_projection`; 'median' sends null so the backend falls
// back to `projection` (the shifted P50).
//   blend   — contest-type ceiling blend (GPP_WEIGHTS_BY_TYPE above)
//   mean    — sim mean
//   median  — sim P50
//   ceiling — pure 95th percentile
// Mirrored in ShowdownOptimizer.jsx's OBJECTIVE_MODES.
const OBJECTIVE_MODES = [
  ['blend',   'GPP blend'],
  ['mean',    'Mean'],
  ['median',  'Median (P50)'],
  ['ceiling', '95% ceiling'],
];

/** Default objective for a contest type -- what the dropdown resets to when
 *  the contest type changes, and what an older saved workspace (no
 *  `objective` field) resolves to. Input: contestType (str). Output: mode (str). */
const naturalObjective = (contestType) => (contestType === 'cash' ? 'median' : 'blend');

/** Solver-objective points for one enriched pool player.
 *  Inputs: p (enrichedPool row -- percentiles already shifted by any manual
 *  projAdjust, plus `meanProj`), mode (OBJECTIVE_MODES value), weights (the
 *  contest type's GPP_WEIGHTS_BY_TYPE entry, used by 'blend').
 *  Output: number (points). Players with no sim distribution (custom adds)
 *  fall back to their flat projection under every mode. */
function computeObjectiveProj(p, mode, weights) {
  if (!p.hasPcts) return parseFloat((p.projection || 0).toFixed(1));
  if (mode === 'median')  return p.projection;
  if (mode === 'mean')    return p.meanProj ?? p.projection;
  if (mode === 'ceiling') return p.p95 ?? p.projection;
  return computeGppProj(p, weights);
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

const MAX_EXPOSURE_KEYS = [
  ['maxExposureQB', 'QB', POS_COLORS.QB],
  ['maxExposureRB', 'RB', POS_COLORS.RB],
  ['maxExposureWR', 'WR', POS_COLORS.WR],
  ['maxExposureTE', 'TE', POS_COLORS.TE],
  ['maxExposureDST', 'DST', POS_COLORS.DST],
];

/** Per-position cap on how often any ONE player at that position can appear
 * across the generated set -- passed to /api/optimize as max_exposure_by_pos.
 * Replaces the old single global Max Exposure % slider: a chalky QB pool
 * can be capped tighter than a deep WR pool without also choking off
 * legitimate RB/WR rotation. Each slider is independent (10-100%, same
 * range/step the old global slider used). */
function MaxExposureSliders({ settings, setSettings }) {
  return (
    <div style={{ marginBottom: '4px' }}>
      <label style={labelStyle} title="Cap on how often any one player at this position can appear across the generated lineups.">
        Max Exposure by Position
      </label>
      {MAX_EXPOSURE_KEYS.map(([key, label, color]) => (
        <div key={key} style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '4px' }}>
          <span style={{ width: '30px', fontSize: '0.74rem', fontWeight: 700, color }}>{label}</span>
          <input type="range" min={10} max={100} step={5}
            value={settings[key] ?? 40}
            onChange={e => setSettings(s => ({ ...s, [key]: parseInt(e.target.value) }))}
            style={{ flex: 1 }}
          />
          <span style={{ fontSize: '0.78rem', fontWeight: 700, color: 'var(--accent-primary)', minWidth: '34px', textAlign: 'right' }}>
            {settings[key] ?? 40}%
          </span>
        </div>
      ))}
    </div>
  );
}

const FLEX_MIX_KEYS = [
  ['flexRB', 'RB', POS_COLORS.RB],
  ['flexWR', 'WR', POS_COLORS.WR],
  ['flexTE', 'TE', POS_COLORS.TE],
  ['flexAny', 'Any', 'var(--text-muted)'],
];

/** Approximate target mix for which position fills FLEX across the generated
 * set -- passed to /api/optimize as flex_position_weights. Raw slider values
 * (0-100 each, independent) are normalized server-side, so they don't need
 * to sum to 100; the % shown next to each slider is that normalized preview,
 * computed the same way, so what's displayed always matches what gets sent. */
function FlexMixSliders({ settings, setSettings }) {
  const total = FLEX_MIX_KEYS.reduce((sum, [key]) => sum + (settings[key] || 0), 0);
  return (
    <div style={{ marginTop: '10px' }}>
      <label style={labelStyle} title="Approximate share of generated lineups where FLEX is filled by each position. Normalized automatically -- doesn't need to add to 100.">
        FLEX Position Mix
      </label>
      {FLEX_MIX_KEYS.map(([key, label, color]) => {
        const pct = total > 0 ? Math.round(100 * (settings[key] || 0) / total) : 0;
        return (
          <div key={key} style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '4px' }}>
            <span style={{ width: '30px', fontSize: '0.74rem', fontWeight: 700, color }}>{label}</span>
            <input type="range" min={0} max={100} step={5}
              value={settings[key] || 0}
              onChange={e => setSettings(s => ({ ...s, [key]: parseInt(e.target.value) }))}
              style={{ flex: 1 }}
            />
            <span style={{ fontSize: '0.78rem', fontWeight: 700, color: 'var(--accent-primary)', minWidth: '34px', textAlign: 'right' }}>
              {pct}%
            </span>
          </div>
        );
      })}
    </div>
  );
}

function SettingsPanel({ settings, setSettings, allTeams, allGames, excludedTeams, setExcludedTeams, excludedGames, setExcludedGames, dkContests = [] }) {
  const [advancedOpen, setAdvancedOpen] = useState(false);
  const objective = settings.objective ?? naturalObjective(settings.contestType);
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
                    // Changing contest type resets the objective to that
                    // type's natural default, so a Median left over from a
                    // cash build can't silently ride into a GPP build.
                    onChange={() => setSettings(s => ({ ...s, contestType: opt.value, objective: naturalObjective(opt.value) }))}
                    style={{ accentColor: 'var(--accent-primary)', cursor: 'pointer' }}
                  />
                  {opt.label}
                </label>
              ))}
            </div>

            <label style={labelStyle}
              title="What the solver maximises per player. Separate from contest type, which still drives payouts/EV and lineup variety. Resets to the contest type's default when you change contest type.">
              Solver Objective
            </label>
            <select style={{ ...inputStyle, marginBottom: '3px' }} value={objective}
              onChange={e => setSettings(s => ({ ...s, objective: e.target.value }))}>
              {OBJECTIVE_MODES.map(([v, l]) => (
                <option key={v} value={v}>{l}{v === naturalObjective(settings.contestType) ? ' (default)' : ''}</option>
              ))}
            </select>
            <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', marginBottom: '10px' }}>
              {objective === 'blend' && (settings.contestType === 'cash'
                ? 'Cash blend weights are pure P50 -- same as Median.'
                : 'Ceiling blend weighted by contest type.')}
              {objective === 'mean' && 'Sim mean -- sits above the median for right-skewed players.'}
              {objective === 'median' && 'Sim P50 -- no ceiling tilt.'}
              {objective === 'ceiling' && 'Pure 95th percentile -- maximum boom.'}
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

        <MaxExposureSliders settings={settings} setSettings={setSettings} />

        <FlexMixSliders settings={settings} setSettings={setSettings} />
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

  // Teams actually on the currently selected slate (Main Slate today) --
  // used to keep buildPlayerPool from listing players whose game can never
  // be rostered here (see that function's comment).
  const slateTeams = useMemo(() => {
    const s = new Set();
    (games || []).forEach(g => {
      if (g.dk_main) { s.add(g.away_team); s.add(g.home_team); }
    });
    return s;
  }, [games]);

  // ── Player pool = PURE sim output. Never mutated by user actions. Rebuilt
  // whenever the week's projections / sims / games change. All of the user's
  // manual work lives in `overlay` below and is re-applied on top in
  // `enrichedPool`, so a mid-week sim refresh no longer wipes it.
  const [playerPool, setPlayerPool] = useState(() =>
    buildPlayerPool(weekProjections, allSimResults, games, slateTeams)
  );

  useEffect(() => {
    const wpList = Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || []);
    if (wpList.length > 0 || Object.keys(allSimResults || {}).length > 0) {
      setPlayerPool(buildPlayerPool(weekProjections, allSimResults, games, slateTeams));
    }
  }, [weekProjections, allSimResults, games, slateTeams]);

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
    includeDstUnique: false,
    projThreshold: 0,
    // Approximate target mix for which position fills FLEX across the
    // generated set (see /api/optimize's flex_position_weights). Default is
    // ANY: 100 / everything else 0, i.e. fully unconstrained -- identical to
    // pre-slider behavior (the solver just picks FLEX on score) for anyone
    // who never touches these.
    flexRB: 0, flexWR: 0, flexTE: 0, flexAny: 100,
    // Per-position exposure caps (see /api/optimize's max_exposure_by_pos),
    // replacing the old single global Max Exposure % slider. 40 matches that
    // slider's old default, so a fresh session behaves the same until these
    // are touched.
    maxExposureQB: 40, maxExposureRB: 40, maxExposureWR: 40, maxExposureTE: 40, maxExposureDST: 40,
  };
  // Merged (not `||`) with whatever's already loaded/saved, so a settings
  // object saved before a new field existed (e.g. the flex-mix sliders, or
  // these per-position exposure caps) still gets that field's default
  // instead of silently missing it -- a bare `||` picks the *entire* saved
  // object as-is, permanently masking any default added after it was saved.
  // (Concretely: this was the FLEX Position Mix sliders showing only 0%/100%
  // for anyone with pre-existing saved state -- the missing `flexAny: 100`
  // dropped out of the mix's denominator entirely, so whichever slider you'd
  // just touched became the *entire* total, always reading back 100%.)
  const [settings, setSettings] = useState(() => ({ ...defaultSettings, ...(optimizerSettings || {}) }));

  // ── Filters
  const [playerSearch, setPlayerSearch] = useState('');
  const [posFilter, setPosFilter] = useState('ALL');
  // Team/game filter for the pool table: 'ALL' | 'G:<AWAY@HOME>' | 'T:<TEAM>'.
  // View-only -- does not exclude anyone from the optimizer.
  const [teamGameFilter, setTeamGameFilter] = useState('ALL');
  // Projection cell currently being typed into: { id, projection, objProjection }
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

  // ── Optimizer run state
  const [isOptimizing, setIsOptimizing] = useState(false);
  const [optimizeProgress, setOptimizeProgress] = useState('');
  const [portfolioStats, setPortfolioStats] = useState(null);

  // ── Portfolio Exposure state
  // Exposure drill-down: keys (`name_team_pos`, same as playerExposures' key)
  // of players clicked in the exposure panel. Non-empty -> the lineups table
  // shows only lineups containing ALL (or ANY, per exposureMatchMode) of them.

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
  // Solver objective (see OBJECTIVE_MODES). Saved workspaces from before the
  // dropdown existed have no `objective`, so they resolve to the contest
  // type's natural default -- i.e. exactly how they optimized before.
  const objective = settings.objective ?? naturalObjective(settings.contestType);
  const objectiveLabel = OBJECTIVE_MODES.find(([v]) => v === objective)?.[1] ?? 'GPP blend';

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
      // Mean on the active platform's (shifted) scale. The pool's `mean` is
      // the DK sim mean; FD has no mean column, so FD (or any row missing a
      // mean) uses the average of the 101-point percentile curve instead --
      // a close approximation of the mean of the underlying sims.
      const curveMean = activePcts && activePcts.length
        ? activePcts.reduce((a, v) => a + v, 0) / activePcts.length : null;
      const rawMean = (!isFD && enriched.mean != null) ? enriched.mean : curveMean;
      enriched.meanProj = rawMean != null ? parseFloat(rawMean.toFixed(1)) : null;
      enriched.gppProjection = computeGppProj(enriched, contestWeights);
      enriched.objProjection = computeObjectiveProj(enriched, objective, contestWeights);
      return enriched;
    });
  }, [playerPool, overlay, settings.platform, contestWeights, objective]);

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
    // 'ptsPerK' is derived (projection per $1K salary) -- see ptsPerK below.
    const sortVal = (p, field) => {
      if (field === 'ptsPerK') {
        const proj = sortVal(p, 'projection');
        return p.salary > 0 && proj != null ? proj / (p.salary / 1000) : null;
      }
      return p.id === editId && field in editingProj ? editingProj[field] : p[field];
    };
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
        const field = sortField;
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
    return enrichedPool.filter(p => p.salary != null).map(p => ({
      name: p.name, team: p.team, pos: p.pos,
      salary: p.salary,
      projection: p.p50 ?? p.projection ?? p.simProjection,   // shifted P50 for evaluation
      // Solver objective for the ILP only (see OBJECTIVE_MODES); null for
      // 'median' so the backend falls back to `projection` above.
      gpp_projection: objective === 'median' ? null : (p.objProjection ?? null),
      locked: p.locked,
      excluded: isPlayerExcluded(p),
      ownership_pct: p.ownershipPct,
      dk_pcts_all: p.active_pcts_all || p.dk_pcts_all || null,
      dk_id: p.dk_id ?? null,
      dk_name: p.dk_name ?? null,
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
    //   gpp_projection = the Solver Objective value (GPP blend by default for GPP
    //                    types; mean / 95% ceiling on request) — used ONLY by the
    //                    ILP objective. null for 'median' (backend uses projection).
    //                    Also computed off the shifted percentiles.
    const payload = {
      players: buildOptimizerPlayersPayload(),
      n_lineups: settings.nLineups,
      salary_cap: salaryCap,
      contest_type: settings.contestType,
      contest_size: settings.contestSize,
      min_unique_players: settings.minUnique,
      include_dst_in_unique: settings.includeDstUnique,
      // Per-position caps (Settings → Max Exposure by Position) -- replaces
      // the old single global max_exposure scalar; the backend still has a
      // default for that field, but the classic optimizer always sends this.
      max_exposure_by_pos: {
        QB: (settings.maxExposureQB ?? 40) / 100,
        RB: (settings.maxExposureRB ?? 40) / 100,
        WR: (settings.maxExposureWR ?? 40) / 100,
        TE: (settings.maxExposureTE ?? 40) / 100,
        DST: (settings.maxExposureDST ?? 40) / 100,
      },
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
      // Approximate FLEX-position target mix (Settings → FLEX Position Mix).
      // Raw slider values; the backend normalizes them itself.
      flex_position_weights: {
        RB: settings.flexRB || 0, WR: settings.flexWR || 0,
        TE: settings.flexTE || 0, ANY: settings.flexAny || 0,
      },
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
        dk_id: pl.dk_id, dk_name: pl.dk_name,
      })),
    }));
    lineups.forEach(lu => lu.players.forEach(pl => {
      const id = `${pl.name}_${pl.team}`;
      if (players_used[id]) return;
      const p = poolById[id];
      players_used[id] = p ? {
        name: p.name, team: p.team, pos: p.pos, salary: p.salary,
        proj: p.projection, gpp_proj: p.gppProjection,
        obj_proj: p.objProjection, objective,
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

  // Send lineups to this week's Shortlist (data/optimizer/.../shortlist_classic.json).
  // Duplicates (same players + slots) are skipped server-side.
  const saveToShortlist = async (lus) => {
    if (!lus || !lus.length) return;
    const res = await ApiService.addToShortlist(selectedWeek, lus.map(slimLineup), 'classic');
    if (!res) { setBuildToast('Could not save to shortlist (backend unreachable)'); return; }
    setBuildToast(`Shortlist: +${res.added} saved${res.duplicates ? `, ${res.duplicates} already there` : ''} (${res.lineups.length} total)`);
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
          ⚠️ Running on baseline projections — run simulations on the Game Explorer page for enhanced percentile data.
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
          <button onClick={pruneBuildsNow} title="Delete this week's autosave builds that aren't pinned, labeled, submitted or tagged to a bankroll account"
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
                  if (!next && sortField === 'objProjection') { setSortField('projection'); setSortAsc(false); }
                  return next;
                })}
                title="Show/hide the per-player value the solver maximises (set by Solver Objective in Advanced Settings)"
                style={{
                  ...pillBtnBase,
                  background: showGppCol ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.04)',
                  color: showGppCol ? 'var(--accent-primary)' : 'var(--text-muted)',
                  borderColor: showGppCol ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.08)',
                  whiteSpace: 'nowrap',
                }}
              >{showGppCol ? 'Hide' : 'Show'} solver proj</button>
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
                  <th style={{ cursor: 'pointer', padding: '7px 8px', whiteSpace: 'nowrap' }} onClick={() => handleSort('ptsPerK')}
                    title="Projection (the editable Proj column) per $1,000 of salary">
                    Pts/$1K {sortField === 'ptsPerK' ? (sortAsc ? '↑' : '↓') : ''}
                  </th>
                  {showGppCol && (
                    <th style={{ cursor: 'pointer', padding: '7px 8px', whiteSpace: 'nowrap', color: 'rgba(0,242,254,0.7)', fontSize: '0.72rem' }}
                      onClick={() => handleSort('objProjection')} title={`What the solver maximises per player: ${objectiveLabel}`}>
                      {objectiveLabel} {sortField === 'objProjection' ? (sortAsc ? '↑' : '↓') : '⬍'}
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
                  <tr><td colSpan={showGppCol ? 17 : 16} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>No players match your filters.</td></tr>
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
                            onFocus={() => setEditingProj({ id: p.id, projection: p.projection, objProjection: p.objProjection })}
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
                      {/* Pts per $1K -- live off the (possibly edited) projection */}
                      <td style={{ padding: '5px 6px', fontWeight: 600, color: 'var(--text-main)' }}>
                        {p.salary > 0 && p.projection != null ? (p.projection / (p.salary / 1000)).toFixed(2) : '—'}
                      </td>
                      {/* Solver proj — the Solver Objective value (read-only display, click to use as proj) */}
                      {showGppCol && (
                        <td style={{ padding: '3px 5px' }}>
                          {p.hasPcts ? (
                            <button
                              onClick={() => setProjection(p.id, p.objProjection)}
                              title={`Set projection to ${objectiveLabel}: ${p.objProjection}`}
                              style={{
                                ...pillBtnBase, padding: '2px 7px', fontSize: '0.72rem', fontWeight: 700,
                                background: 'rgba(0,242,254,0.08)',
                                color: 'rgba(0,242,254,0.85)',
                                borderColor: 'rgba(0,242,254,0.2)',
                              }}
                            >{p.objProjection}</button>
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

          <ClassicLineupAnalysis
            lineups={optimizerLineups}
            pool={enrichedPool}
            portfolioStats={portfolioStats}
            salaryCap={salaryCap}
            resultsMode={resultsMode}
            autoExpandFirst={resultsMode === 'lab'}
            rowActionHeader="Short"
            rowAction={(lu) => (
              <button onClick={() => saveToShortlist([lu])} title="Save this lineup to the Shortlist" style={{
                border: '1px solid rgba(34,197,94,0.3)', background: 'rgba(34,197,94,0.08)', color: '#22c55e',
                borderRadius: '6px', cursor: 'pointer', fontSize: '0.7rem', fontWeight: 700, padding: '2px 7px',
              }}>＋</button>
            )}
            isLocked={(p) => !!overlay.players[`${p.name}_${p.team}`]?.locked}
            subtitleExtra={restoredBuild && (
              <span style={{ marginLeft: '8px', color: 'var(--accent-primary)' }}>
                · viewing a saved build{restoredBuild.label ? ` "${restoredBuild.label}"` : ''}
              </span>
            )}
            headerActions={({ sortedLineups, displayedLineups }) => (
              <>
                {restoredBuild && (
                  <button onClick={restoreBuildSettings} title="Load this build's settings & projection adjustments as a checkpoint -- your current settings/overlay will be replaced" style={{
                    padding: '7px 14px', background: 'rgba(245,197,66,0.1)', color: '#f5c542',
                    border: '1px solid rgba(245,197,66,0.3)', borderRadius: '8px', cursor: 'pointer',
                    fontWeight: 600, fontSize: '0.82rem',
                  }}>↺ Restore Settings/Overlay</button>
                )}
                <button onClick={() => saveToShortlist(displayedLineups)} title="Save every lineup currently shown (respects the exposure filter) to this week's Shortlist" style={{
                  padding: '7px 14px', background: 'rgba(34,197,94,0.1)', color: '#22c55e',
                  border: '1px solid rgba(34,197,94,0.3)', borderRadius: '8px', cursor: 'pointer',
                  fontWeight: 600, fontSize: '0.82rem',
                }}>＋ Save {displayedLineups.length === sortedLineups.length ? 'all' : `${displayedLineups.length} shown`} to Shortlist</button>
                <button onClick={() => exportSummaryCSV(sortedLineups)} title="Human-readable summary -- not accepted by DK's upload form" style={{
                  padding: '7px 14px', background: 'rgba(255,255,255,0.04)', color: 'var(--text-muted)',
                  border: '1px solid rgba(255,255,255,0.1)', borderRadius: '8px', cursor: 'pointer',
                  fontWeight: 600, fontSize: '0.82rem',
                }}>↓ Export Summary</button>
                <button onClick={() => exportDkUploadCSV(sortedLineups)} title="DK's real Bulk Upload format (player IDs from the live salary feed)" style={{
                  padding: '7px 14px', background: 'rgba(0,242,254,0.1)', color: 'var(--accent-primary)',
                  border: '1px solid rgba(0,242,254,0.25)', borderRadius: '8px', cursor: 'pointer',
                  fontWeight: 600, fontSize: '0.82rem',
                }}>↓ Export for DK Upload</button>
              </>
            )}
          />
        </div>
      )}
    </div>
  );
}
