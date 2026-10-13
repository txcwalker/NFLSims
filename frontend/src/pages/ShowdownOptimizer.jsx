import { useState, useMemo, useEffect, useRef, Fragment } from 'react';
import { ApiService } from '../api';
import GameDistribution from '../components/GameDistribution';
import LineupHistogramModal from '../components/LineupHistogramModal';
import SlotSwitcher from '../components/SlotSwitcher';
import { useWorkspaceSlots } from '../hooks/useWorkspaceSlots';

// ─── Shared visual language (matches Optimizer.jsx) ───────────────────────────
const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB:  '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC:  '#E31837',
  LV:  '#888888', LAC: '#0080C6', LAR: '#003594', MIA: '#008E97',
  MIN: '#4F2683', NE:  '#002244', NO:  '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SF:  '#AA0000',
  SEA: '#002244', TB:  '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414',
};
const POS_COLORS = { QB: '#ef4444', RB: '#22c55e', WR: '#3b82f6', TE: '#a855f7', DST: '#f97316' };

const cardStyle = {
  background: 'rgba(255,255,255,0.04)',
  border: '1px solid rgba(255,255,255,0.08)',
  borderRadius: '12px',
  padding: '16px',
};
const inputStyle = {
  background: 'rgba(255,255,255,0.06)', border: '1px solid rgba(255,255,255,0.1)',
  color: 'white', borderRadius: '6px', padding: '5px 8px', width: '100%',
  fontSize: '0.85rem', outline: 'none',
};
const labelStyle = {
  fontSize: '0.72rem', fontWeight: 600, color: 'var(--text-muted)', marginBottom: '3px',
  display: 'block', textTransform: 'uppercase', letterSpacing: '0.05em',
};
const sectionTitleStyle = {
  fontSize: '0.68rem', fontWeight: 700, color: 'var(--accent-primary)', textTransform: 'uppercase',
  letterSpacing: '0.1em', marginBottom: '8px', paddingBottom: '5px',
  borderBottom: '1px solid rgba(255,255,255,0.06)',
};

const round1 = (v) => Math.round((v || 0) * 10) / 10;
const CONTEST_TYPES = [
  ['cash', 'Cash / 50-50'],
  ['flat', 'Flat GPP'],
  ['top_heavy', 'Top-Heavy GPP'],
  ['extreme_top_heavy', 'Extreme Top-Heavy'],
];

// ─── Solver objective ────────────────────────────────────────────────────────
// What per-player score the ILP maximises, sent to the backend as
// `gpp_projection` (it falls back to the raw median when that's null).
//   blend   — contest-type ceiling blend (mirrors Optimizer.jsx GPP_WEIGHTS_BY_TYPE)
//   mean    — sim mean (average of the 101-point percentile curve)
//   median  — sim P50 (null → backend uses `projection`)
//   ceiling — pure 95th percentile
// Changing contest type resets it to naturalObjective() (Cash → median,
// GPP → blend), same rule as Optimizer.jsx.
const GPP_WEIGHTS_BY_TYPE = {
  cash:              { p25: 0,    p50: 1.00, p75: 0,    p95: 0    },
  flat:              { p25: 0.10, p50: 0.30, p75: 0.40, p95: 0.20 },
  top_heavy:         { p25: 0.05, p50: 0.20, p75: 0.35, p95: 0.40 },
  extreme_top_heavy: { p25: 0,    p50: 0.10, p75: 0.25, p95: 0.65 },
};
const OBJECTIVE_MODES = [
  ['blend',   'GPP blend'],
  ['mean',    'Mean'],
  ['median',  'Median (P50)'],
  ['ceiling', '95% ceiling'],
];

/** Default objective for a contest type (Input: contestType str; Output: mode str). */
const naturalObjective = (contestType) => (contestType === 'cash' ? 'median' : 'blend');

/** The solver-objective points for one pool player under the chosen mode.
 *  Manual proj bumps (p.projection vs p.simProjection) shift the whole
 *  distribution additively, same model as the classic optimizer. Returns
 *  null for 'median' so the payload omits gpp_projection. */
function objectiveProjection(p, mode, contestType) {
  if (mode === 'median') return null;
  const pcts = p.dk_pcts_all && p.dk_pcts_all.length === 101 ? p.dk_pcts_all : null;
  const delta = (p.projection ?? 0) - (p.simProjection ?? p.projection ?? 0);
  if (mode === 'ceiling') {
    const base = p.ceiling ?? (pcts ? pcts[95] : p.projection);
    return round1(Math.max(0, (base ?? 0) + delta));
  }
  if (!pcts) return round1(p.projection);        // kickers etc. — no distribution
  if (mode === 'mean') {
    // The 101-point curve follows a Game-Read scenario, so the mean does too.
    const mean = pcts.reduce((a, v) => a + v, 0) / pcts.length;
    return round1(Math.max(0, mean + delta));
  }
  const w = GPP_WEIGHTS_BY_TYPE[contestType] || GPP_WEIGHTS_BY_TYPE.top_heavy;
  const blend = (w.p25 || 0) * pcts[25] + (w.p50 || 0) * pcts[50]
              + (w.p75 || 0) * pcts[75] + (w.p95 || 0) * pcts[95];
  return round1(Math.max(0, blend + delta));
}

/** A player's manual projection bump, as a delta over the sim baseline.
 *  Inputs: `base` (pool row carrying the UNCONDITIONED simProjection),
 *  `e` (that player's entry in `edits`, or undefined).
 *  Output: number (points) -- 0 when there's no bump.
 *  Stored as a delta (`projDelta`) so a bump survives a Game-Read scenario
 *  swapping the baseline underneath it (+2 stays +2 over whatever the
 *  scenario's P50 is). Older saved slots stored an absolute `projection`;
 *  that's converted against the unconditioned base it was typed against. */
function projDeltaOf(base, e) {
  if (!e) return 0;
  if (e.projDelta != null) return e.projDelta;
  if (e.projection != null) return e.projection - (base.simProjection ?? e.projection);
  return 0;
}

const DK_TEAM_ALIASES ={ LAR: 'LA', JAC: 'JAX', LVR: 'LV', WSH: 'WAS' };
const normTeam = (t) => DK_TEAM_ALIASES[t] || t;

/** Mirror of dk_scraper.normalize_player_name: case/punct/suffix-insensitive. */
const SUFFIXES = new Set(['jr', 'sr', 'ii', 'iii', 'iv', 'v']);
function normName(name) {
  let n = (name || '').replace(/\./g, '').replace(/\s+/g, ' ').trim().toLowerCase();
  const parts = n.split(' ');
  if (parts.length > 1 && SUFFIXES.has(parts[parts.length - 1])) parts.pop();
  return parts.join(' ');
}

// A DK Showdown salary estimate for players the sim only has a Classic
// (main-slate) price for, or no price at all. Showdown pricing is flatter than
// Classic — this is a rough monotonic guess off the projection so the pool is
// usable before you paste real Showdown numbers. Flagged in the UI as estimated.
function estimateShowdownSalary(medianProj, classicSalary) {
  if (classicSalary && classicSalary > 0) {
    // Compress Classic pricing toward the Showdown mid ($4-8k band).
    return Math.max(200, Math.round((classicSalary * 0.62 + 2600) / 100) * 100);
  }
  return Math.max(200, Math.min(12000, Math.round((medianProj * 320 + 1500) / 100) * 100));
}

/** Build the showdown pool from a single game's sim result. */
function buildShowdownPool(simRes) {
  if (!simRes?.projections) return [];
  const pool = [];
  const seen = new Set();
  for (const p of simRes.projections) {
    const pos = (p.pos || '').replace(/\d/g, '').toUpperCase();
    if (!['QB', 'RB', 'WR', 'TE', 'DST'].includes(pos)) continue;
    const key = `${p.name}_${p.team}`;
    if (seen.has(key)) continue;
    seen.add(key);
    const pcts = p.dk_pcts_all && p.dk_pcts_all.length === 101 ? p.dk_pcts_all : null;
    const median = pcts ? pcts[50] : (p.dk_points || 0);
    const realSalary = p.salary ?? null;
    pool.push({
      id: key,
      name: p.name,
      pos,
      team: p.team,
      salary: realSalary ?? estimateShowdownSalary(median, null),
      salaryEstimated: realSalary == null,
      projection: round1(median),
      simProjection: round1(median),
      p85: pcts ? round1(pcts[85]) : null,
      ceiling: p.ceiling_dk_points ?? (pcts ? round1(pcts[95]) : null),
      dk_pcts_all: pcts,
      simCptRate: p.optimal_cpt_pct ?? null,    // sim's optimal-captain rate — feeds the ownership model
      simFlexRate: p.optimal_flex_pct ?? null,  // sim's optimal-flex rate — feeds the ownership model
      ownFlexModel: null,  // modelled FLEX ownership % (from /showdown_prep)
      ownCptModel: null,   // modelled CPT ownership %
      ownFlex: null,       // user override for FLEX ownership %
      ownCpt: null,        // user override for CPT ownership %
      locked: false,
      lockedCpt: false,
      excluded: false,
    });
  }
  return pool.sort((a, b) => b.projection - a.projection);
}

// DK's real Showdown cap. settings.maxSalary can only go BELOW it (forces our
// lineups to leave salary on the table); the backend's synthetic field and
// the "over cap" warning always use this real number.
const DK_SALARY_CAP = 50000;

// ─── Post-generation lineup filter (2026-09-29) ──────────────────────────────
// Ownership cuts applied AFTER lineups come back, not solver constraints: a
// lineup failing any set threshold is dropped from the table, exposures,
// portfolio tiles and the CSV export (a real portfolio cut -- unlike the
// exposure drill-down, which is view-only). null = threshold off.
//   maxSumOwn -- max summed ownership % (CPT own + 5 FLEX own)
//   minOneIn  -- product ownership must be at least as rare as "1 in X"
//   maxDupe   -- max estimated duplicates (field size x product ownership)
const DEFAULT_LINEUP_FILTER = { maxSumOwn: null, minOneIn: null, maxDupe: null };

/** Does lineup `lu` pass the ownership filter `f`? Inputs: backend lineup
 *  object, DEFAULT_LINEUP_FILTER-shaped object. Output: bool. A lineup
 *  without product_ownership (older saved slot) is judged on the rest only. */
function passesLineupFilter(lu, f) {
  if (f.maxSumOwn != null && (lu.total_ownership ?? 0) > f.maxSumOwn) return false;
  if (f.maxDupe != null && (lu.dupe_est ?? 0) > f.maxDupe) return false;
  if (f.minOneIn != null && lu.product_ownership != null && lu.product_ownership > 1 / f.minOneIn) return false;
  return true;
}

/** Product ownership as "1 in X" (Input: probability float; Output: str),
 *  e.g. 2.5e-6 -> "1 in 400K". The raw numbers (~1e-7) are unreadable. */
function fmtOneIn(p) {
  if (p == null) return '—';
  if (!(p > 0)) return '1 in ∞';
  const x = 1 / p;
  if (x >= 1e9) return `1 in ${(x / 1e9).toFixed(1)}B`;
  if (x >= 1e6) return `1 in ${(x / 1e6).toFixed(1)}M`;
  if (x >= 1e3) return `1 in ${Math.round(x / 1e3)}K`;
  return `1 in ${Math.round(x)}`;
}

/** Stable identity for a lineup (Input: lineup; Output: str "CPT|flex,flex..").
 *  Keys the paper-trade ✓ map so a flag stays on ITS lineup when the table is
 *  re-sorted, drilled into, or ownership-filtered (was keyed by row index). */
const lineupSig = (lu) => {
  const cpt = lu.players.find(p => p.slot === 'CPT');
  const flex = lu.players.filter(p => p.slot === 'FLEX').map(p => `${p.name}_${p.team}`).sort();
  return `${cpt ? `${cpt.name}_${cpt.team}` : ''}|${flex.join(',')}`;
};

/** How many of the 6 players (captain included) are from the AWAY team.
 *  Inputs: lineup, away team abbr. Output: int 1-5 (DK requires both teams).
 *  Builds are always read away-first: 4 = "4-2" = 4 away / 2 home. */
const awayCountOf = (lu, away) => lu.players.filter(p => p.team === away).length;
const BUILD_AWAY_COUNTS = [5, 4, 3, 2, 1];

const DEFAULT_SETTINGS = {
  contestType: 'top_heavy',
  objective: 'blend',       // 'blend' | 'mean' | 'median' | 'ceiling' — see objectiveProjection
  maxSalary: DK_SALARY_CAP, // max salary OUR lineups may use (<= DK_SALARY_CAP)
  minSalary: 0,             // min salary OUR lineups must use (0 = no floor; <= maxSalary)
  // Per-lineup caps on defenses / kickers (captain slot included). '' = no limit.
  maxDstK: '',              // DST + K combined
  maxDst: '',               // DST alone
  maxK: '',                 // K alone
  // Build minimums: { [away count 1-5]: min % of lineups (0-100) }, blank =
  // no minimum. Floors only -- the solver forces a build only when it falls
  // behind pace; see optimize_showdown's _pick_forced_build.
  buildMinimums: {},
  nLineups: 20,
  minUnique: 2,
  maxExposure: 60,
  cptMaxExposure: 35,
  leverageLambda: 0.25,
  entryFee: 5,
  fieldSize: 50000,
  payingPositions: 12000,
  payoutStructure: null,   // real rank-by-rank tiers from a picked DK contest
  contest: null,           // { dk_contest_id, name, entry_fee, field_size, prize_pool }
};

export default function ShowdownOptimizer({ allSimResults = {}, games = [], selectedWeek, weeks = [], setSelectedWeek, simVersion = null }) {
  // Games that have a sim result with player projections (this tool requires it).
  const simmedGames = useMemo(() => {
    return (games || [])
      .map(g => ({ ...g, sim: allSimResults?.[g.game_id] }))
      .filter(g => g.sim?.projections?.length);
  }, [games, allSimResults]);

  // `rawGameId` is '' until the user picks; fall back to the first simmed game
  // so the page is usable immediately without a setState-in-effect.
  const [rawGameId, setRawGameId] = useState('');
  const gameId = rawGameId || simmedGames[0]?.game_id || '';
  const activeGame = simmedGames.find(g => g.game_id === gameId) || null;

  // Base pool is derived from the picked game's sim; the user's per-player
  // edits live in a separate keyed map and are merged on render, so switching
  // games never needs an effect to rebuild state.
  const simPool = useMemo(
    () => (activeGame?.sim ? buildShowdownPool(activeGame.sim) : []),
    [activeGame],
  );
  const [edits, setEdits] = useState({});          // { [playerId]: {...patch} }
  const [dkKickers, setDkKickers] = useState([]);  // kicker pool rows from the DK feed
  const [prepData, setPrepData] = useState({});    // { [id]: {ownFlexModel, ownCptModel, projection?, ceiling?, dk_pcts_all?} }
  const [frozenOwn, setFrozenOwn] = useState(null); // { [id]: {flex, cpt} } snapshot, or null
  const ownFrozen = frozenOwn != null;

  const basePool = useMemo(() => [...simPool, ...dkKickers], [simPool, dkKickers]);
  // Merge order: base sim row → prep (ownership, opt rates, kicker line) →
  // Game-Read conditioned distribution (if a scenario is active) → edits.
  // `projection` is always derived as simProjection + the manual delta, so
  // the solver objective, CPT Proj and ceiling all follow the scenario.
  const pool = useMemo(
    () => basePool.map(b => {
      const { condSim, ...prep } = prepData[b.id] || {};
      const e = edits[b.id] || {};
      // eslint-disable-next-line no-unused-vars
      const { projection: _absProj, projDelta: _d, ...editRest } = e;
      const merged = { ...b, ...prep, ...editRest };
      const delta = projDeltaOf({ simProjection: merged.simProjection }, e);
      if (condSim) {
        merged.simProjection = condSim.projection;
        merged.p85 = condSim.p85;
        merged.ceiling = condSim.ceiling;
        merged.dk_pcts_all = condSim.dk_pcts_all;
        merged.conditioned = true;
      }
      merged.projDelta = delta;
      merged.projection = round1(Math.max(0, (merged.simProjection ?? 0) + delta));
      return merged;
    }),
    [basePool, prepData, edits],
  );
  // Resolved ownership for the payload: hand override → frozen snapshot → model.
  const ownOf = (p) => ({
    flex: p.ownFlex ?? frozenOwn?.[p.id]?.flex ?? p.ownFlexModel ?? null,
    cpt: p.ownCpt ?? frozenOwn?.[p.id]?.cpt ?? p.ownCptModel ?? null,
  });

  const [settings, setSettings] = useState(DEFAULT_SETTINGS);
  const [view, setView] = useState('pool');
  const [isOptimizing, setIsOptimizing] = useState(false);
  const [error, setError] = useState('');
  const [lineups, setLineups] = useState([]);
  const [portfolio, setPortfolio] = useState(null);
  const [notes, setNotes] = useState([]);
  const [resultsMode, setResultsMode] = useState('optimize'); // 'optimize' | 'lab'
  const [labOpen, setLabOpen] = useState(false);
  const [labRows, setLabRows] = useState([{ label: '', cpt: '', flex: ['', '', '', '', ''] }]);
  const [expanded, setExpanded] = useState(null);
  const [histLineup, setHistLineup] = useState(null);
  const [resSort, setResSort] = useState({ field: 'total_ownership', asc: true });
  const [search, setSearch] = useState('');
  const [dkStatus, setDkStatus] = useState(''); // '', 'loading', or a result message
  const [dkExtras, setDkExtras] = useState([]); // DK-priced deep-bench players not in the sim pool
  const [dkContests, setDkContests] = useState([]); // live contests on this game's showdown slate
  const [contestStatus, setContestStatus] = useState(''); // '', 'loading', 'ok', 'error'
  const [contestFilter, setContestFilter] = useState(''); // contest-name search box
  const [gameReadOpen, setGameReadOpen] = useState(false);
  const [gameDist, setGameDist] = useState(null);   // GET /api/game_distribution response for this game
  const [scenario, setScenario] = useState(null);   // GameDistribution onSelect payload, or null = unconditioned
  const [condN, setCondN] = useState(null);         // # sims the pool's projections are conditioned on (null = unconditioned)
  const [prepLoading, setPrepLoading] = useState(false); // /showdown_prep in flight (scenario re-solve can take a few seconds)
  const prepReqSeq = useRef(0);                      // latest runPrep request id -- stale responses are dropped

  // ── Paper trading (Phase 3): flag a lineup as "I'm actually entering this".
  // score_paper_entries.py settles it later against a dropped-in standings CSV.
  const [paperDefaults, setPaperDefaults] = useState({ contestName: '', entryFee: 20, maxEntries: 1 });
  const [savedPaperIds, setSavedPaperIds] = useState({}); // { [lineupSig]: entry_id }
  // Post-generation ownership cut (see DEFAULT_LINEUP_FILTER). Saved with the
  // workspace slot; the full generated set is kept underneath so loosening a
  // threshold never needs a re-optimize.
  const [lineupFilter, setLineupFilter] = useState(DEFAULT_LINEUP_FILTER);
  const [paperStatus, setPaperStatus] = useState('');

  const patchPlayer = (id, patch) =>
    setEdits(e => ({ ...e, [id]: { ...(e[id] || {}), ...patch } }));

  const pickGame = (id) => {
    setRawGameId(id); setView('pool'); setExpanded(null);
    setDkStatus(''); setDkExtras([]); setDkContests([]); setContestStatus(''); setContestFilter('');
    setDkKickers([]); setPrepData({}); setFrozenOwn(null); setEdits({});
    setLabRows([{ label: '', cpt: '', flex: ['', '', '', '', ''] }]); setLabOpen(false);
    setGameDist(null); setScenario(null); setCondN(null); setGameReadOpen(false);
    setSavedPaperIds({}); setPaperStatus('');
    setSettings(s => ({ ...s, payoutStructure: null, contest: null }));
  };

  // POST the whole pool to /showdown_prep → modelled FLEX/CPT ownership for
  // everyone + a synthesized line for each kicker. Mirrors the classic
  // optimizer pre-populating ownership_proj from the weekly sim.
  const runPrep = async (poolForPrep) => {
    if (!gameId || !poolForPrep.length) return;
    // Only the newest request may write state: a scenario re-solve takes a
    // few seconds, and a quick re-pick must not be overwritten by the older
    // response landing late.
    const reqId = ++prepReqSeq.current;
    setPrepLoading(true);
    try {
      const res = await ApiService.showdownPrep({
        game_id: gameId,
        away_team: activeGame?.away_team, home_team: activeGame?.home_team,
        // Scope the real optimal-captain/FLEX solve and the pool's
        // projection distribution to the Game-Read box-select when one is
        // active (same convention as Optimize/Lab's own iteration_filter).
        // The ownership model is deliberately NOT conditioned -- see
        // showdown_prep's comment.
        iteration_filter: scenario?.idx || undefined,
        // Always the UNCONDITIONED base (simPool + edits), so the ownership
        // model's inputs never move with the Game Read.
        players: poolForPrep.map(p => ({
          name: p.name, team: p.team, pos: p.pos,
          salary: p.salary ? Math.round(p.salary) : null,
          projection: p.pos === 'K' ? null : round1((p.simProjection ?? 0) + projDeltaOf(p, edits[p.id])),
          optimal_cpt_pct: p.simCptRate ?? null,
          optimal_flex_pct: p.simFlexRate ?? null,
          ownership_pct: p.ownFlex ?? null,
          cpt_ownership_pct: p.ownCpt ?? null,
        })),
      });
      if (reqId !== prepReqSeq.current) return;
      const byId = {};
      (res.players || []).forEach(r => {
        const id = `${r.name}_${r.team}`;   // matches buildShowdownPool / kicker row ids
        byId[id] = {
          ownFlexModel: r.ownership_pct ?? null,
          ownCptModel: r.cpt_ownership_pct ?? null,
          // Real per-iteration optimal rates from _compute_showdown_optimal_rates
          // (scoped to the active Game-Read scenario, if any) -- overrides the
          // base pool's static simFlexRate/simCptRate, which came from the bulk
          // week-prepopulation sim's 2-iteration guess (see showdown_prep's
          // docstring).
          simFlexRate: r.optimal_flex_pct ?? null,
          simCptRate: r.optimal_cpt_pct ?? null,
          ...(r.pos === 'K' ? {
            // simProjection too, not just projection -- otherwise the kicker's
            // baseline stayed at the flat 8.0 placeholder and every solver
            // objective read (P50 - 8.0) as a phantom manual bump.
            simProjection: r.projection ?? undefined,
            ceiling: r.ceiling ?? undefined,
            dk_pcts_all: r.dk_pcts_all ?? undefined,
          } : {}),
          // Game-Read scenario distribution (P50/P85/P95/curve over only the
          // box-selected sims). Kept as its own object and applied in the
          // `pool` memo, so clearing the scenario falls straight back to the
          // unconditioned base. Ownership is NOT touched by this -- the
          // backend runs the ownership model on unconditioned inputs.
          condSim: r.cond_projection != null ? {
            projection: r.cond_projection,
            p85: r.cond_p85,
            ceiling: r.cond_ceiling,
            dk_pcts_all: r.cond_dk_pcts_all,
          } : null,
        };
      });
      setPrepData(byId);
      setCondN(res.conditioned_n ?? null);
      const hasKickers = poolForPrep.some(p => p.pos === 'K');
      if (hasKickers && res.kicker_source !== 'game_script') {
        setDkStatus(s => `${s} · kicker lines are flat defaults (game sim has no kicker data)`);
      }
    } catch (err) {
      console.error('showdown_prep failed', err);
    } finally {
      if (reqId === prepReqSeq.current) setPrepLoading(false);
    }
  };

  const freezeOwnership = () => {
    const snap = {};
    for (const p of pool) {
      snap[p.id] = { flex: p.ownFlexModel ?? 0.5, cpt: p.ownCptModel ?? 0.3 };
    }
    setFrozenOwn(snap);
  };
  const unfreezeOwnership = () => setFrozenOwn(null);

  // ── Import real projected ownership from an external tool ──────────────────
  // Accepts pasted rows. Each line: a player name plus its two ownership
  // numbers. We take the LAST two numbers on the line as (total_own, cpt_own)
  // — matching the "Total Own" / "CPT Own" column order in the tools Cam uses
  // — and store FLEX own = max(0, total − cpt), CPT own = cpt, as hand
  // overrides (blue, always win). A header line (no player match) is skipped.
  const [ownImportOpen, setOwnImportOpen] = useState(false);
  const [ownImportText, setOwnImportText] = useState('');
  const [ownImportStatus, setOwnImportStatus] = useState('');

  const applyOwnershipImport = () => {
    const byNorm = new Map(basePool.map(p => [normName(p.name), p]));
    const POS_RE = /\b(QB|RB|WR|TE|K|DST|D\/ST|DEF)\b/i;
    let matched = 0;
    const misses = [];
    const patches = {};
    for (const rawLine of ownImportText.split('\n')) {
      const line = rawLine.trim();
      if (!line) continue;
      const nums = line.match(/\d+(?:\.\d+)?/g);
      if (!nums || nums.length < 2) continue;
      const total = parseFloat(nums[nums.length - 2]);
      const cpt = parseFloat(nums[nums.length - 1]);
      if (!isFinite(total) || !isFinite(cpt)) continue;

      // Name = everything before the position token (or the first $ / digit).
      const posMatch = line.match(POS_RE);
      let name, teamTok = null, isDst = false;
      if (posMatch) {
        name = line.slice(0, posMatch.index).trim();
        isDst = /^(DST|D\/ST|DEF)$/i.test(posMatch[1]);
        const after = line.slice(posMatch.index + posMatch[1].length).trim();
        const tm = after.match(/^([A-Za-z]{2,3})\b/);
        if (tm) teamTok = normTeam(tm[1].toUpperCase());
      } else {
        const cut = line.search(/[$\d]/);
        name = (cut > 0 ? line.slice(0, cut) : line).trim();
      }

      let p = null;
      if (isDst) {
        p = basePool.find(x => x.pos === 'DST' && (!teamTok || normTeam(x.team) === teamTok))
          || basePool.find(x => x.pos === 'DST');
      }
      if (!p) p = byNorm.get(normName(name));
      if (!p && teamTok) p = basePool.find(x => normName(x.name) === normName(name) && normTeam(x.team) === teamTok);
      if (!p) { misses.push(name || rawLine.trim().slice(0, 20)); continue; }
      matched += 1;
      patches[p.id] = { ownFlex: Math.max(0, +(total - cpt).toFixed(1)), ownCpt: +cpt.toFixed(1) };
    }
    if (matched === 0) { setOwnImportStatus('No rows matched. Expected: "Player Name  <total own>  <cpt own>" per line.'); return; }
    setEdits(e => {
      const next = { ...e };
      for (const [id, patch] of Object.entries(patches)) next[id] = { ...(next[id] || {}), ...patch };
      return next;
    });
    setFrozenOwn(null); // hand overrides supersede
    setOwnImportStatus(`Imported ownership for ${matched} player${matched > 1 ? 's' : ''}` +
      (misses.length ? ` · unmatched: ${misses.slice(0, 6).join(', ')}${misses.length > 6 ? '…' : ''}` : ''));
  };

  // Pull the live DK contests for the resolved showdown draft group (same feed
  // the classic optimizer's contest picker uses; entry fee / field size /
  // prize pool per contest, no per-contest call needed for the list).
  const loadContests = async (draftGroupId) => {
    if (!draftGroupId) return;
    setContestStatus('loading');
    try {
      const res = await ApiService.getDkContests(draftGroupId);
      setDkContests(res?.contests || []);
      setContestStatus((res?.contests || []).length ? '' : 'error');
    } catch {
      setContestStatus('error');
    }
  };

  // Apply one picked contest: fill entry fee / field size / paying spots, then
  // fetch its real rank-by-rank payout table for the EV math (falls back to the
  // contest-type-shaped estimate if the contest has no cash tiers yet).
  const applyContest = async (contestId) => {
    const c = dkContests.find(x => String(x.contest_id) === String(contestId));
    if (!c) return;
    setSettings(s => ({
      ...s,
      fieldSize: c.max_entries ?? s.fieldSize,
      entryFee: c.entry_fee ?? s.entryFee,
      payoutStructure: null,
      contest: {
        dk_contest_id: String(c.contest_id), name: c.name ?? null,
        entry_fee: c.entry_fee ?? null, field_size: c.max_entries ?? null,
        prize_pool: c.prize_pool ?? null,
      },
    }));
    setContestStatus('loading');
    const payout = await ApiService.getDkContestPayout(contestId);
    if (payout.tiers && payout.tiers.length > 0) {
      const payingPositions = Math.max(...payout.tiers.map(t => t.rank_end));
      setSettings(s => ({ ...s, payingPositions, payoutStructure: payout.tiers }));
      setContestStatus('ok');
    } else {
      setContestStatus('error');
    }
  };

  // Pull DK's live Showdown salary pool for this game and merge base/captain
  // salaries + draftableIds onto matched pool players (normalised name + team).
  const loadDkSalaries = async () => {
    if (!activeGame) return;
    setDkStatus('loading');
    setDkExtras([]);
    try {
      const res = await ApiService.getDkShowdownSalaries(activeGame.away_team, activeGame.home_team);
      if (!res || !res.found) {
        setDkStatus(`No live DK Showdown slate found for ${activeGame.away_team} @ ${activeGame.home_team}.`);
        return;
      }
      const dkByKey = new Map();
      [...(res.players || []), ...(res.defense || [])].forEach(d => {
        dkByKey.set(`${normName(d.name)}|${normTeam(d.team)}`, d);
      });
      let matched = 0;
      const patches = {};
      for (const p of basePool) {
        const dkKey = p.pos === 'DST' ? `__dst__|${normTeam(p.team)}` : `${normName(p.name)}|${normTeam(p.team)}`;
        // DST from DK is keyed by its own displayName, not "__dst__"; try team match on the defense list.
        let d = dkByKey.get(dkKey);
        if (!d && p.pos === 'DST') d = (res.defense || []).find(x => normTeam(x.team) === normTeam(p.team));
        if (!d) continue;
        matched += 1;
        patches[p.id] = { salary: d.salary, salaryEstimated: false, dkFlexId: d.flex_id ?? null, dkCptId: d.cpt_id ?? null };
      }
      setEdits(e => {
        const next = { ...e };
        for (const [id, patch] of Object.entries(patches)) next[id] = { ...(next[id] || {}), ...patch };
        return next;
      });

      // Kickers: DK prices them but the sim doesn't project them — add them as
      // real pool rows; /showdown_prep fills their projection/ceiling/dist.
      const kickers = (res.players || [])
        .filter(d => d.pos === 'K')
        .map(d => ({
          id: `${d.name}_${normTeam(d.team)}`,
          name: d.name, pos: 'K', team: normTeam(d.team),
          salary: d.salary, salaryEstimated: false,
          projection: 8.0, simProjection: 8.0, ceiling: null, p85: null,
          dk_pcts_all: null, simCptRate: null, simFlexRate: null,
          ownFlexModel: null, ownCptModel: null, ownFlex: null, ownCpt: null,
          dkFlexId: d.flex_id ?? null, dkCptId: d.cpt_id ?? null,
          locked: false, lockedCpt: false, excluded: false,
        }));
      setDkKickers(kickers);

      // Other DK-priced players not in the sim pool (deep bench ≥ $3k) — just a note.
      const poolKeys = new Set(basePool.map(p => p.pos === 'DST'
        ? `__dst__|${normTeam(p.team)}` : `${normName(p.name)}|${normTeam(p.team)}`));
      const extras = (res.players || [])
        .filter(d => d.pos !== 'K' && !poolKeys.has(`${normName(d.name)}|${normTeam(d.team)}`) && d.salary >= 3000)
        .sort((a, b) => b.salary - a.salary);
      setDkExtras(extras);

      const unmatched = simPool.length - matched;
      setDkStatus(
        `DK slate ${res.draft_group_id}: matched ${matched}/${simPool.length}` +
        (unmatched ? `, ${unmatched} unmatched (kept estimate)` : '') +
        (kickers.length ? ` · +${kickers.length} kicker${kickers.length > 1 ? 's' : ''}` : '')
      );
      loadContests(res.draft_group_id);
      // Prep runs against the sim pool + the new kicker rows.
      runPrep([...simPool.map(p => ({ ...p, ...(edits[p.id] || {}) })), ...kickers]);
    } catch (err) {
      console.error(err);
      setDkStatus('DK salary fetch failed.');
    }
  };

  // Auto-load DK salaries + contests + ownership the moment a game is in view
  // (matches the classic optimizer, which comes pre-populated). Runs once per
  // game; the manual "Load DK Salaries" button re-runs it.
  const dkLoadedFor = useRef('');
  // Tracks the last `${gameId}|${scenario.idx}` key runPrep has actually run
  // for -- shared with the Game-Read effect below so its first fire (mount,
  // scenario === null) doesn't duplicate loadDkSalaries' own tail-end
  // runPrep call for that exact same unconditioned case.
  const scenarioPrepFor = useRef(null);
  useEffect(() => {
    if (activeGame && gameId && dkLoadedFor.current !== gameId) {
      dkLoadedFor.current = gameId;
      scenarioPrepFor.current = `${gameId}|`;
      loadDkSalaries();
    }
  }, [gameId, activeGame]); // eslint-disable-line react-hooks/exhaustive-deps

  // Re-run prep (real optimal-captain/FLEX rates + the ownership model that
  // consumes them) whenever the Game-Read scenario changes, so Opt FLEX%/
  // Opt CPT% reflect the box-selected iterations instead of the whole
  // season. Guarded to the initial DK/prep load having already happened for
  // this game (loadDkSalaries' own runPrep call already covers the
  // unconditioned scenario === null case) and to only fire on a genuine
  // scenario change, not every render.
  useEffect(() => {
    if (!gameId || dkLoadedFor.current !== gameId) return;
    const key = `${gameId}|${scenario ? scenario.idx?.join(',') : ''}`;
    if (scenarioPrepFor.current === key) return;
    scenarioPrepFor.current = key;
    runPrep([...simPool.map(p => ({ ...p, ...(edits[p.id] || {}) })), ...dkKickers]);
  }, [scenario, gameId]); // eslint-disable-line react-hooks/exhaustive-deps

  // Game Read: the standalone (no player-projection cost) outcome distribution
  // for this one game, so its box-select always has exact iteration ids ready
  // -- no separate "run a fresh sim" step, unlike the Simulator's week-wide view.
  // Also refetches when a new sim run lands (simVersion = GET /api/sim_status
  // sims_updated_at, 2026-09-23). The new distribution resets GameDistribution's
  // box selection, which clears `scenario` -- correct, since its iteration ids
  // pointed into the previous run.
  const gameDistLoadedFor = useRef('');
  useEffect(() => {
    const loadKey = `${gameId}|${simVersion ?? ''}`;
    if (!activeGame || !gameId || gameDistLoadedFor.current === loadKey) return;
    gameDistLoadedFor.current = loadKey;
    ApiService.getGameDistribution(activeGame.away_team, activeGame.home_team, selectedWeek)
      .then(setGameDist)
      .catch(() => setGameDist(null));
  }, [gameId, activeGame, selectedWeek, simVersion]);

  // ── Save slots: 3 switchable, autosaved workspace snapshots for this game
  // (see useWorkspaceSlots) -- pool edits, settings, lineups, and the Game
  // Read scenario survive a page/tab switch, and up to 3 different takes on
  // the slate (chalk / contrarian / stack) can be kept side by side.
  const slateKey = gameId ? `showdown_${gameId}` : null;
  const workspaceSnapshot = useMemo(() => ({
    edits, frozenOwn, settings, labRows, lineups, portfolio, notes, resultsMode, view, scenario, lineupFilter,
  }), [edits, frozenOwn, settings, labRows, lineups, portfolio, notes, resultsMode, view, scenario, lineupFilter]);
  const onHydrateWorkspace = (data) => {
    // Reset to defaults (not a same-key merge) when the slot has nothing
    // saved for a field -- an empty slot must actually look empty, not
    // silently keep whatever the previously-active slot left in memory.
    setEdits(data.edits || {});
    setFrozenOwn(data.frozenOwn ?? null);
    setSettings(data.settings ? { ...DEFAULT_SETTINGS, ...data.settings } : DEFAULT_SETTINGS);
    setLabRows(data.labRows?.length ? data.labRows : [{ label: '', cpt: '', flex: ['', '', '', '', ''] }]);
    setLineups(data.lineups || []);
    setPortfolio(data.portfolio || null);
    setNotes(data.notes || []);
    setResultsMode(data.resultsMode || 'optimize');
    setView((data.lineups || []).length ? (data.view || 'pool') : 'pool');
    setScenario(data.scenario || null);
    setLineupFilter({ ...DEFAULT_LINEUP_FILTER, ...(data.lineupFilter || {}) });
    setExpanded(null);
  };
  const workspace = useWorkspaceSlots(slateKey, selectedWeek, 2026, workspaceSnapshot, onHydrateWorkspace);

  const activeCount = pool.filter(p => !p.excluded).length;
  const cptLockCount = pool.filter(p => p.lockedCpt).length;
  const cptEligibleCount = pool.filter(p => !p.excluded && p.cptEligible !== false).length;

  // Player pool payload shared by ⚡ Optimize and the Lineup Lab. `all` keeps
  // excluded players (the Lab can score any hand pick); Optimize drops them.
  const buildPlayersPayload = (all = false) =>
    (all ? pool : pool.filter(p => !p.excluded)).map(p => {
      const o = ownOf(p);
      return {
        name: p.name, team: p.team, pos: p.pos,
        salary: Math.round(p.salary),
        projection: p.projection,
        gpp_projection: objectiveProjection(p, settings.objective, settings.contestType),
        locked: p.locked,
        locked_cpt: p.lockedCpt,
        cpt_eligible: p.cptEligible !== false,
        ownership_pct: o.flex,
        cpt_ownership_pct: o.cpt,
        optimal_cpt_pct: p.simCptRate ?? null,
        optimal_flex_pct: p.simFlexRate ?? null,
        dk_pcts_all: p.dk_pcts_all || null,
        dk_id: p.dkFlexId ?? null,
        dk_cpt_id: p.dkCptId ?? null,
      };
    });

  // Build minimums as lineup counts for the current # Lineups -- rounds UP
  // exactly like the backend's _build_quotas, so the hint and the 400 agree.
  // Output: [{ away, pct, count }] for every build with a minimum set.
  const buildMinQuotas = BUILD_AWAY_COUNTS
    .map(a => ({ away: a, pct: +(settings.buildMinimums?.[a] || 0) }))
    .filter(q => q.pct > 0)
    .map(q => ({ ...q, count: Math.ceil((q.pct / 100) * settings.nLineups - 1e-9) }));
  const buildMinTotal = buildMinQuotas.reduce((s, q) => s + q.count, 0);

  const handleOptimize = async () => {
    setError('');
    if (activeCount < 6) { setError('Need at least 6 non-excluded players.'); return; }
    if (buildMinTotal > settings.nLineups) {
      setError(`Build minimums need ${buildMinTotal} lineups but # Lineups is ${settings.nLineups}.`); return;
    }
    if (cptLockCount > 1) { setError('Only one player can be locked as captain.'); return; }
    if (cptEligibleCount < 1) { setError('At least one player must be eligible for the captain pool.'); return; }
    setIsOptimizing(true);
    try {
      const payload = {
        game_id: gameId,
        week: selectedWeek,
        players: buildPlayersPayload(false),
        n_lineups: settings.nLineups,
        salary_cap: Math.min(DK_SALARY_CAP, Math.max(1, settings.maxSalary || DK_SALARY_CAP)),
        min_salary: Math.min(Math.max(0, settings.minSalary || 0), Math.min(DK_SALARY_CAP, Math.max(1, settings.maxSalary || DK_SALARY_CAP))),
        // Blank = no limit (omitted); otherwise clamp to 0-6 lineup slots.
        ...Object.fromEntries([['max_dst_k', settings.maxDstK], ['max_dst', settings.maxDst], ['max_k', settings.maxK]]
          .filter(([, v]) => v !== '' && v != null && !Number.isNaN(+v))
          .map(([k, v]) => [k, Math.min(6, Math.max(0, Math.round(+v)))])),
        contest_type: settings.contestType,
        min_unique_players: settings.minUnique,
        max_exposure: settings.maxExposure / 100,
        cpt_max_exposure: settings.cptMaxExposure / 100,
        leverage_lambda: settings.leverageLambda,
        entry_fee: settings.entryFee,
        total_entries: settings.fieldSize,
        paying_positions: Math.min(settings.payingPositions, settings.fieldSize - 1),
        // Real rank-by-rank tiers from a picked DK contest, when we have them —
        // otherwise omitted so the backend uses its contest-type-shaped estimate.
        payout_structure: settings.payoutStructure || undefined,
        // Game Read scenario: score against ONLY the box-selected iterations
        // (see GameDistribution's onSelect) instead of all 1000 -- "given this
        // game plays out in this range, which lineups actually do well".
        iteration_filter: scenario?.idx || undefined,
        // % -> fraction, blanks/zeros dropped; away_team tells the backend
        // which side the "4" in a 4-2 build counts.
        ...(buildMinQuotas.length ? {
          build_minimums: Object.fromEntries(buildMinQuotas.map(q => [q.away, q.pct / 100])),
          away_team: activeGame?.away_team,
        } : {}),
      };
      const res = await ApiService.optimizeShowdown(payload);
      if (!res || !res.lineups) { setError('Optimizer returned nothing — is the backend running?'); return; }
      setLineups(res.lineups);
      setPortfolio(res.portfolio || null);
      setNotes(res.notes || []);
      setResultsMode('optimize');
      setSavedPaperIds({});
      setView('results');
    } catch (e) {
      console.error(e);
      setError(String(e.message || e));
    } finally {
      setIsOptimizing(false);
    }
  };

  // ── Lineup Lab: score hand-built lineups through the same field sim ─────────
  const labOptions = useMemo(
    () => [...pool].sort((a, b) => (b.salary || 0) - (a.salary || 0)),
    [pool],
  );
  const optKey = p => `${p.name}|${p.team}`;
  const optLabel = p => `${p.pos === 'DST' ? `${p.team} DST` : p.name} · ${p.team} · $${(p.salary || 0).toLocaleString()}`;

  const scoreLab = async () => {
    setError('');
    const rows = labRows
      .map((r, i) => ({ ...r, i }))
      .filter(r => r.cpt && r.flex.filter(Boolean).length === 5);
    if (!rows.length) { setError('Fill in a captain and 5 FLEX for at least one Lab lineup.'); return; }
    for (const r of rows) {
      const picks = [r.cpt, ...r.flex];
      if (new Set(picks).size !== 6) { setError(`Lab lineup ${r.i + 1}: a player is picked twice.`); return; }
    }
    setIsOptimizing(true);
    try {
      const res = await ApiService.optimizeShowdown({
        game_id: gameId,
        week: selectedWeek,
        players: buildPlayersPayload(true),
        salary_cap: DK_SALARY_CAP, // Lab scores hand lineups -- nothing to constrain
        contest_type: settings.contestType,
        entry_fee: settings.entryFee,
        total_entries: settings.fieldSize,
        paying_positions: Math.min(settings.payingPositions, settings.fieldSize - 1),
        payout_structure: settings.payoutStructure || undefined,
        iteration_filter: scenario?.idx || undefined,
        manual_lineups: rows.map(r => ({
          cpt: r.cpt, flex: r.flex, label: r.label?.trim() || `Lab lineup ${r.i + 1}`,
        })),
      });
      if (!res || !res.lineups) { setError('Backend returned nothing — is it running?'); return; }
      setLineups(res.lineups);
      setPortfolio(res.portfolio || null);
      setNotes(res.notes || []);
      setResultsMode('lab');
      setSavedPaperIds({});
      setExpanded(0);
      setView('results');
    } catch (e) {
      console.error(e);
      setError(String(e.message || e));
    } finally {
      setIsOptimizing(false);
    }
  };

  // ── Paper trading: flag one lineup as "I'm actually entering this" into
  // the contest named in paperDefaults. `slateId` matches the ownership
  // archive's folder naming (showdown_<AWAY>_<HOME>) so the entry lands
  // alongside that slate's salaries/standings, ready for
  // scripts/dfs_ownership/score_paper_entries.py once you drop the
  // standings CSV in after it settles.
  const slateId = activeGame ? `showdown_${activeGame.away_team}_${activeGame.home_team}` : null;
  const savePaperEntry = async (lu, idx) => {
    if (!slateId) return;
    if (!paperDefaults.contestName.trim()) { setPaperStatus('⚠ Set a contest name first (same name you\'ll use for the standings CSV).'); return; }
    const entry = {
      slate_format: 'showdown',
      source: resultsMode,
      label: lu.label || null,
      contest_name: paperDefaults.contestName.trim(),
      entry_fee: paperDefaults.entryFee,
      max_entries: paperDefaults.maxEntries,
      players: lu.players.map(p => ({ slot: p.slot, name: p.name, team: p.team, pos: p.pos })),
      model: lu,
    };
    const res = await ApiService.savePaperEntry(slateId, entry, selectedWeek);
    if (res && res.entry_id) {
      setSavedPaperIds(m => ({ ...m, [lineupSig(lu)]: res.entry_id }));
      setPaperStatus(`📝 Saved "${lu.label || `Lineup ${idx + 1}`}" into ${paperDefaults.contestName}`);
    } else {
      setPaperStatus('Save failed — is the backend running?');
    }
  };

  // ── Ownership cut + build tagging ───────────────────────────────────────────
  // `keptLineups` = generated lineups that pass the ownership filter, each
  // tagged with `build_away` (away-team players incl. CPT) for the Build
  // column/sort. EVERYTHING downstream -- exposures, portfolio tiles, the
  // drill-down, the table, the CSV export -- reads keptLineups, never
  // `lineups`, so a filtered-out lineup is really out of the portfolio.
  const awayTeam = activeGame?.away_team;
  const homeTeam = activeGame?.home_team;
  const filterActive = lineupFilter.maxSumOwn != null || lineupFilter.minOneIn != null || lineupFilter.maxDupe != null;
  const keptLineups = useMemo(
    () => lineups
      .filter(lu => passesLineupFilter(lu, lineupFilter))
      .map(lu => ({ ...lu, build_away: awayTeam ? awayCountOf(lu, awayTeam) : null })),
    [lineups, lineupFilter, awayTeam],
  );

  /** Portfolio tiles recomputed over the kept set -- the backend's `portfolio`
   *  describes every generated lineup, which is stale once the filter cuts.
   *  Input: kept lineups + backend portfolio. Output: portfolio-shaped object.
   *  Eff. lineups mirrors the backend (n x (1 - mean pairwise overlap / 6));
   *  coverage reuses the backend's pool size (used players / coverage_score). */
  const viewPortfolio = useMemo(() => {
    if (!portfolio || !filterActive) return portfolio;
    const ls = keptLineups;
    const n = ls.length;
    const mean = (f) => (n ? ls.reduce((s, l) => s + (l[f] ?? 0), 0) / n : 0);
    const max = (f) => (n ? Math.max(...ls.map(l => l[f] ?? 0)) : 0);
    const members = ls.map(l => new Set(l.players.map(p => `${p.name}_${p.team}`)));
    let simSum = 0, pairs = 0;
    for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) {
      let shared = 0; members[i].forEach(k => { if (members[j].has(k)) shared++; });
      simSum += shared / 6; pairs++;
    }
    const allUsed = new Set(lineups.flatMap(l => l.players.map(p => `${p.name}_${p.team}`)));
    const poolN = portfolio.coverage_score > 0 ? allUsed.size / portfolio.coverage_score : allUsed.size;
    const used = new Set(members.flatMap(m => [...m]));
    const r2 = (v) => Math.round(v * 100) / 100;
    return {
      ...portfolio,
      n_generated: n,
      total_ev_pct: r2(mean('ev_pct')),
      avg_total_ownership: Math.round(mean('total_ownership') * 10) / 10,
      unique_captains: new Set(ls.map(l => l.cpt_name)).size,
      effective_lineup_count: Math.round(n * (1 - (pairs ? simSum / pairs : 0)) * 10) / 10,
      coverage_score: poolN ? Math.round((used.size / poolN) * 1000) / 1000 : 0,
      avg_top1_pct: r2(mean('top1_pct')), best_top1_pct: r2(max('top1_pct')),
      avg_top01_pct: r2(mean('top01_pct')), best_top01_pct: r2(max('top01_pct')),
      avg_first_pct: Math.round(mean('first_pct') * 1000) / 1000, best_first_pct: max('first_pct'),
    };
  }, [portfolio, filterActive, keptLineups, lineups]);

  // ── Portfolio exposure roll-up ──────────────────────────────────────────────
  // Output per player: { key, name, team, pos, flex, cpt, total, totalPct,
  //   cptPct, salary (FLEX), projection (FLEX), flexOwn, cptOwn, fieldOwn,
  //   leverage, cptLeverage }. Salary/projection/ownership come from the
  //   lineups themselves (what the run was scored with); the pool is only a
  //   fallback for a slot the player never filled (e.g. never captained ->
  //   no CPT own on any lineup). CPT slot salary/projection are 1.5x, so
  //   they're converted back to the FLEX base.
  //   fieldOwn = flexOwn + cptOwn: FLEX own is a per-player share of the
  //   5-FLEX group and CPT own of the captain slot, which are mutually
  //   exclusive, so their sum is the field's "in the lineup at all" rate --
  //   the apples-to-apples comparison for totalPct.
  const exposures = useMemo(() => {
    if (!keptLineups.length) return [];
    const poolById = new Map(pool.map(p => [p.id, p]));
    const map = {};
    for (const lu of keptLineups) {
      for (const pl of lu.players) {
        const k = `${pl.name}_${pl.team}`;
        map[k] = map[k] || { key: k, name: pl.name, team: pl.team, pos: pl.pos, flex: 0, cpt: 0,
          salary: null, projection: null, flexOwn: null, cptOwn: null };
        const e = map[k];
        if (pl.slot === 'CPT') {
          e.cpt += 1;
          if (e.cptOwn == null && pl.ownership_pct != null) e.cptOwn = pl.ownership_pct;
          if (e.salary == null) e.salary = pl.base_salary ?? (pl.salary != null ? Math.round(pl.salary / 1.5) : null);
          if (e.projection == null && pl.projection != null) e.projection = round1(pl.projection / 1.5);
        } else {
          e.flex += 1;
          if (e.flexOwn == null && pl.ownership_pct != null) e.flexOwn = pl.ownership_pct;
          e.salary = pl.base_salary ?? pl.salary ?? e.salary;
          if (pl.projection != null) e.projection = pl.projection;
        }
      }
    }
    const n = keptLineups.length;
    return Object.values(map)
      .map(e => {
        const pp = poolById.get(e.key);
        const o = pp ? ownOf(pp) : {};
        const flexOwn = e.flexOwn ?? o.flex ?? null;
        const cptOwn = e.cptOwn ?? o.cpt ?? null;
        const totalPct = ((e.cpt + e.flex) / n) * 100;
        const cptPct = (e.cpt / n) * 100;
        const fieldOwn = flexOwn != null || cptOwn != null ? (flexOwn || 0) + (cptOwn || 0) : null;
        return {
          ...e,
          salary: e.salary ?? pp?.salary ?? 0,
          projection: e.projection ?? pp?.projection ?? 0,
          flexOwn, cptOwn, fieldOwn,
          total: e.cpt + e.flex, totalPct, cptPct,
          leverage: fieldOwn != null ? totalPct - fieldOwn : null,
          cptLeverage: cptOwn != null ? cptPct - cptOwn : null,
        };
      })
      .sort((a, b) => b.total - a.total);
  }, [keptLineups, pool, frozenOwn]); // eslint-disable-line react-hooks/exhaustive-deps

  // ── Portfolio Exposure panel controls (mirrors Optimizer.jsx's classic panel)
  const [expSearch, setExpSearch] = useState('');
  const [expPos, setExpPos] = useState('ALL');
  const [expSort, setExpSort] = useState({ field: 'totalPct', asc: false });
  /** exposures narrowed by the search box + position tab and sorted by the
   *  panel's sort buttons. null values (no ownership data) sort last. */
  const filteredExposures = useMemo(() => {
    const q = expSearch.trim().toLowerCase();
    const { field, asc } = expSort;
    return exposures
      .filter(e => (expPos === 'ALL' || e.pos === expPos)
        && (!q || e.name.toLowerCase().includes(q) || e.team.toLowerCase().includes(q)))
      .sort((a, b) => {
        const av = a[field], bv = b[field];
        if (av == null && bv == null) return 0;
        if (av == null) return 1;
        if (bv == null) return -1;
        return asc ? av - bv : bv - av;
      });
  }, [exposures, expSearch, expPos, expSort]);

  const sortedLineups = useMemo(() => {
    const arr = [...keptLineups];
    const { field, asc } = resSort;
    arr.sort((a, b) => {
      const av = a[field] ?? 0, bv = b[field] ?? 0;
      return asc ? av - bv : bv - av;
    });
    return arr;
  }, [keptLineups, resSort]);

  /** Build distribution of the kept portfolio: { [awayCount]: n } for 5..1. */
  const buildCounts = useMemo(() => {
    const c = {};
    for (const lu of keptLineups) if (lu.build_away != null) c[lu.build_away] = (c[lu.build_away] || 0) + 1;
    return c;
  }, [keptLineups]);

  // ── Exposure drill-down (ported from Optimizer.jsx's classic version) ─────
  // Click an exposure row to show only lineups containing that player; click
  // their CPT% to require them *as captain*. Selection keys:
  //   `${name}_${team}`      -> player in any slot
  //   `${name}_${team}|CPT`  -> player in the CPT slot
  // Multiple selections combine with ALL / ANY (drillMode). Clicking a build
  // chip (5-1 .. 1-5) narrows further to that build (drillBuild = away
  // count, AND-ed with the player keys). Exports keep using the full
  // sortedLineups -- the drill-down is view-only; the ownership filter is the cut.
  const [drillKeys, setDrillKeys] = useState(() => new Set());
  const [drillBuild, setDrillBuild] = useState(null); // away-team count, or null
  const [drillMode, setDrillMode] = useState('all'); // 'all' | 'any'
  const [pairPosFilter, setPairPosFilter] = useState('ALL');
  const baseKey = (pl) => `${pl.name}_${pl.team}`;

  const toggleDrill = (key) => setDrillKeys(prev => {
    const next = new Set(prev);
    if (next.has(key)) next.delete(key);
    else {
      // any-slot and CPT-only for the same player are mutually exclusive
      const base = key.replace(/\|CPT$/, '');
      next.delete(base); next.delete(`${base}|CPT`);
      next.add(key);
    }
    return next;
  });

  // Drop selections for players no longer in the portfolio (new optimize / lab run).
  useEffect(() => {
    setDrillKeys(prev => {
      if (prev.size === 0) return prev;
      const live = new Set(exposures.map(e => e.key));
      const next = new Set([...prev].filter(k => live.has(k.replace(/\|CPT$/, ''))));
      return next.size === prev.size ? prev : next;
    });
  }, [exposures]);
  useEffect(() => { setExpanded(null); }, [drillKeys, drillMode, drillBuild]);
  const drillActive = drillKeys.size > 0 || drillBuild != null;

  /** Does lineup `lu` satisfy one selection key? Input: lineup, key string. Output: bool. */
  const lineupHas = (lu, key) => {
    const cptOnly = key.endsWith('|CPT');
    const base = cptOnly ? key.slice(0, -4) : key;
    return lu.players.some(pl => baseKey(pl) === base && (!cptOnly || pl.slot === 'CPT'));
  };

  /** Lineups shown in the results table -- all of sortedLineups when nothing is
   * selected, else those matching ALL/ANY of drillKeys (and drillBuild, if set). */
  const displayedLineups = useMemo(() => {
    if (!drillActive) return sortedLineups;
    const keys = [...drillKeys];
    return sortedLineups.filter(lu =>
      (drillBuild == null || lu.build_away === drillBuild)
      && (keys.length === 0 || (drillMode === 'all' ? keys.every(k => lineupHas(lu, k)) : keys.some(k => lineupHas(lu, k)))));
  }, [sortedLineups, drillKeys, drillMode, drillBuild, drillActive]); // eslint-disable-line react-hooks/exhaustive-deps

  /** Mean per-lineup metrics over a lineup set (subset vs whole-portfolio compare).
   * Input: lineup array. Output: { n, ev_pct, portfolio_score, itm_pct, top1_pct,
   *   top01_pct, first_pct, total_ownership, projected_score, p95, total_salary } (null if absent).
   * p95 = mean lineup_p95 (sim 95th-percentile lineup score), projection if absent.
   * Salary stays here (unlike classic) -- showdown builds vary a lot in cap usage. */
  const summarizeLineups = (ls) => {
    const avg = (f) => {
      const vals = ls.map(l => l[f]).filter(v => v != null && isFinite(v));
      return vals.length ? vals.reduce((s, v) => s + v, 0) / vals.length : null;
    };
    const out = { n: ls.length };
    ['ev_pct', 'portfolio_score', 'itm_pct', 'top1_pct', 'top01_pct', 'first_pct', 'total_ownership', 'projected_score', 'total_salary']
      .forEach(f => { out[f] = avg(f); });
    const p95s = ls.map(l => l.lineup_p95 ?? l.projected_score).filter(v => v != null && isFinite(v));
    out.p95 = p95s.length ? p95s.reduce((s, v) => s + v, 0) / p95s.length : null;
    return out;
  };
  const portfolioAvg = useMemo(() => summarizeLineups(sortedLineups), [sortedLineups]); // eslint-disable-line react-hooks/exhaustive-deps
  const selectionAvg = useMemo(
    () => (drillActive ? summarizeLineups(displayedLineups) : null),
    [displayedLineups, drillActive] // eslint-disable-line react-hooks/exhaustive-deps
  );

  /** "Most paired with": every non-selected player in the filtered lineups.
   * Output: [{ key, name, pos, team, count, cptCount, pairPct, exposure, lift }],
   *   count desc. pairPct = % of filtered lineups they're in (any slot);
   *   exposure = their Tot% across all lineups; lift = pairPct - exposure (pp). */
  const pairedPlayers = useMemo(() => {
    if (!drillActive || displayedLineups.length === 0) return [];
    const selectedBases = new Set([...drillKeys].map(k => k.replace(/\|CPT$/, '')));
    const expByKey = new Map(exposures.map(e => [e.key, e]));
    const counts = new Map();
    displayedLineups.forEach(lu => lu.players.forEach(pl => {
      const k = baseKey(pl);
      if (selectedBases.has(k)) return;
      const c = counts.get(k) || { count: 0, cptCount: 0 };
      c.count += 1; if (pl.slot === 'CPT') c.cptCount += 1;
      counts.set(k, c);
    }));
    const n = displayedLineups.length;
    return [...counts.entries()].map(([k, c]) => {
      const e = expByKey.get(k) || {};
      const pairPct = (c.count / n) * 100;
      return { key: k, name: e.pos === 'DST' ? `${e.team} DST` : e.name, pos: e.pos, team: e.team, ...c, pairPct, exposure: e.totalPct ?? 0, lift: pairPct - (e.totalPct ?? 0) };
    }).sort((a, b) => b.count - a.count || b.lift - a.lift);
  }, [displayedLineups, drillKeys, drillActive, exposures]); // eslint-disable-line react-hooks/exhaustive-deps

  const toggleResSort = (field) =>
    setResSort(s => (s.field === field ? { field, asc: !s.asc } : { field, asc: field === 'total_ownership' || field === 'dupe_est' || field === 'product_ownership' }));

  // DK's bulk-upload format wants a draftableId (slot-specific — CPT and FLEX
  // ids differ) in each cell: "Name (id)". The backend hands each lineup
  // player the id for its slot as `dk_id`. Previously, if even ONE player
  // across ALL lineups lacked an id, the whole export silently switched to a
  // differently-shaped CSV (extra stat columns, no ids) that DK's upload form
  // can't read -- with no indication anything was wrong. Now the header/shape
  // is always DK's real upload format; a row missing an id falls back to a
  // plain name for just that slot, and the count is surfaced via alert() so a
  // bad upload doesn't happen unnoticed (mirrors Optimizer.jsx's exportDkUploadCSV).
  // Exports sortedLineups = the ownership-filtered (kept) set, NOT the view-only
  // drill-down subset (displayedLineups).
  const exportCSV = () => {
    const nm = p => (p.pos === 'DST' ? `${p.team} DST` : p.name);
    const header = 'CPT,FLEX,FLEX,FLEX,FLEX,FLEX';
    let missingIds = 0;
    const rows = sortedLineups.map(lu => {
      const cpt = lu.players.find(p => p.slot === 'CPT');
      const flex = lu.players.filter(p => p.slot === 'FLEX');
      return [cpt, ...flex].map(p => {
        if (!p) return '';
        if (p.dk_id == null) { missingIds++; return nm(p); }
        return `${nm(p)} (${p.dk_id})`;
      }).join(',');
    });
    if (missingIds > 0) {
      alert(`${missingIds} player slot(s) don't have a live DK ID (not matched to DK's current salary feed) and were exported by name only -- DK's upload will likely reject those rows. Refresh salaries or swap those players before uploading.`);
    }
    const blob = new Blob([[header, ...rows].join('\n')], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url; a.download = `showdown_${gameId || 'lineups'}.csv`; a.click();
    URL.revokeObjectURL(url);
  };

  // ─────────────────────────────────────────────────────────────────────────
  const evColor = (v) => (v > 0 ? '#22c55e' : v < 0 ? '#ef4444' : 'var(--text-muted)');
  const ownColor = (v) => (v <= 90 ? '#22c55e' : v <= 130 ? '#f59e0b' : '#ef4444');

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      {/* Control bar */}
      <div className="glass-panel" style={{
        marginBottom: '18px', padding: '12px 20px', borderRadius: '12px',
        border: '1px solid var(--border-glass)', display: 'flex', gap: '18px',
        alignItems: 'center', flexWrap: 'wrap', justifyContent: 'space-between',
      }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: '16px', flexWrap: 'wrap' }}>
          <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>✧ Showdown Optimizer</span>
          {weeks.length > 0 && (
            <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
              <span style={{ color: 'var(--text-muted)' }}>Week</span>
              <select value={selectedWeek} onChange={e => setSelectedWeek?.(Number(e.target.value))}
                style={{ ...inputStyle, width: 'auto', padding: '4px 8px' }}>
                {weeks.map(w => <option key={w} value={w}>{w}</option>)}
              </select>
            </label>
          )}
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>Game</span>
            <select value={gameId} onChange={e => pickGame(e.target.value)}
              style={{ ...inputStyle, width: 'auto', padding: '4px 8px', minWidth: '160px' }}>
              {simmedGames.length === 0 && <option value="">— no simmed games —</option>}
              {simmedGames.map(g => (
                <option key={g.game_id} value={g.game_id}>{g.away_team} @ {g.home_team}</option>
              ))}
            </select>
          </label>
          {slateKey && (
            <SlotSwitcher slots={workspace.slots} active={workspace.active} saveStatus={workspace.saveStatus}
              onSwitch={workspace.switchSlot} onRename={workspace.renameSlot} onClear={workspace.clearSlot} />
          )}
        </div>
        {view === 'results' && (
          <button onClick={() => setView('pool')} style={{
            padding: '8px 14px', background: 'rgba(255,255,255,0.06)', color: 'var(--text-white)',
            border: '1px solid rgba(255,255,255,0.12)', borderRadius: '8px', cursor: 'pointer', fontWeight: 600,
          }}>← Back to Pool</button>
        )}
      </div>

      {simmedGames.length === 0 && (
        <div style={{ ...cardStyle, textAlign: 'center', color: 'var(--text-muted)', padding: '40px' }}>
          No simulated games this week. Run a game in the <strong>Game Explorer</strong> first — the showdown
          optimizer builds its pool from that game's sim distribution.
        </div>
      )}

      {activeGame && view === 'pool' && (
        <div style={{ display: 'grid', gridTemplateColumns: '280px 1fr', gap: '16px', alignItems: 'start' }}>
          {/* ── Settings sidebar ── */}
          <div style={{ ...cardStyle, position: 'sticky', top: '12px' }}>
            <div style={sectionTitleStyle}>Contest</div>

            {dkContests.length > 0 ? (() => {
              const q = contestFilter.trim().toLowerCase();
              const matches = q
                ? dkContests.filter(c => c.name && c.name.toLowerCase().includes(q))
                : dkContests.slice(0, 150);   // biggest first (feed is pre-sorted by entries)
              return (
                <div style={{ marginBottom: '10px' }}>
                  <label style={labelStyle}>Live DK Contest</label>
                  <input type="text" placeholder="Search contests by name…"
                    value={contestFilter} onChange={e => setContestFilter(e.target.value)}
                    style={{ ...inputStyle, marginBottom: '4px' }} />
                  <select style={inputStyle} value={settings.contest?.dk_contest_id || ''}
                    onChange={e => applyContest(e.target.value)}>
                    <option value="" disabled>
                      {q ? `${matches.length} match${matches.length === 1 ? '' : 'es'}…` : 'Pick a contest to auto-fill…'}
                    </option>
                    {matches.map(c => (
                      <option key={c.contest_id} value={c.contest_id}>
                        {c.name} — ${c.entry_fee}, {Number(c.max_entries || 0).toLocaleString()} max
                      </option>
                    ))}
                  </select>
                  <div style={{ fontSize: '0.7rem', color: contestStatus === 'error' ? '#f59e0b' : 'var(--text-muted)', marginTop: '3px' }}>
                    {contestStatus === 'loading' && 'Fetching payout table…'}
                    {contestStatus === 'ok' && `✓ Entry $, field size & paying spots filled; EV uses this contest's real payout table (${settings.payoutStructure?.length} tiers).`}
                    {contestStatus === 'error' && "Entry $ & field size filled — no cash payout table for this contest, using the type-shaped estimate."}
                    {contestStatus === '' && `${dkContests.length} live contests on this slate.`}
                  </div>
                </div>
              );
            })() : (
              <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginBottom: '10px' }}>
                {contestStatus === 'loading' ? 'Loading DK contests…' : 'No live DK contests for this game yet.'}
              </div>
            )}

            <label style={labelStyle}>Type</label>
            <select style={inputStyle} value={settings.contestType}
              onChange={e => setSettings(s => ({ ...s, contestType: e.target.value, objective: naturalObjective(e.target.value) }))}>
              {CONTEST_TYPES.map(([v, l]) => <option key={v} value={v}>{l}</option>)}
            </select>

            <label style={{ ...labelStyle, marginTop: '10px' }}
              title="What the solver maximises per player. GPP blend = contest-type ceiling blend; Mean = sim mean; Median = sim P50; 95% ceiling = pure p95. Resets to the contest type's default when you change contest type.">
              Objective
            </label>
            <select style={inputStyle} value={settings.objective}
              onChange={e => setSettings(s => ({ ...s, objective: e.target.value }))}>
              {OBJECTIVE_MODES.map(([v, l]) => <option key={v} value={v}>{l}{v === naturalObjective(settings.contestType) ? ' (default)' : ''}</option>)}
            </select>
            <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', marginTop: '3px' }}>
              {settings.objective === 'blend' && 'Ceiling blend weighted by contest type.'}
              {settings.objective === 'ceiling' && 'Pure 95th-percentile — maximum boom.'}
              {settings.objective === 'mean' && 'Sim mean — sits above the median for right-skewed players.'}
              {settings.objective === 'median' && 'Sim P50 — cash-style, no ceiling tilt.'}
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '8px', marginTop: '10px' }}>
              <div><label style={labelStyle}>Entry $</label>
                <input type="number" style={inputStyle} value={settings.entryFee}
                  onChange={e => setSettings(s => ({ ...s, entryFee: +e.target.value }))} /></div>
              <div><label style={labelStyle}>Field size</label>
                <input type="number" style={inputStyle} value={settings.fieldSize}
                  onChange={e => setSettings(s => ({ ...s, fieldSize: +e.target.value }))} /></div>
              <div><label style={labelStyle}>Paying spots</label>
                <input type="number" style={inputStyle} value={settings.payingPositions}
                  onChange={e => setSettings(s => ({ ...s, payingPositions: +e.target.value }))} /></div>
            </div>

            <div style={{ ...sectionTitleStyle, marginTop: '18px' }}>Lineups</div>
            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '8px' }}>
              <div><label style={labelStyle}># Lineups</label>
                <input type="number" min={1} max={1000} style={inputStyle} value={settings.nLineups}
                  onChange={e => setSettings(s => ({ ...s, nLineups: +e.target.value }))} /></div>
              <div><label style={labelStyle}>Min unique</label>
                <input type="number" min={1} max={5} style={inputStyle} value={settings.minUnique}
                  onChange={e => setSettings(s => ({ ...s, minUnique: +e.target.value }))} /></div>
              <div><label style={labelStyle}>Max exp %</label>
                <input type="number" min={5} max={100} style={inputStyle} value={settings.maxExposure}
                  onChange={e => setSettings(s => ({ ...s, maxExposure: +e.target.value }))} /></div>
              <div><label style={labelStyle}>CPT max exp %</label>
                <input type="number" min={5} max={100} style={inputStyle} value={settings.cptMaxExposure}
                  onChange={e => setSettings(s => ({ ...s, cptMaxExposure: +e.target.value }))} /></div>
              <div><label style={labelStyle} title="Every generated lineup must cost at most this much. Can only go below DK's $50,000 cap. The simulated field still builds up to $50k.">Max salary $</label>
                <input type="number" min={1000} max={DK_SALARY_CAP} step={100} style={{
                    ...inputStyle,
                    color: (settings.maxSalary ?? DK_SALARY_CAP) < DK_SALARY_CAP ? '#eab308' : inputStyle.color,
                  }}
                  value={settings.maxSalary ?? DK_SALARY_CAP}
                  onChange={e => setSettings(s => ({ ...s, maxSalary: Math.min(DK_SALARY_CAP, +e.target.value) }))} /></div>
              <div><label style={labelStyle} title="Every generated lineup must cost at least this much (captain counts at 1.5x). 0 = no floor. Capped at Max salary. Too high a floor can make the pool infeasible.">Min salary $</label>
                <input type="number" min={0} max={DK_SALARY_CAP} step={100} style={{
                    ...inputStyle,
                    color: (settings.minSalary ?? 0) > 0 ? '#eab308' : inputStyle.color,
                  }}
                  value={settings.minSalary ?? 0}
                  onChange={e => setSettings(s => ({ ...s, minSalary: Math.max(0, Math.min(DK_SALARY_CAP, +e.target.value || 0)) }))} /></div>
            </div>

            <div style={{ ...sectionTitleStyle, marginTop: '18px' }}
              title="Most defenses / kickers any ONE generated lineup may hold (captain slot counts). Blank = no limit. All three apply together, e.g. DST+K 1 allows a defense or a kicker but never both.">
              DST / K limits (per lineup)
            </div>
            <div style={{ display: 'grid', gridTemplateColumns: 'repeat(3, 1fr)', gap: '8px' }}>
              {[['maxDstK', 'DST + K', 'Most defenses + kickers combined in one lineup.'],
                ['maxDst', 'DST', 'Most defenses in one lineup.'],
                ['maxK', 'K', 'Most kickers in one lineup.']].map(([key, label, tip]) => (
                <div key={key}><label style={labelStyle} title={tip}>Max {label}</label>
                  <input type="number" min={0} max={6} step={1} placeholder="—" style={{
                      ...inputStyle,
                      color: settings[key] !== '' && settings[key] != null ? '#eab308' : inputStyle.color,
                    }}
                    value={settings[key] ?? ''}
                    onChange={e => setSettings(s => ({ ...s, [key]: e.target.value === '' ? '' : Math.max(0, Math.min(6, Math.round(+e.target.value) || 0)) }))} /></div>
              ))}
            </div>

            <div style={{ ...sectionTitleStyle, marginTop: '18px' }}
              title="Minimum share of lineups for each build (players per team, captain included, away team first). Blank = no minimum. Everything not owed to a minimum is left to the optimizer.">
              Build minimums{activeGame ? ` · ${activeGame.away_team}-${activeGame.home_team}` : ''}
            </div>
            <div style={{ display: 'grid', gridTemplateColumns: 'repeat(5, 1fr)', gap: '5px' }}>
              {BUILD_AWAY_COUNTS.map(a => {
                const q = buildMinQuotas.find(x => x.away === a);
                return (
                  <div key={a}>
                    <label style={{ ...labelStyle, textAlign: 'center' }}>{a}-{6 - a}</label>
                    <input type="number" min={0} max={100} step={5} placeholder="—"
                      value={settings.buildMinimums?.[a] ?? ''}
                      onChange={e => setSettings(s => {
                        const bm = { ...(s.buildMinimums || {}) };
                        if (e.target.value === '' || +e.target.value <= 0) delete bm[a];
                        else bm[a] = Math.min(100, +e.target.value);
                        return { ...s, buildMinimums: bm };
                      })}
                      style={{ ...inputStyle, padding: '4px 5px', textAlign: 'center',
                        borderColor: q ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.1)' }} />
                    <div style={{ fontSize: '0.64rem', textAlign: 'center', color: 'var(--text-muted)', minHeight: '0.9rem' }}>
                      {q ? `≥${q.count}` : ''}
                    </div>
                  </div>
                );
              })}
            </div>
            <div style={{ fontSize: '0.7rem', marginTop: '2px', color: buildMinTotal > settings.nLineups ? '#ef4444' : 'var(--text-muted)' }}>
              {buildMinQuotas.length
                ? `% of lineups, rounded up · ${buildMinTotal} of ${settings.nLineups} owed, the rest is up to the math`
                : '% of lineups per build, blank = no minimum'}
            </div>

            <div style={{ ...sectionTitleStyle, marginTop: '18px' }}>Leverage</div>
            <label style={labelStyle}>Ownership penalty λ — {settings.leverageLambda.toFixed(2)}</label>
            <input type="range" min={0} max={1.5} step={0.05} value={settings.leverageLambda}
              style={{ width: '100%' }}
              onChange={e => setSettings(s => ({ ...s, leverageLambda: +e.target.value }))} />
            <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: '4px' }}>
              Scale-free ownership fade in the solver's objective. <strong>0</strong> = pure ceiling ·
              <strong> 0.15–0.4</strong> = lean unique · <strong>1+</strong> = hard fade of chalk.
            </div>

            <button onClick={handleOptimize} disabled={isOptimizing}
              style={{
                marginTop: '18px', width: '100%', padding: '11px', borderRadius: '9px',
                border: '1px solid rgba(0,242,254,0.3)', cursor: isOptimizing ? 'wait' : 'pointer',
                background: 'rgba(0,242,254,0.14)', color: 'var(--accent-primary)', fontWeight: 700, fontSize: '0.95rem',
              }}>
              {isOptimizing ? 'Optimizing…' : '⚡ Optimize'}
            </button>
            {error && <div style={{ color: '#ef4444', fontSize: '0.78rem', marginTop: '8px' }}>{error}</div>}
            <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: '8px' }}>
              {activeCount} active players · CPT = 1.5× salary &amp; points
            </div>
          </div>

          {/* ── Player pool ── */}
          <div style={cardStyle}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '8px', gap: '10px', flexWrap: 'wrap' }}>
              <h2 style={{ margin: 0, fontSize: '1rem' }}>
                {activeGame.away_team} @ {activeGame.home_team} — Player Pool
              </h2>
              <div style={{ display: 'flex', gap: '8px', alignItems: 'center' }}>
                <button onClick={loadDkSalaries} disabled={dkStatus === 'loading'}
                  style={{
                    padding: '6px 12px', borderRadius: '7px', border: '1px solid rgba(0,242,254,0.25)',
                    background: 'rgba(0,242,254,0.1)', color: 'var(--accent-primary)', fontWeight: 600,
                    fontSize: '0.8rem', cursor: dkStatus === 'loading' ? 'wait' : 'pointer',
                  }}>
                  {dkStatus === 'loading' ? 'Loading…' : '⬇ Load DK Salaries'}
                </button>
                <button onClick={ownFrozen ? unfreezeOwnership : freezeOwnership}
                  title="Snapshot every player's current ownership so the EV/portfolio math is stable across optimize runs. Hand-edit individuals as news breaks."
                  style={{
                    padding: '6px 12px', borderRadius: '7px', fontSize: '0.8rem', fontWeight: 600, cursor: 'pointer',
                    border: `1px solid ${ownFrozen ? 'rgba(234,179,8,0.4)' : 'rgba(255,255,255,0.12)'}`,
                    background: ownFrozen ? 'rgba(234,179,8,0.15)' : 'rgba(255,255,255,0.04)',
                    color: ownFrozen ? '#eab308' : 'var(--text-white)',
                  }}>
                  {ownFrozen ? '🔒 Own frozen' : 'Freeze own'}
                </button>
                <button onClick={() => setOwnImportOpen(o => !o)}
                  title="Paste projected ownership from an external tool (Total Own% + CPT Own% per player)"
                  style={{
                    padding: '6px 12px', borderRadius: '7px', fontSize: '0.8rem', fontWeight: 600, cursor: 'pointer',
                    border: '1px solid rgba(255,255,255,0.12)', background: 'rgba(255,255,255,0.04)', color: 'var(--text-white)',
                  }}>
                  ⇩ Import own%
                </button>
                <button onClick={() => setLabOpen(o => !o)}
                  title="Score a hand-built lineup through the same tournament field sim as the optimizer"
                  style={{
                    padding: '6px 12px', borderRadius: '7px', fontSize: '0.8rem', fontWeight: 600, cursor: 'pointer',
                    border: `1px solid ${labOpen ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.12)'}`,
                    background: labOpen ? 'rgba(0,242,254,0.12)' : 'rgba(255,255,255,0.04)',
                    color: labOpen ? 'var(--accent-primary)' : 'var(--text-white)',
                  }}>
                  ✍️ Lineup Lab
                </button>
                <button onClick={() => setGameReadOpen(o => !o)}
                  title="Read this game's outcome distribution and optionally condition the optimizer/lab on a range of it (e.g. a low-total, blowout-favored script)"
                  style={{
                    padding: '6px 12px', borderRadius: '7px', fontSize: '0.8rem', fontWeight: 600, cursor: 'pointer',
                    border: `1px solid ${scenario ? 'rgba(234,179,8,0.45)' : (gameReadOpen ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.12)')}`,
                    background: scenario ? 'rgba(234,179,8,0.14)' : (gameReadOpen ? 'rgba(0,242,254,0.12)' : 'rgba(255,255,255,0.04)'),
                    color: scenario ? '#eab308' : (gameReadOpen ? 'var(--accent-primary)' : 'var(--text-white)'),
                  }}>
                  🎯 Game Read{scenario ? ` · ${(scenario.frac * 100).toFixed(0)}%` : ''}
                </button>
                <input placeholder="Search…" value={search} onChange={e => setSearch(e.target.value)}
                  style={{ ...inputStyle, width: '120px' }} />
              </div>
            </div>
            {ownImportOpen && (
              <div style={{ ...cardStyle, padding: '10px', marginBottom: '8px' }}>
                <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginBottom: '4px' }}>
                  One player per line, ending in <strong>total own%</strong> then <strong>CPT own%</strong>.
                  Pasting rows straight from a projections table works — extra columns are ignored.
                  Stored as FLEX own% = total − CPT, CPT own% = CPT (blue = overrides the model).
                  {' '}<em>Load DK Salaries first if your paste includes kickers.</em>
                  <br />e.g. <code>Jaxon Smith-Njigba  WR  SEA  $10,600  19.3  73.5  20.3</code>
                </div>
                <textarea value={ownImportText} onChange={e => setOwnImportText(e.target.value)}
                  rows={6} placeholder={'Jaxon Smith-Njigba 73.5 20.3\nDrake Maye 67.9 12.6\n…'}
                  style={{ ...inputStyle, width: '100%', fontFamily: 'monospace', fontSize: '0.75rem', resize: 'vertical' }} />
                <div style={{ display: 'flex', gap: '8px', alignItems: 'center', marginTop: '6px' }}>
                  <button onClick={applyOwnershipImport}
                    style={{
                      padding: '6px 14px', borderRadius: '7px', border: '1px solid rgba(0,242,254,0.3)',
                      background: 'rgba(0,242,254,0.14)', color: 'var(--accent-primary)', fontWeight: 700, cursor: 'pointer', fontSize: '0.8rem',
                    }}>Apply</button>
                  {ownImportStatus && (
                    <span style={{ fontSize: '0.74rem', color: ownImportStatus.startsWith('Imported') ? 'var(--accent-primary)' : '#f59e0b' }}>
                      {ownImportStatus}
                    </span>
                  )}
                </div>
              </div>
            )}
            {labOpen && (
              <div style={{ ...cardStyle, padding: '10px', marginBottom: '8px' }}>
                <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginBottom: '6px' }}>
                  Pick a captain + 5 FLEX and score them through the same{' '}
                  <strong>{settings.contestType.replace(/_/g, ' ')}</strong> field sim the optimizer uses — EV%, ITM%, Top 1% / 0.1%, 1st%, dupes.
                  Uses the pool's current projections &amp; ownership (freeze / import them first to pin).
                  {pool.length < 6 && <span style={{ color: '#f59e0b' }}> Load the pool first.</span>}
                </div>
                {labRows.map((row, ri) => {
                  const setRow = patch => setLabRows(rs => rs.map((r, i) => (i === ri ? { ...r, ...patch } : r)));
                  const setFlex = (fi, v) => setRow({ flex: row.flex.map((x, i) => (i === fi ? v : x)) });
                  const chosen = new Set([row.cpt, ...row.flex].filter(Boolean));
                  const sel = (val, onCh, isCpt) => (
                    <select value={val} onChange={e => onCh(e.target.value)}
                      style={{ ...inputStyle, minWidth: '132px', flex: '1 1 132px', fontSize: '0.74rem',
                        borderColor: isCpt && val ? 'rgba(0,242,254,0.5)' : 'rgba(255,255,255,0.14)' }}>
                      <option value="">{isCpt ? 'CPT…' : 'FLEX…'}</option>
                      {labOptions.map(p => {
                        const k = optKey(p);
                        return <option key={k} value={k} disabled={k !== val && chosen.has(k)}>{optLabel(p)}</option>;
                      })}
                    </select>
                  );
                  return (
                    <div key={ri} style={{ display: 'flex', flexWrap: 'wrap', gap: '5px', alignItems: 'center', marginBottom: '6px' }}>
                      <input value={row.label} onChange={e => setRow({ label: e.target.value })} placeholder={`Lab ${ri + 1}`}
                        style={{ ...inputStyle, width: '84px', fontSize: '0.74rem' }} />
                      {sel(row.cpt, v => setRow({ cpt: v }), true)}
                      {row.flex.map((fv, fi) => <Fragment key={fi}>{sel(fv, v => setFlex(fi, v), false)}</Fragment>)}
                      {labRows.length > 1 && (
                        <button onClick={() => setLabRows(rs => rs.filter((_, i) => i !== ri))}
                          style={{ padding: '4px 8px', borderRadius: '6px', border: '1px solid rgba(255,255,255,0.12)', background: 'rgba(255,255,255,0.04)', color: '#ef4444', cursor: 'pointer', fontSize: '0.78rem' }}>✕</button>
                      )}
                    </div>
                  );
                })}
                <div style={{ display: 'flex', gap: '8px', alignItems: 'center', marginTop: '4px' }}>
                  <button onClick={() => setLabRows(rs => [...rs, { label: '', cpt: '', flex: ['', '', '', '', ''] }])}
                    style={{ padding: '6px 10px', borderRadius: '7px', border: '1px solid rgba(255,255,255,0.12)', background: 'rgba(255,255,255,0.04)', color: 'var(--text-white)', cursor: 'pointer', fontSize: '0.78rem' }}>
                    + add lineup
                  </button>
                  <button onClick={scoreLab} disabled={isOptimizing}
                    style={{ padding: '6px 16px', borderRadius: '7px', border: '1px solid rgba(0,242,254,0.3)', background: 'rgba(0,242,254,0.14)', color: 'var(--accent-primary)', fontWeight: 700, cursor: isOptimizing ? 'wait' : 'pointer', fontSize: '0.8rem' }}>
                    {isOptimizing ? 'Scoring…' : 'Score lineup(s) →'}
                  </button>
                </div>
              </div>
            )}
            {gameReadOpen && (
              <div style={{ ...cardStyle, padding: '10px', marginBottom: '8px' }}>
                <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginBottom: '6px' }}>
                  Drag a box on the heatmap, then ⚡ Optimize or the Lab will score only against games that land in it —
                  "given this game plays out this way, which lineups actually hold up." Clear the box to go back to unconditioned.
                </div>
                {gameDist ? (
                  <GameDistribution dist={gameDist} onSelect={setScenario} />
                ) : (
                  <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', padding: '8px' }}>Loading this game's outcome distribution…</div>
                )}
                {scenario && (
                  <div style={{
                    display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginTop: '8px',
                    padding: '8px 12px', borderRadius: '8px', background: 'rgba(234,179,8,0.1)', border: '1px solid rgba(234,179,8,0.3)',
                  }}>
                    <span style={{ fontSize: '0.78rem', color: '#eab308' }}>
                      🎯 Conditioning projections, ⚡ Optimize / Lab on this range — <strong>{(scenario.frac * 100).toFixed(1)}%</strong> of games ({scenario.idx?.length} sims)
                      {prepLoading
                        ? <span style={{ marginLeft: '8px', color: 'var(--text-muted)' }}>· loading scenario projections…</span>
                        : condN != null && <span style={{ marginLeft: '8px', color: 'var(--text-muted)' }}>· pool projections from {condN} sims · ownership unchanged</span>}
                      {!prepLoading && condN != null && condN < 100 && (
                        <span style={{ marginLeft: '8px', color: '#f59e0b' }}>⚠ small sample — P85/ceiling are noisy</span>
                      )}
                    </span>
                    <button onClick={() => setScenario(null)}
                      style={{ padding: '3px 10px', borderRadius: '6px', border: '1px solid rgba(234,179,8,0.4)', background: 'none', color: '#eab308', cursor: 'pointer', fontSize: '0.74rem' }}>
                      clear scenario
                    </button>
                  </div>
                )}
              </div>
            )}
            {dkStatus && dkStatus !== 'loading' && (
              <div style={{ fontSize: '0.74rem', color: dkStatus.startsWith('DK slate') ? 'var(--accent-primary)' : '#f59e0b', marginBottom: '6px' }}>
                {dkStatus}
              </div>
            )}
            {dkExtras.length > 0 && (
              <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginBottom: '6px' }}>
                DK-priced but not in the sim pool (add manually on DK if you want them):{' '}
                {dkExtras.map(x => `${x.name} $${x.salary}`).join(' · ')}
              </div>
            )}
            {pool.some(p => p.salaryEstimated) && (
              <div style={{ fontSize: '0.74rem', color: '#f59e0b', marginBottom: '8px' }}>
                ⚠ Some salaries are estimated (no live DK Showdown price yet) — marked with *. Use <strong>Load DK Salaries</strong>
                {' '}or edit them to the real Showdown numbers before trusting EV.
              </div>
            )}
            <div className="table-container" style={{ maxHeight: '72vh', overflowY: 'auto' }}>
              <table style={{ fontSize: '0.8rem', width: '100%' }}>
                <thead>
                  <tr style={{ textAlign: 'left' }}>
                    <th style={{ padding: '6px 5px' }}>Lock</th>
                    <th style={{ padding: '6px 5px' }}>CPT</th>
                    <th style={{ padding: '6px 5px' }}>✕</th>
                    <th style={{ padding: '6px 5px' }}>Player</th>
                    <th style={{ padding: '6px 5px' }}>Pos</th>
                    <th style={{ padding: '6px 5px' }}>Salary</th>
                    <th style={{ padding: '6px 5px' }}>Proj</th>
                    <th style={{ padding: '6px 5px', whiteSpace: 'nowrap' }} title="Projection per $1,000 of salary. Same at CPT (1.5x points at 1.5x salary).">Pts/$1K</th>
                    <th style={{ padding: '6px 5px' }} title="Solver-objective points under the current Objective setting (GPP blend / 95% ceiling / median). This is what the ILP maximises.">Obj</th>
                    <th style={{ padding: '6px 5px' }} title="Captain scoring = 1.5× projection">CPT Proj</th>
                    <th style={{ padding: '6px 5px' }}>Ceil</th>
                    <th style={{ padding: '6px 5px' }} title="Projected FLEX ownership %. Grey = model, blue = your override. Load DK Salaries to compute.">FLEX Own%</th>
                    <th style={{ padding: '6px 5px' }} title="Projected captain ownership %. Grey = model, blue = your override.">CPT Own%</th>
                    <th style={{ padding: '6px 5px' }} title="Sim's optimal-flex rate — a driver of the FLEX ownership model">Opt FLEX%</th>
                    <th style={{ padding: '6px 5px' }} title="Sim's optimal-captain rate — a driver of the CPT ownership model">Opt CPT%</th>
                    <th style={{ padding: '6px 5px' }} title="Opt FLEX% minus FLEX Own% — positive means the sim likes this player as a flex more than the field will roster them">FLEX Lev.</th>
                    <th style={{ padding: '6px 5px' }} title="Opt CPT% minus CPT Own% — positive means the sim likes this player as captain more than the field will">CPT Lev.</th>
                    <th style={{ padding: '6px 5px' }} title="Eligible to be considered as captain by the optimizer">CPT Pool</th>
                  </tr>
                </thead>
                <tbody>
                  {pool.filter(p => !search || p.name.toLowerCase().includes(search.toLowerCase())).map(p => {
                    const ro = ownOf(p);
                    return (
                    <tr key={p.id} style={{
                      borderBottom: '1px solid rgba(255,255,255,0.04)',
                      opacity: p.excluded ? 0.35 : 1,
                    }}>
                      <td style={{ padding: '4px 5px' }}>
                        <button onClick={() => patchPlayer(p.id, { locked: !p.locked, lockedCpt: p.locked ? p.lockedCpt : false })}
                          title="Lock into every lineup"
                          style={{
                            border: 'none', cursor: 'pointer', borderRadius: '4px', padding: '2px 6px',
                            background: p.locked ? 'rgba(234,179,8,0.25)' : 'rgba(255,255,255,0.06)',
                            color: p.locked ? '#eab308' : 'var(--text-muted)',
                          }}>🔒</button>
                      </td>
                      <td style={{ padding: '4px 5px' }}>
                        <button onClick={() => patchPlayer(p.id, { lockedCpt: !p.lockedCpt, locked: !p.lockedCpt ? true : p.locked })}
                          title="Lock in specifically as the captain"
                          style={{
                            border: 'none', cursor: 'pointer', borderRadius: '4px', padding: '2px 6px',
                            background: p.lockedCpt ? 'rgba(0,242,254,0.22)' : 'rgba(255,255,255,0.06)',
                            color: p.lockedCpt ? 'var(--accent-primary)' : 'var(--text-muted)', fontWeight: 700,
                          }}>C</button>
                      </td>
                      <td style={{ padding: '4px 5px' }}>
                        <button onClick={() => patchPlayer(p.id, { excluded: !p.excluded })}
                          style={{
                            border: 'none', cursor: 'pointer', borderRadius: '4px', padding: '2px 6px',
                            background: 'rgba(255,255,255,0.06)', color: p.excluded ? '#22c55e' : '#ef4444',
                          }}>{p.excluded ? '+' : '✕'}</button>
                      </td>
                      <td style={{ padding: '4px 5px', whiteSpace: 'nowrap' }}>
                        <span style={{ display: 'inline-block', width: '7px', height: '7px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '6px' }} />
                        {p.name} <span style={{ color: 'var(--text-muted)', fontSize: '0.72rem' }}>{p.team}</span>
                      </td>
                      <td style={{ padding: '4px 5px', color: POS_COLORS[p.pos], fontWeight: 700 }}>{p.pos}</td>
                      <td style={{ padding: '4px 5px', whiteSpace: 'nowrap' }}>
                        <input type="number" step={100} value={p.salary}
                          onChange={e => patchPlayer(p.id, { salary: +e.target.value, salaryEstimated: false })}
                          style={{ ...inputStyle, width: '72px', padding: '3px 5px' }} />
                        {p.salaryEstimated && <span style={{ color: '#f59e0b' }}>*</span>}
                      </td>
                      <td style={{ padding: '4px 5px' }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: '3px' }}>
                          <input type="number" step={0.5} value={p.projection}
                            onChange={e => patchPlayer(p.id, { projDelta: +e.target.value - (p.simProjection ?? 0), projection: undefined })}
                            style={{
                              ...inputStyle, width: '58px', padding: '3px 5px',
                              background: p.projection > p.simProjection + 0.05 ? 'rgba(245,158,11,0.15)' : inputStyle.background,
                            }} />
                          {p.simProjection != null && Math.abs((p.projection ?? 0) - p.simProjection) > 0.05 && (
                            <button
                              onClick={() => patchPlayer(p.id, { projDelta: 0, projection: undefined })}
                              title={`Reset to ${p.conditioned ? 'scenario' : 'sim'} projection (${p.simProjection})`}
                              style={{
                                border: 'none', cursor: 'pointer', borderRadius: '4px', padding: '2px 4px',
                                background: 'rgba(255,255,255,0.06)', color: 'var(--text-muted)', fontSize: '0.7rem',
                              }}>↺</button>
                          )}
                        </div>
                      </td>
                      <td style={{ padding: '4px 5px', fontWeight: 600, color: 'var(--text-main)' }}>
                        {p.salary > 0 && p.projection != null ? (p.projection / (p.salary / 1000)).toFixed(2) : '—'}
                      </td>
                      <td style={{
                        padding: '4px 5px', fontWeight: 600,
                        color: settings.objective === 'median' ? 'var(--text-muted)' : 'var(--accent-primary)',
                      }}>
                        {round1(objectiveProjection(p, settings.objective, settings.contestType) ?? p.projection)}
                      </td>
                      <td style={{ padding: '4px 5px', color: 'var(--text-muted)' }}>{round1(p.projection * 1.5)}</td>
                      <td style={{ padding: '4px 5px', color: 'var(--text-muted)' }}>{p.ceiling ?? '—'}</td>
                      <td style={{ padding: '4px 5px' }}>
                        <input type="number" step={1} placeholder="model"
                          value={p.ownFlex ?? (ro.flex != null ? round1(ro.flex) : '')}
                          onChange={e => patchPlayer(p.id, { ownFlex: e.target.value === '' ? null : +e.target.value })}
                          style={{
                            ...inputStyle, width: '54px', padding: '3px 5px',
                            color: p.ownFlex != null ? 'var(--accent-primary)'
                              : (ownFrozen ? '#eab308' : 'var(--text-muted)'),
                          }} />
                      </td>
                      <td style={{ padding: '4px 5px' }}>
                        <input type="number" step={1} placeholder="model"
                          value={p.ownCpt ?? (ro.cpt != null ? round1(ro.cpt) : '')}
                          onChange={e => patchPlayer(p.id, { ownCpt: e.target.value === '' ? null : +e.target.value })}
                          style={{
                            ...inputStyle, width: '54px', padding: '3px 5px',
                            color: p.ownCpt != null ? 'var(--accent-primary)'
                              : (ownFrozen ? '#eab308' : 'var(--text-muted)'),
                          }} />
                      </td>
                      <td style={{ padding: '4px 5px', color: 'var(--text-muted)' }}>{p.simFlexRate != null ? `${round1(p.simFlexRate)}%` : '—'}</td>
                      <td style={{ padding: '4px 5px', color: 'var(--text-muted)' }}>{p.simCptRate != null ? `${round1(p.simCptRate)}%` : '—'}</td>
                      <td style={{ padding: '4px 5px', fontWeight: 600 }}>
                        {p.simFlexRate != null && ro.flex != null ? (
                          <span style={{ color: (p.simFlexRate - ro.flex) > 0 ? 'var(--accent-green)' : '#ef4444' }}>
                            {(p.simFlexRate - ro.flex) > 0 ? '+' : ''}{round1(p.simFlexRate - ro.flex)}%
                          </span>
                        ) : '—'}
                      </td>
                      <td style={{ padding: '4px 5px', fontWeight: 600 }}>
                        {p.simCptRate != null && ro.cpt != null ? (
                          <span style={{ color: (p.simCptRate - ro.cpt) > 0 ? 'var(--accent-green)' : '#ef4444' }}>
                            {(p.simCptRate - ro.cpt) > 0 ? '+' : ''}{round1(p.simCptRate - ro.cpt)}%
                          </span>
                        ) : '—'}
                      </td>
                      <td style={{ padding: '4px 5px', textAlign: 'center' }}>
                        <button onClick={() => patchPlayer(p.id, p.cptEligible === false
                            ? { cptEligible: true }
                            : { cptEligible: false, lockedCpt: false })}
                          title={p.cptEligible === false ? 'Excluded from captain pool — click to re-include' : 'Eligible for captain — click to exclude from captain pool'}
                          style={{
                            border: 'none', cursor: 'pointer', borderRadius: '4px', padding: '2px 6px',
                            background: p.cptEligible === false ? 'rgba(239,68,68,0.15)' : 'rgba(255,255,255,0.06)',
                            color: p.cptEligible === false ? '#ef4444' : 'var(--text-muted)',
                          }}>{p.cptEligible === false ? '✕' : '✓'}</button>
                      </td>
                    </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      )}

      {view === 'results' && (
        <div style={{ display: 'flex', flexDirection: 'column', gap: '14px' }}>
          {notes.map((n, i) => (
            <div key={i} style={{ ...cardStyle, borderColor: 'rgba(245,158,11,0.3)', fontSize: '0.82rem', color: '#f59e0b' }}>ℹ {n}</div>
          ))}
          {portfolio && (
            <div style={{ ...cardStyle, borderColor: 'rgba(0,242,254,0.15)' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '12px', flexWrap: 'wrap', gap: '10px' }}>
                <div>
                  <h2 style={{ margin: 0, fontSize: '1rem' }}>
                    {resultsMode === 'lab' ? '✍️ Lineup Lab' : 'Portfolio Summary'}
                    {viewPortfolio.iteration_filter_frac != null && (
                      <span style={{ fontSize: '0.68rem', fontWeight: 700, color: '#eab308', background: 'rgba(234,179,8,0.14)', border: '1px solid rgba(234,179,8,0.3)', borderRadius: '20px', padding: '2px 9px', marginLeft: '8px', verticalAlign: 'middle' }}
                        title={`Scored only against the ${viewPortfolio.iteration_filter_n} sims from the Game Read box-select, not the full field`}>
                        🎯 conditioned · {(viewPortfolio.iteration_filter_frac * 100).toFixed(1)}% of games
                      </span>
                    )}
                  </h2>
                  <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginTop: '2px' }}>
                    {resultsMode === 'lab'
                      ? `${viewPortfolio.n_generated} hand-built lineup${viewPortfolio.n_generated === 1 ? '' : 's'} scored`
                      : filterActive
                        ? `${keptLineups.length} of ${lineups.length} lineups pass the ownership filter`
                        : `${viewPortfolio.n_generated} lineups${viewPortfolio.n_generated < viewPortfolio.n_requested ? ` (${viewPortfolio.n_requested} requested)` : ''}`}
                    {viewPortfolio.field_source && (
                      <span title="Sim slice the EV / Top% / 1st% were scored against">
                        {' · '}field: {viewPortfolio.field_source === 'independent_draws'
                          ? 'independent draws (rough)'
                          : viewPortfolio.field_source.replace('_', '-')}
                        {viewPortfolio.n_sims ? ` · ${(viewPortfolio.n_sims / 1000).toFixed(0)}k sims` : ''}
                      </span>
                    )}
                  </div>
                </div>
                <button onClick={exportCSV} disabled={!keptLineups.length}
                  title={filterActive ? 'Exports only the lineups that pass the ownership filter (the player/build drill-down is ignored)' : 'Exports every generated lineup (the player/build drill-down is ignored)'}
                  style={{
                    padding: '7px 14px', background: 'rgba(0,242,254,0.1)', color: 'var(--accent-primary)',
                    border: '1px solid rgba(0,242,254,0.25)', borderRadius: '8px', cursor: keptLineups.length ? 'pointer' : 'not-allowed', fontWeight: 600, fontSize: '0.82rem',
                  }}>↓ Export CSV{filterActive ? ` (${keptLineups.length} of ${lineups.length})` : ''}</button>
              </div>
              <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(112px, 1fr))', gap: '10px' }}>
                {[
                  { label: 'Portfolio EV%', value: `${viewPortfolio.total_ev_pct > 0 ? '+' : ''}${viewPortfolio.total_ev_pct}%`, color: evColor(viewPortfolio.total_ev_pct) },
                  { label: 'Avg Total Own%', value: `${viewPortfolio.avg_total_ownership}%`, color: ownColor(viewPortfolio.avg_total_ownership) },
                  { label: 'Unique Captains', value: viewPortfolio.unique_captains, color: 'var(--accent-primary)' },
                  { label: 'Eff. Lineups', value: `${viewPortfolio.effective_lineup_count}/${viewPortfolio.n_generated}`, color: 'var(--text-white)' },
                  { label: 'Coverage', value: viewPortfolio.coverage_score?.toFixed(2), color: 'var(--text-white)' },
                  { label: 'Top 1% avg', value: `${viewPortfolio.avg_top1_pct ?? '—'}%`, color: 'var(--text-white)', title: `best lineup ${viewPortfolio.best_top1_pct}%` },
                  { label: 'Top 0.1% avg', value: `${viewPortfolio.avg_top01_pct ?? '—'}%`, color: 'var(--text-white)', title: `best lineup ${viewPortfolio.best_top01_pct}%` },
                  { label: '1st place best', value: `${viewPortfolio.best_first_pct ?? '—'}%`, color: 'var(--accent-primary)', title: `portfolio avg ${viewPortfolio.avg_first_pct}% · share of sims a lineup beats the whole synthetic field` },
                ].map(({ label, value, color, title }) => (
                  <div key={label} title={title} style={{ background: 'rgba(255,255,255,0.03)', borderRadius: '8px', padding: '10px', textAlign: 'center' }}>
                    <div style={{ fontSize: '0.66rem', color: 'var(--text-muted)', marginBottom: '4px', textTransform: 'uppercase', letterSpacing: '0.05em' }}>{label}</div>
                    <div style={{ fontSize: '1.25rem', fontWeight: 700, color }}>{value}</div>
                  </div>
                ))}
              </div>

              <div style={{ display: 'flex', gap: '8px', alignItems: 'center', flexWrap: 'wrap', marginTop: '12px', paddingTop: '12px', borderTop: '1px solid rgba(255,255,255,0.06)' }}>
                <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontWeight: 600, whiteSpace: 'nowrap' }}>📝 Paper trade into</span>
                <input placeholder="contest name" value={paperDefaults.contestName}
                  onChange={e => setPaperDefaults(d => ({ ...d, contestName: e.target.value }))}
                  title="Use the same name you'll give the standings CSV later (<name>_<price>_<xmax>max.csv)"
                  style={{ ...inputStyle, width: '110px', fontSize: '0.74rem', padding: '4px 8px' }} />
                <input type="number" value={paperDefaults.entryFee}
                  onChange={e => setPaperDefaults(d => ({ ...d, entryFee: +e.target.value }))}
                  title="Entry fee" style={{ ...inputStyle, width: '55px', fontSize: '0.74rem', padding: '4px 8px' }} />
                <input type="number" value={paperDefaults.maxEntries}
                  onChange={e => setPaperDefaults(d => ({ ...d, maxEntries: +e.target.value }))}
                  title="Max entries per user" style={{ ...inputStyle, width: '45px', fontSize: '0.74rem', padding: '4px 8px' }} />
                <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)' }}>then 📝 on any lineup below</span>
                {paperStatus && <span style={{ fontSize: '0.7rem', color: paperStatus.startsWith('⚠') || paperStatus.startsWith('Save failed') ? '#f59e0b' : 'var(--accent-primary)' }}>{paperStatus}</span>}
              </div>
            </div>
          )}

          <div style={{ display: 'grid', gridTemplateColumns: '1fr 320px', gap: '14px', alignItems: 'start' }}>
            <div style={cardStyle}>
              <h2 style={{ margin: '0 0 10px 0', fontSize: '1rem' }}>
                Generated Lineups ({selectionAvg ? `${displayedLineups.length} of ${sortedLineups.length}` : sortedLineups.length})
              </h2>

              {/* Ownership cut (real portfolio cut: table, exposures, tiles, export) */}
              {(() => {
                const numIn = (field, width, placeholder, title) => (
                  <input type="number" min={0} value={lineupFilter[field] ?? ''} placeholder={placeholder} title={title}
                    onChange={e => setLineupFilter(f => ({ ...f, [field]: e.target.value === '' ? null : +e.target.value }))}
                    style={{ ...inputStyle, width, padding: '3px 6px', fontSize: '0.74rem',
                      borderColor: lineupFilter[field] != null ? 'rgba(234,179,8,0.5)' : 'rgba(255,255,255,0.1)' }} />
                );
                const lbl = { fontSize: '0.7rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' };
                return (
                  <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '8px 14px', marginBottom: '10px', padding: '8px 10px', borderRadius: '8px',
                    background: filterActive ? 'rgba(234,179,8,0.06)' : 'rgba(255,255,255,0.02)',
                    border: `1px solid ${filterActive ? 'rgba(234,179,8,0.25)' : 'rgba(255,255,255,0.06)'}` }}>
                    <span style={{ fontSize: '0.68rem', fontWeight: 700, color: filterActive ? '#eab308' : 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em' }}>Ownership cut</span>
                    <span style={{ display: 'inline-flex', alignItems: 'center', gap: '5px' }}>
                      <span style={lbl}>Sum own ≤</span>
                      {numIn('maxSumOwn', '64px', 'any', 'Max summed ownership % (CPT own + 5 FLEX own)')}
                      <span style={lbl}>%</span>
                    </span>
                    <span style={{ display: 'inline-flex', alignItems: 'center', gap: '5px' }}>
                      <span style={lbl}>Product own rarer than 1 in</span>
                      {numIn('minOneIn', '96px', 'any', 'Keep lineups whose product ownership (chance a random field entry is this exact lineup) is at most 1 / this number. e.g. 100000 keeps lineups rarer than 1 in 100K.')}
                    </span>
                    <span style={{ display: 'inline-flex', alignItems: 'center', gap: '5px' }}>
                      <span style={lbl}>Dupes ≤</span>
                      {numIn('maxDupe', '60px', 'any', 'Max estimated duplicates (field size × product ownership). Assumes the field picks players independently, so it undercounts chalky lineups -- best for ranking, not as an absolute count.')}
                    </span>
                    {filterActive && (
                      <>
                        <span style={{ fontSize: '0.72rem', color: keptLineups.length ? '#eab308' : '#ef4444', fontWeight: 600 }}>
                          {keptLineups.length} of {lineups.length} pass
                        </span>
                        <button onClick={() => setLineupFilter(DEFAULT_LINEUP_FILTER)}
                          style={{ marginLeft: 'auto', padding: '2px 9px', borderRadius: '20px', border: '1px solid rgba(255,255,255,0.12)', background: 'transparent', color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.7rem' }}>
                          Clear cut
                        </button>
                      </>
                    )}
                  </div>
                );
              })()}

              {/* Build breakdown of the kept portfolio -- click a build to drill into it */}
              {awayTeam && keptLineups.length > 0 && (
                <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '6px', marginBottom: '10px' }}>
                  <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em', marginRight: '2px' }}
                    title="Players per team in each lineup, captain included, away team first">
                    Builds · {awayTeam}-{homeTeam}
                  </span>
                  {BUILD_AWAY_COUNTS.map(a => {
                    const c = buildCounts[a] || 0;
                    const on = drillBuild === a;
                    // Minimum this run was generated with (backend report); after an
                    // ownership cut the kept count can fall below it -- shown in amber.
                    const req = (portfolio?.build_minimums || []).find(b => b.away_count === a)?.required;
                    return (
                      <button key={a} disabled={!c && !on} onClick={() => setDrillBuild(on ? null : a)}
                        title={`${a} ${awayTeam} + ${6 - a} ${homeTeam}: ${c} of ${keptLineups.length} lineups. Click to show only these.`}
                        style={{
                          padding: '2px 9px', borderRadius: '20px', fontSize: '0.72rem', fontWeight: 600, border: '1px solid',
                          cursor: c || on ? 'pointer' : 'default', opacity: c || on ? 1 : 0.35,
                          background: on ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.03)',
                          color: on ? 'var(--accent-primary)' : 'var(--text-white)',
                          borderColor: on ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.08)',
                        }}>
                        {a}-{6 - a} <span style={{ color: 'var(--text-muted)', fontWeight: 500 }}>{c} · {Math.round((c / keptLineups.length) * 100)}%</span>
                        {req != null && (
                          <span title={`Minimum requested: ${req}`} style={{ marginLeft: '4px', fontSize: '0.64rem', color: c >= req ? '#22c55e' : '#f59e0b' }}>
                            min {req}
                          </span>
                        )}
                      </button>
                    );
                  })}
                </div>
              )}

              {/* Exposure drill-down: selected players, subset-vs-portfolio averages, top pairings */}
              {selectionAvg && (() => {
                const pill = (active) => ({
                  padding: '1px 7px', borderRadius: '20px', cursor: 'pointer', fontSize: '0.66rem', fontWeight: 600, border: '1px solid',
                  background: active ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.03)',
                  color: active ? 'var(--accent-primary)' : 'var(--text-muted)',
                  borderColor: active ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.08)',
                });
                const expByKey = new Map(exposures.map(e => [e.key, e]));
                const chips = [...drillKeys].map(k => {
                  const cptOnly = k.endsWith('|CPT');
                  const e = expByKey.get(cptOnly ? k.slice(0, -4) : k) || {};
                  return { k, cptOnly, e };
                });
                const POS_ORDER = ['QB', 'RB', 'WR', 'TE', 'K', 'DST'].filter(pos => pairedPlayers.some(p => p.pos === pos));
                const leaders = POS_ORDER.map(pos => pairedPlayers.find(p => p.pos === pos)).filter(Boolean);
                const list = (pairPosFilter === 'ALL' ? pairedPlayers : pairedPlayers.filter(p => p.pos === pairPosFilter)).slice(0, 8);
                const stats = [
                  { label: 'Lineups', v: selectionAvg.n, base: portfolioAvg.n, fmt: v => `${v}`, baseFmt: v => `of ${v}` },
                  { label: 'Avg EV%', v: selectionAvg.ev_pct, base: portfolioAvg.ev_pct, fmt: v => `${v > 0 ? '+' : ''}${v.toFixed(2)}%`, color: evColor },
                  { label: 'Avg Port.Score', v: selectionAvg.portfolio_score, base: portfolioAvg.portfolio_score, fmt: v => v.toFixed(2) },
                  { label: 'Avg ITM%', v: selectionAvg.itm_pct, base: portfolioAvg.itm_pct, fmt: v => `${v.toFixed(2)}%` },
                  { label: 'Avg Top1%', v: selectionAvg.top1_pct, base: portfolioAvg.top1_pct, fmt: v => `${v.toFixed(2)}%` },
                  { label: 'Avg Top.1%', v: selectionAvg.top01_pct, base: portfolioAvg.top01_pct, fmt: v => `${v.toFixed(2)}%` },
                  { label: 'Avg 1st%', v: selectionAvg.first_pct, base: portfolioAvg.first_pct, fmt: v => `${v.toFixed(2)}%` },
                  { label: 'Avg Own%', v: selectionAvg.total_ownership, base: portfolioAvg.total_ownership, fmt: v => `${v.toFixed(1)}%`, color: ownColor },
                  { label: 'Avg Proj', v: selectionAvg.projected_score, base: portfolioAvg.projected_score, fmt: v => v.toFixed(1) },
                  { label: 'Avg P95', v: selectionAvg.p95, base: portfolioAvg.p95, fmt: v => v.toFixed(1) },
                  { label: 'Avg Salary', v: selectionAvg.total_salary, base: portfolioAvg.total_salary, fmt: v => `$${Math.round(v).toLocaleString()}` },
                ].filter(s => s.v != null);
                return (
                  <div style={{ background: 'rgba(0,242,254,0.04)', border: '1px solid rgba(0,242,254,0.18)', borderRadius: '8px', padding: '10px', marginBottom: '10px' }}>
                    <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '6px', marginBottom: '8px' }}>
                      <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>Lineups with</span>
                      {drillKeys.size > 1 && ['all', 'any'].map(m => (
                        <button key={m} onClick={() => setDrillMode(m)} style={pill(drillMode === m)}>{m.toUpperCase()} of</button>
                      ))}
                      {chips.map(({ k, cptOnly, e }) => (
                        <span key={k} style={{ display: 'inline-flex', alignItems: 'center', gap: '5px', fontSize: '0.74rem', fontWeight: 600, padding: '2px 4px 2px 8px', borderRadius: '20px', background: (POS_COLORS[e.pos] || '#888') + '1f', border: `1px solid ${(POS_COLORS[e.pos] || '#888')}55` }}>
                          {cptOnly && <span style={{ color: '#00f2fe', fontSize: '0.64rem', fontWeight: 800 }}>CPT</span>}
                          {e.pos === 'DST' ? `${e.team} DST` : e.name}
                          <span style={{ color: 'var(--text-muted)', fontWeight: 500 }}>{(cptOnly ? e.cptPct : e.totalPct)?.toFixed(0)}%</span>
                          <button onClick={() => toggleDrill(k)} title="Remove"
                            style={{ border: 'none', background: 'transparent', color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.72rem', padding: '0 3px' }}>✕</button>
                        </span>
                      ))}
                      {drillBuild != null && (
                        <span style={{ display: 'inline-flex', alignItems: 'center', gap: '5px', fontSize: '0.74rem', fontWeight: 600, padding: '2px 4px 2px 8px', borderRadius: '20px', background: 'rgba(0,242,254,0.1)', border: '1px solid rgba(0,242,254,0.35)' }}>
                          {drillKeys.size > 0 && <span style={{ color: 'var(--text-muted)', fontWeight: 500 }}>in</span>}
                          {drillBuild}-{6 - drillBuild} build
                          <span style={{ color: 'var(--text-muted)', fontWeight: 500 }}>{awayTeam}-{homeTeam}</span>
                          <button onClick={() => setDrillBuild(null)} title="Remove"
                            style={{ border: 'none', background: 'transparent', color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.72rem', padding: '0 3px' }}>✕</button>
                        </span>
                      )}
                      <button onClick={() => { setDrillKeys(new Set()); setDrillBuild(null); }} style={{ ...pill(false), marginLeft: 'auto', padding: '2px 9px', fontSize: '0.7rem' }}>Clear filter</button>
                    </div>

                    {selectionAvg.n === 0 ? (
                      <div style={{ fontSize: '0.76rem', color: 'var(--text-muted)' }}>No lineups match this combination.</div>
                    ) : (
                      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(92px, 1fr))', gap: '6px' }}>
                        {stats.map(s => (
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

                    {pairedPlayers.length > 0 && (
                      <div style={{ marginTop: '10px', borderTop: '1px solid rgba(255,255,255,0.06)', paddingTop: '8px' }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: '6px', marginBottom: '6px' }}>
                          <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em' }}>Most paired with</span>
                          <span style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>· click to add to filter</span>
                        </div>
                        <div style={{ display: 'flex', flexWrap: 'wrap', gap: '5px', marginBottom: '8px' }}>
                          {leaders.map(p => (
                            <button key={p.key} onClick={() => toggleDrill(p.key)}
                              title={`${p.name}: in ${p.count}/${displayedLineups.length} of these lineups${p.cptCount ? ` (${p.cptCount} as CPT)` : ''}, ${p.exposure.toFixed(1)}% across the whole portfolio`}
                              style={{ display: 'inline-flex', gap: '5px', alignItems: 'center', padding: '2px 8px', borderRadius: '20px', cursor: 'pointer', fontSize: '0.72rem', background: 'rgba(255,255,255,0.03)', color: 'var(--text-white)', border: `1px solid ${(POS_COLORS[p.pos] || '#888')}55` }}>
                              <span style={{ color: POS_COLORS[p.pos] || '#888', fontWeight: 700, fontSize: '0.64rem' }}>{p.pos}</span>
                              <span style={{ fontWeight: 600 }}>{p.name}</span>
                              <span style={{ color: 'var(--accent-primary)', fontWeight: 700 }}>{p.pairPct.toFixed(0)}%</span>
                            </button>
                          ))}
                        </div>
                        <div style={{ display: 'flex', gap: '3px', marginBottom: '6px' }}>
                          {['ALL', ...POS_ORDER].map(pos => (
                            <button key={pos} onClick={() => setPairPosFilter(pos)} style={pill(pairPosFilter === pos)}>{pos}</button>
                          ))}
                        </div>
                        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(260px, 1fr))', gap: '4px 12px' }}>
                          {list.map(p => (
                            <div key={p.key} onClick={() => toggleDrill(p.key)}
                              title={`In ${p.count}/${displayedLineups.length} of these lineups vs ${p.exposure.toFixed(1)}% of all lineups. Click to add to filter.`}
                              style={{ display: 'grid', gridTemplateColumns: '30px 1fr 90px 44px 30px', alignItems: 'center', gap: '6px', fontSize: '0.74rem', cursor: 'pointer', padding: '2px 0' }}>
                              <span style={{ fontSize: '0.62rem', fontWeight: 700, color: POS_COLORS[p.pos] || '#888' }}>{p.pos}</span>
                              <span style={{ whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis' }}>
                                <span style={{ fontWeight: 600, color: 'var(--text-white)' }}>{p.name}</span>{' '}
                                <span style={{ color: 'var(--text-muted)', fontSize: '0.66rem' }}>{p.team}</span>
                              </span>
                              {/* Bar = share of the filtered lineups; white tick = whole-portfolio Tot% */}
                              <div style={{ position: 'relative', height: '12px', background: 'rgba(255,255,255,0.04)', borderRadius: '3px', overflow: 'hidden' }}>
                                <div style={{ position: 'absolute', left: 0, top: 0, bottom: 0, width: `${p.pairPct}%`, background: (POS_COLORS[p.pos] || '#888') + 'aa', borderRadius: '3px' }} />
                                <div style={{ position: 'absolute', left: `${Math.min(p.exposure, 99.5)}%`, top: 0, bottom: 0, width: '2px', background: 'rgba(255,255,255,0.75)' }} />
                                <div style={{ position: 'absolute', inset: 0, display: 'flex', alignItems: 'center', justifyContent: 'center', fontSize: '0.64rem', fontWeight: 700, color: 'var(--text-white)', textShadow: '0 1px 2px rgba(0,0,0,0.8)' }}>
                                  {p.pairPct.toFixed(0)}% ({p.count})
                                </div>
                              </div>
                              <span title="How many of those pairings had this player at captain" style={{ fontSize: '0.64rem', color: p.cptCount ? '#00f2fe' : 'rgba(255,255,255,0.2)' }}>
                                CPT {p.cptCount}
                              </span>
                              <span title="Percentage points above/below their whole-portfolio exposure"
                                style={{ fontSize: '0.68rem', fontWeight: 700, textAlign: 'right', color: p.lift > 0 ? '#22c55e' : p.lift < 0 ? '#ef4444' : 'var(--text-muted)' }}>
                                {p.lift > 0 ? '+' : ''}{p.lift.toFixed(0)}
                              </span>
                            </div>
                          ))}
                        </div>
                      </div>
                    )}
                  </div>
                );
              })()}
              <div className="table-container" style={{ maxHeight: '72vh', overflowY: 'auto' }}>
                <table style={{ fontSize: '0.78rem', width: '100%' }}>
                  <thead>
                    <tr style={{ textAlign: 'left' }}>
                      <th style={{ padding: '6px 5px' }}>#</th>
                      <th style={{ padding: '6px 5px' }}>CPT</th>
                      <th style={{ padding: '6px 5px' }}>FLEX ×5</th>
                      {[
                        ['build_away', 'Build'], ['total_ownership', 'Own%'], ['product_ownership', 'Prod Own'],
                        ['projected_score', 'Proj'], ['ev_pct', 'EV%'],
                        ['itm_pct', 'ITM%'], ['top1_pct', 'Top1%'], ['top01_pct', 'Top.1%'],
                        ['first_pct', '1st%'], ['dupe_est', 'Dupe~'], ['total_salary', 'Salary'],
                      ].map(([f, l]) => (
                        <th key={f} onClick={() => toggleResSort(f)} style={{ padding: '6px 5px', cursor: 'pointer', whiteSpace: 'nowrap' }}
                          title={f === 'first_pct'
                            ? 'Share of sims this lineup outscored every build in the 1,200-lineup synthetic field — "best possible build" rate, not contest win %'
                            : f === 'top1_pct' ? 'Share of sims this lineup lands in the field\'s top 1%'
                            : f === 'top01_pct' ? 'Share of sims this lineup lands in the field\'s top 0.1%'
                            : f === 'build_away' ? `Players per team, captain included, ${awayTeam || 'away'} first`
                            : f === 'product_ownership' ? 'Product ownership: chance a random field entry is this exact lineup (CPT own × 5 FLEX owns). Rarer = less dupe risk.'
                            : undefined}>
                          {l} {resSort.field === f ? (resSort.asc ? '↑' : '↓') : ''}
                        </th>
                      ))}
                      <th style={{ padding: '6px 5px' }} title="Flag as a paper-trade entry">📝</th>
                    </tr>
                  </thead>
                  <tbody>
                    {selectionAvg && displayedLineups.length === 0 && (
                      <tr><td colSpan={15} style={{ textAlign: 'center', padding: '24px', color: 'var(--text-muted)' }}>No lineups match the selected players / build.</td></tr>
                    )}
                    {displayedLineups.map((lu, idx) => {
                      const cpt = lu.players.find(p => p.slot === 'CPT');
                      const flex = lu.players.filter(p => p.slot === 'FLEX');
                      // Selected players get an underline so the drill-down is visible in each row
                      const hit = p => drillKeys.has(baseKey(p)) || (p.slot === 'CPT' && drillKeys.has(`${baseKey(p)}|CPT`));
                      const nmText = p => (p.pos === 'DST' ? `${p.team} DST` : p.name.split(' ').slice(-1)[0]);
                      const nm = (p, i) => (hit(p)
                        ? <span key={i} style={{ textDecoration: 'underline', textDecorationColor: 'var(--accent-primary)', textUnderlineOffset: '3px' }}>{nmText(p)}</span>
                        : <span key={i}>{nmText(p)}</span>);
                      const isOpen = expanded === idx;
                      return (
                        <Fragment key={idx}>
                          <tr onClick={() => setExpanded(isOpen ? null : idx)} className="lineup-row"
                            style={{ cursor: 'pointer', borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                            <td style={{ padding: '6px 5px', color: 'var(--text-muted)', whiteSpace: 'nowrap', maxWidth: '130px', overflow: 'hidden', textOverflow: 'ellipsis' }}
                              title={lu.label || undefined}>
                              {resultsMode === 'lab' && lu.label ? lu.label : idx + 1}
                            </td>
                            <td style={{ padding: '6px 5px', whiteSpace: 'nowrap', fontWeight: 700 }}>
                              <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[cpt.team] || '#888', marginRight: '4px' }} />
                              {nm(cpt)}
                            </td>
                            <td style={{ padding: '6px 5px', whiteSpace: 'nowrap', color: 'var(--text-main)' }}>
                              {flex.map((p, i) => <Fragment key={i}>{i > 0 && ', '}{nm(p, i)}</Fragment>)}
                            </td>
                            <td style={{ padding: '6px 5px', whiteSpace: 'nowrap', color: 'var(--text-main)' }}
                              title={lu.build_away != null ? `${lu.build_away} ${awayTeam} + ${6 - lu.build_away} ${homeTeam}` : undefined}>
                              {lu.build_away != null ? `${lu.build_away}-${6 - lu.build_away}` : '—'}
                            </td>
                            <td style={{ padding: '6px 5px', fontWeight: 700, color: ownColor(lu.total_ownership) }}>{lu.total_ownership}%</td>
                            <td style={{ padding: '6px 5px', whiteSpace: 'nowrap', color: 'var(--text-muted)' }}
                              title={lu.product_ownership != null ? lu.product_ownership.toExponential(2) : 'Re-optimize to compute (older saved results)'}>
                              {fmtOneIn(lu.product_ownership)}
                            </td>
                            <td style={{ padding: '6px 5px', color: 'var(--accent-green)', fontWeight: 600 }}>{lu.projected_score}</td>
                            <td style={{ padding: '6px 5px', color: evColor(lu.ev_pct), fontWeight: 600 }}>{lu.ev_pct > 0 ? '+' : ''}{lu.ev_pct}%</td>
                            <td style={{ padding: '6px 5px' }}>{lu.itm_pct}%</td>
                            <td style={{ padding: '6px 5px' }}>{lu.top1_pct}%</td>
                            <td style={{ padding: '6px 5px' }}>{lu.top01_pct}%</td>
                            <td style={{ padding: '6px 5px', fontWeight: 600, color: (lu.first_pct || 0) > 0 ? 'var(--accent-primary)' : 'var(--text-muted)' }}
                              title={lu.first_count != null ? `${lu.first_count} of ${lu.n_sims} sims` : undefined}>
                              {lu.first_pct != null ? `${lu.first_pct}%` : '—'}
                            </td>
                            <td style={{ padding: '6px 5px', color: lu.dupe_est > 5 ? '#ef4444' : 'var(--text-muted)' }}>{lu.dupe_est}</td>
                            <td style={{ padding: '6px 5px', color: lu.total_salary > DK_SALARY_CAP ? '#ef4444' : 'var(--text-main)' }}
                              title={lu.over_salary_cap ? `$${(lu.total_salary - DK_SALARY_CAP).toLocaleString()} over the $50k cap — DK would reject this lineup` : undefined}>
                              ${lu.total_salary?.toLocaleString()}{lu.over_salary_cap ? ' ⚠' : ''}
                            </td>
                            <td style={{ padding: '6px 5px', textAlign: 'center' }} onClick={e => e.stopPropagation()}>
                              {savedPaperIds[lineupSig(lu)] ? (
                                <span title="Saved as a paper entry" style={{ color: 'var(--accent-primary)' }}>✓</span>
                              ) : (
                                <button onClick={() => savePaperEntry(lu, idx)}
                                  title="Flag this lineup as one you're actually entering — settle it later with score_paper_entries.py"
                                  style={{ background: 'none', border: '1px solid rgba(255,255,255,0.15)', borderRadius: '5px', color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.72rem', padding: '2px 6px' }}>
                                  📝
                                </button>
                              )}
                            </td>
                          </tr>
                          {isOpen && (
                            <tr>
                              <td colSpan={15} style={{ padding: '8px', background: 'rgba(0,0,0,0.2)' }}>
                                <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(170px, 1fr))', gap: '6px' }}>
                                  {lu.players.map((p, pi) => (
                                    <div key={pi} style={{ background: 'rgba(255,255,255,0.03)', borderRadius: '8px', padding: '8px 10px', border: `1px solid ${(POS_COLORS[p.pos] || '#888')}22` }}>
                                      <div style={{ display: 'flex', justifyContent: 'space-between', marginBottom: '3px' }}>
                                        <span style={{ fontSize: '0.62rem', fontWeight: 700, padding: '1px 5px', borderRadius: '3px', background: (p.slot === 'CPT' ? '#00f2fe' : POS_COLORS[p.pos] || '#888') + '22', color: p.slot === 'CPT' ? '#00f2fe' : POS_COLORS[p.pos] || '#888' }}>{p.slot}</span>
                                        <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)' }}>${(p.salary || 0).toLocaleString()}</span>
                                      </div>
                                      <div style={{ fontWeight: 700, fontSize: '0.82rem' }}>
                                        <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '5px' }} />
                                        {p.pos === 'DST' ? `${p.team} DST` : p.name}
                                      </div>
                                      <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.72rem', marginTop: '3px' }}>
                                        <span style={{ color: 'var(--accent-primary)' }}>Proj {p.projection}</span>
                                        {p.ownership_pct != null && <span style={{ color: 'var(--text-muted)' }}>Own {round1(p.ownership_pct)}%</span>}
                                      </div>
                                    </div>
                                  ))}
                                </div>
                                <div style={{ display: 'flex', gap: '18px', marginTop: '8px', fontSize: '0.74rem', color: 'var(--text-muted)' }}>
                                  <span>P50 <strong style={{ color: 'var(--text-white)' }}>{lu.lineup_p50}</strong></span>
                                  <span>P95 <strong style={{ color: 'var(--accent-gold)' }}>{lu.lineup_p95}</strong></span>
                                  <span>Top1% <strong style={{ color: 'var(--text-white)' }}>{lu.top1_pct}%</strong></span>
                                  <span>Top.1% <strong style={{ color: 'var(--text-white)' }}>{lu.top01_pct}%</strong></span>
                                  {lu.first_count != null && (
                                    <span>1st <strong style={{ color: 'var(--text-white)' }}>{lu.first_count}/{lu.n_sims}</strong> ({lu.first_pct}%)</span>
                                  )}
                                  <span>Port.Score <strong style={{ color: 'var(--text-white)' }}>{lu.portfolio_score}</strong></span>
                                  {lu.histogram && (
                                    <button onClick={e => { e.stopPropagation(); setHistLineup(lu); }}
                                      title="Show this lineup's range of outcomes across all sim runs"
                                      style={{ background: 'rgba(0,242,254,0.1)', border: '1px solid rgba(0,242,254,0.25)', borderRadius: '6px', color: 'var(--accent-primary)', cursor: 'pointer', fontSize: '0.72rem', padding: '3px 8px', fontWeight: 600 }}>
                                      📊 Range of Outcomes
                                    </button>
                                  )}
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

            {/* Portfolio Exposure -- classic-style card panel (Optimizer.jsx), showdown-
                adapted: an ALL bar (any slot, tick = field FLEX+CPT own) and a CPT bar
                (tick = field CPT own). Click the card = filter to lineups with the player
                in any slot; click the CPT bar = filter to them AS captain. */}
            <div style={{ ...cardStyle, display: 'flex', flexDirection: 'column', gap: '10px' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <h2 style={{ margin: 0, fontSize: '1rem' }}>Portfolio Exposure</h2>
                <span style={{ fontSize: '0.75rem', color: 'var(--accent-primary)', fontWeight: 600 }}>{exposures.length} Players</span>
              </div>
              <input type="text" placeholder="Search portfolio..." value={expSearch}
                onChange={e => setExpSearch(e.target.value)} style={inputStyle} />
              <div style={{ display: 'flex', gap: '4px', flexWrap: 'wrap' }}>
                {['ALL', 'QB', 'RB', 'WR', 'TE', ...(exposures.some(e => e.pos === 'K') ? ['K'] : []), 'DST'].map(pos => {
                  const on = expPos === pos;
                  return (
                    <button key={pos} onClick={() => setExpPos(pos)} style={{
                      padding: '2px 6px', borderRadius: '20px', cursor: 'pointer', fontSize: '0.7rem', fontWeight: 600, border: '1px solid',
                      background: on ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.03)',
                      color: on ? 'var(--accent-primary)' : 'var(--text-muted)',
                      borderColor: on ? 'var(--accent-primary)' : 'rgba(255,255,255,0.08)',
                    }}>{pos}</button>
                  );
                })}
              </div>
              <div style={{ display: 'flex', alignItems: 'center', gap: '4px', fontSize: '0.7rem', color: 'var(--text-muted)', flexWrap: 'wrap' }}>
                <span>Sort:</span>
                {[
                  ['totalPct', 'Exposure'], ['cptPct', 'CPT'], ['leverage', 'Leverage'],
                  ['cptLeverage', 'CPT Lev'], ['salary', 'Salary'], ['projection', 'Proj'],
                ].map(([field, label]) => {
                  const on = expSort.field === field;
                  return (
                    <button key={field}
                      onClick={() => setExpSort(s => (s.field === field ? { field, asc: !s.asc } : { field, asc: false }))}
                      style={{
                        background: on ? 'rgba(255,255,255,0.08)' : 'transparent', border: 'none', cursor: 'pointer',
                        color: on ? 'var(--text-white)' : 'var(--text-muted)', padding: '2px 6px', borderRadius: '4px',
                        fontSize: '0.7rem', fontWeight: on ? 700 : 500,
                      }}>
                      {label} {on ? (expSort.asc ? '↑' : '↓') : ''}
                    </button>
                  );
                })}
              </div>

              <div style={{ maxHeight: '72vh', overflowY: 'auto', display: 'flex', flexDirection: 'column', gap: '8px', paddingRight: '4px' }}>
                {filteredExposures.length === 0 ? (
                  <div style={{ textAlign: 'center', color: 'var(--text-muted)', padding: '20px 0', fontSize: '0.8rem' }}>No players match filters</div>
                ) : filteredExposures.map(e => {
                  const posColor = POS_COLORS[e.pos] || '#888';
                  const anySel = drillKeys.has(e.key);
                  const cptSel = drillKeys.has(`${e.key}|CPT`);
                  const sel = anySel || cptSel;
                  const baseBg = sel ? 'rgba(0,242,254,0.08)' : 'rgba(255,255,255,0.02)';
                  const lev = (v) => (v == null ? '—' : `${v > 0 ? '+' : ''}${v.toFixed(1)}%`);
                  const levColor = (v) => (v == null ? 'var(--text-muted)' : v > 0 ? 'var(--accent-green)' : 'var(--accent-red)');
                  /** One exposure bar. Inputs: pct (bar width), tick (field own %,
                   *  or null), CSS background, centered label, tick tooltip. Output: JSX. */
                  const bar = (pct, tick, color, label, tickTitle) => (
                    <div style={{ position: 'relative', height: '15px', background: 'rgba(255,255,255,0.04)', borderRadius: '4px', overflow: 'hidden', flex: 1 }}>
                      <div style={{ position: 'absolute', left: 0, top: 0, bottom: 0, width: `${Math.min(pct, 100)}%`, background: color, borderRadius: '4px', transition: 'width 0.4s ease' }} />
                      {tick != null && tick > 0 && tick <= 100 && (
                        <div title={tickTitle} style={{ position: 'absolute', left: `${tick}%`, top: 0, bottom: 0, width: '2px', backgroundColor: 'rgba(255,255,255,0.7)', boxShadow: '0 0 4px #fff', zIndex: 2 }} />
                      )}
                      <div style={{ position: 'absolute', inset: 0, display: 'flex', alignItems: 'center', justifyContent: 'center', fontSize: '0.66rem', fontWeight: 700, color: 'var(--text-white)', textShadow: '0 1px 2px rgba(0,0,0,0.8)', zIndex: 3 }}>
                        {label}
                      </div>
                    </div>
                  );
                  return (
                    <div key={e.key} onClick={() => toggleDrill(e.key)}
                      title={anySel ? 'Click to remove from the lineup filter' : 'Click to show only lineups with this player (click the CPT bar to require them at captain)'}
                      style={{
                        background: baseBg, border: `1px solid ${sel ? 'rgba(0,242,254,0.45)' : 'rgba(255,255,255,0.04)'}`,
                        borderRadius: '8px', padding: '8px 10px', display: 'flex', flexDirection: 'column', gap: '5px',
                        cursor: 'pointer', transition: 'background 0.2s',
                      }}
                      onMouseEnter={ev => { ev.currentTarget.style.background = sel ? 'rgba(0,242,254,0.12)' : 'rgba(255,255,255,0.04)'; }}
                      onMouseLeave={ev => { ev.currentTarget.style.background = baseBg; }}>
                      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                        <div style={{ display: 'flex', alignItems: 'center', gap: '6px', minWidth: 0 }}>
                          <span style={{ fontSize: '0.62rem', fontWeight: 700, padding: '1px 4px', borderRadius: '3px', background: `${posColor}15`, color: posColor }}>{e.pos}</span>
                          <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[e.team] || '#888', flexShrink: 0 }} />
                          <span style={{ fontWeight: 700, fontSize: '0.82rem', color: 'var(--text-white)', whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis' }}>
                            {e.pos === 'DST' ? `${e.team} DST` : e.name}
                          </span>
                          <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>{e.team}</span>
                        </div>
                        <div style={{ fontSize: '0.75rem', fontWeight: 600, color: 'var(--text-muted)' }}>${(e.salary || 0).toLocaleString()}</div>
                      </div>
                      <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.68rem', color: 'var(--text-muted)', gap: '6px' }}>
                        <span>Proj: <strong style={{ color: 'var(--accent-primary)' }}>{e.projection}</strong></span>
                        {e.fieldOwn != null ? (
                          <>
                            <span title="Field FLEX own / field CPT own">Own: <strong>{e.flexOwn != null ? round1(e.flexOwn) : '—'}%</strong> / <strong style={{ color: '#00f2fe' }}>{e.cptOwn != null ? round1(e.cptOwn) : '—'}%</strong></span>
                            <span title="Total exposure minus field (FLEX + CPT) ownership">Lev: <strong style={{ color: levColor(e.leverage) }}>{lev(e.leverage)}</strong></span>
                          </>
                        ) : <span>No ownership data</span>}
                      </div>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                        <span style={{ fontSize: '0.6rem', width: '26px', color: 'var(--text-muted)' }}>ALL</span>
                        {bar(e.totalPct, e.fieldOwn, `linear-gradient(90deg, ${posColor}88, ${posColor})`,
                          `${e.totalPct.toFixed(1)}% (${e.total}/${keptLineups.length})`,
                          e.fieldOwn != null ? `Field in-lineup rate (FLEX + CPT own): ${round1(e.fieldOwn)}%` : undefined)}
                        <span style={{ width: '42px' }} />
                      </div>
                      <div onClick={ev => { ev.stopPropagation(); if (e.cpt > 0) toggleDrill(`${e.key}|CPT`); }}
                        title={e.cpt > 0 ? (cptSel ? 'Click to remove the CPT filter' : 'Click to show only lineups with this player at captain') : 'Never captained in this portfolio'}
                        style={{ display: 'flex', alignItems: 'center', gap: '6px', cursor: e.cpt > 0 ? 'pointer' : 'default',
                          borderRadius: '4px', outline: cptSel ? '1px solid rgba(0,242,254,0.6)' : 'none' }}>
                        <span style={{ fontSize: '0.6rem', width: '26px', color: '#00f2fe', fontWeight: 700 }}>CPT</span>
                        {bar(e.cptPct, e.cptOwn, 'linear-gradient(90deg, rgba(0,242,254,0.35), rgba(0,242,254,0.7))',
                          `${e.cptPct.toFixed(1)}% (${e.cpt})`,
                          e.cptOwn != null ? `Field CPT own: ${round1(e.cptOwn)}%` : undefined)}
                        <span style={{ fontSize: '0.64rem', fontWeight: 700, width: '42px', textAlign: 'right', color: levColor(e.cptLeverage) }}
                          title="CPT exposure minus field CPT ownership">{lev(e.cptLeverage)}</span>
                      </div>
                    </div>
                  );
                })}
              </div>
            </div>
          </div>
        </div>
      )}
      {histLineup && <LineupHistogramModal lineup={histLineup} onClose={() => setHistLineup(null)} />}
    </div>
  );
}
