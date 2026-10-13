// Classic (DK/FD 9-slot) lineup analysis: summary tiles, FLEX split, lineup table,
// exposure drill-down and Portfolio Exposure panel. Extracted verbatim from
// pages/Optimizer.jsx's results view (2026-10-08) so the Shortlist page shows
// exactly the same tools over its saved lineups.
//
// Inputs
//   lineups      array of optimizer lineup objects ({players[], ev_pct, itm_pct, ...})
//   pool         player pool rows ({id,name,team,ownershipPct,salary,projection}) -- used for the
//                Field Own / Leverage columns of the exposure panel
//   preferPoolOwnership  true on the Shortlist: LIVE pool ownership overrides the
//                ownership frozen into each saved lineup
//   portfolioStats       optimizer's portfolio summary ({total_ev_pct,...}); the tiles hide when null
//   headerActions        ({sortedLineups, displayedLineups}) => node, buttons top-right of the summary card
//   rowAction            (lu, idx) => node, optional extra cell appended to each lineup row
// Outputs: JSX only; all filter/sort/drill-down state is local.
import { useState, useMemo, useEffect, Fragment } from 'react';
import LineupHistogramModal from './LineupHistogramModal';
import { TEAM_COLORS, POS_COLORS, cardStyle, inputStyle, pillBtnBase, getSlottedColumns } from '../optimizerShared';

export default function ClassicLineupAnalysis({
  lineups = [],
  pool = [],
  preferPoolOwnership = false,
  portfolioStats = null,
  salaryCap = 50000,
  resultsMode = 'optimize',
  autoExpandFirst = false,
  isLocked = () => false,
  summaryTitle = null,
  summarySubtitle = null,
  subtitleExtra = null,
  tableTitle = 'Generated Lineups',
  exposureTitle = 'Portfolio Exposure',
  emptyMessage = 'No lineups yet. Click ⚡ Optimize to generate.',
  headerActions = null,
  rowAction = null,
  rowActionHeader = '',
}) {
  const [resSortField, setResSortField] = useState('ev_pct');
  const [resSortAsc, setResSortAsc] = useState(false);
  const [expandedLineupIdx, setExpandedLineupIdx] = useState(null);
  const [histLineup, setHistLineup] = useState(null);
  const [exposureSearch, setExposureSearch] = useState('');
  const [exposurePosFilter, setExposurePosFilter] = useState('ALL');
  const [exposureSortField, setExposureSortField] = useState('exposure');
  const [exposureSortAsc, setExposureSortAsc] = useState(false);
  // Keys (`name_team_pos`, same as playerExposures' key) of players clicked in the exposure panel.
  const [selectedExposureKeys, setSelectedExposureKeys] = useState(() => new Set());
  const [exposureMatchMode, setExposureMatchMode] = useState('all'); // 'all' | 'any'
  // A new lineup set (fresh optimize / loaded build / portfolio change) invalidates expanded-row indices.
  useEffect(() => { setExpandedLineupIdx(autoExpandFirst && lineups.length ? 0 : null); }, [lineups]); // eslint-disable-line react-hooks/exhaustive-deps
  const handleResSort = (field) => {
    if (resSortField === field) setResSortAsc(!resSortAsc);
    else { setResSortField(field); setResSortAsc(false); }
  };

  // ── Results helpers
  const sortedLineups = useMemo(() => {
    const ls = [...(lineups || [])];
    return ls.sort((a, b) => {
      let va = a[resSortField] ?? 0, vb = b[resSortField] ?? 0;
      if (typeof va === 'string') return resSortAsc ? va.localeCompare(vb) : vb.localeCompare(va);
      return resSortAsc ? va - vb : vb - va;
    });
  }, [lineups, resSortField, resSortAsc]);

  const playerExposures = useMemo(() => {
    if (!lineups || lineups.length === 0) return [];
    const counts = {};
    const poolMap = {};
    pool.forEach(p => {
      poolMap[p.id] = p;
      poolMap[p.name.toLowerCase()] = p;
    });

    lineups.forEach(lineup => {
      lineup.players.forEach(p => {
        const key = `${p.name}_${p.team}_${p.pos}`;
        if (!counts[key]) {
          const poolPlayer = poolMap[`${p.name}_${p.team}`] || poolMap[p.name.toLowerCase()];
          // Optimizer: the lineup's own frozen ownership wins. Portfolio: LIVE pool ownership wins.
          const projOwn = preferPoolOwnership
            ? (poolPlayer?.ownershipPct ?? p.ownership_pct ?? 0)
            : (p.ownership_pct ?? poolPlayer?.ownershipPct ?? 0);
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

    const total = lineups.length;
    return Object.values(counts).map(p => {
      const exposure = (p.count / total) * 100;
      const leverage = p.ownership ? (exposure - p.ownership) : exposure;
      return {
        ...p,
        exposure,
        leverage
      };
    });
  }, [lineups, pool]);

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
  // Optional "FLEX filled by" filter, applied on top of the player selection.
  const [flexPosFilter, setFlexPosFilter] = useState(null); // null | 'RB' | 'WR' | 'TE'

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
  useEffect(() => { setExpandedLineupIdx(null); }, [selectedExposureKeys, exposureMatchMode, flexPosFilter]);

  /** Lineups shown in the results table.
   * Inputs: sortedLineups (all lineups, current sort), selectedExposureKeys
   *   (Set<string> of player keys), exposureMatchMode ('all' | 'any').
   * Output: array of lineup objects -- sortedLineups unchanged when nothing is
   *   selected, else only lineups containing all/any of the selected players.
   * Exports keep using sortedLineups, so filtering never trims a DK upload.
   * Built in two steps: playerFilteredLineups (player selection only) feeds the
   * FLEX breakdown, so its RB/WR/TE split doesn't collapse to 100% the moment
   * one FLEX position is clicked; displayedLineups then applies flexPosFilter. */
  const playerFilteredLineups = useMemo(() => {
    if (selectedExposureKeys.size === 0) return sortedLineups;
    const sel = [...selectedExposureKeys];
    return sortedLineups.filter(lu => {
      const keys = new Set(lu.players.map(lineupKey));
      return exposureMatchMode === 'all' ? sel.every(k => keys.has(k)) : sel.some(k => keys.has(k));
    });
  }, [sortedLineups, selectedExposureKeys, exposureMatchMode]);
  /** Position that filled a lineup's FLEX slot ('RB' | 'WR' | 'TE' | null). */
  const flexPosOf = (lu) => lu.players.find(p => p.slot === 'FLEX')?.pos ?? null;
  const displayedLineups = useMemo(
    () => (flexPosFilter ? playerFilteredLineups.filter(lu => flexPosOf(lu) === flexPosFilter) : playerFilteredLineups),
    [playerFilteredLineups, flexPosFilter] // eslint-disable-line react-hooks/exhaustive-deps
  );
  const isFiltered = selectedExposureKeys.size > 0 || flexPosFilter != null;

  /** FLEX-slot position split for a lineup set.
   * Input: array of lineup objects. Output: { RB, WR, TE } -> { count, pct }.
   * Shown in the drill-down (subset vs whole portfolio) so you can see e.g.
   * whether JSN lineups tend to spend FLEX on a second WR or an RB. */
  const flexBreakdown = (ls) => {
    const out = { RB: { count: 0, pct: 0 }, WR: { count: 0, pct: 0 }, TE: { count: 0, pct: 0 } };
    ls.forEach(lu => { const pos = flexPosOf(lu); if (out[pos]) out[pos].count++; });
    Object.values(out).forEach(o => { o.pct = ls.length ? (o.count / ls.length) * 100 : 0; });
    return out;
  };

  /** Average per-lineup metrics over a lineup set.
   * Input: array of lineup objects. Output: { n, ev_pct, portfolio_score,
   *   itm_pct, top1_pct, top01_pct, p95 } (means; null if unavailable).
   *   (No salary on classic -- every lineup sits near the cap, so it says nothing.)
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
    };
  };
  const portfolioAvg = useMemo(() => summarizeLineups(sortedLineups), [sortedLineups]); // eslint-disable-line react-hooks/exhaustive-deps
  const selectionAvg = useMemo(
    () => (isFiltered ? summarizeLineups(displayedLineups) : null),
    [displayedLineups, isFiltered] // eslint-disable-line react-hooks/exhaustive-deps
  );
  const portfolioFlex = useMemo(() => flexBreakdown(sortedLineups), [sortedLineups]); // eslint-disable-line react-hooks/exhaustive-deps
  const selectionFlex = useMemo(() => flexBreakdown(playerFilteredLineups), [playerFilteredLineups]); // eslint-disable-line react-hooks/exhaustive-deps
  const selectedExposurePlayers = useMemo(
    () => playerExposures.filter(p => selectedExposureKeys.has(p.key)),
    [playerExposures, selectedExposureKeys]
  );

  /** "Most paired with" for the exposure drill-down.
   * Inputs: displayedLineups (the filtered subset), selectedExposureKeys,
   *   playerExposures (for each partner's whole-portfolio exposure).
   * Output: array of { key, name, pos, team, count, pairPct, exposure, lift }
   *   for every player (other than the selected ones) appearing in the subset,
   *   sorted by count desc. pairPct = % of the subset's lineups they're in;
   *   exposure = their % across ALL lineups; lift = pairPct - exposure, i.e.
   *   how much more (or less) they show up alongside the selection than usual.
   * The UI slices this per position / overall. */
  const pairedPlayers = useMemo(() => {
    if (selectedExposureKeys.size === 0 || displayedLineups.length === 0) return [];
    const expByKey = new Map(playerExposures.map(p => [p.key, p]));
    const counts = new Map();
    displayedLineups.forEach(lu => lu.players.forEach(pl => {
      const k = lineupKey(pl);
      if (selectedExposureKeys.has(k)) return;
      counts.set(k, (counts.get(k) || 0) + 1);
    }));
    const n = displayedLineups.length;
    return [...counts.entries()].map(([k, count]) => {
      const e = expByKey.get(k) || {};
      const pairPct = (count / n) * 100;
      return { key: k, name: e.name, pos: e.pos, team: e.team, count, pairPct, exposure: e.exposure ?? 0, lift: pairPct - (e.exposure ?? 0) };
    }).sort((a, b) => b.count - a.count || b.lift - a.lift);
  }, [displayedLineups, selectedExposureKeys, playerExposures]); // eslint-disable-line react-hooks/exhaustive-deps
  const [pairPosFilter, setPairPosFilter] = useState('ALL');

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


  const nCols = 18 + (rowAction ? 1 : 0);

  return (
        <div style={{ display: 'flex', flexDirection: 'column', gap: '14px' }}>

          {/* Portfolio Summary Card */}
          {(portfolioStats || summaryTitle) && (
            <div style={{ ...cardStyle, borderColor: 'rgba(0,242,254,0.15)' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '12px', flexWrap: 'wrap', gap: '10px' }}>
                <div>
                  <h2 style={{ margin: 0, fontSize: '1rem' }}>
                    {summaryTitle || (resultsMode === 'lab' ? '✍️ Lineup Lab' : 'Portfolio Summary')}
                  </h2>
                  <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginTop: '2px' }}>
                    {summarySubtitle ?? (resultsMode === 'lab'
                      ? `${portfolioStats.n_generated} hand-built lineup${portfolioStats.n_generated === 1 ? '' : 's'} scored`
                      : `${portfolioStats.n_generated} lineups generated${portfolioStats.n_requested && portfolioStats.n_generated < portfolioStats.n_requested ? ` (${portfolioStats.n_requested} requested)` : ''}`)}
                    {subtitleExtra}
                  </div>
                </div>
                <div style={{ display: 'flex', gap: '8px', flexWrap: 'wrap' }}>
                  {headerActions && headerActions({ sortedLineups, displayedLineups })}
                </div>
              </div>
              {portfolioStats && (<div style={{ display: 'grid', gridTemplateColumns: 'repeat(4, 1fr)', gap: '10px' }}>
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
              </div>)}
              {/* Whole-portfolio FLEX split -- click a position to filter the
                  lineups table (same flexPosFilter as the drill-down). */}
              {sortedLineups.length > 0 && (
                <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '6px', marginTop: '10px' }}>
                  <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.05em' }}>FLEX filled by</span>
                  {['RB', 'WR', 'TE'].map(pos => {
                    const s = portfolioFlex[pos];
                    const active = flexPosFilter === pos;
                    return (
                      <button key={pos} onClick={() => setFlexPosFilter(active ? null : pos)}
                        title={`${s.count}/${sortedLineups.length} lineups use a ${pos} at FLEX. Click to filter the lineups table.`}
                        style={{ ...pillBtnBase, display: 'inline-flex', gap: '5px', alignItems: 'center', padding: '3px 10px', fontSize: '0.78rem',
                          background: active ? (POS_COLORS[pos] || '#888') + '33' : 'rgba(255,255,255,0.03)', color: 'var(--text-white)',
                          borderColor: (POS_COLORS[pos] || '#888') + (active ? 'cc' : '55') }}>
                        <span style={{ color: POS_COLORS[pos] || '#888', fontWeight: 700, fontSize: '0.7rem' }}>{pos}</span>
                        <span style={{ fontWeight: 700 }}>{s.pct.toFixed(0)}%</span>
                        <span style={{ color: 'var(--text-muted)' }}>({s.count})</span>
                      </button>
                    );
                  })}
                </div>
              )}
            </div>
          )}

          {/* Grid Layout for Lineups and Portfolio Exposure */}
          <div className="optimizer-results-layout">
            {/* Lineups Table */}
            <div style={cardStyle}>
              <h2 style={{ margin: '0 0 10px 0', fontSize: '1rem' }}>
                {tableTitle} ({selectionAvg ? `${displayedLineups.length} of ${sortedLineups.length}` : sortedLineups.length})
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
                    {flexPosFilter && (
                      <span style={{ display: 'inline-flex', alignItems: 'center', gap: '5px', fontSize: '0.74rem', fontWeight: 600, padding: '2px 4px 2px 8px', borderRadius: '20px', background: (POS_COLORS[flexPosFilter] || '#888') + '1f', border: `1px solid ${(POS_COLORS[flexPosFilter] || '#888')}55`, color: 'var(--text-white)' }}>
                        FLEX = {flexPosFilter}
                        <button onClick={() => setFlexPosFilter(null)} title="Remove"
                          style={{ border: 'none', background: 'transparent', color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.72rem', padding: '0 3px' }}>✕</button>
                      </span>
                    )}
                    <button onClick={() => { setSelectedExposureKeys(new Set()); setFlexPosFilter(null); }}
                      style={{ ...pillBtnBase, marginLeft: 'auto', padding: '2px 9px', fontSize: '0.7rem', background: 'rgba(255,255,255,0.04)', color: 'var(--text-secondary)', borderColor: 'rgba(255,255,255,0.12)' }}>
                      Clear filter
                    </button>
                  </div>
                  {selectionAvg.n === 0 ? (
                    <div style={{ fontSize: '0.76rem', color: 'var(--text-muted)' }}>No lineups match this filter.</div>
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

                  {/* FLEX filled by -- split over the player-filtered lineups
                      (before the FLEX filter itself); click to filter. */}
                  {playerFilteredLineups.length > 0 && (
                    <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '6px', marginTop: '10px', borderTop: '1px solid rgba(255,255,255,0.06)', paddingTop: '8px' }}>
                      <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em' }}>FLEX filled by</span>
                      <span style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>· click to filter</span>
                      {['RB', 'WR', 'TE'].map(pos => {
                        const s = selectionFlex[pos], base = portfolioFlex[pos];
                        const active = flexPosFilter === pos;
                        const lift = s.pct - base.pct;
                        return (
                          <button key={pos} onClick={() => setFlexPosFilter(active ? null : pos)}
                            title={`${s.count}/${playerFilteredLineups.length} of these lineups use a ${pos} at FLEX (${base.pct.toFixed(1)}% across the whole portfolio)`}
                            style={{ ...pillBtnBase, display: 'inline-flex', gap: '5px', alignItems: 'center', padding: '2px 8px', fontSize: '0.72rem',
                              background: active ? (POS_COLORS[pos] || '#888') + '33' : 'rgba(255,255,255,0.03)', color: 'var(--text-white)',
                              borderColor: (POS_COLORS[pos] || '#888') + (active ? 'cc' : '55') }}>
                            <span style={{ color: POS_COLORS[pos] || '#888', fontWeight: 700, fontSize: '0.64rem' }}>{pos}</span>
                            <span style={{ color: 'var(--accent-primary)', fontWeight: 700 }}>{s.pct.toFixed(0)}%</span>
                            <span style={{ color: 'var(--text-muted)' }}>({s.count})</span>
                            {selectedExposureKeys.size > 0 && (
                              <span title="Percentage points vs whole-portfolio FLEX share"
                                style={{ fontSize: '0.66rem', fontWeight: 700, color: lift > 0 ? 'var(--accent-green)' : lift < 0 ? 'var(--accent-red)' : 'var(--text-muted)' }}>
                                {lift > 0 ? '+' : ''}{lift.toFixed(0)}
                              </span>
                            )}
                          </button>
                        );
                      })}
                    </div>
                  )}

                  {/* Most paired with -- who the optimizer stacks alongside the selection */}
                  {pairedPlayers.length > 0 && (() => {
                    const POS_ORDER = ['QB', 'RB', 'WR', 'TE', 'DST'];
                    const leaders = POS_ORDER.map(pos => pairedPlayers.find(p => p.pos === pos)).filter(Boolean);
                    const list = (pairPosFilter === 'ALL' ? pairedPlayers : pairedPlayers.filter(p => p.pos === pairPosFilter)).slice(0, 8);
                    const liftText = (v) => `${v > 0 ? '+' : ''}${v.toFixed(0)}`;
                    return (
                      <div style={{ marginTop: '10px', borderTop: '1px solid rgba(255,255,255,0.06)', paddingTop: '8px' }}>
                        <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '6px', marginBottom: '6px' }}>
                          <span style={{ fontSize: '0.68rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em' }}>Most paired with</span>
                          <span style={{ fontSize: '0.66rem', color: 'var(--text-muted)' }}>· click to add to filter</span>
                        </div>

                        {/* Top at each position */}
                        <div style={{ display: 'flex', flexWrap: 'wrap', gap: '5px', marginBottom: '8px' }}>
                          {leaders.map(p => (
                            <button key={p.key} onClick={() => toggleExposureSelect(p.key)}
                              title={`${p.name}: in ${p.count}/${displayedLineups.length} of these lineups (${p.exposure.toFixed(1)}% across the whole portfolio)`}
                              style={{ ...pillBtnBase, display: 'inline-flex', gap: '5px', alignItems: 'center', padding: '2px 8px', fontSize: '0.72rem', background: 'rgba(255,255,255,0.03)', color: 'var(--text-white)', borderColor: (POS_COLORS[p.pos] || '#888') + '55' }}>
                              <span style={{ color: POS_COLORS[p.pos] || '#888', fontWeight: 700, fontSize: '0.64rem' }}>{p.pos}</span>
                              <span style={{ fontWeight: 600 }}>{p.name}</span>
                              <span style={{ color: 'var(--accent-primary)', fontWeight: 700 }}>{p.pairPct.toFixed(0)}%</span>
                            </button>
                          ))}
                        </div>

                        {/* Position tabs + top-8 list */}
                        <div style={{ display: 'flex', gap: '3px', marginBottom: '6px' }}>
                          {['ALL', ...POS_ORDER].map(pos => {
                            const active = pairPosFilter === pos;
                            return (
                              <button key={pos} onClick={() => setPairPosFilter(pos)} style={{
                                ...pillBtnBase, padding: '1px 7px', fontSize: '0.66rem',
                                background: active ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.03)',
                                color: active ? 'var(--accent-primary)' : 'var(--text-muted)',
                                borderColor: active ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.08)',
                              }}>{pos}</button>
                            );
                          })}
                        </div>
                        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(250px, 1fr))', gap: '4px 12px' }}>
                          {list.map(p => (
                            <div key={p.key} onClick={() => toggleExposureSelect(p.key)}
                              title={`In ${p.count}/${displayedLineups.length} of these lineups vs ${p.exposure.toFixed(1)}% of all lineups. Click to add to filter.`}
                              style={{ display: 'grid', gridTemplateColumns: '30px 1fr 90px 34px', alignItems: 'center', gap: '6px', fontSize: '0.74rem', cursor: 'pointer', padding: '2px 0' }}>
                              <span style={{ fontSize: '0.62rem', fontWeight: 700, color: POS_COLORS[p.pos] || '#888' }}>{p.pos}</span>
                              <span style={{ whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis' }}>
                                <span style={{ fontWeight: 600, color: 'var(--text-white)' }}>{p.name}</span>{' '}
                                <span style={{ color: 'var(--text-muted)', fontSize: '0.66rem' }}>{p.team}</span>
                              </span>
                              {/* Bar = share of the filtered lineups; white tick = whole-portfolio exposure */}
                              <div style={{ position: 'relative', height: '12px', background: 'rgba(255,255,255,0.04)', borderRadius: '3px', overflow: 'hidden' }}>
                                <div style={{ position: 'absolute', left: 0, top: 0, bottom: 0, width: `${p.pairPct}%`, background: (POS_COLORS[p.pos] || '#888') + 'aa', borderRadius: '3px' }} />
                                <div style={{ position: 'absolute', left: `${Math.min(p.exposure, 99.5)}%`, top: 0, bottom: 0, width: '2px', background: 'rgba(255,255,255,0.75)' }} />
                                <div style={{ position: 'absolute', inset: 0, display: 'flex', alignItems: 'center', justifyContent: 'center', fontSize: '0.64rem', fontWeight: 700, color: 'var(--text-white)', textShadow: '0 1px 2px rgba(0,0,0,0.8)' }}>
                                  {p.pairPct.toFixed(0)}% ({p.count})
                                </div>
                              </div>
                              <span title="Percentage points above/below their whole-portfolio exposure"
                                style={{ fontSize: '0.68rem', fontWeight: 700, textAlign: 'right', color: p.lift > 0 ? 'var(--accent-green)' : p.lift < 0 ? 'var(--accent-red)' : 'var(--text-muted)' }}>
                                {liftText(p.lift)}
                              </span>
                            </div>
                          ))}
                        </div>
                      </div>
                    );
                  })()}
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
                      {['QB','RB','RB','WR','WR','WR','TE','FLEX','DST'].map((slot, i) => (
                        <th key={`${slot}${i}`} style={{ padding: '7px 6px', fontSize: '0.68rem', color: POS_COLORS[slot] || 'var(--text-muted)' }}>{slot}</th>
                      ))}
                      <th style={{ padding: '7px 6px', cursor: 'pointer' }} onClick={() => handleResSort(sortedLineups[0]?.lineup_p95 !== undefined ? 'lineup_p95' : 'projected_score')}>
                        Score (P95) {(resSortField === 'lineup_p95' || resSortField === 'projected_score') ? (resSortAsc ? '↑' : '↓') : ''}
                      </th>
                      <th style={{ padding: '7px 6px', cursor: 'pointer', whiteSpace: 'nowrap' }} onClick={() => handleResSort('total_salary')}>
                        Salary {resSortField === 'total_salary' ? (resSortAsc ? '↑' : '↓') : ''}
                      </th>
                      {rowAction && <th style={{ padding: '7px 6px' }}>{rowActionHeader}</th>}
                    </tr>
                  </thead>
                  <tbody>
                    {displayedLineups.length === 0 ? (
                      <tr><td colSpan={nCols} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>
                        {sortedLineups.length === 0 ? emptyMessage : 'No lineups match the selected players.'}
                      </td></tr>
                    ) : displayedLineups.map((lu, idx) => {
                      const rowTint = getLineupRowTint(lu, sortedLineups);
                      const slotted = getSlottedColumns(lu.players);
                      const isExpanded = expandedLineupIdx === idx;
                      const remaining = salaryCap - lu.total_salary;
                      const hasLocked = lu.players.some(p => isLocked(p));

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
                            {slotted.map((p, pi) => <Fragment key={pi}>{renderSlot(p)}</Fragment>)}
                            <td style={{ padding: '7px 6px', fontWeight: 700, color: 'var(--accent-green)' }}>
                              {lu.lineup_p95 !== undefined ? lu.lineup_p95?.toFixed(1) : lu.projected_score?.toFixed(1)}
                            </td>
                            <td style={{ padding: '7px 6px', fontWeight: 600, color: remaining < 0 ? 'var(--accent-red)' : 'var(--text-main)' }}
                              title={lu.over_salary_cap ? `$${Math.abs(remaining).toLocaleString()} over the $${salaryCap.toLocaleString()} cap — DK would reject this lineup` : undefined}>
                              ${lu.total_salary?.toLocaleString()}{lu.over_salary_cap ? ' ⚠' : ''}
                            </td>
                            {rowAction && <td style={{ padding: '4px 6px' }} onClick={e => e.stopPropagation()}>{rowAction(lu, idx)}</td>}
                          </tr>
                          {isExpanded && (
                            <tr>
                              <td colSpan={nCols} style={{ padding: '8px', background: 'rgba(0,0,0,0.2)' }}>
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
                <h2 style={{ margin: 0, fontSize: '1rem' }}>{exposureTitle}</h2>
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
                            {p.exposure.toFixed(1)}% ({p.count}/{lineups.length})
                          </div>
                        </div>
                      </div>
                    );
                  })
                )}
              </div>
            </div>
          </div>
        {histLineup && <LineupHistogramModal lineup={histLineup} onClose={() => setHistLineup(null)} />}
        </div>
  );
}
