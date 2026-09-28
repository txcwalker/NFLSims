import { useEffect, useMemo, useRef, useState } from 'react';
import { ApiService } from '../api';

/**
 * Prop Bet Finder -- prediction-market player props vs. our sims (DRAFT, 2026-09-26).
 *
 * Venue: Polymarket US (first; Kalshi and others later -- see
 * docs/implementation_plans/prediction_market_props_plan.md). Every Polymarket player prop is a
 * LADDER of "Will X record N+ <stat>?" contracts, so each player-stat is shown
 * as one group: the market's probability curve vs. our sim's, with the
 * market-implied and sim-implied median ("line") as the headline.
 *
 * Data: GET /api/props/polymarket?week=N (src/evaluation/prop_markets.py) --
 * live public book + our week sim parquet. No mock data anywhere on this page:
 * if the API fails, the page says so and shows nothing.
 *
 * What this page is NOT yet: graded. "EV" assumes our sim is right; nothing
 * here has been checked against results. The "How we're doing" card stays
 * empty until pre-kickoff prices are captured (plan, Phase 1).
 *
 * Inputs (props): weeks (App's week list), selectedWeek / setSelectedWeek.
 */

const C_SIM = '#0fa3b1';                 // same validated cyan as Game Lines' sim series
const C_MKT = 'var(--text-muted)';       // market = benchmark gray, as Vegas is on Game Lines
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const selStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px',
  color: 'var(--text-white)', padding: '4px 8px', fontSize: '0.8rem' };
const btn = (active) => ({
  background: active ? 'rgba(15,163,177,0.18)' : 'rgba(0,0,0,0.25)',
  border: `1px solid ${active ? C_SIM : 'rgba(255,255,255,0.14)'}`,
  borderRadius: '6px', color: 'var(--text-white)', padding: '4px 10px', fontSize: '0.8rem', cursor: 'pointer',
});
const th = { padding: '6px 6px', textAlign: 'left', whiteSpace: 'nowrap', fontWeight: 600 };
const td = { padding: '6px 6px', whiteSpace: 'nowrap' };
const label = { fontSize: '0.7rem', textTransform: 'uppercase', letterSpacing: '0.04em', color: 'var(--text-muted)' };

const pct = (v) => (v == null ? '—' : `${Math.round(v * 100)}%`);
const cents = (v) => (v == null ? '—' : `${Math.round(v * 100)}¢`);
const signedPts = (v) => (v == null ? '—' : `${v > 0 ? '+' : ''}${Math.round(v * 100)}`);
const signedC = (v) => (v == null ? '—' : `${v > 0 ? '+' : ''}${(v * 100).toFixed(1)}¢`);
const fmtLine = (v) => (v == null ? '—' : (typeof v === 'string' ? v : Number(v).toFixed(1)));
const ago = (ts) => {
  if (!ts) return '—';
  const s = Math.max(0, Date.now() / 1000 - ts);
  if (s < 90) return `${Math.round(s)}s ago`;
  if (s < 5400) return `${Math.round(s / 60)}m ago`;
  return `${Math.round(s / 3600)}h ago`;
};

const FLAG_TEXT = {
  sim_extreme: 'Sim says 0% or 100% while the market is in between — almost always a sim input problem (usage, injury, bug).',
  large_gap: 'Sim and market differ by 30+ pts — check the sim inputs (role, injury news) before trusting this.',
};
const SPREAD_OPTS = [['any', 'Any spread', Infinity], ['5', '≤ 5¢', 0.05], ['2', '≤ 2¢', 0.02]];
const PAGE = 60;

/**
 * Median implied by a ladder: the threshold where P(stat >= N) crosses 50%,
 * linearly interpolated between rungs.
 * Inputs: rungs sorted by threshold asc, key ('mid' | 'sim_p').
 * Output: number, or '< N' / '> N' string when every rung is on one side, or null.
 */
function ladderMedian(rungs, key) {
  const pts = rungs.filter((r) => r[key] != null);
  if (!pts.length) return null;
  if (pts[0][key] < 0.5) return `< ${pts[0].threshold}`;
  for (let i = 1; i < pts.length; i += 1) {
    const a = pts[i - 1], b = pts[i];
    if (a[key] >= 0.5 && b[key] < 0.5) {
      const t = (a[key] - 0.5) / (a[key] - b[key] || 1);
      return a.threshold + t * (b.threshold - a.threshold);
    }
  }
  return `> ${pts[pts.length - 1].threshold}`;
}

/** Groups rungs into one entry per game/team/player/stat ladder. */
function groupLadders(rows) {
  const m = new Map();
  rows.forEach((r) => {
    const k = `${r.game_id}|${r.team}|${r.pm_player}|${r.stat}`;
    if (!m.has(k)) m.set(k, { key: k, rungs: [] });
    m.get(k).rungs.push(r);
  });
  return [...m.values()].map((g) => {
    const rungs = g.rungs.sort((a, b) => a.threshold - b.threshold);
    const r0 = rungs[0];
    return {
      ...g, rungs,
      game_id: r0.game_id, matchup: r0.matchup, phase: r0.phase, team: r0.team,
      player: r0.pm_player, pos: r0.sim_pos, stat: r0.stat, stat_label: r0.stat_label,
      matched: r0.sim_p != null, sim_mean: r0.sim_mean,
      mkt_line: ladderMedian(rungs, 'mid'), sim_line: ladderMedian(rungs, 'sim_p'),
      flagged: rungs.some((r) => r.flag),
    };
  });
}

/** Best-EV rung of a ladder that passes the spread filter (unflagged unless allowed). */
function bestRung(g, maxSpread, allowFlagged) {
  let best = null;
  g.rungs.forEach((r) => {
    if (r.best_ev == null || r.spread == null || r.spread > maxSpread) return;
    if (r.flag && !allowFlagged) return;
    if (!best || r.best_ev > best.best_ev) best = r;
  });
  return best;
}

/**
 * One ladder as two survival curves: P(stat >= N) by threshold N.
 * Market = gray mid line with a bid–ask whisker per rung; sim = cyan line.
 * Hover a rung for its numbers. Inputs: rungs (sorted by threshold).
 */
function LadderChart({ rungs, statLabel }) {
  const [hover, setHover] = useState(null);
  const W = 460, H = 180, L = 38, R = 60, T = 12, B = 30;
  const xs = rungs.map((r) => r.threshold);
  const lo = Math.min(...xs), hi = Math.max(...xs);
  const x = (v) => L + (hi === lo ? (W - L - R) / 2 : ((v - lo) / (hi - lo)) * (W - L - R));
  const y = (p) => T + (1 - p) * (H - T - B);
  const path = (key) => rungs.filter((r) => r[key] != null)
    .map((r, i) => `${i ? 'L' : 'M'}${x(r.threshold).toFixed(1)},${y(r[key]).toFixed(1)}`).join(' ');
  const lastWith = (key) => [...rungs].reverse().find((r) => r[key] != null);
  const endM = lastWith('mid'), endS = lastWith('sim_p');
  const h = hover != null ? rungs[hover] : null;

  return (
    <div style={{ position: 'relative', width: W }}>
      <div style={{ display: 'flex', gap: '14px', fontSize: '0.75rem', color: 'var(--text-muted)', marginBottom: '4px' }}>
        <span><span style={{ display: 'inline-block', width: 14, height: 2, background: C_SIM, verticalAlign: 'middle', marginRight: 5 }} />Our sims</span>
        <span><span style={{ display: 'inline-block', width: 14, height: 0, borderTop: `2px dashed ${C_MKT}`, verticalAlign: 'middle', marginRight: 5 }} />Market mid (whisker = bid–ask)</span>
      </div>
      <svg width={W} height={H} role="img" aria-label={`Probability of reaching each ${statLabel} threshold: market vs. our sims`}>
        {[0, 0.25, 0.5, 0.75, 1].map((p) => (
          <g key={p}>
            <line x1={L} x2={W - R} y1={y(p)} y2={y(p)} stroke="rgba(255,255,255,0.06)" />
            <text x={L - 6} y={y(p) + 3} textAnchor="end" fontSize="10" fill="var(--text-muted)">{Math.round(p * 100)}%</text>
          </g>
        ))}
        {rungs.map((r) => (
          <text key={`t${r.threshold}`} x={x(r.threshold)} y={H - B + 14} textAnchor="middle" fontSize="10" fill="var(--text-muted)">{r.threshold}+</text>
        ))}
        {rungs.map((r) => (r.bid != null && r.ask != null ? (
          <line key={`w${r.threshold}`} x1={x(r.threshold)} x2={x(r.threshold)} y1={y(r.ask)} y2={y(r.bid)}
            stroke={C_MKT} strokeWidth="6" strokeOpacity="0.35" strokeLinecap="round" />
        ) : null))}
        <path d={path('mid')} fill="none" stroke={C_MKT} strokeWidth="2" strokeDasharray="5 4" />
        <path d={path('sim_p')} fill="none" stroke={C_SIM} strokeWidth="2" />
        {rungs.map((r) => (r.sim_p != null ? (
          <circle key={`s${r.threshold}`} cx={x(r.threshold)} cy={y(r.sim_p)} r="4" fill={C_SIM} stroke="var(--bg-deep)" strokeWidth="2" />
        ) : null))}
        {rungs.map((r) => (r.mid != null ? (
          <circle key={`m${r.threshold}`} cx={x(r.threshold)} cy={y(r.mid)} r="4" fill="var(--bg-deep)" stroke={C_MKT} strokeWidth="2" />
        ) : null))}
        {endS && <text x={x(endS.threshold) + 8} y={y(endS.sim_p) + 3} fontSize="10" fill="var(--text-white)">Sim</text>}
        {endM && <text x={x(endM.threshold) + 8} y={y(endM.mid) + 14} fontSize="10" fill="var(--text-muted)">Market</text>}
        {h && <line x1={x(h.threshold)} x2={x(h.threshold)} y1={T} y2={H - B} stroke="rgba(255,255,255,0.25)" />}
        {/* Hit targets: a full-height band per rung, wider than the marks. */}
        {rungs.map((r, i) => {
          const half = rungs.length > 1 ? (x(rungs[Math.min(i + 1, rungs.length - 1)].threshold) - x(rungs[Math.max(i - 1, 0)].threshold)) / (i === 0 || i === rungs.length - 1 ? 2 : 4) : 30;
          return (
            <rect key={`h${r.threshold}`} x={x(r.threshold) - Math.max(half, 10)} y={T} width={Math.max(half, 10) * 2} height={H - T - B}
              fill="transparent" onMouseEnter={() => setHover(i)} onMouseLeave={() => setHover(null)} />
          );
        })}
      </svg>
      {h && (
        <div style={{ position: 'absolute', left: Math.min(x(h.threshold) + 10, W - 170), top: 30, pointerEvents: 'none',
          background: 'var(--bg-deep)', border: '1px solid var(--border-glass)', borderRadius: 8, padding: '6px 9px', fontSize: '0.75rem', lineHeight: 1.5 }}>
          <div style={{ fontWeight: 600 }}>{h.threshold}+ {statLabel}</div>
          <div>Sim: {pct(h.sim_p)} · Market mid: {pct(h.mid)}</div>
          <div style={{ color: 'var(--text-muted)' }}>Bid {cents(h.bid)} / Ask {cents(h.ask)}</div>
          {h.best_side && <div>Best: {h.best_side} {signedC(h.best_ev)} / contract</div>}
        </div>
      )}
    </div>
  );
}

/** Per-rung table under the chart -- the same numbers, readable without hover. */
function RungTable({ rungs }) {
  return (
    <table style={{ borderCollapse: 'collapse', fontSize: '0.78rem' }}>
      <thead>
        <tr style={{ color: 'var(--text-muted)' }}>
          {['Line', 'Bid', 'Ask', 'Mkt', 'Sim', 'Gap', 'YES EV', 'NO EV', ''].map((h) => <th key={h} style={th}>{h}</th>)}
        </tr>
      </thead>
      <tbody>
        {rungs.map((r) => (
          <tr key={r.threshold} style={{ borderTop: '1px solid rgba(255,255,255,0.05)' }}>
            <td style={td}>{r.threshold}+</td>
            <td style={td}>{cents(r.bid)}</td>
            <td style={td}>{cents(r.ask)}</td>
            <td style={td}>{pct(r.mid)}</td>
            <td style={td}>{pct(r.sim_p)}</td>
            <td style={td}>{signedPts(r.diff)}</td>
            <td style={td}>{signedC(r.yes_ev)}</td>
            <td style={td}>{signedC(r.no_ev)}</td>
            <td style={{ ...td, color: 'var(--text-muted)' }} title={FLAG_TEXT[r.flag] || ''}>{r.flag ? '⚠ check' : ''}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

export default function PropMarkets({ weeks = [], selectedWeek, setSelectedWeek }) {
  // Fetch state is keyed by request ("week|nonce"): loading = the latest
  // result isn't for the current request. Avoids setState-in-effect.
  const [result, setResult] = useState({ key: null, data: null, error: null });
  const [nonce, setNonce] = useState(0);
  const forceRefresh = useRef(false);
  const reqKey = `${selectedWeek}|${nonce}`;
  const loading = result.key !== reqKey;
  const data = result.data;
  const error = loading ? null : result.error;

  const [game, setGame] = useState('ALL');
  const [stat, setStat] = useState('ALL');
  const [pos, setPos] = useState('ALL');
  const [search, setSearch] = useState('');
  const [spreadKey, setSpreadKey] = useState('5');
  const [showFlagged, setShowFlagged] = useState(false);
  const [preOnly, setPreOnly] = useState(true);
  const [sortBy, setSortBy] = useState('ev');
  const [open, setOpen] = useState(null);
  const [page, setPage] = useState({ key: '', limit: PAGE });

  useEffect(() => {
    let alive = true;
    const refresh = forceRefresh.current;
    forceRefresh.current = false;
    ApiService.getPolymarketProps(selectedWeek, refresh).then((res) => {
      if (!alive) return;
      // On failure drop the old data too -- the page says so rather than showing a stale book.
      setResult({ key: reqKey, data: res.ok ? res.data : null, error: res.ok ? null : res.error });
    });
    return () => { alive = false; };
  }, [selectedWeek, reqKey]);

  const ladders = useMemo(() => (data ? groupLadders(data.rows) : []), [data]);
  const maxSpread = SPREAD_OPTS.find((o) => o[0] === spreadKey)[2];

  const filtered = useMemo(() => {
    const q = search.trim().toLowerCase();
    const out = ladders
      .filter((g) => game === 'ALL' || g.game_id === game)
      .filter((g) => stat === 'ALL' || g.stat_label === stat)
      .filter((g) => pos === 'ALL' || g.pos === pos)
      .filter((g) => !preOnly || g.phase === 'pre')
      .filter((g) => !q || String(g.player).toLowerCase().includes(q) || String(g.team).toLowerCase().includes(q))
      .map((g) => ({ ...g, best: bestRung(g, maxSpread, showFlagged) }))
      // One flagged rung makes the whole ladder suspect (it's the same sim
      // distribution), so hide the ladder, not just that rung.
      .filter((g) => showFlagged || !g.flagged);
    const lineGap = (g) => (typeof g.mkt_line === 'number' && typeof g.sim_line === 'number' ? Math.abs(g.sim_line - g.mkt_line) : -1);
    const sorters = {
      ev: (a, b) => (b.best?.best_ev ?? -9) - (a.best?.best_ev ?? -9),
      gap: (a, b) => lineGap(b) - lineGap(a),
      name: (a, b) => String(a.player).localeCompare(String(b.player)),
    };
    return out.sort(sorters[sortBy]);
  }, [ladders, game, stat, pos, preOnly, search, maxSpread, showFlagged, sortBy]);

  // "Show more" resets whenever the filters change (limit is keyed by them).
  const filterKey = [game, stat, pos, preOnly, search, spreadKey, showFlagged, sortBy, selectedWeek].join('|');
  const limit = page.key === filterKey ? page.limit : PAGE;
  const setLimit = (n) => setPage({ key: filterKey, limit: n });

  const statOptions = useMemo(() => (data ? data.summary.by_stat.map((s) => s.stat_label) : []), [data]);
  const unmatched = data?.summary.unmatched || [];
  const unpriced = data?.summary.unpriced || [];

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      <div style={{ marginBottom: '14px' }}>
        <h1 style={{ marginBottom: '2px', fontSize: '1.6rem' }}>🎯 Prop Bet Finder</h1>
        <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', margin: 0, maxWidth: 900 }}>
          Prediction-market player props vs. our 10K-run sims. Each Polymarket prop is a ladder of “N+ yards” contracts,
          so every player-stat is shown as two probability curves — the market’s and ours. Venue: <b>Polymarket US</b> (public
          book, read-only). Kalshi and others come later.
        </p>
      </div>

      <div style={{ ...cardStyle, marginBottom: '14px', borderColor: 'rgba(201,125,0,0.45)', fontSize: '0.82rem', lineHeight: 1.5 }}>
        <b>Draft — not graded.</b> “EV” is what each side is worth <i>if our sim is right</i>, after Polymarket US’s taker
        fee (6.95% × p × (1−p) per contract). Our sims haven’t been checked against these markets yet, so large gaps are
        at least as likely to be sim-input problems (usage, injuries, bugs) as real value. Rows marked ⚠ are hidden by default.
      </div>

      <div style={{ ...cardStyle, display: 'flex', flexWrap: 'wrap', gap: '14px', alignItems: 'center', marginBottom: '14px' }}>
        <div>
          <div style={label}>Week</div>
          <select style={selStyle} value={selectedWeek ?? ''} onChange={(e) => setSelectedWeek(Number(e.target.value))}>
            {(weeks.length ? weeks : [selectedWeek]).filter((w) => w != null).map((w) => {
              const n = typeof w === 'object' ? (w.week ?? w.value) : w;
              return <option key={n} value={n}>Week {n}</option>;
            })}
          </select>
        </div>
        <div style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.5 }}>
          <div>Book pulled: <b style={{ color: 'var(--text-white)' }}>{data ? ago(data.fetched_at) : '—'}</b> (cached {data ? Math.round(data.ttl_s / 60) : 5} min)</div>
          <div>Sims updated: <b style={{ color: 'var(--text-white)' }}>{data ? (data.sims_updated_at ? new Date(data.sims_updated_at * 1000).toLocaleString() : 'no sims for this week') : '—'}</b></div>
        </div>
        <button style={btn(false)} disabled={loading} onClick={() => { forceRefresh.current = true; setNonce((n) => n + 1); }}>
          {loading ? 'Loading…' : '↻ Pull latest book'}
        </button>
      </div>

      {error && (
        <div style={{ ...cardStyle, marginBottom: '14px', borderColor: 'rgba(220,80,80,0.5)' }}>
          Couldn’t load Polymarket props: {error}. Nothing is shown rather than stale or placeholder data.
        </div>
      )}
      {loading && !data && <div style={{ ...cardStyle, marginBottom: '14px', color: 'var(--text-muted)' }}>Pulling this week’s books from Polymarket US and pricing them against our sims (~10s the first time)…</div>}

      {data && (
        <>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: '14px', marginBottom: '14px' }}>
            <div style={cardStyle}>
              <div style={{ fontWeight: 600, marginBottom: 6 }}>How we’re doing</div>
              <div style={{ fontSize: '0.82rem', color: 'var(--text-muted)', lineHeight: 1.55 }}>
                No graded markets yet. Grading needs each contract’s <b>pre-kickoff</b> price, and today’s pull only shows the
                current book. Phase 1 adds a price-capture ledger plus a backfill of closing prices from Polymarket’s public
                price history (weeks 1–3 look recoverable). Then this card shows sim vs. market accuracy (Brier / log-loss),
                calibration, and paper results by stat.
              </div>
            </div>
            <div style={cardStyle}>
              <div style={{ fontWeight: 600, marginBottom: 6 }}>This week’s games</div>
              <div style={{ display: 'flex', flexWrap: 'wrap', gap: '6px' }}>
                <button style={btn(game === 'ALL')} onClick={() => setGame('ALL')}>All</button>
                {data.games.map((g) => (
                  <button key={g.game_id} style={{ ...btn(game === g.game_id), opacity: g.found ? 1 : 0.5 }}
                    disabled={!g.found} onClick={() => setGame(g.game_id)}
                    title={g.found ? `${g.markets} prop rungs · ${g.phase === 'pre' ? 'pre-game' : g.phase}` : 'No Polymarket US event found for this game'}>
                    {g.matchup}{g.phase && g.phase !== 'pre' ? ` · ${g.phase}` : ''}
                  </button>
                ))}
              </div>
            </div>
          </div>

          <div style={{ ...cardStyle, marginBottom: '14px', overflowX: 'auto' }}>
            <div style={{ fontWeight: 600, marginBottom: 4 }}>Coverage & disagreement by stat</div>
            <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginBottom: 8 }}>
              “Sim − market” is the average gap in probability points across every rung: negative means our sims price YES lower than
              the market does. Disagreement, not accuracy.
            </div>
            <table style={{ borderCollapse: 'collapse', fontSize: '0.8rem', width: '100%' }}>
              <thead>
                <tr style={{ color: 'var(--text-muted)' }}>
                  {['Stat', 'Rungs', 'Priced by sim', 'Two-sided book', 'Median spread', 'Sim − market (avg)', 'Avg |gap|', '⚠ flagged'].map((h) => <th key={h} style={th}>{h}</th>)}
                </tr>
              </thead>
              <tbody>
                {data.summary.by_stat.map((s) => (
                  <tr key={s.stat_label} style={{ borderTop: '1px solid rgba(255,255,255,0.05)', cursor: 'pointer', background: stat === s.stat_label ? 'rgba(15,163,177,0.08)' : undefined }}
                    onClick={() => setStat(stat === s.stat_label ? 'ALL' : s.stat_label)}>
                    <td style={td}>{s.stat_label}</td>
                    <td style={td}>{s.markets}</td>
                    <td style={td}>{s.priced} ({pct(s.markets ? s.priced / s.markets : null)})</td>
                    <td style={td}>{s.two_sided}</td>
                    <td style={td}>{cents(s.median_spread)}</td>
                    <td style={td}>{signedPts(s.mean_diff)} pts</td>
                    <td style={td}>{s.mean_abs_diff == null ? '—' : `${Math.round(s.mean_abs_diff * 100)} pts`}</td>
                    <td style={td}>{s.flagged}</td>
                  </tr>
                ))}
              </tbody>
            </table>
            <div style={{ display: 'flex', gap: '24px', flexWrap: 'wrap', marginTop: 10, fontSize: '0.8rem' }}>
              <details>
                <summary style={{ cursor: 'pointer', color: 'var(--text-muted)' }}>
                  {unmatched.length} market players not found in our sims
                </summary>
                <div style={{ marginTop: 6, color: 'var(--text-muted)', maxWidth: 520, lineHeight: 1.6 }}>
                  Usually a player our week sim doesn’t have active (injury overrides, backup QB starting) or a name mismatch.
                  <div style={{ marginTop: 4, color: 'var(--text-white)' }}>
                    {unmatched.map((u) => `${u.pm_player} (${u.team ?? '?'}, ${u.markets})`).join(' · ')}
                  </div>
                </div>
              </details>
              <details>
                <summary style={{ cursor: 'pointer', color: 'var(--text-muted)' }}>{unpriced.length} prop types we can’t price yet</summary>
                <ul style={{ marginTop: 6, color: 'var(--text-muted)', paddingLeft: 18 }}>
                  {unpriced.map((u) => <li key={u.market_type}>{u.market_type.replace('football_player_', '').replaceAll('_', ' ')} ({u.count}) — {u.reason}</li>)}
                </ul>
              </details>
            </div>
          </div>

          <div style={{ ...cardStyle, display: 'flex', flexWrap: 'wrap', gap: '12px', alignItems: 'flex-end', marginBottom: '10px' }}>
            <div><div style={label}>Stat</div>
              <select style={selStyle} value={stat} onChange={(e) => setStat(e.target.value)}>
                <option value="ALL">All stats</option>
                {statOptions.map((s) => <option key={s} value={s}>{s}</option>)}
              </select></div>
            <div><div style={label}>Position</div>
              <div style={{ display: 'flex', gap: 4 }}>
                {['ALL', 'QB', 'RB', 'WR', 'TE'].map((p) => <button key={p} style={btn(pos === p)} onClick={() => setPos(p)}>{p}</button>)}
              </div></div>
            <div><div style={label}>Max bid–ask spread</div>
              <div style={{ display: 'flex', gap: 4 }}>
                {SPREAD_OPTS.map(([k, t]) => <button key={k} style={btn(spreadKey === k)} onClick={() => setSpreadKey(k)}>{t}</button>)}
              </div></div>
            <div><div style={label}>Sort</div>
              <select style={selStyle} value={sortBy} onChange={(e) => setSortBy(e.target.value)}>
                <option value="ev">Best EV rung</option>
                <option value="gap">Sim vs market line gap</option>
                <option value="name">Player</option>
              </select></div>
            <input style={{ ...selStyle, minWidth: 180 }} placeholder="Search player or team…" value={search} onChange={(e) => setSearch(e.target.value)} />
            <label style={{ fontSize: '0.8rem', display: 'flex', gap: 5, alignItems: 'center' }}>
              <input type="checkbox" checked={preOnly} onChange={(e) => setPreOnly(e.target.checked)} /> Pre-kickoff only
            </label>
            <label style={{ fontSize: '0.8rem', display: 'flex', gap: 5, alignItems: 'center' }} title={FLAG_TEXT.large_gap}>
              <input type="checkbox" checked={showFlagged} onChange={(e) => setShowFlagged(e.target.checked)} /> Show ⚠ flagged
            </label>
          </div>

          <div style={{ ...cardStyle, overflowX: 'auto' }}>
            <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginBottom: 8 }}>
              {filtered.length} player-stat ladders. “Line” = where the ladder crosses 50% (the market’s and our sims’ implied median).
              “Best rung” = the single contract with the highest after-fee EV under the current filters. Click a row for the full ladder.
            </div>
            <table style={{ borderCollapse: 'collapse', fontSize: '0.8rem', width: '100%' }}>
              <thead>
                <tr style={{ color: 'var(--text-muted)' }}>
                  {['Player', 'Game', 'Stat', 'Market line', 'Sim line', 'Sim avg', 'Best rung', 'Side', 'Mkt / Sim', 'EV / contract', 'Spread', ''].map((h) => <th key={h} style={th}>{h}</th>)}
                </tr>
              </thead>
              <tbody>
                {filtered.slice(0, limit).map((g) => (
                  <FragmentRow key={g.key} g={g} open={open === g.key} onToggle={() => setOpen(open === g.key ? null : g.key)} />
                ))}
              </tbody>
            </table>
            {filtered.length > limit && (
              <button style={{ ...btn(false), marginTop: 10 }} onClick={() => setLimit(limit + PAGE)}>Show {Math.min(PAGE, filtered.length - limit)} more</button>
            )}
            {!filtered.length && <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', padding: '10px 0' }}>No ladders match these filters.</div>}
          </div>
        </>
      )}
    </div>
  );
}

/** One ladder's summary row + (when open) its chart and rung table. */
function FragmentRow({ g, open, onToggle }) {
  const b = g.best;
  return (
    <>
      <tr onClick={onToggle} style={{ borderTop: '1px solid rgba(255,255,255,0.05)', cursor: 'pointer', background: open ? 'rgba(15,163,177,0.06)' : undefined }}>
        <td style={td}>
          <b>{g.player}</b> <span style={{ color: 'var(--text-muted)' }}>{g.team}{g.pos ? ` · ${g.pos}` : ''}</span>
          {!g.matched && <span style={{ color: 'var(--text-muted)' }} title="Not in this week's sims"> · not simmed</span>}
        </td>
        <td style={{ ...td, color: 'var(--text-muted)' }}>{g.matchup}</td>
        <td style={td}>{g.stat_label}</td>
        <td style={td}>{fmtLine(g.mkt_line)}</td>
        <td style={td}>{fmtLine(g.sim_line)}</td>
        <td style={td}>{g.sim_mean == null ? '—' : g.sim_mean.toFixed(1)}</td>
        <td style={td}>{b ? `${b.threshold}+` : '—'}</td>
        <td style={td}>{b ? b.best_side : '—'}</td>
        <td style={td}>{b ? `${pct(b.mid)} / ${pct(b.sim_p)}` : '—'}</td>
        <td style={{ ...td, fontWeight: b && b.best_ev > 0 ? 700 : 400 }}>{b ? signedC(b.best_ev) : '—'}</td>
        <td style={td}>{b ? cents(b.spread) : '—'}</td>
        <td style={{ ...td, color: 'var(--text-muted)' }} title={g.flagged ? 'At least one rung is flagged — see the ladder' : ''}>{g.flagged ? '⚠' : ''} {open ? '▾' : '▸'}</td>
      </tr>
      {open && (
        <tr>
          <td colSpan={12} style={{ padding: '10px 6px 16px' }}>
            <div style={{ display: 'flex', gap: '24px', flexWrap: 'wrap', alignItems: 'flex-start' }}>
              <LadderChart rungs={g.rungs} statLabel={g.stat_label} />
              <RungTable rungs={g.rungs} />
            </div>
          </td>
        </tr>
      )}
    </>
  );
}
