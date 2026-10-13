import { useEffect, useState } from 'react';
import { ApiService } from '../api';

/**
 * Underdog grading -- the board's legs settled against real stat lines (Pickems > Underdog Grading).
 *
 * Data: GET /api/pickems/results (src/pickems/grade_results.py). Every ledger leg whose game is final
 * is settled and scored at BOTH the opening multiplier (first capture before kickoff) and the closing
 * multiplier (last pre-kickoff capture, only trusted when the leg was re-pasted within the "close
 * window" before kickoff -- otherwise it is a stale open and left out of close numbers).
 *
 * The questions it answers: (1) do legs we flag +EV actually profit, at the open vs at the close
 * ("opening lines are more beatable"); (2) is OUR probability better than the market's (Brier score,
 * lower = better, on legs with both sides posted); (3) calibration. Returns are single-leg, flat
 * $1 -- no parlay structure. Our probability is the current (pre-kickoff-locked) sim, not a
 * snapshot taken at paste time.
 *
 * Samples are small for weeks: read the +/- (one standard error) before believing any ROI.
 *
 * Inputs (props): weeks (App's week list). Output: JSX.
 */
const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };
const card = { padding: '16px', borderRadius: '12px', border: '1px solid var(--border-glass)', marginBottom: '14px' };
const th = { padding: '6px 8px', textAlign: 'left', whiteSpace: 'nowrap', fontWeight: 600, fontSize: '0.72rem', textTransform: 'uppercase', color: 'var(--text-muted)' };
const td = { padding: '5px 8px', whiteSpace: 'nowrap', fontSize: '0.84rem' };

const pct = (v, d = 1) => (v == null ? '—' : `${(v * 100).toFixed(d)}%`);
const roi = (b) => (b?.roi == null ? '—' : `${b.roi > 0 ? '+' : ''}${(b.roi * 100).toFixed(1)}%${b.se != null ? ` ± ${(b.se * 100).toFixed(1)}` : ''}`);
const roiColor = (b) => (b?.roi == null ? 'inherit' : b.roi > 0 ? '#34d399' : '#f87171');
const brier = (v) => (v == null ? '—' : v.toFixed(4));

/** Small table helper: head = column labels, body = array of arrays of cells ({v, color} or primitive). */
function Table({ head, body }) {
  return (
    <div style={{ overflowX: 'auto' }}>
      <table style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>{head.map(h => <th key={h} style={th}>{h}</th>)}</tr></thead>
        <tbody>{body.map((r, i) => (
          <tr key={i} style={{ borderTop: '1px solid rgba(255,255,255,0.06)' }}>
            {r.map((c, j) => <td key={j} style={{ ...td, color: c?.color }}>{c?.v ?? c}</td>)}
          </tr>))}
        </tbody>
      </table>
    </div>
  );
}

export default function PickemResults({ weeks = [] }) {
  const [scope, setScope] = useState('all');
  const [minUsage, setMinUsage] = useState(3);
  const [closeH, setCloseH] = useState(6);
  const [res, setRes] = useState(null);
  const [error, setError] = useState(null);

  useEffect(() => {
    let alive = true;
    setRes(null); setError(null);
    ApiService.getPickemResults(scope === 'all' ? null : Number(scope), minUsage, closeH).then(r => {
      if (!alive) return;
      if (r.ok) setRes(r.data); else setError(r.error);
    });
    return () => { alive = false; };
  }, [scope, minUsage, closeH]);

  const s = res?.summary;
  const voidNonThin = res ? res.no_stat_line.filter(x => x.usage >= minUsage).length : 0;
  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      <div className="glass-panel" style={{ ...card, marginBottom: '18px', padding: '12px 20px', display: 'flex', gap: '18px', alignItems: 'center', flexWrap: 'wrap' }}>
        <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>🧾 Underdog Grading</span>
        <label style={{ fontSize: '0.85rem' }}><span style={{ color: 'var(--text-muted)' }}>Week </span>
          <select value={scope} onChange={e => setScope(e.target.value)} style={{ ...inputStyle, width: 'auto' }}>
            <option value="all">All weeks</option>
            {weeks.map(w => <option key={w} value={w}>{w}</option>)}
          </select></label>
        <label style={{ fontSize: '0.85rem' }} title="Players under this many sim touches+targets per game are left out of the numbers">
          <span style={{ color: 'var(--text-muted)' }}>Min usage </span>
          <input type="number" min="0" step="0.5" value={minUsage} onChange={e => setMinUsage(Number(e.target.value) || 0)} style={{ ...inputStyle, width: '60px' }} /></label>
        <label style={{ fontSize: '0.85rem' }} title="A closing multiplier only counts if the leg was re-pasted this many hours or fewer before kickoff">
          <span style={{ color: 'var(--text-muted)' }}>Close window (h) </span>
          <input type="number" min="0" step="1" value={closeH} onChange={e => setCloseH(Number(e.target.value) || 0)} style={{ ...inputStyle, width: '60px' }} /></label>
      </div>

      {error && <div style={{ color: '#f87171', marginBottom: '12px' }}>{error}</div>}
      {!res && !error && <div style={{ color: 'var(--text-muted)' }}>Grading…</div>}

      {res && (
        <>
          <div className="glass-panel" style={card}>
            <div style={{ fontSize: '0.88rem', color: 'var(--text-white)' }}>
              {res.rows.length} legs settled · {res.pending} waiting on games that aren't final · {res.void.length} pushes · {res.late} first captured after kickoff
            </div>
            <div style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginTop: '4px' }}>
              {voidNonThin} non-thin legs have no stat line (inactive → Underdog voids them; left out) · {res.unmatched.length} legs have no sim player (not graded).
              Nothing settles until a game is final and the stats are downloaded (Player Projections → refresh actuals).
            </div>
          </div>

          {(!s || s.n === 0) && (
            <div className="glass-panel" style={{ ...card, color: 'var(--text-muted)' }}>
              No settled legs yet. As the week's games finish and the real stat lines load, this page fills in.
            </div>
          )}

          {s && s.n > 0 && (
            <>
              <div className="glass-panel" style={card}>
                <h4 style={{ margin: '0 0 8px', color: 'var(--text-white)' }}>Do the legs we flag +EV pay?</h4>
                <Table head={['Multiplier used', 'Legs', 'Hit rate', 'Our avg prob', 'Return per $1 (± 1 SE)']}
                  body={[
                    ['Open — +EV legs', s.plus_ev.open.n, pct(s.plus_ev.open.hit_rate), pct(s.plus_ev.open.mean_sim), { v: roi(s.plus_ev.open), color: roiColor(s.plus_ev.open) }],
                    ['Close — +EV legs', s.plus_ev.close.n, pct(s.plus_ev.close.hit_rate), pct(s.plus_ev.close.mean_sim), { v: roi(s.plus_ev.close), color: roiColor(s.plus_ev.close) }],
                    ['Open — every leg', s.overall.open.n, pct(s.overall.open.hit_rate), pct(s.overall.open.mean_sim), { v: roi(s.overall.open), color: roiColor(s.overall.open) }],
                    ['Close — every leg', s.overall.close.n, pct(s.overall.close.hit_rate), pct(s.overall.close.mean_sim), { v: roi(s.overall.close), color: roiColor(s.overall.close) }],
                  ]} />
                <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginTop: '6px' }}>
                  "+EV" = our sim probability × multiplier &gt; 1 at that multiplier. If the open row beats the close row, early lines are the easier ones to beat.
                  "Every leg" is the margin check: it should sit below zero by roughly Underdog's hold.
                </div>
              </div>

              <div className="glass-panel" style={card}>
                <h4 style={{ margin: '0 0 8px', color: 'var(--text-white)' }}>Whose probability is better — ours or the market's?</h4>
                <Table head={['Against', 'Legs (both sides posted)', 'Our Brier', 'Market Brier', 'Better']}
                  body={['open', 'close'].map(k => {
                    const b = s.brier[k];
                    const better = b.sim == null ? '—' : b.sim < b.market ? 'Ours' : 'Market';
                    return [k === 'open' ? 'Opening line' : 'Closing line', b.n, brier(b.sim), brier(b.market), { v: better, color: better === 'Ours' ? '#34d399' : better === 'Market' ? '#f87171' : 'inherit' }];
                  })} />
                <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginTop: '6px' }}>
                  Brier = average squared miss of the stated probability (lower is better; always guessing 50% scores 0.25).
                  The market probability has Underdog's margin removed. This is the cleanest test of the model.
                </div>
              </div>

              <div className="glass-panel" style={card}>
                <h4 style={{ margin: '0 0 8px', color: 'var(--text-white)' }}>Bigger flagged edge — bigger payoff? (at the open)</h4>
                <Table head={['Our EV at open', 'Legs', 'Hit rate', 'Our avg prob', 'Return per $1 (± 1 SE)']}
                  body={s.ev_buckets.map(b => [b.label, b.n, pct(b.hit_rate), pct(b.mean_sim), { v: roi(b), color: roiColor(b) }])} />
              </div>

              <div className="glass-panel" style={card}>
                <h4 style={{ margin: '0 0 8px', color: 'var(--text-white)' }}>By market and side</h4>
                <Table head={['Group', 'Legs', 'Hit rate', 'Our avg prob', '+EV legs (open)', '+EV return', 'Our Brier', 'Market Brier']}
                  body={[...Object.entries(s.by_market), ...Object.entries(s.by_side)].map(([k, g]) =>
                    [k, g.n, pct(g.hit_rate), pct(g.mean_sim), g.plus_ev_open.n, { v: roi(g.plus_ev_open), color: roiColor(g.plus_ev_open) }, brier(g.brier_sim), brier(g.brier_market)])} />
              </div>

              <div className="glass-panel" style={card}>
                <h4 style={{ margin: '0 0 8px', color: 'var(--text-white)' }}>Calibration — when we say X%, how often does it happen?</h4>
                <Table head={['Our probability', 'Legs', 'Avg we said', 'Actually hit']}
                  body={s.calibration.map(c => [`${Math.round(c.lo * 100)}–${Math.round(c.hi * 100)}%`, c.n, pct(c.mean_sim), pct(c.hit_rate)])} />
              </div>
            </>
          )}

          {res.rows.length > 0 && (
            <div className="glass-panel" style={card}>
              <h4 style={{ margin: '0 0 8px', color: 'var(--text-white)' }}>Settled legs (best open EV first, up to 300)</h4>
              <Table head={['Wk', 'Player', 'Pick', 'Actual', 'Result', 'Open', 'Close', 'Our prob', 'Mkt open', 'EV open', 'EV close']}
                body={[...res.rows].sort((a, b) => b.ev_open - a.ev_open).slice(0, 300).map(r => [
                  r.week, `${r.player} (${r.team})`, `${r.market} ${r.side} ${r.line}`, r.actual,
                  { v: r.hit ? 'Hit' : 'Miss', color: r.hit ? '#34d399' : '#f87171' },
                  `${r.open_multiplier.toFixed(2)}x`, r.close_multiplier == null ? `stale (${r.close_age_h.toFixed(0)}h)` : `${r.close_multiplier.toFixed(2)}x`,
                  pct(r.sim_prob), pct(r.market_prob_open), `${r.ev_open > 0 ? '+' : ''}${r.ev_open.toFixed(0)}%`,
                  r.ev_close == null ? '—' : `${r.ev_close > 0 ? '+' : ''}${r.ev_close.toFixed(0)}%`])} />
            </div>
          )}
        </>
      )}
    </div>
  );
}
