import { useState } from 'react';

/**
 * "Range of outcomes" modal for one lineup -- ETR/Solver-style percentile-finish
 * histogram, driven by the `histogram` block the backend attaches to every lineup
 * in POST /api/optimize and /api/optimize_showdown (see `_compute_lineup_field_stats`
 * in src/api/app.py). Each of the sim's aligned iterations is one self-contained
 * "sim run": the lineup scored some points and, relative to the FULL simulated
 * field (every archetype-composed field lineup, scored in that same game-
 * environment draw -- not one random opponent), finished at some percentile.
 * This chart is the distribution of that percentile across every iteration --
 * bars at/above the cash line (`cash_percentile`) are shaded green, the rest
 * cyan, matching the ETR look.
 *
 * Props:
 *   lineup  - one lineup result object from the optimize response (must include
 *             `histogram`, `cash_percentile`, `ev_pct`, `itm_pct`,
 *             `score_min/mean/max`, `lineup_p50/p95`, `rank1_count`,
 *             `rank_top10_count`, `players`).
 *   onClose - fired on backdrop click or the × button.
 *
 * Pure inline SVG, no chart lib -- same pattern as components/GameDistribution.jsx.
 */

const STAT_LABEL = { fontSize: '0.66rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em', marginBottom: '4px' };
const STAT_VALUE = { fontSize: '1.15rem', fontWeight: 700 };

function Stat({ label, value, color, title }) {
  return (
    <div title={title} style={{ background: 'rgba(255,255,255,0.03)', borderRadius: '8px', padding: '8px 10px', textAlign: 'center' }}>
      <div style={STAT_LABEL}>{label}</div>
      <div style={{ ...STAT_VALUE, color: color || 'var(--text-white)' }}>{value}</div>
    </div>
  );
}

function HistogramChart({ histogram, cashPercentile }) {
  const W = 640, H = 260, padL = 34, padR = 10, padB = 28, padT = 10;
  const plotW = W - padL - padR, plotH = H - padB - padT;
  const [hover, setHover] = useState(null);
  const { bin_edges: edges, counts, n_sims } = histogram;
  const maxCount = Math.max(...counts, 1);
  const nb = counts.length;
  const bw = plotW / nb;

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} width="100%" style={{ display: 'block' }} onMouseLeave={() => setHover(null)}>
        {counts.map((c, i) => {
          const bh = (c / maxCount) * plotH;
          const binMid = (edges[i] + edges[i + 1]) / 2;
          const isCash = binMid >= cashPercentile;
          const fill = isCash ? 'var(--accent-green)' : 'var(--accent-primary)';
          return (
            <rect key={i} x={padL + i * bw + 0.5} y={padT + plotH - bh}
                  width={Math.max(bw - 1, 1)} height={Math.max(bh, 0.5)}
                  fill={fill} opacity={hover === i ? 1 : 0.68}
                  onMouseEnter={() => setHover(i)} />
          );
        })}
        <line x1={padL} y1={padT + plotH} x2={W - padR} y2={padT + plotH} stroke="var(--border-glass)" />
        {cashPercentile != null && cashPercentile >= 0 && cashPercentile <= 100 && (
          <line x1={padL + (cashPercentile / 100) * plotW} y1={padT}
                x2={padL + (cashPercentile / 100) * plotW} y2={padT + plotH}
                stroke="var(--accent-gold)" strokeWidth="1.5" strokeDasharray="4 3" />
        )}
        {Array.from({ length: 6 }, (_, k) => {
          const v = k * 20;
          return <text key={k} x={padL + (v / 100) * plotW} y={H - 12} fontSize="9" fill="var(--text-muted)" textAnchor="middle">{v}%</text>;
        })}
        <text x={W / 2} y={H - 2} fontSize="9" fill="var(--text-muted)" textAnchor="middle">finish percentile (higher = beat more of the field)</text>
        {Array.from({ length: 5 }, (_, k) => {
          const frac = k / 4;
          return <text key={k} x={padL - 6} y={padT + plotH - frac * plotH + 3} fontSize="9" fill="var(--text-muted)" textAnchor="end">{Math.round(frac * maxCount / n_sims * 100)}%</text>;
        })}
      </svg>
      <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', minHeight: '1.1em', marginTop: '2px' }}>
        {hover != null
          ? <span>{edges[hover].toFixed(0)}–{edges[hover + 1].toFixed(0)} percentile: <b style={{ color: 'var(--text-white)' }}>{counts[hover]}</b> of {n_sims} sim runs (<b style={{ color: 'var(--text-white)' }}>{(counts[hover] / n_sims * 100).toFixed(1)}%</b>)</span>
          : <span>hover a bar for the exact sim-run count · <span style={{ borderBottom: '1px dashed var(--accent-gold)' }}>gold line</span> = cash line ({cashPercentile}th pct)</span>}
      </div>
    </div>
  );
}

export default function LineupHistogramModal({ lineup, onClose }) {
  if (!lineup || !lineup.histogram) return null;
  const cpt = lineup.players?.find(p => p.slot === 'CPT');
  const flex = lineup.players?.filter(p => p.slot !== 'CPT') || [];
  const nm = p => (p.pos === 'DST' ? `${p.team} DST` : p.name);

  return (
    <div onClick={onClose} style={{
      position: 'fixed', inset: 0, background: 'rgba(3,6,16,0.72)', backdropFilter: 'blur(2px)',
      display: 'flex', alignItems: 'center', justifyContent: 'center', zIndex: 1000, padding: '16px',
    }}>
      <div onClick={e => e.stopPropagation()} style={{
        background: 'var(--bg-deep)', border: '1px solid var(--border-glass)', borderRadius: '14px',
        padding: '18px 20px', maxWidth: '720px', width: '100%', maxHeight: '88vh', overflowY: 'auto',
        boxShadow: 'var(--shadow-main)',
      }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: '10px' }}>
          <div>
            <h2 style={{ margin: 0, fontSize: '1.05rem' }}>Range of Outcomes</h2>
            <div style={{ fontSize: '0.76rem', color: 'var(--text-muted)', marginTop: '3px' }}>
              {cpt && <span><b style={{ color: 'var(--accent-primary)' }}>CPT</b> {nm(cpt)}</span>}
              {flex.length > 0 && <span> · {flex.map(nm).join(', ')}</span>}
            </div>
          </div>
          <button onClick={onClose} style={{
            background: 'none', border: '1px solid var(--border-glass)', borderRadius: '6px',
            color: 'var(--text-muted)', cursor: 'pointer', fontSize: '0.9rem', padding: '2px 9px', lineHeight: '1.4',
          }}>×</button>
        </div>

        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(88px, 1fr))', gap: '8px', marginBottom: '14px' }}>
          <Stat label="Sim ROI" value={`${lineup.ev_pct > 0 ? '+' : ''}${lineup.ev_pct}%`}
                color={lineup.ev_pct >= 0 ? 'var(--accent-green)' : 'var(--accent-red)'}
                title="EV%% computed by ranking this lineup against the full simulated field (not one random opponent) in every sim run, looking up the real payout at its implied rank, then averaging" />
          <Stat label="Cash Rate" value={`${lineup.itm_pct}%`} />
          <Stat label="Ceiling" value={lineup.score_max} color="var(--accent-gold)" />
          <Stat label="Average" value={lineup.score_mean} />
          <Stat label="Floor" value={lineup.score_min} />
          <Stat label="Top 1%" value={`${lineup.top1_pct}%`} />
          <Stat label="1st Place" value={lineup.rank1_count} title={`out of ${lineup.histogram.n_sims} sim runs`} />
          <Stat label="1-10th Place" value={lineup.rank_top10_count} title={`out of ${lineup.histogram.n_sims} sim runs`} />
        </div>

        <HistogramChart histogram={lineup.histogram} cashPercentile={lineup.cash_percentile} />

        <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', marginTop: '10px', lineHeight: 1.5 }}>
          Each sim run draws one full-slate game environment, scores this lineup in it, and ranks it against
          a field sampled from that same environment. The bars show how often this lineup's finish landed in
          each percentile band across all {lineup.histogram.n_sims.toLocaleString()} sim runs.
        </div>
      </div>
    </div>
  );
}
