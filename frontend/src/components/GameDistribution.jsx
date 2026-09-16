import { useEffect, useMemo, useRef, useState } from 'react';

/**
 * Game-outcome distribution panel for the Simulator's "Sim Game Summary" view.
 *
 * Props:
 *   dist  - the `game_distribution` block from the sim response:
 *     { n, away_team, home_team, ref_total, ref_spread_home,
 *       mean_total, mean_margin_away,
 *       total:  { lo, step, nb, p:[] },              // weighted total-points histogram
 *       margin: { lo, step, nb, p:[] },              // weighted (away - home) histogram
 *       joint:  { t_lo,t_step,t_nb, m_lo,m_step,m_nb, cells:[[mi,ti,p], ...] },
 *       raw:    { iteration:[], total:[], margin:[], weight:[]|null } | null }
 *     The `week_sim_results` payload omits `raw`; a fresh POST /api/simulate includes it.
 *   onSelect(sel|null) - fired when the user drags a box on the joint heatmap.
 *     sel = { tLo,tHi,mLo,mHi, idx:[iteration ids], frac, condMeanTotal,
 *             condMeanMargin, condWinAway, condOver }. Needs `dist.raw`.
 *     Phase 2 (conditional lineup re-scoring) consumes `idx`.
 *
 * Pure inline SVG, no chart lib, theme-aware via the app's CSS vars.
 */

const CARD = { background: 'rgba(11,17,38,0.5)', border: '1px solid var(--border-glass)', borderRadius: '12px', padding: '14px' };
const LBL = { fontSize: '0.75rem', color: 'var(--text-muted)', textTransform: 'uppercase', marginBottom: '6px', fontWeight: 600 };

function Bars({ h, accent, meanVal, refVal, refLabel, xlabel, signed }) {
  const W = 320, H = 150, padL = 4, padR = 4, padB = 26, padT = 8;
  const plotW = W - padL - padR, plotH = H - padB - padT;
  const [hover, setHover] = useState(null);
  const maxP = Math.max(...h.p, 1e-9);
  const bw = plotW / h.nb;
  const span = h.step * h.nb;
  const xOf = (v) => padL + ((v - h.lo) / span) * plotW;
  const fmt = (v) => (signed && v > 0 ? `+${Math.round(v)}` : `${Math.round(v)}`);
  const fmtRef = (v) => (signed && v > 0 ? `+${v}` : `${v}`);

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} width="100%" style={{ display: 'block' }} onMouseLeave={() => setHover(null)}>
        {h.p.map((p, i) => {
          const bh = (p / maxP) * plotH;
          return (
            <rect key={i} x={padL + i * bw + 0.5} y={padT + plotH - bh}
                  width={Math.max(bw - 1, 1)} height={bh}
                  fill={accent} opacity={hover === i ? 1 : 0.62}
                  onMouseEnter={() => setHover(i)} />
          );
        })}
        <line x1={padL} y1={padT + plotH} x2={W - padR} y2={padT + plotH} stroke="var(--border-glass)" />
        {meanVal != null && <line x1={xOf(meanVal)} y1={padT} x2={xOf(meanVal)} y2={padT + plotH} stroke={accent} strokeWidth="1.5" />}
        {refVal != null && <line x1={xOf(refVal)} y1={padT} x2={xOf(refVal)} y2={padT + plotH} stroke="var(--text-muted)" strokeWidth="1.5" strokeDasharray="4 3" />}
        {Array.from({ length: 5 }, (_, k) => {
          const v = h.lo + (k / 4) * span;
          return <text key={k} x={xOf(v)} y={H - 14} fontSize="9" fill="var(--text-muted)" textAnchor="middle">{fmt(v)}</text>;
        })}
        <text x={W / 2} y={H - 3} fontSize="9" fill="var(--text-muted)" textAnchor="middle">{xlabel}</text>
      </svg>
      <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', minHeight: '1em' }}>
        {hover != null
          ? <span>{fmt(h.lo + hover * h.step)}–{fmt(h.lo + (hover + 1) * h.step)}: <b style={{ color: accent }}>{(h.p[hover] * 100).toFixed(1)}%</b></span>
          : <span>mean <b style={{ color: accent }}>{meanVal?.toFixed?.(1)}</b>{refVal != null && <> · <span style={{ borderBottom: '1px dashed var(--text-muted)' }}>{refLabel} {fmtRef(refVal)}</span></>}</span>}
      </div>
    </div>
  );
}

export default function GameDistribution({ dist, onSelect }) {
  const svgRef = useRef(null);
  const [drag, setDrag] = useState(null);
  const [sel, setSel] = useState(null);

  const grid = useMemo(() => {
    if (!dist?.joint) return null;
    const j = dist.joint;
    const g = Array.from({ length: j.m_nb }, () => new Array(j.t_nb).fill(0));
    let gmax = 0;
    for (const [mi, ti, p] of j.cells) { g[mi][ti] = p; if (p > gmax) gmax = p; }
    return { g, gmax, ...j };
  }, [dist]);

  const conditional = useMemo(() => {
    if (!sel || !grid) return null;
    const refT = dist.ref_total;

    // Exact path: per-iteration arrays (only in a fresh /api/simulate response).
    if (dist.raw) {
      const { total, margin, weight, iteration } = dist.raw;
      const idx = [];
      let w = 0, sT = 0, sM = 0, wa = 0, ov = 0, wAll = 0;
      for (let i = 0; i < total.length; i++) {
        const wi = weight ? weight[i] : 1;
        wAll += wi;
        if (total[i] < sel.tLo || total[i] >= sel.tHi || margin[i] < sel.mLo || margin[i] >= sel.mHi) continue;
        idx.push(iteration[i]);
        w += wi; sT += total[i] * wi; sM += margin[i] * wi;
        if (margin[i] > 0) wa += wi;
        if (refT != null && total[i] > refT) ov += wi;
      }
      if (w === 0) return { empty: true, idx: [] };
      return { exact: true, idx, frac: w / wAll, condMeanTotal: sT / w, condMeanMargin: sM / w,
               condWinAway: wa / w, condOver: refT != null ? ov / w : null };
    }

    // Approx path: the joint grid only (week_sim_results payload). Bin-center
    // weighted - good enough for a read; no iteration ids for lineup re-scoring.
    let w = 0, sT = 0, sM = 0, wa = 0, ov = 0;
    for (const [mi, ti, p] of grid.cells) {
      const tc = grid.t_lo + (ti + 0.5) * grid.t_step;
      const mc = grid.m_lo + (mi + 0.5) * grid.m_step;
      if (tc < sel.tLo || tc >= sel.tHi || mc < sel.mLo || mc >= sel.mHi) continue;
      w += p; sT += tc * p; sM += mc * p;
      if (mc > 0) wa += p;
      if (refT != null && tc > refT) ov += p;
    }
    if (w === 0) return { empty: true, idx: [] };
    return { exact: false, idx: [], frac: w, condMeanTotal: sT / w, condMeanMargin: sM / w,
             condWinAway: wa / w, condOver: refT != null ? ov / w : null };
  }, [dist, sel, grid]);

  useEffect(() => {
    if (!onSelect) return;
    if (sel && conditional && !conditional.empty) onSelect({ ...sel, ...conditional });
    else if (!sel) onSelect(null);
  }, [sel, conditional]); // eslint-disable-line react-hooks/exhaustive-deps

  if (!dist || !dist.total || !grid) {
    return <div style={{ color: 'var(--text-muted)', fontSize: '0.85rem', padding: '12px' }}>
      No outcome distribution in this result — re-run the engine to populate it.
    </div>;
  }

  const away = dist.away_team, home = dist.home_team;

  // joint heatmap geometry
  const HW = 460, HH = 300, mL = 46, mB = 34, mT = 10, mR = 10;
  const pw = HW - mL - mR, ph = HH - mT - mB;
  const cellW = pw / grid.t_nb, cellH = ph / grid.m_nb;
  const tSpan = grid.t_step * grid.t_nb, mSpan = grid.m_step * grid.m_nb;
  const xData = (px) => grid.t_lo + ((px - mL) / pw) * tSpan;
  const yData = (py) => grid.m_lo + ((mT + ph - py) / ph) * mSpan;
  const xPix = (v) => mL + ((v - grid.t_lo) / tSpan) * pw;
  const yPix = (v) => mT + ph - ((v - grid.m_lo) / mSpan) * ph;
  const snap = (v, lo, step) => lo + Math.round((v - lo) / step) * step;

  const evt = (e) => {
    const r = svgRef.current.getBoundingClientRect();
    const x = ((e.clientX - r.left) / r.width) * HW;
    const y = ((e.clientY - r.top) / r.height) * HH;
    return { x: Math.max(mL, Math.min(mL + pw, x)), y: Math.max(mT, Math.min(mT + ph, y)) };
  };
  const onDown = (e) => { const p = evt(e); setDrag({ x0: p.x, y0: p.y, x1: p.x, y1: p.y }); setSel(null); };
  const onMove = (e) => { if (!drag) return; const p = evt(e); setDrag({ ...drag, x1: p.x, y1: p.y }); };
  const onUp = () => {
    if (!drag) return;
    const tA = snap(xData(Math.min(drag.x0, drag.x1)), grid.t_lo, grid.t_step);
    const tB = snap(xData(Math.max(drag.x0, drag.x1)), grid.t_lo, grid.t_step);
    const mA = snap(yData(Math.max(drag.y0, drag.y1)), grid.m_lo, grid.m_step);
    const mB2 = snap(yData(Math.min(drag.y0, drag.y1)), grid.m_lo, grid.m_step);
    setDrag(null);
    if (tB - tA < grid.t_step * 0.5 || mB2 - mA < grid.m_step * 0.5) { setSel(null); return; }
    setSel({ tLo: tA, tHi: tB, mLo: mA, mHi: mB2 });
  };

  const fmtM = (v) => (v > 0 ? `+${Math.round(v)}` : `${Math.round(v)}`);

  return (
    <div style={{ marginBottom: '24px' }}>
      <h3>Score Distribution <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontWeight: 400 }}>· {dist.n.toLocaleString()} sims</span></h3>

      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '14px', marginTop: '10px' }}>
        <div style={CARD}>
          <div style={LBL}>Total Points</div>
          <Bars h={dist.total} accent="var(--accent-primary)" meanVal={dist.mean_total}
                refVal={dist.ref_total} refLabel="Vegas" xlabel="combined points" />
        </div>
        <div style={CARD}>
          <div style={LBL}>Score Differential <span style={{ textTransform: 'none', fontWeight: 400 }}>({away} − {home})</span></div>
          <Bars h={dist.margin} accent="var(--accent-gold)" meanVal={dist.mean_margin_away}
                refVal={dist.ref_spread_home != null ? -dist.ref_spread_home : null} refLabel="Vegas line"
                xlabel={`◀ ${home} favored     ${away} favored ▶`} signed />
        </div>
      </div>

      <div style={{ ...CARD, marginTop: '14px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline' }}>
          <div style={LBL}>Joint Outcome — drag to select a range of games</div>
          {sel && <button onClick={() => setSel(null)} style={{ fontSize: '0.72rem', background: 'none', border: '1px solid var(--border-glass)', color: 'var(--text-muted)', borderRadius: '5px', padding: '2px 8px', cursor: 'pointer' }}>clear</button>}
        </div>

        <div style={{ display: 'grid', gridTemplateColumns: 'minmax(0,1fr) 188px', gap: '14px', alignItems: 'center' }}>
          <svg ref={svgRef} viewBox={`0 0 ${HW} ${HH}`} width="100%"
               style={{ display: 'block', cursor: 'crosshair', userSelect: 'none' }}
               onMouseDown={onDown} onMouseMove={onMove} onMouseUp={onUp} onMouseLeave={onUp}>
            {grid.g.map((rowArr, mi) => rowArr.map((p, ti) => p > 0 && (
              <rect key={`${mi}-${ti}`} x={mL + ti * cellW} y={mT + (grid.m_nb - 1 - mi) * cellH}
                    width={cellW + 0.5} height={cellH + 0.5}
                    fill="var(--accent-primary)" opacity={0.1 + 0.9 * Math.sqrt(p / grid.gmax)} />
            )))}
            <line x1={mL} y1={yPix(0)} x2={mL + pw} y2={yPix(0)} stroke="var(--text-muted)" strokeWidth="1" opacity="0.5" />
            {dist.ref_total != null && <line x1={xPix(dist.ref_total)} y1={mT} x2={xPix(dist.ref_total)} y2={mT + ph} stroke="var(--text-muted)" strokeDasharray="3 3" opacity="0.5" />}
            {dist.ref_spread_home != null && <line x1={mL} y1={yPix(-dist.ref_spread_home)} x2={mL + pw} y2={yPix(-dist.ref_spread_home)} stroke="var(--text-muted)" strokeDasharray="3 3" opacity="0.5" />}
            {drag && <rect x={Math.min(drag.x0, drag.x1)} y={Math.min(drag.y0, drag.y1)} width={Math.abs(drag.x1 - drag.x0)} height={Math.abs(drag.y1 - drag.y0)} fill="var(--accent-gold)" opacity="0.18" stroke="var(--accent-gold)" />}
            {sel && !drag && <rect x={xPix(sel.tLo)} y={yPix(sel.mHi)} width={xPix(sel.tHi) - xPix(sel.tLo)} height={yPix(sel.mLo) - yPix(sel.mHi)} fill="var(--accent-gold)" opacity="0.14" stroke="var(--accent-gold)" strokeWidth="1.5" />}
            <line x1={mL} y1={mT + ph} x2={mL + pw} y2={mT + ph} stroke="var(--border-glass)" />
            <line x1={mL} y1={mT} x2={mL} y2={mT + ph} stroke="var(--border-glass)" />
            {Array.from({ length: 5 }, (_, k) => {
              const v = grid.t_lo + (k / 4) * tSpan;
              return <text key={k} x={xPix(v)} y={HH - 18} fontSize="9" fill="var(--text-muted)" textAnchor="middle">{Math.round(v)}</text>;
            })}
            <text x={mL + pw / 2} y={HH - 4} fontSize="9.5" fill="var(--text-muted)" textAnchor="middle">total points</text>
            {Array.from({ length: 5 }, (_, k) => {
              const v = grid.m_lo + (k / 4) * mSpan;
              return <text key={k} x={mL - 6} y={yPix(v) + 3} fontSize="9" fill="var(--text-muted)" textAnchor="end">{fmtM(v)}</text>;
            })}
            <text transform={`translate(11 ${mT + ph / 2}) rotate(-90)`} fontSize="9.5" fill="var(--text-muted)" textAnchor="middle">{away} − {home}</text>
          </svg>

          <div style={{ fontSize: '0.78rem', lineHeight: 1.5 }}>
            {!sel && <div style={{ color: 'var(--text-muted)' }}>Drag a box on the heatmap for the conditional outcome of that slice of games.</div>}
            {sel && conditional?.empty && <div style={{ color: 'var(--accent-red)' }}>No sims in that range.</div>}
            {sel && conditional && !conditional.empty && (
              <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                <div style={{ fontWeight: 700, color: 'var(--accent-gold)' }}>{(conditional.frac * 100).toFixed(1)}% of games</div>
                <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>
                  total {Math.round(sel.tLo)}–{Math.round(sel.tHi)} · {away} {fmtM(sel.mLo)} to {fmtM(sel.mHi)}
                </div>
                <hr style={{ border: 0, borderTop: '1px solid var(--border-glass)', margin: '4px 0' }} />
                <div>mean total <b>{conditional.condMeanTotal.toFixed(1)}</b></div>
                <div>mean margin <b>{fmtM(conditional.condMeanMargin)}</b> {conditional.condMeanMargin >= 0 ? away : home}</div>
                <div>{away} win <b>{(conditional.condWinAway * 100).toFixed(0)}%</b></div>
                {conditional.condOver != null && <div>over {dist.ref_total} <b>{(conditional.condOver * 100).toFixed(0)}%</b></div>}
                <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', marginTop: '3px' }}>
                  {conditional.exact
                    ? <>{conditional.idx.length} sims{onSelect ? ' → lineup re-scoring' : ''}</>
                    : 'approx from binned grid — run a fresh sim for exact'}
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
