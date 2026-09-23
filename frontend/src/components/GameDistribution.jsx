import { useEffect, useMemo, useRef, useState } from 'react';

/**
 * Game-outcome distribution panel for the Game Explorer's "Sim Game Summary"
 * view (also embedded in the Showdown Optimizer).
 *
 * Props:
 *   dist  - the `game_distribution` block from the sim response:
 *     { n, away_team, home_team, ref_total, ref_spread_home,
 *       mean_total, mean_margin_away,
 *       total:  { lo, step, nb, p:[], under_label?, over_label? },  // weighted total-points histogram
 *       margin: { lo, step, nb, p:[], under_label?, over_label? },  // weighted (away - home) histogram
 *       joint:  { t_lo,t_step,t_nb, m_lo,m_step,m_nb, cells:[[mi,ti,p], ...], clamped? },
 *       raw:    { iteration:[], total:[], margin:[], weight:[]|null } | null }
 *     under_label/over_label/clamped (2026-09-22): the first/last bin is a
 *     catch-all ("<10", "80+", "≤−28", "≥+28") -- see app.py's
 *     _build_game_distribution. The joint grid uses coarser 5x5-pt cells than
 *     the 1D bars (1-pt margin / 2-pt total). Older payloads are upgraded by
 *     normalizeDist when they carry `raw`, else rendered as-is.
 *   onSelect(sel|null) - fired when the user drags a box on the joint heatmap.
 *     sel = { tLo,tHi,mLo,mHi, idx:[iteration ids], frac, condMeanTotal,
 *             condMeanMargin, condWinAway, condOver }. Needs `dist.raw`.
 *     The Showdown Optimizer consumes `idx` for conditional lineup re-scoring.
 *
 * Score exploration (2026-09-22, needs `dist.raw`): per-sim final scores are
 * exact from raw -- away = (total + margin) / 2, home = (total - margin) / 2
 * -- so every breakdown below is computed client side, no extra payload:
 *   - click a Total / Differential bar, or drag a heatmap box -> "Most common
 *     final scores" for that slice (no selection = all games)
 *   - "When <team> scores X" -> distribution of the other team's score
 *
 * Pure inline SVG, no chart lib, theme-aware via the app's CSS vars.
 */

const CARD = { background: 'rgba(11,17,38,0.5)', border: '1px solid var(--border-glass)', borderRadius: '12px', padding: '14px' };
const LBL = { fontSize: '0.75rem', color: 'var(--text-muted)', textTransform: 'uppercase', marginBottom: '6px', fontWeight: 600 };
const HINT = { textTransform: 'none', fontWeight: 400, fontSize: '0.7rem' };
const SMALL_BTN = { fontSize: '0.72rem', background: 'none', border: '1px solid var(--border-glass)', color: 'var(--text-muted)', borderRadius: '5px', padding: '2px 8px', cursor: 'pointer' };
const MARGIN_CAP = 28;

const pct = (x, d = 1) => `${(x * 100).toFixed(d)}%`;
const fmtSigned = (v) => (v > 0 ? `+${Math.round(v)}` : `${Math.round(v)}`);

/**
 * Hover/readout label for histogram bin i.
 * Inputs: h (histogram block, see header), i (int bin index), fmt (value formatter).
 * Output: string -- the catch-all label for a clamped edge bin, the single
 * value for a 1-pt bin centred on an integer, else "a–b" (inclusive, since
 * scores are integers: bin [a, a+step) holds a .. a+step-1).
 */
function binLabel(h, i, fmt) {
  if (h.under_label && i === 0) return h.under_label;
  if (h.over_label && i === h.nb - 1) return h.over_label;
  const a = h.lo + i * h.step;
  if (h.step === 1) return fmt(Math.round(a + 0.5));
  return `${fmt(Math.ceil(a))}–${fmt(Math.ceil(a + h.step) - 1)}`;
}

/**
 * Bin index of value v in histogram geometry h ({lo, step, nb}).
 * Output: int in [0, nb-1] -- out-of-range values land in the edge bins,
 * which is exactly the catch-all behaviour of a clamped histogram.
 */
function binOf(h, v) {
  return Math.min(h.nb - 1, Math.max(0, Math.floor((v - h.lo) / h.step)));
}

/**
 * Axis ticks for a histogram.
 * Inputs: h (histogram block), values (numbers to tick between the ends
 * when clamped), fmt (value formatter).
 * Output: [{ v, label }] -- for a clamped histogram the two ends sit on the
 * catch-all bins' centres with their labels; otherwise the old 5 evenly
 * spaced ticks across the auto range (older cached payloads).
 */
function histTicks(h, values, fmt) {
  if (h.under_label || h.over_label) {
    const first = h.lo + h.step / 2, last = h.lo + (h.nb - 0.5) * h.step;
    return [
      { v: first, label: h.under_label || fmt(first) },
      ...values.filter(v => v > first + h.step && v < last - h.step).map(v => ({ v, label: fmt(v) })),
      { v: last, label: h.over_label || fmt(last) },
    ];
  }
  const span = h.step * h.nb;
  return Array.from({ length: 5 }, (_, k) => { const v = h.lo + (k / 4) * span; return { v, label: fmt(v) }; });
}

/** textAnchor/dx so the two end tick labels sit inside the plot instead of clipping. */
const edgeAnchor = (k, n) => ({
  textAnchor: k === 0 ? 'start' : k === n - 1 ? 'end' : 'middle',
  dx: k === 0 ? -3 : k === n - 1 ? 3 : 0,
});

/**
 * Upgrade an old-layout distribution (auto-ranged bins, no catch-all edges)
 * to the current clamped layout, exactly, from its per-sim `raw` arrays.
 * Inputs: dist (game_distribution block).
 * Output: the same object if it is already the current layout or has no `raw` to
 * rebuild from; otherwise a copy whose total/margin/joint mirror app.py's
 * _build_game_distribution (keep the two in sync): total "<10" | 2-pt bins
 * | "80+", margin "≤−28" | 1-pt bins | "≥+28", joint 5x5-pt cells.
 * Purpose: cached payloads (week_sim_results' on-disk JSON, an earlier Run
 * Engine result) can predate the layout change and are expensive to
 * regenerate -- rebinning here is exact and source-agnostic.
 */
export const DIST_LAYOUT = 2;   // must match app.py's _build_game_distribution "layout"

export function normalizeDist(dist) {
  if (!dist || dist.layout === DIST_LAYOUT || !dist.raw?.total?.length) return dist;
  const { total, margin, weight } = dist.raw;
  const CAP = MARGIN_CAP;
  const T = { lo: 8, step: 2, nb: 37 }, M = { lo: -CAP - 0.5, step: 1, nb: 2 * CAP + 1 };
  const JT = { lo: 5, step: 5, nb: 16 }, JM = { lo: -CAP - 4.5, step: 5, nb: 13 };
  const tp = new Array(T.nb).fill(0), mp = new Array(M.nb).fill(0);
  const joint = new Map();
  let wsum = 0;
  for (let i = 0; i < total.length; i++) {
    const w = weight ? weight[i] : 1;
    const m = Math.min(CAP, Math.max(-CAP, Math.round(margin[i])));
    tp[binOf(T, total[i])] += w; mp[binOf(M, m)] += w; wsum += w;
    const k = binOf(JM, m) * JT.nb + binOf(JT, total[i]);
    joint.set(k, (joint.get(k) || 0) + w);
  }
  wsum = wsum || 1;
  const cells = [...joint.entries()].map(([k, w]) => [Math.floor(k / JT.nb), k % JT.nb, w / wsum]);
  return {
    ...dist,
    layout: DIST_LAYOUT,
    total: { ...T, p: tp.map(x => x / wsum), under_label: '<10', over_label: '80+' },
    margin: { ...M, p: mp.map(x => x / wsum), under_label: `≤−${CAP}`, over_label: `≥+${CAP}` },
    joint: { t_lo: JT.lo, t_step: JT.step, t_nb: JT.nb, m_lo: JM.lo, m_step: JM.step, m_nb: JM.nb, cells, clamped: true },
  };
}

/**
 * Open-ended numeric bounds for a heatmap box selection.
 * Inputs: sel ({tLo,tHi,mLo,mHi} snapped to grid edges), grid (joint block).
 * Output: {tLo,tHi,mLo,mHi} where a side touching a clamped catch-all edge is
 * ±Infinity -- so e.g. "≥+28" includes a +41 sim instead of silently
 * dropping it from the lineup re-scoring filter (Showdown consumes idx).
 */
function boxBounds(sel, grid) {
  const tTop = grid.t_lo + grid.t_nb * grid.t_step, mTop = grid.m_lo + grid.m_nb * grid.m_step;
  return {
    tLo: grid.clamped && sel.tLo <= grid.t_lo ? -Infinity : sel.tLo,
    tHi: grid.clamped && sel.tHi >= tTop ? Infinity : sel.tHi,
    mLo: grid.clamped && sel.mLo <= grid.m_lo ? -Infinity : sel.mLo,
    mHi: grid.clamped && sel.mHi >= mTop ? Infinity : sel.mHi,
  };
}

/**
 * Inclusive integer label for a half-open selected range [lo, hi) of scores.
 * Inputs: lo/hi (snapped edges), gLo/gHi (grid extent), clamped (bool), f (formatter).
 * Output: "a to b" | "b or less" | "a or more" | "any".
 */
function rangeLabel(lo, hi, gLo, gHi, clamped, f) {
  const a = Math.ceil(lo), b = Math.ceil(hi) - 1;
  const openLo = clamped && lo <= gLo, openHi = clamped && hi >= gHi;
  if (openLo && openHi) return 'any';
  if (openLo) return `${f(b)} or less`;
  if (openHi) return `${f(a)} or more`;
  return a === b ? f(a) : `${f(a)} to ${f(b)}`;
}

/**
 * Per-sim final scores, recovered exactly from dist.raw.
 * Inputs: dist (normalized game_distribution).
 * Output: { away:Int[], home:Int[], total:Int[], margin:Int[], w:Number[], wAll } or
 * null when the payload has no per-sim arrays (binned-only cache).
 */
function buildScores(dist) {
  const raw = dist?.raw;
  if (!raw?.total?.length) return null;
  const n = raw.total.length;
  const away = new Array(n), home = new Array(n), w = new Array(n);
  let wAll = 0;
  for (let i = 0; i < n; i++) {
    away[i] = Math.round((raw.total[i] + raw.margin[i]) / 2);
    home[i] = Math.round((raw.total[i] - raw.margin[i]) / 2);
    w[i] = raw.weight ? raw.weight[i] : 1;
    wAll += w[i];
  }
  return { away, home, total: raw.total, margin: raw.margin, w, wAll: wAll || 1 };
}

/**
 * Ranked final scores among the sims passing `keep`.
 * Inputs: s (buildScores output), keep (i -> bool), limit (int).
 * Output: { rows:[{a,h,w}] top `limit` by weight, wSel (total weight kept) }.
 */
function topScores(s, keep, limit = 10) {
  const byScore = new Map();
  let wSel = 0;
  for (let i = 0; i < s.away.length; i++) {
    if (!keep(i)) continue;
    const k = s.away[i] * 1000 + s.home[i];
    byScore.set(k, (byScore.get(k) || 0) + s.w[i]);
    wSel += s.w[i];
  }
  const rows = [...byScore.entries()]
    .map(([k, w]) => ({ a: Math.floor(k / 1000), h: k % 1000, w }))
    .sort((x, y) => y.w - x.w)
    .slice(0, limit);
  return { rows, wSel };
}

function Bars({ h, accent, meanVal, refVal, refLabel, xlabel, signed, tickValues, activeBin, onBinClick }) {
  const W = 320, H = 150, padL = 4, padR = 4, padB = 26, padT = 8;
  const plotW = W - padL - padR, plotH = H - padB - padT;
  const [hover, setHover] = useState(null);
  const maxP = Math.max(...h.p, 1e-9);
  const bw = plotW / h.nb;
  const span = h.step * h.nb;
  const xOf = (v) => padL + ((v - h.lo) / span) * plotW;
  const fmt = (v) => (signed && v > 0 ? `+${Math.round(v)}` : `${Math.round(v)}`);
  const fmtRef = (v) => (signed && v > 0 ? `+${v}` : `${v}`);
  const ticks = histTicks(h, tickValues || [], fmt);
  const shown = hover ?? activeBin;

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} width="100%" style={{ display: 'block' }} onMouseLeave={() => setHover(null)}>
        {h.p.map((p, i) => {
          const bh = (p / maxP) * plotH;
          const opacity = activeBin != null ? (i === activeBin ? 1 : hover === i ? 0.7 : 0.28) : (hover === i ? 1 : 0.62);
          return (
            <g key={i} onMouseEnter={() => setHover(i)}
               onClick={onBinClick ? () => onBinClick(i) : undefined}
               style={{ cursor: onBinClick ? 'pointer' : 'default' }}>
              {/* full-height transparent hit area so short bars are still easy to click */}
              <rect x={padL + i * bw} y={padT} width={bw} height={plotH} fill="transparent" />
              <rect x={padL + i * bw + 0.5} y={padT + plotH - bh}
                    width={Math.max(bw - 1, 1)} height={bh} fill={accent} opacity={opacity} />
            </g>
          );
        })}
        <line x1={padL} y1={padT + plotH} x2={W - padR} y2={padT + plotH} stroke="var(--border-glass)" />
        {meanVal != null && <line x1={xOf(meanVal)} y1={padT} x2={xOf(meanVal)} y2={padT + plotH} stroke={accent} strokeWidth="1.5" pointerEvents="none" />}
        {refVal != null && <line x1={xOf(refVal)} y1={padT} x2={xOf(refVal)} y2={padT + plotH} stroke="var(--text-muted)" strokeWidth="1.5" strokeDasharray="4 3" pointerEvents="none" />}
        {ticks.map(({ v, label }, k) => (
          <text key={k} x={xOf(v)} y={H - 14} fontSize="9" fill="var(--text-muted)" {...edgeAnchor(k, ticks.length)}>{label}</text>
        ))}
        <text x={W / 2} y={H - 3} fontSize="9" fill="var(--text-muted)" textAnchor="middle">{xlabel}</text>
      </svg>
      <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', minHeight: '1em' }}>
        {shown != null
          ? <span>{binLabel(h, shown, fmt)}: <b style={{ color: accent }}>{pct(h.p[shown])}</b></span>
          : <span>mean <b style={{ color: accent }}>{meanVal?.toFixed?.(1)}</b>{refVal != null && <> · <span style={{ borderBottom: '1px dashed var(--text-muted)' }}>{refLabel} {fmtRef(refVal)}</span></>}</span>}
      </div>
    </div>
  );
}

/**
 * Ranked list of final scores with inline bars.
 * Inputs: rows ([{a,h,w}] from topScores), wSel (weight of the slice),
 * wAll (weight of all sims), away/home (team abbrs).
 */
function ScoreList({ rows, wSel, wAll, away, home }) {
  if (!rows.length) return <div style={{ color: 'var(--accent-red)', fontSize: '0.8rem' }}>No sims in this slice.</div>;
  const top = rows[0].w;
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '3px' }}>
      {rows.map(({ a, h, w }, k) => (
        <div key={`${a}-${h}`} style={{ display: 'grid', gridTemplateColumns: '14px minmax(92px, auto) 1fr 44px', gap: '8px', alignItems: 'center', fontSize: '0.78rem' }}>
          <span style={{ color: 'var(--text-muted)', fontSize: '0.68rem' }}>{k + 1}</span>
          <span style={{ whiteSpace: 'nowrap' }}>
            <span style={{ fontWeight: a > h ? 700 : 400 }}>{away} {a}</span>
            <span style={{ color: 'var(--text-muted)' }}> – </span>
            <span style={{ fontWeight: h > a ? 700 : 400 }}>{home} {h}</span>
          </span>
          <span style={{ height: '8px', borderRadius: '3px', background: 'var(--accent-primary)', opacity: 0.7, width: `${(w / top) * 100}%` }} />
          <span style={{ textAlign: 'right', fontVariantNumeric: 'tabular-nums' }} title={`${pct(w / wAll, 2)} of all games`}>{pct(w / wSel)}</span>
        </div>
      ))}
    </div>
  );
}

/**
 * "When <team> scores X" card: the other team's score distribution given a
 * condition on one team's score.
 * Inputs: s (buildScores output), away/home (team abbrs).
 * State: side ('away'|'home'), cmp ('eq'|'ge'|'le'), score (int|null -> the
 * conditioned team's most common score).
 */
function ConditionalScore({ s, away, home }) {
  const [side, setSide] = useState('home');
  const [cmp, setCmp] = useState('eq');
  const [score, setScore] = useState(null);

  const mine = side === 'away' ? s.away : s.home;
  const theirs = side === 'away' ? s.home : s.away;
  const team = side === 'away' ? away : home, opp = side === 'away' ? home : away;

  // This team's own score frequencies -> dropdown options + default (mode).
  const options = useMemo(() => {
    const f = new Map();
    for (let i = 0; i < mine.length; i++) f.set(mine[i], (f.get(mine[i]) || 0) + s.w[i]);
    return [...f.entries()].sort((x, y) => x[0] - y[0]);
  }, [mine, s]);
  const mode = options.reduce((best, o) => (o[1] > best[1] ? o : best), [0, -1])[0];
  const x = score ?? mode;

  const res = useMemo(() => {
    const test = cmp === 'eq' ? (v) => v === x : cmp === 'ge' ? (v) => v >= x : (v) => v <= x;
    const CAPS = 50;                       // other-team histogram: 0..49 + "50+"
    const p = new Array(CAPS + 1).fill(0);
    const freq = new Map();
    const kept = [];
    let w = 0, sum = 0, win = 0, tie = 0;
    for (let i = 0; i < mine.length; i++) {
      if (!test(mine[i])) continue;
      const wi = s.w[i], o = theirs[i];
      p[Math.min(o, CAPS)] += wi;
      freq.set(o, (freq.get(o) || 0) + wi);
      kept.push([o, wi]);
      w += wi; sum += o * wi;
      if (mine[i] > o) win += wi; else if (mine[i] === o) tie += wi;
    }
    if (w === 0) return null;
    kept.sort((a, b) => a[0] - b[0]);
    let acc = 0, median = kept[kept.length - 1][0];
    for (const [o, wi] of kept) { acc += wi; if (acc >= w / 2) { median = o; break; } }
    const top = [...freq.entries()].sort((a, b) => b[1] - a[1]).slice(0, 3);
    return { w, mean: sum / w, median, win: win / w, tie: tie / w, top,
             h: { lo: -0.5, step: 1, nb: CAPS + 1, p: p.map(v => v / w), over_label: `${CAPS}+` } };
  }, [mine, theirs, s, cmp, x]);

  const sel = { background: 'rgba(11,17,38,0.8)', color: 'var(--text-white)', border: '1px solid var(--border-glass)', borderRadius: '5px', padding: '2px 4px', fontSize: '0.78rem' };
  return (
    <div style={CARD}>
      <div style={LBL}>Conditional score <span style={HINT}>— one team's score given the other's</span></div>
      <div style={{ display: 'flex', gap: '6px', alignItems: 'center', flexWrap: 'wrap', fontSize: '0.8rem', marginBottom: '8px' }}>
        <span>When</span>
        <select style={sel} value={side} onChange={(e) => { setSide(e.target.value); setScore(null); }}>
          <option value="away">{away}</option>
          <option value="home">{home}</option>
        </select>
        <span>scores</span>
        <select style={sel} value={cmp} onChange={(e) => setCmp(e.target.value)}>
          <option value="eq">exactly</option>
          <option value="ge">at least</option>
          <option value="le">at most</option>
        </select>
        <select style={sel} value={x} onChange={(e) => setScore(parseInt(e.target.value, 10))}>
          {options.map(([v, w]) => <option key={v} value={v}>{v} ({pct(w / s.wAll)})</option>)}
        </select>
      </div>
      {!res ? <div style={{ color: 'var(--accent-red)', fontSize: '0.8rem' }}>No sims match.</div> : (
        <>
          <div style={{ fontSize: '0.78rem', lineHeight: 1.6, marginBottom: '4px' }}>
            <div>
              {opp}'s most common score: <b style={{ color: 'var(--accent-gold)' }}>{res.top[0][0]}</b>
              <span style={{ color: 'var(--text-muted)' }}> ({pct(res.top[0][1] / res.w)})</span>
              {res.top.slice(1).map(([v, w]) => <span key={v} style={{ color: 'var(--text-muted)' }}> · {v} ({pct(w / res.w)})</span>)}
            </div>
            <div>
              median <b>{res.median}</b> · mean <b>{res.mean.toFixed(1)}</b> · {team} win <b>{pct(res.win, 0)}</b>
              {res.tie > 0.0005 && <> · tie <b>{pct(res.tie, 1)}</b></>}
            </div>
            <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>{pct(res.w / s.wAll)} of games</div>
          </div>
          <Bars h={res.h} accent="var(--accent-gold)" meanVal={res.mean} xlabel={`${opp} points`}
                tickValues={[7, 14, 21, 28, 35, 42]} />
        </>
      )}
    </div>
  );
}

export default function GameDistribution({ dist: distProp, onSelect }) {
  const dist = useMemo(() => normalizeDist(distProp), [distProp]);
  const svgRef = useRef(null);
  const [drag, setDrag] = useState(null);
  const [sel, setSel] = useState(null);
  // What the "Most common final scores" card is showing: a clicked bar
  // ({kind:'total'|'margin', bin}), the heatmap box ({kind:'box'}), or null = all games.
  const [focus, setFocus] = useState(null);

  // New game/result -> drop any selection that referred to the old one.
  useEffect(() => { setSel(null); setFocus(null); }, [distProp]);

  const grid = useMemo(() => {
    if (!dist?.joint) return null;
    const j = dist.joint;
    const g = Array.from({ length: j.m_nb }, () => new Array(j.t_nb).fill(0));
    let gmax = 0;
    for (const [mi, ti, p] of j.cells) { g[mi][ti] = p; if (p > gmax) gmax = p; }
    return { g, gmax, ...j };
  }, [dist]);

  const scores = useMemo(() => buildScores(dist), [dist]);

  const conditional = useMemo(() => {
    if (!sel || !grid) return null;
    const refT = dist.ref_total;
    const b = boxBounds(sel, grid);

    // Exact path: per-iteration arrays.
    if (dist.raw) {
      const { total, margin, weight, iteration } = dist.raw;
      const idx = [];
      let w = 0, sT = 0, sM = 0, wa = 0, ov = 0, wAll = 0;
      for (let i = 0; i < total.length; i++) {
        const wi = weight ? weight[i] : 1;
        wAll += wi;
        if (total[i] < b.tLo || total[i] >= b.tHi || margin[i] < b.mLo || margin[i] >= b.mHi) continue;
        idx.push(iteration[i]);
        w += wi; sT += total[i] * wi; sM += margin[i] * wi;
        if (margin[i] > 0) wa += wi;
        if (refT != null && total[i] > refT) ov += wi;
      }
      if (w === 0) return { empty: true, idx: [] };
      return { exact: true, idx, frac: w / wAll, condMeanTotal: sT / w, condMeanMargin: sM / w,
               condWinAway: wa / w, condOver: refT != null ? ov / w : null };
    }

    // Approx path: the joint grid only (binned-only payload). Bin-center
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

  // Most common final scores for the current focus.
  const breakdown = useMemo(() => {
    if (!scores) return null;
    let keep = () => true;
    if (focus?.kind === 'total') keep = (i) => binOf(dist.total, scores.total[i]) === focus.bin;
    else if (focus?.kind === 'margin') keep = (i) => binOf(dist.margin, scores.margin[i]) === focus.bin;
    else if (focus?.kind === 'box' && sel && grid) {
      const b = boxBounds(sel, grid);
      keep = (i) => scores.total[i] >= b.tLo && scores.total[i] < b.tHi && scores.margin[i] >= b.mLo && scores.margin[i] < b.mHi;
    }
    return topScores(scores, keep, 10);
  }, [scores, focus, sel, grid, dist]);

  if (!dist || !dist.total || !grid) {
    return <div style={{ color: 'var(--text-muted)', fontSize: '0.85rem', padding: '12px' }}>
      No outcome distribution in this result — re-run the engine to populate it.
    </div>;
  }

  const away = dist.away_team, home = dist.home_team;
  const clickBin = (kind) => (bin) =>
    setFocus(f => (f?.kind === kind && f.bin === bin ? null : { kind, bin }));

  // "GB by 7" / "ATL by 28+" / "Tie" for a margin bin (margin = away - home).
  const marginBinText = (bin) => {
    const h = dist.margin;
    const edge = (h.under_label && bin === 0) || (h.over_label && bin === h.nb - 1);
    if (h.step !== 1 && !edge) return `${away} − ${home} ${binLabel(h, bin, fmtSigned)}`;
    const v = edge ? (bin === 0 ? -MARGIN_CAP : MARGIN_CAP) : Math.round(h.lo + (bin + 0.5) * h.step);
    if (v === 0) return 'Tie';
    return `${v > 0 ? away : home} by ${Math.abs(v)}${edge ? '+' : ''}`;
  };

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
  // Heatmap axes tick off the JOINT geometry (coarser than the 1D bars), with
  // the same catch-all labels.
  const jT = { lo: grid.t_lo, step: grid.t_step, nb: grid.t_nb, under_label: dist.total.under_label, over_label: dist.total.over_label };
  const jM = { lo: grid.m_lo, step: grid.m_step, nb: grid.m_nb, under_label: dist.margin.under_label, over_label: dist.margin.over_label };
  const xTicks = histTicks(jT, [20, 35, 50, 65], (v) => `${Math.round(v)}`);
  const yTicks = histTicks(jM, [-14, -7, 0, 7, 14], fmtSigned);

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
    if (tB - tA < grid.t_step * 0.5 || mB2 - mA < grid.m_step * 0.5) {
      setSel(null);
      setFocus(f => (f?.kind === 'box' ? null : f));
      return;
    }
    setSel({ tLo: tA, tHi: tB, mLo: mA, mHi: mB2 });
    setFocus({ kind: 'box' });
  };
  const clearBox = () => { setSel(null); setFocus(f => (f?.kind === 'box' ? null : f)); };

  const tTop = grid.t_lo + grid.t_nb * grid.t_step, mTop = grid.m_lo + grid.m_nb * grid.m_step;
  const boxText = sel
    ? `total ${rangeLabel(sel.tLo, sel.tHi, grid.t_lo, tTop, grid.clamped, (v) => `${v}`)} · ${away} ${rangeLabel(sel.mLo, sel.mHi, grid.m_lo, mTop, grid.clamped, fmtSigned)}`
    : '';
  const focusTitle = !focus ? 'All games'
    : focus.kind === 'total' ? `Total ${binLabel(dist.total, focus.bin, (v) => `${Math.round(v)}`)}`
    : focus.kind === 'margin' ? marginBinText(focus.bin)
    : `Box: ${boxText}`;

  return (
    <div style={{ marginBottom: '24px' }}>
      <h3>Score Distribution <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontWeight: 400 }}>· {dist.n.toLocaleString()} sims</span></h3>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: '14px', marginTop: '10px' }}>
        <div style={CARD}>
          <div style={LBL}>Total Points {scores && <span style={HINT}>· click a bar</span>}</div>
          <Bars h={dist.total} accent="var(--accent-primary)" meanVal={dist.mean_total}
                refVal={dist.ref_total} refLabel="Vegas" xlabel="combined points"
                tickValues={[20, 35, 50, 65]}
                activeBin={focus?.kind === 'total' ? focus.bin : null}
                onBinClick={scores ? clickBin('total') : undefined} />
        </div>
        <div style={CARD}>
          <div style={LBL}>Score Differential <span style={{ textTransform: 'none', fontWeight: 400 }}>({away} − {home})</span> {scores && <span style={HINT}>· click a bar</span>}</div>
          <Bars h={dist.margin} accent="var(--accent-gold)" meanVal={dist.mean_margin_away}
                refVal={dist.ref_spread_home != null ? -dist.ref_spread_home : null} refLabel="Vegas line"
                xlabel={`◀ ${home} favored     ${away} favored ▶`} signed
                tickValues={[-14, -7, 0, 7, 14]}
                activeBin={focus?.kind === 'margin' ? focus.bin : null}
                onBinClick={scores ? clickBin('margin') : undefined} />
        </div>
      </div>

      <div style={{ ...CARD, marginTop: '14px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline' }}>
          <div style={LBL}>Joint Outcome — drag to select a range of games</div>
          {sel && <button onClick={clearBox} style={SMALL_BTN}>clear</button>}
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
            {xTicks.map(({ v, label }, k) => (
              <text key={k} x={xPix(v)} y={HH - 18} fontSize="9" fill="var(--text-muted)" {...edgeAnchor(k, xTicks.length)}>{label}</text>
            ))}
            <text x={mL + pw / 2} y={HH - 4} fontSize="9.5" fill="var(--text-muted)" textAnchor="middle">total points</text>
            {yTicks.map(({ v, label }, k) => (
              <text key={k} x={mL - 6} y={yPix(v) + 3} fontSize="9" fill="var(--text-muted)" textAnchor="end">{label}</text>
            ))}
            <text transform={`translate(11 ${mT + ph / 2}) rotate(-90)`} fontSize="9.5" fill="var(--text-muted)" textAnchor="middle">{away} − {home}</text>
          </svg>

          <div style={{ fontSize: '0.78rem', lineHeight: 1.5 }}>
            {!sel && <div style={{ color: 'var(--text-muted)' }}>Drag a box on the heatmap for the conditional outcome of that slice of games.</div>}
            {sel && conditional?.empty && <div style={{ color: 'var(--accent-red)' }}>No sims in that range.</div>}
            {sel && conditional && !conditional.empty && (
              <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                <div style={{ fontWeight: 700, color: 'var(--accent-gold)' }}>{pct(conditional.frac)} of games</div>
                <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>{boxText}</div>
                <hr style={{ border: 0, borderTop: '1px solid var(--border-glass)', margin: '4px 0' }} />
                <div>mean total <b>{conditional.condMeanTotal.toFixed(1)}</b></div>
                <div>mean margin <b>{fmtSigned(conditional.condMeanMargin)}</b> {conditional.condMeanMargin >= 0 ? away : home}</div>
                <div>{away} win <b>{pct(conditional.condWinAway, 0)}</b></div>
                {conditional.condOver != null && <div>over {dist.ref_total} <b>{pct(conditional.condOver, 0)}</b></div>}
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

      {scores ? (
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '14px', marginTop: '14px' }}>
          <div style={CARD}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', gap: '8px' }}>
              <div style={LBL}>Most common final scores</div>
              {focus && <button onClick={() => { if (focus.kind === 'box') setSel(null); setFocus(null); }} style={SMALL_BTN}>show all</button>}
            </div>
            <div style={{ fontSize: '0.8rem', marginBottom: '8px' }}>
              <b style={{ color: 'var(--accent-gold)' }}>{focusTitle}</b>
              <span style={{ color: 'var(--text-muted)' }}> · {pct(breakdown.wSel / scores.wAll)} of games</span>
              {!focus && <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>Click a bar above or drag a box on the heatmap to narrow this down.</div>}
            </div>
            <ScoreList rows={breakdown.rows} wSel={breakdown.wSel} wAll={scores.wAll} away={away} home={home} />
          </div>
          {/* keyed on the scores object so a new game/result resets the card's picks */}
          <ConditionalScore key={scores.away.length + away + home} s={scores} away={away} home={home} />
        </div>
      ) : (
        <div style={{ ...CARD, marginTop: '14px', color: 'var(--text-muted)', fontSize: '0.8rem' }}>
          Final-score breakdowns need per-sim data — re-run the engine for this game to enable them.
        </div>
      )}
    </div>
  );
}
