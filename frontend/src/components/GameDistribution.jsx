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
 *   - select Total / Differential bars (click, drag a range, Ctrl/Cmd-click
 *     to add/remove, Shift-click to extend) and/or drag a heatmap box. All
 *     active selections COMBINE into one pool of sims; each chart has its own
 *     reset, plus "reset all".
 *   - "Most common final scores" and "When <team> scores X" (the other team's
 *     score distribution) both read from that pool (no selection = all games).
 *   - every % also shows the raw sim count behind it ("81 games").
 *   - cross-filtering (2026-09-23): each chart redraws from the sims passing
 *     the OTHER charts' selections (never its own), with the unfiltered
 *     distribution as a faint outline; the heatmap dims to a backdrop and
 *     lights only the cells holding the bar-selected games.
 *   - pages refetch `dist` when a new sim run lands (App.jsx sim-status poll ->
 *     simVersion); a new dist resets every selection here, since box/bar
 *     picks and Showdown's iteration ids refer to the previous run.
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
 * Output: { away:Int[], home:Int[], total:Int[], margin:Int[], w:Number[], wAll, n } or
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
  return { away, home, total: raw.total, margin: raw.margin, w, wAll: wAll || 1, n };
}

/**
 * Raw sim count per histogram bin (for "N games" labels).
 * Inputs: values (Int[] per sim), h (histogram geometry).
 * Output: Int[h.nb].
 */
function binCounts(values, h) {
  const c = new Array(h.nb).fill(0);
  for (const v of values) c[binOf(h, v)] += 1;
  return c;
}

/**
 * Histogram of one per-sim field over the sims passing `keep`.
 * Inputs: s (buildScores output), field ('total' | 'margin'), h (histogram
 * geometry to bin into), keep (i -> bool).
 * Output: { p:Number[] (share of the POOL's weight), counts:Int[], mean, n }.
 */
function subsetHist(s, field, h, keep) {
  const vals = s[field];
  const w = new Array(h.nb).fill(0), counts = new Array(h.nb).fill(0);
  let wSum = 0, sum = 0, n = 0;
  for (let i = 0; i < vals.length; i++) {
    if (!keep(i)) continue;
    const b = binOf(h, vals[i]);
    w[b] += s.w[i]; counts[b] += 1; wSum += s.w[i]; sum += vals[i] * s.w[i]; n += 1;
  }
  return { p: w.map(x => (wSum ? x / wSum : 0)), counts, mean: wSum ? sum / wSum : null, n };
}

/**
 * Ranked final scores among the sims passing `keep`.
 * Inputs: s (buildScores output), keep (i -> bool), limit (int).
 * Output: { rows:[{a,h,w,n}] top `limit` by weight, wSel (weight kept), nSel (sims kept) }.
 */
function topScores(s, keep, limit = 10) {
  const byScore = new Map();
  let wSel = 0, nSel = 0;
  for (let i = 0; i < s.away.length; i++) {
    if (!keep(i)) continue;
    const k = s.away[i] * 1000 + s.home[i];
    const cur = byScore.get(k) || { w: 0, n: 0 };
    cur.w += s.w[i]; cur.n += 1;
    byScore.set(k, cur);
    wSel += s.w[i]; nSel += 1;
  }
  const rows = [...byScore.entries()]
    .map(([k, v]) => ({ a: Math.floor(k / 1000), h: k % 1000, w: v.w, n: v.n }))
    .sort((x, y) => y.w - x.w)
    .slice(0, limit);
  return { rows, wSel, nSel };
}

/** "81 games" / "1 game" */
const games = (n) => `${Math.round(n).toLocaleString()} game${Math.round(n) === 1 ? '' : 's'}`;

/**
 * Human label for a set of selected bins.
 * Inputs: h (histogram), bins (sorted Int[]), fmt (value formatter).
 * Output: "44–45" | "44–45 to 50–51" (contiguous run) | "44–45, 50–51"
 * (scattered, up to 4 listed) | "6 bars" (more scattered picks than that).
 */
function binsLabel(h, bins, fmt) {
  if (!bins.length) return '';
  const contiguous = bins.every((b, k) => k === 0 || b === bins[k - 1] + 1);
  if (bins.length === 1) return binLabel(h, bins[0], fmt);
  if (contiguous) return `${binLabel(h, bins[0], fmt)} to ${binLabel(h, bins[bins.length - 1], fmt)}`;
  if (bins.length <= 4) return bins.map(b => binLabel(h, b, fmt)).join(', ');
  return `${bins.length} bars`;
}

const ResetBtn = ({ onClick, label = 'reset' }) => (
  <button onClick={onClick} style={SMALL_BTN} title="Clear this selection">{label}</button>
);

/**
 * Histogram with optional multi-bin selection.
 * Inputs: h (histogram block), accent (CSS color), meanVal/refVal/refLabel
 * (reference lines), xlabel, signed (bool, +/- labels), tickValues,
 * counts (Int[] per bin, optional -> "N games" labels), selected (Int[]
 * sorted bin ids), onSelectBins (Int[] -> void; omit for a read-only chart),
 * ghost (Number[] per bin | null -- cross-filtering, 2026-09-23: the chart's
 * UNFILTERED distribution, drawn as a faint outline behind `h.p`, which is
 * then the distribution within the other charts' selections. Both are
 * shares of their own pool, drawn on one shared scale so shapes compare).
 * Selection gestures (2026-09-22):
 *   click            -> just that bar (click it again alone to clear)
 *   drag across bars -> that contiguous range
 *   Ctrl/Cmd-click   -> add/remove one bar (scattered picks)
 *   Shift-click      -> range from the last clicked bar
 */
function Bars({ h, accent, meanVal, refVal, refLabel, xlabel, signed, tickValues, counts, selected = [], onSelectBins, ghost = null }) {
  const W = 320, H = 150, padL = 4, padR = 4, padB = 26, padT = 8;
  const plotW = W - padL - padR, plotH = H - padB - padT;
  const [hover, setHover] = useState(null);
  const [drag, setDragState] = useState(null);     // {a, b} bin ids while dragging (drives the preview)
  // Mirror of `drag` read by the mouseup handler: a fast click can release
  // before React re-renders, so the commit must not depend on render state.
  const dragRef = useRef(null);
  const setDrag = (v) => { dragRef.current = v; setDragState(v); };
  const anchorRef = useRef(null);                  // last clicked bar, for Shift-click
  const maxP = Math.max(...h.p, ...(ghost || []), 1e-9);
  const bw = plotW / h.nb;
  const span = h.step * h.nb;
  const xOf = (v) => padL + ((v - h.lo) / span) * plotW;
  const fmt = (v) => (signed && v > 0 ? `+${Math.round(v)}` : `${Math.round(v)}`);
  const fmtRef = (v) => (signed && v > 0 ? `+${v}` : `${v}`);
  const ticks = histTicks(h, tickValues || [], fmt);
  const range = (a, b) => Array.from({ length: Math.abs(b - a) + 1 }, (_, k) => Math.min(a, b) + k);

  // While dragging, preview the range; otherwise show the committed selection.
  const active = new Set(drag ? range(drag.a, drag.b) : selected);
  const hasSel = active.size > 0;

  // Finish a drag wherever the mouse is released (even outside the chart).
  // Registered once at mount; commitRef always points at this render's commit.
  const commitRef = useRef(null);
  commitRef.current = commitDrag;
  useEffect(() => {
    const up = () => commitRef.current?.();
    window.addEventListener('mouseup', up);
    return () => window.removeEventListener('mouseup', up);
  }, []);

  function commitDrag() {
    const cur = dragRef.current;
    if (!cur) return;
    const bins = range(cur.a, cur.b);
    setDrag(null);
    // A plain click on the only selected bar toggles it off.
    if (bins.length === 1 && selected.length === 1 && selected[0] === bins[0]) onSelectBins([]);
    else onSelectBins(bins);
  }

  const onDown = (i, e) => {
    if (!onSelectBins) return;
    e.preventDefault();
    if (e.ctrlKey || e.metaKey) {
      const s = new Set(selected);
      s.has(i) ? s.delete(i) : s.add(i);
      onSelectBins([...s].sort((a, b) => a - b));
      anchorRef.current = i;
    } else if (e.shiftKey && anchorRef.current != null) {
      onSelectBins(range(anchorRef.current, i));
    } else {
      anchorRef.current = i;
      setDrag({ a: i, b: i });
    }
  };

  // Readout: hovered bar, else the selection's total, else mean/ref.
  const selP = selected.reduce((s, b) => s + h.p[b], 0);
  const selN = counts ? selected.reduce((s, b) => s + counts[b], 0) : null;

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} width="100%" style={{ display: 'block', userSelect: 'none' }}
           onMouseLeave={() => setHover(null)}>
        {ghost && ghost.map((p, i) => {
          const gh = (p / maxP) * plotH;
          return gh > 0 && (
            <rect key={`g${i}`} x={padL + i * bw + 0.5} y={padT + plotH - gh} width={Math.max(bw - 1, 1)} height={gh}
                  fill="none" stroke="var(--text-muted)" strokeWidth="0.6" opacity="0.55" pointerEvents="none" />
          );
        })}
        {h.p.map((p, i) => {
          const bh = (p / maxP) * plotH;
          const opacity = hasSel ? (active.has(i) ? 1 : hover === i ? 0.6 : 0.25) : (hover === i ? 1 : 0.62);
          return (
            <g key={i}
               onMouseEnter={() => { setHover(i); if (dragRef.current) setDrag({ ...dragRef.current, b: i }); }}
               onMouseDown={(e) => onDown(i, e)}
               style={{ cursor: onSelectBins ? 'pointer' : 'default' }}>
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
        {hover != null
          ? <span>{binLabel(h, hover, fmt)}: <b style={{ color: accent }}>{pct(h.p[hover])}</b>{counts && <> ({games(counts[hover])})</>}{ghost && <> · all games {pct(ghost[hover])}</>}</span>
          : selected.length > 0
            ? <span>selected {binsLabel(h, selected, fmt)}: <b style={{ color: accent }}>{pct(selP)}</b>{selN != null && <> ({games(selN)})</>}</span>
            : <span>mean <b style={{ color: accent }}>{meanVal?.toFixed?.(1)}</b>{refVal != null && <> · <span style={{ borderBottom: '1px dashed var(--text-muted)' }}>{refLabel} {fmtRef(refVal)}</span></>}</span>}
      </div>
    </div>
  );
}

/**
 * Gold "showing games in: ..." line above a cross-filtered chart.
 * Inputs: cross (subsetHist output | null -> renders nothing), text (array of
 * the OTHER charts' selection descriptions, nulls ignored).
 */
function CrossNote({ cross, text }) {
  if (!cross) return null;
  return (
    <div style={{ fontSize: '0.7rem', color: 'var(--accent-gold)', margin: '-2px 0 4px' }}>
      showing games in: {text.filter(Boolean).join(' · ')} ({games(cross.n)}) <span style={{ color: 'var(--text-muted)' }}>· outline = all games</span>
    </div>
  );
}

/**
 * Ranked list of final scores with inline bars.
 * Inputs: rows ([{a,h,w,n}] from topScores), wSel (weight of the slice),
 * wAll (weight of all sims), away/home (team abbrs).
 */
function ScoreList({ rows, wSel, wAll, away, home }) {
  if (!rows.length) return <div style={{ color: 'var(--accent-red)', fontSize: '0.8rem' }}>No sims in this slice.</div>;
  const top = rows[0].w;
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '3px' }}>
      {rows.map(({ a, h, w, n }, k) => (
        <div key={`${a}-${h}`} style={{ display: 'grid', gridTemplateColumns: '14px minmax(92px, auto) 1fr 44px 58px', gap: '8px', alignItems: 'center', fontSize: '0.78rem' }}>
          <span style={{ color: 'var(--text-muted)', fontSize: '0.68rem' }}>{k + 1}</span>
          <span style={{ whiteSpace: 'nowrap' }}>
            <span style={{ fontWeight: a > h ? 700 : 400 }}>{away} {a}</span>
            <span style={{ color: 'var(--text-muted)' }}> – </span>
            <span style={{ fontWeight: h > a ? 700 : 400 }}>{home} {h}</span>
          </span>
          <span style={{ height: '8px', borderRadius: '3px', background: 'var(--accent-primary)', opacity: 0.7, width: `${(w / top) * 100}%` }} />
          <span style={{ textAlign: 'right', fontVariantNumeric: 'tabular-nums' }} title={`${pct(w / wAll, 2)} of all games`}>{pct(w / wSel)}</span>
          <span style={{ textAlign: 'right', color: 'var(--text-muted)', fontSize: '0.7rem', fontVariantNumeric: 'tabular-nums' }}>{games(n)}</span>
        </div>
      ))}
    </div>
  );
}

/**
 * "When <team> scores X" card: the other team's score distribution given a
 * condition on one team's score, WITHIN the games the other charts'
 * selections leave in play (keep).
 * Inputs: s (buildScores output), away/home (team abbrs), keep (i -> bool,
 * the combined selection filter), scopeText (string describing it, '' = all
 * games), resetKey (changes whenever the page-level "reset all" fires).
 * State: side ('away'|'home'), cmp ('eq'|'ge'|'le'), score (int|null -> the
 * conditioned team's most common score in the pool).
 */
function ConditionalScore({ s, away, home, keep, scopeText, resetKey }) {
  const [side, setSide] = useState('home');
  const [cmp, setCmp] = useState('eq');
  const [score, setScore] = useState(null);
  const reset = () => { setSide('home'); setCmp('eq'); setScore(null); };
  useEffect(reset, [resetKey]);
  const dirty = side !== 'home' || cmp !== 'eq' || score != null;

  const mine = side === 'away' ? s.away : s.home;
  const theirs = side === 'away' ? s.home : s.away;
  const team = side === 'away' ? away : home, opp = side === 'away' ? home : away;

  // This team's own score frequencies within the pool -> dropdown options + default (mode).
  const { options, poolN } = useMemo(() => {
    const f = new Map();
    let n = 0;
    for (let i = 0; i < mine.length; i++) {
      if (!keep(i)) continue;
      const cur = f.get(mine[i]) || { w: 0, n: 0 };
      cur.w += s.w[i]; cur.n += 1; f.set(mine[i], cur); n += 1;
    }
    return { options: [...f.entries()].sort((x, y) => x[0] - y[0]), poolN: n };
  }, [mine, s, keep]);
  const mode = options.reduce((best, o) => (o[1].w > best[1].w ? o : best), [0, { w: -1 }])[0];
  const x = score != null && options.some(o => o[0] === score) ? score : mode;

  const res = useMemo(() => {
    const test = cmp === 'eq' ? (v) => v === x : cmp === 'ge' ? (v) => v >= x : (v) => v <= x;
    const CAPS = 50;                       // other-team histogram: 0..49 + "50+"
    const p = new Array(CAPS + 1).fill(0), counts = new Array(CAPS + 1).fill(0);
    const freq = new Map();
    const kept = [];
    let w = 0, n = 0, sum = 0, win = 0, tie = 0;
    for (let i = 0; i < mine.length; i++) {
      if (!keep(i) || !test(mine[i])) continue;
      const wi = s.w[i], o = theirs[i];
      p[Math.min(o, CAPS)] += wi; counts[Math.min(o, CAPS)] += 1;
      const cur = freq.get(o) || { w: 0, n: 0 };
      cur.w += wi; cur.n += 1; freq.set(o, cur);
      kept.push([o, wi]);
      w += wi; n += 1; sum += o * wi;
      if (mine[i] > o) win += wi; else if (mine[i] === o) tie += wi;
    }
    if (w === 0) return null;
    kept.sort((a, b) => a[0] - b[0]);
    let acc = 0, median = kept[kept.length - 1][0];
    for (const [o, wi] of kept) { acc += wi; if (acc >= w / 2) { median = o; break; } }
    const top = [...freq.entries()].sort((a, b) => b[1].w - a[1].w).slice(0, 3);
    return { w, n, mean: sum / w, median, win: win / w, tie: tie / w, top, counts,
             h: { lo: -0.5, step: 1, nb: CAPS + 1, p: p.map(v => v / w), over_label: `${CAPS}+` } };
  }, [mine, theirs, s, keep, cmp, x]);

  const sel = { background: 'rgba(11,17,38,0.8)', color: 'var(--text-white)', border: '1px solid var(--border-glass)', borderRadius: '5px', padding: '2px 4px', fontSize: '0.78rem' };
  return (
    <div style={CARD}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', gap: '8px' }}>
        <div style={LBL}>Conditional score <span style={HINT}>— one team's score given the other's</span></div>
        {dirty && <ResetBtn onClick={reset} />}
      </div>
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
          {options.map(([v, o]) => <option key={v} value={v}>{v} ({pct(o.w / s.wAll)} · {games(o.n)})</option>)}
        </select>
      </div>
      {scopeText && (
        <div style={{ fontSize: '0.7rem', color: 'var(--accent-gold)', marginBottom: '6px' }}>
          within your selection: {scopeText} ({games(poolN)})
        </div>
      )}
      {!res ? <div style={{ color: 'var(--accent-red)', fontSize: '0.8rem' }}>No sims match.</div> : (
        <>
          <div style={{ fontSize: '0.78rem', lineHeight: 1.6, marginBottom: '4px' }}>
            <div>
              {opp}'s most common score: <b style={{ color: 'var(--accent-gold)' }}>{res.top[0][0]}</b>
              <span style={{ color: 'var(--text-muted)' }}> ({pct(res.top[0][1].w / res.w)} · {games(res.top[0][1].n)})</span>
              {res.top.slice(1).map(([v, o]) => <span key={v} style={{ color: 'var(--text-muted)' }}> · {v} ({pct(o.w / res.w)} · {o.n})</span>)}
            </div>
            <div>
              median <b>{res.median}</b> · mean <b>{res.mean.toFixed(1)}</b> · {team} win <b>{pct(res.win, 0)}</b>
              {res.tie > 0.0005 && <> · tie <b>{pct(res.tie, 1)}</b></>}
            </div>
            <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>{pct(res.w / s.wAll)} of all games ({games(res.n)})</div>
          </div>
          <Bars h={res.h} accent="var(--accent-gold)" meanVal={res.mean} xlabel={`${opp} points`}
                tickValues={[7, 14, 21, 28, 35, 42]} counts={res.counts} />
        </>
      )}
    </div>
  );
}

export default function GameDistribution({ dist: distProp, onSelect }) {
  const dist = useMemo(() => normalizeDist(distProp), [distProp]);
  const svgRef = useRef(null);
  const [drag, setDrag] = useState(null);
  const [sel, setSel] = useState(null);          // heatmap box
  // Bar selections (sorted bin ids). All active selections -- total bars,
  // differential bars, heatmap box -- COMBINE (a sim must pass every one) to
  // form the pool the score cards below read from.
  const [totalBins, setTotalBins] = useState([]);
  const [marginBins, setMarginBins] = useState([]);
  const [resetKey, setResetKey] = useState(0);   // bumped by "reset all" -> ConditionalScore resets too

  const resetAll = () => { setSel(null); setTotalBins([]); setMarginBins([]); setResetKey(k => k + 1); };
  // New game/result -> drop any selection that referred to the old one.
  useEffect(resetAll, [distProp]); // eslint-disable-line react-hooks/exhaustive-deps

  const grid = useMemo(() => {
    if (!dist?.joint) return null;
    const j = dist.joint;
    const g = Array.from({ length: j.m_nb }, () => new Array(j.t_nb).fill(0));
    let gmax = 0;
    for (const [mi, ti, p] of j.cells) { g[mi][ti] = p; if (p > gmax) gmax = p; }
    return { g, gmax, ...j };
  }, [dist]);

  const scores = useMemo(() => buildScores(dist), [dist]);
  const totalCounts = useMemo(() => (scores ? binCounts(scores.total, dist.total) : null), [scores, dist]);
  const marginCounts = useMemo(() => (scores ? binCounts(scores.margin, dist.margin) : null), [scores, dist]);

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

  // Selection filters over sims. `keep` = total bars AND differential bars AND
  // box (the pool the score cards read). Cross-filtering (2026-09-23) draws
  // each chart from the sims passing the OTHER charts' selections -- never its
  // own, or selecting a bar would just zero out every other bar on that chart.
  // A for* entry is null when that chart has no other selection to follow.
  const filters = useMemo(() => {
    if (!scores) return null;
    const tSet = totalBins.length ? new Set(totalBins) : null;
    const mSet = marginBins.length ? new Set(marginBins) : null;
    const b = sel && grid ? boxBounds(sel, grid) : null;
    const inT = (i) => !tSet || tSet.has(binOf(dist.total, scores.total[i]));
    const inM = (i) => !mSet || mSet.has(binOf(dist.margin, scores.margin[i]));
    const inB = (i) => !b || (scores.total[i] >= b.tLo && scores.total[i] < b.tHi && scores.margin[i] >= b.mLo && scores.margin[i] < b.mHi);
    return {
      keep: (i) => inT(i) && inM(i) && inB(i),
      forTotal: !mSet && !b ? null : (i) => inM(i) && inB(i),
      forMargin: !tSet && !b ? null : (i) => inT(i) && inB(i),
      forJoint: !tSet && !mSet ? null : (i) => inT(i) && inM(i),
    };
  }, [scores, totalBins, marginBins, sel, grid, dist]);
  const keep = filters ? filters.keep : () => true;

  // Cross-filtered histograms ({p, counts, mean, n} over that chart's pool)
  // and heatmap grid, or null = draw that chart unfiltered.
  const crossTotal = useMemo(() => (filters?.forTotal ? subsetHist(scores, 'total', dist.total, filters.forTotal) : null), [filters, scores, dist]);
  const crossMargin = useMemo(() => (filters?.forMargin ? subsetHist(scores, 'margin', dist.margin, filters.forMargin) : null), [filters, scores, dist]);
  const crossJoint = useMemo(() => {
    if (!filters?.forJoint || !grid) return null;
    const JT = { lo: grid.t_lo, step: grid.t_step, nb: grid.t_nb }, JM = { lo: grid.m_lo, step: grid.m_step, nb: grid.m_nb };
    const g = Array.from({ length: grid.m_nb }, () => new Array(grid.t_nb).fill(0));
    let gmax = 0;
    for (let i = 0; i < scores.total.length; i++) {
      if (!filters.forJoint(i)) continue;
      const mi = binOf(JM, scores.margin[i]), ti = binOf(JT, scores.total[i]);
      g[mi][ti] += scores.w[i];
      if (g[mi][ti] > gmax) gmax = g[mi][ti];
    }
    return { g, gmax };
  }, [filters, scores, grid]);

  const breakdown = useMemo(() => (scores ? topScores(scores, keep, 10) : null), [scores, keep]);

  if (!dist || !dist.total || !grid) {
    return <div style={{ color: 'var(--text-muted)', fontSize: '0.85rem', padding: '12px' }}>
      No outcome distribution in this result — re-run the engine to populate it.
    </div>;
  }

  const away = dist.away_team, home = dist.home_team;

  // "GB by 7" / "ATL by 28+" / "Tie" for one margin bin (margin = away - home).
  const marginBinText = (bin) => {
    const h = dist.margin;
    const edge = (h.under_label && bin === 0) || (h.over_label && bin === h.nb - 1);
    if (h.step !== 1 && !edge) return `${away} − ${home} ${binLabel(h, bin, fmtSigned)}`;
    const v = edge ? (bin === 0 ? -MARGIN_CAP : MARGIN_CAP) : Math.round(h.lo + (bin + 0.5) * h.step);
    if (v === 0) return 'Tie';
    return `${v > 0 ? away : home} by ${Math.abs(v)}${edge ? '+' : ''}`;
  };
  const marginSelText = (bins) => {
    if (bins.length === 1) return marginBinText(bins[0]);
    const contiguous = bins.every((b, k) => k === 0 || b === bins[k - 1] + 1);
    return contiguous
      ? `${away} − ${home} ${binLabel(dist.margin, bins[0], fmtSigned)} to ${binLabel(dist.margin, bins[bins.length - 1], fmtSigned)}`
      : bins.length <= 4 ? bins.map(marginBinText).join(', ') : `${bins.length} differential bars`;
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
    if (tB - tA < grid.t_step * 0.5 || mB2 - mA < grid.m_step * 0.5) { setSel(null); return; }
    setSel({ tLo: tA, tHi: tB, mLo: mA, mHi: mB2 });
  };

  const tTop = grid.t_lo + grid.t_nb * grid.t_step, mTop = grid.m_lo + grid.m_nb * grid.m_step;
  const boxText = sel
    ? `total ${rangeLabel(sel.tLo, sel.tHi, grid.t_lo, tTop, grid.clamped, (v) => `${v}`)} · ${away} ${rangeLabel(sel.mLo, sel.mHi, grid.m_lo, mTop, grid.clamped, fmtSigned)}`
    : '';
  // Plain-English description of every active selection, combined.
  const totalSelText0 = totalBins.length ? `total ${binsLabel(dist.total, totalBins, (v) => `${Math.round(v)}`)}` : null;
  const marginSelText0 = marginBins.length ? marginSelText(marginBins) : null;
  const scopeParts = [
    totalSelText0,
    marginSelText0,
    sel ? `box: ${boxText}` : null,
  ].filter(Boolean);
  const scopeText = scopeParts.join(' · ');
  const anySel = scopeParts.length > 0;

  return (
    <div style={{ marginBottom: '24px' }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', gap: '10px' }}>
        <h3>Score Distribution <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontWeight: 400 }}>· {dist.n.toLocaleString()} sims</span></h3>
        {anySel && <ResetBtn onClick={resetAll} label="reset all" />}
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: '14px', marginTop: '10px' }}>
        <div style={CARD}>
          <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', gap: '8px' }}>
            <div style={LBL}>Total Points {scores && <span style={HINT}>· click, drag, or Ctrl-click bars</span>}</div>
            {totalBins.length > 0 && <ResetBtn onClick={() => setTotalBins([])} />}
          </div>
          <CrossNote cross={crossTotal} text={[marginSelText0, sel ? `box: ${boxText}` : null]} />
          <Bars h={crossTotal ? { ...dist.total, p: crossTotal.p } : dist.total} ghost={crossTotal ? dist.total.p : null}
                accent="var(--accent-primary)" meanVal={crossTotal ? crossTotal.mean : dist.mean_total}
                refVal={dist.ref_total} refLabel="Vegas" xlabel="combined points"
                tickValues={[20, 35, 50, 65]} counts={crossTotal ? crossTotal.counts : totalCounts}
                selected={totalBins} onSelectBins={scores ? setTotalBins : undefined} />
        </div>
        <div style={CARD}>
          <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', gap: '8px' }}>
            <div style={LBL}>Score Differential <span style={{ textTransform: 'none', fontWeight: 400 }}>({away} − {home})</span></div>
            {marginBins.length > 0 && <ResetBtn onClick={() => setMarginBins([])} />}
          </div>
          <CrossNote cross={crossMargin} text={[totalSelText0, sel ? `box: ${boxText}` : null]} />
          <Bars h={crossMargin ? { ...dist.margin, p: crossMargin.p } : dist.margin} ghost={crossMargin ? dist.margin.p : null}
                accent="var(--accent-gold)" meanVal={crossMargin ? crossMargin.mean : dist.mean_margin_away}
                refVal={dist.ref_spread_home != null ? -dist.ref_spread_home : null} refLabel="Vegas line"
                xlabel={`◀ ${home} favored     ${away} favored ▶`} signed
                tickValues={[-14, -7, 0, 7, 14]} counts={crossMargin ? crossMargin.counts : marginCounts}
                selected={marginBins} onSelectBins={scores ? setMarginBins : undefined} />
        </div>
      </div>

      <div style={{ ...CARD, marginTop: '14px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline' }}>
          <div style={LBL}>Joint Outcome — drag to select a range of games</div>
          {sel && <ResetBtn onClick={() => setSel(null)} />}
        </div>
        {crossJoint && (
          <div style={{ fontSize: '0.7rem', color: 'var(--accent-gold)', marginBottom: '4px' }}>
            highlighting games in: {[totalSelText0, marginSelText0].filter(Boolean).join(' · ')}
          </div>
        )}

        <div style={{ display: 'grid', gridTemplateColumns: 'minmax(0,1fr) 188px', gap: '14px', alignItems: 'center' }}>
          <svg ref={svgRef} viewBox={`0 0 ${HW} ${HH}`} width="100%"
               style={{ display: 'block', cursor: 'crosshair', userSelect: 'none' }}
               onMouseDown={onDown} onMouseMove={onMove} onMouseUp={onUp} onMouseLeave={onUp}>
            {/* Cross-filtered: the full grid fades to a backdrop and the cells
                holding games from the bar selections light up on their own scale. */}
            {grid.g.map((rowArr, mi) => rowArr.map((p, ti) => p > 0 && (
              <rect key={`${mi}-${ti}`} x={mL + ti * cellW} y={mT + (grid.m_nb - 1 - mi) * cellH}
                    width={cellW + 0.5} height={cellH + 0.5}
                    fill="var(--accent-primary)" opacity={crossJoint ? 0.06 : 0.1 + 0.9 * Math.sqrt(p / grid.gmax)} />
            )))}
            {crossJoint && crossJoint.g.map((rowArr, mi) => rowArr.map((p, ti) => p > 0 && (
              <rect key={`x${mi}-${ti}`} x={mL + ti * cellW} y={mT + (grid.m_nb - 1 - mi) * cellH}
                    width={cellW + 0.5} height={cellH + 0.5}
                    fill="var(--accent-primary)" opacity={0.15 + 0.85 * Math.sqrt(p / crossJoint.gmax)} />
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
                <div style={{ fontWeight: 700, color: 'var(--accent-gold)' }}>
                  {pct(conditional.frac)} of games{conditional.exact && <span style={{ fontWeight: 400, color: 'var(--text-muted)' }}> ({games(conditional.idx.length)})</span>}
                </div>
                <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>{boxText}</div>
                <hr style={{ border: 0, borderTop: '1px solid var(--border-glass)', margin: '4px 0' }} />
                <div>mean total <b>{conditional.condMeanTotal.toFixed(1)}</b></div>
                <div>mean margin <b>{fmtSigned(conditional.condMeanMargin)}</b> {conditional.condMeanMargin >= 0 ? away : home}</div>
                <div>{away} win <b>{pct(conditional.condWinAway, 0)}</b></div>
                {conditional.condOver != null && <div>over {dist.ref_total} <b>{pct(conditional.condOver, 0)}</b></div>}
                <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', marginTop: '3px' }}>
                  {conditional.exact
                    ? (onSelect ? 'these sims → lineup re-scoring' : '')
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
              {anySel && <ResetBtn onClick={resetAll} label="show all" />}
            </div>
            <div style={{ fontSize: '0.8rem', marginBottom: '8px' }}>
              <b style={{ color: 'var(--accent-gold)' }}>{anySel ? scopeText : 'All games'}</b>
              <span style={{ color: 'var(--text-muted)' }}> · {pct(breakdown.wSel / scores.wAll)} of games ({games(breakdown.nSel)})</span>
              {!anySel && <div style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}>Select bars above or drag a box on the heatmap to narrow this down — selections combine.</div>}
            </div>
            <ScoreList rows={breakdown.rows} wSel={breakdown.wSel} wAll={scores.wAll} away={away} home={home} />
          </div>
          {/* keyed on the matchup so a different game resets the card's picks */}
          <ConditionalScore key={scores.n + away + home} s={scores} away={away} home={home}
                            keep={keep} scopeText={anySel ? scopeText : ''} resetKey={resetKey} />
        </div>
      ) : (
        <div style={{ ...CARD, marginTop: '14px', color: 'var(--text-muted)', fontSize: '0.8rem' }}>
          Final-score breakdowns need per-sim data — re-run the engine for this game to enable them.
        </div>
      )}
    </div>
  );
}
