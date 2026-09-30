import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';

/**
 * Game Lines evaluation (2026-09-25) -- the sim's spreads / totals / win
 * probabilities graded against Vegas (opening AND closing line) and against
 * what actually happened. Rendered by pages/EvaluationPage.jsx (Evaluation > Game Lines).
 *
 * Inputs (props):
 *   selectedWeek -- the page's week (default filter for the per-game table)
 * Data: GET /api/eval/game_lines (src/evaluation/game_line_eval.py) --
 *   { games: [...per-game rows], summary: {...season}, running: [...per week], weeks }
 *
 * Sections, top to bottom:
 *   1. Scoreboard      -- sim vs. Vegas accuracy, ATS / O-U / moneyline records, CLV
 *   2. Running trend   -- cumulative units (spread/total/ML) + (sim MAE - Vegas MAE) by week
 *   3. Calibration     -- pick-probability reliability + PIT histograms
 *   4. Agreement grid  -- agree / disagree / strong-disagree with Vegas, per market
 *   5. Where we win/lose -- W/L/units sliced by fav/dog, home/away, roof, week, ...
 *   6. Per-game table  -- sim vs. Vegas vs. actual; spread, total and moneyline
 *                         picks with agreement tags, results, CLV
 * "Line" toggle (Open | Close) drives every pick/record/calibration view;
 * Open is the default since jumping soft openers is the actual strategy.
 *
 * Charts are hand-rolled inline SVG (the DFS site has no chart library).
 * Palette validated for the dark surface (dataviz validate_palette.js):
 *   spread / sim = #0fa3b1, total = #c97d00, Vegas benchmark = --text-muted gray.
 */

const C_SPREAD = '#0fa3b1';
const C_TOTAL = '#c97d00';
const C_ML = '#9b6ad6';   // validated with the two above on the dark surface (2026-09-25)
const C_VEGAS = 'var(--text-muted)';
const GRID = 'rgba(255,255,255,0.08)';

const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const btn = (active) => ({
  background: active ? 'rgba(15,163,177,0.18)' : 'rgba(0,0,0,0.25)',
  border: `1px solid ${active ? C_SPREAD : 'rgba(255,255,255,0.14)'}`,
  borderRadius: '6px', color: 'var(--text-white)', padding: '4px 10px', fontSize: '0.8rem', cursor: 'pointer',
});
const th = { padding: '6px 5px', textAlign: 'left', whiteSpace: 'nowrap' };
const td = { padding: '5px 5px', whiteSpace: 'nowrap' };

const f1 = (v) => (v == null ? '—' : Number(v).toFixed(1));
const pct = (v) => (v == null ? '—' : `${(v * 100).toFixed(1)}%`);
const signed = (v, d = 1) => (v == null ? '—' : `${v > 0 ? '+' : ''}${Number(v).toFixed(d)}`);
const unitsColor = (v) => (v == null || v === 0 ? 'var(--text-muted)' : v > 0 ? 'var(--accent-green)' : 'var(--accent-red)');
const resColor = { W: 'var(--accent-green)', L: 'var(--accent-red)', P: 'var(--text-muted)' };

/** Home-perspective line (home favored if > 0) -> "GB -4.5" / "PK". */
function spreadText(home, away, homeLine) {
  if (homeLine == null) return '—';
  const r = Math.round(homeLine * 2) / 2;          // display to the half point
  if (r === 0) return 'PK';
  return r > 0 ? `${home} -${r}` : `${away} -${Math.abs(r)}`;
}
function recText(r) { return r ? `${r.w}-${r.l}${r.p ? `-${r.p}` : ''}` : '—'; }
/** Actual result in the same notation as the Vegas column: winner gets the
 *  minus ("BUF -10" = BUF won by 10), so it lines up against "BUF -3". */
function resultText(home, away, margin) {
  if (margin == null) return '—';
  if (margin === 0) return 'TIE';
  return margin > 0 ? `${home} -${margin}` : `${away} -${Math.abs(margin)}`;
}
const oddsText = (o) => (o == null ? '—' : o > 0 ? `+${Math.round(o)}` : `${Math.round(o)}`);

// Agreement tiers: how far our line is from Vegas's (see game_line_eval.py).
const TIER_LABEL = { agree: 'agree', disagree: 'disagree', strong: 'strong' };
const TIER_TITLE = {
  agree: 'Agree — our line is within 1 pt of Vegas (moneyline: within 3 win-prob pts)',
  disagree: 'Disagree — 1 to 2.5 pts off Vegas (moneyline: 3-7 win-prob pts)',
  strong: 'Strong disagreement — more than 2.5 pts off Vegas (moneyline: more than 7 win-prob pts)',
};
/** Small ordinal badge: text tokens only (tiers aren't good/bad, just how far apart). */
function TierTag({ t }) {
  if (!t) return null;
  const strong = t === 'strong';
  return (
    <span title={TIER_TITLE[t]} style={{
      marginLeft: 6, padding: '0 5px', borderRadius: '4px', fontSize: '0.64rem',
      border: `1px solid ${strong ? 'var(--text-main)' : 'rgba(255,255,255,0.14)'}`,
      color: strong ? 'var(--text-white)' : t === 'disagree' ? 'var(--text-main)' : 'var(--text-muted)',
      fontWeight: strong ? 700 : 400,
    }}>{TIER_LABEL[t]}</span>
  );
}

// ── Tiny SVG line chart (weeks on x) ────────────────────────────────────────
/**
 * Inputs: data (array of {week, ...}), series ([{key, label, color, dash}]),
 * yLabel (string), zeroLine (bool). Output: responsive SVG line chart with a
 * per-point hover tooltip (native <title>) and direct end labels.
 */
function LineChart({ data, series, yLabel, zeroLine = true, height = 190 }) {
  const W = 520, H = height, L = 44, R = 92, T = 12, B = 28;
  const vals = data.flatMap(d => series.map(s => d[s.key])).filter(v => v != null);
  if (!data.length || !vals.length) return <Empty text="Needs at least one graded week." />;
  // "Nice" ticks: step = 1/2/5 x 10^k so gridlines land on round numbers,
  // and the domain snaps outward to whole steps (always including 0).
  const rawLo = Math.min(0, ...vals), rawHi = Math.max(0, ...vals);
  const span = Math.max(rawHi - rawLo, 1);
  const mag = 10 ** Math.floor(Math.log10(span / 4));
  const step = [1, 2, 5, 10].map(m => m * mag).find(s => span / s <= 5);
  const lo = Math.floor(rawLo / step) * step, hi = Math.ceil(rawHi / step) * step;
  const ticks = [];
  for (let t = lo; t <= hi + step / 2; t += step) ticks.push(Math.round(t * 100) / 100);
  const weeks = data.map(d => d.week);
  const x = (w) => weeks.length === 1 ? L + (W - L - R) / 2 : L + ((w - weeks[0]) / (weeks[weeks.length - 1] - weeks[0])) * (W - L - R);
  const y = (v) => T + (1 - (v - lo) / (hi - lo)) * (H - T - B);
  // End labels: sort by y and push apart so overlapping series (e.g. open and
  // close units identical so far) stay readable instead of printing on top.
  const endLabels = series.map(s => {
    const pts = data.filter(d => d[s.key] != null);
    return pts.length ? { s, ly: y(pts[pts.length - 1][s.key]), lx: x(pts[pts.length - 1].week) } : null;
  }).filter(Boolean).sort((a, b) => a.ly - b.ly);
  for (let i = 1; i < endLabels.length; i++) {
    if (endLabels[i].ly - endLabels[i - 1].ly < 11) endLabels[i].ly = endLabels[i - 1].ly + 11;
  }
  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: '100%', height: 'auto' }} role="img" aria-label={yLabel}>
      {ticks.map((t, i) => (
        <g key={i}>
          <line x1={L} x2={W - R} y1={y(t)} y2={y(t)} stroke={GRID} />
          <text x={L - 6} y={y(t) + 3} textAnchor="end" fontSize="10" fill="var(--text-muted)">{t}</text>
        </g>
      ))}
      {zeroLine && lo < 0 && hi > 0 && <line x1={L} x2={W - R} y1={y(0)} y2={y(0)} stroke="var(--text-muted)" strokeWidth="1" />}
      {weeks.map(w => <text key={w} x={x(w)} y={H - 10} textAnchor="middle" fontSize="10" fill="var(--text-muted)">W{w}</text>)}
      {series.map(s => {
        const pts = data.filter(d => d[s.key] != null);
        if (!pts.length) return null;
        return (
          <g key={s.key}>
            <polyline fill="none" stroke={s.color} strokeWidth="2" strokeDasharray={s.dash ? '5 4' : undefined}
              points={pts.map(d => `${x(d.week)},${y(d[s.key])}`).join(' ')} />
            {pts.map(d => (
              <circle key={d.week} cx={x(d.week)} cy={y(d[s.key])} r="4" fill={s.color} stroke="var(--bg-deep)" strokeWidth="2">
                <title>{`${s.label} — thru W${d.week}: ${signed(d[s.key], 2)}`}</title>
              </circle>
            ))}
          </g>
        );
      })}
      {endLabels.map(({ s, lx, ly }) => (
        <text key={`lbl${s.key}`} x={lx + 8} y={ly + 3} fontSize="10" fill="var(--text-main)">{s.label}</text>
      ))}
    </svg>
  );
}

// ── Reliability (calibration) chart ─────────────────────────────────────────
/**
 * Inputs: bins ({spread: [...], total: [...]}), each bin {lo, hi, n, mean_pred, hit_rate}.
 * Output: SVG scatter -- x = predicted P(pick wins), y = realized hit rate,
 * diagonal = perfect calibration, dot label = games in bin.
 */
function Reliability({ bins }) {
  const W = 300, H = 240, L = 40, R = 12, T = 12, B = 32;
  const x = (p) => L + ((p - 0.45) / 0.35) * (W - L - R);          // 45%..80% predicted
  const y = (p) => T + (1 - p) * (H - T - B);                        // 0..100% realized
  const clampX = (p) => Math.min(0.8, Math.max(0.45, p));
  const be = 110 / 210;
  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: '100%', height: 'auto' }} role="img" aria-label="Pick probability calibration">
      {[0, 0.25, 0.5, 0.75, 1].map(t => (
        <g key={t}>
          <line x1={L} x2={W - R} y1={y(t)} y2={y(t)} stroke={GRID} />
          <text x={L - 5} y={y(t) + 3} textAnchor="end" fontSize="10" fill="var(--text-muted)">{t * 100}%</text>
        </g>
      ))}
      {[0.5, 0.6, 0.7, 0.8].map(t => <text key={t} x={x(t)} y={H - 16} textAnchor="middle" fontSize="10" fill="var(--text-muted)">{t * 100}%</text>)}
      <text x={(L + W - R) / 2} y={H - 3} textAnchor="middle" fontSize="10" fill="var(--text-muted)">sim's P(pick wins)</text>
      <line x1={x(0.45)} y1={y(0.45)} x2={x(0.8)} y2={y(0.8)} stroke="var(--text-muted)" strokeDasharray="3 3" />
      <line x1={L} x2={W - R} y1={y(be)} y2={y(be)} stroke="var(--text-muted)" strokeOpacity="0.4" />
      <text x={W - R} y={y(be) - 4} textAnchor="end" fontSize="9" fill="var(--text-muted)">52.4% break-even</text>
      {[['spread', C_SPREAD, -3], ['total', C_TOTAL, 3]].map(([k, c, dx]) =>
        (bins[k] || []).filter(b => b.n > 0).map((b, i) => (
          <g key={`${k}${i}`}>
            <circle cx={x(clampX(b.mean_pred)) + dx} cy={y(b.hit_rate)} r={Math.min(9, 4 + b.n / 2)} fill={c} fillOpacity="0.85" stroke="var(--bg-deep)" strokeWidth="2">
              <title>{`${k}: predicted ${pct(b.mean_pred)} · hit ${pct(b.hit_rate)} · ${b.n} games`}</title>
            </circle>
          </g>
        )))}
    </svg>
  );
}

// ── PIT histogram ───────────────────────────────────────────────────────────
/**
 * Inputs: counts (10 ints -- actual results binned by where they fell in the
 * sim's own distribution), color, label. Output: SVG bar chart with the
 * "perfectly calibrated" uniform expectation as a reference line.
 */
export function PitHist({ counts, color, label }) {
  const W = 300, H = 150, L = 26, R = 8, T = 10, B = 30;
  const n = (counts || []).reduce((a, b) => a + b, 0);
  if (!n) return <Empty text="No graded games." />;
  const exp = n / 10;
  const max = Math.max(exp * 1.5, ...counts);
  const bw = (W - L - R) / 10;
  const y = (v) => T + (1 - v / max) * (H - T - B);
  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: '100%', height: 'auto' }} role="img" aria-label={label}>
      {counts.map((c, i) => (
        <rect key={i} x={L + i * bw + 1} y={y(c)} width={bw - 2} height={Math.max(0, H - B - y(c))} rx="2" fill={color}>
          <title>{`${label}: ${i * 10}-${i * 10 + 10}th percentile of sims — ${c} games (≈${exp.toFixed(1)} if calibrated)`}</title>
        </rect>
      ))}
      <line x1={L} x2={W - R} y1={y(exp)} y2={y(exp)} stroke="var(--text-white)" strokeDasharray="4 3" strokeOpacity="0.7" />
      <line x1={L} x2={W - R} y1={H - B} y2={H - B} stroke="var(--text-muted)" />
      <text x={L} y={H - B + 12} fontSize="9" fill="var(--text-muted)">below sims</text>
      <text x={W - R} y={H - B + 12} textAnchor="end" fontSize="9" fill="var(--text-muted)">above sims</text>
      <text x={(L + W - R) / 2} y={H - 4} textAnchor="middle" fontSize="10" fill="var(--text-main)">{label}</text>
    </svg>
  );
}

export function Empty({ text }) {
  return <div style={{ color: 'var(--text-muted)', fontSize: '0.8rem', padding: '16px', textAlign: 'center' }}>{text}</div>;
}

/** One scoreboard tile: title, our number vs. Vegas's, and a one-line verdict. */
export function Tile({ title, ours, theirs, oursLabel = 'Sim', theirsLabel = 'Vegas', note, noteColor }) {
  return (
    <div style={{ ...cardStyle, padding: '12px 14px', minWidth: 0 }}>
      <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.04em' }}>{title}</div>
      <div style={{ display: 'flex', gap: '14px', alignItems: 'baseline', marginTop: '6px' }}>
        <div><span style={{ fontSize: '1.35rem', fontWeight: 700, color: 'var(--text-white)' }}>{ours}</span>
          <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginLeft: 4 }}>{oursLabel}</span></div>
        {theirs != null && <div><span style={{ fontSize: '1rem', fontWeight: 600, color: 'var(--text-main)' }}>{theirs}</span>
          <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginLeft: 4 }}>{theirsLabel}</span></div>}
      </div>
      {note && <div style={{ fontSize: '0.72rem', marginTop: '4px', color: noteColor || 'var(--text-muted)' }}>{note}</div>}
    </div>
  );
}

function SliceTable({ title, rows }) {
  if (!rows || !rows.length) return null;
  return (
    <div style={{ minWidth: 0 }}>
      <div style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text-main)', marginBottom: 4 }}>{title}</div>
      <table style={{ fontSize: '0.76rem', width: '100%' }}>
        <tbody>
          {rows.map(r => (
            <tr key={r.key} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
              <td style={{ ...td, color: 'var(--text-main)' }}>{r.key}</td>
              <td style={td}>{recText(r)}</td>
              <td style={{ ...td, color: 'var(--text-muted)' }}>{r.win_pct == null ? '—' : pct(r.win_pct)}</td>
              <td style={{ ...td, textAlign: 'right', fontWeight: 600, color: unitsColor(r.units) }}>{signed(r.units, 2)}u</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

const SLICE_LABELS = { fav_dog: 'Favorite vs. underdog pick', home_away: 'Home vs. away pick',
  roof: 'Roof', div_game: 'Division game', side: 'Over vs. under pick' };

/**
 * Agree / disagree / strong-disagree grid (Cam, 2026-09-25).
 * Inputs: tiers ({spread|total|ml: [{tier, games, w, l, p, units, win_pct,
 * sim_closer_pct | sim_brier+vegas_brier}]}) for the selected line, kLabel.
 * Rows = market, columns = tier. Spread/total cells answer "when we disagree
 * by this much, whose number did the actual result land closer to?";
 * moneyline cells compare Brier scores (probability accuracy) in that tier.
 */
function AgreementCard({ tiers, kLabel }) {
  if (!tiers) return null;
  const MARKETS = [['spread', 'Spread', C_SPREAD], ['total', 'Total', C_TOTAL], ['ml', 'Moneyline', C_ML]];
  const COLS = [['agree', 'Agree', '≤1 pt · ≤3 win-prob pts'], ['disagree', 'Disagree', '1–2.5 pts · 3–7 win-prob pts'],
    ['strong', 'Strong disagreement', '>2.5 pts · >7 win-prob pts']];
  return (
    <div style={cardStyle}>
      <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>When we agree vs. disagree with Vegas — {kLabel} line</h3>
      <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
        Each game is sorted by how far our number is from Vegas's, separately for each market (a game can agree on the total and
        strongly disagree on the spread). <b>Sim closer</b> = share of games where the final result landed nearer our number than Vegas's
        (50% = a wash). Moneyline compares Brier scores (lower = better win probabilities); its record counts value bets only.
      </p>
      <div className="table-container" style={{ overflowX: 'auto' }}>
        <table style={{ fontSize: '0.78rem', width: '100%' }}>
          <thead>
            <tr>
              <th style={th}></th>
              {COLS.map(([k, label, sub]) => (
                <th key={k} style={{ ...th, borderLeft: '1px solid var(--border-glass)' }}>
                  {label}<div style={{ fontSize: '0.66rem', fontWeight: 400, color: 'var(--text-muted)' }}>{sub}</div>
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {MARKETS.map(([m, label, color]) => (
              <tr key={m} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                <td style={{ ...td, fontWeight: 600 }}><span style={{ color }}>●</span> {label}</td>
                {(tiers[m] || []).map(c => (
                  <td key={c.tier} style={{ ...td, borderLeft: '1px solid var(--border-glass)', verticalAlign: 'top' }}>
                    {c.games === 0 ? <span style={{ color: 'var(--text-muted)' }}>no games</span> : (<>
                      <div><span style={{ color: 'var(--text-muted)' }}>{c.games} games · </span>
                        {m === 'ml' && c.n === 0 ? <span style={{ color: 'var(--text-muted)' }}>no bets</span> : (<>
                          <b>{recText(c)}</b> <span style={{ color: unitsColor(c.units) }}>{signed(c.units, 2)}u</span></>)}
                      </div>
                      <div style={{ fontSize: '0.72rem', color: 'var(--text-main)', marginTop: 2 }}>
                        {m === 'ml'
                          ? <>Brier sim <b>{c.sim_brier?.toFixed(3) ?? '—'}</b> vs. Vegas {c.vegas_brier?.toFixed(3) ?? '—'}</>
                          : <>Sim closer: <b>{c.sim_closer_pct == null ? '—' : `${Math.round(c.sim_closer_pct * 100)}%`}</b></>}
                      </div>
                    </>)}
                  </td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

export default function GameLinesEval({ selectedWeek }) {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [kind, setKind] = useState('open');            // 'open' | 'close'
  const [weekFilter, setWeekFilter] = useState('page'); // 'page' | 'all'

  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(async () => {
      const d = await ApiService.getGameLinesEval();
      if (!cancelled) { setData(d); setLoading(false); }
    });
    return () => { cancelled = true; };
  }, [selectedWeek]);

  const s = data?.summary || {};
  const games = useMemo(() => {
    const all = data?.games || [];
    const rows = weekFilter === 'all' ? all : all.filter(g => g.week === Number(selectedWeek));
    return [...rows].sort((a, b) => (a.kickoff_ts ?? 0) - (b.kickoff_ts ?? 0));
  }, [data, weekFilter, selectedWeek]);

  const running = useMemo(() => (data?.running || []).map(r => ({
    ...r,
    mae_gap_margin: r.sim_margin_mae != null && r.close_margin_mae != null ? r.sim_margin_mae - r.close_margin_mae : null,
    mae_gap_total: r.sim_total_mae != null && r.close_total_mae != null ? r.sim_total_mae - r.close_total_mae : null,
  })), [data]);

  if (loading) return <div style={cardStyle}><Empty text="Loading game-line evaluation…" /></div>;
  if (!data) return <div style={cardStyle}><Empty text="Couldn't reach /api/eval/game_lines — is the DFS API (port 8002) running?" /></div>;

  const acc = s.accuracy || {};
  const rec = s.records || {};
  const kLabel = kind === 'open' ? 'opening' : 'closing';
  const beatBy = (ours, theirs) => (ours == null || theirs == null ? null : theirs - ours);
  const marginGap = beatBy(acc.margin?.sim_mae, acc.margin?.[`${kind}_mae`]);
  const totalGap = beatBy(acc.total?.sim_mae, acc.total?.[`${kind}_mae`]);
  const gapNote = (g) => (g == null ? null : g >= 0 ? `${f1(g)} pts better than Vegas` : `${f1(-g)} pts worse than Vegas`);
  const sl = s.slices || {};

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>
      {/* ── Header + controls ── */}
      <div style={{ ...cardStyle, display: 'flex', flexWrap: 'wrap', gap: '14px', alignItems: 'center' }}>
        <div style={{ flex: '1 1 320px' }}>
          <h2 style={{ margin: 0, fontSize: '1.05rem' }}>Game Lines</h2>
          <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '2px 0 0 0' }}>
            Our sim's spread, total and win probability vs. Vegas and vs. what happened. {s.n_played ?? 0} games graded
            ({s.n_pregame_verified ?? 0} provably simmed before kickoff). Small samples — treat records as provisional.
          </p>
        </div>
        <div style={{ display: 'flex', gap: '6px', alignItems: 'center' }}>
          <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>Grade vs.</span>
          <button style={btn(kind === 'open')} onClick={() => setKind('open')}>Opening line</button>
          <button style={btn(kind === 'close')} onClick={() => setKind('close')}>Closing line</button>
        </div>
      </div>

      {/* ── 1. Scoreboard ── */}
      {/* 300px min -> 3 across on desktop (2 even rows of 3), 2 on tablet, 1 on phone */}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '12px' }}>
        <Tile title="Margin error (avg pts off)" ours={f1(acc.margin?.sim_mae)} theirs={f1(acc.margin?.[`${kind}_mae`])}
          theirsLabel={`Vegas ${kind}`} note={gapNote(marginGap)} noteColor={marginGap >= 0 ? 'var(--accent-green)' : 'var(--text-muted)'} />
        <Tile title="Total error (avg pts off)" ours={f1(acc.total?.sim_mae)} theirs={f1(acc.total?.[`${kind}_mae`])}
          theirsLabel={`Vegas ${kind}`} note={`${gapNote(totalGap) || ''} · sim bias ${signed(acc.total?.sim_bias)} (+ = games went over us)`}
          noteColor={totalGap >= 0 ? 'var(--accent-green)' : 'var(--text-muted)'} />
        <Tile title={`ATS vs. ${kLabel}`} ours={recText(rec[`${kind}_spread`])} oursLabel=""
          theirs={`${signed(rec[`${kind}_spread`]?.units, 2)}u`} theirsLabel={pct(rec[`${kind}_spread`]?.win_pct)}
          note="Break-even at -110: 52.4%" />
        <Tile title={`O/U vs. ${kLabel}`} ours={recText(rec[`${kind}_total`])} oursLabel=""
          theirs={`${signed(rec[`${kind}_total`]?.units, 2)}u`} theirsLabel={pct(rec[`${kind}_total`]?.win_pct)}
          note="Break-even at -110: 52.4%" />
        <Tile title="Line movement (open → close)" ours={signed(s.clv?.spread?.avg_pts)} oursLabel="spread pts"
          theirs={signed(s.clv?.total?.avg_pts)} theirsLabel="total pts"
          note={`Toward our opening side: ${s.clv?.spread?.moved_toward_us ?? 0}/${s.clv?.spread?.moved_against_us ?? 0} spread, ${s.clv?.total?.moved_toward_us ?? 0}/${s.clv?.total?.moved_against_us ?? 0} total (toward/against)`} />
        <Tile title={`Moneyline value bets vs. ${kLabel}`} ours={recText(rec[`${kind}_ml`])} oursLabel=""
          theirs={`${signed(rec[`${kind}_ml`]?.units, 2)}u`} theirsLabel="at real odds"
          note={`Bet only when our win prob beats the price incl. vig · Brier sim ${s.win_prob?.sim_brier?.toFixed(3) ?? '—'} vs. Vegas ${s.win_prob?.vegas_brier?.toFixed(3) ?? '—'} (lower = better)`} />
      </div>

      {/* ── 2. Running trend ── */}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: '12px' }}>
        <div style={cardStyle}>
          <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Cumulative units vs. {kLabel} line</h3>
          <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 6px 0' }}>
            Spread and total: the sim's side every game at -110. Moneyline: value bets only, paid at the real odds.
          </p>
          <LineChart data={running} yLabel="Cumulative units" series={[
            { key: `units_${kind}_spread`, label: 'Spread', color: C_SPREAD },
            { key: `units_${kind}_total`, label: 'Total', color: C_TOTAL },
            { key: `units_${kind}_ml`, label: 'Moneyline', color: C_ML },
          ]} />
        </div>
        <div style={cardStyle}>
          <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Accuracy gap vs. Vegas closing line</h3>
          <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 6px 0' }}>Season-to-date sim error minus Vegas error (pts). Below 0 = we're more accurate than the close.</p>
          <LineChart data={running} yLabel="Sim MAE minus Vegas MAE" series={[
            { key: 'mae_gap_margin', label: 'Margin', color: C_SPREAD },
            { key: 'mae_gap_total', label: 'Total', color: C_TOTAL },
          ]} />
        </div>
      </div>

      {/* ── 3. Calibration ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Calibration</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          <b>Left:</b> when the sim says its side wins X% of the time vs. the {kLabel} line, does it? Dots on the dashed diagonal = calibrated
          (<span style={{ color: C_SPREAD }}>●</span> spread, <span style={{ color: C_TOTAL }}>●</span> total; bigger dot = more games).{' '}
          <b>Right:</b> where each actual result landed inside the sim's own range of outcomes. Calibrated sims give flat bars near the dashed line;
          a pile on the right = reality kept beating our numbers (Week 1 overs); tall ends and a low middle = our ranges are too narrow.
        </p>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: '14px', alignItems: 'start' }}>
          <Reliability bins={{ spread: s.calibration?.[`${kind}_spread`], total: s.calibration?.[`${kind}_total`] }} />
          <PitHist counts={s.calibration?.pit_margin} color={C_SPREAD} label="Actual margin vs. sim range" />
          <PitHist counts={s.calibration?.pit_total} color={C_TOTAL} label="Actual total vs. sim range" />
        </div>
      </div>

      {/* ── 4. Agree vs. disagree with Vegas ── */}
      <AgreementCard tiers={s.tiers?.[kind]} kLabel={kLabel} />

      {/* ── 5. Where we win / lose ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Where we win and lose — vs. {kLabel} line</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          Record and units for the sim's picks, split by game type. Spread "favorite/underdog" = whether the sim took the team
          laying or getting points; moneyline "favorite/underdog" = the price of the team we bet.
        </p>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(230px, 1fr))', gap: '16px' }}>
          {['fav_dog', 'home_away', 'roof', 'div_game'].map(d => (
            <SliceTable key={`s${d}`} title={`Spread · ${SLICE_LABELS[d]}`} rows={sl[`${kind}_spread_by_${d}`]} />
          ))}
          {['side', 'roof'].map(d => (
            <SliceTable key={`t${d}`} title={`Total · ${SLICE_LABELS[d]}`} rows={sl[`${kind}_total_by_${d}`]} />
          ))}
          {['fav_dog', 'home_away'].map(d => (
            <SliceTable key={`m${d}`} title={`Moneyline · ${SLICE_LABELS[d].replace(' pick', ' bet')}`} rows={sl[`${kind}_ml_by_${d}`]} />
          ))}
        </div>
        {(sl.week || []).length > 0 && (
          <div className="table-container" style={{ overflowX: 'auto', marginTop: '14px' }}>
            <div style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text-main)', marginBottom: 4 }}>By week</div>
            <table style={{ fontSize: '0.76rem', width: '100%' }}>
              <thead><tr>
                <th style={th}>Week</th><th style={th}>Games</th>
                <th style={th} title="Actual total minus sim total, averaged. + = games went over our number">Sim total bias</th>
                <th style={th} title="Actual total minus Vegas closing total, averaged">Vegas total bias</th>
                <th style={th}>Overs hit</th>
                <th style={th}>Margin err: sim / Vegas</th>
                <th style={th}>ATS ({kind})</th><th style={th}>O/U ({kind})</th><th style={th}>ML bets ({kind})</th>
              </tr></thead>
              <tbody>
                {sl.week.map(w => (
                  <tr key={w.week} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                    <td style={td}>W{w.week}</td><td style={td}>{w.n}</td>
                    <td style={td}>{signed(w.sim_total_bias)}</td><td style={td}>{signed(w.close_total_bias)}</td>
                    <td style={td}>{w.overs_hit}/{w.n}</td>
                    <td style={td}>{f1(w.sim_margin_mae)} / {f1(w.close_margin_mae)}</td>
                    <td style={td}>{recText(w[`${kind}_spread`])} <span style={{ color: unitsColor(w[`${kind}_spread`]?.units) }}>({signed(w[`${kind}_spread`]?.units, 2)}u)</span></td>
                    <td style={td}>{recText(w[`${kind}_total`])} <span style={{ color: unitsColor(w[`${kind}_total`]?.units) }}>({signed(w[`${kind}_total`]?.units, 2)}u)</span></td>
                    <td style={td}>{recText(w[`${kind}_ml`])} <span style={{ color: unitsColor(w[`${kind}_ml`]?.units) }}>({signed(w[`${kind}_ml`]?.units, 2)}u)</span></td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      {/* ── 5. Per-game table ── */}
      <div style={cardStyle}>
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: '10px', alignItems: 'center', marginBottom: '8px' }}>
          <h3 style={{ margin: 0, fontSize: '0.92rem', flex: '1 1 auto' }}>Games — sim vs. Vegas {kLabel} line vs. actual</h3>
          <button style={btn(weekFilter === 'page')} onClick={() => setWeekFilter('page')}>Week {selectedWeek}</button>
          <button style={btn(weekFilter === 'all')} onClick={() => setWeekFilter('all')}>All weeks</button>
        </div>
        {games.length === 0 ? <Empty text={`No simmed games for week ${selectedWeek}.`} /> : (
          <div className="table-container" style={{ overflowX: 'auto' }}>
            <table style={{ fontSize: '0.76rem', width: '100%' }}>
              <thead>
                <tr style={{ color: 'var(--text-muted)' }}>
                  <th style={th}>Game</th>
                  <th style={{ ...th, borderLeft: '1px solid var(--border-glass)' }}>Sim spread</th>
                  <th style={th}>Vegas</th><th style={th}>Actual</th>
                  <th style={th} title="Side the sim takes at the Vegas number, and the sim's P(that side covers | no push)">Pick</th>
                  <th style={th}>Result</th>
                  <th style={th} title="Open→close move relative to the side we'd take at the OPEN. + = market moved our way">CLV</th>
                  <th style={{ ...th, borderLeft: '1px solid var(--border-glass)' }}>Sim total</th>
                  <th style={th}>Vegas</th><th style={th}>Actual</th><th style={th}>Pick</th><th style={th}>Result</th><th style={th}>CLV</th>
                  <th style={{ ...th, borderLeft: '1px solid var(--border-glass)' }}>ML (away / home)</th>
                  <th style={th} title="Home team's win probability: our sim vs. Vegas with the vig removed">Home win: sim / Vegas</th>
                  <th style={th} title="Bet the side whose sim win prob beats the price's break-even (vig included). Edge = sim prob minus that break-even.">Value bet</th>
                  <th style={th}>Result</th>
                </tr>
              </thead>
              <tbody>
                {games.map(g => {
                  const sp = g[`${kind}_spread`], tt = g[`${kind}_total`];
                  const sres = g[`${kind}_spread_result`], tres = g[`${kind}_total_result`];
                  return (
                    <tr key={g.game_id} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                      <td style={{ ...td, fontWeight: 600 }}>
                        {weekFilter === 'all' && <span style={{ color: 'var(--text-muted)', fontWeight: 400 }}>W{g.week} </span>}
                        {g.away_team} @ {g.home_team}
                        {g.sim_timing !== 'pregame' && (
                          <span title={g.sim_timing === 'unknown'
                            ? 'Simmed before the kickoff lock existed and the file was rewritten after kickoff — graded anyway, but not provably pre-game.'
                            : 'Simmed after kickoff'} style={{ marginLeft: 6, fontSize: '0.68rem', color: 'var(--accent-gold)' }}>⚠ timing</span>)}
                      </td>
                      <td style={{ ...td, borderLeft: '1px solid var(--border-glass)', color: 'var(--text-white)' }}>{spreadText(g.home_team, g.away_team, g.sim_margin)}</td>
                      <td style={td} title={g[`${kind}_spread_src`] === 'override' ? 'Hand-entered line (line_overrides.csv)' : 'nflverse feed'}>
                        {spreadText(g.home_team, g.away_team, sp)}{g[`${kind}_spread_src`] === 'override' ? '*' : ''}</td>
                      <td style={td}>{g.played ? resultText(g.home_team, g.away_team, g.actual_margin) : '—'}</td>
                      <td style={td}>{g[`${kind}_spread_pick`] ?? '—'} <span style={{ color: 'var(--text-muted)' }}>{g[`${kind}_spread_pick_prob`] != null ? pct(g[`${kind}_spread_pick_prob`]) : ''}</span>
                        <TierTag t={g[`${kind}_spread_tier`]} /></td>
                      <td style={{ ...td, fontWeight: 700, color: resColor[sres] }}>{sres ?? '—'}</td>
                      <td style={{ ...td, color: unitsColor(g.spread_clv) }}>{signed(g.spread_clv)}</td>
                      <td style={{ ...td, borderLeft: '1px solid var(--border-glass)', color: 'var(--text-white)' }}>{f1(g.sim_total)}</td>
                      <td style={td} title={g[`${kind}_total_src`] === 'override' ? 'Hand-entered line (line_overrides.csv)' : 'nflverse feed'}>
                        {f1(tt)}{g[`${kind}_total_src`] === 'override' ? '*' : ''}</td>
                      <td style={td}>{g.played ? g.actual_total : '—'}</td>
                      <td style={td}>{g[`${kind}_total_pick`] ?? '—'} <span style={{ color: 'var(--text-muted)' }}>{g[`${kind}_total_pick_prob`] != null ? pct(g[`${kind}_total_pick_prob`]) : ''}</span>
                        <TierTag t={g[`${kind}_total_tier`]} /></td>
                      <td style={{ ...td, fontWeight: 700, color: resColor[tres] }}>{tres ?? '—'}</td>
                      <td style={{ ...td, color: unitsColor(g.total_clv) }}>{signed(g.total_clv)}</td>
                      <td style={{ ...td, borderLeft: '1px solid var(--border-glass)', color: 'var(--text-main)' }}>
                        {oddsText(g[`${kind}_ml_away`])} / {oddsText(g[`${kind}_ml_home`])}</td>
                      <td style={td}>{g.home_team} {pct(g.sim_home_win)} / <span style={{ color: C_VEGAS }}>{pct(g[`${kind}_ml_novig_home`])}</span></td>
                      <td style={td}>
                        {g[`${kind}_ml_pick`]
                          ? <>{g[`${kind}_ml_pick`]} {oddsText(g[`${kind}_ml_pick_odds`])}{' '}
                              <span style={{ color: 'var(--text-muted)' }}>+{(g[`${kind}_ml_edge`] * 100).toFixed(1)} pts</span></>
                          : <span style={{ color: 'var(--text-muted)' }}>no value</span>}
                        <TierTag t={g[`${kind}_ml_tier`]} />
                      </td>
                      <td style={{ ...td, fontWeight: 700, color: resColor[g[`${kind}_ml_result`]] }}>
                        {g[`${kind}_ml_result`] ?? '—'}
                        {g[`${kind}_ml_units`] != null && g[`${kind}_ml_result`] && (
                          <span style={{ fontWeight: 400, color: unitsColor(g[`${kind}_ml_units`]) }}> {signed(g[`${kind}_ml_units`], 2)}u</span>)}
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
            <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: 6 }}>
              * hand-entered line from <code>data/eval/2026/line_overrides.csv</code>. Sim line = mean of all sim runs. Actual uses the same
              notation as the lines (winner gets the minus: "BUF -10" = BUF won by 10). Tags = how far our line is from Vegas's
              (agree / disagree / strong). CLV is always measured from the opening line.
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
