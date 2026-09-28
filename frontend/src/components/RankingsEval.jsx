import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';
import { Empty } from './GameLinesEval';

/**
 * Rankings evaluation (2026-09-26) -- our de facto weekly positional rankings
 * (the Slate Leaders page) graded against actual positional finishes, in
 * season-long scoring (PPR / half / standard, 4- or 6-pt pass TD, no yardage
 * bonuses -- the exact Slate Leaders formula). Rendered on EvaluationTab.jsx.
 *
 * Inputs (props): selectedWeek -- the page week (table default when complete).
 * Data: GET /api/eval/rankings?fmt= (src/evaluation/rankings_eval.py).
 *
 * Both ranking bases are graded (Cam, 2026-09-26): by our projected MEAN and
 * by our projected MEDIAN (the Slate Leaders default sort). The "Mean vs.
 * median" card says which ranked better -- that decides the page's default.
 * Season-long (draft / rest-of-season) rankings are only SNAPSHOTTED so far
 * (src/evaluation/season_rankings.py); their grading comes later in the year.
 */

const C_DOT = '#0fa3b1';
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const btn = (active) => ({
  background: active ? 'rgba(15,163,177,0.18)' : 'rgba(0,0,0,0.25)',
  border: `1px solid ${active ? C_DOT : 'rgba(255,255,255,0.14)'}`,
  borderRadius: '6px', color: 'var(--text-white)', padding: '4px 10px', fontSize: '0.8rem', cursor: 'pointer',
});
const selStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px',
  color: 'var(--text-white)', padding: '4px 8px', fontSize: '0.8rem' };
const th = { padding: '6px 6px', textAlign: 'left', whiteSpace: 'nowrap' };
const td = { padding: '5px 6px', whiteSpace: 'nowrap' };
const POSITIONS = ['QB', 'RB', 'WR', 'TE'];

const num = (v, d = 1) => (v == null ? '—' : Number(v).toFixed(d));
const pct0 = (v) => (v == null ? '—' : `${Math.round(v * 100)}%`);
const corr = (v) => (v == null ? '—' : Number(v).toFixed(2));
const signed = (v, d = 0) => (v == null ? '—' : `${v > 0 ? '+' : ''}${Number(v).toFixed(d)}`);
const dateText = (ts) => (ts == null ? '—' : new Date(ts * 1000).toLocaleDateString(undefined, { month: 'short', day: 'numeric' }));

/**
 * Reliability chart for the top-12 probability (0-100% on both axes).
 * Inputs: bins [{lo, hi, n, mean_pred, hit_rate}]. Diagonal = calibrated.
 * Dot size scales with the bin's player count; hover gives exact numbers.
 */
function Top12Reliability({ bins }) {
  const W = 300, H = 240, L = 40, R = 12, T = 12, B = 32;
  const x = (p) => L + p * (W - L - R);
  const y = (p) => T + (1 - p) * (H - T - B);
  const ticks = [0, 0.25, 0.5, 0.75, 1];
  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: '100%', maxWidth: 420, height: 'auto' }} role="img" aria-label="Top-12 probability calibration">
      {ticks.map(t => (
        <g key={t}>
          <line x1={L} x2={W - R} y1={y(t)} y2={y(t)} stroke="rgba(255,255,255,0.08)" />
          <text x={L - 5} y={y(t) + 3} textAnchor="end" fontSize="10" fill="var(--text-muted)">{t * 100}%</text>
          <text x={x(t)} y={H - 16} textAnchor="middle" fontSize="10" fill="var(--text-muted)">{t * 100}%</text>
        </g>
      ))}
      <text x={(L + W - R) / 2} y={H - 3} textAnchor="middle" fontSize="10" fill="var(--text-muted)">our P(top-12 finish)</text>
      <line x1={x(0)} y1={y(0)} x2={x(1)} y2={y(1)} stroke="var(--text-muted)" strokeDasharray="3 3" />
      {(bins || []).filter(b => b.n > 0).map((b, i) => (
        <circle key={i} cx={x(b.mean_pred)} cy={y(b.hit_rate)} r={Math.min(10, 4 + Math.sqrt(b.n) / 2)}
          fill={C_DOT} fillOpacity="0.85" stroke="var(--bg-deep)" strokeWidth="2">
          <title>{`Predicted ${pct0(b.mean_pred)} · actually top-12 ${pct0(b.hit_rate)} · ${b.n} player-weeks`}</title>
        </circle>
      ))}
    </svg>
  );
}

function MissTable({ title, note, rows }) {
  return (
    <div style={{ minWidth: 0 }}>
      <div style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text-main)', marginBottom: 2 }}>{title}</div>
      <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginBottom: 4 }}>{note}</div>
      {rows.length === 0 ? <Empty text="None." /> : (
        <table style={{ fontSize: '0.75rem', width: '100%' }}>
          <thead><tr style={{ color: 'var(--text-muted)' }}>
            <th style={th}>Wk</th><th style={th}>Player</th><th style={th}>Our rank</th><th style={th}>Finished</th><th style={th}>Pts: proj → actual</th>
          </tr></thead>
          <tbody>{rows.map((r, i) => (
            <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
              <td style={td}>{r.week}</td>
              <td style={{ ...td, fontWeight: 600 }}>{r.Player} <span style={{ color: 'var(--text-muted)', fontWeight: 400 }}>{r.Team}</span></td>
              <td style={td}>{r.Pos}{r.rank_mean}</td>
              <td style={{ ...td, color: 'var(--text-white)' }}>{r.Pos}{r.actual_rank}</td>
              <td style={{ ...td, color: 'var(--text-muted)' }}>{num(r.proj_mean)} → {num(r.actual_score)}</td>
            </tr>))}
          </tbody>
        </table>
      )}
    </div>
  );
}

export default function RankingsEval({ selectedWeek }) {
  const [fmt, setFmt] = useState('4_ppr');
  const [basis, setBasis] = useState('mean');
  const [tablePos, setTablePos] = useState('WR');
  const [tableWeek, setTableWeek] = useState(null);     // null = page week if complete, else latest complete
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(async () => {
      if (cancelled) return;
      setLoading(true);
      const d = await ApiService.getRankingsEval(fmt);
      if (!cancelled) { setData(d); setLoading(false); }
    });
    return () => { cancelled = true; };
  }, [fmt]);

  const done = data?.completed_weeks || [];
  const effWeek = tableWeek ?? (done.includes(Number(selectedWeek)) ? Number(selectedWeek) : done[done.length - 1]);
  const rankKey = `rank_${basis}`;
  const tableRows = useMemo(() => (data?.rows || [])
    .filter(r => r.week === effWeek && r.Pos === tablePos)
    .sort((a, b) => a[rankKey] - b[rankKey]), [data, effWeek, tablePos, rankKey]);

  const formats = data?.formats || { '4_ppr': 'PPR', '4_half': 'Half-PPR', '4_std': 'Standard' };
  const header = (
    <div style={{ ...cardStyle, display: 'flex', flexWrap: 'wrap', gap: '14px', alignItems: 'center' }}>
      <div style={{ flex: '1 1 380px' }}>
        <h2 style={{ margin: 0, fontSize: '1.05rem' }}>Rankings</h2>
        <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '2px 0 0 0' }}>
          Our weekly positional rankings (Slate Leaders) vs. where players actually finished, in season-long scoring — no 100/300-yard
          bonuses, INT −2, fumble −1. Actual finishes rank everyone at the position, including players we didn't project.
          Only fully completed weeks count.
        </p>
      </div>
      <div style={{ display: 'flex', gap: '8px', alignItems: 'center', flexWrap: 'wrap' }}>
        <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>Scoring</span>
        <select value={fmt} onChange={e => setFmt(e.target.value)} style={selStyle}>
          {Object.entries(formats).map(([k, v]) => <option key={k} value={k}>{v}</option>)}
        </select>
        <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginLeft: 6 }}>Rank by</span>
        <button style={btn(basis === 'mean')} onClick={() => setBasis('mean')}>Mean</button>
        <button style={btn(basis === 'median')} onClick={() => setBasis('median')} title="The Slate Leaders page's default sort">Median</button>
      </div>
      {data && (
        <div style={{ flexBasis: '100%', fontSize: '0.72rem', color: 'var(--text-muted)' }}>
          Graded weeks: {done.length ? done.map(w => `W${w}`).join(', ') : 'none yet'}
          {(data.pending_weeks || []).length > 0 && <> · waiting on final games: {data.pending_weeks.map(w => `W${w}`).join(', ')}</>}
          {' · '}Season-long rankings: {data.season_snapshots?.count ?? 0} snapshots saved
          {data.season_snapshots?.preseason_at ? ` (preseason ${dateText(data.season_snapshots.preseason_at)}, latest ${dateText(data.season_snapshots.latest_at)})` : ''}
          {' '}— graded later in the season.
        </div>
      )}
    </div>
  );

  if (loading) return <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>{header}
    <div style={cardStyle}><Empty text="Grading rankings… (a newly completed week takes ~30s the first time)" /></div></div>;
  if (!data) return <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>{header}
    <div style={cardStyle}><Empty text="Couldn't reach /api/eval/rankings — is the DFS API (port 8002) running?" /></div></div>;
  const s = data.summary || {};
  if (!done.length) return <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>{header}
    <div style={cardStyle}><Empty text="No fully completed weeks with player stats yet." /></div></div>;

  const bp = s.by_pos || {};
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>
      {header}

      {/* ── 1. Mean vs. median ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Mean vs. median — which ranks better?</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          <b>Rank correlation</b>: how well our order matched the actual finish order among the players who matter (our top
          24 QB / 48 RB / 72 WR / 24 TE) — 1.00 = perfect order, 0 = random. <b>Top-12 hit</b>: of our top 12, the share who
          finished top 12. Tiny gaps between mean and median are noise at this sample size.
        </p>
        <div className="table-container" style={{ overflowX: 'auto' }}>
          <table style={{ fontSize: '0.8rem', width: '100%' }}>
            <thead><tr style={{ color: 'var(--text-muted)' }}>
              <th style={th}>Pos</th>
              <th style={th}>Rank correlation: mean / median</th>
              <th style={th}>Top-12 hit: mean / median</th>
              <th style={th}>Better basis</th>
            </tr></thead>
            <tbody>{POSITIONS.filter(p => bp[p]).map(p => (
              <tr key={p} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                <td style={{ ...td, fontWeight: 600 }}>{p}</td>
                <td style={td}>{corr(bp[p].mean.spearman)} / {corr(bp[p].median.spearman)}</td>
                <td style={td}>{pct0(bp[p].mean.hits['12']?.hit)} / {pct0(bp[p].median.hits['12']?.hit)}</td>
                <td style={{ ...td, color: 'var(--text-white)', fontWeight: 600 }}>{s.winner?.[p] ?? '—'}</td>
              </tr>))}
            </tbody>
          </table>
        </div>
      </div>

      {/* ── 2. Accuracy by position (chosen basis) ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Ranking accuracy — {formats[fmt]}, ranked by {basis}</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          <b>Avg rank miss</b>: average spots between our rank and the actual finish (we said WR8, he finished WR20 = 12), split by
          where we ranked him. <b>Hit rates</b>: of our top N, the share who actually finished top N — the start/sit test.
        </p>
        <div className="table-container" style={{ overflowX: 'auto' }}>
          <table style={{ fontSize: '0.8rem', width: '100%' }}>
            <thead><tr style={{ color: 'var(--text-muted)' }}>
              <th style={th}>Pos</th><th style={th}>Rank correlation</th><th style={th}>Avg rank miss</th>
              <th style={th}>…our top 12</th><th style={th}>…13–24</th><th style={th}>…25+</th>
              <th style={th}>Top-12 hit</th><th style={th}>Top-24 hit</th><th style={th}>Top-36 hit</th>
            </tr></thead>
            <tbody>{POSITIONS.filter(p => bp[p]).map(p => {
              const b = bp[p][basis];
              const hit = (n) => (b.hits[n] ? `${pct0(b.hits[n].hit)} (${b.hits[n].n})` : '—');
              return (
                <tr key={p} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                  <td style={{ ...td, fontWeight: 600 }}>{p}</td>
                  <td style={{ ...td, color: 'var(--text-white)' }}>{corr(b.spearman)}</td>
                  <td style={td}>{num(b.mae_rank)}</td>
                  <td style={td}>{num(b.tiers['1-12'])}</td><td style={td}>{num(b.tiers['13-24'])}</td><td style={td}>{num(b.tiers['25+'])}</td>
                  <td style={td}>{hit('12')}</td><td style={td}>{hit('24')}</td><td style={td}>{hit('36')}</td>
                </tr>
              );
            })}</tbody>
          </table>
        </div>
        {(s.by_week || []).length > 0 && (
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: 8 }}>
            By week (rank correlation, {basis}): {done.map(w => (
              <span key={w} style={{ marginRight: 14 }}>W{w}: {POSITIONS.map(p => {
                const r = s.by_week.find(x => x.week === w && x.pos === p);
                return `${p} ${corr(r?.[`${basis}_spearman`])}`;
              }).join(' · ')}</span>))}
          </div>
        )}
      </div>

      {/* ── 3. Top-12 probability calibration ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Are our top-12 odds honest?</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          Slate Leaders shows each player's chance of a top-12 finish. When we said 60%, did about 60% of those players get there?
          Dots on the dashed diagonal = honest odds; below it = we were too optimistic. <b>Skill</b> compares our odds to a naive
          guess that gives every player the same chance: 0% = no better than naive, higher = better.
        </p>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(260px, 1fr))', gap: 16, alignItems: 'center' }}>
          <Top12Reliability bins={s.prob?.bins} />
          <table style={{ fontSize: '0.8rem' }}>
            <thead><tr style={{ color: 'var(--text-muted)' }}>
              <th style={th}>Pos</th>
              <th style={th} title="Mean squared error of our top-12 probabilities, lower = better">Brier: ours</th>
              <th style={th} title="Brier of a naive forecast giving every projected player the same base-rate chance">vs. naive</th>
              <th style={th} title="1 - ours/naive. 0 = no better than naive, 1 = perfect, below 0 = worse than naive">Skill</th>
            </tr></thead>
            <tbody>
              {[...POSITIONS.filter(p => bp[p]?.prob).map(p => [p, bp[p].prob]), ['All', s.prob]].map(([p, pr]) => (
                <tr key={p} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                  <td style={{ ...td, fontWeight: 600 }}>{p}</td>
                  <td style={td}>{num(pr?.brier_top12, 3)}</td>
                  <td style={{ ...td, color: 'var(--text-muted)' }}>{num(pr?.brier_top12_naive, 3)}</td>
                  <td style={{ ...td, color: 'var(--text-white)', fontWeight: 600 }}>{pr?.skill_top12 == null ? '—' : pct0(pr.skill_top12)}</td>
                </tr>))}
            </tbody>
          </table>
        </div>
      </div>

      {/* ── 4. Biggest misses ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 10px 0', fontSize: '0.92rem' }}>Biggest ranking misses — {formats[fmt]} (by mean)</h3>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: 16 }}>
          <MissTable title="Ranked too low" note="Finished far above where we ranked them." rows={s.misses?.underrated || []} />
          <MissTable title="Ranked too high" note="Finished far below where we ranked them." rows={s.misses?.overrated || []} />
        </div>
        {(s.repeat || []).length > 0 && (
          <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)', marginTop: 12 }}>
            <b style={{ color: 'var(--text-main)' }}>Repeat patterns (2+ weeks):</b>{' '}
            too low — {s.repeat.filter(r => r.avg_diff >= 8).slice(0, 6).map(r => `${r.Player} (${r.Pos}${Math.round(r.avg_rank)} → ${r.Pos}${Math.round(r.avg_finish)})`).join(', ') || 'none'};
            {' '}too high — {[...s.repeat].reverse().filter(r => r.avg_diff <= -8).slice(0, 6).map(r => `${r.Player} (${r.Pos}${Math.round(r.avg_rank)} → ${r.Pos}${Math.round(r.avg_finish)})`).join(', ') || 'none'}.
          </div>
        )}
      </div>

      {/* ── 5. Weekly ranking table ── */}
      <div style={cardStyle}>
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: '8px', alignItems: 'center', marginBottom: 8 }}>
          <h3 style={{ margin: 0, fontSize: '0.92rem', flex: '1 1 auto' }}>Week {effWeek} {tablePos} rankings vs. finish</h3>
          {POSITIONS.map(p => <button key={p} style={btn(tablePos === p)} onClick={() => setTablePos(p)}>{p}</button>)}
          <select value={effWeek} onChange={e => setTableWeek(Number(e.target.value))} style={selStyle}>
            {done.map(w => <option key={w} value={w}>Week {w}</option>)}
          </select>
        </div>
        {!done.includes(Number(selectedWeek)) && tableWeek == null && (
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginBottom: 6 }}>
            Week {selectedWeek} isn't complete yet — showing Week {effWeek}.
          </div>
        )}
        <div className="table-container" style={{ overflowX: 'auto', maxHeight: 520, overflowY: 'auto' }}>
          <table style={{ fontSize: '0.76rem', width: '100%' }}>
            <thead><tr style={{ color: 'var(--text-muted)' }}>
              <th style={th}>Our rank</th><th style={th}>Player</th><th style={th}>Proj pts ({basis})</th>
              <th style={th}>P(top-12)</th><th style={th}>Actual pts</th><th style={th}>Finished</th>
              <th style={th} title="Our rank minus actual finish: + = finished better than we ranked">Diff</th>
            </tr></thead>
            <tbody>{tableRows.map(r => {
              const diff = r[rankKey] - r.actual_rank;
              return (
                <tr key={`${r.Player}-${r.Team}`} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                  <td style={{ ...td, fontWeight: 600 }}>{tablePos}{r[rankKey]}</td>
                  <td style={td}>{r.Player} <span style={{ color: 'var(--text-muted)' }}>{r.Team}</span></td>
                  <td style={td}>{num(r[`proj_${basis}`])}</td>
                  <td style={{ ...td, color: 'var(--text-muted)' }}>{pct0(r.p_top12)}</td>
                  <td style={{ ...td, color: 'var(--text-white)' }}>{num(r.actual_score)}</td>
                  <td style={td}>{tablePos}{r.actual_rank}</td>
                  <td style={{ ...td, color: Math.abs(diff) >= 12 ? 'var(--text-white)' : 'var(--text-muted)', fontWeight: Math.abs(diff) >= 12 ? 700 : 400 }}>
                    {signed(diff)}</td>
                </tr>
              );
            })}</tbody>
          </table>
        </div>
        {(data.absent || []).length > 0 && (
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', marginTop: 8 }}>
            Ranked but no stat line (inactive etc., not graded): {data.absent.map(r => `W${r.week} ${r.Player} (${r.Pos}${r.rank_mean})`).join(', ')}.
          </div>
        )}
      </div>
    </div>
  );
}
