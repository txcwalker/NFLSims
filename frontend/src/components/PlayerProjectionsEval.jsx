import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';
import { PitHist, Empty, Tile } from './GameLinesEval';

/**
 * Player Projections evaluation (2026-09-25) -- our sim's player projections
 * vs. the real stat line, and where that real line landed inside the player's
 * own 10K sim runs ("percentile finish"). Rendered under Game Lines on
 * pages/EvaluationPage.jsx. Evaluated against our sims only for now; prop lines
 * (Vegas / prediction markets) are a later column on the same rows.
 *
 * Inputs (props): selectedWeek -- the page's week (default table filter).
 * Data: GET /api/eval/player_projections (src/evaluation/player_proj_eval.py)
 *   rows (every graded QB/RB/WR/TE player-game, all stats), summary (server-
 *   side, filtered by min projection), no_stat_line, unprojected, stale_games.
 *
 * Reading the percentile: 0.94 = the real line beat 94% of our sim runs for
 * that player. Across many players, percentiles should spread evenly (flat
 * histogram); inside the 10-90 range ~80% of the time, 25-75 ~50%.
 *
 * Defaults (Cam, 2026-09-25): players projected < 5 DK hidden (toggle);
 * no-stat-line players listed separately, never graded as zeros; DST/K skipped.
 */

const C_BAND = '#0fa3b1';        // same validated cyan as Game Lines' spread series
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const btn = (active) => ({
  background: active ? 'rgba(15,163,177,0.18)' : 'rgba(0,0,0,0.25)',
  border: `1px solid ${active ? C_BAND : 'rgba(255,255,255,0.14)'}`,
  borderRadius: '6px', color: 'var(--text-white)', padding: '4px 10px', fontSize: '0.8rem', cursor: 'pointer',
});
const selStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px',
  color: 'var(--text-white)', padding: '4px 8px', fontSize: '0.8rem' };
const th = { padding: '6px 5px', textAlign: 'left', whiteSpace: 'nowrap' };
const td = { padding: '5px 5px', whiteSpace: 'nowrap' };
const POSITIONS = ['ALL', 'QB', 'RB', 'WR', 'TE'];

const pct0 = (v) => (v == null ? '—' : `${Math.round(v * 100)}%`);
const num = (v, d = 1) => (v == null ? '—' : Number(v).toFixed(d));
const signed = (v, d = 1) => (v == null ? '—' : `${v > 0 ? '+' : ''}${Number(v).toFixed(d)}`);
const ordinal = (p) => {
  if (p == null) return '—';
  const n = Math.round(p * 100);
  const s = n % 100 >= 11 && n % 100 <= 13 ? 'th' : ({ 1: 'st', 2: 'nd', 3: 'rd' }[n % 10] || 'th');
  return `${n}${s}`;
};
/**
 * Coverage vs. its target, sample-size aware: only call it off-target when
 * the gap exceeds 2 standard errors of a binomial proportion at this n
 * (n=392 -> ~4 pts for the 80% band; n=20 -> ~18 pts), so small slices
 * don't cry wolf and big ones don't hide a real miss.
 */
const covNote = (v, target, n) => {
  if (v == null || !n) return null;
  const tol = 2 * Math.sqrt((target * (1 - target)) / n);
  if (Math.abs(v - target) <= tol) return `target ${pct0(target)} — within noise (±${Math.round(tol * 100)} pts at n=${n})`;
  return `target ${pct0(target)} — ranges too ${v < target ? 'narrow' : 'wide'} (beyond ±${Math.round(tol * 100)} pts noise)`;
};

/**
 * One row's projection range: 10-90 band, median tick, actual dot.
 * Inputs: q10/q50/q90/actual (numbers). Scale is per row (its own band +
 * actual), so it shows WHERE the actual fell, not magnitude across rows.
 */
function RangeBar({ q10, q50, q90, actual }) {
  const W = 120, H = 16;
  if (q10 == null || q90 == null || actual == null) return null;
  let lo = Math.min(q10, actual), hi = Math.max(q90, actual);
  if (hi - lo < 1e-9) { lo -= 1; hi += 1; }
  const pad = (hi - lo) * 0.08; lo -= pad; hi += pad;
  const x = (v) => ((v - lo) / (hi - lo)) * W;
  return (
    <svg width={W} height={H} style={{ verticalAlign: 'middle' }} role="img"
      aria-label={`range ${q10} to ${q90}, median ${q50}, actual ${actual}`}>
      <line x1="0" x2={W} y1={H / 2} y2={H / 2} stroke="rgba(255,255,255,0.08)" />
      <rect x={x(q10)} y={H / 2 - 4} width={Math.max(2, x(q90) - x(q10))} height="8" rx="3" fill={C_BAND} fillOpacity="0.35" />
      <line x1={x(q50)} x2={x(q50)} y1={H / 2 - 5} y2={H / 2 + 5} stroke={C_BAND} strokeWidth="2" />
      <circle cx={x(actual)} cy={H / 2} r="4" fill="var(--text-white)" stroke="var(--bg-deep)" strokeWidth="2" />
      <title>{`10th–90th pct of sims: ${num(q10)}–${num(q90)} · median ${num(q50)} · actual ${num(actual)}`}</title>
    </svg>
  );
}

function SmallTable({ title, note, head, rows }) {
  return (
    <div style={{ minWidth: 0 }}>
      <div style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text-main)', marginBottom: 2 }}>{title}</div>
      {note && <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginBottom: 4 }}>{note}</div>}
      {rows.length === 0 ? <Empty text="None." /> : (
        <div className="table-container" style={{ overflowX: 'auto', maxHeight: 300, overflowY: 'auto' }}>
          <table style={{ fontSize: '0.75rem', width: '100%' }}>
            <thead><tr style={{ color: 'var(--text-muted)' }}>{head.map(h => <th key={h} style={th}>{h}</th>)}</tr></thead>
            <tbody>{rows.map((cells, i) => (
              <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                {cells.map((c, j) => <td key={j} style={td}>{c}</td>)}
              </tr>))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

export default function PlayerProjectionsEval({ selectedWeek }) {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [showBackups, setShowBackups] = useState(false);
  const [pos, setPos] = useState('ALL');
  const [stat, setStat] = useState('dk_score');
  const [weekFilter, setWeekFilter] = useState('page');
  const [sortBy, setSortBy] = useState('surprise');
  const [refreshing, setRefreshing] = useState(false);
  const [refreshMsg, setRefreshMsg] = useState('');
  const [reloadTick, setReloadTick] = useState(0);
  const minDk = showBackups ? 0 : 5;

  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(async () => {
      if (cancelled) return;
      setLoading(true);
      const d = await ApiService.getPlayerProjectionsEval(minDk);
      if (!cancelled) { setData(d); setLoading(false); }
    });
    return () => { cancelled = true; };
  }, [minDk, reloadTick]);

  const statLabels = data?.stat_labels || {};
  const posStats = data?.pos_stats || {};
  // Stats offered for the chosen position (ALL = DK only -- the one stat every position shares).
  const statOptions = pos === 'ALL' ? ['dk_score'] : (posStats[pos] || ['dk_score']);
  const activeStat = statOptions.includes(stat) ? stat : 'dk_score';

  const rows = useMemo(() => {
    const all = (data?.rows || []).filter(r => r.proj_dk >= minDk
      && (pos === 'ALL' || r.Pos === pos)
      && (weekFilter === 'all' || r.week === Number(selectedWeek))
      && r[`${activeStat}_pit`] != null);
    const surprise = (r) => Math.abs(r[`${activeStat}_pit`] - 0.5);
    const key = { surprise: (r) => -surprise(r), proj: (r) => -r[`${activeStat}_mean`],
      over: (r) => -r[`${activeStat}_pit`], under: (r) => r[`${activeStat}_pit`] }[sortBy];
    return [...all].sort((a, b) => key(a) - key(b));
  }, [data, minDk, pos, weekFilter, selectedWeek, activeStat, sortBy]);

  const onRefresh = async () => {
    setRefreshing(true); setRefreshMsg('');
    const r = await ApiService.refreshPlayerActuals();
    setRefreshing(false);
    setRefreshMsg(r ? `Pulled ${r.rows} stat lines · weeks ${r.weeks.join(', ')} · ${r.games} games` : 'Refresh failed — nflverse unreachable?');
    if (r) setReloadTick(t => t + 1);
  };

  const header = (
    <div style={{ ...cardStyle, display: 'flex', flexWrap: 'wrap', gap: '14px', alignItems: 'center' }}>
      <div style={{ flex: '1 1 360px' }}>
        <h2 style={{ margin: 0, fontSize: '1.05rem' }}>Player Projections</h2>
        <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '2px 0 0 0' }}>
          Our sim's projection vs. the real stat line, and the <b>percentile finish</b>: where the real line landed among that
          player's 10,000 sim runs (94th = beat 94% of our sims). Graded against our own sims for now — prop lines come later.
          QB/RB/WR/TE only.
        </p>
      </div>
      <div style={{ display: 'flex', gap: '8px', alignItems: 'center', flexWrap: 'wrap' }}>
        <label style={{ fontSize: '0.78rem', color: 'var(--text-muted)', display: 'flex', gap: 6, alignItems: 'center' }}>
          <input type="checkbox" checked={showBackups} onChange={e => setShowBackups(e.target.checked)} />
          Include backups (&lt; 5 DK proj)
        </label>
        <button style={btn(false)} onClick={onRefresh} disabled={refreshing}
          title="Re-download nflverse's weekly player stats (do this after games finish)">
          {refreshing ? 'Refreshing…' : '🔄 Refresh actual stats'}
        </button>
      </div>
      {(refreshMsg || data?.actuals_updated_at || (data?.stale_games || []).length > 0) && (
        <div style={{ flexBasis: '100%', fontSize: '0.72rem', color: 'var(--text-muted)' }}>
          {data?.actuals_updated_at && <>Actual stats pulled {new Date(data.actuals_updated_at * 1000).toLocaleString()}. </>}
          {(data?.stale_games || []).length > 0 && (
            <span style={{ color: 'var(--accent-gold)' }}>⚠ {data.stale_games.length} final game(s) have no player stats yet — hit refresh. </span>)}
          {refreshMsg}
        </div>
      )}
    </div>
  );

  if (loading) return <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>{header}
    <div style={cardStyle}><Empty text="Crunching player projections… (the first load after a new sim run takes ~30s)" /></div></div>;
  if (!data) return <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>{header}
    <div style={cardStyle}><Empty text="Couldn't reach /api/eval/player_projections — is the DFS API (port 8002) running?" /></div></div>;

  const s = data.summary || {};
  if (!s.n) return <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>{header}
    <div style={cardStyle}><Empty text="No graded player-games yet — refresh actual stats once games are final." /></div></div>;

  const block = pos === 'ALL' ? s.overall?.dk_score : s.by_pos?.[pos]?.[activeStat];
  const statName = statLabels[activeStat] || activeStat;
  const scopeName = `${pos === 'ALL' ? 'All positions' : pos} · ${statName}`;

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>
      {header}

      {/* ── Controls ── */}
      <div style={{ ...cardStyle, display: 'flex', gap: '10px', alignItems: 'center', flexWrap: 'wrap', padding: '10px 16px' }}>
        <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>Position</span>
        {POSITIONS.map(p => <button key={p} style={btn(pos === p)} onClick={() => setPos(p)}>{p === 'ALL' ? 'All' : p}</button>)}
        <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginLeft: 8 }}>Stat</span>
        <select value={activeStat} onChange={e => setStat(e.target.value)} style={selStyle} disabled={pos === 'ALL'}
          title={pos === 'ALL' ? 'Pick a position to grade individual stats' : ''}>
          {statOptions.map(k => <option key={k} value={k}>{statLabels[k] || k}</option>)}
        </select>
        <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>
          {s.n} player-games · {s.n_players} players{showBackups ? '' : ' · projected ≥ 5 DK'}
        </span>
      </div>

      {/* ── 1. Scoreboard for the chosen position + stat ── */}
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(220px, 1fr))', gap: '12px' }}>
        <Tile title={`Inside our 10–90 range · ${scopeName}`} ours={pct0(block?.cov80)} oursLabel="" note={covNote(block?.cov80, 0.8, block?.n)} />
        <Tile title="Inside our 25–75 range" ours={pct0(block?.cov50)} oursLabel="" note={covNote(block?.cov50, 0.5, block?.n)} />
        <Tile title="Average miss" ours={num(block?.mae)} oursLabel={statName} note="Actual vs. our mean projection" />
        <Tile title="Bias" ours={signed(block?.bias)} oursLabel={statName}
          note="+ = players beat our projection on average; − = fell short" />
        <Tile title="Average percentile finish" ours={ordinal(block?.mean_pit)} oursLabel=""
          note="50th = centered. Above = we under-project, below = we over-project" />
      </div>

      {/* ── 2. Calibration ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Percentile finish — {scopeName}</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          Where each real stat line landed among that player's sim runs. Calibrated projections give flat bars near the dashed line.
          A pile on the right = real lines kept beating our sims (we under-project); on the left = we over-project; tall ends with a low
          middle = our player ranges are too narrow (boom/bust outcomes more common than we think).
        </p>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))', gap: '14px', alignItems: 'start' }}>
          <PitHist counts={block?.pit_hist} color={C_BAND} label={scopeName} />
          {pos === 'ALL' && ['QB', 'RB', 'WR', 'TE'].filter(p => s.by_pos?.[p]).map(p => (
            <PitHist key={p} counts={s.by_pos[p].dk_score.pit_hist} color={C_BAND} label={`${p} · DK pts`} />
          ))}
        </div>
      </div>

      {/* ── 3. Where we miss ── */}
      <div style={cardStyle}>
        <h3 style={{ margin: '0 0 2px 0', fontSize: '0.92rem' }}>Where we miss — DK points</h3>
        <p style={{ fontSize: '0.74rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
          Range coverage and bias by position, by projection size (stars vs. depth), and by week.
        </p>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '16px' }}>
          <SmallTable title="By position" head={['Pos', 'Games', 'In 10–90', 'In 25–75', 'Bias', 'Avg miss']}
            rows={['QB', 'RB', 'WR', 'TE'].filter(p => s.by_pos?.[p]).map(p => {
              const b = s.by_pos[p].dk_score;
              return [p, b.n, pct0(b.cov80), pct0(b.cov50), signed(b.bias), num(b.mae)];
            })} />
          <SmallTable title="By projection size (DK)" head={['Projected', 'Games', 'In 10–90', 'Bias', 'Avg miss']}
            rows={(s.by_bucket || []).map(b => [b.bucket, b.n, pct0(b.cov80), signed(b.bias), num(b.mae)])} />
          <SmallTable title="By week" head={['Week', 'Games', 'In 10–90', 'Bias', 'Avg pct']}
            rows={(s.by_week || []).map(b => [`W${b.week}`, b.n, pct0(b.cov80), signed(b.bias), ordinal(b.mean_pit)])} />
        </div>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '16px', marginTop: 16 }}>
          <SmallTable title="Repeat under-projections" note="2+ games, average DK percentile finish — candidates for a usage/role bump."
            head={['Player', 'Team', 'Pos', 'Games', 'Avg pct', 'Proj → actual']}
            rows={(s.repeat || []).filter(r => r.mean_pit >= 0.6).slice(0, 12).map(r =>
              [r.Player, r.Team, r.Pos, r.games, ordinal(r.mean_pit), `${num(r.avg_proj)} → ${num(r.avg_actual)}`])} />
          <SmallTable title="Repeat over-projections" note="2+ games, consistently below our range — role may be shrinking."
            head={['Player', 'Team', 'Pos', 'Games', 'Avg pct', 'Proj → actual']}
            rows={[...(s.repeat || [])].reverse().filter(r => r.mean_pit <= 0.4).slice(0, 12).map(r =>
              [r.Player, r.Team, r.Pos, r.games, ordinal(r.mean_pit), `${num(r.avg_proj)} → ${num(r.avg_actual)}`])} />
        </div>
      </div>

      {/* ── 4. Player table ── */}
      <div style={cardStyle}>
        <div style={{ display: 'flex', flexWrap: 'wrap', gap: '8px', alignItems: 'center', marginBottom: 8 }}>
          <h3 style={{ margin: 0, fontSize: '0.92rem', flex: '1 1 auto' }}>Players — {scopeName}</h3>
          <select value={sortBy} onChange={e => setSortBy(e.target.value)} style={selStyle}>
            <option value="surprise">Sort: biggest surprise</option>
            <option value="over">Sort: beat us most</option>
            <option value="under">Sort: fell shortest</option>
            <option value="proj">Sort: projection</option>
          </select>
          <button style={btn(weekFilter === 'page')} onClick={() => setWeekFilter('page')}>Week {selectedWeek}</button>
          <button style={btn(weekFilter === 'all')} onClick={() => setWeekFilter('all')}>All weeks</button>
        </div>
        {rows.length === 0 ? <Empty text={`No graded players for week ${selectedWeek} yet.`} /> : (
          <div className="table-container" style={{ overflowX: 'auto', maxHeight: 560, overflowY: 'auto' }}>
            <table style={{ fontSize: '0.76rem', width: '100%' }}>
              <thead><tr style={{ color: 'var(--text-muted)' }}>
                <th style={th}>Player</th><th style={th}>Pos</th><th style={th}>Wk</th>
                <th style={th} title="Mean of the player's sim runs">Proj</th>
                <th style={th} title="10th – 90th percentile of the player's sim runs">Range 10–90</th>
                <th style={th} title="Band = 10-90 range, tick = median, dot = actual">Where it landed</th>
                <th style={th}>Actual</th>
                <th style={th} title="Share of the player's sim runs the real line beat">Percentile</th>
                <th style={th}>Miss</th>
              </tr></thead>
              <tbody>
                {rows.map(r => (
                  <tr key={`${r.game_id}-${r.Team}-${r.Player}`} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                    <td style={{ ...td, fontWeight: 600 }}>{r.Player} <span style={{ color: 'var(--text-muted)', fontWeight: 400 }}>{r.Team}</span></td>
                    <td style={td}>{r.Pos}</td><td style={td}>{r.week}</td>
                    <td style={td}>{num(r[`${activeStat}_mean`])}</td>
                    <td style={{ ...td, color: 'var(--text-muted)' }}>{num(r[`${activeStat}_q10`])}–{num(r[`${activeStat}_q90`])}</td>
                    <td style={td}><RangeBar q10={r[`${activeStat}_q10`]} q50={r[`${activeStat}_q50`]} q90={r[`${activeStat}_q90`]} actual={r[`${activeStat}_actual`]} /></td>
                    <td style={{ ...td, color: 'var(--text-white)', fontWeight: 600 }}>{num(r[`${activeStat}_actual`])}</td>
                    <td style={td}>{ordinal(r[`${activeStat}_pit`])}</td>
                    <td style={{ ...td, color: 'var(--text-main)' }}>{signed(r[`${activeStat}_miss`])}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      {/* ── 5. Roster-status misses ── */}
      <div style={{ ...cardStyle, display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: '16px' }}>
        <SmallTable title="Projected, but no stat line"
          note="Inactive, hurt early, or active with zero touches/targets — roster-status misses, not graded as zeros."
          head={['Wk', 'Player', 'Team', 'Pos', 'Proj DK']}
          rows={(data.no_stat_line || []).map(r => [r.week, r.Player, r.Team, r.Pos, num(r.proj_dk)])} />
        <SmallTable title="Real production we never projected"
          note="≥ 5 DK real lines with no matching sim player (backup who came in, or missing from our roster)."
          head={['Wk', 'Player', 'Team', 'Pos', 'DK']}
          rows={(data.unprojected || []).map(r => [r.week, r.player_name, r.team, r.position, num(r.dk_score)])} />
      </div>
      <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: -8 }}>
        Actual DK points use the same scoring as our sim (−1 per fumble, lost or not; no 2-pt conversions or return TDs), so
        they can differ slightly from DraftKings' official number.
      </div>
    </div>
  );
}
