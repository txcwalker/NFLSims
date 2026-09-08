import { useState, useEffect, useMemo } from 'react';
import { ApiService } from '../api';
import { Trophy } from 'lucide-react';

const TEAM_DIVISIONS = {
  BUF: ['AFC', 'East'], MIA: ['AFC', 'East'], NE: ['AFC', 'East'], NYJ: ['AFC', 'East'],
  BAL: ['AFC', 'North'], CIN: ['AFC', 'North'], CLE: ['AFC', 'North'], PIT: ['AFC', 'North'],
  HOU: ['AFC', 'South'], IND: ['AFC', 'South'], JAX: ['AFC', 'South'], TEN: ['AFC', 'South'],
  DEN: ['AFC', 'West'], KC: ['AFC', 'West'], LV: ['AFC', 'West'], LAC: ['AFC', 'West'],
  DAL: ['NFC', 'East'], NYG: ['NFC', 'East'], PHI: ['NFC', 'East'], WAS: ['NFC', 'East'],
  CHI: ['NFC', 'North'], DET: ['NFC', 'North'], GB: ['NFC', 'North'], MIN: ['NFC', 'North'],
  ATL: ['NFC', 'South'], CAR: ['NFC', 'South'], NO: ['NFC', 'South'], TB: ['NFC', 'South'],
  ARI: ['NFC', 'West'], LA: ['NFC', 'West'], SF: ['NFC', 'West'], SEA: ['NFC', 'West'],
};

const SUB_TABS = ['Standings', 'Leaders', 'Team Stats', 'Matchups', 'Teams'];

// Column-sum of a usage table. Volume stats are additive across players, so a
// plain sum of the per-player medians is a fine at-a-glance team total (the
// median-vs-mean gap on these counting stats is <1%). Ratio stats (Cmp %) are
// recomputed from the summed numerator/denominator, never summed directly.
const sumKey = (rows, k) => (rows || []).reduce((s, r) => s + (Number(r[k]) || 0), 0);
const fmt1 = (n) => (Math.round(n * 10) / 10).toFixed(1);

// --- Betting-line helpers (all lines rendered home-team-relative) ----------
// A point spread stated FROM THE HOME TEAM'S SIDE: negative = home favored
// ("CAR -2.5"), positive = home underdog ("CAR +5.7"), 0 = pick'em.
const fmtSpread = (n) => {
  if (n == null || !isFinite(n)) return null;
  const r = Math.round(n * 10) / 10;
  if (r === 0) return 'PK';
  return r > 0 ? `+${r.toFixed(1)}` : r.toFixed(1);
};
// American moneyline implied by a win probability p (0..1). Favorite (p >= .5)
// comes back negative, underdog positive, rounded to the nearest 5 the way a
// book posts it. null for degenerate probabilities.
const probToAmericanML = (p) => {
  if (p == null || !isFinite(p) || p <= 0 || p >= 1) return null;
  const ml = p >= 0.5 ? (-100 * p) / (1 - p) : (100 * (1 - p)) / p;
  return Math.round(ml / 5) * 5;
};
const fmtML = (ml) => {
  if (ml == null || !isFinite(ml)) return '—';
  const v = Math.round(ml);
  return v > 0 ? `+${v}` : `${v}`;
};

function useSortableData(rows, initialKey, initialDir = 'desc') {
  const [sortKey, setSortKey] = useState(initialKey);
  const [sortDir, setSortDir] = useState(initialDir);

  const sorted = useMemo(() => {
    if (!rows) return [];
    const copy = [...rows];
    copy.sort((a, b) => {
      const av = a[sortKey], bv = b[sortKey];
      if (typeof av === 'string') return sortDir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
      return sortDir === 'asc' ? (av ?? 0) - (bv ?? 0) : (bv ?? 0) - (av ?? 0);
    });
    return copy;
  }, [rows, sortKey, sortDir]);

  const toggleSort = (key) => {
    if (key === sortKey) setSortDir(d => (d === 'asc' ? 'desc' : 'asc'));
    else { setSortKey(key); setSortDir('desc'); }
  };

  return { sorted, sortKey, sortDir, toggleSort };
}

function SortableTh({ label, sortKeyName, activeKey, dir, onClick }) {
  return (
    <th style={{ cursor: 'pointer', userSelect: 'none' }} onClick={() => onClick(sortKeyName)}>
      {label}{activeKey === sortKeyName ? (dir === 'asc' ? ' ▲' : ' ▼') : ''}
    </th>
  );
}

// ---------------------------------------------------------------------------
// STANDINGS
// ---------------------------------------------------------------------------
function StandingsTab({ standings }) {
  const divisions = ['East', 'North', 'South', 'West'];
  const teamsByDiv = (conf, div) =>
    standings.filter(t => t.Conference === conf && t.Division === div).sort((a, b) => b.Wins_Expected - a.Wins_Expected);

  const renderConference = (conf) => (
    <div key={conf} style={{ display: 'flex', flexDirection: 'column', gap: '20px', marginBottom: '32px' }}>
      <h2 style={{ fontSize: '18px', borderBottom: '2px solid var(--border-color)', paddingBottom: '8px', color: 'var(--accent-cyan)', textTransform: 'uppercase', letterSpacing: '0.05em' }}>
        {conf} Conference
      </h2>
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(380px, 1fr))', gap: '16px' }}>
        {divisions.map(div => {
          const teams = teamsByDiv(conf, div);
          return (
            <div key={div} className="panel" style={{ padding: 0 }}>
              <div style={{ padding: '10px 14px', borderBottom: '1px solid var(--border-color)', fontWeight: 700, fontSize: '13px', textTransform: 'uppercase', color: 'var(--text-secondary)' }}>
                {div}
              </div>
              <div style={{ overflowX: 'auto' }}>
                <table className="tactical-table">
                  <thead>
                    <tr><th style={{ padding: '6px 12px' }}>Team</th><th style={{ padding: '6px 12px' }} title="Expected wins/losses: sum of each game's raw simulated win probability across the schedule">W-L</th><th style={{ padding: '6px 12px' }}>PF/PA</th><th style={{ padding: '6px 12px' }}>Div %</th><th style={{ padding: '6px 12px' }}>Playoffs</th><th style={{ padding: '6px 12px' }}>SB</th></tr>
                  </thead>
                  <tbody>
                    {teams.map((t, idx) => (
                      <tr key={t.Team}>
                        <td style={{ padding: '8px 12px', fontWeight: idx === 0 ? 700 : 400, color: idx === 0 ? 'var(--accent-cyan)' : 'var(--text-primary)' }}>
                          {t.Team} {idx === 0 && <Trophy size={10} style={{ color: 'var(--accent-orange)', marginLeft: '2px' }} />}
                        </td>
                        <td style={{ padding: '8px 12px' }}>{t.Wins_Expected.toFixed(1)}-{t.Losses_Expected.toFixed(1)}</td>
                        <td style={{ padding: '8px 12px', color: 'var(--text-secondary)' }}>{t.PF_Avg.toFixed(0)}/{t.PA_Avg.toFixed(0)}</td>
                        <td style={{ padding: '8px 12px', color: 'var(--text-secondary)' }}>{t['Division_%'].toFixed(0)}%</td>
                        <td style={{ padding: '8px 12px' }}>
                          <span className={t['Playoffs_%'] > 50 ? 'badge badge-green' : 'badge badge-cyan'} style={{ fontSize: '10px', padding: '1px 6px' }}>{t['Playoffs_%'].toFixed(0)}%</span>
                        </td>
                        <td style={{ padding: '8px 12px', color: 'var(--accent-orange)', fontWeight: 600 }}>{t['Champion_%'].toFixed(1)}%</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );

  return <div>{renderConference('AFC')}{renderConference('NFC')}</div>;
}

// ---------------------------------------------------------------------------
// LEADERS
// ---------------------------------------------------------------------------
const QB_COLS = [
  ['Player', 'Player'], ['Team', 'Team'], ['pAtt_avg', 'Att'], ['pCmp_avg', 'Cmp'], ['cmp_pct', 'Cmp %'], ['pYds_avg', 'Pass Yds'],
  ['pTD_avg', 'Pass TD'], ['rYds_avg', 'Rush Yds'], ['rTD_avg', 'Rush TD'], ['totalTD', 'Total TD'], ['fumbles_avg', 'Fum'],
  ['sacks_taken_avg', 'Sacks'], ['int_avg', 'INT'], ['air_yards_avg', 'Air Yds'], ['adot', 'ADOT'],
  ['std_score_avg', 'Std'],
];
const WR_TE_COLS = [
  ['Player', 'Player'], ['Team', 'Team'], ['Slot', 'Slot'], ['rAtt_avg', 'Rush Att'], ['rYds_avg', 'Rush Yds'],
  ['rTD_avg', 'Rush TD'], ['targets_avg', 'Targets'], ['rec_avg', 'Rec'], ['recYds_avg', 'Rec Yds'],
  ['recTD_avg', 'Rec TD'], ['totalTD', 'Total TD'], ['fumbles_avg', 'Fum'], ['air_yards_avg', 'Air Yds'], ['adot', 'ADOT'],
  ['std_score_avg', 'Std'],
];
const RB_COLS = WR_TE_COLS.filter(([key]) => key !== 'air_yards_avg' && key !== 'adot');

function LeadersTable({ rows, cols, defaultSort }) {
  const { sorted, sortKey, sortDir, toggleSort } = useSortableData(rows, defaultSort);
  if (!rows || rows.length === 0) return <div style={{ padding: '20px', color: 'var(--text-muted)' }}>No players in this category.</div>;
  return (
    <div style={{ overflowX: 'auto' }}>
      <table className="tactical-table">
        <thead><tr>{cols.map(([key, label]) => <SortableTh key={key} label={label} sortKeyName={key} activeKey={sortKey} dir={sortDir} onClick={toggleSort} />)}</tr></thead>
        <tbody>
          {sorted.map((r, i) => (
            <tr key={r.Player + r.Team + i}>
              {cols.map(([key]) => (
                <td key={key} style={{ fontWeight: key === 'Player' ? 600 : 400 }}>
                  {typeof r[key] === 'number' ? r[key].toFixed(1) : r[key]}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function LeadersTab({ leaders }) {
  const [pos, setPos] = useState('qb');
  const [scope, setScope] = useState('overall');
  const positions = [['qb', 'QB'], ['rb', 'RB'], ['wr', 'WR'], ['te', 'TE']];
  const rawData = leaders?.[scope === 'overall' ? 'overall' : 'rookies']?.[pos] || [];
  const data = useMemo(() => rawData.map(r => ({
    ...r,
    totalTD: pos === 'qb' ? (r.pTD_avg || 0) + (r.rTD_avg || 0) : (r.rTD_avg || 0) + (r.recTD_avg || 0),
  })), [rawData, pos]);
  const cols = pos === 'qb' ? QB_COLS : (pos === 'rb' ? RB_COLS : WR_TE_COLS);
  const defaultSort = pos === 'qb' ? 'pYds_avg' : (pos === 'rb' ? 'rYds_avg' : 'recYds_avg');

  return (
    <div>
      <div style={{ display: 'flex', gap: '12px', marginBottom: '16px', flexWrap: 'wrap' }}>
        <div className="tabs-container" style={{ marginBottom: 0 }}>
          {positions.map(([id, label]) => (
            <button key={id} className={`tab-btn ${pos === id ? 'active' : ''}`} onClick={() => setPos(id)}>{label}</button>
          ))}
        </div>
        <div className="tabs-container" style={{ marginBottom: 0 }}>
          <button className={`tab-btn ${scope === 'overall' ? 'active' : ''}`} onClick={() => setScope('overall')}>Overall</button>
          <button className={`tab-btn ${scope === 'rookies' ? 'active' : ''}`} onClick={() => setScope('rookies')}>Rookies</button>
        </div>
      </div>
      <LeadersTable rows={data} cols={cols} defaultSort={defaultSort} />
    </div>
  );
}

// ---------------------------------------------------------------------------
// TEAM STATS
// ---------------------------------------------------------------------------
// Every team plays exactly 17 regular-season games in the 2026 schedule.
const GAMES_PER_TEAM = 17;

const TEAM_STATS_COLS = [
  ['Team', 'Team'], ['PF_avg', 'PF'], ['PA_avg', 'PA'], ['PointDiff_avg', 'Diff'],
  ['pYds_avg', 'Pass Yds'], ['rYds_avg', 'Rush Yds'], ['TotalYds_avg', 'Total Yds'],
  ['PF_per_game', 'PF/G'], ['PA_per_game', 'PA/G'], ['pYds_per_game', 'Pass Yds/G'],
  ['rYds_per_game', 'Rush Yds/G'], ['TotalYds_per_game', 'Total Yds/G'],
];

function TeamStatsTab({ teamStats }) {
  const withPerGame = useMemo(() => teamStats.map(r => ({
    ...r,
    PF_per_game: r.PF_avg / GAMES_PER_TEAM,
    PA_per_game: r.PA_avg / GAMES_PER_TEAM,
    pYds_per_game: r.pYds_avg / GAMES_PER_TEAM,
    rYds_per_game: r.rYds_avg / GAMES_PER_TEAM,
    TotalYds_per_game: r.TotalYds_avg / GAMES_PER_TEAM,
  })), [teamStats]);
  const { sorted, sortKey, sortDir, toggleSort } = useSortableData(withPerGame, 'PointDiff_avg');
  return (
    <div style={{ overflowX: 'auto' }}>
      <table className="tactical-table">
        <thead><tr>{TEAM_STATS_COLS.map(([key, label]) => <SortableTh key={key} label={label} sortKeyName={key} activeKey={sortKey} dir={sortDir} onClick={toggleSort} />)}</tr></thead>
        <tbody>
          {sorted.map(r => (
            <tr key={r.Team}>
              {TEAM_STATS_COLS.map(([key]) => (
                <td key={key} style={{ fontWeight: key === 'Team' ? 700 : 400 }}>
                  {typeof r[key] === 'number' ? r[key].toFixed(1) : r[key]}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

// ---------------------------------------------------------------------------
// MATCHUPS
// ---------------------------------------------------------------------------
function MatchupsTab({ matchups }) {
  const weeks = Object.keys(matchups?.weeks || {}).sort((a, b) => Number(a) - Number(b));
  const [week, setWeek] = useState(weeks[0] || '1');
  useEffect(() => { if (weeks.length && !weeks.includes(week)) setWeek(weeks[0]); }, [weeks]);
  const games = matchups?.weeks?.[week] || [];

  return (
    <div>
      <div className="form-group" style={{ maxWidth: '200px', marginBottom: '20px' }}>
        <label className="form-label">Week</label>
        <select className="form-select" value={week} onChange={e => setWeek(e.target.value)}>
          {weeks.map(w => <option key={w} value={w}>Week {w}</option>)}
        </select>
      </div>
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: '10px' }}>
        {games.map(g => {
          const awayFav = g.away_win_pct >= g.home_win_pct;
          return (
            <div key={g.game_id} className="panel" style={{ padding: '14px 16px' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <div style={{ fontWeight: awayFav ? 700 : 400, color: awayFav ? 'var(--accent-cyan)' : 'var(--text-primary)' }}>
                  {g.away_team} <span style={{ fontSize: '12px', color: 'var(--text-muted)' }}>{g.avg_away_score.toFixed(1)}</span>
                </div>
                <div style={{ fontSize: '11px', color: 'var(--text-muted)' }}>@</div>
                <div style={{ fontWeight: !awayFav ? 700 : 400, color: !awayFav ? 'var(--accent-cyan)' : 'var(--text-primary)', textAlign: 'right' }}>
                  {g.home_team} <span style={{ fontSize: '12px', color: 'var(--text-muted)' }}>{g.avg_home_score.toFixed(1)}</span>
                </div>
              </div>
              <div style={{ display: 'flex', height: '6px', borderRadius: '3px', overflow: 'hidden', marginTop: '10px', backgroundColor: 'var(--bg-primary)' }}>
                <div style={{ width: `${g.away_win_pct}%`, backgroundColor: 'var(--accent-cyan)' }} />
                <div style={{ width: `${g.home_win_pct}%`, backgroundColor: 'var(--accent-orange)' }} />
              </div>
              <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', color: 'var(--text-secondary)', marginTop: '4px' }}>
                <span>{g.away_win_pct.toFixed(1)}%</span>
                <span>{g.home_win_pct.toFixed(1)}%</span>
              </div>
              {(() => {
                // Everything below is stated from the HOME team's side so the
                // sim and the market can be read against each other directly.
                // nflverse spread_line is +ve when the home team is favored
                // (it's the home margin), so the home-side line is its neg.
                const vegasSpread = g.spread_line != null ? fmtSpread(-g.spread_line) : null;
                const simSpread = fmtSpread(g.avg_away_score - g.avg_home_score);
                const simTotal = (g.avg_away_score + g.avg_home_score).toFixed(1);
                const simMlHome = probToAmericanML(g.home_win_pct / 100);
                const simMlAway = probToAmericanML(g.away_win_pct / 100);
                const hasVegasMl = g.away_moneyline != null && g.home_moneyline != null;
                const row = { display: 'grid', gridTemplateColumns: 'auto 1fr', columnGap: '8px', alignItems: 'baseline' };
                const tag = { fontWeight: 700, color: 'var(--text-secondary)', letterSpacing: '0.03em' };
                return (
                  <div style={{ fontSize: '11px', color: 'var(--text-muted)', marginTop: '8px', paddingTop: '8px', borderTop: '1px solid var(--border-color)', display: 'grid', rowGap: '3px' }}>
                    <div style={row}>
                      <span style={tag}>VEGAS</span>
                      <span>
                        {vegasSpread ? `${g.home_team} ${vegasSpread}` : 'Line TBD'}
                        {' · '}{g.total_line != null ? `O/U ${g.total_line}` : 'O/U TBD'}
                        {hasVegasMl ? ` · ML ${g.away_team} ${fmtML(g.away_moneyline)} / ${g.home_team} ${fmtML(g.home_moneyline)}` : ''}
                      </span>
                    </div>
                    <div style={row}>
                      <span style={tag}>SIM</span>
                      <span>
                        {simSpread ? `${g.home_team} ${simSpread}` : '—'}
                        {' · '}{`O/U ${simTotal}`}
                        {` · ML ${g.away_team} ${fmtML(simMlAway)} / ${g.home_team} ${fmtML(simMlHome)}`}
                      </span>
                    </div>
                  </div>
                );
              })()}
            </div>
          );
        })}
      </div>
    </div>
  );
}

// ---------------------------------------------------------------------------
// TEAMS
// ---------------------------------------------------------------------------
function TeamsTab({ teams }) {
  const teamIds = Object.keys(teams || {}).sort();
  const [team, setTeam] = useState(teamIds[0] || '');
  useEffect(() => { if (teamIds.length && !teamIds.includes(team)) setTeam(teamIds[0]); }, [teamIds]);
  const data = teams?.[team];

  if (!data) return <div style={{ padding: '20px', color: 'var(--text-muted)' }}>No team data loaded.</div>;

  return (
    <div>
      <div className="form-group" style={{ maxWidth: '200px', marginBottom: '20px' }}>
        <label className="form-label">Team</label>
        <select className="form-select" value={team} onChange={e => setTeam(e.target.value)}>
          {teamIds.map(t => <option key={t} value={t}>{t}</option>)}
        </select>
      </div>

      <div className="panel" style={{ marginBottom: '20px' }}>
        <div className="panel-header"><span className="panel-title">QB Stats (median)</span></div>
        <div style={{ overflowX: 'auto' }}>
          <table className="tactical-table">
            <thead><tr><th>Player</th><th>Slot</th><th>Att</th><th>Cmp</th><th>Cmp %</th><th>Pass Yds</th><th>Pass TD</th><th>INT</th><th>Rush Att</th><th>Rush Yds</th><th>Rush TD</th><th>Std</th></tr></thead>
            <tbody>
              {(data.usage_qb || []).map((p, i) => (
                <tr key={i}>
                  <td style={{ fontWeight: 600 }}>{p.Player}</td><td>{p.Slot}</td><td>{p.pAtt_p50}</td><td>{p.pCmp_p50}</td><td>{p.cmp_pct_p50}</td>
                  <td>{p.pYds_p50}</td><td>{p.pTD_p50}</td><td>{p.int_p50}</td><td>{p.rAtt_p50}</td><td>{p.rYds_p50}</td><td>{p.rTD_p50}</td><td>{p.std_score_p50}</td>
                </tr>
              ))}
              {(data.usage_qb || []).length > 0 && (() => {
                const r = data.usage_qb, att = sumKey(r, 'pAtt_p50'), cmp = sumKey(r, 'pCmp_p50');
                return (
                  <tr style={{ borderTop: '2px solid var(--border)', fontWeight: 700 }}>
                    <td>Total</td><td>—</td><td>{fmt1(att)}</td><td>{fmt1(cmp)}</td><td>{att > 0 ? fmt1(cmp / att * 100) : '—'}</td>
                    <td>{fmt1(sumKey(r, 'pYds_p50'))}</td><td>{fmt1(sumKey(r, 'pTD_p50'))}</td><td>{fmt1(sumKey(r, 'int_p50'))}</td>
                    <td>{fmt1(sumKey(r, 'rAtt_p50'))}</td><td>{fmt1(sumKey(r, 'rYds_p50'))}</td><td>{fmt1(sumKey(r, 'rTD_p50'))}</td><td>{fmt1(sumKey(r, 'std_score_p50'))}</td>
                  </tr>
                );
              })()}
            </tbody>
          </table>
        </div>
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: '16px', marginBottom: '20px' }}>
        <div className="panel">
          <div className="panel-header"><span className="panel-title">Rushing Usage (median)</span></div>
          <table className="tactical-table">
            <thead><tr><th>Player</th><th>Pos</th><th>Att</th><th>Yds</th><th>TD</th><th>Std</th></tr></thead>
            <tbody>
              {data.usage_rushing.map((p, i) => (
                <tr key={i}><td style={{ fontWeight: 600 }}>{p.Player}</td><td>{p.Pos}</td><td>{p.rAtt_p50}</td><td>{p.rYds_p50}</td><td>{p.rTD_p50}</td><td>{p.std_score_p50}</td></tr>
              ))}
              {data.usage_rushing.length > 0 && (
                <tr style={{ borderTop: '2px solid var(--border)', fontWeight: 700 }}>
                  <td>Total</td><td>—</td><td>{fmt1(sumKey(data.usage_rushing, 'rAtt_p50'))}</td><td>{fmt1(sumKey(data.usage_rushing, 'rYds_p50'))}</td>
                  <td>{fmt1(sumKey(data.usage_rushing, 'rTD_p50'))}</td><td>{fmt1(sumKey(data.usage_rushing, 'std_score_p50'))}</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
        <div className="panel">
          <div className="panel-header"><span className="panel-title">Target Usage (median)</span></div>
          <table className="tactical-table">
            <thead><tr><th>Player</th><th>Pos</th><th>Tgt</th><th>Rec</th><th>Yds</th><th>TD</th><th>Std</th></tr></thead>
            <tbody>
              {data.usage_targets.map((p, i) => (
                <tr key={i}><td style={{ fontWeight: 600 }}>{p.Player}</td><td>{p.Pos}</td><td>{p.targets_p50}</td><td>{p.rec_p50}</td><td>{p.recYds_p50}</td><td>{p.recTD_p50}</td><td>{p.std_score_p50}</td></tr>
              ))}
              {data.usage_targets.length > 0 && (
                <tr style={{ borderTop: '2px solid var(--border)', fontWeight: 700 }}>
                  <td>Total</td><td>—</td><td>{fmt1(sumKey(data.usage_targets, 'targets_p50'))}</td><td>{fmt1(sumKey(data.usage_targets, 'rec_p50'))}</td>
                  <td>{fmt1(sumKey(data.usage_targets, 'recYds_p50'))}</td><td>{fmt1(sumKey(data.usage_targets, 'recTD_p50'))}</td><td>{fmt1(sumKey(data.usage_targets, 'std_score_p50'))}</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>

      <div className="panel">
        <div className="panel-header"><span className="panel-title">{team} Matchups</span></div>
        <table className="tactical-table">
          <thead><tr><th>Week</th><th>Matchup</th><th>Win %</th><th>Opp Win %</th><th>Avg Score</th><th>Opp Avg</th></tr></thead>
          <tbody>
            {data.matchups.map((m, i) => (
              <tr key={i}>
                <td>{m.week ?? '?'}</td>
                <td>{m.is_away ? `${team} @ ${m.opponent}` : `${m.opponent} @ ${team}`}</td>
                <td style={{ fontWeight: 700, color: m.team_win_pct >= m.opp_win_pct ? 'var(--accent-cyan)' : 'var(--text-primary)' }}>{m.team_win_pct}%</td>
                <td>{m.opp_win_pct}%</td>
                <td>{m.team_avg_score}</td>
                <td>{m.opp_avg_score}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

// ---------------------------------------------------------------------------
// MAIN PAGE
// ---------------------------------------------------------------------------
function Season2026() {
  const [subTab, setSubTab] = useState('Standings');
  const [loading, setLoading] = useState(true);
  const [standings, setStandings] = useState([]);
  const [teamStats, setTeamStats] = useState([]);
  const [leaders, setLeaders] = useState(null);
  const [matchups, setMatchups] = useState(null);
  const [teams, setTeams] = useState(null);

  useEffect(() => {
    async function loadAll() {
      const [s, ts, l, m, t] = await Promise.all([
        ApiService.getSeason2026Standings(),
        ApiService.getSeason2026TeamStats(),
        ApiService.getSeason2026Leaders(),
        ApiService.getSeason2026Matchups(),
        ApiService.getSeason2026Teams(),
      ]);
      setStandings((s || []).map(t => ({ ...t, Conference: t.Conference || TEAM_DIVISIONS[t.Team]?.[0], Division: t.Division || TEAM_DIVISIONS[t.Team]?.[1] })));
      setTeamStats(ts || []);
      setLeaders(l || { overall: {}, rookies: {} });
      setMatchups(m || { weeks: {} });
      setTeams(t || {});
      setLoading(false);
    }
    loadAll();
  }, []);

  return (
    <div>
      <div style={{ marginBottom: '20px' }}>
        <h1 style={{ fontSize: '24px' }}>2026 Season</h1>
        <p style={{ color: 'var(--text-secondary)', fontSize: '13px' }}>
          Full simulated season -- 272 real matchups x 1,000 iterations, 100-season Monte Carlo for standings and playoff odds.
        </p>
      </div>

      <div className="tabs-container">
        {SUB_TABS.map(t => (
          <button key={t} className={`tab-btn ${subTab === t ? 'active' : ''}`} onClick={() => setSubTab(t)}>{t}</button>
        ))}
      </div>

      {loading ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>LOADING 2026 SEASON DATA...</div>
      ) : (
        <>
          {subTab === 'Standings' && <StandingsTab standings={standings} />}
          {subTab === 'Leaders' && <LeadersTab leaders={leaders} />}
          {subTab === 'Team Stats' && <TeamStatsTab teamStats={teamStats} />}
          {subTab === 'Matchups' && <MatchupsTab matchups={matchups} />}
          {subTab === 'Teams' && <TeamsTab teams={teams} />}
        </>
      )}
    </div>
  );
}

export default Season2026;
