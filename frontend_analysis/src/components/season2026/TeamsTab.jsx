import { useState, useEffect } from 'react';
import { sumKey, fmt1 } from './shared';

// showFullSchedule=false (Current Season) hides the projected multi-week
// matchup table -- there's no projected schedule on that page, just
// whatever single current-week matchup the page itself renders separately.
function TeamsTab({ teams, showFullSchedule = true }) {
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

      {showFullSchedule && (
        <div className="panel">
          <div className="panel-header"><span className="panel-title">{team} Matchups</span></div>
          <table className="tactical-table">
            <thead><tr><th>Week</th><th>Matchup</th><th>Win %</th><th>Opp Win %</th><th>Avg Score</th><th>Opp Avg</th></tr></thead>
            <tbody>
              {(data.matchups || []).map((m, i) => (
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
      )}
    </div>
  );
}

export default TeamsTab;
