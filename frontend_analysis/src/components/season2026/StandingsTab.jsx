import { Trophy } from 'lucide-react';

const DIVISIONS = ['East', 'North', 'South', 'West'];

// showProjections=false (Current Season) drops the Playoffs/SB columns,
// which don't exist for real, actual standings -- those are projections
// and live on the Rest of Season page instead.
function StandingsTab({ standings, showProjections = true }) {
  const teamsByDiv = (conf, div) =>
    standings.filter(t => t.Conference === conf && t.Division === div).sort((a, b) => b.Wins_Expected - a.Wins_Expected);

  const renderConference = (conf) => (
    <div key={conf} style={{ display: 'flex', flexDirection: 'column', gap: '20px', marginBottom: '32px' }}>
      <h2 style={{ fontSize: '18px', borderBottom: '2px solid var(--border-color)', paddingBottom: '8px', color: 'var(--accent-cyan)', textTransform: 'uppercase', letterSpacing: '0.05em' }}>
        {conf} Conference
      </h2>
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(380px, 1fr))', gap: '16px' }}>
        {DIVISIONS.map(div => {
          const teams = teamsByDiv(conf, div);
          return (
            <div key={div} className="panel" style={{ padding: 0 }}>
              <div style={{ padding: '10px 14px', borderBottom: '1px solid var(--border-color)', fontWeight: 700, fontSize: '13px', textTransform: 'uppercase', color: 'var(--text-secondary)' }}>
                {div}
              </div>
              <div style={{ overflowX: 'auto' }}>
                <table className="tactical-table">
                  <thead>
                    <tr>
                      <th style={{ padding: '6px 12px' }}>Team</th>
                      <th style={{ padding: '6px 12px' }} title="Expected wins/losses: sum of each game's raw simulated win probability across the schedule">W-L</th>
                      <th style={{ padding: '6px 12px' }}>PF/PA</th>
                      {showProjections && <th style={{ padding: '6px 12px' }}>Div %</th>}
                      {showProjections && <th style={{ padding: '6px 12px' }}>Playoffs</th>}
                      {showProjections && <th style={{ padding: '6px 12px' }}>SB</th>}
                    </tr>
                  </thead>
                  <tbody>
                    {teams.map((t, idx) => (
                      <tr key={t.Team}>
                        <td style={{ padding: '8px 12px', fontWeight: idx === 0 ? 700 : 400, color: idx === 0 ? 'var(--accent-cyan)' : 'var(--text-primary)' }}>
                          {t.Team} {idx === 0 && <Trophy size={10} style={{ color: 'var(--accent-orange)', marginLeft: '2px' }} />}
                        </td>
                        <td style={{ padding: '8px 12px' }}>{t.Wins_Expected.toFixed(1)}-{t.Losses_Expected.toFixed(1)}</td>
                        <td style={{ padding: '8px 12px', color: 'var(--text-secondary)' }}>{t.PF_Avg.toFixed(0)}/{t.PA_Avg.toFixed(0)}</td>
                        {showProjections && (
                          <td style={{ padding: '8px 12px', color: 'var(--text-secondary)' }}>{t['Division_%'].toFixed(0)}%</td>
                        )}
                        {showProjections && (
                          <td style={{ padding: '8px 12px' }}>
                            <span className={t['Playoffs_%'] > 50 ? 'badge badge-green' : 'badge badge-cyan'} style={{ fontSize: '10px', padding: '1px 6px' }}>{t['Playoffs_%'].toFixed(0)}%</span>
                          </td>
                        )}
                        {showProjections && (
                          <td style={{ padding: '8px 12px', color: 'var(--accent-orange)', fontWeight: 600 }}>{t['Champion_%'].toFixed(1)}%</td>
                        )}
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

export default StandingsTab;
