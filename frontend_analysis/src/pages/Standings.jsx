import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import { Award, Trophy } from 'lucide-react';

function Standings() {
  const [standings, setStandings] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function loadStandings() {
      try {
        const fullList = await ApiService.getFullStandings();
        setStandings(fullList || []);
      } catch (err) {
        console.error('Failed to load full standings', err);
      } finally {
        setLoading(false);
      }
    }
    loadStandings();
  }, []);

  // Helper to group teams by Conference and Division
  const getTeamsByDiv = (conf, div) => {
    return standings
      .filter(t => t.conference === conf && t.division === div)
      .sort((a, b) => b.wins - a.wins);
  };

  const divisions = ['East', 'North', 'South', 'West'];

  const renderConference = (confName) => {
    return (
      <div style={{ display: 'flex', flexDirection: 'column', gap: '24px' }}>
        <h2 style={{ fontSize: '20px', borderBottom: '2px solid var(--border-color)', paddingBottom: '8px', color: 'var(--accent-cyan)', textTransform: 'uppercase', letterSpacing: '0.05em' }}>
          {confName} Conference
        </h2>
        
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))', gap: '20px' }}>
          {divisions.map(div => {
            const teams = getTeamsByDiv(confName, div);
            return (
              <div key={div} className="panel" style={{ padding: '0' }}>
                <div style={{ padding: '12px 16px', borderBottom: '1px solid var(--border-color)', backgroundColor: 'rgba(255,255,255,0.02)', fontWeight: '700', fontSize: '14px', textTransform: 'uppercase', color: 'var(--text-secondary)' }}>
                  {div} Division
                </div>
                
                <table className="tactical-table">
                  <thead>
                    <tr>
                      <th style={{ padding: '8px 12px' }}>Team</th>
                      <th style={{ padding: '8px 12px' }}>W-L</th>
                      <th style={{ padding: '8px 12px' }}>Playoffs</th>
                      <th style={{ padding: '8px 12px' }}>SB</th>
                    </tr>
                  </thead>
                  <tbody>
                    {teams.map((t, idx) => {
                      const isLeader = idx === 0;
                      const hasGlow = t.playoff_pct > 75.0;
                      return (
                        <tr 
                          key={t.team} 
                          style={hasGlow ? { backgroundColor: 'rgba(0, 242, 254, 0.02)' } : {}}
                        >
                          <td style={{ 
                            padding: '10px 12px', 
                            fontWeight: isLeader ? '700' : '400',
                            color: isLeader ? 'var(--accent-cyan)' : 'var(--text-primary)',
                            display: 'flex',
                            alignItems: 'center',
                            gap: '4px'
                          }}>
                            {t.team} {isLeader && <Trophy size={10} style={{ color: 'var(--accent-orange)' }} />}
                          </td>
                          <td style={{ padding: '10px 12px' }}>{t.wins.toFixed(1)}-{t.losses.toFixed(1)}</td>
                          <td style={{ padding: '10px 12px' }}>
                            <span className={t.playoff_pct > 50.0 ? 'badge badge-green' : 'badge badge-outline'} style={{ fontSize: '10px', padding: '1px 4px' }}>
                              {t.playoff_pct}%
                            </span>
                          </td>
                          <td style={{ padding: '10px 12px', color: 'var(--accent-orange)', fontWeight: '600' }}>{t.super_bowl_pct}%</td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            );
          })}
        </div>
      </div>
    );
  };

  return (
    <div>
      <div style={{ marginBottom: '24px' }}>
        <h1 style={{ fontSize: '24px' }}>Simulated Playoff Standings</h1>
        <p style={{ color: 'var(--text-secondary)', fontSize: '13px' }}>10,000 full-season runs predicting simulated record ranges, division leadership, and championship rates.</p>
      </div>

      {loading ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>COMPILING 32-TEAM STANDINGS GRID...</div>
      ) : (
        <div style={{ display: 'flex', flexDirection: 'column', gap: '40px' }}>
          {renderConference('AFC')}
          {renderConference('NFC')}
        </div>
      )}
    </div>
  );
}

export default Standings;
