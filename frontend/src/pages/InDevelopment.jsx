import { useState } from 'react'
import { PAGES } from '../pagesConfig'

export default function InDevelopment({ currentPage }) {
  const pageConfig = PAGES.find(p => p.id === currentPage);

  // OPTIMIZER STATE
  const [optBudget, setOptBudget] = useState(50000);
  const [optCovariance, setOptCovariance] = useState(0.4);
  const [optLineup, setOptLineup] = useState([
    { pos: 'QB', name: 'Patrick Mahomes', salary: 7600, points: 21.2, value: 2.78 },
    { pos: 'RB1', name: 'Christian McCaffrey', salary: 9200, points: 23.4, value: 2.54 },
    { pos: 'RB2', name: 'Breece Hall', salary: 7800, points: 19.8, value: 2.53 },
    { pos: 'WR1', name: 'Justin Jefferson', salary: 8800, points: 21.5, value: 2.44 },
    { pos: 'WR2', name: 'Mico Hardman', salary: 3800, points: 8.6, value: 2.26 },
    { pos: 'WR3', name: 'Empty', salary: 0, points: 0, value: 0 },
    { pos: 'TE', name: 'Travis Kelce', salary: 6200, points: 15.4, value: 2.48 },
    { pos: 'FLEX', name: 'Empty', salary: 0, points: 0, value: 0 },
    { pos: 'DST', name: 'Chiefs DST', salary: 3200, points: 8.2, value: 2.56 }
  ]);

  // PROPS STATE
  const [propTab, setPropTab] = useState('weekly'); // 'weekly' or 'season'
  const [minEdge, setMinEdge] = useState(5);
  const [propSearch, setPropSearch] = useState('');

  const mockProps = [
    { player: 'Lamar Jackson', team: 'BAL', type: 'Pass Yards', line: 224.5, sim: 248.2, edge: 10.6, oOdds: -110, uOdds: -110, pick: 'OVER' },
    { player: 'Saquon Barkley', team: 'PHI', type: 'Rush Yards', line: 78.5, sim: 84.6, edge: 7.7, oOdds: -115, uOdds: -105, pick: 'OVER' },
    { player: 'Amon-Ra St. Brown', team: 'DET', type: 'Receptions', line: 6.5, sim: 7.4, edge: 13.8, oOdds: +105, uOdds: -135, pick: 'OVER' },
    { player: 'Josh Allen', team: 'BUF', type: 'Rush TDs', line: 0.5, sim: 0.72, edge: 44.0, oOdds: +110, uOdds: -140, pick: 'OVER' },
    { player: 'Justin Jefferson', team: 'MIN', type: 'Rec Yards', line: 92.5, sim: 87.1, edge: -5.8, oOdds: -115, uOdds: -115, pick: 'UNDER' },
    { player: 'Davante Adams', team: 'NYJ', type: 'Receptions', line: 5.5, sim: 5.2, edge: -5.4, oOdds: -120, uOdds: +100, pick: 'UNDER' }
  ];

  const mockFutures = [
    { team: 'Lions', type: 'Season Wins', line: 10.5, sim: 11.8, edge: 12.3, oOdds: -120, pick: 'OVER' },
    { team: 'Chiefs', type: 'Season Wins', line: 11.5, sim: 12.4, edge: 7.8, oOdds: -110, pick: 'OVER' },
    { team: 'Cowboys', type: 'Season Wins', line: 9.5, sim: 8.6, edge: -9.4, oOdds: +115, pick: 'UNDER' },
    { team: 'Texans', type: 'Win AFC South', line: 58.0, sim: 64.2, edge: 10.7, oOdds: -135, pick: 'YES' }
  ];

  // WAR STATE
  const [warPos, setWarPos] = useState('ALL');
  const mockWar = [
    { name: 'Patrick Mahomes', team: 'KC', pos: 'QB', war: 4.82, salary: 45.0, valueRatio: 1.07 },
    { name: 'Lamar Jackson', team: 'BAL', pos: 'QB', war: 4.65, salary: 42.5, valueRatio: 1.09 },
    { name: 'Christian McCaffrey', team: 'SF', pos: 'RB', war: 2.84, salary: 16.0, valueRatio: 1.77 },
    { name: 'Justin Jefferson', team: 'MIN', pos: 'WR', war: 2.45, salary: 35.0, valueRatio: 0.70 },
    { name: 'CeeDee Lamb', team: 'DAL', pos: 'WR', war: 2.32, salary: 34.0, valueRatio: 0.68 },
    { name: 'Travis Kelce', team: 'KC', pos: 'TE', war: 1.88, salary: 14.2, valueRatio: 1.32 }
  ];

  // 4TH DOWN STATE
  const [botDist, setBotDist] = useState(4);
  const [botField, setBotField] = useState(42); // yards to opponent goal line (e.g. Opp 42)

  if (!pageConfig) {
    return <div className="glass-panel">Page configuration not found.</div>;
  }

  // --- RENDER COMPONENT HEADER ---
  const renderHeader = () => (
    <div style={{
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      borderBottom: '1px solid rgba(255,255,255,0.06)',
      paddingBottom: '20px',
      marginBottom: '24px',
      flexWrap: 'wrap',
      gap: '15px'
    }}>
      <div>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
          <span style={{ fontSize: '2.5rem' }}>{pageConfig.icon || '🛠️'}</span>
          <div>
            <h1 style={{ fontSize: '1.8rem', color: 'var(--text-white)' }}>{pageConfig.label}</h1>
            <span style={{ fontSize: '0.85rem', color: 'var(--text-muted)' }}>{pageConfig.description}</span>
          </div>
        </div>
      </div>
      
      <div style={{
        background: 'rgba(255, 170, 0, 0.08)',
        border: '1px solid rgba(255, 170, 0, 0.2)',
        color: 'var(--accent-gold)',
        padding: '8px 16px',
        borderRadius: '8px',
        textAlign: 'right'
      }}>
        <div style={{ fontSize: '0.72rem', fontWeight: 800, textTransform: 'uppercase' }}>Preview Wireframe</div>
        <div style={{ fontSize: '0.9rem', fontWeight: 700 }}>Target: {pageConfig.targetDate}</div>
      </div>
    </div>
  );

  // --- 1. OPTIMIZER PREVIEW ---
  const renderOptimizer = () => {
    const totalSpent = optLineup.reduce((sum, p) => sum + p.salary, 0);
    const remSalary = optBudget - totalSpent;
    const projectedPts = optLineup.reduce((sum, p) => sum + p.points, 0);

    return (
      <div className="dashboard-grid">
        {/* Controls Column */}
        <div className="settings-section">
          <div className="glass-panel">
            <h2>Optimization Settings</h2>
            
            {/* Salary Slider */}
            <div className="control-group" style={{ marginTop: '16px' }}>
              <div className="control-label">
                <span>Salary Cap Limit</span>
                <span className="control-val">${optBudget.toLocaleString()}</span>
              </div>
              <input 
                type="range" min="40000" max="60000" step="500"
                value={optBudget}
                onChange={(e) => setOptBudget(parseInt(e.target.value))}
              />
            </div>

            {/* Risk / Correlation Slider */}
            <div className="control-group">
              <div className="control-label">
                <span>Covariance Decay (Stacking)</span>
                <span className="control-val">{(optCovariance * 100).toFixed(0)}%</span>
              </div>
              <input 
                type="range" min="0.0" max="1.0" step="0.05"
                value={optCovariance}
                onChange={(e) => setOptCovariance(parseFloat(e.target.value))}
              />
            </div>

            <div style={{ display: 'flex', flexDirection: 'column', gap: '10px', marginTop: '24px' }}>
              <label style={{ display: 'flex', alignItems: 'center', gap: '8px', fontSize: '0.85rem', cursor: 'pointer' }}>
                <input type="checkbox" defaultChecked style={{ accentColor: 'var(--accent-secondary)' }} />
                Force QB + WR Stacking
              </label>
              <label style={{ display: 'flex', alignItems: 'center', gap: '8px', fontSize: '0.85rem', cursor: 'pointer' }}>
                <input type="checkbox" defaultChecked style={{ accentColor: 'var(--accent-secondary)' }} />
                Force Opposing Bring-Back WR
              </label>
            </div>

            <button 
              className="btn-primary" 
              style={{ marginTop: '24px', background: 'linear-gradient(135deg, var(--accent-secondary) 0%, #8a2be2 100%)', boxShadow: 'var(--glow-purple)' }}
              onClick={() => alert("Simulating optimizer runs... Roster mathematical knapsack limits balanced.")}
            >
              Solve Lineups
            </button>
          </div>
        </div>

        {/* Lineup Column */}
        <div className="settings-section">
          <div className="glass-panel">
            <div className="flex-between" style={{ marginBottom: '16px' }}>
              <h2>Optimized Lineup Preview</h2>
              <div style={{ fontSize: '0.9rem', color: remSalary < 0 ? 'var(--accent-red)' : 'var(--accent-green)', fontWeight: 700 }}>
                Cap Left: ${remSalary.toLocaleString()}
              </div>
            </div>

            <div className="table-container" style={{ maxHeight: '420px', marginBottom: '20px' }}>
              <table>
                <thead>
                  <tr>
                    <th>Position</th>
                    <th>Player</th>
                    <th>Salary</th>
                    <th>Projected Pts</th>
                    <th>Value Rating</th>
                  </tr>
                </thead>
                <tbody>
                  {optLineup.map((p, idx) => (
                    <tr key={idx}>
                      <td style={{ fontWeight: 700, color: 'var(--accent-secondary)' }}>{p.pos}</td>
                      <td style={{ color: p.name === 'Empty' ? 'var(--text-muted)' : 'var(--text-white)' }}>{p.name}</td>
                      <td style={{ fontFamily: 'monospace' }}>${p.salary.toLocaleString()}</td>
                      <td style={{ fontFamily: 'monospace' }}>{p.points.toFixed(1)}</td>
                      <td>
                        {p.value > 0 ? (
                          <span style={{
                            padding: '2px 6px',
                            borderRadius: '4px',
                            background: 'rgba(0, 242, 254, 0.1)',
                            color: 'var(--accent-primary)',
                            fontSize: '0.8rem',
                            fontWeight: 700
                          }}>
                            {p.value.toFixed(2)}x
                          </span>
                        ) : '-'}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            <div className="flex-between">
              <div style={{ fontSize: '0.95rem', color: 'var(--text-main)' }}>
                Total Projected Pts: <strong style={{ color: 'var(--accent-primary)' }}>{projectedPts.toFixed(1)}</strong>
              </div>
              <button 
                disabled 
                style={{
                  background: 'rgba(255,255,255,0.05)',
                  border: '1px solid rgba(255,255,255,0.08)',
                  color: 'var(--text-muted)',
                  padding: '8px 16px',
                  borderRadius: '8px',
                  cursor: 'not-allowed'
                }}
              >
                Export CSV (CSV Lock)
              </button>
            </div>
          </div>
        </div>
      </div>
    );
  };

  // --- 2. PROP FINDER PREVIEW ---
  const renderProps = () => {
    const list = propTab === 'weekly' ? mockProps : mockFutures;
    const filtered = list.filter(p => {
      const nameMatch = p.player ? p.player.toLowerCase().includes(propSearch.toLowerCase()) : p.team.toLowerCase().includes(propSearch.toLowerCase());
      const edgeMatch = Math.abs(p.edge) >= minEdge;
      return nameMatch && edgeMatch;
    });

    return (
      <div className="glass-panel">
        <div className="flex-between" style={{ marginBottom: '20px', flexWrap: 'wrap', gap: '15px' }}>
          {/* Sub tabs */}
          <div className="tab-switcher" style={{ maxWidth: '280px' }}>
            <button className={`tab-btn ${propTab === 'weekly' ? 'active' : ''}`} onClick={() => setPropTab('weekly')}>Weekly Props</button>
            <button className={`tab-btn ${propTab === 'season' ? 'active' : ''}`} onClick={() => setPropTab('season')}>Season Futures</button>
          </div>
          
          {/* Slider edge constraint */}
          <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
            <span style={{ fontSize: '0.85rem', color: 'var(--text-muted)' }}>Min Value Edge:</span>
            <input 
              type="range" min="2" max="15" step="1"
              value={minEdge}
              onChange={(e) => setMinEdge(parseInt(e.target.value))}
              style={{ width: '100px' }}
            />
            <span style={{ fontSize: '0.88rem', fontWeight: 700, color: 'var(--accent-gold)', fontFamily: 'monospace' }}>+{minEdge}%</span>
          </div>
        </div>

        {/* Search */}
        <div style={{ marginBottom: '16px' }}>
          <input 
            type="text"
            placeholder={propTab === 'weekly' ? "Search player name..." : "Search team name..."}
            className="search-input"
            value={propSearch}
            onChange={(e) => setPropSearch(e.target.value)}
            style={{ width: '100%', maxWidth: '300px' }}
          />
        </div>

        {/* Data Table */}
        <div className="table-container">
          <table>
            <thead>
              <tr>
                <th>Subject</th>
                <th>Type</th>
                <th>Vegas Line</th>
                <th>Sim Probability / Avg</th>
                <th>Implied Odds</th>
                <th>Value Edge</th>
                <th>Sim Verdict</th>
              </tr>
            </thead>
            <tbody>
              {filtered.map((item, idx) => (
                <tr key={idx} style={{
                  background: Math.abs(item.edge) >= 10 ? 'rgba(0, 242, 254, 0.04)' : 'transparent'
                }}>
                  <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>{item.player || item.team}</td>
                  <td>{item.type}</td>
                  <td style={{ fontFamily: 'monospace' }}>{item.line}</td>
                  <td style={{ fontFamily: 'monospace', color: 'var(--accent-primary)' }}>
                    {typeof item.sim === 'number' && item.sim > 20 ? item.sim.toFixed(1) : item.sim}
                  </td>
                  <td style={{ fontFamily: 'monospace' }}>{item.oOdds > 0 ? `+${item.oOdds}` : item.oOdds}</td>
                  <td style={{
                    fontFamily: 'monospace',
                    fontWeight: 700,
                    color: item.edge > 0 ? 'var(--accent-green)' : 'var(--accent-red)'
                  }}>
                    {item.edge > 0 ? `+${item.edge}%` : `${item.edge}%`}
                  </td>
                  <td>
                    <span style={{
                      padding: '2px 8px',
                      borderRadius: '4px',
                      background: item.pick === 'OVER' || item.pick === 'YES' ? 'rgba(0, 245, 212, 0.12)' : 'rgba(255, 56, 56, 0.12)',
                      color: item.pick === 'OVER' || item.pick === 'YES' ? 'var(--accent-green)' : 'var(--accent-red)',
                      fontSize: '0.78rem',
                      fontWeight: 800
                    }}>
                      {item.pick}
                    </span>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    );
  };

  // --- 3. WAR METRIC PREVIEW ---
  const renderWAR = () => {
    return (
      <div className="glass-panel">
        <div className="flex-between" style={{ marginBottom: '20px' }}>
          <h2>Roster WAR Matrix</h2>
          
          <select 
            value={warPos} 
            onChange={(e) => setWarPos(e.target.value)}
            style={{
              background: 'rgba(11, 17, 38, 0.6)',
              border: '1px solid var(--border-glass)',
              color: 'var(--text-white)',
              borderRadius: '8px',
              padding: '6px 12px',
              fontWeight: 600,
              cursor: 'pointer'
            }}
          >
            <option value="ALL">All Positions</option>
            <option value="QB">Quarterbacks</option>
            <option value="RB">Running Backs</option>
            <option value="WR">Receivers</option>
            <option value="TE">Tight Ends</option>
          </select>
        </div>

        <div className="table-container">
          <table>
            <thead>
              <tr>
                <th>Player</th>
                <th>Team</th>
                <th>Position</th>
                <th className="tooltip-trigger" data-tooltip="Simulated Wins Above Replacement metric">Simulated WAR</th>
                <th>Salary Cap Hit</th>
                <th>Cap Value Rating</th>
              </tr>
            </thead>
            <tbody>
              {mockWar
                .filter(p => warPos === 'ALL' || p.pos === warPos)
                .map((p, idx) => (
                  <tr key={idx}>
                    <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>{p.name}</td>
                    <td>{p.team}</td>
                    <td><span style={{
                      padding: '2px 6px',
                      borderRadius: '4px',
                      background: 'rgba(157, 78, 221, 0.1)',
                      color: 'var(--accent-secondary)',
                      fontSize: '0.8rem',
                      fontWeight: 600
                    }}>{p.pos}</span></td>
                    <td style={{ fontFamily: 'monospace', fontWeight: 700, color: 'var(--accent-primary)' }}>+{p.war.toFixed(2)}</td>
                    <td style={{ fontFamily: 'monospace' }}>${p.salary.toFixed(1)}M</td>
                    <td>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                        <div style={{ width: '80px', height: '6px', background: 'rgba(255,255,255,0.06)', borderRadius: '3px', overflow: 'hidden' }}>
                          <div style={{ width: `${Math.min(p.valueRatio * 50, 100)}%`, height: '100%', background: p.valueRatio >= 1.0 ? 'var(--accent-green)' : 'var(--accent-red)' }}></div>
                        </div>
                        <span style={{ fontSize: '0.8rem', fontWeight: 600, color: p.valueRatio >= 1.0 ? 'var(--accent-green)' : 'var(--accent-red)', fontFamily: 'monospace' }}>
                          {p.valueRatio.toFixed(2)}
                        </span>
                      </div>
                    </td>
                  </tr>
                ))}
            </tbody>
          </table>
        </div>
      </div>
    );
  };

  // --- 4. 4TH DOWN BOT PREVIEW ---
  const renderBot = () => {
    return (
      <div className="dashboard-grid">
        {/* Simulator controls */}
        <div className="settings-section">
          <div className="glass-panel">
            <h2>Calculator Inputs</h2>

            {/* Distance Slider */}
            <div className="control-group" style={{ marginTop: '16px' }}>
              <div className="control-label">
                <span>Distance to Gain</span>
                <span className="control-val">4th & {botDist} yds</span>
              </div>
              <input 
                type="range" min="1" max="15" step="1"
                value={botDist}
                onChange={(e) => setBotDist(parseInt(e.target.value))}
              />
            </div>

            {/* Field Position Slider */}
            <div className="control-group">
              <div className="control-label">
                <span>Yards to Goal line</span>
                <span className="control-val">Opp {botField} yd line</span>
              </div>
              <input 
                type="range" min="1" max="99" step="1"
                value={botField}
                onChange={(e) => setBotField(parseInt(e.target.value))}
              />
            </div>
          </div>
        </div>

        {/* Results meter bars */}
        <div className="settings-section">
          <div className="glass-panel">
            <h2>Decision Probabilities</h2>
            
            <div style={{ display: 'flex', flexDirection: 'column', gap: '20px', marginTop: '16px' }}>
              {/* Go option */}
              <div>
                <div className="flex-between" style={{ marginBottom: '6px', fontSize: '0.9rem' }}>
                  <span style={{ fontWeight: 700, color: 'var(--accent-green)' }}>👉 GO FOR IT (Sim Recommended)</span>
                  <span style={{ fontWeight: 700, fontFamily: 'monospace' }}>Sim WP: 54.2%</span>
                </div>
                <div style={{ width: '100%', height: '12px', background: 'rgba(255,255,255,0.06)', borderRadius: '6px', overflow: 'hidden' }}>
                  <div style={{ width: '54.2%', height: '100%', background: 'var(--accent-green)', boxShadow: '0 0 10px rgba(0, 245, 212, 0.4)' }}></div>
                </div>
                <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)', marginTop: '4px' }}>
                  Go conversion success odds: 48.5%
                </div>
              </div>

              {/* Punt option */}
              <div>
                <div className="flex-between" style={{ marginBottom: '6px', fontSize: '0.9rem' }}>
                  <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>🏈 PUNT</span>
                  <span style={{ fontWeight: 700, fontFamily: 'monospace' }}>Sim WP: 49.8%</span>
                </div>
                <div style={{ width: '100%', height: '12px', background: 'rgba(255,255,255,0.06)', borderRadius: '6px', overflow: 'hidden' }}>
                  <div style={{ width: '49.8%', height: '100%', background: 'var(--text-main)' }}></div>
                </div>
                <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)', marginTop: '4px' }}>
                  Avg net punt distance: 34.6 yards to inside-20
                </div>
              </div>

              {/* FG option */}
              <div>
                <div className="flex-between" style={{ marginBottom: '6px', fontSize: '0.9rem' }}>
                  <span style={{ fontWeight: 700, color: 'var(--accent-secondary)' }}>👟 FIELD GOAL TRY</span>
                  <span style={{ fontWeight: 700, fontFamily: 'monospace' }}>Sim WP: 46.1%</span>
                </div>
                <div style={{ width: '100%', height: '12px', background: 'rgba(255,255,255,0.06)', borderRadius: '6px', overflow: 'hidden' }}>
                  <div style={{ width: '46.1%', height: '100%', background: 'var(--accent-secondary)' }}></div>
                </div>
                <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)', marginTop: '4px' }}>
                  FG conversion odds (59-yard attempt): 38.2%
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  };

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      {renderHeader()}

      {/* RENDER ACTIVE PREVIEW DASHBOARD */}
      {currentPage === 'optimizer' && renderOptimizer()}
      {currentPage === 'props' && renderProps()}
      {currentPage === 'war' && renderWAR()}
      {currentPage === 'bot' && renderBot()}
    </div>
  )
}
