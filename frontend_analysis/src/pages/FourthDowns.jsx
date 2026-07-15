import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, Cell } from 'recharts';
import { ShieldAlert, Cpu, Calculator, Play, CheckCircle } from 'lucide-react';

function FourthDowns() {
  const [activeTab, setActiveTab] = useState('live'); // 'live' or 'manual'
  const [games, setGames] = useState([]);
  const [selectedGameId, setSelectedGameId] = useState('');
  const [fourthDowns, setFourthDowns] = useState([]);
  const [selectedPlay, setSelectedPlay] = useState(null);
  
  // Manual Calculator States
  const [calcParams, setCalcParams] = useState({
    distance: 2,
    yardline: 45, // Distance from opponent goal line
    awayScore: 24,
    homeScore: 27,
    quarter: 4,
    timeRemaining: '2:15',
    possession: 'Away',
  });
  const [calcResult, setCalcResult] = useState(null);
  const [calculating, setCalculating] = useState(false);

  useEffect(() => {
    async function loadGames() {
      try {
        const gameList = await ApiService.getLiveGames();
        setGames(gameList || []);
        if (gameList && gameList.length > 0) {
          setSelectedGameId(gameList[0].game_id);
        }
      } catch (err) {
        console.error('Failed to load games', err);
      }
    }
    loadGames();
  }, []);

  useEffect(() => {
    if (!selectedGameId || activeTab !== 'live') return;
    async function loadFourthDowns() {
      try {
        const plays = await ApiService.getFourthDowns(selectedGameId);
        setFourthDowns(plays || []);
        if (plays && plays.length > 0) {
          setSelectedPlay(plays[0]);
        } else {
          setSelectedPlay(null);
        }
      } catch (err) {
        console.error('Failed to load 4th downs', err);
      }
    }
    loadFourthDowns();
  }, [selectedGameId, activeTab]);

  const handleCalculate = async (e) => {
    e.preventDefault();
    setCalculating(true);
    try {
      const res = await ApiService.calculate4thDown(calcParams);
      setCalcResult(res);
    } catch (err) {
      console.error('Failed to calculate manual scenario', err);
    } finally {
      setCalculating(false);
    }
  };

  const getRecommendationBadge = (rec) => {
    if (!rec) return null;
    if (rec.includes('GO')) return <span className="badge badge-green">👉 {rec}</span>;
    if (rec.includes('PUNT')) return <span className="badge badge-cyan">🏈 {rec}</span>;
    return <span className="badge badge-orange">🎯 {rec}</span>;
  };

  return (
    <div>
      {/* Page Header */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '20px' }}>
        <div>
          <h1 style={{ fontSize: '24px' }}>4th Down Decision Explorer</h1>
          <p style={{ color: 'var(--text-secondary)', fontSize: '13px' }}>Evaluate live fourth down decision analytics or run your own customized scenarios.</p>
        </div>
      </div>

      {/* Tabs */}
      <div className="tabs-container">
        <button 
          className={`tab-btn ${activeTab === 'live' ? 'active' : ''}`}
          onClick={() => setActiveTab('live')}
        >
          <Play size={14} style={{ marginRight: '6px', display: 'inline' }} /> Live Decision Feed
        </button>
        <button 
          className={`tab-btn ${activeTab === 'manual' ? 'active' : ''}`}
          onClick={() => setActiveTab('manual')}
        >
          <Calculator size={14} style={{ marginRight: '6px', display: 'inline' }} /> Play Sandbox / Calculator
        </button>
      </div>

      {/* Tab 1: Live Decision Feed */}
      {activeTab === 'live' && (
        <div className="live-wp-workspace">
          {/* Left panel: 4th down list */}
          <div className="panel feed-container" style={{ height: '100%' }}>
            <div className="panel-header">
              <span className="panel-title"><ShieldAlert size={16} /> 4th Down Plays</span>
              <select 
                className="form-select" 
                value={selectedGameId} 
                onChange={(e) => setSelectedGameId(e.target.value)}
                style={{ width: '180px', fontSize: '12px' }}
              >
                {games.map(g => (
                  <option key={g.game_id} value={g.game_id}>{g.away_team} @ {g.home_team}</option>
                ))}
              </select>
            </div>

            <div style={{ marginTop: '12px' }}>
              {fourthDowns.length === 0 ? (
                <div style={{ padding: '24px', textAlign: 'center', color: 'var(--text-muted)' }}>
                  No 4th down occurrences logged in this game feed yet.
                </div>
              ) : (
                fourthDowns.map((play) => (
                  <div 
                    key={play.play_id}
                    className={`play-card ${selectedPlay?.play_id === play.play_id ? 'active' : ''}`}
                    onClick={() => setSelectedPlay(play)}
                  >
                    <div style={{ fontWeight: '600', color: 'var(--accent-orange)', marginBottom: '4px' }}>
                      {play.desc}
                    </div>
                    <div style={{ fontSize: '12px', color: 'var(--text-secondary)' }}>
                      Actual Choice: <strong style={{ color: 'var(--text-primary)' }}>{play.actual}</strong>
                    </div>
                  </div>
                ))
              )}
            </div>
          </div>

          {/* Right panel: Math breakdown */}
          <div className="panel" style={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
            <div className="panel-header">
              <span className="panel-title"><Cpu size={16} /> Expected Value Utility</span>
            </div>

            {selectedPlay ? (
              <div style={{ flex: 1, display: 'flex', flexDirection: 'column', gap: '16px', marginTop: '12px' }}>
                <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                  <div style={{ fontSize: '14px', fontWeight: '600' }}>{selectedPlay.desc}</div>
                  <div>{getRecommendationBadge(selectedPlay.recharts_data.find(d => d.wp === Math.max(...selectedPlay.recharts_data.map(x => x.wp)))?.name)}</div>
                </div>

                <div style={{ flex: 1, minHeight: '220px' }}>
                  <ResponsiveContainer width="100%" height="100%">
                    <BarChart data={selectedPlay.recharts_data} margin={{ top: 10, right: 10, left: -20, bottom: 0 }}>
                      <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                      <XAxis dataKey="label" stroke="var(--text-muted)" fontSize={11} />
                      <YAxis domain={[0, 100]} stroke="var(--text-muted)" fontSize={11} />
                      <Tooltip contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)' }} />
                      <Bar dataKey="wp" name="Win Probability %">
                        {selectedPlay.recharts_data.map((entry, index) => {
                          let color = 'var(--text-muted)';
                          if (entry.name === 'GO') color = 'var(--accent-green)';
                          if (entry.name === 'FG') color = 'var(--accent-orange)';
                          if (entry.name === 'PUNT') color = 'var(--accent-cyan)';
                          return <Cell key={`cell-${index}`} fill={color} />;
                        })}
                      </Bar>
                    </BarChart>
                  </ResponsiveContainer>
                </div>

                <div style={{ borderTop: '1px solid var(--border-color)', paddingTop: '16px', fontSize: '13px' }}>
                  <h4 style={{ fontSize: '13px', textTransform: 'uppercase', marginBottom: '8px', color: 'var(--text-muted)' }}>Decision Table</h4>
                  <table className="tactical-table">
                    <thead>
                      <tr>
                        <th>Option</th>
                        <th>Success Odds</th>
                        <th>Expected Win Probability</th>
                      </tr>
                    </thead>
                    <tbody>
                      {selectedPlay.recharts_data.map(opt => (
                        <tr key={opt.name}>
                          <td style={{ fontWeight: '600' }}>{opt.label}</td>
                          <td>{opt.name === 'PUNT' ? '-' : `${opt.success_rate}%`}</td>
                          <td style={{ color: 'var(--accent-cyan)', fontWeight: '600' }}>{opt.wp}%</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            ) : (
              <div style={{ textAlign: 'center', padding: '40px', color: 'var(--text-muted)' }}>
                Select a fourth down play on the left to analyze numbers.
              </div>
            )}
          </div>
        </div>
      )}

      {/* Tab 2: Manual Calculator */}
      {activeTab === 'manual' && (
        <div className="live-wp-workspace">
          {/* Left panel: Input parameters */}
          <form className="panel" onSubmit={handleCalculate} style={{ height: '100%', overflowY: 'auto' }}>
            <div className="panel-header">
              <span className="panel-title"><Calculator size={16} /> Parameters</span>
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '12px', marginTop: '12px' }}>
              <div className="form-group">
                <label className="form-label">Quarter</label>
                <select 
                  className="form-select"
                  value={calcParams.quarter}
                  onChange={(e) => setCalcParams({ ...calcParams, quarter: parseInt(e.target.value) })}
                >
                  <option value={1}>1st</option>
                  <option value={2}>2nd</option>
                  <option value={3}>3rd</option>
                  <option value={4}>4th</option>
                </select>
              </div>

              <div className="form-group">
                <label className="form-label">Time Remaining</label>
                <input 
                  type="text" 
                  className="form-input" 
                  value={calcParams.timeRemaining}
                  onChange={(e) => setCalcParams({ ...calcParams, timeRemaining: e.target.value })}
                  placeholder="e.g. 2:15"
                />
              </div>
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '12px' }}>
              <div className="form-group">
                <label className="form-label">Offense Score</label>
                <input 
                  type="number" 
                  className="form-input" 
                  value={calcParams.awayScore}
                  onChange={(e) => setCalcParams({ ...calcParams, awayScore: parseInt(e.target.value) })}
                />
              </div>

              <div className="form-group">
                <label className="form-label">Defense Score</label>
                <input 
                  type="number" 
                  className="form-input" 
                  value={calcParams.homeScore}
                  onChange={(e) => setCalcParams({ ...calcParams, homeScore: parseInt(e.target.value) })}
                />
              </div>
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '12px' }}>
              <div className="form-group">
                <label className="form-label">Yards to Go</label>
                <input 
                  type="number" 
                  className="form-input" 
                  value={calcParams.distance}
                  onChange={(e) => setCalcParams({ ...calcParams, distance: parseInt(e.target.value) })}
                  min={1}
                  max={20}
                />
              </div>

              <div className="form-group">
                <label className="form-label">Yard Line (Dist to Goal)</label>
                <input 
                  type="number" 
                  className="form-input" 
                  value={calcParams.yardline}
                  onChange={(e) => setCalcParams({ ...calcParams, yardline: parseInt(e.target.value) })}
                  min={1}
                  max={99}
                />
                <span style={{ fontSize: '10px', color: 'var(--text-muted)' }}>1 = Opp 1-yd line, 99 = Own 1-yd line</span>
              </div>
            </div>

            <button type="submit" className="btn btn-cyan" style={{ width: '100%', marginTop: '16px' }} disabled={calculating}>
              {calculating ? 'CALCULATING UTILITY...' : 'SOLVE STRATEGIC UTILITY'}
            </button>
          </form>

          {/* Right panel: Calculation output */}
          <div className="panel" style={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
            <div className="panel-header">
              <span className="panel-title"><Cpu size={16} /> Simulated EV Results</span>
            </div>

            {calcResult ? (
              <div style={{ flex: 1, display: 'flex', flexDirection: 'column', gap: '16px', marginTop: '12px' }}>
                <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                  <div style={{ fontSize: '14px', fontWeight: '600' }}>
                    4th & {calcParams.distance} at Opp {calcParams.yardline}
                  </div>
                  <div>{getRecommendationBadge(calcResult.recommendation)}</div>
                </div>

                <div style={{ flex: 1, minHeight: '220px' }}>
                  <ResponsiveContainer width="100%" height="100%">
                    <BarChart data={calcResult.recharts_data} margin={{ top: 10, right: 10, left: -20, bottom: 0 }}>
                      <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                      <XAxis dataKey="label" stroke="var(--text-muted)" fontSize={11} />
                      <YAxis domain={[0, 100]} stroke="var(--text-muted)" fontSize={11} />
                      <Tooltip contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)' }} />
                      <Bar dataKey="wp" name="Win Probability %">
                        {calcResult.recharts_data.map((entry, index) => {
                          let color = 'var(--text-muted)';
                          if (entry.name === 'GO') color = 'var(--accent-green)';
                          if (entry.name === 'FG') color = 'var(--accent-orange)';
                          if (entry.name === 'PUNT') color = 'var(--accent-cyan)';
                          return <Cell key={`cell-${index}`} fill={color} />;
                        })}
                      </Bar>
                    </BarChart>
                  </ResponsiveContainer>
                </div>

                <div style={{ borderTop: '1px solid var(--border-color)', paddingTop: '16px', fontSize: '13px' }}>
                  <h4 style={{ fontSize: '13px', textTransform: 'uppercase', marginBottom: '8px', color: 'var(--text-muted)' }}>Expected Values</h4>
                  <table className="tactical-table">
                    <thead>
                      <tr>
                        <th>Option</th>
                        <th>Model Success Rate</th>
                        <th>Win Expectancy</th>
                      </tr>
                    </thead>
                    <tbody>
                      <tr>
                        <td>Go For It</td>
                        <td>{calcResult.success_go.toFixed(1)}%</td>
                        <td style={{ color: 'var(--accent-green)', fontWeight: '600' }}>{calcResult.wp_go}%</td>
                      </tr>
                      <tr>
                        <td>Field Goal</td>
                        <td>{calcResult.success_fg.toFixed(1)}%</td>
                        <td style={{ color: 'var(--accent-orange)', fontWeight: '600' }}>{calcResult.wp_fg}%</td>
                      </tr>
                      <tr>
                        <td>Punt</td>
                        <td>-</td>
                        <td style={{ color: 'var(--accent-cyan)', fontWeight: '600' }}>{calcResult.wp_punt}%</td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </div>
            ) : (
              <div style={{ textAlign: 'center', padding: '40px', color: 'var(--text-muted)', margin: 'auto' }}>
                Fill out the situation details on the left and click Solve to verify expected win probabilities.
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}

export default FourthDowns;
