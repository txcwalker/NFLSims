import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, BarChart, Bar, Cell } from 'recharts';
import { Play, TrendingUp, Info, Activity, ShieldAlert, Cpu, ArrowLeft, Layers } from 'lucide-react';

function GameSummary({ gameId, navigateTo }) {
  const [games, setGames] = useState([]);
  const [playByPlay, setPlayByPlay] = useState([]);
  const [selectedPlayId, setSelectedPlayId] = useState(null);
  const [fourthDowns, setFourthDowns] = useState([]);
  const [stats, setStats] = useState(null);
  const [chessEval, setChessEval] = useState(null);
  const [conceptResult, setConceptResult] = useState(null);
  const [conceptLoading, setConceptLoading] = useState(false);

  const [activeTab, setActiveTab] = useState('center'); // 'center', 'fourth', 'chess'
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function loadAllGameDetails() {
      if (!gameId) return;
      setLoading(true);
      try {
        const [gameList, plays, fourths, gameStats, evaluator] = await Promise.all([
          ApiService.getLiveGames(),
          ApiService.getPlayByPlay(gameId),
          ApiService.getFourthDowns(gameId),
          ApiService.getGameStats(gameId),
          ApiService.getChessEvaluator(gameId)
        ]);
        
        setGames(gameList || []);
        setPlayByPlay(plays || []);
        setFourthDowns(fourths || []);
        setStats(gameStats);
        setChessEval(evaluator);

        if (plays && plays.length > 0) {
          setSelectedPlayId(plays[plays.length - 1].play_id);
        }
      } catch (err) {
        console.error('Failed to load game summary metrics', err);
      } finally {
        setLoading(false);
      }
    }
    loadAllGameDetails();
  }, [gameId]);

  // Fetch concept recommendations from the slider API when a chess play is selected
  useEffect(() => {
    if (activeTab !== 'chess' || !chessEval) return;
    const evals = chessEval.evaluations;
    if (!evals?.length) return;
    const ev = evals.find(e => String(e.play_id) === String(selectedPlayId)) ?? evals[evals.length - 1];
    if (!ev) return;

    const [mm, ss] = ev.clock.split(':').map(Number);
    const gameSec = (4 - ev.qtr) * 900 + mm * 60 + ss;

    const params = new URLSearchParams({
      down: ev.down, distance: ev.ydstogo, yardline_100: ev.yardline_100,
      clock: gameSec, score_differential: ev.score_differential,
      posteam_timeouts: 3, defteam_timeouts: 3, n_sims: 300,
    });

    const controller = new AbortController();
    setConceptLoading(true);
    setConceptResult(null);
    fetch(`/api/positional-evaluator?${params}`, { signal: controller.signal })
      .then(r => r.ok ? r.json() : Promise.reject())
      .then(data => { setConceptResult(data); setConceptLoading(false); })
      .catch(err => { if (err.name !== 'AbortError') setConceptLoading(false); });

    return () => controller.abort();
  }, [selectedPlayId, activeTab, chessEval]);

  const activeGame = games.find(g => g.game_id === gameId);
  const activePlay = playByPlay.find(p => p.play_id === selectedPlayId);

  // Recharts WP configuration
  const chartData = playByPlay.map((play, index) => ({
    name: `P${index + 1}`,
    [activeGame?.home_team || 'Home']: play.home_wp,
    [activeGame?.away_team || 'Away']: play.away_wp,
    play_id: play.play_id,
    desc: play.desc
  }));

  const getRecommendationBadge = (rec) => {
    if (!rec) return null;
    if (rec.includes('GO')) return <span className="badge badge-green">👉 {rec}</span>;
    if (rec.includes('PUNT')) return <span className="badge badge-cyan">🏈 {rec}</span>;
    return <span className="badge badge-orange">🎯 {rec}</span>;
  };

  return (
    <div>
      {/* Return button */}
      <button 
        className="btn btn-outline" 
        onClick={() => navigateTo('home')}
        style={{ marginBottom: '16px', fontSize: '12px', padding: '6px 12px' }}
      >
        <ArrowLeft size={14} style={{ marginRight: '4px' }} /> Return to Dashboard
      </button>

      {loading || !activeGame ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>SYNCING GAME WORKSPACE STATE...</div>
      ) : (
        <div style={{ display: 'flex', flexDirection: 'column', gap: '20px' }}>
          
          {/* Sticky Status Banner */}
          <div className="panel" style={{ 
            background: 'linear-gradient(90deg, var(--bg-secondary) 0%, rgba(0, 242, 254, 0.02) 100%)',
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
            padding: '16px 24px',
            borderLeft: '4px solid var(--accent-cyan)'
          }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: '24px' }}>
              <div style={{ display: 'flex', gap: '16px', alignItems: 'center' }}>
                <span style={{ fontSize: '20px', fontWeight: '800', color: activeGame.possession === activeGame.away_team ? 'var(--accent-cyan)' : 'inherit' }}>
                  {activeGame.away_team} {activeGame.away_score}
                </span>
                <span style={{ color: 'var(--text-muted)' }}>@</span>
                <span style={{ fontSize: '20px', fontWeight: '800', color: activeGame.possession === activeGame.home_team ? 'var(--accent-cyan)' : 'inherit' }}>
                  {activeGame.home_team} {activeGame.home_score}
                </span>
              </div>
              <div className="badge badge-cyan" style={{ fontSize: '10px' }}>
                Q{activeGame.quarter} | {activeGame.time_remaining}
              </div>
            </div>

            <div style={{ display: 'flex', gap: '20px', fontSize: '13px' }}>
              {activeGame.down && (
                <div style={{ backgroundColor: 'var(--bg-tertiary)', padding: '6px 12px', borderRadius: '4px' }}>
                  State: <strong style={{ color: 'var(--accent-orange)' }}>{activeGame.down}nd & {activeGame.distance}</strong> at {activeGame.yardline}
                </div>
              )}
              <div style={{ backgroundColor: 'var(--bg-tertiary)', padding: '6px 12px', borderRadius: '4px' }}>
                Leverage Index: <strong style={{ color: 'var(--accent-cyan)' }}>{activeGame.leverage}</strong>
              </div>
            </div>
          </div>

          {/* Workspace Menu Tabs */}
          <div className="tabs-container" style={{ margin: '0' }}>
            <button className={`tab-btn ${activeTab === 'center' ? 'active' : ''}`} onClick={() => setActiveTab('center')}>
              <TrendingUp size={14} style={{ marginRight: '6px', display: 'inline' }} /> Game Center (WP & Stats)
            </button>
            <button className={`tab-btn ${activeTab === 'fourth' ? 'active' : ''}`} onClick={() => setActiveTab('fourth')}>
              <ShieldAlert size={14} style={{ marginRight: '6px', display: 'inline' }} /> 4th Down Decisions
            </button>
            <button className={`tab-btn ${activeTab === 'chess' ? 'active' : ''}`} onClick={() => setActiveTab('chess')}>
              <Cpu size={14} style={{ marginRight: '6px', display: 'inline' }} /> Chess Tactical Evaluator
            </button>
          </div>

          {/* Tab 1: Game Center */}
          {activeTab === 'center' && (
            <div className="dashboard-grid" style={{ margin: '0' }}>
              {/* Left Column: Recharts and Stats */}
              <div style={{ display: 'flex', flexDirection: 'column', gap: '20px' }}>
                {/* Line Chart */}
                <div className="panel" style={{ flex: 1, minHeight: '340px' }}>
                  <div className="panel-header">
                    <span className="panel-title"><TrendingUp size={16} /> Win Expectancy Graph</span>
                  </div>
                  
                  <div style={{ height: '240px', width: '100%', marginTop: '12px' }}>
                    <ResponsiveContainer width="100%" height="100%">
                      <LineChart data={chartData} margin={{ top: 10, right: 10, left: -20, bottom: 0 }}>
                        <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                        <XAxis dataKey="name" stroke="var(--text-muted)" fontSize={11} />
                        <YAxis domain={[0, 100]} stroke="var(--text-muted)" fontSize={11} />
                        <Tooltip contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)' }} />
                        <ReferenceLine y={50} stroke="var(--text-muted)" strokeDasharray="3 3" />
                        <Line type="monotone" dataKey={activeGame.away_team} stroke="var(--accent-cyan)" strokeWidth={2} dot={{ r: 3 }} />
                        <Line type="monotone" dataKey={activeGame.home_team} stroke="var(--text-muted)" strokeWidth={1.5} strokeDasharray="4 4" dot={{ r: 2 }} />
                      </LineChart>
                    </ResponsiveContainer>
                  </div>
                </div>

                {/* Team Stats */}
                {stats && (
                  <div className="panel">
                    <div className="panel-header">
                      <span className="panel-title"><Layers size={16} /> Team Matchup Stats</span>
                    </div>
                    <table className="tactical-table">
                      <thead>
                        <tr>
                          <th>Stat Category</th>
                          <th>{stats.away.team} (Away)</th>
                          <th>{stats.home.team} (Home)</th>
                        </tr>
                      </thead>
                      <tbody>
                        <tr>
                          <td>First Downs</td>
                          <td>{stats.away.first_downs}</td>
                          <td>{stats.home.first_downs}</td>
                        </tr>
                        <tr>
                          <td>Total Yards</td>
                          <td style={{ fontWeight: '600' }}>{stats.away.total_yds}</td>
                          <td style={{ fontWeight: '600' }}>{stats.home.total_yds}</td>
                        </tr>
                        <tr>
                          <td>Passing Yards</td>
                          <td>{stats.away.pass_yds}</td>
                          <td>{stats.home.pass_yds}</td>
                        </tr>
                        <tr>
                          <td>Rushing Yards</td>
                          <td>{stats.away.rush_yds}</td>
                          <td>{stats.home.rush_yds}</td>
                        </tr>
                        <tr>
                          <td>Turnovers</td>
                          <td style={{ color: stats.away.turnovers > 0 ? 'var(--accent-orange)' : 'inherit' }}>{stats.away.turnovers}</td>
                          <td style={{ color: stats.home.turnovers > 0 ? 'var(--accent-orange)' : 'inherit' }}>{stats.home.turnovers}</td>
                        </tr>
                        <tr>
                          <td>EPA per Play</td>
                          <td style={{ color: stats.away.epa_play > 0 ? 'var(--accent-cyan)' : 'inherit' }}>{stats.away.epa_play}</td>
                          <td style={{ color: stats.home.epa_play > 0 ? 'var(--accent-cyan)' : 'inherit' }}>{stats.home.epa_play}</td>
                        </tr>
                      </tbody>
                    </table>
                  </div>
                )}
              </div>

              {/* Right Column: Interactive Play Feed */}
              <div className="panel feed-container" style={{ maxHeight: '550px' }}>
                <div className="panel-header">
                  <span className="panel-title"><Info size={16} /> Play Log</span>
                </div>
                <div style={{ marginTop: '12px' }}>
                  {playByPlay.map(play => (
                    <div 
                      key={play.play_id}
                      className={`play-card ${selectedPlayId === play.play_id ? 'active' : ''}`}
                      onClick={() => setSelectedPlayId(play.play_id)}
                    >
                      <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', color: 'var(--text-muted)', marginBottom: '4px' }}>
                        <span>Q{play.qtr} | {play.time}</span>
                        <span style={{ color: play.possession === activeGame.away_team ? 'var(--accent-cyan)' : 'inherit', fontWeight: '700' }}>{play.possession} Ball</span>
                      </div>
                      <div style={{ fontSize: '12px', lineHeight: '1.4' }}>{play.desc}</div>
                    </div>
                  ))}
                </div>
              </div>
            </div>
          )}

          {/* Tab 2: 4th Down Decisions */}
          {activeTab === 'fourth' && (
            <div className="dashboard-grid" style={{ margin: '0' }}>
              {/* Left Column: 4th downs list */}
              <div className="panel feed-container" style={{ maxHeight: '450px' }}>
                <div className="panel-header">
                  <span className="panel-title"><ShieldAlert size={16} /> Game 4th Downs</span>
                </div>
                <div style={{ marginTop: '12px' }}>
                  {fourthDowns.length === 0 ? (
                    <div style={{ padding: '24px', textAlign: 'center', color: 'var(--text-muted)' }}>
                      No fourth-down plays analyzed in this matchup.
                    </div>
                  ) : (
                    fourthDowns.map(play => (
                      <div 
                        key={play.play_id}
                        className={`play-card ${selectedPlayId === play.play_id ? 'active' : ''}`}
                        onClick={() => setSelectedPlayId(play.play_id)}
                      >
                        <div style={{ fontWeight: '600', color: 'var(--accent-orange)' }}>{play.desc}</div>
                        <div style={{ fontSize: '11px', color: 'var(--text-secondary)', marginTop: '4px' }}>Actual: {play.actual}</div>
                      </div>
                    ))
                  )}
                </div>
              </div>

              {/* Right Column: Comparative bar chart stats */}
              <div className="panel">
                <div className="panel-header">
                  <span className="panel-title"><Cpu size={16} /> EV Decision Analytics</span>
                </div>
                
                {fourthDowns.find(d => d.play_id === selectedPlayId) ? (
                  (() => {
                    const play = fourthDowns.find(d => d.play_id === selectedPlayId);
                    return (
                      <div style={{ display: 'flex', flexDirection: 'column', gap: '16px', marginTop: '12px' }}>
                        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                          <div style={{ fontWeight: '700' }}>{play.desc}</div>
                          <div>{getRecommendationBadge(play.recharts_data.find(d => d.wp === Math.max(...play.recharts_data.map(x => x.wp)))?.name)}</div>
                        </div>

                        <div style={{ height: '180px', width: '100%' }}>
                          <ResponsiveContainer width="100%" height="100%">
                            <BarChart data={play.recharts_data} margin={{ top: 10, right: 10, left: -20, bottom: 0 }}>
                              <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                              <XAxis dataKey="label" stroke="var(--text-muted)" fontSize={11} />
                              <YAxis domain={[0, 100]} stroke="var(--text-muted)" fontSize={11} />
                              <Tooltip contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)' }} />
                              <Bar dataKey="wp" name="Win Probability">
                                {play.recharts_data.map((entry, index) => {
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

                        <table className="tactical-table">
                          <thead>
                            <tr>
                              <th>Option</th>
                              <th>Success Rate</th>
                              <th>Expected Win Probability</th>
                            </tr>
                          </thead>
                          <tbody>
                            {play.recharts_data.map(opt => (
                              <tr key={opt.name}>
                                <td style={{ fontWeight: '600' }}>{opt.label}</td>
                                <td>{opt.name === 'PUNT' ? '-' : `${opt.success_rate}%`}</td>
                                <td style={{ color: 'var(--accent-cyan)', fontWeight: '600' }}>{opt.wp}%</td>
                              </tr>
                            ))}
                          </tbody>
                        </table>
                      </div>
                    );
                  })()
                ) : (
                  <div style={{ textAlign: 'center', padding: '40px', color: 'var(--text-muted)' }}>
                    Select a fourth down play log card to examine the mathematical utilities.
                  </div>
                )}
              </div>
            </div>
          )}

          {/* Tab 3: Chess Tactical Evaluator */}
          {activeTab === 'chess' && (() => {
            // Build chart data: KEP over play sequence (offense perspective)
            const chessEvals = chessEval?.evaluations || [];
            const chessChartData = chessEvals.map((ev, idx) => ({
              name: `P${idx + 1}`,
              play_id: ev.play_id,
              kep: ev.kep,
              ep: ev.ep,
              off: ev.off,
              label: `Q${ev.qtr} ${ev.clock} | ${ev.off} ${ev.down}&${ev.ydstogo}`,
            }));
            // Find evaluation for the selected play; fall back to last eval if none selected
            const selectedEval = chessEvals.find(e => String(e.play_id) === String(selectedPlayId))
              || chessEvals[chessEvals.length - 1]
              || null;

            return (
              <div className="dashboard-grid" style={{ margin: '0' }}>
                {/* Left: KEP timeline chart */}
                <div className="panel" style={{ display: 'flex', flexDirection: 'column', gap: '12px' }}>
                  <div className="panel-header">
                    <span className="panel-title"><Activity size={16} /> KEP Game Timeline</span>
                    {chessEval && (
                      <span style={{ fontSize: '11px', color: 'var(--text-muted)' }}>
                        {chessEval.away_team} @ {chessEval.home_team} — {chessEval.n_plays} evaluated plays
                      </span>
                    )}
                  </div>

                  {chessChartData.length > 0 ? (
                    <div style={{ height: '240px', width: '100%', marginTop: '8px' }}>
                      <ResponsiveContainer width="100%" height="100%">
                        <LineChart
                          data={chessChartData}
                          margin={{ top: 10, right: 10, left: -20, bottom: 0 }}
                          onClick={(data) => {
                            const pid = data?.activePayload?.[0]?.payload?.play_id;
                            if (pid != null) setSelectedPlayId(pid);
                          }}
                          style={{ cursor: 'pointer' }}
                        >
                          <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                          <XAxis dataKey="name" stroke="var(--text-muted)" fontSize={11} />
                          <YAxis stroke="var(--text-muted)" fontSize={11} />
                          <Tooltip
                            contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)', fontSize: '11px' }}
                            formatter={(val, name) => [val.toFixed(2), name.toUpperCase()]}
                            labelFormatter={(_, payload) => payload?.[0]?.payload?.label || ''}
                          />
                          <ReferenceLine y={0} stroke="var(--text-muted)" strokeDasharray="4 4" />
                          <Line type="monotone" dataKey="kep" name="kep" stroke="var(--accent-cyan)" strokeWidth={2}
                            dot={{ r: 4, cursor: 'pointer' }}
                            activeDot={{ r: 6, strokeWidth: 2, stroke: 'var(--accent-cyan)' }} />
                          <Line type="monotone" dataKey="ep" name="ep" stroke="var(--accent-orange)" strokeWidth={1.5}
                            strokeDasharray="4 4" dot={{ r: 3 }} />
                        </LineChart>
                      </ResponsiveContainer>
                    </div>
                  ) : (
                    <div style={{ padding: '40px', textAlign: 'center', color: 'var(--text-muted)', fontSize: '12px' }}>
                      No positional evaluations available for this game.
                    </div>
                  )}

                  {/* Legend */}
                  {chessChartData.length > 0 && (
                    <div style={{ display: 'flex', gap: '20px', fontSize: '11px', color: 'var(--text-muted)', paddingLeft: '4px' }}>
                      <span><span style={{ color: 'var(--accent-cyan)', fontWeight: 700 }}>— KEP</span> clock-adjusted positional value</span>
                      <span><span style={{ color: 'var(--accent-orange)', fontWeight: 700 }}>- - EP</span> situational field value</span>
                    </div>
                  )}
                </div>

                {/* Right: selected play detail */}
                <div className="panel">
                  <div className="panel-header">
                    <span className="panel-title"><Cpu size={16} /> Play Positional Detail</span>
                  </div>

                  {selectedEval ? (
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '14px', marginTop: '12px' }}>
                      {/* Play context */}
                      <div style={{
                        backgroundColor: 'var(--bg-tertiary)',
                        padding: '10px 14px',
                        borderRadius: '6px',
                        borderLeft: '3px solid var(--accent-cyan)',
                        fontSize: '12px'
                      }}>
                        <div style={{ color: 'var(--text-muted)', marginBottom: '4px', fontSize: '11px' }}>
                          Q{selectedEval.qtr} {selectedEval.clock}
                        </div>
                        <strong style={{ color: 'var(--text-primary)' }}>
                          {selectedEval.off} {selectedEval.down}&{selectedEval.ydstogo} at {selectedEval.yardline_100} yds out
                        </strong>
                        <div style={{ color: 'var(--text-muted)', marginTop: '4px' }}>
                          vs {selectedEval.def} | margin {selectedEval.score_differential > 0 ? `+${selectedEval.score_differential}` : selectedEval.score_differential}
                        </div>
                      </div>

                      {/* EP + KEP stats */}
                      <table className="tactical-table">
                        <tbody>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>Expected Points (EP)</td>
                            <td style={{ color: 'var(--accent-orange)', fontWeight: '700', fontFamily: 'monospace' }}>
                              {selectedEval.ep >= 0 ? '+' : ''}{selectedEval.ep.toFixed(3)}
                            </td>
                          </tr>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>KEP (clock-aware)</td>
                            <td style={{
                              color: selectedEval.kep >= 0 ? 'var(--accent-cyan)' : 'var(--accent-red)',
                              fontWeight: '700',
                              fontFamily: 'monospace'
                            }}>
                              {selectedEval.kep >= 0 ? '+' : ''}{selectedEval.kep.toFixed(3)}
                            </td>
                          </tr>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>Field Position</td>
                            <td style={{ fontWeight: '600' }}>{selectedEval.yardline_100} yds to end zone</td>
                          </tr>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>Situation</td>
                            <td style={{ fontWeight: '600' }}>{selectedEval.down}&{selectedEval.ydstogo}</td>
                          </tr>
                        </tbody>
                      </table>

                      {/* KEP interpretation */}
                      <div style={{
                        padding: '10px 14px',
                        borderRadius: '6px',
                        background: selectedEval.kep >= 0 ? 'rgba(0,242,254,0.05)' : 'rgba(255,56,56,0.05)',
                        border: `1px solid ${selectedEval.kep >= 0 ? 'rgba(0,242,254,0.15)' : 'rgba(255,56,56,0.15)'}`,
                        fontSize: '12px',
                        color: 'var(--text-secondary)'
                      }}>
                        <strong style={{ color: selectedEval.kep >= 0 ? 'var(--accent-cyan)' : 'var(--accent-red)' }}>
                          {selectedEval.off}
                        </strong>{' '}
                        {selectedEval.kep >= 0
                          ? `holds a +${selectedEval.kep.toFixed(2)} KEP positional advantage — equivalent to receiving a kickoff up ${Math.abs(selectedEval.kep).toFixed(1)} pts.`
                          : `is at a ${selectedEval.kep.toFixed(2)} KEP positional deficit — equivalent to receiving a kickoff down ${Math.abs(selectedEval.kep).toFixed(1)} pts.`}
                      </div>

                      {/* Concept recommendation (requires live backend) */}
                      <div>
                        <div style={{ fontSize: '11px', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.05em', marginBottom: '8px', display: 'flex', justifyContent: 'space-between' }}>
                          <span>Play Concept Recommendation</span>
                          {conceptLoading && <span style={{ color: 'var(--accent-cyan)' }}>running sims…</span>}
                        </div>
                        {conceptResult?.concepts ? (() => {
                          const sorted = [...conceptResult.concepts].sort((a, b) => (b.delta_kep ?? -Infinity) - (a.delta_kep ?? -Infinity));
                          const maxAbs = sorted.reduce((m, c) => Math.max(m, Math.abs(c.delta_kep ?? 0)), 0.5);
                          return (
                            <div style={{ display: 'flex', flexDirection: 'column', gap: '7px' }}>
                              {sorted.map((c, i) => {
                                const dk = c.delta_kep;
                                const isNull = dk === null || c.n < 5;
                                const barPct = isNull ? 0 : Math.min(100, (Math.abs(dk) / maxAbs) * 100);
                                const barColor = isNull ? 'rgba(255,255,255,0.12)' : dk > 0 ? 'var(--accent-green)' : 'var(--accent-red)';
                                return (
                                  <div key={c.concept}>
                                    <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', marginBottom: '3px' }}>
                                      <span style={{ display: 'flex', gap: '5px', alignItems: 'center' }}>
                                        {i === 0 && !isNull && (
                                          <span style={{ padding: '1px 5px', borderRadius: '3px', background: 'rgba(0,245,212,0.14)', color: 'var(--accent-green)', fontSize: '9px', fontWeight: 800 }}>BEST</span>
                                        )}
                                        <span style={{ color: 'var(--text-primary)', fontWeight: 600 }}>{c.concept}</span>
                                        <span style={{ color: 'var(--text-muted)' }}>({c.n})</span>
                                      </span>
                                      <span style={{ fontFamily: 'monospace', color: barColor }}>
                                        {isNull ? 'N/A' : (dk > 0 ? `+${dk.toFixed(3)}` : dk.toFixed(3))}
                                      </span>
                                    </div>
                                    <div style={{ width: '100%', height: '4px', background: 'rgba(255,255,255,0.05)', borderRadius: '2px', overflow: 'hidden' }}>
                                      <div style={{ width: `${barPct}%`, height: '100%', background: barColor, transition: 'width 0.3s ease' }} />
                                    </div>
                                  </div>
                                );
                              })}
                            </div>
                          );
                        })() : !conceptLoading && (
                          <div style={{ fontSize: '11px', color: 'var(--text-muted)', fontStyle: 'italic' }}>
                            Start the backend to see play recommendations.
                          </div>
                        )}
                      </div>
                    </div>
                  ) : (
                    <div style={{ textAlign: 'center', padding: '40px', color: 'var(--text-muted)', fontSize: '12px' }}>
                      {chessChartData.length > 0
                        ? 'Click a point on the KEP chart or select a play from the Play Log to see positional detail.'
                        : 'No evaluations available for this game.'}
                    </div>
                  )}
                </div>
              </div>
            );
          })()}

        </div>
      )}
    </div>
  );
}

export default GameSummary;
