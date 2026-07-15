import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine } from 'recharts';
import { Compass, Info, Award } from 'lucide-react';

function LiveWP() {
  const [games, setGames] = useState([]);
  const [selectedGameId, setSelectedGameId] = useState('');
  const [playByPlay, setPlayByPlay] = useState([]);
  const [selectedPlayId, setSelectedPlayId] = useState(null);
  const [loading, setLoading] = useState(true);

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
    if (!selectedGameId) return;
    async function loadPlayByPlay() {
      setLoading(true);
      try {
        const plays = await ApiService.getPlayByPlay(selectedGameId);
        setPlayByPlay(plays || []);
        if (plays && plays.length > 0) {
          setSelectedPlayId(plays[plays.length - 1].play_id);
        }
      } catch (err) {
        console.error('Failed to load plays', err);
      } finally {
        setLoading(false);
      }
    }
    loadPlayByPlay();
  }, [selectedGameId]);

  const selectedGame = games.find(g => g.game_id === selectedGameId);
  const selectedPlay = playByPlay.find(p => p.play_id === selectedPlayId);

  // Map play-by-play data to Recharts format
  // Recharts needs a continuous timeline
  const chartData = playByPlay.map((play, index) => ({
    name: `P${index + 1}`,
    time: play.time,
    qtr: play.qtr,
    [selectedGame?.home_team || 'Home']: play.home_wp,
    [selectedGame?.away_team || 'Away']: play.away_wp,
    play_id: play.play_id,
    desc: play.desc
  }));

  return (
    <div>
      {/* Page Header */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '20px' }}>
        <div>
          <h1 style={{ fontSize: '24px' }}>Live Win Probability Tracker</h1>
          <p style={{ color: 'var(--text-secondary)', fontSize: '13px' }}>Monitor live game state and track expected shifts in win expectancy play-by-play.</p>
        </div>
        <div>
          <select 
            className="form-select" 
            value={selectedGameId} 
            onChange={(e) => setSelectedGameId(e.target.value)}
            style={{ width: '220px', fontWeight: '600' }}
          >
            {games.map(g => (
              <option key={g.game_id} value={g.game_id}>{g.away_team} @ {g.home_team} (Q{g.quarter})</option>
            ))}
          </select>
        </div>
      </div>

      {loading ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>SYNCING WITH LIVE PLAY-BY-PLAY DATAFEED...</div>
      ) : (
        <div className="live-wp-workspace">
          
          {/* Left Panel: Line Chart */}
          <div className="panel" style={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
            <div className="panel-header">
              <span className="panel-title"><Compass size={16} /> Probability Timeline</span>
              {selectedGame && (
                <div style={{ fontSize: '12px', fontWeight: '600', color: 'var(--accent-cyan)' }}>
                  {selectedGame.away_team} {selectedGame.away_score} - {selectedGame.home_score} {selectedGame.home_team}
                </div>
              )}
            </div>

            <div style={{ flex: 1, minHeight: '300px', width: '100%', marginTop: '12px' }}>
              <ResponsiveContainer width="100%" height="100%">
                <LineChart
                  data={chartData}
                  margin={{ top: 10, right: 10, left: -20, bottom: 0 }}
                  onClick={(state) => {
                    if (state && state.activePayload && state.activePayload.length > 0) {
                      setSelectedPlayId(state.activePayload[0].payload.play_id);
                    }
                  }}
                >
                  <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                  <XAxis dataKey="name" stroke="var(--text-muted)" fontSize={11} />
                  <YAxis domain={[0, 100]} stroke="var(--text-muted)" fontSize={11} />
                  <Tooltip 
                    contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)', color: 'var(--text-primary)' }}
                    labelStyle={{ color: 'var(--accent-cyan)', fontWeight: '600' }}
                  />
                  <ReferenceLine y={50} stroke="var(--text-muted)" strokeDasharray="3 3" />
                  
                  {selectedGame && (
                    <>
                      <Line 
                        type="monotone" 
                        dataKey={selectedGame.away_team} 
                        stroke="var(--accent-cyan)" 
                        strokeWidth={2}
                        dot={{ r: 3, fill: 'var(--bg-primary)' }}
                        activeDot={{ r: 6 }} 
                      />
                      <Line 
                        type="monotone" 
                        dataKey={selectedGame.home_team} 
                        stroke="var(--text-secondary)" 
                        strokeWidth={1.5}
                        strokeDasharray="4 4"
                        dot={{ r: 2 }}
                      />
                    </>
                  )}
                </LineChart>
              </ResponsiveContainer>
            </div>

            {/* Selected Play Context Details */}
            {selectedPlay && (
              <div style={{ marginTop: '16px', borderTop: '1px solid var(--border-color)', paddingTop: '16px', fontSize: '13px' }}>
                <div style={{ display: 'flex', gap: '12px', alignItems: 'center', marginBottom: '8px' }}>
                  <span className="badge badge-cyan">Q{selectedPlay.qtr} | {selectedPlay.time}</span>
                  <span className="badge badge-orange" style={{ textTransform: 'uppercase' }}>
                    Leverage: {selectedPlay.leverage.toFixed(1)}x
                  </span>
                </div>
                <div style={{ color: 'var(--text-primary)', fontWeight: '500', marginBottom: '8px' }}>
                  {selectedPlay.desc}
                </div>
                <div style={{ display: 'flex', gap: '24px', color: 'var(--text-secondary)' }}>
                  <div>{selectedGame?.away_team} WP: <strong style={{ color: 'var(--accent-cyan)' }}>{selectedPlay.away_wp}%</strong></div>
                  <div>{selectedGame?.home_team} WP: <strong>{selectedPlay.home_wp}%</strong></div>
                </div>
              </div>
            )}
          </div>

          {/* Right Panel: Play Stream */}
          <div className="panel feed-container" style={{ height: '100%' }}>
            <div className="panel-header">
              <span className="panel-title"><Info size={16} /> Live Play Stream</span>
              <span style={{ fontSize: '11px', color: 'var(--text-muted)' }}>Click play card to highlight on chart</span>
            </div>

            <div style={{ marginTop: '12px' }}>
              {playByPlay.map((play) => (
                <div 
                  key={play.play_id} 
                  className={`play-card ${selectedPlayId === play.play_id ? 'active' : ''}`}
                  onClick={() => setSelectedPlayId(play.play_id)}
                >
                  <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', color: 'var(--text-muted)', marginBottom: '6px' }}>
                    <span>Q{play.qtr} - {play.time}</span>
                    <span style={{ color: play.possession === selectedGame?.away_team ? 'var(--accent-cyan)' : 'inherit', fontWeight: '700' }}>
                      {play.possession} Ball
                    </span>
                  </div>
                  <div style={{ fontSize: '13px', lineHeight: '1.4', marginBottom: '8px' }}>
                    {play.desc}
                  </div>
                  <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', fontSize: '11px' }}>
                    <div style={{ color: 'var(--text-secondary)' }}>
                      Win Expectancy: <strong style={{ color: 'var(--accent-cyan)' }}>{play.away_wp}%</strong>
                    </div>
                    {play.leverage > 2.0 && (
                      <span className="badge badge-orange" style={{ padding: '1px 5px', fontSize: '9px' }}>Critical</span>
                    )}
                  </div>
                </div>
              ))}
            </div>
          </div>

        </div>
      )}
    </div>
  );
}

export default LiveWP;
