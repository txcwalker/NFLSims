import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import { Play, TrendingUp, Cpu, Award, Trophy, ArrowRight } from 'lucide-react';

function Home({ navigateTo }) {
  const [liveGames, setLiveGames] = useState([]);
  const [contenders, setContenders] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function loadData() {
      try {
        const [games, standings] = await Promise.all([
          ApiService.getLiveGames(),
          ApiService.getPlayoffOdds()
        ]);
        setLiveGames(games || []);
        // Sort standings to get top contenders for summary block
        const sortedContenders = (standings || []).sort((a, b) => b.playoff_pct - a.playoff_pct);
        setContenders(sortedContenders.slice(0, 5));
      } catch (err) {
        console.error('Failed to load home data', err);
      } finally {
        setLoading(false);
      }
    }
    loadData();
  }, []);

  return (
    <div>
      {/* Hero Strategic Overview */}
      <div className="panel" style={{ marginBottom: '24px', background: 'linear-gradient(135deg, var(--bg-secondary) 0%, rgba(0, 242, 254, 0.03) 100%)' }}>
        <h1 style={{ fontSize: '32px', marginBottom: '8px', color: 'var(--text-primary)' }}>TACTICAL COMMAND & STRATEGY CENTER</h1>
        <p style={{ color: 'var(--text-secondary)', fontSize: '14px', maxWidth: '800px' }}>
          Evaluate coaching aggressiveness, situation utilities, and live game dynamics driven by 10,000 parallel Monte Carlo simulation runs. Keep emotion out of the evaluation—track raw win probabilities and expected values.
        </p>
      </div>

      {loading ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>LOADING TACTICAL GRID STATE...</div>
      ) : (
        <div className="dashboard-grid">
          
          {/* Left Column: Live Matches & Tools */}
          <div>
            <h2 style={{ fontSize: '18px', marginBottom: '16px', textTransform: 'uppercase', letterSpacing: '0.05em' }}>Live Matchup Feeds</h2>
            <div className="games-grid" style={{ marginBottom: '24px' }}>
              {liveGames.map(game => (
                <div 
                  key={game.game_id} 
                  className="panel" 
                  style={{ display: 'flex', flexDirection: 'column', gap: '12px', cursor: 'pointer' }}
                  onClick={() => navigateTo('game-summary', { id: game.game_id })}
                >
                  <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '12px', color: 'var(--text-muted)' }}>
                    <span>Q{game.quarter} | {game.time_remaining}</span>
                    <span className="badge badge-cyan" style={{ fontSize: '9px' }}>{game.leverage} Leverage</span>
                  </div>
                  
                  <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '8px', fontWeight: '600' }}>
                        <span style={{ color: game.possession === game.away_team ? 'var(--accent-cyan)' : 'inherit' }}>
                          {game.away_team}
                        </span>
                        <span style={{ fontSize: '18px' }}>{game.away_score}</span>
                      </div>
                      <div style={{ display: 'flex', alignItems: 'center', gap: '8px', fontWeight: '600' }}>
                        <span style={{ color: game.possession === game.home_team ? 'var(--accent-cyan)' : 'inherit' }}>
                          {game.home_team}
                        </span>
                        <span style={{ fontSize: '18px' }}>{game.home_score}</span>
                      </div>
                    </div>
                    
                    <div style={{ textAlign: 'right', display: 'flex', flexDirection: 'column', gap: '4px' }}>
                      <div style={{ fontSize: '10px', color: 'var(--text-muted)', textTransform: 'uppercase' }}>Win Probabilities</div>
                      <div style={{ color: 'var(--accent-cyan)', fontWeight: '700', fontSize: '14px' }}>
                        {game.away_team} {game.away_wp}%
                      </div>
                      <div style={{ color: 'var(--text-secondary)', fontSize: '12px' }}>
                        {game.home_team} {game.home_wp}%
                      </div>
                    </div>
                  </div>

                  <div style={{ height: '4px', backgroundColor: 'var(--bg-tertiary)', borderRadius: '2px', overflow: 'hidden', display: 'flex' }}>
                    <div style={{ width: `${game.away_wp}%`, backgroundColor: 'var(--accent-cyan)', height: '100%' }}></div>
                    <div style={{ width: `${game.home_wp}%`, backgroundColor: 'var(--border-color)', height: '100%' }}></div>
                  </div>

                  {game.down && (
                    <div style={{ fontSize: '12px', color: 'var(--text-secondary)', backgroundColor: 'var(--bg-tertiary)', padding: '6px 8px', borderRadius: '4px', textAlign: 'center' }}>
                      Active state: <strong style={{ color: 'var(--accent-orange)' }}>{game.down}nd & {game.distance}</strong> at {game.yardline}
                    </div>
                  )}

                  <div style={{ display: 'flex', gap: '8px', marginTop: '4px' }} onClick={(e) => e.stopPropagation()}>
                    <button className="btn btn-outline" style={{ flex: 1, fontSize: '11px', padding: '6px' }} onClick={() => navigateTo('live-wp')}>
                      <TrendingUp size={12} /> WP Graph
                    </button>
                    <button className="btn btn-cyan" style={{ flex: 1, fontSize: '11px', padding: '6px' }} onClick={() => navigateTo('game-summary', { id: game.game_id })}>
                      <Play size={12} /> Analyze Center
                    </button>
                  </div>
                </div>
              ))}
            </div>

            {/* Quick Strategic Actions */}
            <h2 style={{ fontSize: '18px', marginBottom: '16px', textTransform: 'uppercase', letterSpacing: '0.05em' }}>Command Tools</h2>
            <div className="games-grid">
              <div className="panel" style={{ cursor: 'pointer' }} onClick={() => navigateTo('live-wp')}>
                <div style={{ color: 'var(--accent-cyan)', marginBottom: '12px' }}><TrendingUp size={24} /></div>
                <h3 style={{ fontSize: '16px', marginBottom: '6px' }}>Live Win Probability</h3>
                <p style={{ fontSize: '12px', color: 'var(--text-secondary)' }}>
                  Monitor ongoing matchups with real-time probability charts and expected points analytics.
                </p>
              </div>
              <div className="panel" style={{ cursor: 'pointer' }} onClick={() => navigateTo('fourth-downs')}>
                <div style={{ color: 'var(--accent-orange)', marginBottom: '12px' }}><Cpu size={24} /></div>
                <h3 style={{ fontSize: '16px', marginBottom: '6px' }}>4th Down Decision Explorer</h3>
                <p style={{ fontSize: '12px', color: 'var(--text-secondary)' }}>
                  Evaluate actual live game calls or input your own parameters in the situational strategy sandbox.
                </p>
              </div>
            </div>
          </div>

          {/* Right Column: Top Contenders & Model Status */}
          <div>
            <h2 style={{ fontSize: '18px', marginBottom: '16px', textTransform: 'uppercase', letterSpacing: '0.05em' }}>Top Contenders Summary</h2>
            <div className="panel" style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>
              <div style={{ display: 'flex', alignItems: 'center', gap: '8px', color: 'var(--accent-cyan)', fontWeight: '600', fontSize: '14px', borderBottom: '1px solid var(--border-color)', paddingBottom: '10px' }}>
                <Trophy size={16} /> Projected Playoff Leaders
              </div>
              
              <div style={{ display: 'flex', flexDirection: 'column', gap: '12px' }}>
                {contenders.map(row => (
                  <div key={row.team} style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', fontSize: '13px' }}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                      <span style={{ fontWeight: '700', fontSize: '14px' }}>{row.team}</span>
                      <span style={{ color: 'var(--text-muted)' }}>Wins: {row.wins.toFixed(1)}</span>
                    </div>
                    <div style={{ display: 'flex', gap: '8px' }}>
                      <span className="badge badge-cyan">Playoffs: {row.playoff_pct}%</span>
                      <span className="badge badge-orange">SB: {row.super_bowl_pct}%</span>
                    </div>
                  </div>
                ))}
              </div>

              <button 
                className="btn btn-outline" 
                style={{ width: '100%', fontSize: '12px', marginTop: '8px' }}
                onClick={() => navigateTo('standings')}
              >
                View Full 32-Team Standings <ArrowRight size={14} style={{ marginLeft: '4px' }} />
              </button>
            </div>

            <div className="panel" style={{ marginTop: '24px', backgroundColor: 'var(--bg-tertiary)' }}>
              <div className="panel-header" style={{ marginBottom: '8px', paddingBottom: '8px' }}>
                <span className="panel-title" style={{ fontSize: '12px' }}><Award size={14} /> Analytics Status</span>
              </div>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '8px', fontSize: '12px' }}>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}>
                  <span style={{ color: 'var(--text-secondary)' }}>Win Probability Model:</span>
                  <span style={{ color: 'var(--accent-green)' }}>ACTIVE</span>
                </div>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}>
                  <span style={{ color: 'var(--text-secondary)' }}>4th Down Mono-XGBoost:</span>
                  <span style={{ color: 'var(--accent-green)' }}>ACTIVE</span>
                </div>
                <div style={{ display: 'flex', justifyContent: 'space-between' }}>
                  <span style={{ color: 'var(--text-secondary)' }}>Playoff Sim Iterations:</span>
                  <span style={{ color: 'var(--accent-cyan)' }}>10,000 parallel</span>
                </div>
              </div>
            </div>
          </div>

        </div>
      )}
    </div>
  );
}

export default Home;
