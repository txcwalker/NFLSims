import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import { Play, TrendingUp, Cpu, Award, Trophy, ArrowRight } from 'lucide-react';

// Formats a Date as ESPN's YYYYMMDD scoreboard param.
function toEspnDate(d) {
  const yyyy = d.getFullYear();
  const mm = String(d.getMonth() + 1).padStart(2, '0');
  const dd = String(d.getDate()).padStart(2, '0');
  return `${yyyy}${mm}${dd}`;
}

// Reverse of toEspnDate, for display -- "20260818" -> "Aug 18, 2026".
function formatEspnDate(yyyymmdd) {
  const y = yyyymmdd.slice(0, 4), m = yyyymmdd.slice(4, 6), d = yyyymmdd.slice(6, 8);
  const date = new Date(`${y}-${m}-${d}T12:00:00`);
  return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
}

function Home({ navigateTo }) {
  const [liveGames, setLiveGames] = useState([]);
  const [contenders, setContenders] = useState([]);
  const [loading, setLoading] = useState(true);
  const [gamesLoading, setGamesLoading] = useState(false);
  // '' = today (live default). Otherwise an ESPN YYYYMMDD string, driven by
  // the date picker below -- lets us review a past day's slate (e.g. to
  // troubleshoot the live bot against yesterday's real games) without
  // waiting for the next live window.
  const [gameDate, setGameDate] = useState('');

  useEffect(() => {
    async function loadStandings() {
      try {
        const standings = await ApiService.getSeason2026Standings();
        const sortedContenders = (standings || []).sort((a, b) => b['Playoffs_%'] - a['Playoffs_%']);
        setContenders(sortedContenders.slice(0, 5));
      } catch (err) {
        console.error('Failed to load standings', err);
      } finally {
        setLoading(false);
      }
    }
    loadStandings();
  }, []);

  useEffect(() => {
    async function loadGames() {
      setGamesLoading(true);
      try {
        const games = await ApiService.getLiveGames(gameDate || undefined);
        setLiveGames(games || []);
      } catch (err) {
        console.error('Failed to load live games', err);
      } finally {
        setGamesLoading(false);
      }
    }
    loadGames();
  }, [gameDate]);

  const yesterdayEspn = toEspnDate(new Date(Date.now() - 24 * 60 * 60 * 1000));
  // The backend tags every game with is_fallback/fallback_date when today (or
  // the requested date) had no games and it walked back to the most recent
  // real slate instead -- surface that plainly so these never read as live.
  const isFallback = !gameDate && liveGames.length > 0 && liveGames[0].is_fallback;
  const fallbackDate = isFallback ? liveGames[0].fallback_date : null;

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
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '16px', flexWrap: 'wrap', gap: '8px' }}>
              <h2 style={{ fontSize: '18px', textTransform: 'uppercase', letterSpacing: '0.05em', margin: 0 }}>
                {isFallback ? 'Most Recent Games' : 'Live Matchup Feeds'}{' '}
                {gameDate && <span style={{ color: 'var(--text-muted)', fontSize: '12px', textTransform: 'none', letterSpacing: 'normal' }}>({gameDate})</span>}
                {isFallback && <span style={{ color: 'var(--text-muted)', fontSize: '12px', textTransform: 'none', letterSpacing: 'normal' }}>(no games today — showing {formatEspnDate(fallbackDate)})</span>}
              </h2>
              <div style={{ display: 'flex', gap: '6px', alignItems: 'center' }}>
                <button
                  className={`btn ${!gameDate ? 'btn-cyan' : 'btn-outline'}`}
                  style={{ fontSize: '11px', padding: '5px 10px' }}
                  onClick={() => setGameDate('')}
                >
                  Today
                </button>
                <button
                  className={`btn ${gameDate === yesterdayEspn ? 'btn-cyan' : 'btn-outline'}`}
                  style={{ fontSize: '11px', padding: '5px 10px' }}
                  onClick={() => setGameDate(yesterdayEspn)}
                >
                  Yesterday
                </button>
                <input
                  type="date"
                  style={{ fontSize: '11px', padding: '4px 6px', backgroundColor: 'var(--bg-tertiary)', color: 'var(--text-primary)', border: '1px solid var(--border-color)', borderRadius: '4px' }}
                  onChange={(e) => {
                    if (!e.target.value) return;
                    const [y, m, d] = e.target.value.split('-');
                    setGameDate(`${y}${m}${d}`);
                  }}
                />
              </div>
            </div>
            <div className="games-grid" style={{ marginBottom: '24px' }}>
              {gamesLoading && <div style={{ color: 'var(--text-muted)', fontSize: '13px' }}>Loading games...</div>}
              {!gamesLoading && liveGames.length === 0 && (
                <div style={{ color: 'var(--text-muted)', fontSize: '13px' }}>No games found for this date.</div>
              )}
              {liveGames.map(game => (
                <div 
                  key={game.game_id} 
                  className="panel" 
                  style={{ display: 'flex', flexDirection: 'column', gap: '12px', cursor: 'pointer' }}
                  onClick={() => navigateTo('game-summary', { id: game.game_id, date: gameDate || fallbackDate || '' })}
                >
                  <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '12px', color: 'var(--text-muted)' }}>
                    <span>{game.quarter ? `Q${game.quarter} | ` : ''}{game.time_remaining}</span>
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
                      Active state: <strong style={{ color: 'var(--accent-orange)' }}>{game.down}{game.down === 1 ? 'st' : game.down === 2 ? 'nd' : game.down === 3 ? 'rd' : 'th'} & {game.distance}</strong> at {game.yardline}
                    </div>
                  )}

                  <div style={{ display: 'flex', gap: '8px', marginTop: '4px' }} onClick={(e) => e.stopPropagation()}>
                    <button className="btn btn-outline" style={{ flex: 1, fontSize: '11px', padding: '6px' }} onClick={() => navigateTo('live-wp')}>
                      <TrendingUp size={12} /> WP Graph
                    </button>
                    <button className="btn btn-cyan" style={{ flex: 1, fontSize: '11px', padding: '6px' }} onClick={() => navigateTo('game-summary', { id: game.game_id, date: gameDate || fallbackDate || '' })}>
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
                  <div key={row.Team} style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', fontSize: '13px' }}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                      <span style={{ fontWeight: '700', fontSize: '14px' }}>{row.Team}</span>
                      <span style={{ color: 'var(--text-muted)' }}>Wins: {row.Wins_Expected.toFixed(1)}</span>
                    </div>
                    <div style={{ display: 'flex', gap: '8px' }}>
                      <span className="badge badge-cyan">Playoffs: {row['Playoffs_%'].toFixed(0)}%</span>
                      <span className="badge badge-orange">SB: {row['Champion_%'].toFixed(1)}%</span>
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
