import { useState, useEffect, useLayoutEffect, useMemo, useRef, Fragment } from 'react';
import { DFS_RULES, validateLineup } from '../dfsRules';
import { ApiService } from '../api';
import { SandboxBadge } from '../components/SandboxBadge';

const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB: '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC: '#E31837',
  LV: '#000000', LAC: '#0080C6', LAR: '#003594', MIA: '#008E97',
  MIN: '#4F2683', NE: '#002244', NO: '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SF: '#AA0000',
  SEA: '#002244', TB: '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414'
};

function normalCDF(x) {
  const t = 1 / (1 + 0.2316419 * Math.abs(x));
  const d = 0.3989423 * Math.exp(-x * x / 2);
  const p = d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
  return x > 0 ? 1 - p : p;
}

export default function Simulator({ 
  slateOverrides, 
  setSlateOverrides,
  selectedSlate,
  setSelectedSlate,
  weeks,
  setWeeks,
  selectedWeek,
  setSelectedWeek,
  games,
  setGames,
  weekProjections,
  setWeekProjections,
  selectedGame,
  setSelectedGame,
  hasSimResults,
  setHasSimResults,
  allSimResults,
  setAllSimResults,
  simResults,
  setSimResults,
  generatedLineups,
  setGeneratedLineups,
  setCurrentPage
}) {
  // --- SLATE & MATCHUP STATE ---
  const [slateFilter, setSlateFilter] = useState('ALL'); // 'ALL', 'DK_MAIN', 'FD_MAIN'
  const [isSandbox, setIsSandbox] = useState(false);

  // --- SHARED SIMULATION TRIGGER STATES ---
  const [iterations, setIterations] = useState(5000);
  const [simTriggerSignal, setSimTriggerSignal] = useState(0);
  const [isSimulating, setIsSimulating] = useState(false);
  const [simProgress, setSimProgress] = useState('');
  const [childWorkspaceView, setChildWorkspaceView] = useState('summary');

  const runAllSimulations = async () => {
    setIsSimulating(true);
    setSimProgress('Initializing full slate simulation...');
    
    try {
      const simPromises = games.map(async (g) => {
        // Games with no saved user overrides already have baseline results
        // prepopulated from the parquet cache (see App.jsx's getWeekSimResults
        // call) — reuse them instead of re-running /api/simulate.
        const gameHasOverrides = !!slateOverrides?.[g.game_id];
        const prepopulated = allSimResults?.[g.game_id];
        if (!gameHasOverrides && prepopulated) {
          return prepopulated;
        }

        // 1. Get baseline rosters and settings
        const rosterData = await ApiService.getRosters(g.away_team, g.home_team);
        
        // 2. Get overrides from slateOverrides if present, otherwise use defaults
        const gameOverrides = slateOverrides?.[g.game_id] || {};
        
        const activeTeamOverrides = {};
        [g.away_team, g.home_team].forEach(team => {
          activeTeamOverrides[team] = gameOverrides.teamOverrides?.[team] || { ...rosterData[team].team_settings };
        });
        
        const activePlayerOverrides = [];
        [g.away_team, g.home_team].forEach(team => {
          const defaultRoster = rosterData[team].roster;
          defaultRoster.forEach(p => {
            const savedPlayer = gameOverrides.playerOverrides?.find(x => x.name === p.name);
            if (savedPlayer) {
              activePlayerOverrides.push(savedPlayer);
            } else {
              activePlayerOverrides.push({
                name: p.name,
                pos: p.pos,
                team: team,
                target_share: p.target_share,
                carry_share: p.carry_share,
                catch_rate: p.catch_rate,
                rush_td_share: p.rush_td_share || 0.0,
                rec_td_share: p.rec_td_share || 0.0,
                salary: p.salary,
                ownership_proj: 12.5,
                ownership_leverage: 0.0
              });
            }
          });
        });
        
        // Offset logic
        const spreadOffset = gameOverrides.spreadOffset !== undefined ? gameOverrides.spreadOffset : 0;
        const totalOffset = gameOverrides.totalOffset !== undefined ? gameOverrides.totalOffset : 0;
        
        const baseSpread = -g.spread_line;
        const baseTotal = g.total_line;
        
        const payload = {
          away_team: g.away_team,
          home_team: g.home_team,
          year: 2025,
          iterations: iterations,
          spread_override: -(baseSpread + spreadOffset),
          total_override: baseTotal + totalOffset,
          apply_weighting: true, // Apply weighting for slate games
          team_overrides: activeTeamOverrides,
          player_overrides: activePlayerOverrides
        };
        
        const simRes = await ApiService.runSimulation(payload);
        
        // Format the results to include opponent and DFS metadata
        const updatedProjections = simRes.projections.map(p => {
          const matchedPlayer = activePlayerOverrides.find(x => x.name === p.name);
          const own = matchedPlayer ? matchedPlayer.ownership_proj : 12.5;
          const leverage = own > 0 ? (p.dk_points / own) : 0.0;
          const opp = p.team === g.away_team ? `vs ${g.home_team}` : `@${g.away_team}`;
          return {
            ...p,
            floor_dk_points: parseFloat((p.dk_points * 0.45).toFixed(1)),
            ceiling_dk_points: parseFloat((p.dk_points * 1.65).toFixed(1)),
            ownership_proj: own,
            ownership_leverage: parseFloat(leverage.toFixed(2)),
            opponent: opp
          };
        });
        
        return {
          game_id: g.game_id,
          away_team: g.away_team,
          home_team: g.home_team,
          summary: simRes.summary,
          projections: updatedProjections
        };
      });
      
      setSimProgress('Running simulations in parallel across all games...');
      const resultsArray = await Promise.all(simPromises);
      
      const newAllSimResults = {};
      resultsArray.forEach(r => {
        newAllSimResults[r.game_id] = r;
      });
      
      // Store all results
      setAllSimResults(newAllSimResults);
      setHasSimResults(true);
      
      // Navigate to DFS Summary page
      setCurrentPage('dfs_summary');
      window.location.hash = 'dfs_summary';
      
      setSimProgress('');
      setIsSimulating(false);
      
    } catch (err) {
      console.error("Error running all simulations:", err);
      setSimProgress('Simulation failed.');
      setIsSimulating(false);
    }
  };

  // Sync initial state from URL on mount
  useEffect(() => {
    const hash = window.location.hash.replace('#', '');
    const parts = hash.split('?');
    const params = new URLSearchParams(parts[1] || '');
    const urlSlate = params.get('slate');
    const urlWeek = params.get('week');

    if (urlSlate) {
      setSelectedSlate(urlSlate.toUpperCase());
    }
    if (urlWeek) {
      setSelectedWeek(parseInt(urlWeek));
    }
  }, []);

  // Set default active game when games array changes (fetched globally by App.jsx)
  useEffect(() => {
    const hash = window.location.hash.replace('#', '');
    const parts = hash.split('?');
    const params = new URLSearchParams(parts[1] || '');
    const urlGame = params.get('game');
    
    if (games && games.length > 0) {
      const matchedGame = urlGame ? games.find(g => g.game_id === urlGame) : null;
      setSelectedGame(matchedGame || games[0]);
    } else {
      setSelectedGame(null);
    }
    setIsSandbox(ApiService.isSandbox());
  }, [games]);

  // Sync sandbox state when projections change
  useEffect(() => {
    setIsSandbox(ApiService.isSandbox());
  }, [weekProjections]);

  // Synchronize state changes to URL query string
  useEffect(() => {
    if (!selectedGame) return;
    const params = new URLSearchParams();
    params.set('slate', selectedSlate.toLowerCase());
    params.set('week', selectedWeek.toString());
    params.set('game', selectedGame.game_id);
    window.history.replaceState(null, '', `#simulator?${params.toString()}`);
  }, [selectedSlate, selectedWeek, selectedGame]);

  // Handle URL change events (e.g. forward/back buttons)
  useEffect(() => {
    const handleHashChange = () => {
      const hash = window.location.hash.replace('#', '');
      const parts = hash.split('?');
      const params = new URLSearchParams(parts[1] || '');
      
      const urlSlate = params.get('slate');
      const urlWeek = params.get('week');
      const urlGame = params.get('game');
      
      if (urlSlate && urlSlate.toUpperCase() !== selectedSlate) {
        setSelectedSlate(urlSlate.toUpperCase());
      }
      if (urlWeek && parseInt(urlWeek) !== selectedWeek) {
        setSelectedWeek(parseInt(urlWeek));
      }
      if (urlGame && selectedGame && urlGame !== selectedGame.game_id) {
        const matchedGame = games.find(g => g.game_id === urlGame);
        if (matchedGame) {
          setSelectedGame(matchedGame);
        }
      }
    };
    
    window.addEventListener('hashchange', handleHashChange);
    return () => window.removeEventListener('hashchange', handleHashChange);
  }, [selectedSlate, selectedWeek, selectedGame, games]);

  const filteredGames = games.filter(g => {
    if (slateFilter === 'DK_MAIN') return g.dk_main;
    if (slateFilter === 'FD_MAIN') return g.fd_main;
    return true;
  });

  return (
    <div style={{ flexGrow: 1, paddingBottom: '30px' }}>
      {/* LOADING OVERLAY SCREEN */}
      {isSimulating && (
        <div className="loading-overlay">
          <div className="spinner"></div>
          <div className="loading-text">{simProgress}</div>
        </div>
      )}

      {/* DASHBOARD CONTROL BAR */}
      <div style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        marginBottom: '20px',
        background: 'var(--bg-glass)',
        padding: '12px 20px',
        borderRadius: '12px',
        border: '1px solid var(--border-glass)',
        flexWrap: 'wrap',
        gap: '15px'
      }}>
        {/* Slate selections */}
        <div className="flex-center" style={{ gap: '15px' }}>
          <div className="flex-center">
            <span style={{fontWeight: 700, color: 'var(--text-white)', marginRight: '10px'}}>Slate Focus:</span>
            <div className="slate-selector">
              <button className={`slate-btn ${selectedSlate === 'TRADITIONAL' ? 'active' : ''}`} onClick={() => setSelectedSlate('TRADITIONAL')}>Traditional Multi-Game</button>
              <button className={`slate-btn ${selectedSlate === 'SHOWDOWN' ? 'active' : ''}`} onClick={() => setSelectedSlate('SHOWDOWN')}>Showdown Single-Game</button>
            </div>
          </div>

          {selectedSlate === 'TRADITIONAL' && (hasSimResults || weekProjections.length > 0) && (
            <button
              className="btn-primary"
              onClick={() => {
                setCurrentPage('dfs_summary');
                window.location.hash = 'dfs_summary';
              }}
              style={{
                fontSize: '0.85rem',
                padding: '6px 12px',
                background: 'rgba(255,255,255,0.03)',
                color: '#fff',
                border: '1px solid var(--border-glass)',
                borderRadius: '6px',
                cursor: 'pointer',
                fontWeight: 600,
                display: 'flex',
                alignItems: 'center',
                gap: '6px'
              }}
            >
              📊 Slate DFS Summary
            </button>
          )}
        </div>

        {selectedSlate === 'SHOWDOWN' ? (
          <div className="flex-center">
            <span style={{color: 'var(--text-muted)', marginRight: '10px'}}>Active Matchup:</span>
            <select 
              value={selectedGame?.game_id || ''} 
              onChange={(e) => setSelectedGame(games.find(g => g.game_id === e.target.value))}
              style={{
                background: 'rgba(11, 17, 38, 0.6)',
                border: '1px solid var(--border-glass)',
                color: 'var(--text-white)',
                borderRadius: '8px',
                padding: '6px 12px',
                fontWeight: 600,
                outline: 'none',
                cursor: 'pointer'
              }}
            >
              {games.map(g => (
                <option key={g.game_id} value={g.game_id}>{g.away_team} @ {g.home_team}</option>
              ))}
            </select>
          </div>
        ) : (
          <div className="flex-center">
            <span style={{fontWeight: 600, color: 'var(--text-white)', marginRight: '10px'}}>Week:</span>
            <select 
              value={selectedWeek} 
              onChange={(e) => setSelectedWeek(parseInt(e.target.value))}
              style={{
                background: 'rgba(11, 17, 38, 0.6)',
                border: '1px solid var(--border-glass)',
                color: 'var(--text-white)',
                borderRadius: '8px',
                padding: '6px 12px',
                fontWeight: 600,
                outline: 'none',
                cursor: 'pointer'
              }}
            >
              {weeks.map(w => <option key={w} value={w}>{w}</option>)}
            </select>
          </div>
        )}

        {selectedSlate === 'SHOWDOWN' && (
          <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
            <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'flex-end' }}>
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>Sim Iterations</span>
              <select 
                value={iterations} 
                onChange={(e) => setIterations(parseInt(e.target.value))} 
                style={{ background: 'rgba(0,0,0,0.3)', border: '1px solid var(--border-glass)', color: '#fff', padding: '3px 8px', borderRadius: '4px', fontSize: '0.8rem', outline: 'none' }}
              >
                <option value={1000}>1,000 runs</option>
                <option value={5000}>5,000 runs</option>
                <option value={10000}>10,000 runs</option>
              </select>
            </div>
            <button 
              className="btn-primary" 
              onClick={() => setSimTriggerSignal(prev => prev + 1)} 
              disabled={isSimulating}
              style={{ padding: '8px 16px', fontSize: '0.9rem', fontWeight: '700', width: 'auto', whiteSpace: 'nowrap' }}
            >
              {isSimulating ? 'Simulating...' : `Run Engine`}
            </button>
          </div>
        )}
      </div>

      {/* SCHEDULE SCROLL BAR & RUN ENGINE INLINE */}
      {selectedSlate === 'TRADITIONAL' && (
        <div style={{ display: 'flex', gap: '15px', alignItems: 'center', marginBottom: '20px' }}>
          <div className="schedule-scroll-container" style={{ flex: 1, marginBottom: 0 }}>
            <div className="schedule-bar">
              {filteredGames.map(g => (
                <div 
                  key={g.game_id} 
                  className={`game-card ${selectedGame?.game_id === g.game_id ? 'active' : ''}`}
                  onClick={() => {
                    setSelectedGame(g);
                    if (childWorkspaceView === 'dfs_summary') {
                      setChildWorkspaceView('workloads');
                    }
                  }}
                >
                  <div className="game-card-teams">
                    <span>{g.away_team}</span>
                    <span>@</span>
                    <span>{g.home_team}</span>
                  </div>
                  <div className="game-card-line">
                    <span>Line: {(-g.spread_line) > 0 ? `+${-g.spread_line}` : -g.spread_line}</span>
                    <span>O/U: {g.total_line}</span>
                  </div>
                </div>
              ))}
            </div>
          </div>

          <div className="glass-panel" style={{ display: 'flex', alignItems: 'center', gap: '12px', padding: '8px 16px', margin: 0, height: '66px', justifyContent: 'center' }}>
            <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'flex-end' }}>
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>Sim Iterations</span>
              <select 
                value={iterations} 
                onChange={(e) => setIterations(parseInt(e.target.value))} 
                style={{ background: 'rgba(0,0,0,0.3)', border: '1px solid var(--border-glass)', color: '#fff', padding: '3px 8px', borderRadius: '4px', fontSize: '0.8rem', outline: 'none' }}
              >
                <option value={1000}>1,000 runs</option>
                <option value={5000}>5,000 runs</option>
                <option value={10000}>10,000 runs</option>
              </select>
            </div>
            <button 
              className="btn-primary" 
              onClick={() => {
                if (selectedSlate === 'TRADITIONAL') {
                  runAllSimulations();
                } else {
                  setSimTriggerSignal(prev => prev + 1);
                }
              }} 
              disabled={isSimulating}
              style={{ padding: '8px 16px', fontSize: '0.9rem', fontWeight: '700', width: 'auto', whiteSpace: 'nowrap' }}
            >
              {isSimulating ? 'Simulating...' : `Run Engine`}
            </button>
          </div>
        </div>
      )}

      {selectedGame ? (
        <GameSimulatorWorkspace
          key={selectedGame.game_id}
          selectedGame={selectedGame}
          selectedSlate={selectedSlate}
          weekProjections={weekProjections}
          iterations={iterations}
          simTriggerSignal={simTriggerSignal}
          isSimulating={isSimulating}
          setIsSimulating={setIsSimulating}
          simProgress={simProgress}
          setSimProgress={setSimProgress}
          onSimResultsLoaded={(results) => {
            setHasSimResults(!!results);
            if (results && selectedGame) {
              setAllSimResults(prev => ({
                ...prev,
                [selectedGame.game_id]: results
              }));
            } else if (!results && selectedGame) {
              setAllSimResults(prev => {
                const updated = { ...prev };
                delete updated[selectedGame.game_id];
                return updated;
              });
            }
          }}
          workspaceView={childWorkspaceView}
          setWorkspaceView={setChildWorkspaceView}
          slateOverrides={slateOverrides}
          setSlateOverrides={setSlateOverrides}
          allSimResults={allSimResults}
          simResults={simResults}
          setSimResults={setSimResults}
          generatedLineups={generatedLineups}
          setGeneratedLineups={setGeneratedLineups}
        />
      ) : (
        <div style={{ textAlign: 'center', padding: '40px' }}>Select week/slate matchups to configure Monte Carlo workloads.</div>
      )}
    </div>
  );
}

// ==========================================
// GAME SIMULATOR WORKSPACE (CHILD COMPONENT)
// ==========================================
function GameSimulatorWorkspace({
  selectedGame,
  selectedSlate,
  weekProjections,
  iterations,
  simTriggerSignal,
  isSimulating,
  setIsSimulating,
  simProgress,
  setSimProgress,
  onSimResultsLoaded,
  workspaceView,
  setWorkspaceView,
  slateOverrides,
  setSlateOverrides,
  allSimResults,
  simResults,
  setSimResults,
  generatedLineups,
  setGeneratedLineups
}) {
  const saved = slateOverrides?.[selectedGame.game_id] || {};

  // --- GPP VEGAS SCENARIO STATE ---
  const [activeSpreadScenario, setActiveSpreadScenario] = useState(saved.activeSpreadScenario || 'VEGAS_STANDARD_SPREAD');
  const [activeTotalScenario, setActiveTotalScenario] = useState(saved.activeTotalScenario || 'VEGAS_STANDARD_TOTAL');
  const [spreadOffset, setSpreadOffset] = useState(saved.spreadOffset !== undefined ? saved.spreadOffset : 0);
  const [totalOffset, setTotalOffset] = useState(saved.totalOffset !== undefined ? saved.totalOffset : 0);

  // --- OVERRIDES & SETTINGS STATE ---
  const [rosterData, setRosterData] = useState(null);
  const [teamOverrides, setTeamOverrides] = useState(saved.teamOverrides || {});
  const [playerOverrides, setPlayerOverrides] = useState(saved.playerOverrides || []);
  const [playerPercentiles, setPlayerPercentiles] = useState({});

  // --- UI SWITCHES ---
  const [rosterTab, setRosterTab] = useState('away');
  const [playerSearch, setPlayerSearch] = useState('');
  const [scoringFormat, setScoringFormat] = useState('DK');
  const [selectedPosition, setSelectedPosition] = useState('QB');
  const [selectedPercentile, setSelectedPercentile] = useState(50);
  const [expandedPlayerKey, setExpandedPlayerKey] = useState(null);
  const [playerPercentile, setPlayerPercentile] = useState(50);

  // --- SIMULATION RUN STATE ---
  const [baselineResults, setBaselineResults] = useState(saved.baselineResults || allSimResults?.[selectedGame.game_id] || null);

  // --- OPTIMIZER STATES ---
  const [lineupCount, setLineupCount] = useState(20);
  const [forceStack, setForceStack] = useState(true);
  const [forceBringback, setForceBringback] = useState(true);
  const [solving, setSolving] = useState(false);

  // simResults is lifted state (owned by App.jsx, shared across every game),
  // so switching games doesn't reset it — without a layout effect here, the
  // "sync state up to slateOverrides" effect below (a regular useEffect) can
  // run in the same commit while simResults still holds the *previous* game's
  // data, permanently writing that stale value into this game's slateOverrides
  // entry. useLayoutEffect fires before any useEffect in the same commit, so
  // simResults is corrected first.
  useLayoutEffect(() => {
    setSimResults(saved.simResults || allSimResults?.[selectedGame.game_id] || null);
    setBaselineResults(saved.baselineResults || allSimResults?.[selectedGame.game_id] || null);
    setGeneratedLineups([]);
  }, [selectedGame.game_id, allSimResults]);
  const [sortField, setSortField] = useState('dk_points');
  const [sortAsc, setSortAsc] = useState(false);
  const [filterLowProjections, setFilterLowProjections] = useState(false);

  const getPlayerPoints = (p, format) => {
    if (p.pos === 'DST') {
      return format === 'DK' || format === 'UNDERDOG' ? (p.dk_points || p.dk_score || 0) : (p.fd_points || p.fd_score || 0);
    }
    const pYds = p.pYds || 0;
    const pTD = p.pTD || 0;
    const intercept = p.int || 0;
    const rYds = p.rYds || 0;
    const rTD = p.rTD || 0;
    const rec = p.rec || 0;
    const recYds = p.recYds || 0;
    const recTD = p.recTD || 0;
    const fumbles = p.fumbles || 0;

    if (!p.pYds && !p.rYds && !p.rec && !p.pTD && !p.rTD && !p.recTD) {
      if (format === 'DK') return p.dk_points || p.dk_score || 0;
      if (format === 'FD') return p.fd_points || p.fd_score || 0;
      return p.fd_points || p.fd_score || 0;
    }

    let score = 0;
    if (format === 'DK') {
      score += pYds * 0.04;
      score += pTD * 4.0;
      score += intercept * -1.0;
      score += rYds * 0.1;
      score += rTD * 6.0;
      score += rec * 1.0;
      score += recYds * 0.1;
      score += recTD * 6.0;
      score += fumbles * -1.0;
      if (pYds >= 300) score += 3.0;
      if (rYds >= 100) score += 3.0;
      if (recYds >= 100) score += 3.0;
    } else if (format === 'FD') {
      score += pYds * 0.04;
      score += pTD * 4.0;
      score += intercept * -2.0;
      score += rYds * 0.1;
      score += rTD * 6.0;
      score += rec * 0.5;
      score += recYds * 0.1;
      score += recTD * 6.0;
      score += fumbles * -2.0;
    } else if (format === 'UNDERDOG') {
      score += pYds * 0.04;
      score += pTD * 4.0;
      score += intercept * -1.0;
      score += rYds * 0.1;
      score += rTD * 6.0;
      score += rec * 0.5;
      score += recYds * 0.1;
      score += recTD * 6.0;
      score += fumbles * -2.0;
    }
    return score;
  };

  const getPlayerPointsAtPercentile = (player, field, format) => {
    const isDk = format === 'DK' || format === 'UNDERDOG';
    const basePcts = isDk ? player.dk_pcts_all : player.fd_pcts_all;
    const baseMean = isDk ? (player.dk_points || player.dk_score || 0) : (player.fd_points || player.fd_score || 0);

    if (field === 'mean') return parseFloat(baseMean.toFixed(2));

    const pctIdx = typeof field === 'number' ? field : 50;
    if (basePcts && basePcts[pctIdx] !== undefined) {
      return parseFloat(basePcts[pctIdx].toFixed(2));
    }

    const baseVal = field === 50 ? (player[`${isDk ? 'dk' : 'fd'}_p50`] || baseMean) : baseMean;
    return parseFloat(baseVal.toFixed(2));
  };

  useEffect(() => {
    if (expandedPlayerKey) {
      setPlayerPercentile(selectedPercentile);
    }
  }, [expandedPlayerKey, selectedPercentile]);

  // Sync state up to parent when changed.
  // simResults/baselineResults are only safe to persist under this game's key
  // if they're actually tagged for this game — simResults is lifted state
  // (owned by App.jsx, not reset on remount) and can briefly still hold the
  // *previous* game's data in the same commit this effect runs in. Writing
  // that stale value here would permanently poison slateOverrides[game_id],
  // which every later visit to this game reads back via `saved.simResults`.
  useEffect(() => {
    if (!selectedGame) return;
    const safeSimResults = simResults?.game_id === selectedGame.game_id ? simResults : null;
    // baselineResults is local state (freshly initialized per game on mount),
    // not lifted like simResults, so it doesn't have the same staleness risk.
    const safeBaselineResults = baselineResults;
    setSlateOverrides(prev => {
      const existing = prev[selectedGame.game_id] || {};
      // Simple equality check to prevent updates if nothing changed
      if (
        existing.activeSpreadScenario === activeSpreadScenario &&
        existing.activeTotalScenario === activeTotalScenario &&
        existing.spreadOffset === spreadOffset &&
        existing.totalOffset === totalOffset &&
        existing.teamOverrides === teamOverrides &&
        existing.playerOverrides === playerOverrides &&
        existing.simResults === safeSimResults &&
        existing.baselineResults === safeBaselineResults
      ) {
        return prev;
      }
      return {
        ...prev,
        [selectedGame.game_id]: {
          activeSpreadScenario,
          activeTotalScenario,
          spreadOffset,
          totalOffset,
          teamOverrides,
          playerOverrides,
          simResults: safeSimResults,
          baselineResults: safeBaselineResults
        }
      };
    });
  }, [
    selectedGame.game_id,
    activeSpreadScenario,
    activeTotalScenario,
    spreadOffset,
    totalOffset,
    teamOverrides,
    playerOverrides,
    simResults,
    baselineResults,
    setSlateOverrides
  ]);

  // Sync active simulations back up on load if they were saved
  useEffect(() => {
    const activeResults = saved.simResults || allSimResults?.[selectedGame.game_id];
    if (activeResults) {
      onSimResultsLoaded(activeResults);
    }
  }, []);

  // Fetch rosters when selected game changes (which runs once on mount due to keyed parent)
  useEffect(() => {
    if (!selectedGame) return;

    const savedSimResults = slateOverrides?.[selectedGame.game_id]?.simResults || allSimResults?.[selectedGame.game_id];

    if (slateOverrides?.[selectedGame.game_id] && savedSimResults) {
      // Just fetch rosterData for UI if it's already saved locally
      setIsSimulating(true);
      setSimProgress('Loading rosters...');
      ApiService.getRosters(selectedGame.away_team, selectedGame.home_team)
        .then(data => {
          setRosterData(data);
          setIsSimulating(false);
        })
        .catch(err => {
          console.error("Error fetching rosters:", err);
          setIsSimulating(false);
        });
      return;
    }

    // Fresh game (no saved overrides yet this session): load baseline
    // roster/DNA data and initialize workload sliders from it. Every game's
    // baseline is already prepopulated in allSimResults on week load (see
    // App.jsx's getWeekSimResults call, sourced from the parquet cache) —
    // show it immediately instead of an empty "Adjust Workloads" state, no
    // "Run Engine" click required just to see cached results. Customizing a
    // slider and clicking "Run Engine" still triggers a fresh recompute.
    setIsSimulating(true);
    setSimProgress('Loading rosters & Trench DNA profiles...');
    ApiService.getRosters(selectedGame.away_team, selectedGame.home_team)
      .then(data => {
        setRosterData(data);

        // Initialize team overrides from saved slateOverrides or baseline rosters
        const initialTeams = slateOverrides?.[selectedGame.game_id]?.teamOverrides || {};
        if (Object.keys(initialTeams).length === 0) {
          [selectedGame.away_team, selectedGame.home_team].forEach(team => {
            initialTeams[team] = { ...data[team].team_settings };
          });
        }
        setTeamOverrides(initialTeams);

        // Initialize player overrides from saved slateOverrides or baseline rosters
        let initialPlayers = slateOverrides?.[selectedGame.game_id]?.playerOverrides || [];
        if (initialPlayers.length === 0) {
          [selectedGame.away_team, selectedGame.home_team].forEach(team => {
            data[team].roster.forEach(p => {
              initialPlayers.push({
                name: p.name,
                pos: p.pos,
                team: team,
                target_share: p.target_share,
                carry_share: p.carry_share,
                catch_rate: p.catch_rate,
                rush_td_share: p.rush_td_share || 0.0,
                rec_td_share: p.rec_td_share || 0.0,
                salary: p.salary,
                ownership_proj: 12.5,
                ownership_leverage: 0.0
              });
            });
          });
        }
        setPlayerOverrides(initialPlayers);
        setPlayerPercentiles({});

        const prepopulated = allSimResults?.[selectedGame.game_id] || null;
        setSimResults(prepopulated);
        setBaselineResults(prepopulated);
        onSimResultsLoaded(prepopulated);
        setGeneratedLineups([]);
        setRosterTab('away');
        setActiveSpreadScenario('VEGAS_STANDARD_SPREAD');
        setActiveTotalScenario('VEGAS_STANDARD_TOTAL');
        setSpreadOffset(0);
        setTotalOffset(0);
        setWorkspaceView(prepopulated ? 'summary' : 'workloads');
        setIsSimulating(false);
      })
      .catch(err => {
        console.error("Error fetching rosters:", err);
        setIsSimulating(false);
      });
  }, [selectedGame]);

  // Handle parent-driven simulation trigger signal
  const lastSignalRef = useRef(simTriggerSignal);
  useEffect(() => {
    if (simTriggerSignal > 0 && simTriggerSignal !== lastSignalRef.current) {
      lastSignalRef.current = simTriggerSignal;
      triggerSimulation();
    }
  }, [simTriggerSignal]);

  const getWorkloadSum = (team, field) => {
    return playerOverrides
      .filter(p => p.team === team && p.pos !== 'DST')
      .reduce((sum, p) => sum + parseFloat(p[field] || 0), 0);
  };

  const handleTeamOverride = (team, field, value) => {
    setTeamOverrides(prev => ({
      ...prev,
      [team]: {
        ...prev[team],
        [field]: parseFloat(value)
      }
    }));
  };

  const handlePlayerOverride = (playerName, field, value) => {
    setPlayerOverrides(prev =>
      prev.map(p => (p.name === playerName ? { ...p, [field]: parseFloat(value) } : p))
    );
  };

  const resetAllOverrides = () => {
    if (!rosterData || !selectedGame) return;
    
    const initialTeams = {};
    [selectedGame.away_team, selectedGame.home_team].forEach(team => {
      initialTeams[team] = { ...rosterData[team].team_settings };
    });
    setTeamOverrides(initialTeams);

    const initialPlayers = [];
    [selectedGame.away_team, selectedGame.home_team].forEach(team => {
      rosterData[team].roster.forEach(p => {
        initialPlayers.push({
          name: p.name,
          pos: p.pos,
          team: team,
          target_share: p.target_share,
          carry_share: p.carry_share,
          catch_rate: p.catch_rate,
          rush_td_share: p.rush_td_share || 0.0,
          rec_td_share: p.rec_td_share || 0.0,
          salary: p.salary,
          ownership_proj: 12.5,
          ownership_leverage: 0.0
        });
      });
    });
    setPlayerOverrides(initialPlayers);
    setActiveSpreadScenario('STANDARD_SPREAD');
    setActiveTotalScenario('STANDARD_TOTAL');
    setSpreadOffset(0);
    setTotalOffset(0);
    setWorkspaceView('workloads');
    setSimResults(null);
    onSimResultsLoaded(null);
  };

  const handleCustomSpreadChange = (newSpread) => {
    if (!selectedGame) return;
    const baselineSpread = -selectedGame.spread_line;
    const baselineTotal = selectedGame.total_line;
    const newSpreadOffset = newSpread - baselineSpread;
    setSpreadOffset(newSpreadOffset);

    // Keep the underdog's score from dropping too low proportionally
    const targetTotal = (baselineTotal - Math.abs(baselineSpread)) + Math.abs(newSpread);
    setTotalOffset(targetTotal - baselineTotal);
  };

  const handleCustomTotalChange = (newTotal) => {
    if (!selectedGame) return;
    const baselineSpread = -selectedGame.spread_line;
    const baselineTotal = selectedGame.total_line;
    const newTotalOffset = newTotal - baselineTotal;
    setTotalOffset(newTotalOffset);

    // Adjust spread proportionally based on total
    const newSpread = baselineSpread * (newTotal / (baselineTotal || 1));
    setSpreadOffset(newSpread - baselineSpread);
  };

  const handleImpliedScoreChange = (team, val) => {
    if (!selectedGame) return;
    const currentSpread = -selectedGame.spread_line + spreadOffset;
    const currentTotal = selectedGame.total_line + totalOffset;
    let awayImplied = (currentTotal + currentSpread) / 2;
    let homeImplied = (currentTotal - currentSpread) / 2;

    if (team === 'away') {
      awayImplied = val;
    } else {
      homeImplied = val;
    }

    const newSpread = awayImplied - homeImplied;
    const newTotal = awayImplied + homeImplied;

    setSpreadOffset(newSpread - (-selectedGame.spread_line));
    setTotalOffset(newTotal - selectedGame.total_line);
    setActiveSpreadScenario('CUSTOM');
    setActiveTotalScenario('CUSTOM');
  };

  const handleScenarioChange = (scenario) => {
    if (!selectedGame) return;

    const baselineSpread = -selectedGame.spread_line;
    const baselineTotal = selectedGame.total_line;

    let targetSpreadOffset = spreadOffset;
    let targetTotalOffset = totalOffset;
    let nextSpreadScenario = activeSpreadScenario;
    let nextTotalScenario = activeTotalScenario;

    const simSpread = baselineResults ? (baselineResults.summary.away_avg_score - baselineResults.summary.home_avg_score) : baselineSpread;
    const simTotal = baselineResults ? (baselineResults.summary.away_avg_score + baselineResults.summary.home_avg_score) : baselineTotal;

    if (scenario === 'VEGAS_STANDARD_SPREAD') {
      nextSpreadScenario = 'VEGAS_STANDARD_SPREAD';
      targetSpreadOffset = 0;
    } else if (scenario === 'SIM_STANDARD_SPREAD') {
      nextSpreadScenario = 'SIM_STANDARD_SPREAD';
      targetSpreadOffset = simSpread - baselineSpread;
    } else if (scenario === 'VEGAS_STANDARD_TOTAL') {
      nextTotalScenario = 'VEGAS_STANDARD_TOTAL';
      targetTotalOffset = 0;
    } else if (scenario === 'VEGAS_SHOOTOUT') {
      nextTotalScenario = 'VEGAS_SHOOTOUT';
      targetTotalOffset = 10;
    } else if (scenario === 'VEGAS_DEFENSIVE') {
      nextTotalScenario = 'VEGAS_DEFENSIVE';
      targetTotalOffset = -10;
    } else if (scenario === 'SIM_STANDARD_TOTAL') {
      nextTotalScenario = 'SIM_STANDARD_TOTAL';
      targetTotalOffset = simTotal - baselineTotal;
    } else if (scenario === 'SIM_SHOOTOUT') {
      nextTotalScenario = 'SIM_SHOOTOUT';
      targetTotalOffset = (simTotal + 10) - baselineTotal;
    } else if (scenario === 'SIM_DEFENSIVE') {
      nextTotalScenario = 'SIM_DEFENSIVE';
      targetTotalOffset = (simTotal - 10) - baselineTotal;
    }

    setActiveSpreadScenario(nextSpreadScenario);
    setActiveTotalScenario(nextTotalScenario);
    setSpreadOffset(targetSpreadOffset);
    setTotalOffset(targetTotalOffset);
  };

  const triggerSimulation = (
    customGame = null,
    customTeamOverrides = null,
    customPlayerOverrides = null,
    customSpreadOffset = null,
    customTotalOffset = null
  ) => {
    const activeGame = customGame || selectedGame;
    if (!activeGame) return;

    setIsSimulating(true);
    setSimProgress('Initializing Monte Carlo Simulation (10k runs)...');

    const baseSpread = -activeGame.spread_line;
    const baseTotal = activeGame.total_line;

    const activeTeamOverrides = customTeamOverrides || teamOverrides;
    const activePlayerOverrides = customPlayerOverrides || playerOverrides;

    const activeSpreadOffset = customSpreadOffset !== null ? customSpreadOffset : spreadOffset;
    const activeTotalOffset = customTotalOffset !== null ? customTotalOffset : totalOffset;

    const payload = {
      away_team: activeGame.away_team,
      home_team: activeGame.home_team,
      year: 2025,
      iterations: iterations,
      spread_override: -(baseSpread + activeSpreadOffset),
      total_override: baseTotal + activeTotalOffset,
      apply_weighting: customGame === null,
      team_overrides: activeTeamOverrides,
      player_overrides: activePlayerOverrides
    };

    setSimProgress('Running parallel processes across CPU cores...');
    
    ApiService.runSimulation(payload)
      .then(data => {
        const updatedProjections = data.projections.map(p => {
          const matchedPlayer = activePlayerOverrides.find(x => x.name === p.name);
          const own = matchedPlayer ? matchedPlayer.ownership_proj : 12.5;
          const leverage = own > 0 ? (p.dk_points / own) : 0.0;
          const opp = p.team === activeGame.away_team ? `vs ${activeGame.home_team}` : `@${activeGame.away_team}`;
          return {
            ...p,
            floor_dk_points: parseFloat((p.dk_points * 0.45).toFixed(1)),
            ceiling_dk_points: parseFloat((p.dk_points * 1.65).toFixed(1)),
            ownership_proj: own,
            ownership_leverage: parseFloat(leverage.toFixed(2)),
            opponent: opp
          };
        });

        const processedResults = {
          ...data,
          game_id: activeGame.game_id,
          away_team: activeGame.away_team,
          home_team: activeGame.home_team,
          projections: updatedProjections
        };

        setSimResults(processedResults);
        onSimResultsLoaded(processedResults);
        
        if (customGame) {
          setBaselineResults(processedResults);
        }

        let optProjections = updatedProjections;
        if (selectedSlate === 'TRADITIONAL') {
          const simulatedTeams = [activeGame.away_team, activeGame.home_team];
          const otherGamesProjections = weekProjections.filter(p => !simulatedTeams.includes(p.team));
          const normalizedOther = otherGamesProjections.map(p => ({
            ...p,
            dk_points: p.dk_score || p.dk_points || 0,
            fd_points: p.fd_score || p.fd_points || 0,
            dk_value: p.salary > 0 ? (p.dk_score || p.dk_points || 0) / (p.salary / 1000.0) : 0,
            fd_value: p.salary > 0 ? (p.fd_score || p.fd_points || 0) / (p.salary / 1000.0) : 0,
            ownership_proj: p.ownership_proj !== undefined ? p.ownership_proj : 12.5,
            ownership_leverage: p.ownership_leverage !== undefined ? p.ownership_leverage : 0
          }));
          optProjections = [...updatedProjections, ...normalizedOther];
        }

        runDfsOptimizer(optProjections);
        setWorkspaceView('summary');
        setIsSimulating(false);
      })
      .catch(err => {
        console.error(err);
        alert("Simulation failed.");
        setIsSimulating(false);
      });
  };

  const runDfsOptimizer = (customProjections = null) => {
    const activeProjections = customProjections || simResults?.projections;
    if (!activeProjections) return;
    setSolving(true);

    const rules = selectedSlate === 'SHOWDOWN' ? DFS_RULES.DK_SHOWDOWN : DFS_RULES.DK_TRADITIONAL;
    const roster = activeProjections;

    const generated = [];
    const iterationsCount = 150;

    let currentLineupIndex = 0;
    const batchSize = 5; // Generate 5 lineups at a time to prevent UI thread freeze

    const processNextBatch = () => {
      const targetLimit = Math.min(currentLineupIndex + batchSize, lineupCount);
      
      for (let l = currentLineupIndex; l < targetLimit; l++) {
        let bestLineup = null;
        let bestPoints = 0;

        for (let i = 0; i < iterationsCount; i++) {
          const candidate = [];
          let capSpent = 0;
          const draftedNames = new Set();

          if (selectedSlate === 'SHOWDOWN') {
            const capEligible = roster.filter(p => p.pos !== 'DST');
            if (capEligible.length === 0) continue;

            const captainCandidate = capEligible[Math.floor(Math.random() * Math.min(8, capEligible.length))];
            
            candidate.push({
              ...captainCandidate,
              slot: 'CPT',
              salary: captainCandidate.salary * 1.5,
              dk_points: captainCandidate.dk_points * 1.5
            });
            capSpent += captainCandidate.salary * 1.5;
            draftedNames.add(captainCandidate.name);

            const flexPool = roster.filter(p => !draftedNames.has(p.name));
            
            flexPool.sort((a, b) => {
              const ratioA = (a.dk_points / a.salary) * (0.85 + Math.random() * 0.3);
              const ratioB = (b.dk_points / b.salary) * (0.85 + Math.random() * 0.3);
              return ratioB - ratioA;
            });

            for (let f = 0; f < flexPool.length && candidate.length < 6; f++) {
              const p = flexPool[f];
              if (capSpent + p.salary <= rules.salaryCap) {
                candidate.push({ ...p, slot: 'FLEX' });
                capSpent += p.salary;
                draftedNames.add(p.name);
              }
            }
          } else {
            const qbs = roster.filter(p => p.pos === 'QB');
            const selectedQB = qbs[Math.floor(Math.random() * qbs.length)];
            if (!selectedQB) continue;
            candidate.push({ ...selectedQB, slot: 'QB' });
            capSpent += selectedQB.salary;
            draftedNames.add(selectedQB.name);

            if (forceStack) {
              const stackPool = roster.filter(p => p.team === selectedQB.team && (p.pos === 'WR' || p.pos === 'TE'));
              if (stackPool.length > 0) {
                const wrStack = stackPool[Math.floor(Math.random() * stackPool.length)];
                candidate.push({ ...wrStack, slot: wrStack.pos });
                capSpent += wrStack.salary;
                draftedNames.add(wrStack.name);
              }
            }

            if (forceBringback) {
              let oppTeam = '';
              if (selectedQB.opponent) {
                oppTeam = selectedQB.opponent.replace('@', '').replace('vs ', '').trim();
              } else {
                oppTeam = selectedQB.team === selectedGame.away_team ? selectedGame.home_team : selectedGame.away_team;
              }
              const oppWRs = roster.filter(p => p.team === oppTeam && p.pos === 'WR');
              if (oppWRs.length > 0) {
                const wrOpp = oppWRs[Math.floor(Math.random() * oppWRs.length)];
                candidate.push({ ...wrOpp, slot: 'WR' });
                capSpent += wrOpp.salary;
                draftedNames.add(wrOpp.name);
              }
            }

            const remainingSlots = rules.slots.slice(candidate.length);
            const pool = roster.filter(p => !draftedNames.has(p.name));
            
            pool.sort((a, b) => {
              const scoreA = (a.dk_points / a.salary) * (0.8 + Math.random() * 0.4);
              const scoreB = (b.dk_points / b.salary) * (0.8 + Math.random() * 0.4);
              return scoreB - scoreA;
            });

            for (let s = 0; s < remainingSlots.length; s++) {
              const slotType = remainingSlots[s];
              const matchingPlayer = pool.find(p => {
                if (draftedNames.has(p.name)) return false;
                if (capSpent + p.salary > rules.salaryCap) return false;
                
                if (slotType === 'QB') return p.pos === 'QB';
                if (slotType === 'RB') return p.pos === 'RB';
                if (slotType === 'WR') return p.pos === 'WR';
                if (slotType === 'TE') return p.pos === 'TE';
                if (slotType === 'DST') return p.pos === 'DST';
                if (slotType === 'FLEX') return rules.flexEligible.includes(p.pos);
                return false;
              });

              if (matchingPlayer) {
                candidate.push({ ...matchingPlayer, slot: slotType });
                capSpent += matchingPlayer.salary;
                draftedNames.add(matchingPlayer.name);
              }
            }
          }

          if (candidate.length === rules.slots.length) {
            const totalPoints = candidate.reduce((sum, p) => sum + p.dk_points, 0);
            if (totalPoints > bestPoints) {
              bestPoints = totalPoints;
              bestLineup = candidate;
            }
          }
        }

        if (bestLineup) {
          generated.push({
            lineup_id: l + 1,
            players: bestLineup,
            total_salary: bestLineup.reduce((sum, p) => sum + p.salary, 0),
            total_points: bestPoints
          });
        }
      }

      currentLineupIndex = targetLimit;

      if (currentLineupIndex < lineupCount) {
        setTimeout(processNextBatch, 0);
      } else {
        setGeneratedLineups(generated.sort((a, b) => b.total_points - a.total_points));
        setSolving(false);
      }
    };

    setTimeout(processNextBatch, 10);
  };

  const exportLineupsToCsv = () => {
    if (generatedLineups.length === 0) return;
    
    let headers = [];
    if (selectedSlate === 'SHOWDOWN') {
      headers = ['CPT', 'FLEX', 'FLEX', 'FLEX', 'FLEX', 'FLEX'];
    } else {
      headers = ['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'TE', 'FLEX', 'DST'];
    }

    const csvRows = [headers.join(',')];

    generatedLineups.forEach(lineup => {
      const row = headers.map(slot => {
        const matchingPlayer = lineup.players.find(p => p.slot === slot);
        return matchingPlayer ? `"${matchingPlayer.name}"` : '""';
      });
      csvRows.push(row.join(','));
    });

    const blob = new Blob([csvRows.join('\n')], { type: 'text/csv' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.setAttribute('href', url);
    a.setAttribute('download', `nflsims_lineups_${selectedSlate.toLowerCase()}.csv`);
    a.click();
  };

  const handleSort = (field) => {
    if (sortField === field) {
      setSortAsc(!sortAsc);
    } else {
      setSortField(field);
      setSortAsc(false);
    }
  };

  const getOptimalRate = (playerName) => {
    if (generatedLineups.length === 0) return 0;
    const appearances = generatedLineups.filter(lineup => 
      lineup.players.some(p => p.name === playerName)
    ).length;
    return parseFloat(((appearances / generatedLineups.length) * 100).toFixed(1));
  };

  const combinedProjections = useMemo(() => {
    if (selectedSlate === 'SHOWDOWN') {
      return simResults ? simResults.projections : [];
    }
    
    // Combine projections from all games in allSimResults
    const simulatedProjections = [];
    const simulatedTeams = new Set();
    
    Object.values(allSimResults || {}).forEach(res => {
      if (res && res.projections) {
        simulatedProjections.push(...res.projections);
        simulatedTeams.add(res.away_team);
        simulatedTeams.add(res.home_team);
      }
    });
    
    // Fall back to weekProjections for teams not simulated yet
    const otherGamesProjections = weekProjections.filter(p => !simulatedTeams.has(p.team));
    const normalizedOther = otherGamesProjections.map(p => ({
      ...p,
      dk_points: p.dk_score || p.dk_points || 0,
      fd_points: p.fd_score || p.fd_points || 0,
      dk_value: p.salary > 0 ? (p.dk_score || p.dk_points || 0) / (p.salary / 1000.0) : 0,
      fd_value: p.salary > 0 ? (p.fd_score || p.fd_points || 0) / (p.salary / 1000.0) : 0,
      ownership_proj: p.ownership_proj !== undefined ? p.ownership_proj : 12.5,
      ownership_leverage: p.ownership_leverage !== undefined ? p.ownership_leverage : 0
    }));
    
    // If the currently selected game is simulated but not in allSimResults, make sure we include it
    if (simResults && !simulatedTeams.has(simResults.away_team)) {
      simulatedProjections.push(...simResults.projections);
    }
    
    return [...simulatedProjections, ...normalizedOther];
  }, [simResults, allSimResults, weekProjections, selectedSlate]);

  const sortedProjections = useMemo(() => {
    if (!simResults && Object.keys(allSimResults || {}).length === 0) return [];
    const baseList = workspaceView === 'dfs_summary' ? combinedProjections : (simResults ? simResults.projections : []);
    return [...baseList]
      .filter(p => p.name.toLowerCase().includes(playerSearch.toLowerCase()))
      .filter(p => !filterLowProjections || getPlayerPoints(p, scoringFormat) >= 3.0)
      .sort((a, b) => {
        let valA = a[sortField];
        let valB = b[sortField];

        if (sortField === 'leverage') {
          const optimalA = a.optimal_pct !== undefined ? a.optimal_pct : getOptimalRate(a.name);
          const optimalB = b.optimal_pct !== undefined ? b.optimal_pct : getOptimalRate(b.name);
          valA = optimalA - (a.ownership_proj || 0);
          valB = optimalB - (b.ownership_proj || 0);
        } else if (sortField === 'optimal_pct') {
          valA = a.optimal_pct !== undefined ? a.optimal_pct : getOptimalRate(a.name);
          valB = b.optimal_pct !== undefined ? b.optimal_pct : getOptimalRate(b.name);
        } else if (sortField === 'dk_points' || sortField === 'points') {
          valA = getPlayerPoints(a, scoringFormat);
          valB = getPlayerPoints(b, scoringFormat);
        } else if (sortField === 'dk_value' || sortField === 'value') {
          const ptsA = getPlayerPoints(a, scoringFormat);
          const ptsB = getPlayerPoints(b, scoringFormat);
          valA = a.salary > 0 ? ptsA / (a.salary / 1000.0) : 0;
          valB = b.salary > 0 ? ptsB / (b.salary / 1000.0) : 0;
        } else if (sortField === 'fd_points') {
          const pctA = playerPercentiles[a.name] !== undefined ? playerPercentiles[a.name] : 50;
          const pctB = playerPercentiles[b.name] !== undefined ? playerPercentiles[b.name] : 50;
          valA = a.fd_pcts_all ? a.fd_pcts_all[pctA] : a.fd_points;
          valB = b.fd_pcts_all ? b.fd_pcts_all[pctB] : b.fd_points;
        }

        if (typeof valA === 'string') {
          return sortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
        }
        return sortAsc ? valA - valB : valB - valA;
      });
  }, [simResults, workspaceView, combinedProjections, playerSearch, filterLowProjections, sortField, sortAsc, playerPercentiles, scoringFormat]);

  const getSimSpread = (summary) => {
    if (!summary) return '--';
    const spread = summary.away_avg_score - summary.home_avg_score;
    return spread > 0 ? `+${spread.toFixed(1)}` : spread.toFixed(1);
  };

  const getSimTotal = (summary) => {
    if (!summary) return '--';
    return (summary.away_avg_score + summary.home_avg_score).toFixed(1);
  };

  const getTeamSimSummary = (team) => {
    if (!simResults) return { plays: 60, pass_yds: 200, rush_yds: 100, tds: 2.0, turnovers: 1.0 };
    
    const players = simResults.projections.filter(p => p.team === team);
    const qbs = players.filter(p => p.pos === 'QB');
    
    const pass_yds = qbs.reduce((sum, p) => sum + (p.pYds || 0), 0);
    const rush_yds = players.reduce((sum, p) => sum + (p.rYds || 0), 0);
    
    const qb_att = qbs.reduce((sum, p) => sum + (p.pAtt || 0), 0);
    const qb_sacks = qbs.reduce((sum, p) => sum + (p.sacks_taken || 0), 0);
    const team_r_att = players.reduce((sum, p) => sum + (p.rAtt || 0), 0);
    const plays = qb_att + qb_sacks + team_r_att;
    
    // Offensive TDs = Rushing TDs + Receiving TDs
    const tds = players.reduce((sum, p) => sum + (p.rTD || 0) + (p.pos !== 'QB' ? (p.recTD || 0) : 0), 0);
    
    // Turnovers = Interceptions + Fumbles
    const turnovers = qbs.reduce((sum, p) => sum + (p.int || 0), 0) + players.reduce((sum, p) => sum + (p.fumbles || 0), 0);
    
    return { plays, pass_yds, rush_yds, tds, turnovers };
  };

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '20px' }}>
      {/* MAIN MATCHUP SUMMARY BANNER */}
      <div className="glass-panel" style={{ 
        marginBottom: '20px', 
        background: 'linear-gradient(90deg, rgba(11, 17, 38, 0.8) 0%, rgba(0, 242, 254, 0.05) 100%)',
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        flexWrap: 'wrap',
        gap: '15px'
      }}>
        <div>
          <h1 style={{ fontSize: '1.4rem', color: 'var(--text-white)', display: 'flex', alignItems: 'center', gap: '10px', flexWrap: 'wrap' }}>
            <span>{selectedGame.away_team} ({selectedGame.away_record || '0-0'}) @ {selectedGame.home_team} ({selectedGame.home_record || '0-0'})</span>
            {ApiService.isSandbox() && <SandboxBadge />}
          </h1>
          <span style={{ fontSize: '0.8rem', color: 'var(--text-muted)' }}>
            Vegas Market Line: Spread {(-selectedGame.spread_line) > 0 ? `+${-selectedGame.spread_line}` : -selectedGame.spread_line} | O/U {selectedGame.total_line}
            <span style={{ margin: '0 8px', color: 'rgba(255,255,255,0.15)' }}>|</span>
            Implied: {selectedGame.away_team} {((selectedGame.total_line + (-selectedGame.spread_line)) / 2).toFixed(2)} - {((selectedGame.total_line - (-selectedGame.spread_line)) / 2).toFixed(2)} {selectedGame.home_team}
          </span>
        </div>

        <div style={{ display: 'flex', gap: '20px', fontSize: '0.9rem' }}>
          <div style={{ textAlign: 'right' }} title="Computed from our own Monte Carlo simulation output — not derived from or matched to the Vegas line shown on the left.">
            <div style={{ color: 'var(--text-muted)', fontSize: '0.72rem', textTransform: 'uppercase' }}>
              Weekly Sim Baseline <span style={{ opacity: 0.6, cursor: 'help' }}>ⓘ</span>
            </div>
            <strong style={{ color: 'var(--text-white)' }}>Spread: {baselineResults ? getSimSpread(baselineResults.summary) : '--'}</strong>
            <span style={{ margin: '0 6px', color: 'var(--text-muted)' }}>|</span>
            <strong style={{ color: 'var(--text-white)' }}>O/U: {baselineResults ? getSimTotal(baselineResults.summary) : '--'}</strong>
          </div>

          {baselineResults && (
            <div
              style={{ textAlign: 'right', borderLeft: '1px solid var(--border-glass)', paddingLeft: '20px' }}
              title="Computed from our own Monte Carlo simulation output — not derived from or matched to the Vegas line shown on the left."
            >
              <div style={{ color: 'var(--text-muted)', fontSize: '0.72rem', textTransform: 'uppercase' }}>
                NFLSims Projection <span style={{ opacity: 0.6, cursor: 'help' }}>ⓘ</span>
              </div>
              <strong style={{ color: 'var(--accent-green)' }}>
                Sim Score: {baselineResults.summary.away_avg_score} - {baselineResults.summary.home_avg_score}
              </strong>
            </div>
          )}
        </div>
      </div>

      {/* VEGAS SCENARIO PANEL */}
      <div className="glass-panel">
        <div style={{ marginBottom: '14px' }}>
          <h2>Vegas & Simulation Scenario Controls</h2>
          <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)' }}>
            Shift the Monte Carlo distribution mean to simulate Vegas market conditions or baseline Sim model expectations.
          </p>
        </div>
        
        {/* SEPARATED SCENARIOS GROUPS */}
        <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '20px', marginBottom: '16px' }}>
          {/* VEGAS MARKET FLOW */}
          <div className="glass-panel" style={{ background: 'rgba(255, 255, 255, 0.01)', padding: '14px', border: '1px solid var(--border-glass)', borderRadius: '8px', margin: 0 }}>
            <h3 style={{ fontSize: '0.9rem', color: 'var(--text-white)', marginBottom: '12px', borderBottom: '1px solid var(--border-glass)', paddingBottom: '6px', fontWeight: 'bold' }}>
              Vegas Market Flow
            </h3>
            <div style={{ display: 'flex', flexDirection: 'column', gap: '10px' }}>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', fontWeight: '600' }}>Spread Scenario</span>
                <button 
                  className={`scenario-btn ${activeSpreadScenario === 'VEGAS_STANDARD_SPREAD' ? 'active' : ''}`} 
                  onClick={() => handleScenarioChange('VEGAS_STANDARD_SPREAD')}
                >
                  Standard Spread ({(-selectedGame.spread_line) > 0 ? `+${-selectedGame.spread_line}` : -selectedGame.spread_line})
                </button>
              </div>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', fontWeight: '600' }}>Total Scenarios</span>
                <div style={{ display: 'flex', gap: '6px' }}>
                  <button 
                    className={`scenario-btn ${activeTotalScenario === 'VEGAS_STANDARD_TOTAL' ? 'active' : ''}`} 
                    onClick={() => handleScenarioChange('VEGAS_STANDARD_TOTAL')}
                  >
                    Standard Total ({selectedGame.total_line})
                  </button>
                  <button 
                    className={`scenario-btn ${activeTotalScenario === 'VEGAS_SHOOTOUT' ? 'active' : ''}`} 
                    onClick={() => handleScenarioChange('VEGAS_SHOOTOUT')}
                  >
                    Shootout (+10)
                  </button>
                  <button 
                    className={`scenario-btn ${activeTotalScenario === 'VEGAS_DEFENSIVE' ? 'active' : ''}`} 
                    onClick={() => handleScenarioChange('VEGAS_DEFENSIVE')}
                  >
                    Defensive (-10)
                  </button>
                </div>
              </div>
            </div>
          </div>

          {/* SIM MODEL FLOW */}
          <div className="glass-panel" style={{ background: 'rgba(255, 255, 255, 0.01)', padding: '14px', border: '1px solid var(--border-glass)', borderRadius: '8px', margin: 0 }}>
            <h3 style={{ fontSize: '0.9rem', color: 'var(--text-white)', marginBottom: '12px', borderBottom: '1px solid var(--border-glass)', paddingBottom: '6px', fontWeight: 'bold' }}>
              Sim Model Flow
            </h3>
            <div style={{ display: 'flex', flexDirection: 'column', gap: '10px' }}>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', fontWeight: '600' }}>Spread Scenario</span>
                <button 
                  className={`scenario-btn ${activeSpreadScenario === 'SIM_STANDARD_SPREAD' ? 'active' : ''}`} 
                  onClick={() => handleScenarioChange('SIM_STANDARD_SPREAD')}
                  disabled={!baselineResults}
                >
                  Standard Spread ({baselineResults ? getSimSpread(baselineResults.summary) : '--'})
                </button>
              </div>
              <div style={{ display: 'flex', flexDirection: 'column', gap: '4px' }}>
                <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', fontWeight: '600' }}>Total Scenarios</span>
                <div style={{ display: 'flex', gap: '6px' }}>
                  <button 
                    className={`scenario-btn ${activeTotalScenario === 'SIM_STANDARD_TOTAL' ? 'active' : ''}`} 
                    onClick={() => handleScenarioChange('SIM_STANDARD_TOTAL')}
                    disabled={!baselineResults}
                  >
                    Standard Total ({baselineResults ? getSimTotal(baselineResults.summary) : '--'})
                  </button>
                  <button 
                    className={`scenario-btn ${activeTotalScenario === 'SIM_SHOOTOUT' ? 'active' : ''}`} 
                    onClick={() => handleScenarioChange('SIM_SHOOTOUT')}
                    disabled={!baselineResults}
                  >
                    Shootout (+10)
                  </button>
                  <button 
                    className={`scenario-btn ${activeTotalScenario === 'SIM_DEFENSIVE' ? 'active' : ''}`} 
                    onClick={() => handleScenarioChange('SIM_DEFENSIVE')}
                    disabled={!baselineResults}
                  >
                    Defensive (-10)
                  </button>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* IMPLIED TEAM TOTALS OVERRIDES */}
        <div style={{ borderTop: '1px solid var(--border-glass)', paddingTop: '14px' }}>
          <div style={{ marginBottom: '10px', display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
              <span style={{ fontSize: '0.82rem', fontWeight: '600' }}>Custom Implied Team Totals Override:</span>
              <button 
                className={`scenario-btn ${activeSpreadScenario === 'CUSTOM' ? 'active' : ''}`} 
                onClick={() => {
                  setActiveSpreadScenario('CUSTOM');
                  setActiveTotalScenario('CUSTOM');
                }}
                style={{ padding: '2px 8px', fontSize: '0.72rem' }}
              >
                Custom Mode
              </button>
            </div>
            <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>
              Calculated Line: Spread {((-selectedGame.spread_line + spreadOffset) > 0 ? '+' : '') + (-selectedGame.spread_line + spreadOffset).toFixed(1)} | O/U {(selectedGame.total_line + totalOffset).toFixed(1)}
            </span>
          </div>
          {(() => {
            const currentSpread = -selectedGame.spread_line + spreadOffset;
            const currentTotal = selectedGame.total_line + totalOffset;
            const awayImplied = (currentTotal + currentSpread) / 2;
            const homeImplied = (currentTotal - currentSpread) / 2;
            const isCustom = activeSpreadScenario === 'CUSTOM';

            return (
              <div style={{ 
                display: 'flex', 
                justifyContent: 'space-around', 
                alignItems: 'center', 
                background: 'rgba(255,255,255,0.02)', 
                border: '1px solid var(--border-glass)', 
                borderRadius: '8px', 
                padding: '12px',
                fontSize: '0.85rem'
              }}>
                <div style={{ textAlign: 'center' }}>
                  <span style={{ color: 'var(--text-muted)', display: 'block', fontSize: '0.72rem', textTransform: 'uppercase', marginBottom: '6px' }}>{selectedGame.away_team} Implied Total</span>
                  <input 
                    type="number" 
                    step="0.5"
                    value={parseFloat(awayImplied.toFixed(2))}
                    disabled={!isCustom}
                    onChange={(e) => {
                      const val = parseFloat(e.target.value);
                      if (!isNaN(val)) {
                        handleImpliedScoreChange('away', val);
                      }
                    }}
                    style={{ 
                      fontFamily: 'monospace', 
                      width: '85px', 
                      textAlign: 'center', 
                      fontSize: '1.1rem', 
                      fontWeight: 'bold',
                      background: isCustom ? 'rgba(0,0,0,0.3)' : 'rgba(0,0,0,0.1)', 
                      border: '1px solid var(--border-glass)', 
                      color: isCustom ? 'var(--accent-cyan)' : 'var(--text-muted)', 
                      borderRadius: '4px',
                      padding: '3px 2px',
                      cursor: isCustom ? 'text' : 'not-allowed',
                      opacity: isCustom ? 1 : 0.6
                    }} 
                  />
                </div>
                <div style={{ width: '1px', height: '28px', background: 'var(--border-glass)' }}></div>
                <div style={{ textAlign: 'center' }}>
                  <span style={{ color: 'var(--text-muted)', display: 'block', fontSize: '0.72rem', textTransform: 'uppercase', marginBottom: '6px' }}>{selectedGame.home_team} Implied Total</span>
                  <input 
                    type="number" 
                    step="0.5"
                    value={parseFloat(homeImplied.toFixed(2))}
                    disabled={!isCustom}
                    onChange={(e) => {
                      const val = parseFloat(e.target.value);
                      if (!isNaN(val)) {
                        handleImpliedScoreChange('home', val);
                      }
                    }}
                    style={{ 
                      fontFamily: 'monospace', 
                      width: '85px', 
                      textAlign: 'center', 
                      fontSize: '1.1rem', 
                      fontWeight: 'bold',
                      background: isCustom ? 'rgba(0,0,0,0.3)' : 'rgba(0,0,0,0.1)', 
                      border: '1px solid var(--border-glass)', 
                      color: isCustom ? 'var(--accent-cyan)' : 'var(--text-muted)', 
                      borderRadius: '4px',
                      padding: '3px 2px',
                      cursor: isCustom ? 'text' : 'not-allowed',
                      opacity: isCustom ? 1 : 0.6
                    }} 
                  />
                </div>
              </div>
            );
          })()}
        </div>
      </div>

      {/* PLAYBOOK OVERRIDES + ADJUST SETTINGS BAR PANEL */}
      <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '20px', alignItems: 'stretch' }}>
        {/* TEAM COACH DNA PARAMETERS */}
        <div className="glass-panel">
          <div className="flex-between" style={{ marginBottom: '14px' }}>
            <h2>Playbook Overrides</h2>
            <button onClick={resetAllOverrides} style={{ background: 'rgba(255,255,255,0.05)', border: '1px solid var(--border-glass)', color: '#fff', borderRadius: '4px', padding: '3px 8px', fontSize: '0.75rem' }}>Reset All</button>
          </div>

          <div className="tab-switcher" style={{ marginBottom: '16px' }}>
            <button className={`tab-btn ${rosterTab === 'away' ? 'active' : ''}`} onClick={() => setRosterTab('away')}>{selectedGame.away_team}</button>
            <button className={`tab-btn ${rosterTab === 'home' ? 'active' : ''}`} onClick={() => setRosterTab('home')}>{selectedGame.home_team}</button>
          </div>

          {(() => {
            const activeTeam = rosterTab === 'away' ? selectedGame.away_team : selectedGame.home_team;
            const settings = teamOverrides[activeTeam];
            if (!settings) return null;

            return (
              <div style={{ display: 'flex', flexDirection: 'column', gap: '14px' }}>
                <div className="control-group" style={{ marginBottom: 0 }}>
                  <div className="control-label">
                    <span>Pace (Plays per Game)</span>
                    <span className="control-val">{settings.plays_per_game}</span>
                  </div>
                  <input type="range" min="50" max="80" step="0.5" value={settings.plays_per_game} onChange={(e) => handleTeamOverride(activeTeam, 'plays_per_game', e.target.value)} />
                </div>

                <div className="control-group" style={{ marginBottom: 0 }}>
                  <div className="control-label">
                    <span>Pressure Rate %</span>
                    <span className="control-val">{(settings.def_pressure_rate * 100).toFixed(0)}%</span>
                  </div>
                  <input type="range" min="0.10" max="0.50" step="0.01" value={settings.def_pressure_rate} onChange={(e) => handleTeamOverride(activeTeam, 'def_pressure_rate', e.target.value)} />
                </div>

                <div className="control-group" style={{ marginBottom: 0 }}>
                  <div className="control-label">
                    <span>PROE %</span>
                    <span className="control-val">{Number(settings.proe).toFixed(1)}%</span>
                  </div>
                  <input type="range" min="-15" max="15" step="0.5" value={settings.proe} onChange={(e) => handleTeamOverride(activeTeam, 'proe', e.target.value)} />
                </div>
              </div>
            );
          })()}
        </div>

        {/* ADJUST SETTINGS WORKSPACE CONTROLLER */}
        <div className="glass-panel" style={{ display: 'flex', flexDirection: 'column', justifyContent: 'center' }}>
          <h2>Adjust Settings</h2>
          <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', marginBottom: '14px' }}>
            Select a tab to configure specific sim overrides, inspect baseline/sim details, or compile DraftKings optimal lineups.
          </p>
          {(simResults || (weekProjections && weekProjections.length > 0)) ? (
            <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
              {simResults && (
                <>
                  <button 
                    className={`scenario-btn ${workspaceView === 'workloads' ? 'active' : ''}`} 
                    onClick={() => setWorkspaceView('workloads')}
                  >
                    Adjust Workloads
                  </button>
                  <button 
                    className={`scenario-btn ${workspaceView === 'summary' ? 'active' : ''}`} 
                    onClick={() => setWorkspaceView('summary')}
                  >
                    Sim Game Summary
                  </button>
                  <button 
                    className={`scenario-btn ${workspaceView === 'projections' ? 'active' : ''}`} 
                    onClick={() => setWorkspaceView('projections')}
                  >
                    Simulated Projections
                  </button>
                </>
              )}
            </div>
          ) : (
            <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', textAlign: 'center', padding: '15px', border: '1px dashed var(--border-glass)', borderRadius: '6px' }}>
              Please run the engine simulations to enable these settings views.
            </div>
          )}
        </div>
      </div>

      {/* ACTIVE WORKSPACE PANEL */}
      <div style={{ display: 'flex', flexDirection: 'column', gap: '20px' }}>
        {/* A. SIM GAME SUMMARY VIEW */}
        {simResults && workspaceView === 'summary' && (
          <div className="glass-panel" style={{ minHeight: '500px' }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '20px' }}>
              <h2>Simulated Matchup Outcomes</h2>
            </div>

            {/* Score and Probability Dial */}
            <div style={{
              display: 'grid',
              gridTemplateColumns: '1fr 1fr',
              gap: '16px',
              marginBottom: '20px'
            }}>
              {/* Expected scores card */}
              {(() => {
                const awayScore = simResults.summary.away_avg_score;
                const homeScore = simResults.summary.home_avg_score;

                const awayWP = Math.round(parseFloat(simResults.summary.win_probability_away));
                const homeWP = Math.round(parseFloat(simResults.summary.win_probability_home));

                const awayColor = TEAM_COLORS[selectedGame.away_team] || 'var(--accent-primary)';
                const homeColor = TEAM_COLORS[selectedGame.home_team] || 'var(--accent-green)';

                return (
                  <>
                    <div style={{
                      background: 'rgba(11,17,38,0.5)',
                      border: '1px solid var(--border-glass)',
                      borderRadius: '12px',
                      padding: '16px',
                      textAlign: 'center'
                    }}>
                      <div style={{ fontSize: '0.8rem', color: 'var(--text-muted)', textTransform: 'uppercase', marginBottom: '8px' }}>Projected Average Score</div>
                      <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center', gap: '15px' }}>
                        <div>
                          <span style={{ fontSize: '1.8rem', fontWeight: 800, color: awayColor }}>{awayScore}</span>
                          <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)', fontWeight: 600 }}>{selectedGame.away_team}</div>
                        </div>
                        <span style={{ fontSize: '1.5rem', color: 'var(--text-muted)' }}>-</span>
                        <div>
                          <span style={{ fontSize: '1.8rem', fontWeight: 800, color: homeColor }}>{homeScore}</span>
                          <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)', fontWeight: 600 }}>{selectedGame.home_team}</div>
                        </div>
                      </div>
                    </div>

                    {/* Win Probability dial card */}
                    <div style={{
                      background: 'rgba(11,17,38,0.5)',
                      border: '1px solid var(--border-glass)',
                      borderRadius: '12px',
                      padding: '16px'
                    }}>
                      <div style={{ fontSize: '0.8rem', color: 'var(--text-muted)', textTransform: 'uppercase', marginBottom: '8px', textAlign: 'center' }}>Win Probability</div>
                      <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                        <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.8rem', color: '#fff' }}>
                          <span style={{ color: awayColor, fontWeight: 700 }}>{selectedGame.away_team}: {awayWP}%</span>
                          <span style={{ color: homeColor, fontWeight: 700 }}>{selectedGame.home_team}: {homeWP}%</span>
                        </div>
                        <div style={{ width: '100%', height: '8px', background: 'rgba(255,255,255,0.05)', borderRadius: '4px', overflow: 'hidden', display: 'flex' }}>
                          <div style={{ width: `${awayWP}%`, background: awayColor, height: '100%' }}></div>
                          <div style={{ width: `${homeWP}%`, background: homeColor, height: '100%' }}></div>
                        </div>
                      </div>
                    </div>
                  </>
                );
              })()}
            </div>

            {/* Team offence breakdown comparison */}
            <div style={{ marginBottom: '24px' }}>
              <h3>Simulated Team Offensive Averages</h3>
              <div className="table-container" style={{ marginTop: '10px' }}>
                <table>
                  <thead>
                    <tr>
                      <th>Category</th>
                      <th>{selectedGame.away_team}</th>
                      <th>{selectedGame.home_team}</th>
                    </tr>
                  </thead>
                  <tbody>
                    {(() => {
                      const awayStats = getTeamSimSummary(selectedGame.away_team);
                      const homeStats = getTeamSimSummary(selectedGame.home_team);
                      return (
                        <>
                          <tr>
                            <td>Expected Plays</td>
                            <td>{awayStats.plays.toFixed(1)}</td>
                            <td>{homeStats.plays.toFixed(1)}</td>
                          </tr>
                          <tr>
                            <td>Passing Yards</td>
                            <td>{awayStats.pass_yds.toFixed(2)} yds</td>
                            <td>{homeStats.pass_yds.toFixed(2)} yds</td>
                          </tr>
                          <tr>
                            <td>Rushing Yards</td>
                            <td>{awayStats.rush_yds.toFixed(2)} yds</td>
                            <td>{homeStats.rush_yds.toFixed(2)} yds</td>
                          </tr>
                          <tr>
                            <td>Projected Offensive TDs</td>
                            <td>{awayStats.tds.toFixed(1)}</td>
                            <td>{homeStats.tds.toFixed(1)}</td>
                          </tr>
                          <tr>
                            <td>Turnovers</td>
                            <td style={{ color: 'var(--accent-red)' }}>{awayStats.turnovers.toFixed(1)}</td>
                            <td style={{ color: 'var(--accent-red)' }}>{homeStats.turnovers.toFixed(1)}</td>
                          </tr>
                        </>
                      );
                    })()}
                  </tbody>
                </table>
              </div>
            </div>

          </div>
        )}

        {/* B. WORKLOAD EDITOR PANEL */}
        {((!simResults && workspaceView !== 'dfs_summary') || workspaceView === 'workloads' || workspaceView === 'projections') && (
          <div className="glass-panel" style={{ minHeight: '500px' }}>
            <div className="flex-between" style={{ marginBottom: '14px' }}>
              <h2>{(simResults && workspaceView === 'projections') ? 'Simulated Projections' : 'Adjust Workloads'}</h2>
              <div className="tab-switcher" style={{ maxWidth: '200px' }}>
                <button className={`tab-btn ${rosterTab === 'away' ? 'active' : ''}`} onClick={() => setRosterTab('away')}>{selectedGame.away_team}</button>
                <button className={`tab-btn ${rosterTab === 'home' ? 'active' : ''}`} onClick={() => setRosterTab('home')}>{selectedGame.home_team}</button>
              </div>
            </div>

            {(!simResults || workspaceView === 'workloads') ? (
              <div>
                {/* Share Balancer summaries */}
                <div style={{
                  display: 'grid',
                  gridTemplateColumns: 'repeat(4, 1fr)',
                  gap: '12px',
                  marginBottom: '16px',
                  background: 'rgba(11,17,38,0.5)',
                  padding: '12px',
                  borderRadius: '10px',
                  border: '1px solid var(--border-glass)'
                }}>
                  {(() => {
                    const activeTeam = rosterTab === 'away' ? selectedGame.away_team : selectedGame.home_team;
                    const tgtSum = getWorkloadSum(activeTeam, 'target_share');
                    const rushSum = getWorkloadSum(activeTeam, 'carry_share');
                    const rushTdSum = getWorkloadSum(activeTeam, 'rush_td_share');
                    const recTdSum = getWorkloadSum(activeTeam, 'rec_td_share');
                    
                    return (
                      <>
                        <div style={{textAlign: 'center'}}>
                          <div style={{fontSize: '0.8rem', color: 'var(--text-muted)'}}>Target Share</div>
                          <div style={{
                            fontSize: '1.2rem', 
                            fontWeight: 700, 
                            color: Math.abs(tgtSum - 100.0) > 0.5 ? 'var(--accent-red)' : 'var(--accent-green)'
                          }}>{tgtSum.toFixed(1)}%</div>
                        </div>
                        <div style={{textAlign: 'center'}}>
                          <div style={{fontSize: '0.8rem', color: 'var(--text-muted)'}}>Rushing Share</div>
                          <div style={{
                            fontSize: '1.2rem', 
                            fontWeight: 700, 
                            color: Math.abs(rushSum - 100.0) > 0.5 ? 'var(--accent-red)' : 'var(--accent-green)'
                          }}>{rushSum.toFixed(1)}%</div>
                        </div>
                        <div style={{textAlign: 'center'}}>
                          <div style={{fontSize: '0.8rem', color: 'var(--text-muted)'}}>Rushing TD Share</div>
                          <div style={{
                            fontSize: '1.2rem', 
                            fontWeight: 700, 
                            color: Math.abs(rushTdSum - 100.0) > 0.5 ? 'var(--accent-red)' : 'var(--accent-green)'
                          }}>{rushTdSum.toFixed(1)}%</div>
                        </div>
                        <div style={{textAlign: 'center'}}>
                          <div style={{fontSize: '0.8rem', color: 'var(--text-muted)'}}>Rec TD Share</div>
                          <div style={{
                            fontSize: '1.2rem', 
                            fontWeight: 700, 
                            color: Math.abs(recTdSum - 100.0) > 0.5 ? 'var(--accent-red)' : 'var(--accent-green)'
                          }}>{recTdSum.toFixed(1)}%</div>
                        </div>
                      </>
                    );
                  })()}
                </div>

                <div className="table-container" style={{ maxHeight: '550px' }}>
                  <table>
                    <thead>
                      <tr>
                        <th>Player</th>
                        <th>Pos</th>
                        <th>Salary</th>
                        <th>Target Share</th>
                        <th>Rushing Share</th>
                        <th>Rushing TD Share</th>
                        <th>Rec TD Share</th>
                        <th>Ownership %</th>
                      </tr>
                    </thead>
                    <tbody>
                      {playerOverrides
                        .filter(p => p.team === (rosterTab === 'away' ? selectedGame.away_team : selectedGame.home_team) && p.pos !== 'DST')
                        .map(p => (
                          <tr key={p.name}>
                            <td style={{ fontWeight: 700 }}>{p.name}</td>
                            <td>{p.pos.replace(/[0-9]/g, '')}</td>
                            <td>${p.salary.toLocaleString()}</td>
                            <td>
                              {p.pos !== 'QB' ? (
                                <input type="number" min="0" max="100" step="0.5" value={p.target_share} onChange={(e) => handlePlayerOverride(p.name, 'target_share', e.target.value)} style={{ width: '50px' }} />
                              ) : '-'}
                            </td>
                            <td>
                              <input type="number" min="0" max="100" step="0.5" value={p.carry_share} onChange={(e) => handlePlayerOverride(p.name, 'carry_share', e.target.value)} style={{ width: '50px' }} />
                            </td>
                            <td>
                              <input type="number" min="0" max="100" step="0.5" value={p.rush_td_share} onChange={(e) => handlePlayerOverride(p.name, 'rush_td_share', e.target.value)} style={{ width: '50px' }} />
                            </td>
                            <td>
                              {p.pos !== 'QB' ? (
                                <input type="number" min="0" max="100" step="0.5" value={p.rec_td_share} onChange={(e) => handlePlayerOverride(p.name, 'rec_td_share', e.target.value)} style={{ width: '50px' }} />
                              ) : '-'}
                            </td>
                            <td>
                              <input type="number" min="0.1" max="99.9" step="0.5" value={p.ownership_proj} onChange={(e) => handlePlayerOverride(p.name, 'ownership_proj', e.target.value)} style={{ width: '50px' }} />
                            </td>
                          </tr>
                        ))}
                    </tbody>
                  </table>
                </div>
              </div>
            ) : (
              // DISPLAY COMPILED PROJECTIONS INCLUDING CEILING / FLOOR / OPTIMAL RATES
              <div style={{ display: 'flex', flexDirection: 'column' }}>
                <div className="table-header-row" style={{ marginBottom: '12px', display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '10px' }}>
                  <input 
                    type="text" 
                    placeholder="Search player name..."
                    className="search-input"
                    value={playerSearch}
                    onChange={(e) => setPlayerSearch(e.target.value)}
                    style={{ width: '100%', maxWidth: '240px' }}
                  />
                  <label style={{
                    display: 'flex',
                    alignItems: 'center',
                    gap: '8px',
                    fontSize: '0.82rem',
                    color: 'var(--text-muted)',
                    cursor: 'pointer',
                    userSelect: 'none'
                  }}>
                    <input
                      type="checkbox"
                      checked={filterLowProjections}
                      onChange={() => setFilterLowProjections(!filterLowProjections)}
                      style={{
                        accentColor: 'var(--accent-primary)',
                        cursor: 'pointer',
                        width: '15px',
                        height: '15px'
                      }}
                    />
                    <span>Filter out players with low projections (&lt; 3.0 Proj Pts)</span>
                  </label>
                </div>
                <div className="table-container" style={{ maxHeight: '500px' }}>
                  <table>
                    <thead>
                      <tr>
                        <th onClick={() => handleSort('name')}>Player</th>
                        <th onClick={() => handleSort('pos')}>Pos</th>
                        <th onClick={() => handleSort('salary')}>Salary</th>
                        <th>Percentile Slider</th>
                        <th onClick={() => handleSort('dk_points')}>Proj Pts</th>
                        <th onClick={() => handleSort('floor_dk_points')}>5% Floor</th>
                        <th onClick={() => handleSort('ceiling_dk_points')}>95% Ceiling</th>
                        <th onClick={() => handleSort('ownership_proj')}>Own %</th>
                        <th>Optimal %</th>
                        <th onClick={() => handleSort('ownership_leverage')}>Leverage</th>
                      </tr>
                    </thead>
                    <tbody>
                      {sortedProjections.map(p => {
                        const pct = playerPercentiles[p.name] !== undefined ? playerPercentiles[p.name] : 50;
                        const currentPoints = p.dk_pcts_all ? p.dk_pcts_all[pct] : p.dk_points;
                        const displayName = p.pos === 'DST' ? `${p.team} DST` : p.name;
                        const displayPos = p.pos.replace(/[0-9]/g, '');
                        return (
                          <tr key={p.name}>
                            <td style={{ fontWeight: 700 }}>{displayName}</td>
                            <td>{displayPos}</td>
                            <td>${p.salary.toLocaleString()}</td>
                            <td>
                              <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                                <input 
                                  type="range" 
                                  min="0" 
                                  max="100" 
                                  value={pct} 
                                  onChange={(e) => {
                                    const val = parseInt(e.target.value);
                                    setPlayerPercentiles(prev => ({
                                      ...prev,
                                      [p.name]: val
                                    }));
                                  }}
                                  style={{ width: '85px', height: '4px', cursor: 'pointer' }}
                                />
                                <span style={{ fontSize: '0.8rem', minWidth: '32px', textAlign: 'right', color: 'var(--text-muted)' }}>
                                  {pct === 50 ? 'Med' : `${pct}%`}
                                </span>
                              </div>
                            </td>
                            <td style={{ color: 'var(--accent-primary)', fontWeight: '700' }}>{currentPoints.toFixed(1)}</td>
                            <td style={{ color: 'var(--accent-red)' }}>{p.floor_dk_points}</td>
                            <td style={{ color: 'var(--accent-green)', fontWeight: '600' }}>{p.ceiling_dk_points}</td>
                            <td>{p.ownership_proj}%</td>
                            <td style={{ fontWeight: '600', color: getOptimalRate(p.name) > 0 ? 'var(--accent-cyan)' : 'inherit' }}>
                              {getOptimalRate(p.name)}%
                            </td>
                            <td style={{ color: p.ownership_leverage > 2.0 ? 'var(--accent-green)' : 'inherit', fontWeight: '700' }}>{p.ownership_leverage}x</td>
                          </tr>
                        );
                      })}
                    </tbody>
                  </table>
                </div>
              </div>
            )}
          </div>
        )}

        {/* B. OPTIMIZER PANEL WORKSPACE */}
        {simResults && workspaceView === 'optimizer' && (
          <div className="glass-panel" style={{ minHeight: '500px' }}>
            <div className="panel-header">
              <span className="panel-title">DFS Knapsack Optimizer</span>
            </div>

            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '16px', margin: '14px 0' }}>
              <div>
                <label className="form-label">Number of Lineups</label>
                <select className="form-select" value={lineupCount} onChange={(e) => setLineupCount(parseInt(e.target.value))}>
                  <option value={20}>20 Lineups</option>
                  <option value={50}>50 Lineups</option>
                  <option value={150}>150 Lineups (GPP MME)</option>
                </select>
              </div>

              {selectedSlate === 'TRADITIONAL' && (
                <div style={{ display: 'flex', flexDirection: 'column', gap: '6px', justifyContent: 'center' }}>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '8px', fontSize: '0.85rem' }}>
                    <input type="checkbox" checked={forceStack} onChange={(e) => setForceStack(e.target.checked)} />
                    Force QB + WR Stack
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '8px', fontSize: '0.85rem' }}>
                    <input type="checkbox" checked={forceBringback} onChange={(e) => setForceBringback(e.target.checked)} />
                    Force Opposing Bringback
                  </label>
                </div>
              )}
            </div>

            <button className="btn-primary" onClick={runDfsOptimizer} disabled={solving} style={{ width: '100%', marginBottom: '20px' }}>
              {solving ? 'Solving Lineups...' : 'Solve Optimal GPP Lineups'}
            </button>

            {generatedLineups.length > 0 && (
              <div>
                <div className="flex-between" style={{ marginBottom: '12px' }}>
                  <h3>Generated {generatedLineups.length} Lineups</h3>
                  <button className="btn-primary" onClick={exportLineupsToCsv} style={{ fontSize: '0.8rem', padding: '6px 12px' }}>
                    Export CSV to DraftKings
                  </button>
                </div>

                <div className="table-container" style={{ maxHeight: '350px' }}>
                  <table>
                    <thead>
                      <tr>
                        <th>ID</th>
                        <th>Lineup Configuration</th>
                        <th>Total Salary</th>
                        <th>Sim Projected Pts</th>
                      </tr>
                    </thead>
                    <tbody>
                      {generatedLineups.map(l => (
                        <tr key={l.lineup_id}>
                          <td style={{ fontWeight: '700' }}>#{l.lineup_id}</td>
                          <td style={{ fontSize: '0.8rem', whiteSpace: 'normal', maxWidth: '300px' }}>
                            {l.players.map(p => `${p.slot}: ${p.name}`).join(' | ')}
                          </td>
                          <td>${l.total_salary.toLocaleString()}</td>
                          <td style={{ color: 'var(--accent-primary)', fontWeight: '700' }}>{l.total_points.toFixed(2)}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
