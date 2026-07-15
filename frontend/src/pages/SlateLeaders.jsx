import { useState, useEffect, useMemo, Fragment } from 'react';
import { ApiService } from '../api';

// Configurable thresholds for filtering out low projections
const FILTER_MEDIAN_THRESHOLD = 3.0;
const FILTER_CEILING_THRESHOLD = 6.0;

// Season-long fantasy points calculation helper (no bonuses, fumbles -1, ints -2)
const calculateMeanScore = (player, format) => {
  if (player.pos === 'DST') {
    // DST doesn't have raw offensive stats. Map from backend simulated scores.
    // DraftKings score is PPR-like, FanDuel score is half-PPR/standard-like.
    return format.includes("ppr") ? player.dk_score : player.fd_score;
  }

  const is6PtTD = format.startsWith("6");
  const pprType = format.endsWith("ppr") ? 1.0 : format.endsWith("half") ? 0.5 : 0.0;
  const passTDVal = is6PtTD ? 6.0 : 4.0;
  
  // Stats
  const pYds = player.pYds || 0;
  const pTD = player.pTD || 0;
  const intercept = player.int || 0;
  
  const rYds = player.rYds || 0;
  const rTD = player.rTD || 0;
  
  const rec = player.rec || 0;
  const recYds = player.recYds || 0;
  const recTD = player.recTD || 0;
  
  const fumbles = player.fumbles || 0;
  
  // Scoring formula
  let score = 0;
  score += pYds * 0.04;
  score += pTD * passTDVal;
  score += intercept * -2.0;
  
  score += rYds * 0.1;
  score += rTD * 6.0;
  
  score += rec * pprType;
  score += recYds * 0.1;
  score += recTD * 6.0;
  
  score += fumbles * -1.0;
  
  return parseFloat(score.toFixed(2));
};

// Shift the backend percentile distribution relative to the custom mean difference
const getScoringValue = (player, field, format) => {
  const customMean = calculateMeanScore(player, format);
  
  if (field === 'mean') {
    return customMean;
  }
  
  const pctIdx = typeof field === 'number' ? field : 50; 
  const isPpr = format.endsWith("ppr");
  const basePcts = isPpr ? player.dk_pcts_all : player.fd_pcts_all;
  const baseMean = isPpr ? player.dk_score : player.fd_score;
  
  if (!basePcts || basePcts[pctIdx] === undefined) {
    const baseVal = field === 50 ? (player[`${isPpr ? 'dk' : 'fd'}_p50`] || player[`${isPpr ? 'dk' : 'fd'}_score`]) : (player[`${isPpr ? 'dk' : 'fd'}_score`]);
    const diff = customMean - baseMean;
    return parseFloat((baseVal + diff).toFixed(2));
  }
  
  const diff = customMean - baseMean;
  const val = basePcts[pctIdx] + diff;
  return parseFloat(Math.max(0, val).toFixed(2)); // Scores cannot be negative
};

const statPillStyle = {
  background: 'rgba(255,255,255,0.04)',
  border: '1px solid rgba(255,255,255,0.06)',
  borderRadius: '6px',
  padding: '4px 8px',
  fontSize: '0.75rem',
  color: 'var(--text-white)'
};

export default function SlateLeaders() {
  const [weeks, setWeeks] = useState([]);
  const [selectedWeek, setSelectedWeek] = useState(1);
  const [loading, setLoading] = useState(false);
  const [data, setData] = useState({ players: [], games: [] });
  const [isSandbox, setIsSandbox] = useState(false);
  
  // Filters
  const [selectedPosition, setSelectedPosition] = useState('QB'); // 'QB', 'RB', 'WR', 'TE', 'DST'
  const [searchTerm, setSearchTerm] = useState('');
  const [filterLowProjections, setFilterLowProjections] = useState(true);
  
  // Expanded player detail states
  const [expandedPlayerKey, setExpandedPlayerKey] = useState(null);
  const [playerPercentile, setPlayerPercentile] = useState(50);
  
  // Percentile selection for primary view/sorting via the slider (0 to 100)
  const [selectedPercentile, setSelectedPercentile] = useState(50); // Default to median (50%)
  const [scoringSystem, setScoringSystem] = useState('4_half'); // Default to 4pt Passing TD, Half PPR
 
  // Sorting
  const [sortField, setSortField] = useState(50); // Defaults to median fantasy points (50)
  const [sortAsc, setSortAsc] = useState(false);

  // Sync player percentile slider with global percentile slider on expansion
  useEffect(() => {
    if (expandedPlayerKey) {
      setPlayerPercentile(selectedPercentile);
    }
  }, [expandedPlayerKey, selectedPercentile]);

  // Fetch weeks on load
  useEffect(() => {
    ApiService.getWeeks()
      .then(res => {
        setWeeks(res.weeks || [1]);
        if (res.weeks && res.weeks.length > 0) {
          setSelectedWeek(res.weeks[0]);
        }
        setIsSandbox(ApiService.isSandbox());
      })
      .catch(err => {
        console.error('Error fetching weeks:', err);
        setIsSandbox(ApiService.isSandbox());
      });
  }, []);

  // Fetch weekly projections
  useEffect(() => {
    setLoading(true);
    ApiService.getWeekProjections(selectedWeek)
      .then(res => {
        setData(res);
        setLoading(false);
        setIsSandbox(ApiService.isSandbox());
      })
      .catch(err => {
        console.error('Error fetching weekly projections:', err);
        setLoading(false);
        setIsSandbox(ApiService.isSandbox());
      });
  }, [selectedWeek]);

  // Handle Sort Toggle
  const handleSort = (field) => {
    if (sortField === field) {
      setSortAsc(!sortAsc);
    } else {
      setSortField(field);
      setSortAsc(false);
    }
  };

  const getSortValue = (player, field) => {
    if (typeof field === 'number' || field === 'mean') {
      return getScoringValue(player, field, scoringSystem);
    }
    return player[field] || 0;
  };

  // Filter players
  const filteredPlayers = data.players.filter(player => {
    // 1. Position filter
    if (selectedPosition !== 'ALL') {
      if (player.pos !== selectedPosition) return false;
    }

    // 2. Search Term filter with Comma-Separated Multi-Search
    if (searchTerm.trim() !== '') {
      const terms = searchTerm.split(',').map(t => t.trim().toLowerCase()).filter(Boolean);
      if (terms.length > 0) {
        const teamsInDb = new Set(data.players.map(p => p.team.toLowerCase()));
        const isTeamSearch = teamsInDb.has(terms[0]);

        if (isTeamSearch) {
          const teamMatch = terms.some(t => player.team.toLowerCase() === t || player.team.toLowerCase().includes(t));
          if (!teamMatch) return false;
        } else {
          const nameMatch = terms.some(t => player.name.toLowerCase().includes(t));
          if (!nameMatch) return false;
        }
      }
    }

    // 3. Low Projections Filter
    if (filterLowProjections) {
      const median = getScoringValue(player, 50, scoringSystem);
      const ceiling = getScoringValue(player, 95, scoringSystem); // P95
      if (median <= FILTER_MEDIAN_THRESHOLD || ceiling <= FILTER_CEILING_THRESHOLD) {
        return false;
      }
    }

    return true;
  });

  // Sort players
  const sortedPlayers = [...filteredPlayers].sort((a, b) => {
    const valA = getSortValue(a, sortField);
    const valB = getSortValue(b, sortField);

    if (typeof valA === 'string') {
      return sortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
    }
    return sortAsc ? valA - valB : valB - valA;
  });

  const renderExpandedDetails = (player) => {
    const top12Prob = player.rank_probs ? player.rank_probs[`top12_${scoringSystem}`] : 0.0;
    const top1Prob = player.rank_probs ? player.rank_probs[`top1_${scoringSystem}`] : 0.0;
    const minScore = getScoringValue(player, 5, scoringSystem);
    const maxScore = getScoringValue(player, 95, scoringSystem);
    const medianScore = getScoringValue(player, 50, scoringSystem);
    const currentPctScore = getScoringValue(player, playerPercentile, scoringSystem);
    
    return (
      <div style={{
        padding: '20px',
        background: 'rgba(11, 17, 38, 0.75)',
        borderRadius: '12px',
        border: '1px solid var(--border-glass)',
        margin: '10px 0',
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
        gap: '20px',
        textAlign: 'left'
      }}>
        {/* Left Column: Positional Projections & Ranges */}
        <div style={{ display: 'flex', flexDirection: 'column', gap: '15px' }}>
          <h3 style={{ margin: 0, fontSize: '1.1rem', color: 'var(--accent-primary)', display: 'flex', alignItems: 'center', gap: '8px' }}>
            <span>📈</span> Positional Range &amp; Finish Odds
          </h3>
          
          <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '10px' }}>
            <div style={{ background: 'rgba(255,255,255,0.02)', padding: '10px', borderRadius: '8px', border: '1px solid rgba(255,255,255,0.04)' }}>
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', display: 'block' }}>Top 12 Finish %</span>
              <strong style={{ fontSize: '1.5rem', color: top12Prob > 50 ? 'var(--accent-green)' : 'var(--text-white)' }}>
                {top12Prob}%
              </strong>
              <div style={{ width: '100%', height: '4px', background: 'rgba(255,255,255,0.05)', borderRadius: '2px', marginTop: '6px', overflow: 'hidden' }}>
                <div style={{ width: `${top12Prob}%`, background: 'var(--accent-green)', height: '100%' }}></div>
              </div>
            </div>

            <div style={{ background: 'rgba(255,255,255,0.02)', padding: '10px', borderRadius: '8px', border: '1px solid rgba(255,255,255,0.04)' }}>
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)', display: 'block' }}>Overall #1 Finish %</span>
              <strong style={{ fontSize: '1.5rem', color: top1Prob > 5 ? 'var(--accent-primary)' : 'var(--text-white)' }}>
                {top1Prob}%
              </strong>
              <div style={{ width: '100%', height: '4px', background: 'rgba(255,255,255,0.05)', borderRadius: '2px', marginTop: '6px', overflow: 'hidden' }}>
                <div style={{ width: `${top1Prob}%`, background: 'var(--accent-primary)', height: '100%' }}></div>
              </div>
            </div>
          </div>

          <div style={{ background: 'rgba(255,255,255,0.02)', padding: '12px', borderRadius: '8px', border: '1px solid rgba(255,255,255,0.04)' }}>
            <span style={{ fontSize: '0.8rem', color: 'var(--text-muted)', display: 'block', marginBottom: '8px' }}>Range of Outcomes (Weekly Projections)</span>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
              <div style={{ textAlign: 'center' }}>
                <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)', display: 'block' }}>Floor (P5)</span>
                <strong style={{ color: 'var(--text-white)' }}>{minScore}</strong>
              </div>
              <div style={{ width: '30px', height: '1px', background: 'rgba(255,255,255,0.1)' }}></div>
              <div style={{ textAlign: 'center' }}>
                <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)', display: 'block' }}>Median (P50)</span>
                <strong style={{ color: 'var(--accent-cyan)' }}>{medianScore}</strong>
              </div>
              <div style={{ width: '30px', height: '1px', background: 'rgba(255,255,255,0.1)' }}></div>
              <div style={{ textAlign: 'center' }}>
                <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)', display: 'block' }}>Ceiling (P95)</span>
                <strong style={{ color: 'var(--accent-green)' }}>{maxScore}</strong>
              </div>
            </div>
          </div>
        </div>

        {/* Right Column: Personal Percentile Slider & Average Volume Stats */}
        <div style={{ display: 'flex', flexDirection: 'column', gap: '15px' }}>
          <h3 style={{ margin: 0, fontSize: '1.1rem', color: 'var(--accent-primary)', display: 'flex', alignItems: 'center', gap: '8px' }}>
            <span>🛠️</span> Interactive Percentile Simulator
          </h3>
          
          <div style={{ background: 'rgba(11, 17, 38, 0.4)', padding: '12px', borderRadius: '8px', border: '1px solid rgba(255,255,255,0.04)' }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '6px' }}>
              <span style={{ fontSize: '0.8rem', fontWeight: 600, color: 'var(--text-white)' }}>
                Percentile: <span className="text-cyan">{playerPercentile}%</span>
              </span>
              <span style={{ fontSize: '0.9rem', fontWeight: 'bold', color: 'var(--accent-primary)' }}>
                {currentPctScore} pts
              </span>
            </div>
            <input
              type="range"
              min="0"
              max="100"
              value={playerPercentile}
              onChange={(e) => setPlayerPercentile(parseInt(e.target.value))}
              style={{ width: '100%', cursor: 'pointer' }}
            />
          </div>

          <div>
            <span style={{ fontSize: '0.8rem', color: 'var(--text-muted)', display: 'block', marginBottom: '6px' }}>Average Weekly Volume Baselines</span>
            <div style={{ display: 'flex', flexWrap: 'wrap', gap: '8px' }}>
              {player.pos === 'QB' && (
                <>
                  <div style={statPillStyle}>Pass Att: {player.pAtt}</div>
                  <div style={statPillStyle}>Pass Yds: {player.pYds}</div>
                  <div style={statPillStyle}>Pass TDs: {player.pTD}</div>
                  <div style={statPillStyle}>Ints: {player.int}</div>
                  <div style={statPillStyle}>Carries: {player.rAtt}</div>
                  <div style={statPillStyle}>Rush Yds: {player.rYds}</div>
                </>
              )}
              {player.pos === 'RB' && (
                <>
                  <div style={statPillStyle}>Carries: {player.rAtt}</div>
                  <div style={statPillStyle}>Rush Yds: {player.rYds}</div>
                  <div style={statPillStyle}>Rush TDs: {player.rTD}</div>
                  <div style={statPillStyle}>Targets: {player.targets}</div>
                  <div style={statPillStyle}>Catches: {player.rec}</div>
                  <div style={statPillStyle}>Rec Yds: {player.recYds}</div>
                </>
              )}
              {(player.pos === 'WR' || player.pos === 'TE') && (
                <>
                  <div style={statPillStyle}>Targets: {player.targets}</div>
                  <div style={statPillStyle}>Catches: {player.rec}</div>
                  <div style={statPillStyle}>Rec Yds: {player.recYds}</div>
                  <div style={statPillStyle}>Rec TDs: {player.recTD}</div>
                  <div style={statPillStyle}>Carries: {player.rAtt}</div>
                  <div style={statPillStyle}>Rush Yds: {player.rYds}</div>
                </>
              )}
              {player.pos === 'DST' && (
                <>
                  <div style={statPillStyle}>Sacks: {player.sacks || 2.1}</div>
                  <div style={statPillStyle}>Ints: {player.int || 0.9}</div>
                  <div style={statPillStyle}>Fumbles Rec: {player.fumbles || 0.8}</div>
                </>
              )}
            </div>
          </div>
        </div>
      </div>
    );
  };

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '20px', width: '100%' }}>
      {/* Header Panel */}
      <div className="glass-panel" style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '15px' }}>
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
            <h1 style={{ margin: 0, fontSize: '2rem' }}>
              📊 Week {selectedWeek} Weekly Matchups Summary
            </h1>
            {/* Connection Status Badge */}
            <div style={{
              display: 'inline-flex',
              alignItems: 'center',
              gap: '6px',
              fontSize: '0.75rem',
              fontWeight: 600,
              padding: '4px 8px',
              borderRadius: '6px',
              background: isSandbox ? 'rgba(245, 158, 11, 0.1)' : 'rgba(16, 185, 129, 0.1)',
              color: isSandbox ? '#f59e0b' : '#10b981',
              border: `1px solid ${isSandbox ? 'rgba(245, 158, 11, 0.2)' : 'rgba(16, 185, 129, 0.2)'}`
            }}>
              <span style={{
                width: '6px',
                height: '6px',
                borderRadius: '50%',
                background: isSandbox ? '#f59e0b' : '#10b981',
                display: 'inline-block'
              }}></span>
              {isSandbox ? 'Sandbox Fallback' : 'Live API'}
            </div>
          </div>
          <p style={{ color: 'var(--text-muted)', marginTop: '4px' }}>
            Aggregated Monte Carlo projections and percentiles across all games.
          </p>
        </div>
        
        {/* Week Selector */}
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
          <span style={{ fontWeight: 600, color: 'var(--text-white)' }}>Select Week:</span>
          <select 
            value={selectedWeek} 
            onChange={(e) => {
              setSelectedWeek(Number(e.target.value));
            }}
            style={{
              background: 'var(--bg-glass-active)',
              color: 'var(--text-white)',
              border: '1px solid var(--border-glass)',
              padding: '8px 16px',
              borderRadius: '8px',
              outline: 'none',
              cursor: 'pointer',
              fontWeight: 600
            }}
          >
            {weeks.map(w => (
              <option key={w} value={w}>Week {w}</option>
            ))}
          </select>
        </div>
      </div>

      {/* Main Content Dashboard */}
      <div className="glass-panel" style={{ display: 'flex', flexDirection: 'column', gap: '20px' }}>
        {/* Filter Controls Grid */}
        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))',
          gap: '15px',
          background: 'rgba(11, 17, 38, 0.4)',
          padding: '16px',
          borderRadius: '12px',
          border: '1px solid var(--border-glass)'
        }}>
          {/* Scoring Selector */}
          <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
            <label style={{ fontSize: '0.85rem', fontWeight: 600, color: 'var(--text-muted)' }}>Scoring Format</label>
            <select
              value={scoringSystem}
              onChange={(e) => setScoringSystem(e.target.value)}
              style={{
                background: 'var(--bg-deep)',
                color: 'var(--text-white)',
                border: '1px solid var(--border-glass)',
                padding: '10px',
                borderRadius: '8px',
                outline: 'none',
                cursor: 'pointer',
                fontSize: '0.95rem'
              }}
            >
              <option value="4_ppr">4 Pt Passing TD: PPR</option>
              <option value="4_half">4 Pt Passing TD: Half PPR</option>
              <option value="4_std">4 Pt Passing TD: Standard</option>
              <option value="6_ppr">6 Pt Passing TD: PPR</option>
              <option value="6_half">6 Pt Passing TD: Half PPR</option>
              <option value="6_std">6 Pt Passing TD: Standard</option>
            </select>
          </div>

          {/* Primary Metric (Percentile Slider Display) */}
          <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
            <label style={{ fontSize: '0.85rem', fontWeight: 600, color: 'var(--text-muted)' }}>
              Percentile Slider: <span className="text-cyan" style={{ fontWeight: 'bold' }}>{selectedPercentile === 50 ? 'P50 (Median)' : `P${selectedPercentile}`}</span>
            </label>
            <div style={{ display: 'flex', alignItems: 'center', gap: '10px', marginTop: '4px' }}>
              <input
                type="range"
                min="0"
                max="100"
                value={selectedPercentile}
                onChange={(e) => {
                  const val = parseInt(e.target.value);
                  setSelectedPercentile(val);
                  setSortField(val);
                }}
                style={{ flexGrow: 1, cursor: 'pointer' }}
              />
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.75rem', color: 'var(--text-muted)' }}>
              <span>Floor (P0)</span>
              <span>Median (P50)</span>
              <span>Ceiling (P100)</span>
            </div>
          </div>

          {/* Search Input */}
          <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
            <label style={{ fontSize: '0.85rem', fontWeight: 600, color: 'var(--text-muted)' }}>Search Player / Team</label>
            <input
              type="text"
              placeholder="e.g. PHI, KC or Barkley, Hurts..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              style={{
                background: 'var(--bg-deep)',
                color: 'var(--text-white)',
                border: '1px solid var(--border-glass)',
                padding: '10px 14px',
                borderRadius: '8px',
                outline: 'none',
                fontSize: '0.95rem'
              }}
            />
          </div>
        </div>

        {/* Position Tab Navigation */}
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '10px' }}>
          <div className="tab-switcher" style={{ width: 'auto', alignSelf: 'flex-start' }}>
            {['QB', 'RB', 'WR', 'TE', 'DST'].map(pos => (
              <button
                key={pos}
                className={`tab-btn ${selectedPosition === pos ? 'active' : ''}`}
                onClick={() => setSelectedPosition(pos)}
                style={{ padding: '10px 20px', minWidth: '80px' }}
              >
                {pos}
              </button>
            ))}
          </div>

          <div style={{ fontSize: '0.9rem', color: 'var(--text-muted)' }}>
            Showing <strong className="text-cyan">{sortedPlayers.length}</strong> players
          </div>
        </div>

        {/* Projections Leaders Table */}
        {loading ? (
          <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', padding: '60px 0', gap: '15px' }}>
            <div className="spinner"></div>
            <p style={{ color: 'var(--text-muted)' }}>Running simulations aggregation analytics...</p>
          </div>
        ) : (isSandbox && data.players.length === 0) ? (
          <div style={{ 
            textAlign: 'center', 
            padding: '50px 20px', 
            color: 'var(--text-muted)', 
            border: '1px dashed #f59e0b', 
            borderRadius: '12px',
            background: 'rgba(245, 158, 11, 0.05)',
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            gap: '15px'
          }}>
            <span style={{ fontSize: '2.5rem' }}>⚠️</span>
            <h3 style={{ color: '#f59e0b', margin: 0 }}>Weekly Projections Unreachable</h3>
            <p style={{ maxWidth: '500px', margin: 0, fontSize: '0.95rem' }}>
              {data.sandboxReason || 'The connection to the NFLSims API server failed. Please ensure the backend is running on port 8002.'}
            </p>
            <button 
              onClick={() => {
                setLoading(true);
                ApiService.getWeekProjections(selectedWeek)
                  .then(res => {
                    setData(res);
                    setLoading(false);
                    setIsSandbox(ApiService.isSandbox());
                  })
                  .catch(() => {
                    setLoading(false);
                    setIsSandbox(ApiService.isSandbox());
                  });
              }}
              className="btn-primary"
              style={{ marginTop: '10px', background: '#f59e0b', color: '#000', padding: '8px 16px', borderRadius: '6px', border: 'none', fontWeight: 600, cursor: 'pointer' }}
            >
              Retry Connection
            </button>
          </div>
        ) : sortedPlayers.length === 0 ? (
          <div style={{ textAlign: 'center', padding: '40px 0', color: 'var(--text-muted)', border: '1px dashed var(--border-glass)', borderRadius: '12px' }}>
            No players matching current filter selection.
          </div>
        ) : (
          <div className="table-container">
            <table>
              <thead>
                <tr>
                  <th onClick={() => handleSort('name')}>Player</th>
                  <th onClick={() => handleSort('pos')}>Pos</th>
                  <th onClick={() => handleSort('team')}>Team</th>
                  <th onClick={() => handleSort('opponent')}>Opp</th>

                  {/* Fantasy Projections Percentiles */}
                  <th onClick={() => handleSort(50)} style={{ color: selectedPercentile === 50 ? 'var(--accent-primary)' : 'inherit' }}>P50 (Med)</th>
                  {selectedPercentile !== 50 && (
                    <th onClick={() => handleSort(selectedPercentile)} style={{ color: 'var(--accent-primary)' }}>
                      P{selectedPercentile}
                    </th>
                  )}
                  <th onClick={() => handleSort('mean')} style={{ color: selectedPercentile === 'mean' ? 'var(--accent-primary)' : 'inherit' }}>Mean</th>

                  {/* Context-aware volume metrics */}
                  {(selectedPosition === 'ALL' || selectedPosition === 'QB') && (
                    <>
                      <th onClick={() => handleSort('pAtt')}>P.Att</th>
                      <th onClick={() => handleSort('pYds')}>P.Yds</th>
                      <th onClick={() => handleSort('pTD')}>P.TD</th>
                    </>
                  )}
                  {(selectedPosition === 'ALL' || selectedPosition === 'RB' || selectedPosition === 'QB' || selectedPosition === 'WR' || selectedPosition === 'TE') && (
                    <>
                      <th onClick={() => handleSort('rAtt')}>Carries</th>
                      <th onClick={() => handleSort('rYds')}>R.Yds</th>
                      <th onClick={() => handleSort('rTD')}>R.TD</th>
                    </>
                  )}
                  {(selectedPosition === 'ALL' || selectedPosition === 'WR' || selectedPosition === 'TE' || selectedPosition === 'RB') && (
                    <>
                      <th onClick={() => handleSort('targets')}>Targets</th>
                      <th onClick={() => handleSort('rec')}>Catches</th>
                      <th onClick={() => handleSort('recYds')}>Rec.Yds</th>
                      <th onClick={() => handleSort('recTD')}>Rec.TD</th>
                    </>
                  )}
                </tr>
              </thead>
              <tbody>
                {sortedPlayers.map((player) => {
                  const p50Val = getScoringValue(player, 50, scoringSystem);
                  const currentPctVal = selectedPercentile !== 50 ? getScoringValue(player, selectedPercentile, scoringSystem) : p50Val;
                  const meanVal = getScoringValue(player, 'mean', scoringSystem);
                  const playerKey = `${player.name}-${player.team}`;
                  const isExpanded = expandedPlayerKey === playerKey;
                  
                  return (
                    <Fragment key={playerKey}>
                      <tr 
                        onClick={() => setExpandedPlayerKey(isExpanded ? null : playerKey)}
                        style={{ cursor: 'pointer', background: isExpanded ? 'rgba(0, 242, 254, 0.04)' : 'transparent' }}
                      >
                        <td style={{ fontWeight: 'bold', color: 'var(--text-white)' }}>
                          <span style={{ marginRight: '8px', color: 'var(--text-muted)', fontSize: '0.8rem' }}>
                            {isExpanded ? '▼' : '▶'}
                          </span>
                          {player.name}
                        </td>
                        <td>
                          <span style={{
                            padding: '2px 6px',
                            borderRadius: '4px',
                            fontSize: '0.8rem',
                            background: 'rgba(255,255,255,0.06)',
                            color: player.pos === 'QB' ? 'var(--accent-primary)' : 
                                   player.pos === 'RB' ? 'var(--accent-green)' : 
                                   player.pos === 'WR' ? 'var(--accent-purple)' : 'var(--accent-gold)'
                          }}>
                            {player.pos}
                          </span>
                        </td>
                        <td style={{ fontWeight: 600 }}>{player.team}</td>
                        <td style={{ color: 'var(--text-muted)', fontSize: '0.9rem' }}>{player.opponent}</td>

                        {/* Percentiles */}
                        <td style={{ color: selectedPercentile === 50 ? 'var(--accent-primary)' : 'inherit', fontWeight: 'bold', background: 'rgba(255,255,255,0.02)' }}>
                          {p50Val}
                        </td>
                        {selectedPercentile !== 50 && (
                          <td style={{ color: 'var(--accent-primary)', fontWeight: 'bold' }}>
                            {currentPctVal}
                          </td>
                        )}
                        <td style={{ color: selectedPercentile === 'mean' ? 'var(--accent-primary)' : 'var(--accent-green)', fontWeight: '600' }}>
                          {meanVal}
                        </td>

                        {/* QB Passing volume stats */}
                        {(selectedPosition === 'ALL' || selectedPosition === 'QB') && (
                          <>
                            <td>{player.pos === 'QB' ? player.pAtt : '-'}</td>
                            <td>{player.pos === 'QB' ? player.pYds : '-'}</td>
                            <td>{player.pos === 'QB' ? player.pTD : '-'}</td>
                          </>
                        )}

                        {/* Rushing volume stats */}
                        {(selectedPosition === 'ALL' || selectedPosition === 'RB' || selectedPosition === 'QB' || selectedPosition === 'WR' || selectedPosition === 'TE') && (
                          <>
                            <td>{player.pos !== 'DST' ? player.rAtt : '-'}</td>
                            <td>{player.pos !== 'DST' ? player.rYds : '-'}</td>
                            <td>{player.pos !== 'DST' ? player.rTD : '-'}</td>
                          </>
                        )}

                        {/* Receiving volume stats */}
                        {(selectedPosition === 'ALL' || selectedPosition === 'WR' || selectedPosition === 'TE' || selectedPosition === 'RB') && (
                          <>
                            <td>{player.pos !== 'QB' && player.pos !== 'DST' ? player.targets : '-'}</td>
                            <td>{player.pos !== 'QB' && player.pos !== 'DST' ? player.rec : '-'}</td>
                            <td>{player.pos !== 'QB' && player.pos !== 'DST' ? player.recYds : '-'}</td>
                            <td>{player.pos !== 'QB' && player.pos !== 'DST' ? player.recTD : '-'}</td>
                          </>
                        )}
                      </tr>
                      {isExpanded && (
                        <tr style={{ background: 'rgba(0,0,0,0.2)' }}>
                          <td colSpan="100%">
                            {renderExpandedDetails(player)}
                          </td>
                        </tr>
                      )}
                    </Fragment>
                  );
                })}
              </tbody>
            </table>
          </div>
        )}

        {/* Low Projections Toggle Checkbox at the bottom */}
        <div style={{
          display: 'flex',
          justifyContent: 'flex-start',
          paddingTop: '15px',
          borderTop: '1px solid var(--border-glass)',
          marginTop: '10px'
        }}>
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
            <span>Filter out players with low projections (Median ≤ {FILTER_MEDIAN_THRESHOLD} or Ceiling ≤ {FILTER_CEILING_THRESHOLD})</span>
          </label>
        </div>
      </div>
    </div>
  );
}
