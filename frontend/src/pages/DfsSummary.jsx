import { useState, useMemo, Fragment } from 'react';
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

const statPillStyle = {
  background: 'rgba(255,255,255,0.04)',
  border: '1px solid rgba(255,255,255,0.06)',
  borderRadius: '6px',
  padding: '4px 8px',
  fontSize: '0.75rem',
  color: 'var(--text-white)'
};

export default function DfsSummary({
  selectedWeek,
  selectedSlate,
  weekProjections,
  allSimResults,
  simResults,
  generatedLineups,
  setCurrentPage
}) {
  // --- LOCAL UI STATES ---
  const [playerSearch, setPlayerSearch] = useState('');
  const [scoringFormat, setScoringFormat] = useState('DK');
  const [selectedPosition, setSelectedPosition] = useState('ALL');
  const [selectedPercentile, setSelectedPercentile] = useState(50);
  const [expandedPlayerKey, setExpandedPlayerKey] = useState(null);
  const [playerPercentile, setPlayerPercentile] = useState(50);
  const [sortField, setSortField] = useState('dk_points');
  const [sortAsc, setSortAsc] = useState(false);
  const [filterLowProjections, setFilterLowProjections] = useState(false);

  const handleSort = (field) => {
    if (sortField === field) {
      setSortAsc(!sortAsc);
    } else {
      setSortField(field);
      setSortAsc(false);
    }
  };

  const getOptimalRate = (playerName) => {
    if (!generatedLineups || generatedLineups.length === 0) return 0;
    const appearances = generatedLineups.filter(lineup => 
      lineup.players.some(p => p.name === playerName)
    ).length;
    return parseFloat(((appearances / generatedLineups.length) * 100).toFixed(1));
  };

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
    const basePcts = format === 'FD' ? player.fd_pcts_all : player.dk_pcts_all;
    const baseMean = format === 'FD' ? (player.fd_points || player.fd_score || 0) : (player.dk_points || player.dk_score || 0);

    if (field === 'mean') return parseFloat(baseMean.toFixed(2));

    const pctIdx = typeof field === 'number' ? field : 50;
    if (basePcts && basePcts[pctIdx] !== undefined) {
      return parseFloat(basePcts[pctIdx].toFixed(2));
    }

    const baseVal = field === 50 ? (player.pos === 'DST' ? baseMean : (player.dk_p50 || baseMean)) : baseMean;
    return parseFloat(baseVal.toFixed(2));
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
        res.projections.forEach(p => {
          if (p.team) simulatedTeams.add(p.team);
        });
        if (res.away_team) simulatedTeams.add(res.away_team);
        if (res.home_team) simulatedTeams.add(res.home_team);
      }
    });
    
    // If the currently selected game is simulated but not in allSimResults, make sure we include it
    if (simResults) {
      const localSimTeams = new Set();
      if (simResults.projections) {
        simResults.projections.forEach(p => {
          if (p.team) localSimTeams.add(p.team);
        });
      }
      if (simResults.away_team) localSimTeams.add(simResults.away_team);
      if (simResults.home_team) localSimTeams.add(simResults.home_team);

      let alreadyIncluded = false;
      localSimTeams.forEach(t => {
        if (simulatedTeams.has(t)) alreadyIncluded = true;
      });

      if (!alreadyIncluded && simResults.projections) {
        simulatedProjections.push(...simResults.projections);
        localSimTeams.forEach(t => simulatedTeams.add(t));
      }
    }
    
    // Fall back to weekProjections for teams not simulated yet
    const finalOtherProjections = (weekProjections || [])
      .filter(p => !simulatedTeams.has(p.team))
      .map(p => ({
        ...p,
        dk_points: p.dk_score || p.dk_points || 0,
        fd_points: p.fd_score || p.fd_points || 0,
        dk_value: p.salary > 0 ? (p.dk_score || p.dk_points || 0) / (p.salary / 1000.0) : 0,
        fd_value: p.salary > 0 ? (p.fd_score || p.fd_points || 0) / (p.salary / 1000.0) : 0,
        ownership_proj: p.ownership_proj !== undefined ? p.ownership_proj : 12.5,
        ownership_leverage: p.ownership_leverage !== undefined ? p.ownership_leverage : 0,
        dk_pcts_all: p.dk_pcts_all || [p.dk_score || p.dk_points || 0].fill(p.dk_score || p.dk_points || 0, 0, 101),
        fd_pcts_all: p.fd_pcts_all || [p.fd_score || p.fd_points || 0].fill(p.fd_score || p.fd_points || 0, 0, 101)
      }));

    return [...simulatedProjections, ...finalOtherProjections];
  }, [simResults, allSimResults, weekProjections, selectedSlate]);

  const sortedProjections = useMemo(() => {
    if (!simResults && Object.keys(allSimResults || {}).length === 0 && (!weekProjections || weekProjections.length === 0)) return [];
    const baseList = combinedProjections;
    console.log("DEBUG DFS Summary: selectedPosition =", selectedPosition);
    console.log("DEBUG DFS Summary: baseList length =", baseList.length);
    const filtered = [...baseList]
      .filter(p => p.name.toLowerCase().includes(playerSearch.toLowerCase()))
      .filter(p => {
        if (!p || !p.pos) return false;
        if (selectedPosition === 'ALL') return true;
        const match = p.pos.toUpperCase() === selectedPosition.toUpperCase();
        if (p.name.includes("Brown")) {
          console.log(`DEBUG DFS Summary: Checked ${p.name}, pos = ${p.pos}, selectedPosition = ${selectedPosition}, match = ${match}`);
        }
        return match;
      });
    console.log("DEBUG DFS Summary: filtered length =", filtered.length);
    return filtered
      .filter(p => {
        if (!filterLowProjections) return true;
        const p50Val = getPlayerPointsAtPercentile(p, 50, scoringFormat);
        return p50Val >= 3.0;
      })
      .sort((a, b) => {
        let valA, valB;
        if (typeof sortField === 'number') {
          valA = getPlayerPointsAtPercentile(a, sortField, scoringFormat);
          valB = getPlayerPointsAtPercentile(b, sortField, scoringFormat);
        } else if (sortField === 'mean') {
          valA = getPlayerPointsAtPercentile(a, 'mean', scoringFormat);
          valB = getPlayerPointsAtPercentile(b, 'mean', scoringFormat);
        } else if (sortField === 'leverage') {
          const optA = a.optimal_pct !== undefined ? a.optimal_pct : getOptimalRate(a.name);
          const optB = b.optimal_pct !== undefined ? b.optimal_pct : getOptimalRate(b.name);
          valA = optA - a.ownership_proj;
          valB = optB - b.ownership_proj;
        } else if (sortField === 'optimal_pct') {
          valA = a.optimal_pct !== undefined ? a.optimal_pct : getOptimalRate(a.name);
          valB = b.optimal_pct !== undefined ? b.optimal_pct : getOptimalRate(b.name);
        } else if (sortField === 'salary') {
          valA = a.salary || 3000;
          valB = b.salary || 3000;
        } else if (sortField === 'name' || sortField === 'pos' || sortField === 'team' || sortField === 'opponent') {
          valA = a[sortField] || '';
          valB = b[sortField] || '';
        } else {
          valA = a[sortField] !== undefined ? a[sortField] : getPlayerPoints(a, scoringFormat);
          valB = b[sortField] !== undefined ? b[sortField] : getPlayerPoints(b, scoringFormat);
        }

        if (typeof valA === 'string') {
          return sortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
        }
        return sortAsc ? valA - valB : valB - valA;
      });
  }, [simResults, allSimResults, combinedProjections, playerSearch, filterLowProjections, sortField, sortAsc, scoringFormat, selectedPosition]);

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      {/* Header with navigation back */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '20px' }}>
        <div>
          <h1 style={{ margin: 0, color: 'var(--text-white)' }}>Slate DFS Summary</h1>
          <p style={{ fontSize: '0.9rem', color: 'var(--text-muted)' }}>
            Week {selectedWeek} • Slate Focus: {selectedSlate === 'TRADITIONAL' ? 'Traditional Multi-Game' : 'Showdown Single-Game'}
          </p>
        </div>
        <button
          onClick={() => {
            setCurrentPage('simulator');
            window.location.hash = 'simulator';
          }}
          style={{
            padding: '8px 16px',
            background: 'var(--accent-primary)',
            color: '#000',
            border: 'none',
            borderRadius: '8px',
            cursor: 'pointer',
            fontWeight: 700,
            fontSize: '0.9rem',
            transition: 'all 0.2s',
            boxShadow: 'var(--glow-cyan)'
          }}
        >
          🏈 Back to Simulator
        </button>
      </div>

      <div className="glass-panel" style={{ minHeight: '500px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '20px' }}>
          <h2 style={{ display: 'flex', alignItems: 'center', gap: '10px', flexWrap: 'wrap' }}>
            <span>DFS Projections & Target Summary (Current Filter: {selectedPosition})</span>
            {ApiService.isSandbox() && <SandboxBadge />}
          </h2>
          <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
            <span style={{ fontSize: '0.82rem', color: 'var(--text-muted)' }}>Scoring:</span>
            <select
              value={scoringFormat}
              onChange={(e) => setScoringFormat(e.target.value)}
              style={{
                background: 'var(--bg-deep)',
                color: 'var(--text-white)',
                border: '1px solid var(--border-glass)',
                padding: '6px 12px',
                borderRadius: '8px',
                outline: 'none',
                cursor: 'pointer',
                fontSize: '0.85rem',
                fontWeight: 600
              }}
            >
              <option value="DK">DraftKings (DK)</option>
              <option value="FD">FanDuel (FD)</option>
              <option value="UNDERDOG">Underdog</option>
            </select>
          </div>
        </div>

        {/* Filter and Simulator controls */}
        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))',
          gap: '15px',
          background: 'rgba(11, 17, 38, 0.4)',
          padding: '16px',
          borderRadius: '12px',
          border: '1px solid var(--border-glass)',
          marginBottom: '20px'
        }}>
          {/* Percentile Slider */}
          <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
            <label style={{ fontSize: '0.85rem', fontWeight: 600, color: 'var(--text-muted)' }}>
              Percentile Slider: <span className="text-cyan" style={{ fontWeight: 'bold' }}>{selectedPercentile === 50 ? 'P50 (Median)' : `P${selectedPercentile}`}</span>
            </label>
            <div style={{ display: 'flex', alignItems: 'center', gap: '10px', marginTop: '4px' }}>
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>Floor (P5)</span>
              <input
                type="range"
                min="5"
                max="95"
                step="5"
                value={selectedPercentile}
                onChange={(e) => setSelectedPercentile(parseInt(e.target.value))}
                style={{ flexGrow: 1, cursor: 'pointer' }}
              />
              <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>Ceiling (P95)</span>
            </div>
          </div>

          {/* Search Box */}
          <div style={{ display: 'flex', flexDirection: 'column', gap: '6px' }}>
            <label style={{ fontSize: '0.85rem', fontWeight: 600, color: 'var(--text-muted)' }}>Search Player</label>
            <input
              type="text"
              placeholder="Search player name..."
              className="search-input"
              value={playerSearch}
              onChange={(e) => setPlayerSearch(e.target.value)}
              style={{
                background: 'var(--bg-deep)',
                color: 'var(--text-white)',
                border: '1px solid var(--border-glass)',
                padding: '8px 12px',
                borderRadius: '8px',
                outline: 'none',
                fontSize: '0.9rem',
                width: '100%'
              }}
            />
          </div>

          {/* Low Projections Checkbox */}
          <div style={{ display: 'flex', alignItems: 'center', height: '100%', paddingTop: '20px' }}>
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
        </div>

        {/* Position Tab Navigation */}
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '10px', marginBottom: '15px' }}>
          <div className="tab-switcher" style={{ width: 'auto', alignSelf: 'flex-start' }}>
            {['ALL', 'QB', 'RB', 'WR', 'TE', 'DST'].map(pos => (
              <button
                key={pos}
                className={`tab-btn ${selectedPosition === pos ? 'active' : ''}`}
                onClick={() => setSelectedPosition(pos)}
                style={{ padding: '8px 16px', minWidth: '70px' }}
              >
                {pos}
              </button>
            ))}
          </div>
          <div style={{ fontSize: '0.9rem', color: 'var(--text-muted)' }}>
            Showing <strong className="text-cyan">{sortedProjections.length}</strong> players
          </div>
        </div>

        <div className="table-container" style={{ maxHeight: '600px' }}>
          <table>
            <thead>
              <tr>
                <th onClick={() => handleSort('name')} style={{ cursor: 'pointer' }}>Player</th>
                <th onClick={() => handleSort('pos')} style={{ cursor: 'pointer' }}>Pos</th>
                <th onClick={() => handleSort('team')} style={{ cursor: 'pointer' }}>Team</th>
                <th onClick={() => handleSort('opponent')} style={{ cursor: 'pointer' }}>Opp</th>
                <th onClick={() => handleSort('salary')} style={{ cursor: 'pointer' }}>Salary</th>
                <th onClick={() => handleSort('ownership_proj')} style={{ cursor: 'pointer' }}>Own %</th>
                <th onClick={() => handleSort('optimal_pct')} style={{ cursor: 'pointer' }}>Optimal %</th>
                <th onClick={() => handleSort('leverage')} style={{ cursor: 'pointer' }}>Leverage</th>
                <th onClick={() => handleSort(50)} style={{ cursor: 'pointer', color: selectedPercentile === 50 ? 'var(--accent-primary)' : 'inherit' }}>P50 (Med)</th>
                {selectedPercentile !== 50 && (
                  <th onClick={() => handleSort(selectedPercentile)} style={{ cursor: 'pointer', color: 'var(--accent-primary)' }}>
                    P{selectedPercentile}
                  </th>
                )}
                <th onClick={() => handleSort('mean')} style={{ cursor: 'pointer', color: selectedPercentile === 'mean' ? 'var(--accent-primary)' : 'inherit' }}>Mean</th>
                
                {/* QB volume stats */}
                {(selectedPosition === 'ALL' || selectedPosition === 'QB') && (
                  <>
                    <th onClick={() => handleSort('pAtt')} style={{ cursor: 'pointer' }}>P.Att</th>
                    <th onClick={() => handleSort('pYds')} style={{ cursor: 'pointer' }}>P.Yds</th>
                    <th onClick={() => handleSort('pTD')} style={{ cursor: 'pointer' }}>P.TD</th>
                  </>
                )}
                {/* Rush volume stats */}
                {(selectedPosition === 'ALL' || selectedPosition === 'RB' || selectedPosition === 'QB' || selectedPosition === 'WR' || selectedPosition === 'TE') && (
                  <>
                    <th onClick={() => handleSort('rAtt')} style={{ cursor: 'pointer' }}>Carries</th>
                    <th onClick={() => handleSort('rYds')} style={{ cursor: 'pointer' }}>R.Yds</th>
                    <th onClick={() => handleSort('rTD')} style={{ cursor: 'pointer' }}>R.TD</th>
                  </>
                )}
                {/* Rec volume stats */}
                {(selectedPosition === 'ALL' || selectedPosition === 'WR' || selectedPosition === 'TE' || selectedPosition === 'RB') && (
                  <>
                    <th onClick={() => handleSort('targets')} style={{ cursor: 'pointer' }}>Targets</th>
                    <th onClick={() => handleSort('rec')} style={{ cursor: 'pointer' }}>Catches</th>
                    <th onClick={() => handleSort('recYds')} style={{ cursor: 'pointer' }}>Rec.Yds</th>
                    <th onClick={() => handleSort('recTD')} style={{ cursor: 'pointer' }}>Rec.TD</th>
                  </>
                )}
              </tr>
            </thead>
            <tbody>
              {sortedProjections.length === 0 ? (
                <tr>
                  <td colSpan={15} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>
                    No simulation results available. Run simulations on the DFS Simulator page to populate summary data.
                  </td>
                </tr>
              ) : (
                sortedProjections.map(p => {
                  const displayName = p.pos === 'DST' ? `${p.team} DST` : p.name;
                  const displayPos = p.pos.replace(/[0-9]/g, '');
                  const optimalVal = p.optimal_pct !== undefined ? p.optimal_pct : getOptimalRate(p.name);
                  const optimalCptVal = p.optimal_cpt_pct !== undefined ? p.optimal_cpt_pct : 0.0;
                  const optimalFlexVal = p.optimal_flex_pct !== undefined ? p.optimal_flex_pct : 0.0;
                  const leverageVal = parseFloat((optimalVal - p.ownership_proj).toFixed(2));
                  
                  const p50Val = getPlayerPointsAtPercentile(p, 50, scoringFormat);
                  const currentPctVal = selectedPercentile !== 50 ? getPlayerPointsAtPercentile(p, selectedPercentile, scoringFormat) : p50Val;
                  const meanVal = getPlayerPointsAtPercentile(p, 'mean', scoringFormat);
                  
                  const playerKey = `${p.name}-${p.team}`;
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
                          {displayName}
                        </td>
                        <td>
                          <span style={{
                            padding: '2px 6px',
                            borderRadius: '4px',
                            fontSize: '0.8rem',
                            background: 'rgba(255,255,255,0.06)',
                            color: p.pos === 'QB' ? 'var(--accent-primary)' : 
                                   p.pos === 'RB' ? 'var(--accent-green)' : 
                                   p.pos === 'WR' ? 'var(--accent-purple)' : 'var(--accent-gold)'
                          }}>
                            {displayPos}
                          </span>
                        </td>
                        <td style={{ fontWeight: 600 }}>{p.team}</td>
                        <td style={{ color: 'var(--text-muted)', fontSize: '0.9rem' }}>{p.opponent || 'vs --'}</td>
                        <td>${p.salary ? p.salary.toLocaleString() : '3,000'}</td>
                        <td>{p.ownership_proj !== undefined ? p.ownership_proj : 12.5}%</td>
                        <td style={{ verticalAlign: 'middle' }}>
                          <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center' }}>
                            <span style={{ fontWeight: 700, color: 'var(--accent-cyan)' }}>{optimalVal.toFixed(1)}%</span>
                            {selectedSlate === 'SHOWDOWN' && (optimalCptVal > 0 || optimalFlexVal > 0) && (
                              <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>
                                C:{optimalCptVal.toFixed(0)}% / F:{optimalFlexVal.toFixed(0)}%
                              </span>
                            )}
                          </div>
                        </td>
                        <td style={{ color: leverageVal > 0 ? 'var(--accent-green)' : leverageVal < 0 ? 'var(--accent-red)' : 'inherit', fontWeight: 700 }}>
                          {leverageVal > 0 ? `+${leverageVal}` : leverageVal}%
                        </td>
                        <td style={{ color: selectedPercentile === 50 ? 'var(--accent-primary)' : 'inherit', fontWeight: 'bold', background: 'rgba(255,255,255,0.02)' }}>{p50Val.toFixed(1)}</td>
                        {selectedPercentile !== 50 && (
                          <td style={{ color: 'var(--accent-primary)', fontWeight: 'bold' }}>{currentPctVal.toFixed(1)}</td>
                        )}
                        <td style={{ color: selectedPercentile === 'mean' ? 'var(--accent-primary)' : 'var(--accent-green)', fontWeight: '600' }}>{meanVal.toFixed(1)}</td>
                        
                        {/* QB Passing volume stats */}
                        {(selectedPosition === 'ALL' || selectedPosition === 'QB') && (
                          <>
                            <td>{p.pos === 'QB' ? (p.pAtt !== undefined ? p.pAtt.toFixed(1) : p.pAtt_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos === 'QB' ? (p.pYds !== undefined ? p.pYds.toFixed(1) : p.pYds_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos === 'QB' ? (p.pTD !== undefined ? p.pTD.toFixed(1) : p.pTD_avg?.toFixed(1) || '-') : '-'}</td>
                          </>
                        )}
                        {/* Rushing volume stats */}
                        {(selectedPosition === 'ALL' || selectedPosition === 'RB' || selectedPosition === 'QB' || selectedPosition === 'WR' || selectedPosition === 'TE') && (
                          <>
                            <td>{p.pos !== 'DST' ? (p.rAtt !== undefined ? p.rAtt.toFixed(1) : p.rAtt_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos !== 'DST' ? (p.rYds !== undefined ? p.rYds.toFixed(1) : p.rYds_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos !== 'DST' ? (p.rTD !== undefined ? p.rTD.toFixed(1) : p.rTD_avg?.toFixed(1) || '-') : '-'}</td>
                          </>
                        )}
                        {/* Receiving volume stats */}
                        {(selectedPosition === 'ALL' || selectedPosition === 'WR' || selectedPosition === 'TE' || selectedPosition === 'RB') && (
                          <>
                            <td>{p.pos !== 'DST' ? (p.targets !== undefined ? p.targets.toFixed(1) : p.targets_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos !== 'DST' ? (p.rec !== undefined ? p.rec.toFixed(1) : p.rec_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos !== 'DST' ? (p.recYds !== undefined ? p.recYds.toFixed(1) : p.recYds_avg?.toFixed(1) || '-') : '-'}</td>
                            <td>{p.pos !== 'DST' ? (p.recTD !== undefined ? p.recTD.toFixed(1) : p.recTD_avg?.toFixed(1) || '-') : '-'}</td>
                          </>
                        )}
                      </tr>

                      {/* Expanded details */}
                      {isExpanded && (
                        <tr style={{ background: 'rgba(255,255,255,0.01)' }}>
                          <td colSpan={15} style={{ padding: '15px 24px' }}>
                            <div style={{ display: 'grid', gridTemplateColumns: '1fr 2fr', gap: '20px' }}>
                              <div style={{ borderRight: '1px solid rgba(255,255,255,0.06)', paddingRight: '20px' }}>
                                <h3 style={{ margin: 0, fontSize: '1rem', color: 'var(--accent-primary)', marginBottom: '8px' }}>
                                  Interactive Percentile Simulator
                                </h3>
                                <div style={{ background: 'rgba(11,17,38,0.4)', padding: '10px', borderRadius: '8px', border: '1px solid rgba(255,255,255,0.04)' }}>
                                  <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '6px' }}>
                                    <span style={{ fontSize: '0.8rem', fontWeight: 600, color: '#fff' }}>
                                      Percentile: <span className="text-cyan">{playerPercentile}%</span>
                                    </span>
                                  </div>
                                  <input
                                    type="range"
                                    min="0"
                                    max="100"
                                    step="1"
                                    value={playerPercentile}
                                    onChange={(e) => setPlayerPercentile(parseInt(e.target.value))}
                                    style={{ width: '100%', cursor: 'pointer', marginBottom: '8px' }}
                                  />
                                  <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '0.85rem', fontWeight: 'bold' }}>
                                    <span style={{ color: 'var(--text-muted)' }}>Proj Points:</span>
                                    <span style={{ color: 'var(--accent-green)' }}>
                                      {getPlayerPointsAtPercentile(p, playerPercentile, scoringFormat).toFixed(2)}
                                    </span>
                                  </div>
                                </div>
                              </div>

                              <div>
                                <h3 style={{ margin: 0, fontSize: '1rem', color: 'var(--accent-primary)', marginBottom: '8px' }}>
                                  Simulated Stat Outcomes (Mean Averages)
                                </h3>
                                <div style={{ display: 'flex', flexWrap: 'wrap', gap: '8px' }}>
                                  {p.pos === 'QB' && (
                                    <>
                                      <div style={statPillStyle}>
                                        Pass Attempts: {(p.pAtt !== undefined ? p.pAtt : p.pAtt_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Completions: {(p.pCmp !== undefined ? p.pCmp : p.pCmp_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Pass Yards: {(p.pYds !== undefined ? p.pYds : p.pYds_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Pass TDs: {(p.pTD !== undefined ? p.pTD : p.pTD_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Interceptions: {(p.int !== undefined ? p.int : p.int_avg || 0).toFixed(1)}
                                      </div>
                                    </>
                                  )}
                                  {p.pos !== 'DST' && (
                                    <>
                                      <div style={statPillStyle}>
                                        Carries: {(p.rAtt !== undefined ? p.rAtt : p.rAtt_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Rush Yards: {(p.rYds !== undefined ? p.rYds : p.rYds_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Rush TDs: {(p.rTD !== undefined ? p.rTD : p.rTD_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Targets: {(p.targets !== undefined ? p.targets : p.targets_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Receptions: {(p.rec !== undefined ? p.rec : p.rec_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Rec Yards: {(p.recYds !== undefined ? p.recYds : p.recYds_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Rec TDs: {(p.recTD !== undefined ? p.recTD : p.recTD_avg || 0).toFixed(1)}
                                      </div>
                                      <div style={statPillStyle}>
                                        Fumbles: {(p.fumbles !== undefined ? p.fumbles : p.fumbles_avg || 0).toFixed(1)}
                                      </div>
                                    </>
                                  )}
                                </div>
                              </div>
                            </div>
                          </td>
                        </tr>
                      )}
                    </Fragment>
                  );
                })
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}
