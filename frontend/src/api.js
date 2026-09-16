import { ALL_ROSTERS } from './allRosters';

const API_BASE = import.meta.env.VITE_API_BASE_URL ?? 'http://127.0.0.1:8002/api';

let isSandboxMode = false;

// Retries before falling back to mock data — the backend can take ~10s to warm up
// (loading season sim caches) after the frontend has already mounted and fired its
// first requests, so a single failed attempt shouldn't strand the UI in sandbox mode.
const FETCH_RETRIES = 4;
const FETCH_RETRY_DELAY_MS = 2000;

async function safeFetch(url, options = {}, fallbackData = null) {
  for (let attempt = 0; attempt <= FETCH_RETRIES; attempt++) {
    try {
      const res = await fetch(url, {
        ...options,
        headers: {
          'Content-Type': 'application/json',
          ...options.headers,
        },
      });
      if (!res.ok) throw new Error(`HTTP Error: ${res.status}`);
      isSandboxMode = false;
      return await res.json();
    } catch (err) {
      if (attempt < FETCH_RETRIES) {
        await new Promise(r => setTimeout(r, FETCH_RETRY_DELAY_MS));
        continue;
      }
      console.warn(`DFS API Connection to ${url} failed after ${FETCH_RETRIES + 1} attempts. Using mock sandbox fallback.`, err);
      isSandboxMode = true;
      return fallbackData;
    }
  }
}

// -------------------------------------------------------------------------
// MOCK DFS DATA
// -------------------------------------------------------------------------

const MOCK_WEEKS = { weeks: [1, 2, 3, 4, 5] };

const MOCK_GAMES = {
  games: [
    {
      game_id: 'game_1',
      away_team: 'DAL',
      home_team: 'PHI',
      gameday: '2025-09-07',
      gametime: '16:25',
      weekday: 'Sunday',
      spread_line: 8.5,
      total_line: 47.5,
      dk_main: true,
      fd_main: true
    },
    {
      game_id: 'game_2',
      away_team: 'KC',
      home_team: 'LAC',
      gameday: '2025-09-07',
      gametime: '16:25',
      weekday: 'Sunday',
      spread_line: -3.0,
      total_line: 47.5,
      dk_main: true,
      fd_main: true
    },
    {
      game_id: 'game_3',
      away_team: 'TB',
      home_team: 'ATL',
      gameday: '2025-09-07',
      gametime: '13:00',
      weekday: 'Sunday',
      spread_line: -1.5,
      total_line: 47.5,
      dk_main: true,
      fd_main: true
    },
    {
      game_id: 'game_4',
      away_team: 'CIN',
      home_team: 'CLE',
      gameday: '2025-09-07',
      gametime: '13:00',
      weekday: 'Sunday',
      spread_line: -5.5,
      total_line: 47.5,
      dk_main: true,
      fd_main: true
    },
    {
      game_id: 'game_5',
      away_team: 'MIA',
      home_team: 'IND',
      gameday: '2025-09-07',
      gametime: '13:00',
      weekday: 'Sunday',
      spread_line: 1.5,
      total_line: 47.5,
      dk_main: true,
      fd_main: true
    }
  ]
};

// Clean mock rosters to only keep starting QBs (prioritize known starting QBs, then highest salary)
const KNOWN_STARTING_QBS = [
  'Dak Prescott', 'Jalen Hurts', 'Patrick Mahomes', 'Justin Herbert',
  'Baker Mayfield', 'Michael Penix', 'Joe Burrow', 'Joe Flacco',
  'Tua Tagovailoa', 'Daniel Jones', 'Geno Smith', 'Drake Maye',
  'Kyler Murray', 'Spencer Rattler', 'Aaron Rodgers', 'Justin Fields',
  'Russell Wilson', 'Jayden Daniels', 'Cam Ward', 'Bo Nix',
  'Brock Purdy', 'Sam Darnold', 'Jared Goff', 'Jordan Love',
  'C.J. Stroud', 'Matthew Stafford', 'Lamar Jackson', 'Josh Allen',
  'J.J. McCarthy', 'Caleb Williams'
];

const MOCK_ROSTERS = {};
for (const [team, data] of Object.entries(ALL_ROSTERS)) {
  if (data && Array.isArray(data.roster)) {
    const qbs = data.roster.filter(p => p.pos === 'QB');
    let filteredRoster = data.roster;
    if (qbs.length > 1) {
      const starter = qbs.reduce((best, qb) => {
        const bestKnown = KNOWN_STARTING_QBS.includes(best.name);
        const qbKnown = KNOWN_STARTING_QBS.includes(qb.name);
        if (qbKnown && !bestKnown) return qb;
        if (!qbKnown && bestKnown) return best;
        return qb.salary > best.salary ? qb : best;
      }, qbs[0]);
      filteredRoster = data.roster.filter(p => p.pos !== 'QB' || p.name === starter.name);
    }
    MOCK_ROSTERS[team] = {
      ...data,
      roster: filteredRoster
    };
  } else {
    MOCK_ROSTERS[team] = data;
  }
}

function generateMockGames(week) {
  const matchups = [
    { away: 'DAL', home: 'PHI', spread: 8.5, total: 47.5, day: 'Thursday', time: '20:20' },
    { away: 'KC', home: 'LAC', spread: -3.0, total: 47.5, day: 'Friday', time: '20:00' },
    { away: 'TB', home: 'ATL', spread: -1.5, total: 47.5, day: 'Sunday', time: '13:00' },
    { away: 'CIN', home: 'CLE', spread: -5.5, total: 47.5, day: 'Sunday', time: '13:00' },
    { away: 'MIA', home: 'IND', spread: 1.5, total: 47.5, day: 'Sunday', time: '13:00' },
    { away: 'CAR', home: 'JAX', spread: 4.5, total: 45.5, day: 'Sunday', time: '13:00' },
    { away: 'LV', home: 'NE', spread: 2.5, total: 44.5, day: 'Sunday', time: '13:00' },
    { away: 'ARI', home: 'NO', spread: -6.0, total: 44.5, day: 'Sunday', time: '13:00' },
    { away: 'PIT', home: 'NYJ', spread: -3.0, total: 37.5, day: 'Sunday', time: '13:00' },
    { away: 'NYG', home: 'WAS', spread: 6.0, total: 45.5, day: 'Sunday', time: '13:00' },
    { away: 'TEN', home: 'DEN', spread: 8.5, total: 42.5, day: 'Sunday', time: '16:05' },
    { away: 'SF', home: 'SEA', spread: -2.5, total: 43.5, day: 'Sunday', time: '16:05' },
    { away: 'DET', home: 'GB', spread: 1.5, total: 48.5, day: 'Sunday', time: '16:25' },
    { away: 'HOU', home: 'LAR', spread: 3.5, total: 43.5, day: 'Sunday', time: '16:25' },
    { away: 'BAL', home: 'BUF', spread: -1.5, total: 50.5, day: 'Sunday', time: '20:20' },
    { away: 'MIN', home: 'CHI', spread: -1.5, total: 43.5, day: 'Monday', time: '20:15' }
  ];
  
  return {
    games: matchups.map((m, idx) => ({
      game_id: `game_${week}_${idx}`,
      away_team: m.away,
      home_team: m.home,
      gameday: '2025-09-07',
      gametime: m.time,
      weekday: m.day,
      spread_line: m.spread,
      total_line: m.total,
      dk_main: m.day === 'Sunday' && m.time !== '20:20',
      fd_main: m.day === 'Sunday' && m.time !== '20:20'
    }))
  };
}

function generateMockWeekProjections(week) {
  const mockGames = generateMockGames(week).games;
  const players = [];
  
  mockGames.forEach(g => {
    const teams = [g.away_team, g.home_team];
    teams.forEach(team => {
      const rosterList = MOCK_ROSTERS[team]?.roster || [];
      const opponent = g.away_team === team ? `@${g.home_team}` : `vs ${g.away_team}`;
      
      rosterList.forEach(p => {
        let basePts = 0;
        if (p.pos === 'QB') basePts = 18.5;
        else if (p.pos === 'RB') basePts = 14.8;
        else if (p.pos === 'WR') basePts = 13.2;
        else if (p.pos === 'TE') basePts = 9.8;
        else if (p.pos === 'DST') basePts = 7.2;

        const hash = (p.name.charCodeAt(0) || 0) + (p.name.charCodeAt(1) || 0);
        const randomFactor = 0.8 + ((hash % 100) / 100) * 0.4;
        const dkPoints = basePts * randomFactor;
        const fdPoints = dkPoints * 0.85;

        const dk_pcts_all = [];
        const fd_pcts_all = [];
        for (let i = 0; i <= 100; i++) {
          dk_pcts_all.push(parseFloat((dkPoints * (0.3 + (i / 100) * 1.4)).toFixed(2)));
          fd_pcts_all.push(parseFloat((fdPoints * (0.3 + (i / 100) * 1.4)).toFixed(2)));
        }

        players.push({
          name: p.name,
          pos: p.pos,
          team: team,
          opponent: opponent,
          game_id: g.game_id,
          is_main: g.dk_main,
          salary: p.salary || 5000,
          dk_points: parseFloat(dkPoints.toFixed(2)),
          fd_points: parseFloat(fdPoints.toFixed(2)),
          dk_value: parseFloat((dkPoints / ((p.salary || 5000) / 1000.0)).toFixed(2)),
          fd_value: parseFloat((fdPoints / ((p.salary || 5000) / 1000.0)).toFixed(2)),
          dk_pcts_all: dk_pcts_all,
          fd_pcts_all: fd_pcts_all,
          dk_p5: parseFloat(dk_pcts_all[5].toFixed(2)),
          dk_p25: parseFloat(dk_pcts_all[25].toFixed(2)),
          dk_p50: parseFloat(dk_pcts_all[50].toFixed(2)),
          dk_p75: parseFloat(dk_pcts_all[75].toFixed(2)),
          dk_p95: parseFloat(dk_pcts_all[95].toFixed(2)),
          fd_p5: parseFloat(fd_pcts_all[5].toFixed(2)),
          fd_p25: parseFloat(fd_pcts_all[25].toFixed(2)),
          fd_p50: parseFloat(fd_pcts_all[50].toFixed(2)),
          fd_p75: parseFloat(fd_pcts_all[75].toFixed(2)),
          fd_p95: parseFloat(fd_pcts_all[95].toFixed(2)),
          optimal_pct: parseFloat(((hash % 15) + 2).toFixed(2)),
          boom_pct: parseFloat(((hash % 20) + 5).toFixed(2)),
          value_pct: parseFloat(((hash % 25) + 10).toFixed(2)),
          rAtt: p.pos === 'RB' ? 15 : 0,
          rYds: p.pos === 'RB' ? 65 : 0,
          rTD: p.pos === 'RB' ? 0.5 : 0,
          targets: p.pos === 'WR' || p.pos === 'TE' ? 7 : 0,
          rec: p.pos === 'WR' || p.pos === 'TE' ? 5 : 0,
          recYds: p.pos === 'WR' || p.pos === 'TE' ? 58 : 0,
          recTD: p.pos === 'WR' || p.pos === 'TE' ? 0.3 : 0,
          pAtt: p.pos === 'QB' ? 32 : 0,
          pCmp: p.pos === 'QB' ? 21 : 0,
          pYds: p.pos === 'QB' ? 245 : 0,
          pTD: p.pos === 'QB' ? 1.6 : 0,
          int: p.pos === 'QB' ? 0.6 : 0,
          fumbles: 0.1
        });
      });
    });
  });

  return {
    week: week,
    players: players,
    games: mockGames
  };
}


// -------------------------------------------------------------------------
// MOCK OPTIMIZER DATA
// -------------------------------------------------------------------------
function generateMockLineups(payload) {
  const { players, n_lineups = 5, entry_fee = 18, total_entries = 11000, paying_positions = 2200 } = payload;
  const activePlayers = players.filter(p => !p.excluded);

  const byPos = {
    QB: activePlayers.filter(p => p.pos === 'QB'),
    RB: activePlayers.filter(p => p.pos === 'RB'),
    WR: activePlayers.filter(p => p.pos === 'WR'),
    TE: activePlayers.filter(p => p.pos === 'TE'),
    DST: activePlayers.filter(p => p.pos === 'DST')
  };

  if (!byPos.QB.length || byPos.RB.length < 2 || byPos.WR.length < 3 || !byPos.TE.length || !byPos.DST.length) {
    return { lineups: [], portfolio: { total_ev_pct: 0, effective_lineup_count: 0, avg_correlation: 0, coverage_score: 0, n_generated: 0, n_requested: n_lineups } };
  }

  const shuffle = arr => [...arr].sort(() => Math.random() - 0.5);
  const lineups = [];

  for (let i = 0; i < Math.min(n_lineups, 20); i++) {
    const qb = shuffle(byPos.QB)[0];
    const rbs = shuffle(byPos.RB).slice(0, 3); // 2 RB + 1 flex candidate
    const wrs = shuffle(byPos.WR).slice(0, 4); // 3 WR + 1 flex candidate
    const te = shuffle(byPos.TE)[0];
    const dst = shuffle(byPos.DST)[0];

    // Assign flex from remaining
    const flexCandidates = [...rbs.slice(2), ...wrs.slice(3)];
    const flex = flexCandidates[0] || rbs[2] || wrs[3];

    const slottedPlayers = [
      { ...qb, slot: 'QB' },
      { ...rbs[0], slot: 'RB' },
      { ...rbs[1], slot: 'RB' },
      { ...wrs[0], slot: 'WR' },
      { ...wrs[1], slot: 'WR' },
      { ...wrs[2], slot: 'WR' },
      { ...(flex || wrs[3] || rbs[2]), slot: 'FLEX' },
      { ...te, slot: 'TE' },
      { ...dst, slot: 'DST' }
    ].filter(Boolean).slice(0, 9);

    // Pad to 9 if needed
    while (slottedPlayers.length < 9) {
      const extra = shuffle(activePlayers.filter(p => !slottedPlayers.find(s => s.name === p.name)))[0];
      if (!extra) break;
      slottedPlayers.push({ ...extra, slot: 'FLEX' });
    }

    const totalSalary = slottedPlayers.reduce((s, p) => s + (p.salary || 5000), 0);
    const projScore = parseFloat(slottedPlayers.reduce((s, p) => s + (p.projection || 10), 0).toFixed(1));
    const itm = parseFloat((15 + Math.random() * 10).toFixed(1));
    const top1 = parseFloat((1.5 + Math.random() * 3).toFixed(1));
    const top01 = parseFloat((0.2 + Math.random() * 1).toFixed(1));
    const ev = parseFloat((-20 + Math.random() * 40).toFixed(1));

    lineups.push({
      players: slottedPlayers.map(p => ({
        name: p.name, pos: p.pos, team: p.team,
        salary: p.salary || 5000,
        projection: parseFloat((p.projection || 10).toFixed(1)),
        slot: p.slot,
        ownership_pct: parseFloat((5 + Math.random() * 30).toFixed(1))
      })),
      total_salary: totalSalary,
      projected_score: projScore,
      ev_pct: ev,
      portfolio_score: parseFloat((-2 + Math.random() * 4).toFixed(2)),
      itm_pct: itm,
      top1_pct: top1,
      top01_pct: top01
    });
  }

  const avgEv = lineups.length ? lineups.reduce((s, l) => s + l.ev_pct, 0) / lineups.length : 0;
  return {
    lineups,
    portfolio: {
      total_ev_pct: parseFloat(avgEv.toFixed(1)),
      effective_lineup_count: parseFloat((lineups.length * 0.75).toFixed(1)),
      avg_correlation: parseFloat((0.2 + Math.random() * 0.3).toFixed(3)),
      coverage_score: parseFloat((0.5 + Math.random() * 0.4).toFixed(2)),
      n_generated: lineups.length,
      n_requested: n_lineups
    }
  };
}

// -------------------------------------------------------------------------
// PUBLIC API SERVICE
// -------------------------------------------------------------------------
export const ApiService = {
  async checkHealth() {
    try {
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 3000);
      const res = await fetch(`${API_BASE}/health`, {
        signal: controller.signal
      });
      clearTimeout(timeoutId);
      return res.ok;
    } catch {
      return false;
    }
  },

  isSandbox() {
    return isSandboxMode;
  },

  async getWeeks() {
    return safeFetch(`${API_BASE}/weeks`, {}, MOCK_WEEKS);
  },

  async getGames(week, draftGroupId) {
    const q = draftGroupId ? `&draft_group_id=${draftGroupId}` : '';
    return safeFetch(`${API_BASE}/games?week=${week}${q}`, {}, generateMockGames(week));
  },

  async getWeekProjections(week, draftGroupId) {
    const fallback = {
      week: week,
      players: [],
      games: [],
      isSandbox: true,
      sandboxReason: 'Backend API unreachable — simulation projections unavailable.'
    };
    const q = draftGroupId ? `&draft_group_id=${draftGroupId}` : '';
    return safeFetch(`${API_BASE}/week_projections?week=${week}${q}`, {}, fallback);
  },

  async getWeekSimResults(week) {
    const fallback = { week: week, games: {} };
    return safeFetch(`${API_BASE}/week_sim_results?week=${week}`, {}, fallback);
  },

  async getWeekCashLineups(week) {
    const fallback = { week: week, lineups: [] };
    return safeFetch(`${API_BASE}/week_cash_lineups?week=${week}`, {}, fallback);
  },

  async getDkSlates(week, year = 2026) {
    const fallback = { is_live: false, fetched_at: null, default_draft_group_id: null, slates: [] };
    const q = week ? `?week=${week}&year=${year}` : '';
    return safeFetch(`${API_BASE}/dk/slates${q}`, {}, fallback);
  },

  async getDkContests(draftGroupId) {
    const fallback = { is_live: false, draft_group_id: null, fetched_at: null, contests: [] };
    const q = draftGroupId ? `?draft_group_id=${draftGroupId}` : '';
    return safeFetch(`${API_BASE}/dk/contests${q}`, {}, fallback);
  },

  async getDkContestPayout(contestId) {
    const fallback = { contest_id: contestId, entries: null, max_entries: null, entry_fee: null, tiers: [], error: 'unreachable' };
    return safeFetch(`${API_BASE}/dk/contest_payout?contest_id=${contestId}`, {}, fallback);
  },

  async getRosters(away, home, draftGroupId) {
    const result = {
      [away]: MOCK_ROSTERS[away] || { team_settings: {}, roster: [] },
      [home]: MOCK_ROSTERS[home] || { team_settings: {}, roster: [] }
    };
    const q = draftGroupId ? `&draft_group_id=${draftGroupId}` : '';
    return safeFetch(`${API_BASE}/rosters?away=${away}&home=${home}&year=2026${q}`, {}, result);
  },

  async runSimulation(payload) {
    // Generate realistic simulated outcomes proportional to player salaries/baselines and target spread/totals
    const projections = [];
    const rosters = [payload.away_team, payload.home_team];
    
    // Compute target team scores from overrides (home_score - away_score = spread, home_score + away_score = total)
    const targetTotal = payload.total_override !== undefined && payload.total_override !== null ? payload.total_override : 45.0;
    const targetSpread = payload.spread_override !== undefined && payload.spread_override !== null ? payload.spread_override : 0.0;
    
    const awayScore = Math.max(0, (targetTotal - targetSpread) / 2.0);
    const homeScore = Math.max(0, (targetTotal + targetSpread) / 2.0);
    
    const teamScores = {
      [payload.away_team]: awayScore,
      [payload.home_team]: homeScore
    };
    
    // Determine base pace (expected plays per team)
    let basePace = 63.5;
    if (targetTotal > 48) basePace += 3.5;
    if (targetTotal < 42) basePace -= 3.5;
    
    // Assume 58% pass play rate
    const teamPassPlays = basePace * 0.58;
    const teamRunPlays = basePace - teamPassPlays;

    rosters.forEach(team => {
      const rosterList = MOCK_ROSTERS[team]?.roster || [];
      const teamScore = teamScores[team];
      const teamTDs = teamScore / 7.0;
      
      rosterList.forEach(p => {
        // Custom player workload override factors if adjusted
        const userOverride = payload.player_overrides.find(x => x.name === p.name);
        const targetShare = userOverride ? userOverride.target_share : (p.target_share || 0.0);
        const carryShare = userOverride ? userOverride.carry_share : (p.carry_share || 0.0);
        const tdShare = userOverride ? userOverride.td_share : (p.td_share || 0.0);
        
        let rAtt = 0, rYds = 0, rTD = 0;
        let targets = 0, rec = 0, recYds = 0, recTD = 0;
        let pAtt = 0, pCmp = 0, pYds = 0, pTD = 0, int = 0;
        
        const playerTDs = teamTDs * (tdShare / 100.0);
        const pShare = targetShare / 100.0;
        const rShare = carryShare / 100.0;
        
        if (p.pos === 'QB') {
          pAtt = +(teamPassPlays).toFixed(2);
          pCmp = +(pAtt * 0.65).toFixed(2);
          pYds = +(pCmp * 7.5).toFixed(2);
          pTD = +(playerTDs * 0.95).toFixed(2);
          int = +(pAtt * 0.02).toFixed(2);
          
          rAtt = +(teamRunPlays * rShare).toFixed(2);
          rYds = +(rAtt * 4.2).toFixed(2);
          rTD = +(playerTDs * 0.05).toFixed(2);
        } else if (p.pos === 'RB') {
          rAtt = +(teamRunPlays * rShare).toFixed(2);
          rYds = +(rAtt * 4.4).toFixed(2);
          rTD = +(playerTDs * 0.65).toFixed(2);
          
          targets = +(teamPassPlays * pShare).toFixed(2);
          rec = +(targets * 0.75).toFixed(2);
          recYds = +(rec * 7.2).toFixed(2);
          recTD = +(playerTDs * 0.05).toFixed(2);
        } else if (p.pos === 'WR' || p.pos === 'TE') {
          targets = +(teamPassPlays * pShare).toFixed(2);
          rec = +(targets * (p.pos === 'TE' ? 0.70 : 0.65)).toFixed(2);
          recYds = +(rec * (p.pos === 'TE' ? 10.5 : 13.2)).toFixed(2);
          recTD = +(playerTDs * (p.pos === 'TE' ? 0.10 : 0.20)).toFixed(2);
        }
        
        // Compute DraftKings / FanDuel points
        const dkPoints = (pYds * 0.04) + (pTD * 4) - (int * 1) + (pYds >= 300 ? 3 : 0) +
                         (rYds * 0.1) + (rTD * 6) + (rYds >= 100 ? 3 : 0) +
                         (rec * 1.0) + (recYds * 0.1) + (recTD * 6) + (recYds >= 100 ? 3 : 0) -
                         0.05;
                         
        const fdPoints = (pYds * 0.04) + (pTD * 4) - (int * 2) +
                         (rYds * 0.1) + (rTD * 6) +
                         (rec * 0.5) + (recYds * 0.1) + (recTD * 6) -
                         0.10;
                         
        projections.push({
          name: p.name,
          pos: p.pos,
          team: team,
          salary: p.salary,
          dk_points: parseFloat(Math.max(0, dkPoints).toFixed(2)),
          fd_points: parseFloat(Math.max(0, fdPoints).toFixed(2)),
          dk_value: parseFloat((Math.max(0, dkPoints) / (p.salary / 1000.0)).toFixed(2)),
          fd_value: parseFloat((Math.max(0, fdPoints) / (p.salary / 1000.0)).toFixed(2)),
          rAtt, rYds, rTD,
          targets, rec, recYds, recTD,
          pAtt, pCmp, pYds, pTD, int,
          fumbles: 0.1
        });
      });
    });

    const awayWinProb = targetSpread >= 0
      ? Math.max(25.0, +(50.0 - Math.abs(targetSpread) * 2.5).toFixed(1))
      : Math.min(75.0, +(50.0 + Math.abs(targetSpread) * 2.5).toFixed(1));
    const homeWinProb = +(100.0 - awayWinProb).toFixed(1);

    const mockResponse = {
      summary: {
        away_avg_score: parseFloat(awayScore.toFixed(1)),
        home_avg_score: parseFloat(homeScore.toFixed(1)),
        win_probability_away: awayWinProb,
        win_probability_home: homeWinProb,
        away_cover_rate: 50.0,
        home_cover_rate: 50.0,
        over_probability: 50.0,
        under_probability: 50.0,
        away_plays: parseFloat(basePace.toFixed(1)),
        home_plays: parseFloat(basePace.toFixed(1)),
        away_pass_yds: parseFloat(projections.filter(p => p.team === payload.away_team && p.pos === 'QB').reduce((sum, p) => sum + p.pYds, 0).toFixed(1)),
        home_pass_yds: parseFloat(projections.filter(p => p.team === payload.home_team && p.pos === 'QB').reduce((sum, p) => sum + p.pYds, 0).toFixed(1)),
        away_rush_yds: parseFloat(projections.filter(p => p.team === payload.away_team).reduce((sum, p) => sum + p.rYds, 0).toFixed(1)),
        home_rush_yds: parseFloat(projections.filter(p => p.team === payload.home_team).reduce((sum, p) => sum + p.rYds, 0).toFixed(1)),
        away_turnovers: parseFloat(projections.filter(p => p.team === payload.away_team && p.pos === 'QB').reduce((sum, p) => sum + p.int, 0).toFixed(1)),
        home_turnovers: parseFloat(projections.filter(p => p.team === payload.home_team && p.pos === 'QB').reduce((sum, p) => sum + p.int, 0).toFixed(1)),
        away_projected_tds: parseFloat((awayScore / 6.5).toFixed(1)),
        home_projected_tds: parseFloat((homeScore / 6.5).toFixed(1))
      },
      projections: projections,
      score_density: []
    };

    return safeFetch(`${API_BASE}/simulate`, {
      method: 'POST',
      body: JSON.stringify(payload)
    }, mockResponse);
  },

  async optimizeLineups(payload) {
    const mockResult = generateMockLineups(payload);
    return safeFetch(`${API_BASE}/optimize`, {
      method: 'POST',
      body: JSON.stringify(payload)
    }, mockResult);
  },

  // Showdown (single-game) optimizer — see /api/optimize_showdown. No mock
  // fallback: the page requires a real sim run for the game anyway, so if the
  // backend is unreachable there's nothing sensible to synthesize.
  async optimizeShowdown(payload) {
    return safeFetch(`${API_BASE}/optimize_showdown`, {
      method: 'POST',
      body: JSON.stringify(payload)
    }, null);
  },

  // Live DK Showdown salary pool for one game (base + captain salary + both
  // draftableIds). Found by away/home team match against DK's live showdown
  // slates. Returns { found:false, ... } when DK has no showdown slate for
  // that game yet.
  async getDkShowdownSalaries(away, home) {
    const q = new URLSearchParams({ away, home }).toString();
    return safeFetch(`${API_BASE}/dk/showdown_salaries?${q}`, {}, { found: false, players: [], defense: [] });
  },

  // Pre-compute modelled FLEX/CPT ownership for the whole showdown pool + a
  // synthetic line for each kicker (see /api/showdown_prep). Called once the
  // pool + DK salaries are in place, to fill those columns before optimizing.
  async showdownPrep(payload) {
    return safeFetch(`${API_BASE}/showdown_prep`, {
      method: 'POST', body: JSON.stringify(payload),
    }, { players: [], kicker_source: 'none' });
  },

  // Standalone game-outcome distribution (see GET /api/game_distribution) --
  // just the total/margin histograms + joint grid + raw iteration arrays for
  // <GameDistribution>'s box-select, no player-projection simulation. Used to
  // embed the "Game Read" panel on the showdown optimizer without paying for
  // a full /api/simulate call. No mock fallback (null) -- a scenario panel
  // with fabricated numbers is worse than none.
  async getGameDistribution(awayTeam, homeTeam, week) {
    const q = new URLSearchParams({ away_team: awayTeam, home_team: homeTeam, ...(week != null ? { week } : {}) }).toString();
    return safeFetch(`${API_BASE}/game_distribution?${q}`, {}, null);
  },

  // ── Paper trading (see paper_store.py) -- flag a lineup as "I'm actually
  // entering this"; scripts/dfs_ownership/score_paper_entries.py settles it
  // later against a dropped-in standings CSV. `slateId` is the ownership-
  // archive folder name (`showdown_<AWAY>_<HOME>` / `main_slate`), not a
  // game_id. Light-touch, no retry: a failed save just means try again.
  async listPaperEntries(slateId, week, year = 2026) {
    try {
      const res = await fetch(`${API_BASE}/paper/entries?${new URLSearchParams({ slate_id: slateId, week, year })}`);
      return res.ok ? await res.json() : { entries: [] };
    } catch {
      return { entries: [] };
    }
  },

  async savePaperEntry(slateId, entry, week, year = 2026) {
    try {
      const res = await fetch(`${API_BASE}/paper/entries?${new URLSearchParams({ slate_id: slateId, week, year })}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(entry),
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },

  async deletePaperEntry(entryId, slateId, week, year = 2026) {
    try {
      const res = await fetch(`${API_BASE}/paper/entries/${entryId}?${new URLSearchParams({ slate_id: slateId, week, year })}`, { method: 'DELETE' });
      return res.ok;
    } catch {
      return false;
    }
  },

  // ── Evaluation tab reads (offline-built by score_paper_entries.py /
  // eval_field.py -- these are just JSON views over their parquet output).
  async getFieldEval(slateId) {
    try {
      const res = await fetch(`${API_BASE}/eval/field?${new URLSearchParams({ slate_id: slateId })}`);
      return res.ok ? await res.json() : { rows: [] };
    } catch {
      return { rows: [] };
    }
  },

  async getPaperResults(slateId) {
    try {
      const res = await fetch(`${API_BASE}/eval/paper?${new URLSearchParams({ slate_id: slateId })}`);
      return res.ok ? await res.json() : { rows: [] };
    } catch {
      return { rows: [] };
    }
  },

  async getSimReplay(slateId) {
    try {
      const res = await fetch(`${API_BASE}/eval/sim_replay?${new URLSearchParams({ slate_id: slateId })}`);
      return res.ok ? await res.json() : { rows: [] };
    } catch {
      return { rows: [] };
    }
  },

  // ── Contest Replays (see src/api/sim_replay_store.py) -- your own entries,
  // auto-found by DK username in the archived standings CSVs, vs our sim's
  // projected distribution for that exact lineup. Distinct from getSimReplay
  // above (which needs a paper-entry flag + a full field rescore).
  async listMyContests(year, week) {
    try {
      const params = {};
      if (year != null) params.year = year;
      if (week != null) params.week = week;
      const res = await fetch(`${API_BASE}/sim_replay/contests?${new URLSearchParams(params)}`);
      return res.ok ? await res.json() : { contests: [] };
    } catch {
      return { contests: [] };
    }
  },

  async getMyContestEntries(year, week, slateId, contestName) {
    try {
      const res = await fetch(`${API_BASE}/sim_replay/entries?${new URLSearchParams({ year, week, slate_id: slateId, contest_name: contestName })}`);
      return res.ok ? await res.json() : { entries: [] };
    } catch {
      return { entries: [] };
    }
  },

  // Solver-style field-rescore stats (Sim ROI/Cash Rate/Ceiling/etc, see
  // LineupHistogramModal) for your entries + the real top `topPct`% of the
  // field, ranked against each other under our own sim.
  async getContestFieldStats(year, week, slateId, contestName, { contestType, payingPositions, topPct } = {}) {
    try {
      const params = { year, week, slate_id: slateId, contest_name: contestName };
      if (contestType) params.contest_type = contestType;
      if (payingPositions != null) params.paying_positions = payingPositions;
      if (topPct != null) params.top_pct = topPct;
      const res = await fetch(`${API_BASE}/sim_replay/field_stats?${new URLSearchParams(params)}`);
      return res.ok ? await res.json() : { entries: [] };
    } catch {
      return { entries: [] };
    }
  },

  // ── Optimizer weekly working-state persistence (see optimizer_store.py).
  // Light-touch on purpose: one attempt, no retry loop, no sandbox-mode flip.
  // "backend down" and "nothing saved yet" both resolve to {} so the Optimizer
  // just starts clean; a failed autosave simply retries on the next change.
  async getOptimizerState(week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/state?week=${week}&season=${season}`, {
        headers: { 'Content-Type': 'application/json' },
      });
      return res.ok ? await res.json() : {};
    } catch {
      return {};
    }
  },

  async putOptimizerState(week, state, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/state?week=${week}&season=${season}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(state),
      });
      return res.ok;
    } catch {
      return false;
    }
  },

  // ── Saved builds (one immutable record per Optimize run).
  async listOptimizerBuilds(week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/builds?week=${week}&season=${season}`, {
        headers: { 'Content-Type': 'application/json' },
      });
      return res.ok ? await res.json() : [];
    } catch {
      return [];
    }
  },
  async getOptimizerBuild(week, buildId, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/builds/${buildId}?week=${week}&season=${season}`, {
        headers: { 'Content-Type': 'application/json' },
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },
  async createOptimizerBuild(week, build, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/builds?week=${week}&season=${season}`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(build),
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },
  async patchOptimizerBuild(week, buildId, patch, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/builds/${buildId}?week=${week}&season=${season}`, {
        method: 'PATCH', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(patch),
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },
  async deleteOptimizerBuild(week, buildId, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/builds/${buildId}?week=${week}&season=${season}`, { method: 'DELETE' });
      return res.ok;
    } catch {
      return false;
    }
  },
  async pruneOptimizerBuilds(week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/optimizer/prune?week=${week}&season=${season}`, { method: 'POST' });
      return res.ok ? await res.json() : { removed: [] };
    } catch {
      return { removed: [] };
    }
  },

  // ── Bankroll accounts (see account_store.py) -- paper/real buckets a
  // Build tags itself into via account_id. Same light-touch pattern as the
  // optimizer-state calls above.
  async getAccounts() {
    try {
      const res = await fetch(`${API_BASE}/accounts`);
      return res.ok ? (await res.json()).accounts : [];
    } catch {
      return [];
    }
  },
  async createAccount(label, kind, startingBankroll = 0) {
    try {
      const res = await fetch(`${API_BASE}/accounts`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ label, kind, starting_bankroll: startingBankroll }),
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },
  async patchAccount(accountId, patch) {
    try {
      const res = await fetch(`${API_BASE}/accounts/${accountId}`, {
        method: 'PATCH', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(patch),
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },
  async deleteAccount(accountId) {
    try {
      const res = await fetch(`${API_BASE}/accounts/${accountId}`, { method: 'DELETE' });
      return res.ok;
    } catch {
      return false;
    }
  },
  async getBankroll() {
    try {
      const res = await fetch(`${API_BASE}/bankroll`);
      return res.ok ? (await res.json()).accounts : [];
    } catch {
      return [];
    }
  },
  async getBankrollAccountEntries(accountId) {
    try {
      const res = await fetch(`${API_BASE}/bankroll/${accountId}/entries`);
      return res.ok ? (await res.json()).builds : [];
    } catch {
      return [];
    }
  },
  async patchPaperEntry(entryId, slateId, week, patch, year = 2026) {
    try {
      const res = await fetch(`${API_BASE}/paper/entries/${entryId}?${new URLSearchParams({ slate_id: slateId, week, year })}`, {
        method: 'PATCH', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(patch),
      });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },
  async clearBankrollAccount(accountId) {
    try {
      const res = await fetch(`${API_BASE}/bankroll/${accountId}/clear`, { method: 'POST' });
      return res.ok ? await res.json() : null;
    } catch {
      return null;
    }
  },

  // ── Workspace save slots (see workspace_store.py) -- 3 switchable,
  // autosaved workspace snapshots per (season, week, slateKey), shared by the
  // classic and showdown optimizer pages. Same light-touch pattern as the
  // optimizer-state calls above: one attempt, no retry, safe fallback so a
  // backend hiccup never blocks the page -- it just means that one autosave
  // tick or slot-switch silently didn't persist.
  async getWorkspaceSlots(slateKey, week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/workspace/slots?${new URLSearchParams({ slate_key: slateKey, week, season })}`);
      return res.ok ? await res.json() : { active: 1, slots: [1, 2, 3].map(slot => ({ slot, label: `Slot ${slot}`, updated_at: null, has_data: false })) };
    } catch {
      return { active: 1, slots: [1, 2, 3].map(slot => ({ slot, label: `Slot ${slot}`, updated_at: null, has_data: false })) };
    }
  },

  async getWorkspaceSlot(slateKey, slot, week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/workspace/slot?${new URLSearchParams({ slate_key: slateKey, slot, week, season })}`);
      return res.ok ? await res.json() : { slot, label: `Slot ${slot}`, updated_at: null, data: {} };
    } catch {
      return { slot, label: `Slot ${slot}`, updated_at: null, data: {} };
    }
  },

  async putWorkspaceSlot(slateKey, slot, data, week, season = 2026, label = undefined) {
    try {
      const res = await fetch(`${API_BASE}/workspace/slot?${new URLSearchParams({ slate_key: slateKey, slot, week, season })}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ data, ...(label !== undefined ? { label } : {}) }),
      });
      return res.ok;
    } catch {
      return false;
    }
  },

  async setWorkspaceActive(slateKey, slot, week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/workspace/active`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ slate_key: slateKey, slot, week, season }),
      });
      return res.ok;
    } catch {
      return false;
    }
  },

  async renameWorkspaceSlot(slateKey, slot, label, week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/workspace/slot?${new URLSearchParams({ slate_key: slateKey, slot, week, season })}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ label }),
      });
      return res.ok;
    } catch {
      return false;
    }
  },

  async clearWorkspaceSlot(slateKey, slot, week, season = 2026) {
    try {
      const res = await fetch(`${API_BASE}/workspace/slot?${new URLSearchParams({ slate_key: slateKey, slot, week, season })}`, { method: 'DELETE' });
      return res.ok;
    } catch {
      return false;
    }
  },
};
