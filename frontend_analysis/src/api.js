// src/api.js

const API_BASE = '/api';

// Helper to perform API requests with immediate mock fallback on failure
async function safeFetch(url, options = {}, fallbackData = null) {
  try {
    const res = await fetch(url, {
      ...options,
      headers: {
        'Content-Type': 'application/json',
        ...options.headers,
      },
    });
    if (!res.ok) throw new Error(`HTTP Error: ${res.status}`);
    return await res.json();
  } catch (err) {
    console.warn(`API Connection to ${url} failed. Using mock sandbox fallback.`, err);
    return fallbackData;
  }
}

// -------------------------------------------------------------------------
// MOCK DATA GENERATORS
// -------------------------------------------------------------------------

const MOCK_GAMES = [
  {
    game_id: 'live_game_1',
    away_team: 'PHI',
    home_team: 'KC',
    away_score: 24,
    home_score: 27,
    quarter: 4,
    time_remaining: '2:15',
    possession: 'PHI',
    down: 4,
    distance: 2,
    yardline: 'Opp 45',
    away_wp: 46.0,
    home_wp: 54.0,
    leverage: 'High',
  },
  {
    game_id: 'live_game_2',
    away_team: 'SF',
    home_team: 'LAR',
    away_score: 17,
    home_score: 10,
    quarter: 3,
    time_remaining: '8:45',
    possession: 'LAR',
    down: 2,
    distance: 8,
    yardline: 'Own 35',
    away_wp: 72.5,
    home_wp: 27.5,
    leverage: 'Medium',
  },
  {
    game_id: 'live_game_3',
    away_team: 'DET',
    home_team: 'GB',
    away_score: 20,
    home_score: 21,
    quarter: 4,
    time_remaining: '0:34',
    possession: 'GB',
    down: 3,
    distance: 4,
    yardline: 'Own 48',
    away_wp: 38.0,
    home_wp: 62.0,
    leverage: 'Critical',
  },
  {
    game_id: 'live_game_4',
    away_team: 'MIA',
    home_team: 'BUF',
    away_score: 7,
    home_score: 28,
    quarter: 3,
    time_remaining: '4:00',
    possession: 'MIA',
    down: 2,
    distance: 14,
    yardline: 'Own 35',
    away_wp: 4.0,
    home_wp: 96.0,
    leverage: 'Low',
  },
  {
    game_id: 'live_game_5',
    away_team: 'LAC',
    home_team: 'KC',
    away_score: 10,
    home_score: 17,
    quarter: 4,
    time_remaining: '2:00',
    possession: 'LAC',
    down: 3,
    distance: 2,
    yardline: 'Opp 22',
    away_wp: 28.5,
    home_wp: 71.5,
    leverage: 'High',
  }
];

const MOCK_PLAYOFF_ODDS = [
  { team: 'KC', wins: 12.4, division_pct: 88.5, playoff_pct: 98.2, super_bowl_pct: 18.4 },
  { team: 'SF', wins: 11.8, division_pct: 79.2, playoff_pct: 94.0, super_bowl_pct: 14.2 },
  { team: 'PHI', wins: 11.2, division_pct: 74.0, playoff_pct: 91.5, super_bowl_pct: 11.8 },
  { team: 'DET', wins: 10.9, division_pct: 68.4, playoff_pct: 88.0, super_bowl_pct: 9.5 },
  { team: 'BUF', wins: 10.5, division_pct: 62.1, playoff_pct: 82.3, super_bowl_pct: 8.0 },
  { team: 'BAL', wins: 10.2, division_pct: 59.8, playoff_pct: 78.4, super_bowl_pct: 7.2 },
  { team: 'GB', wins: 9.8, division_pct: 42.5, playoff_pct: 66.2, super_bowl_pct: 5.4 },
  { team: 'LAR', wins: 8.6, division_pct: 20.8, playoff_pct: 44.0, super_bowl_pct: 2.1 }
];

const MOCK_FULL_STANDINGS = [
  // AFC East
  { team: 'BUF', wins: 11.2, losses: 5.8, conference: 'AFC', division: 'East', division_pct: 82.5, playoff_pct: 94.0, super_bowl_pct: 8.5 },
  { team: 'MIA', wins: 9.4, losses: 7.6, conference: 'AFC', division: 'East', division_pct: 12.4, playoff_pct: 56.2, super_bowl_pct: 2.4 },
  { team: 'NYJ', wins: 7.8, losses: 9.2, conference: 'AFC', division: 'East', division_pct: 4.8, playoff_pct: 28.5, super_bowl_pct: 0.8 },
  { team: 'NE', wins: 5.2, losses: 11.8, conference: 'AFC', division: 'East', division_pct: 0.3, playoff_pct: 2.1, super_bowl_pct: 0.0 },

  // AFC North
  { team: 'BAL', wins: 11.5, losses: 5.5, conference: 'AFC', division: 'North', division_pct: 64.2, playoff_pct: 92.5, super_bowl_pct: 10.2 },
  { team: 'CIN', wins: 10.1, losses: 6.9, conference: 'AFC', division: 'North', division_pct: 22.8, playoff_pct: 74.0, super_bowl_pct: 6.8 },
  { team: 'CLE', wins: 8.4, losses: 8.6, conference: 'AFC', division: 'North', division_pct: 8.5, playoff_pct: 42.1, super_bowl_pct: 1.5 },
  { team: 'PIT', wins: 8.1, losses: 8.9, conference: 'AFC', division: 'North', division_pct: 4.5, playoff_pct: 35.8, super_bowl_pct: 1.1 },

  // AFC South
  { team: 'HOU', wins: 10.8, losses: 6.2, conference: 'AFC', division: 'South', division_pct: 72.0, playoff_pct: 88.4, super_bowl_pct: 7.4 },
  { team: 'JAX', wins: 8.5, losses: 8.5, conference: 'AFC', division: 'South', division_pct: 18.2, playoff_pct: 46.5, super_bowl_pct: 1.8 },
  { team: 'IND', wins: 7.9, losses: 9.1, conference: 'AFC', division: 'South', division_pct: 8.6, playoff_pct: 32.0, super_bowl_pct: 0.9 },
  { team: 'TEN', wins: 5.8, losses: 11.2, conference: 'AFC', division: 'South', division_pct: 1.2, playoff_pct: 6.4, super_bowl_pct: 0.1 },

  // AFC West
  { team: 'KC', wins: 12.8, losses: 4.2, conference: 'AFC', division: 'West', division_pct: 91.5, playoff_pct: 99.1, super_bowl_pct: 18.5 },
  { team: 'LAC', wins: 9.6, losses: 7.4, conference: 'AFC', division: 'West', division_pct: 7.2, playoff_pct: 62.4, super_bowl_pct: 3.1 },
  { team: 'DEN', wins: 7.5, losses: 9.5, conference: 'AFC', division: 'West', division_pct: 1.1, playoff_pct: 22.0, super_bowl_pct: 0.4 },
  { team: 'LV', wins: 6.2, losses: 10.8, conference: 'AFC', division: 'West', division_pct: 0.2, playoff_pct: 8.5, super_bowl_pct: 0.1 },

  // NFC East
  { team: 'PHI', wins: 11.9, losses: 5.1, conference: 'NFC', division: 'East', division_pct: 78.4, playoff_pct: 95.2, super_bowl_pct: 11.2 },
  { team: 'DAL', wins: 9.8, losses: 7.2, conference: 'NFC', division: 'East', division_pct: 18.5, playoff_pct: 64.0, super_bowl_pct: 4.5 },
  { team: 'WAS', wins: 7.6, losses: 9.4, conference: 'NFC', division: 'East', division_pct: 2.8, playoff_pct: 24.1, super_bowl_pct: 0.5 },
  { team: 'NYG', wins: 5.4, losses: 11.6, conference: 'NFC', division: 'East', division_pct: 0.3, playoff_pct: 3.2, super_bowl_pct: 0.0 },

  // NFC North
  { team: 'DET', wins: 11.6, losses: 5.4, conference: 'NFC', division: 'North', division_pct: 66.5, playoff_pct: 93.8, super_bowl_pct: 9.8 },
  { team: 'GB', wins: 10.4, losses: 6.6, conference: 'NFC', division: 'North', division_pct: 26.2, playoff_pct: 76.5, super_bowl_pct: 5.8 },
  { team: 'MIN', wins: 8.2, losses: 8.8, conference: 'NFC', division: 'North', division_pct: 6.8, playoff_pct: 38.0, super_bowl_pct: 1.2 },
  { team: 'CHI', wins: 7.9, losses: 9.1, conference: 'NFC', division: 'North', division_pct: 0.5, playoff_pct: 31.4, super_bowl_pct: 0.9 },

  // NFC South
  { team: 'ATL', wins: 9.9, losses: 7.1, conference: 'NFC', division: 'South', division_pct: 62.0, playoff_pct: 72.8, super_bowl_pct: 4.1 },
  { team: 'TB', wins: 8.8, losses: 8.2, conference: 'NFC', division: 'South', division_pct: 26.5, playoff_pct: 49.2, super_bowl_pct: 1.9 },
  { team: 'NO', wins: 7.8, losses: 9.2, conference: 'NFC', division: 'South', division_pct: 10.2, playoff_pct: 32.5, super_bowl_pct: 0.8 },
  { team: 'CAR', wins: 4.8, losses: 12.2, conference: 'NFC', division: 'South', division_pct: 1.3, playoff_pct: 2.4, super_bowl_pct: 0.0 },

  // NFC West
  { team: 'SF', wins: 12.1, losses: 4.9, conference: 'NFC', division: 'West', division_pct: 84.2, playoff_pct: 98.0, super_bowl_pct: 14.5 },
  { team: 'LAR', wins: 9.2, losses: 7.8, conference: 'NFC', division: 'West', division_pct: 12.8, playoff_pct: 54.0, super_bowl_pct: 2.2 },
  { team: 'SEA', wins: 7.8, losses: 9.2, conference: 'NFC', division: 'West', division_pct: 2.8, playoff_pct: 28.5, super_bowl_pct: 0.7 },
  { team: 'ARI', wins: 6.5, losses: 10.5, conference: 'NFC', division: 'West', division_pct: 0.2, playoff_pct: 11.2, super_bowl_pct: 0.1 }
];

const MOCK_PLAY_BY_PLAY = {
  live_game_1: [
    { play_id: 1, qtr: 4, time: '15:00', desc: 'Start of 4th Quarter.', possession: 'KC', home_wp: 62.0, away_wp: 38.0, leverage: 1.0 },
    { play_id: 2, qtr: 4, time: '14:20', desc: 'Isiah Pacheco rush right guard for 4 yards.', possession: 'KC', home_wp: 64.2, away_wp: 35.8, leverage: 0.8 },
    { play_id: 3, qtr: 4, time: '13:45', desc: 'Patrick Mahomes pass deep middle to Travis Kelce for 21 yards.', possession: 'KC', home_wp: 75.0, away_wp: 25.0, leverage: 1.8 },
    { play_id: 4, qtr: 4, time: '12:30', desc: 'Patrick Mahomes sacked by Jalen Carter for -8 yards.', possession: 'KC', home_wp: 63.5, away_wp: 36.5, leverage: 2.1 },
    { play_id: 5, qtr: 4, time: '11:50', desc: 'Harrison Butker 48 yard field goal is GOOD.', possession: 'KC', home_wp: 72.0, away_wp: 28.0, leverage: 1.5 },
    { play_id: 6, qtr: 4, time: '11:45', desc: 'Jake Elliott kick-off to end zone, touchback.', possession: 'PHI', home_wp: 72.0, away_wp: 28.0, leverage: 0.1 },
    { play_id: 7, qtr: 4, time: '11:15', desc: 'Saquon Barkley rush left end for 12 yards.', possession: 'PHI', home_wp: 67.5, away_wp: 32.5, leverage: 1.2 },
    { play_id: 8, qtr: 4, time: '10:30', desc: 'Jalen Hurts pass short left to A.J. Brown for 8 yards.', possession: 'PHI', home_wp: 64.0, away_wp: 36.0, leverage: 0.9 },
    { play_id: 9, qtr: 4, time: '9:45', desc: 'Jalen Hurts pass deep right to DeVonta Smith for 38 yards, TOUCHDOWN.', possession: 'PHI', home_wp: 51.5, away_wp: 48.5, leverage: 3.8 },
    { play_id: 10, qtr: 4, time: '9:38', desc: 'Jake Elliott extra point is GOOD.', possession: 'PHI', home_wp: 50.5, away_wp: 49.5, leverage: 0.2 },
    { play_id: 11, qtr: 4, time: '7:15', desc: 'Patrick Mahomes pass short right to Rashee Rice for 15 yards.', possession: 'KC', home_wp: 58.2, away_wp: 41.8, leverage: 1.5 },
    { play_id: 12, qtr: 4, time: '6:30', desc: 'Isiah Pacheco rush for 2 yards, FUMBLE, recovered by PHI.', possession: 'PHI', home_wp: 32.0, away_wp: 68.0, leverage: 4.2 },
    { play_id: 13, qtr: 4, time: '4:15', desc: 'Saquon Barkley rush left guard for 3 yards.', possession: 'PHI', home_wp: 35.5, away_wp: 64.5, leverage: 1.1 },
    { play_id: 14, qtr: 4, time: '3:00', desc: 'Jalen Hurts pass incomplete deep left to A.J. Brown.', possession: 'PHI', home_wp: 42.0, away_wp: 58.0, leverage: 2.5 },
    { play_id: 15, qtr: 4, time: '2:15', desc: '4th & 2 at Opp 45. Jalen Hurts pre-snap alignment...', possession: 'PHI', home_wp: 54.0, away_wp: 46.0, leverage: 4.8 }
  ],
  live_game_2: [
    { play_id: 1, qtr: 3, time: '15:00', desc: 'Kickoff to end zone.', possession: 'LAR', home_wp: 20.0, away_wp: 80.0, leverage: 0.1 },
    { play_id: 2, qtr: 3, time: '14:15', desc: 'Kyren Williams rush for 6 yards.', possession: 'LAR', home_wp: 22.4, away_wp: 77.6, leverage: 0.5 },
    { play_id: 3, qtr: 3, time: '13:30', desc: 'Kyren Williams rush for -2 yards.', possession: 'LAR', home_wp: 19.5, away_wp: 80.5, leverage: 0.8 },
    { play_id: 4, qtr: 3, time: '12:45', desc: 'Matthew Stafford pass short middle to Cooper Kupp for 11 yards.', possession: 'LAR', home_wp: 26.0, away_wp: 74.0, leverage: 1.6 },
    { play_id: 5, qtr: 3, time: '11:15', desc: 'Matthew Stafford pass deep right intercepted by Charvarius Ward.', possession: 'SF', home_wp: 12.0, away_wp: 88.0, leverage: 2.8 }
  ],
  live_game_3: [
    { play_id: 1, qtr: 4, time: '5:00', desc: 'Jared Goff pass deep middle to Amon-Ra St. Brown for 32 yards, TOUCHDOWN.', possession: 'DET', home_wp: 48.0, away_wp: 52.0, leverage: 3.5 },
    { play_id: 2, qtr: 4, time: '4:52', desc: 'Michael Badgley extra point is GOOD.', possession: 'DET', home_wp: 47.0, away_wp: 53.0, leverage: 0.2 },
    { play_id: 3, qtr: 4, time: '3:15', desc: 'Jordan Love pass deep left to Romeo Doubs for 22 yards.', possession: 'GB', home_wp: 58.0, away_wp: 42.0, leverage: 1.9 },
    { play_id: 4, qtr: 4, time: '1:45', desc: 'Josh Jacobs rush left tackle for 9 yards.', possession: 'GB', home_wp: 64.0, away_wp: 36.0, leverage: 1.1 },
    { play_id: 5, qtr: 4, time: '1:00', desc: 'Jordan Love pass short right to Tucker Kraft for 5 yards.', possession: 'GB', home_wp: 71.0, away_wp: 29.0, leverage: 1.4 },
    { play_id: 6, qtr: 4, time: '0:34', desc: '3rd & 4 at Own 48. Jordan Love drops back...', possession: 'GB', home_wp: 62.0, away_wp: 38.0, leverage: 3.9 }
  ]
};

const MOCK_FOURTH_DOWNS = {
  live_game_1: [
    {
      play_id: 15,
      desc: '4th & 2 at Opp 45 | Q4 2:15',
      actual: 'Went For It (Pass conversion to A.J. Brown is GOOD)',
      recharts_data: [
        { name: 'GO', wp: 54.0, success_rate: 58.5, label: 'Go For It' },
        { name: 'PUNT', wp: 50.0, success_rate: 100.0, label: 'Punt' },
        { name: 'FG', wp: 39.0, success_rate: 22.0, label: 'Field Goal' }
      ]
    }
  ],
  live_game_2: [],
  live_game_3: [],
  live_game_4: [
    { play_id: 1, qtr: 1, time: '12:00', desc: 'BUF 1st & 10 at own 25. Tied.', possession: 'BUF', home_wp: 50.0, away_wp: 50.0, leverage: 0.5 },
    { play_id: 2, qtr: 1, time: '7:30',  desc: 'MIA 1st & 10 at own 25. BUF up 3.', possession: 'MIA', home_wp: 54.8, away_wp: 45.2, leverage: 0.7 },
    { play_id: 3, qtr: 2, time: '8:00',  desc: 'BUF 1st & 10 at midfield. BUF up 7.', possession: 'BUF', home_wp: 74.5, away_wp: 25.5, leverage: 1.2 },
    { play_id: 4, qtr: 2, time: '4:00',  desc: 'MIA 3rd & 5 at own 35. BUF up 7.', possession: 'MIA', home_wp: 73.2, away_wp: 26.8, leverage: 1.5 },
    { play_id: 5, qtr: 2, time: '0:30',  desc: 'MIA 2-min drill, 2nd & 10 at opp 40. BUF up 14.', possession: 'MIA', home_wp: 90.1, away_wp: 9.9, leverage: 0.8 },
    { play_id: 6, qtr: 3, time: '9:00',  desc: 'BUF 1st & 10 at own 25. BUF up 21.', possession: 'BUF', home_wp: 99.8, away_wp: 0.2, leverage: 0.1 },
    { play_id: 7, qtr: 3, time: '4:00',  desc: 'MIA 2nd & 14 at own 35. BUF up 21.', possession: 'MIA', home_wp: 99.9, away_wp: 0.1, leverage: 0.1 },
    { play_id: 8, qtr: 4, time: '9:00',  desc: 'BUF 1st & 10, running out the clock. BUF up 21.', possession: 'BUF', home_wp: 100.0, away_wp: 0.0, leverage: 0.0 },
  ],
  live_game_5: [
    { play_id: 1, qtr: 1, time: '2:00',  desc: 'KC 1st & 10 at own 25. KC up 3.', possession: 'KC', home_wp: 56.8, away_wp: 43.2, leverage: 0.8 },
    { play_id: 2, qtr: 2, time: '10:00', desc: 'LAC 1st & 10 at own 30. KC up 7.', possession: 'LAC', home_wp: 66.5, away_wp: 33.5, leverage: 1.1 },
    { play_id: 3, qtr: 2, time: '5:00',  desc: 'KC 2nd & 3 at opp 28. KC up 7.', possession: 'KC', home_wp: 80.2, away_wp: 19.8, leverage: 1.4 },
    { play_id: 4, qtr: 3, time: '10:00', desc: 'LAC 1st & 10 at own 35. KC up 10.', possession: 'LAC', home_wp: 90.3, away_wp: 9.7, leverage: 0.8 },
    { play_id: 5, qtr: 3, time: '5:00',  desc: 'KC 3rd & 5 at midfield. KC up 10.', possession: 'KC', home_wp: 95.7, away_wp: 4.3, leverage: 0.5 },
    { play_id: 6, qtr: 4, time: '10:00', desc: 'LAC 2nd & 8 at opp 35. KC up 10.', possession: 'LAC', home_wp: 95.7, away_wp: 4.3, leverage: 0.6 },
    { play_id: 7, qtr: 4, time: '6:00',  desc: 'KC 1st & 10 at own 42. KC up 10.', possession: 'KC', home_wp: 97.2, away_wp: 2.8, leverage: 0.4 },
    { play_id: 8, qtr: 4, time: '2:00',  desc: 'LAC 3rd & 2 at opp 22. KC up 7. Last chance drive.', possession: 'LAC', home_wp: 71.5, away_wp: 28.5, leverage: 3.2 },
    { play_id: 9, qtr: 4, time: '0:30',  desc: 'LAC 4th & 10 at opp 10. KC up 7. Final play.', possession: 'LAC', home_wp: 87.0, away_wp: 13.0, leverage: 2.8 },
  ]
};

const MOCK_GAME_STATS = {
  live_game_1: {
    away: { team: 'PHI', first_downs: 22, total_yds: 385, pass_yds: 265, rush_yds: 120, turnovers: 1, epa_play: 0.12 },
    home: { team: 'KC', first_downs: 24, total_yds: 412, pass_yds: 310, rush_yds: 102, turnovers: 1, epa_play: 0.18 }
  },
  live_game_2: {
    away: { team: 'SF', first_downs: 14, total_yds: 245, pass_yds: 180, rush_yds: 65, turnovers: 0, epa_play: 0.08 },
    home: { team: 'LAR', first_downs: 11, total_yds: 198, pass_yds: 120, rush_yds: 78, turnovers: 1, epa_play: -0.04 }
  },
  live_game_3: {
    away: { team: 'DET', first_downs: 19, total_yds: 320, pass_yds: 235, rush_yds: 85, turnovers: 0, epa_play: 0.09 },
    home: { team: 'GB', first_downs: 20, total_yds: 342, pass_yds: 250, rush_yds: 92, turnovers: 0, epa_play: 0.11 }
  },
  live_game_4: {
    away: { team: 'MIA', first_downs: 8, total_yds: 142, pass_yds: 98, rush_yds: 44, turnovers: 3, epa_play: -0.22 },
    home: { team: 'BUF', first_downs: 26, total_yds: 448, pass_yds: 312, rush_yds: 136, turnovers: 0, epa_play: 0.34 }
  },
  live_game_5: {
    away: { team: 'LAC', first_downs: 16, total_yds: 274, pass_yds: 198, rush_yds: 76, turnovers: 1, epa_play: 0.04 },
    home: { team: 'KC', first_downs: 21, total_yds: 358, pass_yds: 264, rush_yds: 94, turnovers: 0, epa_play: 0.18 }
  }
};

// Mock data format mirrors /api/games/{game_id}/positional-eval response.
// KEP is from the offense's perspective (positive = offense in positional advantage).
const MOCK_CHESS_EVALUATOR = {
  live_game_1: {
    game_id: 'live_game_1', home_team: 'KC', away_team: 'PHI', n_plays: 8,
    evaluations: [
      { play_id: 2,  qtr: 4, clock: '14:20', off: 'KC',  def: 'PHI', down: 2, ydstogo: 6,  yardline_100: 68, score_differential:  3, ep: 1.12, kep:  2.31 },
      { play_id: 3,  qtr: 4, clock: '13:45', off: 'KC',  def: 'PHI', down: 1, ydstogo: 10, yardline_100: 47, score_differential:  3, ep: 1.88, kep:  2.76 },
      { play_id: 4,  qtr: 4, clock: '12:30', off: 'KC',  def: 'PHI', down: 1, ydstogo: 10, yardline_100: 26, score_differential:  3, ep: 3.21, kep:  3.42 },
      { play_id: 5,  qtr: 4, clock: '11:50', off: 'KC',  def: 'PHI', down: 4, ydstogo: 8,  yardline_100: 34, score_differential:  3, ep: 0.98, kep:  2.95 },
      { play_id: 7,  qtr: 4, clock: '11:15', off: 'PHI', def: 'KC',  down: 1, ydstogo: 10, yardline_100: 75, score_differential: -6, ep: 0.81, kep: -3.12 },
      { play_id: 8,  qtr: 4, clock: '10:30', off: 'PHI', def: 'KC',  down: 2, ydstogo: 2,  yardline_100: 63, score_differential: -6, ep: 1.44, kep: -2.78 },
      { play_id: 11, qtr: 4, clock: '7:15',  off: 'KC',  def: 'PHI', down: 1, ydstogo: 10, yardline_100: 75, score_differential: -3, ep: 0.81, kep: -1.22 },
      { play_id: 15, qtr: 4, clock: '2:15',  off: 'PHI', def: 'KC',  down: 4, ydstogo: 2,  yardline_100: 45, score_differential: -3, ep: 0.62, kep: -0.84 },
    ]
  },
  live_game_2: {
    game_id: 'live_game_2', home_team: 'LAR', away_team: 'SF', n_plays: 4,
    evaluations: [
      { play_id: 2, qtr: 3, clock: '14:15', off: 'LAR', def: 'SF', down: 1, ydstogo: 10, yardline_100: 75, score_differential: -7, ep: 0.81, kep: -5.20 },
      { play_id: 3, qtr: 3, clock: '13:30', off: 'LAR', def: 'SF', down: 2, ydstogo: 4,  yardline_100: 69, score_differential: -7, ep: 1.18, kep: -4.91 },
      { play_id: 4, qtr: 3, clock: '12:45', off: 'LAR', def: 'SF', down: 3, ydstogo: 6,  yardline_100: 67, score_differential: -7, ep: 0.62, kep: -4.74 },
      { play_id: 5, qtr: 3, clock: '11:15', off: 'SF',  def: 'LAR', down: 1, ydstogo: 10, yardline_100: 67, score_differential:  7, ep: 1.32, kep:  5.44 },
    ]
  },
  live_game_3: {
    game_id: 'live_game_3', home_team: 'GB', away_team: 'DET', n_plays: 5,
    evaluations: [
      { play_id: 1, qtr: 4, clock: '5:00', off: 'DET', def: 'GB', down: 1, ydstogo: 10, yardline_100: 75, score_differential: -1, ep: 0.81, kep: -0.45 },
      { play_id: 3, qtr: 4, clock: '3:15', off: 'GB',  def: 'DET', down: 1, ydstogo: 10, yardline_100: 75, score_differential:  1, ep: 0.81, kep:  0.62 },
      { play_id: 4, qtr: 4, clock: '1:45', off: 'GB',  def: 'DET', down: 2, ydstogo: 1,  yardline_100: 66, score_differential:  1, ep: 1.52, kep:  1.18 },
      { play_id: 5, qtr: 4, clock: '1:00', off: 'GB',  def: 'DET', down: 1, ydstogo: 10, yardline_100: 61, score_differential:  1, ep: 1.38, kep:  1.32 },
      { play_id: 6, qtr: 4, clock: '0:34', off: 'GB',  def: 'DET', down: 3, ydstogo: 4,  yardline_100: 56, score_differential:  1, ep: 0.94, kep:  1.15 },
    ]
  },
  // Blowout: BUF dominates MIA — shows KEP ceiling early and losing team's collapse
  live_game_4: {
    game_id: 'live_game_4', home_team: 'BUF', away_team: 'MIA', n_plays: 8,
    evaluations: [
      { play_id: 1,  qtr: 1, clock: '12:00', off: 'BUF', def: 'MIA', down: 1, ydstogo: 10, yardline_100: 75, score_differential:   0, ep:  1.09, kep:   0.64 },
      { play_id: 2,  qtr: 1, clock: '7:30',  off: 'MIA', def: 'BUF', down: 1, ydstogo: 10, yardline_100: 75, score_differential:  -3, ep:  1.09, kep:  -0.99 },
      { play_id: 3,  qtr: 2, clock: '8:00',  off: 'BUF', def: 'MIA', down: 1, ydstogo: 10, yardline_100: 50, score_differential:   7, ep:  2.55, kep:  12.27 },
      { play_id: 4,  qtr: 2, clock: '4:00',  off: 'MIA', def: 'BUF', down: 3, ydstogo:  5, yardline_100: 65, score_differential:  -7, ep:  0.61, kep:  -4.01 },
      { play_id: 5,  qtr: 2, clock: '0:30',  off: 'MIA', def: 'BUF', down: 2, ydstogo: 10, yardline_100: 40, score_differential: -14, ep:  2.61, kep: -20.42 },
      { play_id: 6,  qtr: 3, clock: '9:00',  off: 'BUF', def: 'MIA', down: 1, ydstogo: 10, yardline_100: 75, score_differential:  21, ep:  1.09, kep:  24.00 },
      { play_id: 7,  qtr: 3, clock: '4:00',  off: 'MIA', def: 'BUF', down: 2, ydstogo: 14, yardline_100: 65, score_differential: -21, ep:  0.83, kep: -24.00 },
      { play_id: 8,  qtr: 4, clock: '9:00',  off: 'BUF', def: 'MIA', down: 1, ydstogo: 10, yardline_100: 68, score_differential:  21, ep:  1.51, kep:  24.00 },
    ]
  },
  // 10-14pt game: KC vs LAC — shows how same score diff feels different at different game times
  live_game_5: {
    game_id: 'live_game_5', home_team: 'KC', away_team: 'LAC', n_plays: 9,
    evaluations: [
      { play_id: 1,  qtr: 1, clock: '2:00',  off: 'KC',  def: 'LAC', down: 1, ydstogo: 10, yardline_100: 75, score_differential:   3, ep:  1.09, kep:   4.75 },
      { play_id: 2,  qtr: 2, clock: '10:00', off: 'LAC', def: 'KC',  down: 1, ydstogo: 10, yardline_100: 70, score_differential:  -7, ep:  1.38, kep:  -7.01 },
      { play_id: 3,  qtr: 2, clock: '5:00',  off: 'KC',  def: 'LAC', down: 2, ydstogo:  3, yardline_100: 28, score_differential:   7, ep:  3.86, kep:  14.52 },
      { play_id: 4,  qtr: 3, clock: '10:00', off: 'LAC', def: 'KC',  down: 1, ydstogo: 10, yardline_100: 65, score_differential: -10, ep:  1.58, kep: -20.24 },
      { play_id: 5,  qtr: 3, clock: '5:00',  off: 'KC',  def: 'LAC', down: 3, ydstogo:  5, yardline_100: 50, score_differential:  10, ep:  1.55, kep:  24.00 },
      { play_id: 6,  qtr: 4, clock: '10:00', off: 'LAC', def: 'KC',  down: 2, ydstogo:  8, yardline_100: 35, score_differential: -10, ep:  3.04, kep: -20.35 },
      { play_id: 7,  qtr: 4, clock: '6:00',  off: 'KC',  def: 'LAC', down: 1, ydstogo: 10, yardline_100: 58, score_differential:  10, ep:  2.00, kep:  24.00 },
      { play_id: 8,  qtr: 4, clock: '2:00',  off: 'LAC', def: 'KC',  down: 3, ydstogo:  2, yardline_100: 22, score_differential:  -7, ep:  3.95, kep:  -9.91 },
      { play_id: 9,  qtr: 4, clock: '0:30',  off: 'LAC', def: 'KC',  down: 4, ydstogo: 10, yardline_100: 10, score_differential:  -7, ep:  2.66, kep: -20.00 },
    ]
  }
};

// -------------------------------------------------------------------------
// PUBLIC API SERVICE
// -------------------------------------------------------------------------
export const ApiService = {
  async getLiveGames() {
    return safeFetch(`${API_BASE}/live-games`, {}, MOCK_GAMES);
  },

  async getPlayoffOdds() {
    return safeFetch(`${API_BASE}/playoff-odds`, {}, MOCK_PLAYOFF_ODDS);
  },

  async getFullStandings() {
    return safeFetch(`${API_BASE}/standings`, {}, MOCK_FULL_STANDINGS);
  },

  async getPlayByPlay(gameId) {
    return safeFetch(`${API_BASE}/games/${gameId}/play-by-play`, {}, MOCK_PLAY_BY_PLAY[gameId] || []);
  },

  async getFourthDowns(gameId) {
    return safeFetch(`${API_BASE}/games/${gameId}/fourth-downs`, {}, MOCK_FOURTH_DOWNS[gameId] || []);
  },

  async getGameStats(gameId) {
    return safeFetch(`${API_BASE}/games/${gameId}/stats`, {}, MOCK_GAME_STATS[gameId] || null);
  },

  async getChessEvaluator(gameId) {
    return safeFetch(`${API_BASE}/games/${gameId}/positional-eval`, {}, MOCK_CHESS_EVALUATOR[gameId] || null);
  },

  async calculate4thDown(params) {
    const successRateGo = Math.max(20, Math.min(95, 85 - (params.distance * 8)));
    const successRateFG = params.yardline > 45 ? 0 : Math.max(5, Math.min(99, 100 - ((60 - params.yardline) * 2.5)));
    
    const wpGo = params.awayScore > params.homeScore ? 72 : 48;
    const wpPunt = params.awayScore > params.homeScore ? 65 : 38;
    const wpFG = params.awayScore > params.homeScore ? 68 : 42;

    const mockResponse = {
      success_go: successRateGo,
      success_fg: successRateFG,
      wp_go: wpGo,
      wp_fg: wpFG,
      wp_punt: wpPunt,
      recommendation: wpGo > Math.max(wpPunt, wpFG) ? 'STRONG GO' : (wpFG > wpPunt ? 'FIELD GOAL' : 'PUNT'),
      recharts_data: [
        { name: 'GO', wp: wpGo, success_rate: successRateGo, label: 'Go For It' },
        { name: 'PUNT', wp: wpPunt, success_rate: 100.0, label: 'Punt' },
        { name: 'FG', wp: wpFG, success_rate: successRateFG, label: 'Field Goal' }
      ]
    };

    return safeFetch(`${API_BASE}/fourth-down-evaluate`, {
      method: 'POST',
      body: JSON.stringify(params)
    }, mockResponse);
  }
};
