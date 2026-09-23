import { useState, useEffect } from 'react';
import { ApiService } from '../api';
import StandingsTab from '../components/season2026/StandingsTab';
import LeadersTab from '../components/season2026/LeadersTab';
import TeamStatsTab from '../components/season2026/TeamStatsTab';
import MatchupsTab from '../components/season2026/MatchupsTab';
import TeamsTab from '../components/season2026/TeamsTab';

const TEAM_DIVISIONS = {
  BUF: ['AFC', 'East'], MIA: ['AFC', 'East'], NE: ['AFC', 'East'], NYJ: ['AFC', 'East'],
  BAL: ['AFC', 'North'], CIN: ['AFC', 'North'], CLE: ['AFC', 'North'], PIT: ['AFC', 'North'],
  HOU: ['AFC', 'South'], IND: ['AFC', 'South'], JAX: ['AFC', 'South'], TEN: ['AFC', 'South'],
  DEN: ['AFC', 'West'], KC: ['AFC', 'West'], LV: ['AFC', 'West'], LAC: ['AFC', 'West'],
  DAL: ['NFC', 'East'], NYG: ['NFC', 'East'], PHI: ['NFC', 'East'], WAS: ['NFC', 'East'],
  CHI: ['NFC', 'North'], DET: ['NFC', 'North'], GB: ['NFC', 'North'], MIN: ['NFC', 'North'],
  ATL: ['NFC', 'South'], CAR: ['NFC', 'South'], NO: ['NFC', 'South'], TB: ['NFC', 'South'],
  ARI: ['NFC', 'West'], LA: ['NFC', 'West'], SF: ['NFC', 'West'], SEA: ['NFC', 'West'],
};

const SUB_TABS = ['Standings', 'Leaders', 'Team Stats', 'Matchups', 'Teams'];

// ---------------------------------------------------------------------------
// MAIN PAGE
// ---------------------------------------------------------------------------
function Season2026() {
  const [subTab, setSubTab] = useState('Standings');
  const [loading, setLoading] = useState(true);
  const [standings, setStandings] = useState([]);
  const [teamStats, setTeamStats] = useState([]);
  const [leaders, setLeaders] = useState(null);
  const [matchups, setMatchups] = useState(null);
  const [teams, setTeams] = useState(null);

  useEffect(() => {
    async function loadAll() {
      const [s, ts, l, m, t] = await Promise.all([
        ApiService.getSeason2026Standings(),
        ApiService.getSeason2026TeamStats(),
        ApiService.getSeason2026Leaders(),
        ApiService.getSeason2026Matchups(),
        ApiService.getSeason2026Teams(),
      ]);
      setStandings((s || []).map(t => ({ ...t, Conference: t.Conference || TEAM_DIVISIONS[t.Team]?.[0], Division: t.Division || TEAM_DIVISIONS[t.Team]?.[1] })));
      setTeamStats(ts || []);
      setLeaders(l || { overall: {}, rookies: {} });
      setMatchups(m || { weeks: {} });
      setTeams(t || {});
      setLoading(false);
    }
    loadAll();
  }, []);

  return (
    <div>
      <div style={{ marginBottom: '20px' }}>
        <h1 style={{ fontSize: '24px' }}>2026 Rest of Season</h1>
        <p style={{ color: 'var(--text-secondary)', fontSize: '13px' }}>
          Additive: real results through the last completed week, plus simulated projections for the rest of the
          schedule -- 272 real matchups x 1,000 iterations, 100-season Monte Carlo for standings and playoff odds.
        </p>
      </div>

      <div className="tabs-container">
        {SUB_TABS.map(t => (
          <button key={t} className={`tab-btn ${subTab === t ? 'active' : ''}`} onClick={() => setSubTab(t)}>{t}</button>
        ))}
      </div>

      {loading ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>LOADING 2026 SEASON DATA...</div>
      ) : (
        <>
          {subTab === 'Standings' && <StandingsTab standings={standings} />}
          {subTab === 'Leaders' && <LeadersTab leaders={leaders} />}
          {subTab === 'Team Stats' && <TeamStatsTab teamStats={teamStats} />}
          {subTab === 'Matchups' && <MatchupsTab matchups={matchups} />}
          {subTab === 'Teams' && <TeamsTab teams={teams} />}
        </>
      )}
    </div>
  );
}

export default Season2026;
