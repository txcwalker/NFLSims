import { useMemo } from 'react';
import { useSortableData, SortableTh } from './shared';

// Every team plays exactly 17 regular-season games in the 2026 schedule --
// used as the per-game divisor for projected/simulated data. Real ("Current
// Season") rows carry their own actual games_played instead.
const GAMES_PER_TEAM = 17;

const TEAM_STATS_COLS = [
  ['Team', 'Team'], ['PF_avg', 'PF'], ['PA_avg', 'PA'], ['PointDiff_avg', 'Diff'],
  ['pYds_avg', 'Pass Yds'], ['rYds_avg', 'Rush Yds'], ['TotalYds_avg', 'Total Yds'],
  ['PF_per_game', 'PF/G'], ['PA_per_game', 'PA/G'], ['pYds_per_game', 'Pass Yds/G'],
  ['rYds_per_game', 'Rush Yds/G'], ['TotalYds_per_game', 'Total Yds/G'],
];

function TeamStatsTab({ teamStats }) {
  const withPerGame = useMemo(() => teamStats.map(r => {
    const games = r.games_played > 0 ? r.games_played : GAMES_PER_TEAM;
    return {
      ...r,
      PF_per_game: r.PF_avg / games,
      PA_per_game: r.PA_avg / games,
      pYds_per_game: r.pYds_avg / games,
      rYds_per_game: r.rYds_avg / games,
      TotalYds_per_game: r.TotalYds_avg / games,
    };
  }), [teamStats]);
  const { sorted, sortKey, sortDir, toggleSort } = useSortableData(withPerGame, 'PointDiff_avg');
  return (
    <div style={{ overflowX: 'auto' }}>
      <table className="tactical-table">
        <thead><tr>{TEAM_STATS_COLS.map(([key, label]) => <SortableTh key={key} label={label} sortKeyName={key} activeKey={sortKey} dir={sortDir} onClick={toggleSort} />)}</tr></thead>
        <tbody>
          {sorted.map(r => (
            <tr key={r.Team}>
              {TEAM_STATS_COLS.map(([key]) => (
                <td key={key} style={{ fontWeight: key === 'Team' ? 700 : 400 }}>
                  {typeof r[key] === 'number' ? r[key].toFixed(1) : r[key]}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export default TeamStatsTab;
