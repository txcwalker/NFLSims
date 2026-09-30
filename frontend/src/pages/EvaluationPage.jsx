import GameLinesEval from '../components/GameLinesEval';
import PlayerProjectionsEval from '../components/PlayerProjectionsEval';
import RankingsEval from '../components/RankingsEval';

const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };

// view id -> header title + the grading component it renders. One page per
// entry in the navbar's Evaluation dropdown (pagesConfig.js, category 'eval').
const VIEWS = {
  games: { title: '🎲 Game Lines', Component: GameLinesEval },
  players: { title: '🏃 Player Projections', Component: PlayerProjectionsEval },
  rankings: { title: '🏆 Rankings', Component: RankingsEval },
};

/**
 * Model-accuracy evaluation page (2026-09-29 split of the old single
 * EvaluationTab.jsx into one page per grader).
 *
 * Inputs (props):
 *   view            -- 'games' | 'players' | 'rankings' (string, from App.jsx's route)
 *   weeks           -- simmed weeks for the week picker (number[])
 *   selectedWeek    -- the app-wide selected week (number); each grader uses it
 *                      as its default table filter
 *   setSelectedWeek -- App.jsx setter, so changing week here changes it site-wide
 * Output: the page's JSX -- a header bar with the week picker, then the one
 *   grading component for `view`. All data fetching lives in the components
 *   (/api/eval/game_lines, /player_projections, /rankings).
 *
 * DFS contest review (field analysis, field sim replay) lives on the Sim
 * Replays page; paper-trade results live on Bankroll.
 */
export default function EvaluationPage({ view, weeks = [], selectedWeek, setSelectedWeek }) {
  const { title, Component } = VIEWS[view] || VIEWS.games;
  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      <div className="glass-panel" style={{
        marginBottom: '18px', padding: '12px 20px', borderRadius: '12px',
        border: '1px solid var(--border-glass)', display: 'flex', gap: '18px',
        alignItems: 'center', flexWrap: 'wrap',
      }}>
        <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>{title}</span>
        {weeks.length > 0 && (
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>Week</span>
            <select value={selectedWeek} onChange={e => setSelectedWeek?.(Number(e.target.value))}
              style={{ ...inputStyle, width: 'auto' }}>
              {weeks.map(w => <option key={w} value={w}>{w}</option>)}
            </select>
          </label>
        )}
      </div>
      <Component selectedWeek={selectedWeek} />
    </div>
  );
}
