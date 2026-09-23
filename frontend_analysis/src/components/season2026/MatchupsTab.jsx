import { useState, useEffect } from 'react';
import { fmtSpread, probToAmericanML, fmtML } from './shared';

// Works unmodified for both a full season's worth of weeks (Rest of Season)
// and a single-week map (Current Season, {weeks: {"<currentWeek>": [...]}})
// -- the week selector just has one option in the latter case.
function MatchupsTab({ matchups }) {
  const weeks = Object.keys(matchups?.weeks || {}).sort((a, b) => Number(a) - Number(b));
  const [week, setWeek] = useState(weeks[0] || '1');
  useEffect(() => { if (weeks.length && !weeks.includes(week)) setWeek(weeks[0]); }, [weeks]);
  const games = matchups?.weeks?.[week] || [];

  return (
    <div>
      <div className="form-group" style={{ maxWidth: '200px', marginBottom: '20px' }}>
        <label className="form-label">Week</label>
        <select className="form-select" value={week} onChange={e => setWeek(e.target.value)}>
          {weeks.map(w => <option key={w} value={w}>Week {w}</option>)}
        </select>
      </div>
      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))', gap: '10px' }}>
        {games.map(g => {
          const awayFav = g.away_win_pct >= g.home_win_pct;
          return (
            <div key={g.game_id} className="panel" style={{ padding: '14px 16px' }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <div style={{ fontWeight: awayFav ? 700 : 400, color: awayFav ? 'var(--accent-cyan)' : 'var(--text-primary)' }}>
                  {g.away_team} <span style={{ fontSize: '12px', color: 'var(--text-muted)' }}>{g.avg_away_score.toFixed(1)}</span>
                </div>
                <div style={{ fontSize: '11px', color: 'var(--text-muted)' }}>@</div>
                <div style={{ fontWeight: !awayFav ? 700 : 400, color: !awayFav ? 'var(--accent-cyan)' : 'var(--text-primary)', textAlign: 'right' }}>
                  {g.home_team} <span style={{ fontSize: '12px', color: 'var(--text-muted)' }}>{g.avg_home_score.toFixed(1)}</span>
                </div>
              </div>
              <div style={{ display: 'flex', height: '6px', borderRadius: '3px', overflow: 'hidden', marginTop: '10px', backgroundColor: 'var(--bg-primary)' }}>
                <div style={{ width: `${g.away_win_pct}%`, backgroundColor: 'var(--accent-cyan)' }} />
                <div style={{ width: `${g.home_win_pct}%`, backgroundColor: 'var(--accent-orange)' }} />
              </div>
              <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', color: 'var(--text-secondary)', marginTop: '4px' }}>
                <span>{g.away_win_pct.toFixed(1)}%</span>
                <span>{g.home_win_pct.toFixed(1)}%</span>
              </div>
              {(() => {
                // Everything below is stated from the HOME team's side so the
                // sim and the market can be read against each other directly.
                // nflverse spread_line is +ve when the home team is favored
                // (it's the home margin), so the home-side line is its neg.
                const vegasSpread = g.spread_line != null ? fmtSpread(-g.spread_line) : null;
                const simSpread = fmtSpread(g.avg_away_score - g.avg_home_score);
                const simTotal = (g.avg_away_score + g.avg_home_score).toFixed(1);
                const simMlHome = probToAmericanML(g.home_win_pct / 100);
                const simMlAway = probToAmericanML(g.away_win_pct / 100);
                const hasVegasMl = g.away_moneyline != null && g.home_moneyline != null;
                const row = { display: 'grid', gridTemplateColumns: 'auto 1fr', columnGap: '8px', alignItems: 'baseline' };
                const tag = { fontWeight: 700, color: 'var(--text-secondary)', letterSpacing: '0.03em' };
                return (
                  <div style={{ fontSize: '11px', color: 'var(--text-muted)', marginTop: '8px', paddingTop: '8px', borderTop: '1px solid var(--border-color)', display: 'grid', rowGap: '3px' }}>
                    <div style={row}>
                      <span style={tag}>VEGAS</span>
                      <span>
                        {vegasSpread ? `${g.home_team} ${vegasSpread}` : 'Line TBD'}
                        {' · '}{g.total_line != null ? `O/U ${g.total_line}` : 'O/U TBD'}
                        {hasVegasMl ? ` · ML ${g.away_team} ${fmtML(g.away_moneyline)} / ${g.home_team} ${fmtML(g.home_moneyline)}` : ''}
                      </span>
                    </div>
                    <div style={row}>
                      <span style={tag}>SIM</span>
                      <span>
                        {simSpread ? `${g.home_team} ${simSpread}` : '—'}
                        {' · '}{`O/U ${simTotal}`}
                        {` · ML ${g.away_team} ${fmtML(simMlAway)} / ${g.home_team} ${fmtML(simMlHome)}`}
                      </span>
                    </div>
                  </div>
                );
              })()}
            </div>
          );
        })}
      </div>
    </div>
  );
}

export default MatchupsTab;
