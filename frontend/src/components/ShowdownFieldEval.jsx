import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';

// ─── Shared visual language (matches SimReplays.jsx / Optimizer.jsx) ──
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };

function fmt(v, digits = 1) { return v == null ? '—' : Number(v).toFixed(digits); }
function fmtInt(v) { return v == null ? '—' : Number(v).toLocaleString(); }
function parseNames(json) { try { return JSON.parse(json || '[]'); } catch { return []; } }

/**
 * Per-showdown-game DFS contest review: Field Analysis (what happened in each
 * settled contest -- winner, percentile cutoffs, hindsight-optimal, top-finisher
 * ownership) and field Sim Replay (the real field's rosters redrawn through our
 * own week sim). Moved verbatim off the old EvaluationTab.jsx on 2026-09-29;
 * rendered at the bottom of SimReplays.jsx. Read-only views over offline-built
 * parquet (scripts/dfs_ownership/eval_field.py, sim_replay_field.py).
 *
 * Inputs (props, all from App.jsx via SimReplays):
 *   games          -- the selected week's schedule rows (object[] with game_id/away_team/home_team)
 *   allSimResults  -- {game_id: {projections: [...]}}; only games with sims are pickable
 *   selectedWeek   -- app-wide week (number); picks which week's games are listed
 *   weeks          -- simmed weeks for the week picker (number[])
 *   setSelectedWeek -- App.jsx setter (changing week here changes it site-wide,
 *                     same as the Evaluation pages' picker)
 * Output: JSX -- a game picker plus the two tables for that game's
 *   showdown_{AWAY}_{HOME} slate (GET /api/eval/field, /api/eval/sim_replay).
 */
export default function ShowdownFieldEval({ games = [], allSimResults = {}, selectedWeek, weeks = [], setSelectedWeek }) {
  const simmedGames = useMemo(
    () => (games || []).filter(g => allSimResults?.[g.game_id]?.projections?.length),
    [games, allSimResults],
  );
  const [rawGameId, setRawGameId] = useState('');
  const gameId = rawGameId || simmedGames[0]?.game_id || '';
  const activeGame = simmedGames.find(g => g.game_id === gameId) || null;
  const slateId = activeGame ? `showdown_${activeGame.away_team}_${activeGame.home_team}` : null;

  const [fieldRows, setFieldRows] = useState([]);
  const [replayRows, setReplayRows] = useState([]);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    let cancelled = false;
    // Fetch inside a microtask so a slateId change can't cascade a render
    // during the effect pass itself (same pattern as useWorkspaceSlots).
    Promise.resolve().then(async () => {
      if (cancelled) return;
      if (!slateId) { setFieldRows([]); setReplayRows([]); return; }
      setLoading(true);
      const [field, replay] = await Promise.all([
        ApiService.getFieldEval(slateId),
        ApiService.getSimReplay(slateId),
      ]);
      if (cancelled) return;
      setFieldRows(field.rows || []);
      setReplayRows(replay.rows || []);
      setLoading(false);
    });
    return () => { cancelled = true; };
  }, [slateId]);

  return (
    <div style={{ marginTop: '26px' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: '14px', flexWrap: 'wrap', margin: '0 0 10px 2px' }}>
        <h2 style={{ margin: 0, fontSize: '1.05rem' }}>
          Showdown Field Review <span style={{ fontSize: '0.78rem', fontWeight: 400, color: 'var(--text-muted)' }}>— per showdown game</span>
        </h2>
        {weeks.length > 0 && (
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>Week</span>
            <select value={selectedWeek} onChange={e => { setRawGameId(''); setSelectedWeek?.(Number(e.target.value)); }}
              style={{ ...inputStyle, width: 'auto' }}>
              {weeks.map(w => <option key={w} value={w}>{w}</option>)}
            </select>
          </label>
        )}
        <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
          <span style={{ color: 'var(--text-muted)' }}>Game</span>
          <select value={gameId} onChange={e => setRawGameId(e.target.value)}
            style={{ ...inputStyle, width: 'auto', minWidth: '160px' }}>
            {simmedGames.length === 0 && <option value="">— no simmed games —</option>}
            {simmedGames.map(g => (
              <option key={g.game_id} value={g.game_id}>{g.away_team} @ {g.home_team}</option>
            ))}
          </select>
        </label>
        {loading && <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>loading…</span>}
      </div>
      {!activeGame ? (
        <div style={{ ...cardStyle, textAlign: 'center', color: 'var(--text-muted)', padding: '40px' }}>
          Pick a simmed game above.
        </div>
      ) : (
        <div style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>

          {/* ── Field Analysis (Phase 4a) ── */}
          <div style={cardStyle}>
            <h2 style={{ margin: '0 0 4px 0', fontSize: '1rem' }}>Field Analysis</h2>
            <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
              What actually happened in each settled contest for this game — winner, percentile cutoffs, the ownership
              profile of the top finishers, and the hindsight-optimal lineup (best possible score under the cap, known
              only after the fact). Built by <code>scripts/dfs_ownership/eval_field.py</code>.
            </p>
            {fieldRows.length === 0 ? (
              <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', padding: '10px' }}>
                No settled contests for this slate yet — drop a standings CSV into its archive folder and run <code>eval_field.py</code>.
              </div>
            ) : (
              <div className="table-container" style={{ overflowX: 'auto' }}>
                <table style={{ fontSize: '0.78rem', width: '100%' }}>
                  <thead>
                    <tr style={{ textAlign: 'left' }}>
                      <th style={{ padding: '6px 5px' }}>Contest</th>
                      <th style={{ padding: '6px 5px' }}>Fee / Max</th>
                      <th style={{ padding: '6px 5px' }}>Field</th>
                      <th style={{ padding: '6px 5px' }}>Winner</th>
                      <th style={{ padding: '6px 5px' }}>Hindsight-Optimal</th>
                      <th style={{ padding: '6px 5px' }} title="Winner's score minus the hindsight-optimal score — 0 means they nailed the perfect lineup">Δ vs Optimal</th>
                      <th style={{ padding: '6px 5px' }}>Top 1% / 0.1%</th>
                      <th style={{ padding: '6px 5px' }} title="Average combined roster ownership of the top ~20 finishers (or top 1% if the field is bigger) — chalky or contrarian winners?">Top-N Own%</th>
                    </tr>
                  </thead>
                  <tbody>
                    {fieldRows.map((r, i) => (
                      <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                        <td style={{ padding: '6px 5px', fontWeight: 600 }}>{r.contest_name}</td>
                        <td style={{ padding: '6px 5px', color: 'var(--text-muted)' }}>${r.entry_fee} / {r.max_entries}-max</td>
                        <td style={{ padding: '6px 5px' }}>{fmtInt(r.field_size)}</td>
                        <td style={{ padding: '6px 5px' }}>
                          <div style={{ fontWeight: 700, color: 'var(--accent-green)' }}>{fmt(r.winner_score)}</div>
                          <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', maxWidth: '220px', whiteSpace: 'normal' }}>
                            {parseNames(r.winner_players).join(', ')}
                          </div>
                        </td>
                        <td style={{ padding: '6px 5px' }}>
                          <div style={{ fontWeight: 700, color: 'var(--accent-primary)' }}>{fmt(r.hindsight_optimal_score)}</div>
                          <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)', maxWidth: '220px', whiteSpace: 'normal' }}>
                            {parseNames(r.hindsight_optimal_players).join(', ')}
                          </div>
                        </td>
                        <td style={{ padding: '6px 5px', color: r.hindsight_vs_winner < -0.01 ? '#f59e0b' : 'var(--text-muted)' }}>
                          {r.hindsight_vs_winner == null ? '—' : (r.hindsight_vs_winner < 0 ? r.hindsight_vs_winner : `+${r.hindsight_vs_winner}`)}
                        </td>
                        <td style={{ padding: '6px 5px' }}>{fmt(r.top1pct_cutoff_score)} / {fmt(r.top01pct_cutoff_score)}</td>
                        <td style={{ padding: '6px 5px', fontWeight: 600 }}>{fmt(r.top_n_avg_ownership)}%</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}
          </div>

          {/* ── Sim Replay (Phase 5) ── */}
          <div style={cardStyle}>
            <h2 style={{ margin: '0 0 4px 0', fontSize: '1rem' }}>Sim Replay</h2>
            <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
              Not "how did we actually do" (that's Bankroll) — this replays the real field's real rosters
              through <em>our own</em> week sim instead of the real result, redrawn across every sim iteration. Answers
              "if our model were reality, how would this lineup have done against this field?", which isolates our
              process from one week's real-world variance. Built by <code>scripts/dfs_ownership/sim_replay_field.py</code>.
            </p>
            {replayRows.length === 0 ? (
              <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', padding: '10px' }}>
                No sim-replay results for this slate yet — needs a settled standings CSV, a flagged paper entry for
                that contest, and that week's sim run, then <code>sim_replay_field.py</code>.
              </div>
            ) : (
              <div className="table-container" style={{ overflowX: 'auto' }}>
                <table style={{ fontSize: '0.78rem', width: '100%' }}>
                  <thead>
                    <tr style={{ textAlign: 'left' }}>
                      <th style={{ padding: '6px 5px' }}>Lineup</th>
                      <th style={{ padding: '6px 5px' }}>Contest</th>
                      <th style={{ padding: '6px 5px' }} title="Sim-score distribution across every iteration (P10 / P50 / P90)">Sim Score (P10/P50/P90)</th>
                      <th style={{ padding: '6px 5px' }} title="Rank against the real field's real rosters, averaged across every sim iteration">Avg Rank</th>
                      <th style={{ padding: '6px 5px' }} title="Finish percentile averaged across every sim iteration — lower is better">Avg Finish %ile</th>
                      <th style={{ padding: '6px 5px' }} title="Share of sim iterations landing in the real field's top 10%">P(Top 10%)</th>
                      <th style={{ padding: '6px 5px' }} title="Share of sim iterations landing in the real field's top 1%">P(Top 1%)</th>
                      <th style={{ padding: '6px 5px' }} title="Share of sim iterations finishing in the top half of the real field">P(Top Half)</th>
                    </tr>
                  </thead>
                  <tbody>
                    {replayRows.map((r, i) => (
                      <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                        <td style={{ padding: '6px 5px', fontWeight: 600 }}>{r.label || r.entry_id}</td>
                        <td style={{ padding: '6px 5px', color: 'var(--text-muted)' }}>{r.contest_name}</td>
                        <td style={{ padding: '6px 5px' }}>{fmt(r.sim_p10_score)} / <strong>{fmt(r.sim_p50_score)}</strong> / {fmt(r.sim_p90_score)}</td>
                        <td style={{ padding: '6px 5px' }}>{fmt(r.mean_rank, 0)} / {fmtInt(r.field_size)}</td>
                        <td style={{ padding: '6px 5px', fontWeight: 700, color: 'var(--accent-primary)' }}>top {fmt(r.mean_percentile)}%</td>
                        <td style={{ padding: '6px 5px' }}>{fmt(r.prob_top10pct)}%</td>
                        <td style={{ padding: '6px 5px' }}>{fmt(r.prob_top1pct)}%</td>
                        <td style={{ padding: '6px 5px', color: r.prob_beat_field_median >= 50 ? 'var(--accent-primary)' : '#ef4444' }}>{fmt(r.prob_beat_field_median)}%</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}
