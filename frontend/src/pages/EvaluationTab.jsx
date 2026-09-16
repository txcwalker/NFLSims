import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';

// ─── Shared visual language (matches Optimizer.jsx / ShowdownOptimizer.jsx) ──
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };
const evColor = (v) => (v == null ? 'var(--text-muted)' : v > 0 ? 'var(--accent-primary)' : '#ef4444');

function fmt(v, digits = 1) { return v == null ? '—' : Number(v).toFixed(digits); }
function fmtInt(v) { return v == null ? '—' : Number(v).toLocaleString(); }
function parseNames(json) { try { return JSON.parse(json || '[]'); } catch { return []; } }

/**
 * Phase 4 view: field analysis (what actually happened in a settled contest
 * -- winner, percentile cutoffs, hindsight-optimal, top-finisher ownership)
 * and paper-trade results (predicted vs actual for lineups flagged as
 * "I'm actually entering this"). Both tables are read-only JSON views over
 * offline-built parquet (scripts/dfs_ownership/eval_field.py and
 * score_paper_entries.py) -- run those after dropping a standings CSV in;
 * this page just displays whatever they last produced.
 */
export default function EvaluationTab({ allSimResults = {}, games = [], selectedWeek, weeks = [], setSelectedWeek }) {
  const simmedGames = useMemo(
    () => (games || []).filter(g => allSimResults?.[g.game_id]?.projections?.length),
    [games, allSimResults],
  );
  const [rawGameId, setRawGameId] = useState('');
  const gameId = rawGameId || simmedGames[0]?.game_id || '';
  const activeGame = simmedGames.find(g => g.game_id === gameId) || null;
  const slateId = activeGame ? `showdown_${activeGame.away_team}_${activeGame.home_team}` : null;

  const [fieldRows, setFieldRows] = useState([]);
  const [paperRows, setPaperRows] = useState([]);
  const [replayRows, setReplayRows] = useState([]);
  const [pendingEntries, setPendingEntries] = useState([]);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    let cancelled = false;
    // Every setState below runs inside this microtask's `.then`, not the
    // synchronous effect body, so a slateId change can't cascade a render
    // during the effect pass itself (same pattern as useWorkspaceSlots).
    Promise.resolve().then(async () => {
      if (cancelled) return;
      if (!slateId) { setFieldRows([]); setPaperRows([]); setReplayRows([]); setPendingEntries([]); return; }
      setLoading(true);
      const [field, paper, replay, entries] = await Promise.all([
        ApiService.getFieldEval(slateId),
        ApiService.getPaperResults(slateId),
        ApiService.getSimReplay(slateId),
        ApiService.listPaperEntries(slateId, selectedWeek),
      ]);
      if (cancelled) return;
      setFieldRows(field.rows || []);
      const settledIds = new Set((paper.rows || []).map(r => r.entry_id));
      setPaperRows(paper.rows || []);
      setReplayRows(replay.rows || []);
      setPendingEntries((entries.entries || []).filter(e => !settledIds.has(e.entry_id)));
      setLoading(false);
    });
    return () => { cancelled = true; };
  }, [slateId, selectedWeek]);

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      <div className="glass-panel" style={{
        marginBottom: '18px', padding: '12px 20px', borderRadius: '12px',
        border: '1px solid var(--border-glass)', display: 'flex', gap: '18px',
        alignItems: 'center', flexWrap: 'wrap',
      }}>
        <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>📊 Evaluation</span>
        {weeks.length > 0 && (
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>Week</span>
            <select value={selectedWeek} onChange={e => setSelectedWeek?.(Number(e.target.value))}
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

          {/* ── Paper Trades (Phase 3) ── */}
          <div style={cardStyle}>
            <h2 style={{ margin: '0 0 4px 0', fontSize: '1rem' }}>Paper Trades</h2>
            <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
              Lineups flagged with 📝 on the Showdown Optimizer, checked against the real settled result once you drop a
              standings CSV in and run <code>score_paper_entries.py</code>. The score/ownership diff columns are the
              actual process backcheck — are we lucky, unlucky, or systematically off?
            </p>
            {pendingEntries.length > 0 && (
              <div style={{ fontSize: '0.78rem', color: '#f59e0b', marginBottom: '8px' }}>
                ⏳ {pendingEntries.length} paper {pendingEntries.length === 1 ? 'entry' : 'entries'} not settled yet: {' '}
                {pendingEntries.map(e => `${e.label || e.entry_id} (${e.contest_name})`).join(', ')}
              </div>
            )}
            {paperRows.length === 0 ? (
              <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', padding: '10px' }}>
                No settled paper-trade results for this slate yet.
              </div>
            ) : (
              <div className="table-container" style={{ overflowX: 'auto' }}>
                <table style={{ fontSize: '0.78rem', width: '100%' }}>
                  <thead>
                    <tr style={{ textAlign: 'left' }}>
                      <th style={{ padding: '6px 5px' }}>Lineup</th>
                      <th style={{ padding: '6px 5px' }}>Contest</th>
                      <th style={{ padding: '6px 5px' }}>Proj vs Actual</th>
                      <th style={{ padding: '6px 5px' }}>Own% Pred vs Actual</th>
                      <th style={{ padding: '6px 5px' }}>Predicted EV%</th>
                      <th style={{ padding: '6px 5px' }}>Rank</th>
                      <th style={{ padding: '6px 5px' }}>Beat Field</th>
                      <th style={{ padding: '6px 5px' }}>Finish %ile</th>
                    </tr>
                  </thead>
                  <tbody>
                    {paperRows.map((r, i) => (
                      <tr key={i} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                        <td style={{ padding: '6px 5px', fontWeight: 600 }}>{r.label || r.entry_id}</td>
                        <td style={{ padding: '6px 5px', color: 'var(--text-muted)' }}>{r.contest_name}</td>
                        <td style={{ padding: '6px 5px' }}>
                          {fmt(r.predicted_score)} → <strong style={{ color: r.score_diff >= 0 ? 'var(--accent-primary)' : '#ef4444' }}>{fmt(r.actual_score)}</strong>
                          {r.score_diff != null && <span style={{ color: 'var(--text-muted)', fontSize: '0.7rem' }}> ({r.score_diff >= 0 ? '+' : ''}{fmt(r.score_diff)})</span>}
                        </td>
                        <td style={{ padding: '6px 5px' }}>
                          {r.predicted_total_ownership == null ? '—' : `${fmt(r.predicted_total_ownership)}%`} → {r.actual_total_ownership == null ? '—' : `${fmt(r.actual_total_ownership)}%`}
                          {r.ownership_diff != null && (
                            <span style={{ color: Math.abs(r.ownership_diff) > 50 ? '#f59e0b' : 'var(--text-muted)', fontSize: '0.7rem' }}>
                              {' '}({r.ownership_diff >= 0 ? '+' : ''}{fmt(r.ownership_diff)})
                            </span>
                          )}
                        </td>
                        <td style={{ padding: '6px 5px', color: evColor(r.predicted_ev_pct) }}>{r.predicted_ev_pct != null ? `${r.predicted_ev_pct > 0 ? '+' : ''}${fmt(r.predicted_ev_pct)}%` : '—'}</td>
                        <td style={{ padding: '6px 5px' }}>{fmtInt(r.actual_rank)} / {fmtInt(r.field_size)}</td>
                        <td style={{ padding: '6px 5px', fontWeight: 600 }}>{r.beat_field_pct != null ? `${fmt(r.beat_field_pct)}%` : '—'}</td>
                        <td style={{ padding: '6px 5px', fontWeight: 700, color: 'var(--accent-primary)' }}>{r.finish_percentile != null ? `top ${fmt(r.finish_percentile)}%` : '—'}</td>
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
              Not "how did we actually do" (that's Paper Trades above) — this replays the real field's real rosters
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
