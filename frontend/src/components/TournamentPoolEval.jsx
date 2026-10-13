import { useEffect, useState } from 'react';
import { ApiService } from '../api';
import { Empty, Tile } from './GameLinesEval';

/**
 * Lineup Pool evaluation (2026-10-09) -- every lineup the Optimizer generated
 * PRE-LOCK for a week, played or not, graded against the real $1 First Down
 * field (100K pool, $5K to 1st). It tests the optimizer's own math (are the
 * predicted ITM / top-1% / EV numbers honest?), the inputs (which builds and
 * input versions did better?) and the process (how fast do early builds go
 * stale?) at volume, without submitting anything.
 *
 * Inputs (props): selectedWeek -- the page's week.
 * Data: GET /api/eval/tournament_pool (src/evaluation/tournament_pool_eval.py).
 *   First load for a week can take 20-40s; cached afterwards.
 *
 * Reading it:
 *  - Headline numbers use VALID lineups only. "Stale" = holds a player who was
 *    unavailable by lock ("ruled out" per the week sheet, or "field abandoned":
 *    the field rostered him <1% though our pool expected >=5%).
 *  - Calibration bins: lineups sorted by predicted probability and split into
 *    fifths; a calibrated optimizer has the realized bar matching the
 *    predicted bar. One slate is one set of game outcomes and the lineups are
 *    heavily correlated, so read direction over magnitude until several weeks
 *    accumulate.
 *  - "Finish" is percentile among real entries (0 = first place).
 */

const C_PRED = '#0fa3b1';     // predicted (same cyan the other graders use)
const C_REAL = '#e8a33d';     // realized
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const th = { padding: '6px 6px', textAlign: 'left', whiteSpace: 'nowrap', fontWeight: 600, color: 'var(--text-muted)' };
const td = { padding: '5px 6px', whiteSpace: 'nowrap' };
const h3 = { fontSize: '0.85rem', fontWeight: 700, color: 'var(--text-white)', margin: '0 0 8px' };
const sub = { fontSize: '0.74rem', color: 'var(--text-muted)', marginBottom: 8 };

const num = (v, d = 1) => (v == null ? '—' : Number(v).toFixed(d));
const int = (v) => (v == null ? '—' : Number(v).toLocaleString());
const pct = (v, d = 1) => (v == null ? '—' : `${Number(v).toFixed(d)}%`);
const money = (v) => (v == null ? '—' : `$${Number(v).toLocaleString()}`);
const hrs = (h) => (h == null ? '—' : h >= 48 ? `${(h / 24).toFixed(1)}d` : `${h.toFixed(1)}h`);

/** Paired predicted-vs-realized bars for one calibration series (ITM or top-1%). */
function CalBins({ title, series, color }) {
  if (!series) return null;
  const bins = series.bins || [];
  const max = Math.max(0.0001, ...bins.map(b => Math.max(b.pred_mean, b.realized)), series.pred_mean, series.realized);
  const bar = (v, c) => (
    <div style={{ height: 7, width: `${Math.max(1, (v / max) * 100)}%`, background: c, borderRadius: 3 }} />
  );
  return (
    <div style={{ minWidth: 0 }}>
      <div style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text-main)' }}>{title}</div>
      <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', margin: '2px 0 6px' }}>
        overall: predicted {pct(series.pred_mean, 2)} · realized <b style={{ color: C_REAL }}>{pct(series.realized, 2)}</b> · n={int(series.n)}
      </div>
      {bins.map((b, i) => (
        <div key={i} style={{ display: 'grid', gridTemplateColumns: '1fr 74px', gap: 8, alignItems: 'center', marginBottom: 4 }}
          title={`bin ${i + 1} of ${bins.length} (low → high predicted) · n=${b.n} · predicted ${b.pred_mean}% · realized ${b.realized}%`}>
          <div>{bar(b.pred_mean, color)}<div style={{ height: 2 }} />{bar(b.realized, C_REAL)}</div>
          <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', textAlign: 'right' }}>{pct(b.pred_mean, 2)} → {pct(b.realized, 2)}</div>
        </div>
      ))}
    </div>
  );
}

/** Histogram of percentile finish; a flat bar at the dotted line = a random lineup. */
function FinishHist({ counts }) {
  const W = 320, H = 150, L = 8, R = 8, T = 10, B = 26;
  const n = (counts || []).reduce((a, b) => a + b, 0);
  if (!n) return <Empty text="No valid lineups." />;
  const exp = n / counts.length;
  const max = Math.max(exp * 1.4, ...counts);
  const bw = (W - L - R) / counts.length;
  const y = (v) => T + (1 - v / max) * (H - T - B);
  return (
    <svg viewBox={`0 0 ${W} ${H}`} style={{ width: '100%', height: 'auto' }} role="img" aria-label="Distribution of percentile finish">
      {counts.map((c, i) => (
        <rect key={i} x={L + i * bw + 1} y={y(c)} width={bw - 2} height={Math.max(0, H - B - y(c))} rx="2" fill={C_REAL}>
          <title>{`finish ${i * 5}–${i * 5 + 5}% — ${c} lineups (≈${exp.toFixed(0)} if no skill)`}</title>
        </rect>
      ))}
      <line x1={L} x2={W - R} y1={y(exp)} y2={y(exp)} stroke="var(--text-white)" strokeDasharray="4 3" strokeOpacity="0.7" />
      <line x1={L} x2={W - R} y1={H - B} y2={H - B} stroke="var(--text-muted)" />
      <text x={L} y={H - B + 12} fontSize="9" fill="var(--text-muted)">best finish</text>
      <text x={W - R} y={H - B + 12} textAnchor="end" fontSize="9" fill="var(--text-muted)">worst</text>
      <text x={W / 2} y={H - 4} textAnchor="middle" fontSize="10" fill="var(--text-main)">percentile finish vs real field</text>
    </svg>
  );
}

function Table({ cols, rows, empty = 'None.', maxHeight }) {
  if (!rows || !rows.length) return <Empty text={empty} />;
  return (
    <div style={{ overflow: 'auto', maxHeight }}>
      <table style={{ fontSize: '0.76rem', width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>{cols.map(c => <th key={c.h} style={th}>{c.h}</th>)}</tr></thead>
        <tbody>
          {rows.map((r, i) => (
            <tr key={i} style={{ borderTop: '1px solid rgba(255,255,255,0.05)' }}>
              {cols.map(c => <td key={c.h} style={{ ...td, ...(c.style ? c.style(r) : {}) }}>{c.f(r)}</td>)}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export default function TournamentPoolEval({ selectedWeek }) {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(false);
  const [view, setView] = useState('top');   // 'top' | 'repeat'

  useEffect(() => {
    let cancelled = false;
    setLoading(true); setData(null);
    ApiService.getTournamentPoolEval(selectedWeek).then(d => { if (!cancelled) { setData(d); setLoading(false); } });
    return () => { cancelled = true; };
  }, [selectedWeek]);

  if (loading) return <Empty text={`Grading Week ${selectedWeek}'s lineup pool against the real field… first load can take 20–40 seconds.`} />;
  if (!data) return null;
  if (!data.gradable) {
    return <Empty text={`Week ${selectedWeek} can't be graded yet: ${data.reason || 'no data'}. Weeks with saved builds: ${(data.available_weeks || []).join(', ') || 'none'}.`} />;
  }

  const { field, summary: s, stale, by_age: byAge, calibration: cal, ownership: own, projection: proj } = data;
  const v = s.valid || {};
  const lineupRows = view === 'top' ? data.top_lineups : data.most_regenerated;

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 16 }}>
      <div style={{ ...cardStyle, padding: '10px 14px', fontSize: '0.8rem', color: 'var(--text-muted)' }}>
        Graded against <b style={{ color: 'var(--text-main)' }}>{field.contest}</b> — {int(field.size)} entries, winner {num(field.winner_score, 2)} pts,
        {' '}{money(field.first_prize)} to 1st (payouts: {field.payout_source}). {int(s.n_builds)} pre-lock builds · {int(s.n_generated)} lineups generated ·
        {' '}{int(s.n_unique)} unique.
        {s.post_lock_builds_excluded?.length > 0 && ` ${s.post_lock_builds_excluded.length} post-lock build(s) excluded.`}
        {' '}Headline numbers are for <b style={{ color: 'var(--text-main)' }}>valid</b> lineups ({int(stale.n_valid)}); {int(stale.n_stale)} stale ones are broken out below.
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(190px, 1fr))', gap: 12 }}>
        <Tile title="Best finish" ours={`#${int(v.best_rank)}`} oursLabel={`of ${int(field.size)}`}
          note={`${num(v.best_score, 2)} pts vs winner ${num(field.winner_score, 2)} · top-10: ${s.n_top10} · top-100: ${s.n_top100}`} />
        <Tile title="Cashed" ours={pct(v.paid_pct)} oursLabel="of lineups"
          note={`top 1%: ${pct(v.top1_pct, 2)} · top 0.1%: ${pct(v.top01_pct, 3)}`} />
        <Tile title="ROI ($1 each)" ours={pct(v.roi_pct)} oursLabel="flat"
          noteColor={v.roi_pct > 0 ? '#3fb950' : undefined}
          note={`weighted by regeneration: ${pct(s.generation_weighted?.roi_pct)}`} />
        <Tile title="Avg finish" ours={`${num(v.mean_pct_finish)}%`} oursLabel="percentile"
          note="50% = a random lineup; lower is better" />
        <Tile title="Repeats" ours={num(s.mean_regen, 2)} oursLabel="× per lineup"
          note={`max ${s.max_regen}× · ${pct(s.share_gen_from_repeats_5plus)} of generations are 5×+ repeats`} />
        <Tile title="Stale lineups" ours={pct(stale.pct_stale)} oursLabel={`(${int(stale.n_stale)})`}
          noteColor={stale.pct_stale > 10 ? '#f85149' : undefined}
          note="held a player unavailable by lock" />
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: 16 }}>
        <div style={cardStyle}>
          <h3 style={h3}>Are the optimizer's probabilities honest?</h3>
          <div style={sub}>Valid lineups, split into fifths by predicted value. Cyan = predicted, amber = realized. ITM is judged against each build's own paying fraction.</div>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 14 }}>
            <CalBins title="In the money" series={cal.itm} color={C_PRED} />
            <CalBins title="Top 1%" series={cal.top1} color={C_PRED} />
            {cal.top01 && (
              <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)' }}>
                Top 0.1%: predicted {pct(cal.top01.pred_mean, 3)} · realized {pct(cal.top01.realized, 3)} (n={int(cal.top01.n)})
              </div>
            )}
            {cal.ev && (
              <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)' }}>
                EV: predicted {pct(cal.ev.pred_mean_roi_pct)} vs realized {pct(cal.ev.realized_roi_pct)}. {cal.ev.note}.
              </div>
            )}
            {cal.score && (
              <div style={{ fontSize: '0.74rem', color: 'var(--text-muted)' }}>
                Lineup score: projected mean {num(cal.score.pred_mean)} vs actual {num(cal.score.actual_mean)} · correlation {cal.score.corr ?? '—'}
              </div>
            )}
          </div>
        </div>

        <div style={cardStyle}>
          <h3 style={h3}>Where the pool finished</h3>
          <div style={sub}>Dotted line = what a no-skill pool would look like. Bars piling on the left = real edge.</div>
          <FinishHist counts={data.finish_hist} />
        </div>
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: 16 }}>
        <div style={cardStyle}>
          <h3 style={h3}>How fast do early builds go stale?</h3>
          <div style={sub}>Grouped by how long before lock each lineup's latest build was made. Late-week news (a scratch also re-prices teammates) invalidates early lineups; this shows how much.</div>
          <Table rows={byAge} empty="No build timestamps."
            cols={[
              { h: 'Built', f: r => r.label }, { h: 'Lineups', f: r => int(r.n) },
              { h: 'Stale', f: r => pct(r.pct_stale), style: r => ({ color: r.pct_stale > 10 ? '#f85149' : undefined }) },
              { h: 'Cashed', f: r => pct(r.paid_pct) }, { h: 'Avg finish', f: r => `${num(r.mean_pct_finish)}%` },
              { h: 'ROI', f: r => pct(r.roi_pct) },
            ]} />
        </div>
        <div style={cardStyle}>
          <h3 style={h3}>Players who invalidated lineups</h3>
          <div style={sub}>"ruled out" = week sheet says out · "field abandoned" = field &lt;1% though our pool expected ≥5% (scratch, benching, role change).</div>
          <Table rows={stale.players} empty="No stale players." maxHeight={260}
            cols={[
              { h: 'Player', f: r => `${r.name} (${r.team})` }, { h: 'Why', f: r => r.reason },
              { h: 'Lineups', f: r => int(r.n_lineups) }, { h: 'Pool exp.', f: r => pct(r.pool_pred_own) },
              { h: 'Field', f: r => pct(r.field_own) }, { h: 'Scored', f: r => num(r.actual) },
            ]} />
        </div>
      </div>

      <div style={cardStyle}>
        <h3 style={h3}>Who the field held that we didn't (and the reverse)</h3>
        <div style={sub}>Pool exposure = share of our unique lineups holding the player. A big "field minus pool" gap on a player who then scored is the Smith-out → Wicks effect: value the early builds couldn't see.</div>
        <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(340px, 1fr))', gap: 16 }}>
          {[['Underused (field > pool)', data.exposure_gaps?.underused], ['Overused (pool > field)', data.exposure_gaps?.overused]].map(([t, rows]) => (
            <div key={t}>
              <div style={{ fontSize: '0.78rem', fontWeight: 600, color: 'var(--text-main)', marginBottom: 4 }}>{t}</div>
              <Table rows={rows} cols={[
                { h: 'Player', f: r => r.name }, { h: 'Pool', f: r => pct(r.pool_exposure) }, { h: 'Field', f: r => pct(r.field_own) },
                { h: 'Gap', f: r => `${r.gap > 0 ? '+' : ''}${num(r.gap)}` }, { h: 'Scored', f: r => num(r.actual) },
                { h: '', f: r => (r.stale ? `⚠ ${r.stale}` : '') },
              ]} />
            </div>
          ))}
        </div>
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(190px, 1fr))', gap: 12 }}>
        <Tile title="Ownership model" ours={own.corr ?? '—'} oursLabel="corr"
          note={`bias ${num(own.bias_pred_minus_actual)} pts · MAE ${num(own.mae)} · ${own.n_with_prediction} players (scratched excluded)`} />
        <Tile title="Pool ownership sum" ours={num(own.lineup_own_sum?.pred_mean)} oursLabel="predicted"
          theirs={num(own.lineup_own_sum?.actual_mean)} theirsLabel="actual" />
        <Tile title="Field dupes" ours={num(own.field_dupes?.mean, 2)} oursLabel="avg per lineup"
          note={`${pct(own.field_dupes?.pct_lineups_with_any_dupe)} of lineups had any duplicate`} />
        <Tile title="Player projections" ours={proj.corr ?? '—'} oursLabel="corr"
          note={`bias ${num(proj.bias_pred_minus_actual)} · MAE ${num(proj.mae)} DK pts · ${proj.n} players`} />
      </div>
      {own.biggest_misses?.length > 0 && (
        <div style={cardStyle}>
          <h3 style={h3}>Largest ownership misses (available players)</h3>
          <Table rows={own.biggest_misses} cols={[
            { h: 'Player', f: r => `${r.name} (${r.team})` }, { h: 'Pos', f: r => r.pos },
            { h: 'Predicted', f: r => pct(r.pred) }, { h: 'Actual', f: r => pct(r.actual) },
          ]} />
        </div>
      )}

      <div style={cardStyle}>
        <h3 style={h3}>Build by build</h3>
        <div style={sub}>Every Optimize run for the week, oldest first. Results are for each build's valid lineups only.</div>
        <Table rows={data.by_build} maxHeight={340} cols={[
          { h: 'Built', f: r => `${hrs(r.hours_before_lock)} pre-lock` }, { h: 'Label', f: r => r.label || '' },
          { h: 'Inputs', f: r => r.inputs_hash || '—' }, { h: 'Lineups', f: r => int(r.n_lineups) },
          { h: 'Stale', f: r => pct(r.pct_stale, 0) }, { h: 'Cashed', f: r => pct(r.paid_pct) },
          { h: 'Top 1%', f: r => pct(r.top1_pct, 2) }, { h: 'Avg finish', f: r => `${num(r.mean_pct_finish)}%` },
          { h: 'Best rank', f: r => int(r.best_rank) }, { h: 'ROI', f: r => pct(r.roi_pct, 0) },
          { h: 'Pred. EV', f: r => pct(r.portfolio_ev_pct, 0) },
        ]} />
      </div>

      <div style={cardStyle}>
        <h3 style={h3}>Input versions</h3>
        <div style={sub}>Builds sharing an inputs hash used identical projections/ownership/settings. Few builds per version = noisy, compare with care.</div>
        <Table rows={[...(data.by_inputs_hash || [])].sort((a, b) => b.n - a.n)} maxHeight={260} cols={[
          { h: 'Inputs', f: r => r.inputs_hash || '—' }, { h: 'Builds', f: r => r.n_builds }, { h: 'Lineups', f: r => int(r.n) },
          { h: 'Cashed', f: r => pct(r.paid_pct) }, { h: 'Avg finish', f: r => `${num(r.mean_pct_finish)}%` },
          { h: 'Best rank', f: r => int(r.best_rank) }, { h: 'ROI', f: r => pct(r.roi_pct, 0) },
        ]} />
      </div>

      <div style={cardStyle}>
        <div style={{ display: 'flex', gap: 8, alignItems: 'center', marginBottom: 8 }}>
          <h3 style={{ ...h3, margin: 0 }}>Lineups</h3>
          {[['top', 'Best finishes'], ['repeat', 'Most regenerated']].map(([id, label]) => (
            <button key={id} onClick={() => setView(id)} style={{
              background: view === id ? 'rgba(15,163,177,0.18)' : 'rgba(0,0,0,0.25)',
              border: `1px solid ${view === id ? C_PRED : 'rgba(255,255,255,0.14)'}`, borderRadius: 6,
              color: 'var(--text-white)', padding: '4px 10px', fontSize: '0.8rem', cursor: 'pointer' }}>{label}</button>
          ))}
        </div>
        <Table rows={lineupRows} maxHeight={420} cols={[
          { h: 'Rank', f: r => int(r.rank) }, { h: 'Score', f: r => num(r.actual_score, 2) }, { h: 'Pay', f: r => money(r.payout) },
          { h: '×gen', f: r => r.n_generated }, { h: 'Pred top1%', f: r => pct(r.pred_top1, 2) },
          { h: 'Dupes', f: r => r.field_dupes }, { h: 'Own Σ', f: r => num(r.own_actual_sum, 0) },
          { h: 'Valid', f: r => (r.valid ? '✓' : `⚠ ${r.stale_players.map(p => p.name).join(', ')}`) },
          { h: 'Players', f: r => r.players.map(p => p.name.split(' ').slice(-1)[0]).join(' · '), style: () => ({ whiteSpace: 'normal', minWidth: 260 }) },
        ]} />
      </div>
    </div>
  );
}
