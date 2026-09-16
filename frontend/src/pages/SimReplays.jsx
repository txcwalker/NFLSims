import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';
import LineupHistogramModal from '../components/LineupHistogramModal';

// ─── Shared visual language (matches Optimizer.jsx / ShowdownOptimizer.jsx / EvaluationTab.jsx) ──
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };
const evColor = (v) => (v == null ? 'var(--text-muted)' : v > 0 ? 'var(--accent-primary)' : '#ef4444');

const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB:  '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC:  '#E31837',
  LV:  '#888888', LAC: '#0080C6', LA:  '#003594', LAR: '#003594', MIA: '#008E97',
  MIN: '#4F2683', NE:  '#002244', NO:  '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SF:  '#AA0000',
  SEA: '#002244', TB:  '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414',
};

function fmt(v, digits = 1) { return v == null ? '—' : Number(v).toFixed(digits); }
function fmtInt(v) { return v == null ? '—' : Number(v).toLocaleString(); }
function contestKey(c) { return `${c.year}|${c.week}|${c.slate_id}|${c.contest_name}`; }
function lastName(p) { return p.pos === 'DST' ? `${p.team} DST` : (p.name || '').split(' ').slice(-1)[0]; }

function TeamDot({ team }) {
  return <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[team] || '#888', marginRight: '4px', verticalAlign: 'middle' }} />;
}

// Classic: fixed QB/RB/RB/WR/WR/WR/FLEX/TE/DST columns, same convention as
// Optimizer.jsx's getSlottedColumns/renderSlot -- one player per column.
const CLASSIC_SLOTS = ['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'FLEX', 'TE', 'DST'];
function classicColumns(players) {
  const byPos = { QB: [], RB: [], WR: [], FLEX: [], TE: [], DST: [] };
  players.forEach(p => { const s = p.slot || p.pos; if (byPos[s]) byPos[s].push(p); });
  return [byPos.QB[0], byPos.RB[0], byPos.RB[1], byPos.WR[0], byPos.WR[1], byPos.WR[2], byPos.FLEX[0], byPos.TE[0], byPos.DST[0]];
}

// Sortable numeric columns shown in the table -- (key, label, title?, digits?, money?)
const COLUMNS = [
  { key: 'rank', label: 'Rank' },
  { key: 'actual_points', label: 'Actual Points', digits: 2 },
  { key: 'ev_pct', label: 'Sim ROI', title: 'EV%% from ranking this lineup against the real (rescored) field in every sim run, looking up the assumed payout at its implied rank', pct: true },
  { key: 'itm_pct', label: 'Cash Rate', pct: true },
  { key: 'score_max', label: 'Ceiling' },
  { key: 'lineup_p50', label: 'Median' },
  { key: 'top1_pct', label: 'Top 1%', pct: true },
  { key: 'top01_pct', label: 'Top 0.1%', pct: true },
  { key: 'total_ownership', label: 'Total Own%', title: 'Sum of real published ownership%% across every roster slot', pct: true },
];

const PAGE_SIZE = 100;

/**
 * Sim Replays: pick a contest you've actually entered (auto-found by your DK
 * username in the archived standings CSVs -- see src/api/sim_replay_store.py)
 * and see your entries plus the real top X% of the field, each rescored with
 * OUR sim against the REAL field's real rosters (not a synthetic one) -- same
 * Sim ROI/Cash Rate/Ceiling/Top1%/histogram treatment the live optimizer
 * already gives freshly-generated lineups (LineupHistogramModal), just
 * applied to real submitted lineups instead. Distinct from the "Sim Replay"
 * section on the Evaluation tab, which needs a paper-entry flag and a
 * different (percentile-only) computation.
 */
export default function SimReplays() {
  const [contests, setContests] = useState([]);
  const [loadingContests, setLoadingContests] = useState(true);
  const [selectedKey, setSelectedKey] = useState('');
  const [entries, setEntries] = useState([]);
  const [loadingEntries, setLoadingEntries] = useState(false);
  const [error, setError] = useState(null);
  const [modalLineup, setModalLineup] = useState(null);
  const [sort, setSort] = useState({ key: 'rank', asc: true });
  const [page, setPage] = useState(0);

  const [showSettings, setShowSettings] = useState(false);
  const [contestType, setContestType] = useState('top_heavy');
  const [payingPositions, setPayingPositions] = useState('');
  // Defaults to the whole field -- rescoring a real contest's full real
  // roster set is tractable up to MAX_FIELD_SIZE_FOR_RESCORE entries
  // (src/api/sim_replay_store.py), and Cam wants the full tournament by
  // default now that that's been confirmed workable, not just mine + top 1%.
  const [topPct, setTopPct] = useState(100);

  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(async () => {
      const res = await ApiService.listMyContests();
      if (cancelled) return;
      setContests(res.contests || []);
      setLoadingContests(false);
    });
    return () => { cancelled = true; };
  }, []);

  const selected = useMemo(
    () => contests.find(c => contestKey(c) === selectedKey) || contests[0] || null,
    [contests, selectedKey],
  );

  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(async () => {
      if (!selected) { setEntries([]); return; }
      setLoadingEntries(true);
      setError(null);
      const res = await ApiService.getContestFieldStats(selected.year, selected.week, selected.slate_id, selected.contest_name, {
        contestType, payingPositions: payingPositions === '' ? null : Number(payingPositions), topPct: Number(topPct) || 1,
      });
      if (cancelled) return;
      if (res.error) setError(res.error);
      setEntries(res.entries || []);
      setLoadingEntries(false);
      setPage(0);
    });
    return () => { cancelled = true; };
  }, [selected, contestType, payingPositions, topPct]);

  const sortedEntries = useMemo(() => {
    const arr = [...entries];
    arr.sort((a, b) => {
      const av = a[sort.key], bv = b[sort.key];
      if (av == null && bv == null) return 0;
      if (av == null) return 1;
      if (bv == null) return -1;
      return sort.asc ? av - bv : bv - av;
    });
    return arr;
  }, [entries, sort]);

  // The backend already scores the whole field -- pagination here is purely
  // to keep the DOM light (a "whole tournament" contest can be thousands of
  // rows); sorting/highlighting still apply across all of them, just one
  // page's worth renders at a time.
  const pageCount = Math.max(1, Math.ceil(sortedEntries.length / PAGE_SIZE));
  const pagedEntries = sortedEntries.slice(page * PAGE_SIZE, (page + 1) * PAGE_SIZE);

  const toggleSort = (key) => { setSort(s => (s.key === key ? { key, asc: !s.asc } : { key, asc: true })); setPage(0); };

  const isClassic = selected?.slate_format === 'classic';

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      <div className="glass-panel" style={{
        marginBottom: '18px', padding: '12px 20px', borderRadius: '12px',
        border: '1px solid var(--border-glass)', display: 'flex', gap: '18px',
        alignItems: 'center', flexWrap: 'wrap',
      }}>
        <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>🔁 Sim Replays</span>
        <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
          <span style={{ color: 'var(--text-muted)' }}>Contest</span>
          <select value={selected ? contestKey(selected) : ''} onChange={e => setSelectedKey(e.target.value)}
            style={{ ...inputStyle, width: 'auto', minWidth: '320px' }}>
            {contests.length === 0 && <option value="">— no entered contests found —</option>}
            {contests.map(c => (
              <option key={contestKey(c)} value={contestKey(c)}>
                Week {c.week} · {c.away_team && c.home_team ? `${c.away_team}@${c.home_team}` : c.slate_id} · {c.contest_name} (${c.entry_fee}/{c.max_entries}-max) · {c.my_entry_count} {c.my_entry_count === 1 ? 'entry' : 'entries'} · field {fmtInt(c.field_size)}
              </option>
            ))}
          </select>
        </label>
        <button onClick={() => setShowSettings(s => !s)} style={{ ...inputStyle, cursor: 'pointer' }}>
          ⚙️ Settings
        </button>
        {(loadingContests || loadingEntries) && <span style={{ fontSize: '0.78rem', color: 'var(--text-muted)' }}>loading…</span>}
      </div>

      {showSettings && (
        <div className="glass-panel" style={{
          marginBottom: '18px', padding: '10px 20px', borderRadius: '12px',
          border: '1px solid var(--border-glass)', display: 'flex', gap: '18px', alignItems: 'center', flexWrap: 'wrap',
        }}>
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.8rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>Payout curve</span>
            <select value={contestType} onChange={e => setContestType(e.target.value)} style={{ ...inputStyle, width: 'auto' }}>
              <option value="extreme_top_heavy">Extreme top-heavy</option>
              <option value="top_heavy">Top-heavy</option>
              <option value="flat">Flat</option>
              <option value="cash">Cash (50/50)</option>
            </select>
          </label>
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.8rem' }}>
            <span style={{ color: 'var(--text-muted)' }} title="How many places actually cash -- defaults to ~20% of the field if left blank">Paying positions</span>
            <input type="number" value={payingPositions} onChange={e => setPayingPositions(e.target.value)}
              placeholder={selected ? `~${Math.max(1, Math.round(selected.field_size * 0.2))} (default)` : 'default'}
              style={{ ...inputStyle, width: '140px' }} />
          </label>
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.8rem' }}>
            <span style={{ color: 'var(--text-muted)' }} title="Include the real top X% of the field alongside your own entries">Top % of field</span>
            <input type="number" min="0" max="100" step="0.5" value={topPct} onChange={e => setTopPct(e.target.value)}
              style={{ ...inputStyle, width: '70px' }} />
          </label>
          <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>
            No real DK payout table is archived for a settled contest, so this picks an assumed curve for Sim ROI -- set it to match the real contest once you know it.
          </span>
        </div>
      )}

      {loadingContests ? null : contests.length === 0 ? (
        <div style={{ ...cardStyle, textAlign: 'center', color: 'var(--text-muted)', padding: '40px' }}>
          No entered contests found. Drop a settled standings CSV into its <code>data/dfs_ownership/&lt;year&gt;/week_NN/&lt;slate&gt;/</code>{' '}
          folder — this page finds your own rows automatically by DK username (configured in{' '}
          <code>data/dfs_ownership/config.json</code>).
        </div>
      ) : error ? (
        <div style={{ ...cardStyle, textAlign: 'center', color: '#ef4444', padding: '40px' }}>{error}</div>
      ) : (
        <div style={cardStyle}>
          <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', margin: '0 0 10px 0' }}>
            Your entries (highlighted){Number(topPct) >= 100 ? ' plus the whole field' : ` plus the real top ${topPct}% of the field`} — every real submitted
            roster rescored with our own sim and ranked against each other, the field this contest actually had. Click a row for the
            full range-of-outcomes breakdown.
          </p>
          {entries.length === 0 ? (
            <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem', padding: '10px' }}>No entries found.</div>
          ) : (
            <div className="table-container" style={{ overflowX: 'auto' }}>
              <table style={{ fontSize: '0.78rem', width: '100%' }}>
                <thead>
                  <tr style={{ textAlign: 'left' }}>
                    {(isClassic ? CLASSIC_SLOTS : ['CPT', 'FLEX']).map((slot, i) => (
                      <th key={`${slot}${i}`} style={{ padding: '6px 5px', fontSize: '0.68rem', color: 'var(--text-muted)' }}>{slot}</th>
                    ))}
                    {COLUMNS.map(c => (
                      <th key={c.key} title={c.title} onClick={() => toggleSort(c.key)}
                        style={{ padding: '6px 5px', cursor: 'pointer', whiteSpace: 'nowrap' }}>
                        {c.label} {sort.key === c.key ? (sort.asc ? '↑' : '↓') : ''}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {pagedEntries.map((e, i) => {
                    const cpt = !isClassic ? e.players.find(p => p.slot === 'CPT') : null;
                    const flex = !isClassic ? e.players.filter(p => p.slot !== 'CPT') : null;
                    return (
                      <tr key={e.entry_id || i} onClick={() => setModalLineup(e)}
                        style={{
                          borderBottom: '1px solid rgba(255,255,255,0.04)', cursor: 'pointer',
                          background: e.is_mine ? 'rgba(34,211,238,0.06)' : 'transparent',
                        }}>
                        {isClassic ? (
                          classicColumns(e.players).map((p, pi) => (
                            <td key={pi} style={{ padding: '5px 6px', whiteSpace: 'nowrap' }}>
                              {p ? <><TeamDot team={p.team} /><span style={{ fontWeight: 600 }}>{lastName(p)}</span></> : <span style={{ color: 'rgba(255,255,255,0.2)' }}>—</span>}
                            </td>
                          ))
                        ) : (
                          <>
                            <td style={{ padding: '5px 6px', whiteSpace: 'nowrap', fontWeight: 700 }}>
                              {cpt ? <><TeamDot team={cpt.team} />{lastName(cpt)}</> : '—'}
                            </td>
                            <td style={{ padding: '5px 6px', whiteSpace: 'nowrap' }}>{flex.map(lastName).join(', ')}</td>
                          </>
                        )}
                        <td style={{ padding: '6px 5px' }}>
                          {fmtInt(e.rank)}
                          {e.is_mine && <div style={{ fontSize: '0.65rem', color: 'var(--accent-primary)', fontWeight: 700 }}>MINE</div>}
                        </td>
                        <td style={{ padding: '6px 5px', fontWeight: 700 }}>{fmt(e.actual_points, 2)}</td>
                        <td style={{ padding: '6px 5px', color: evColor(e.ev_pct) }}>{e.ev_pct != null ? `${e.ev_pct > 0 ? '+' : ''}${fmt(e.ev_pct)}%` : '—'}</td>
                        <td style={{ padding: '6px 5px' }}>{e.itm_pct != null ? `${fmt(e.itm_pct)}%` : '—'}</td>
                        <td style={{ padding: '6px 5px', color: 'var(--accent-gold)' }}>{fmt(e.score_max)}</td>
                        <td style={{ padding: '6px 5px' }}>{fmt(e.lineup_p50)}</td>
                        <td style={{ padding: '6px 5px' }}>{e.top1_pct != null ? `${fmt(e.top1_pct)}%` : '—'}</td>
                        <td style={{ padding: '6px 5px' }}>{e.top01_pct != null ? `${fmt(e.top01_pct)}%` : '—'}</td>
                        <td style={{ padding: '6px 5px' }}>{e.total_ownership != null ? `${fmt(e.total_ownership)}%` : '—'}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          )}
          {pageCount > 1 && (
            <div style={{ display: 'flex', alignItems: 'center', gap: '10px', marginTop: '10px', fontSize: '0.78rem' }}>
              <button onClick={() => setPage(p => Math.max(0, p - 1))} disabled={page === 0}
                style={{ ...inputStyle, cursor: page === 0 ? 'default' : 'pointer', opacity: page === 0 ? 0.4 : 1 }}>← Prev</button>
              <span style={{ color: 'var(--text-muted)' }}>
                Page {page + 1} of {pageCount} · {sortedEntries.length.toLocaleString()} lineups
              </span>
              <button onClick={() => setPage(p => Math.min(pageCount - 1, p + 1))} disabled={page >= pageCount - 1}
                style={{ ...inputStyle, cursor: page >= pageCount - 1 ? 'default' : 'pointer', opacity: page >= pageCount - 1 ? 0.4 : 1 }}>Next →</button>
            </div>
          )}
        </div>
      )}

      {modalLineup && <LineupHistogramModal lineup={modalLineup} onClose={() => setModalLineup(null)} />}
    </div>
  );
}
