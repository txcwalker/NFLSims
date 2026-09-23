import { Fragment, useEffect, useState } from 'react';
import { ApiService } from '../api';

// ─── Shared visual language (matches Optimizer.jsx / SimReplays.jsx / EvaluationTab.jsx) ──
const cardStyle = { background: 'rgba(255,255,255,0.02)', border: '1px solid var(--border-glass)', borderRadius: '14px', padding: '16px' };
const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };
const pnlColor = (v) => (v == null ? 'var(--text-muted)' : v > 0 ? 'var(--accent-green, #22c55e)' : v < 0 ? '#ef4444' : 'var(--text-secondary)');

const KIND_LABELS = {
  paper_baseline: 'Paper · Baseline',
  paper_catered: 'Paper · Catered',
  real: 'Real money',
  high_stakes: 'High Stakes',
};

function money(v) {
  if (v == null) return '—';
  const sign = v < 0 ? '-' : '';
  return `${sign}$${Math.abs(v).toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
}
function pct(v) { return v == null ? '—' : `${v > 0 ? '+' : ''}${v.toFixed(1)}%`; }

/** One TOTAL COST / PENDING / SETTLED WINNINGS / NET P&L row. `label`
 * (optional) captions the row -- only shown once there's more than one row
 * to tell apart, e.g. once a High Stakes account splits off its own totals. */
function TotalsRow({ label, totals, accent = false }) {
  return (
    <div style={{ ...cardStyle, display: 'flex', flexDirection: 'column', gap: '8px',
      border: accent ? '1px solid rgba(234,179,8,0.35)' : cardStyle.border }}>
      {label && <div style={{ fontSize: '0.68rem', color: accent ? '#eab308' : 'var(--text-muted)', fontWeight: 600, letterSpacing: '0.03em' }}>{label}</div>}
      <div style={{ display: 'flex', gap: '24px', flexWrap: 'wrap' }}>
        <div>
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>TOTAL COST</div>
          <div style={{ fontSize: '1.1rem' }}>{money(totals.cost_total)}</div>
        </div>
        <div>
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>PENDING (UNSETTLED)</div>
          <div style={{ fontSize: '1.1rem' }}>{money(totals.cost_pending)}</div>
        </div>
        <div>
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>SETTLED WINNINGS</div>
          <div style={{ fontSize: '1.1rem' }}>{money(totals.winnings_settled)}</div>
        </div>
        <div>
          <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>NET P&amp;L (SETTLED)</div>
          <div style={{ fontSize: '1.1rem', color: pnlColor(totals.net_pnl_settled) }}>{money(totals.net_pnl_settled)}</div>
        </div>
      </div>
    </div>
  );
}

/**
 * Bankroll: per-account roll-up of every Build tagged with an account_id
 * (see account_store.py / app.py's /api/bankroll) -- cost charged the moment
 * a build is tagged, settled winnings once
 * scripts/dfs_ownership/score_paper_entries.py has a real standings CSV to
 * work from (estimated_payout -- a backtested real score against a generic
 * payout curve, not DK's own, which isn't observable; see that script's
 * _estimated_payout). Pending = tagged but not yet settled, not counted in
 * net P&L. Classic-only for now, same scope as the Builds system this reads.
 */
export default function Bankroll() {
  const [accounts, setAccounts] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [editingId, setEditingId] = useState(null);
  const [newLabel, setNewLabel] = useState('');
  const [newKind, setNewKind] = useState('paper_baseline');
  const [viewingAccount, setViewingAccount] = useState(null);
  const [grading, setGrading] = useState(false);
  const [gradeStatus, setGradeStatus] = useState('');

  const reload = () => {
    setLoading(true);
    ApiService.getBankroll().then(rows => { setAccounts(rows); setLoading(false); })
      .catch(() => { setError('Failed to load bankroll data'); setLoading(false); });
  };

  // Re-settle every paper entry against whatever standings CSVs have been
  // dropped in (see score_paper_entries.py) before showing numbers -- cheap
  // and idempotent, so it's safe to run on every page load, not just on
  // click. The "Grade Now" button below calls the same function on demand.
  const runGrading = () => {
    setGrading(true);
    ApiService.gradePaperEntries().then(res => {
      setGrading(false);
      setGradeStatus(res ? `Graded ${res.graded} settled entr${res.graded === 1 ? 'y' : 'ies'}` : 'Grading failed — is the backend running?');
      reload();
    });
  };
  // Deferred into a microtask (not called directly as the effect body) so
  // this doesn't trip react-hooks/set-state-in-effect -- same pattern as
  // useWorkspaceSlots / EvaluationTab's load effect.
  useEffect(() => {
    let cancelled = false;
    Promise.resolve().then(() => { if (!cancelled) runGrading(); });
    return () => { cancelled = true; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const saveStartingBankroll = async (accountId, value) => {
    const n = parseFloat(value);
    await ApiService.patchAccount(accountId, { starting_bankroll: Number.isFinite(n) ? n : 0 });
    setEditingId(null);
    reload();
  };

  const addAccount = async () => {
    if (!newLabel.trim()) return;
    await ApiService.createAccount(newLabel.trim(), newKind, 0);
    setNewLabel('');
    reload();
  };

  const clearAccount = async (a) => {
    if (!window.confirm(
      `Clear "${a.label}"? This un-tags every Build currently pointed at this account ` +
      `(the lineups themselves are NOT deleted -- still there in the Optimizer's Builds panel, ` +
      `just no longer tracked here) and deletes its ${a.n_lineups_total} paper entr${a.n_lineups_total === 1 ? 'y' : 'ies'}. ` +
      `Use this before rerunning lineups against corrected data.`
    )) return;
    await ApiService.clearBankrollAccount(a.account_id);
    reload();
  };

  const sumTotals = (list) => list.reduce((acc, a) => ({
    cost_total: acc.cost_total + (a.cost_total || 0),
    cost_pending: acc.cost_pending + (a.cost_pending || 0),
    winnings_settled: acc.winnings_settled + (a.winnings_settled || 0),
    net_pnl_settled: acc.net_pnl_settled + (a.net_pnl_settled || 0),
  }), { cost_total: 0, cost_pending: 0, winnings_settled: 0, net_pnl_settled: 0 });
  // High Stakes accounts get their own totals row instead of being blended
  // into the main one -- a big-stakes account would otherwise dominate/
  // distort the headline P&L for everything else.
  const highStakesAccounts = accounts.filter(a => a.kind === 'high_stakes');
  const otherAccounts = accounts.filter(a => a.kind !== 'high_stakes');
  const totals = sumTotals(otherAccounts);
  const highStakesTotals = sumTotals(highStakesAccounts);

  if (viewingAccount) {
    return <AccountDetail account={viewingAccount} onBack={() => setViewingAccount(null)} />;
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>
      <div>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px', flexWrap: 'wrap' }}>
          <h1 style={{ fontSize: '1.4rem', margin: 0 }}>🏦 Bankroll</h1>
          <button onClick={runGrading} disabled={grading} title="Rescan every paper entry against any dropped-in standings CSV"
            style={{ ...inputStyle, cursor: grading ? 'default' : 'pointer', fontSize: '0.75rem', opacity: grading ? 0.6 : 1 }}>
            {grading ? 'Grading…' : '🔄 Grade Now'}
          </button>
          {gradeStatus && !grading && <span style={{ fontSize: '0.72rem', color: 'var(--text-muted)' }}>{gradeStatus}</span>}
        </div>
        <div style={{ color: 'var(--text-muted)', fontSize: '0.85rem', marginTop: '4px' }}>
          Every account rolls up the Builds tagged to it (Optimizer → Builds panel → Account column).
          Cost is charged the moment a build is tagged; winnings only count once
          real results are settled (drop a standings CSV in — grading re-runs automatically on load,
          or click Grade Now) — until then an entry shows as pending.
        </div>
      </div>

      {loading && <div style={{ color: 'var(--text-muted)' }}>Loading…</div>}
      {error && <div style={{ color: '#ef4444' }}>{error}</div>}

      {!loading && !error && (
        <>
          <TotalsRow label={highStakesAccounts.length ? 'ALL OTHER ACCOUNTS' : null} totals={totals} />
          {highStakesAccounts.length > 0 && <TotalsRow label="HIGH STAKES" totals={highStakesTotals} accent />}

          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '14px' }}>
            {accounts.map(a => (
              <div key={a.account_id} style={cardStyle}>
                <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline' }}>
                  <div style={{ fontWeight: 600, cursor: 'pointer' }} title="View every lineup in this account"
                    onClick={() => setViewingAccount(a)}>
                    {a.label} <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)' }}>🔍</span>
                  </div>
                  <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                    <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', textTransform: 'uppercase' }}>
                      {KIND_LABELS[a.kind] || a.kind}
                    </div>
                    {a.n_lineups_total > 0 && (
                      <button onClick={() => clearAccount(a)} title="Un-tag this account's builds and delete its paper entries -- e.g. before rerunning lineups against corrected data"
                        style={{ ...inputStyle, padding: '2px 6px', fontSize: '0.68rem', cursor: 'pointer', color: '#ef4444', borderColor: 'rgba(239,68,68,0.3)' }}>
                        Clear
                      </button>
                    )}
                  </div>
                </div>

                <div style={{ marginTop: '10px', display: 'grid', gridTemplateColumns: '1fr 1fr', rowGap: '6px', fontSize: '0.82rem' }}>
                  <span style={{ color: 'var(--text-muted)' }}>Starting bankroll</span>
                  {editingId === a.account_id ? (
                    <input autoFocus defaultValue={a.starting_bankroll} style={{ ...inputStyle, width: '90px', justifySelf: 'end' }}
                      onBlur={e => saveStartingBankroll(a.account_id, e.target.value)}
                      onKeyDown={e => { if (e.key === 'Enter') e.target.blur(); }} />
                  ) : (
                    <span style={{ justifySelf: 'end', cursor: 'pointer', textDecoration: 'underline dotted' }}
                      title="Click to edit" onClick={() => setEditingId(a.account_id)}>
                      {money(a.starting_bankroll)}
                    </span>
                  )}

                  <span style={{ color: 'var(--text-muted)' }}>Builds / lineups</span>
                  <span style={{ justifySelf: 'end' }}>{a.n_builds} / {a.n_lineups_total}</span>

                  <span style={{ color: 'var(--text-muted)' }}>Cost (total)</span>
                  <span style={{ justifySelf: 'end' }}>{money(a.cost_total)}</span>

                  <span style={{ color: 'var(--text-muted)' }}>Cost (pending)</span>
                  <span style={{ justifySelf: 'end' }}>{money(a.cost_pending)}</span>

                  <span style={{ color: 'var(--text-muted)' }}>Settled: {a.n_lineups_settled} / {a.n_lineups_total}</span>
                  <span style={{ justifySelf: 'end' }}>{money(a.winnings_settled)} won</span>

                  <span style={{ color: 'var(--text-muted)', fontWeight: 600 }}>Net P&amp;L (settled)</span>
                  <span style={{ justifySelf: 'end', fontWeight: 600, color: pnlColor(a.net_pnl_settled) }}>{money(a.net_pnl_settled)}</span>

                  <span style={{ color: 'var(--text-muted)' }}>ROI (settled)</span>
                  <span style={{ justifySelf: 'end', color: pnlColor(a.roi_settled_pct) }}>{pct(a.roi_settled_pct)}</span>

                  <span style={{ color: 'var(--text-muted)', fontWeight: 600 }}>Current bankroll</span>
                  <span style={{ justifySelf: 'end', fontWeight: 600 }}>{money(a.current_bankroll)}</span>
                </div>

                {a.weeks && a.weeks.length > 0 && (
                  <table style={{ width: '100%', fontSize: '0.72rem', marginTop: '12px', borderCollapse: 'collapse' }}>
                    <thead>
                      <tr style={{ color: 'var(--text-muted)', textAlign: 'right' }}>
                        <th style={{ textAlign: 'left', padding: '3px 4px' }}>Wk</th>
                        <th style={{ padding: '3px 4px' }}>Lineups</th>
                        <th style={{ padding: '3px 4px' }}>Cost</th>
                        <th style={{ padding: '3px 4px' }}>Settled</th>
                        <th style={{ padding: '3px 4px' }}>Won</th>
                      </tr>
                    </thead>
                    <tbody>
                      {a.weeks.map(w => (
                        <tr key={w.week} style={{ borderTop: '1px solid var(--border-glass)', textAlign: 'right' }}>
                          <td style={{ textAlign: 'left', padding: '3px 4px' }}>{w.week}</td>
                          <td style={{ padding: '3px 4px' }}>{w.n_lineups}</td>
                          <td style={{ padding: '3px 4px' }}>{money(w.cost)}</td>
                          <td style={{ padding: '3px 4px' }}>{w.n_settled}/{w.n_lineups}</td>
                          <td style={{ padding: '3px 4px' }}>{money(w.winnings_settled)}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                )}
              </div>
            ))}
          </div>

          <div style={{ ...cardStyle, display: 'flex', gap: '8px', alignItems: 'center', flexWrap: 'wrap' }}>
            <span style={{ fontSize: '0.8rem', color: 'var(--text-muted)' }}>Add account:</span>
            <input placeholder="Label" value={newLabel} onChange={e => setNewLabel(e.target.value)}
              style={{ ...inputStyle, width: '180px' }} />
            <select value={newKind} onChange={e => setNewKind(e.target.value)} style={{ ...inputStyle, width: '160px' }}>
              <option value="paper_baseline">Paper · Baseline</option>
              <option value="paper_catered">Paper · Catered</option>
              <option value="real">Real money</option>
              <option value="high_stakes">High Stakes</option>
            </select>
            <button onClick={addAccount} style={{ ...inputStyle, cursor: 'pointer', background: 'rgba(59,130,246,0.15)', borderColor: 'rgba(59,130,246,0.4)' }}>
              + Add
            </button>
          </div>
        </>
      )}
    </div>
  );
}

/** Roll-up stats for one set of entries (a build's, or several builds'
 * combined for a week) -- entry fees, settled P&L/ROI, and top-finish
 * counts (Top 1% / Top 0.1% / ITM, read off each entry's own
 * finish_percentile / estimated_payout -- no extra backend call needed).
 * ROI/P&L only ever count SETTLED entries, same convention as the account
 * cards above (net_pnl_settled) -- a pending lineup has no result to weigh
 * in yet. */
function computeStats(entries, entryFee) {
  const settled = entries.filter(e => e.settled);
  const cost_total = entries.length * entryFee;
  const cost_settled = settled.length * entryFee;
  const won = settled.reduce((s, e) => s + (e.estimated_payout || 0), 0);
  return {
    nTotal: entries.length, nSettled: settled.length,
    cost_total, cost_settled, won, pnl: won - cost_settled,
    top1: entries.filter(e => e.finish_percentile != null && e.finish_percentile <= 1).length,
    top01: entries.filter(e => e.finish_percentile != null && e.finish_percentile <= 0.1).length,
    itm: entries.filter(e => e.estimated_payout != null && e.estimated_payout > 0).length,
  };
}
/** Sums several computeStats() results (e.g. every build in one week) into one. */
function sumStats(list) {
  const s = list.reduce((a, x) => ({
    nTotal: a.nTotal + x.nTotal, nSettled: a.nSettled + x.nSettled,
    cost_total: a.cost_total + x.cost_total, cost_settled: a.cost_settled + x.cost_settled,
    won: a.won + x.won, top1: a.top1 + x.top1, top01: a.top01 + x.top01, itm: a.itm + x.itm,
  }), { nTotal: 0, nSettled: 0, cost_total: 0, cost_settled: 0, won: 0, top1: 0, top01: 0, itm: 0 });
  return { ...s, pnl: s.won - s.cost_settled };
}
function roiOf(stats) { return stats.cost_settled ? (stats.pnl / stats.cost_settled) * 100 : null; }

/** Drill-down for one account: every build's lineups, contest info, settled
 * real result once available, and an editable notes/late-swap annotation
 * per lineup (see paper_store.update_entry). */
function AccountDetail({ account, onBack }) {
  const [builds, setBuilds] = useState([]);
  const [loading, setLoading] = useState(true);
  const [openBuildId, setOpenBuildId] = useState(null);
  const [openPlayersId, setOpenPlayersId] = useState(null);
  const [sortKey, setSortKey] = useState(null);
  const [sortDir, setSortDir] = useState('asc');

  const toggleSort = (key) => {
    if (sortKey === key) { setSortDir(d => d === 'asc' ? 'desc' : 'asc'); }
    else { setSortKey(key); setSortDir('asc'); }
  };
  // Nulls (not-yet-settled columns) always sort last regardless of direction.
  const sortEntries = (entries) => {
    if (!sortKey) return entries;
    const dir = sortDir === 'asc' ? 1 : -1;
    return [...entries].sort((a, b) => {
      const av = a[sortKey], bv = b[sortKey];
      if (av == null && bv == null) return 0;
      if (av == null) return 1;
      if (bv == null) return -1;
      return av > bv ? dir : av < bv ? -dir : 0;
    });
  };
  const SortTh = ({ label, sortKeyName }) => (
    <th onClick={() => toggleSort(sortKeyName)} title="Click to sort"
      style={{ padding: '3px 4px', cursor: 'pointer', userSelect: 'none' }}>
      {label}{sortKey === sortKeyName ? (sortDir === 'asc' ? ' ▲' : ' ▼') : ''}
    </th>
  );

  useEffect(() => {
    setLoading(true);
    ApiService.getBankrollAccountEntries(account.account_id).then(bs => {
      setBuilds(bs);
      setLoading(false);
      if (bs.length === 1) setOpenBuildId(bs[0].build_id);
    });
  }, [account.account_id]);

  const patchEntry = async (build, entryId, patch) => {
    const saved = await ApiService.patchPaperEntry(entryId, build.slate_id, build.week, patch);
    if (!saved) return;
    setBuilds(bs => bs.map(b => b.build_id !== build.build_id ? b : {
      ...b,
      entries: b.entries.map(e => e.entry_id === entryId ? { ...e, notes: saved.notes, late_swap: !!saved.late_swap } : e),
    }));
  };

  // Safety net for a lineup filed under the wrong contest (or any other
  // mis-tag) -- pulls it out of bankroll tracking entirely without touching
  // the Optimizer's own Builds panel. Does not delete/undo the Build itself.
  const removeEntry = async (build, entryId, label) => {
    if (!window.confirm(`Remove "${label}" from this account's bankroll tracking? This can't be undone.`)) return;
    const ok = await ApiService.deletePaperEntry(entryId, build.slate_id, build.week);
    if (!ok) { window.alert('Remove failed — is the backend running?'); return; }
    setBuilds(bs => bs.map(b => b.build_id !== build.build_id ? b : {
      ...b,
      entries: b.entries.filter(e => e.entry_id !== entryId),
    }));
  };

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '14px' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
        <button onClick={onBack} style={{ ...inputStyle, cursor: 'pointer' }}>← Back</button>
        <h1 style={{ fontSize: '1.2rem', margin: 0 }}>{account.label}</h1>
        <span style={{ fontSize: '0.7rem', color: 'var(--text-muted)', textTransform: 'uppercase' }}>
          {KIND_LABELS[account.kind] || account.kind}
        </span>
      </div>

      {loading && <div style={{ color: 'var(--text-muted)' }}>Loading…</div>}
      {!loading && builds.length === 0 && (
        <div style={{ color: 'var(--text-muted)' }}>No builds tagged to this account yet.</div>
      )}

      {!loading && builds.length > 0 && (() => {
        const byWeek = {};
        for (const b of builds) {
          if (!byWeek[b.week]) byWeek[b.week] = [];
          byWeek[b.week].push(b);
        }
        const weeks = Object.keys(byWeek).map(Number).sort((a, z) => a - z);
        return (
          <div style={{ ...cardStyle, overflowX: 'auto' }}>
            <div style={{ fontSize: '0.72rem', color: 'var(--text-muted)', fontWeight: 600, marginBottom: '8px' }}>TOTALS BY WEEK</div>
            <table style={{ width: '100%', fontSize: '0.76rem', borderCollapse: 'collapse' }}>
              <thead>
                <tr style={{ color: 'var(--text-muted)', textAlign: 'right' }}>
                  <th style={{ textAlign: 'left', padding: '3px 6px' }}>Week</th>
                  <th style={{ padding: '3px 6px' }}>Lineups</th>
                  <th style={{ padding: '3px 6px' }}>Settled</th>
                  <th style={{ padding: '3px 6px' }}>Entry Fees</th>
                  <th style={{ padding: '3px 6px' }}>ROI</th>
                  <th style={{ padding: '3px 6px' }}>P&amp;L</th>
                  <th style={{ padding: '3px 6px' }}>Top 1%</th>
                  <th style={{ padding: '3px 6px' }}>Top 0.1%</th>
                  <th style={{ padding: '3px 6px' }}>ITM</th>
                </tr>
              </thead>
              <tbody>
                {weeks.map(wk => {
                  const s = sumStats(byWeek[wk].map(b => computeStats(b.entries, b.entry_fee)));
                  return (
                    <tr key={wk} style={{ borderTop: '1px solid var(--border-glass)', textAlign: 'right' }}>
                      <td style={{ textAlign: 'left', padding: '3px 6px' }}>{wk}</td>
                      <td style={{ padding: '3px 6px' }}>{s.nTotal}</td>
                      <td style={{ padding: '3px 6px' }}>{s.nSettled}/{s.nTotal}</td>
                      <td style={{ padding: '3px 6px' }}>{money(s.cost_total)}</td>
                      <td style={{ padding: '3px 6px', color: pnlColor(roiOf(s)) }}>{pct(roiOf(s))}</td>
                      <td style={{ padding: '3px 6px', color: pnlColor(s.pnl) }}>{money(s.pnl)}</td>
                      <td style={{ padding: '3px 6px' }}>{s.top1}</td>
                      <td style={{ padding: '3px 6px' }}>{s.top01}</td>
                      <td style={{ padding: '3px 6px' }}>{s.itm}</td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        );
      })()}

      {builds.map(b => {
        const isOpen = openBuildId === b.build_id;
        const stats = computeStats(b.entries, b.entry_fee);
        const roi = roiOf(stats);
        return (
          <div key={b.build_id} style={cardStyle}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', cursor: 'pointer' }}
              onClick={() => setOpenBuildId(isOpen ? null : b.build_id)}>
              <div>
                <div>
                  <span style={{ fontWeight: 600 }}>{isOpen ? '▾' : '▸'} {b.label || b.build_id}</span>
                  <span style={{ marginLeft: '10px', fontSize: '0.75rem', color: 'var(--text-muted)' }}>
                    Wk {b.week} · {b.contest_name} · {money(b.entry_fee)}/entry · {b.entries.length} lineups · {stats.nSettled} settled
                  </span>
                </div>
                <div style={{ marginTop: '3px', fontSize: '0.72rem', color: 'var(--text-muted)' }}>
                  Entry fees {money(stats.cost_total)} · ROI <span style={{ color: pnlColor(roi) }}>{pct(roi)}</span>
                  {' '}· P&amp;L <span style={{ color: pnlColor(stats.pnl) }}>{money(stats.pnl)}</span>
                  {' '}· Top 1% {stats.top1} · Top 0.1% {stats.top01} · ITM {stats.itm}
                </div>
              </div>
            </div>

            {isOpen && (
              <div style={{ overflowX: 'auto', marginTop: '10px' }}>
                <table style={{ width: '100%', fontSize: '0.74rem', borderCollapse: 'collapse' }}>
                  <thead>
                    <tr style={{ color: 'var(--text-muted)', textAlign: 'right' }}>
                      <th style={{ textAlign: 'left', padding: '3px 4px' }}></th>
                      <SortTh label="Predicted" sortKeyName="predicted_score" />
                      <SortTh label="Actual" sortKeyName="actual_score" />
                      <SortTh label="Rank" sortKeyName="actual_rank" />
                      <SortTh label="Finish %ile" sortKeyName="finish_percentile" />
                      <SortTh label="Payout" sortKeyName="estimated_payout" />
                      <th style={{ textAlign: 'left', padding: '3px 4px' }}>Late swap</th>
                      <th style={{ textAlign: 'left', padding: '3px 4px' }}>Notes</th>
                      <th style={{ padding: '3px 4px' }}></th>
                    </tr>
                  </thead>
                  <tbody>
                    {sortEntries(b.entries).map((e, i) => (
                      <Fragment key={e.entry_id}>
                        <tr style={{ borderTop: '1px solid var(--border-glass)', textAlign: 'right' }}>
                          <td style={{ textAlign: 'left', padding: '3px 4px' }}>
                            <span style={{ cursor: 'pointer', color: 'var(--text-muted)' }}
                              onClick={() => setOpenPlayersId(openPlayersId === e.entry_id ? null : e.entry_id)}>
                              #{i + 1} {openPlayersId === e.entry_id ? '▾' : '▸'}
                            </span>
                          </td>
                          <td style={{ padding: '3px 4px' }}>{e.predicted_score ?? '—'}</td>
                          <td style={{ padding: '3px 4px' }}>{e.settled ? e.actual_score : (e.actual_score != null ? e.actual_score : 'pending')}</td>
                          <td style={{ padding: '3px 4px' }}>{e.actual_rank != null ? `${e.actual_rank.toLocaleString()} / ${e.field_size?.toLocaleString() ?? '—'}` : '—'}</td>
                          <td style={{ padding: '3px 4px' }}>{e.finish_percentile != null ? `${e.finish_percentile}%` : '—'}</td>
                          <td style={{ padding: '3px 4px', color: pnlColor(e.estimated_payout != null ? e.estimated_payout - b.entry_fee : null) }}>
                            {e.estimated_payout != null ? money(e.estimated_payout) : (e.settled ? 'n/a' : '—')}
                          </td>
                          <td style={{ textAlign: 'left', padding: '3px 4px' }}>
                            <input type="checkbox" checked={!!e.late_swap}
                              onChange={ev => patchEntry(b, e.entry_id, { late_swap: ev.target.checked })} />
                          </td>
                          <td style={{ textAlign: 'left', padding: '3px 4px' }}>
                            <input defaultValue={e.notes || ''} placeholder="—"
                              onBlur={ev => { const v = ev.target.value.trim() || null; if (v !== e.notes) patchEntry(b, e.entry_id, { notes: v }); }}
                              style={{ ...inputStyle, padding: '2px 5px', fontSize: '0.72rem', width: '140px' }} />
                          </td>
                          <td style={{ padding: '3px 4px' }}>
                            <button onClick={() => removeEntry(b, e.entry_id, b.label || `Lineup #${i + 1}`)}
                              title="Remove this lineup from bankroll tracking (e.g. filed under the wrong contest)"
                              style={{ ...inputStyle, padding: '2px 6px', fontSize: '0.68rem', cursor: 'pointer', color: '#ef4444', borderColor: 'rgba(239,68,68,0.3)' }}>
                              🗑
                            </button>
                          </td>
                        </tr>
                        {openPlayersId === e.entry_id && (
                          <tr>
                            <td colSpan={9} style={{ padding: '4px 4px 8px 24px', color: 'var(--text-secondary)' }}>
                              {e.players.map(p => `${p.slot || p.pos}: ${p.name} (${p.team})`).join('  ·  ')}
                            </td>
                          </tr>
                        )}
                      </Fragment>
                    ))}
                  </tbody>
                </table>
              </div>
            )}
          </div>
        );
      })}
    </div>
  );
}
