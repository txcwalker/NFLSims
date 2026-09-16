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
};

function money(v) {
  if (v == null) return '—';
  const sign = v < 0 ? '-' : '';
  return `${sign}$${Math.abs(v).toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
}
function pct(v) { return v == null ? '—' : `${v > 0 ? '+' : ''}${v.toFixed(1)}%`; }

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

  const reload = () => {
    setLoading(true);
    ApiService.getBankroll().then(rows => { setAccounts(rows); setLoading(false); })
      .catch(() => { setError('Failed to load bankroll data'); setLoading(false); });
  };
  useEffect(reload, []);

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

  const totals = accounts.reduce((acc, a) => ({
    cost_total: acc.cost_total + (a.cost_total || 0),
    cost_pending: acc.cost_pending + (a.cost_pending || 0),
    winnings_settled: acc.winnings_settled + (a.winnings_settled || 0),
    net_pnl_settled: acc.net_pnl_settled + (a.net_pnl_settled || 0),
  }), { cost_total: 0, cost_pending: 0, winnings_settled: 0, net_pnl_settled: 0 });

  if (viewingAccount) {
    return <AccountDetail account={viewingAccount} onBack={() => setViewingAccount(null)} />;
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '16px' }}>
      <div>
        <h1 style={{ fontSize: '1.4rem', margin: 0 }}>🏦 Bankroll</h1>
        <div style={{ color: 'var(--text-muted)', fontSize: '0.85rem', marginTop: '4px' }}>
          Every account rolls up the Builds tagged to it (Optimizer → Builds panel → Account column).
          Cost is charged the moment a build is tagged; winnings only count once
          real results are settled (drop a standings CSV in and run
          score_paper_entries.py) — until then an entry shows as pending.
        </div>
      </div>

      {loading && <div style={{ color: 'var(--text-muted)' }}>Loading…</div>}
      {error && <div style={{ color: '#ef4444' }}>{error}</div>}

      {!loading && !error && (
        <>
          <div style={{ ...cardStyle, display: 'flex', gap: '24px', flexWrap: 'wrap' }}>
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

/** Drill-down for one account: every build's lineups, contest info, settled
 * real result once available, and an editable notes/late-swap annotation
 * per lineup (see paper_store.update_entry). */
function AccountDetail({ account, onBack }) {
  const [builds, setBuilds] = useState([]);
  const [loading, setLoading] = useState(true);
  const [openBuildId, setOpenBuildId] = useState(null);
  const [openPlayersId, setOpenPlayersId] = useState(null);

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

      {builds.map(b => {
        const isOpen = openBuildId === b.build_id;
        const nSettled = b.entries.filter(e => e.settled).length;
        return (
          <div key={b.build_id} style={cardStyle}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', cursor: 'pointer' }}
              onClick={() => setOpenBuildId(isOpen ? null : b.build_id)}>
              <div>
                <span style={{ fontWeight: 600 }}>{isOpen ? '▾' : '▸'} {b.label || b.build_id}</span>
                <span style={{ marginLeft: '10px', fontSize: '0.75rem', color: 'var(--text-muted)' }}>
                  Wk {b.week} · {b.contest_name} · {money(b.entry_fee)}/entry · {b.entries.length} lineups · {nSettled} settled
                </span>
              </div>
            </div>

            {isOpen && (
              <div style={{ overflowX: 'auto', marginTop: '10px' }}>
                <table style={{ width: '100%', fontSize: '0.74rem', borderCollapse: 'collapse' }}>
                  <thead>
                    <tr style={{ color: 'var(--text-muted)', textAlign: 'right' }}>
                      <th style={{ textAlign: 'left', padding: '3px 4px' }}></th>
                      <th style={{ padding: '3px 4px' }}>Predicted</th>
                      <th style={{ padding: '3px 4px' }}>Actual</th>
                      <th style={{ padding: '3px 4px' }}>Rank</th>
                      <th style={{ padding: '3px 4px' }}>Finish %ile</th>
                      <th style={{ padding: '3px 4px' }}>Payout</th>
                      <th style={{ textAlign: 'left', padding: '3px 4px' }}>Late swap</th>
                      <th style={{ textAlign: 'left', padding: '3px 4px' }}>Notes</th>
                    </tr>
                  </thead>
                  <tbody>
                    {b.entries.map((e, i) => (
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
                        </tr>
                        {openPlayersId === e.entry_id && (
                          <tr>
                            <td colSpan={8} style={{ padding: '4px 4px 8px 24px', color: 'var(--text-secondary)' }}>
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
