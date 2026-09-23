import { useState, useEffect, useMemo } from 'react';
import { ApiService } from '../api';

const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB: '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC: '#E31837',
  LV: '#000000', LAC: '#0080C6', LA: '#003594', MIA: '#008E97',
  MIN: '#4F2683', NE: '#002244', NO: '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SF: '#AA0000',
  SEA: '#002244', TB: '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414'
};

const POS_COLORS = { QB: '#ef4444', RB: '#22c55e', WR: '#3b82f6', TE: '#a855f7', FLEX: '#eab308', DST: '#f97316' };

const cardStyle = {
  background: 'rgba(11, 17, 38, 0.6)',
  border: '1px solid var(--border-glass)',
  borderRadius: '12px',
  padding: '16px',
};

const pillBtnBase = {
  padding: '4px 10px', borderRadius: '20px', fontSize: '0.76rem', fontWeight: 700,
  border: '1px solid rgba(255,255,255,0.08)', background: 'rgba(255,255,255,0.04)',
  color: 'var(--text-muted)', cursor: 'pointer',
};

const inputStyle = {
  background: 'rgba(0,0,0,0.3)', border: '1px solid var(--border-glass)', color: '#fff',
  borderRadius: '6px', padding: '5px 10px', fontSize: '0.82rem', outline: 'none',
};

const SLOT_ORDER = ['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'TE', 'FLEX', 'DST'];

function getSlottedColumns(slots) {
  const bySlot = { QB: [], RB: [], WR: [], TE: [], FLEX: [], DST: [] };
  slots.forEach(p => { if (bySlot[p.slot]) bySlot[p.slot].push(p); });
  return [bySlot.QB[0], bySlot.RB[0], bySlot.RB[1], bySlot.WR[0], bySlot.WR[1], bySlot.WR[2], bySlot.TE[0], bySlot.FLEX[0], bySlot.DST[0]];
}

const poolKey = (name, team) => `${name}_${team}`;

/** This slate's own top cash-optimal (pure median, zero-variance) builds --
 * the same output _generate_cash_consensus_lineups() feeds into ownership
 * as a chalk signal, surfaced directly here so overlap across the set is
 * visible at a glance (see WORKLOG discussion: real-world cash consensus
 * is normally "solved" to 1-2 builds by Thursday, occasionally 3-5).
 *
 * Player pool: the solver treats every priced player as a fair cash play,
 * which isn't true (a rookie/backup can look value-efficient on pure salary
 * math but not be a real-world cash play). Laid out like the Optimizer's
 * pool table (search, position filter, sortable columns, lock/exclude per
 * row) but deliberately without anything tournament-specific -- no
 * ownership, ceiling/GPP blend, exposure caps, or contest settings, since
 * cash is just "most median points, zero variance" and nothing else. Rerun
 * re-solves with the current lock/exclude picks and saves them server-side
 * (cash_pool_store) so they're still applied next time this week loads. */
export default function CashLineups() {
  const [weeks, setWeeks] = useState([]);
  const [selectedWeek, setSelectedWeek] = useState(1);
  const [lineups, setLineups] = useState([]);
  const [loading, setLoading] = useState(false);

  const [allPlayers, setAllPlayers] = useState([]);
  const [excluded, setExcluded] = useState([]);
  const [locked, setLocked] = useState([]);
  // What the lineups table currently reflects (i.e. what was last actually
  // sent to the backend) -- lets a toggle-then-forget-to-Rerun state show up
  // as "pending" instead of silently going stale.
  const [appliedExcluded, setAppliedExcluded] = useState([]);
  const [appliedLocked, setAppliedLocked] = useState([]);
  const [regenerating, setRegenerating] = useState(false);
  const [regenError, setRegenError] = useState('');

  const [poolOpen, setPoolOpen] = useState(true);
  const [playerSearch, setPlayerSearch] = useState('');
  const [posFilter, setPosFilter] = useState('ALL');
  const [sortField, setSortField] = useState('median');
  const [sortAsc, setSortAsc] = useState(false);

  useEffect(() => {
    ApiService.getWeeks().then(data => setWeeks(data.weeks || [1])).catch(() => {});
  }, []);

  useEffect(() => {
    setLoading(true);
    setRegenError('');
    Promise.all([
      ApiService.getWeekCashLineups(selectedWeek),
      ApiService.getWeekProjections(selectedWeek),
    ])
      .then(([cashData, projData]) => {
        setLineups(cashData.lineups || []);
        setExcluded(cashData.excluded || []);
        setLocked(cashData.locked || []);
        setAppliedExcluded(cashData.excluded || []);
        setAppliedLocked(cashData.locked || []);
        // Main-slate, priced players only -- an off-slate/unpriced player
        // can never actually land in a real DK lineup here anyway (mirrors
        // the Optimizer's own slate-restriction, see buildPlayerPool there).
        const priced = (projData.players || []).filter(p => p.is_main && p.salary != null);
        setAllPlayers(priced);
      })
      .catch(() => {
        setLineups([]);
        setExcluded([]);
        setLocked([]);
        setAppliedExcluded([]);
        setAppliedLocked([]);
        setAllPlayers([]);
      })
      .finally(() => setLoading(false));
  }, [selectedWeek]);

  // How many of these builds each player appears in -- the overlap signal
  // itself, computed the same way the backend does for ownership.
  const appearanceCounts = useMemo(() => {
    const counts = {};
    lineups.forEach(lu => {
      lu.slots.forEach(p => {
        const key = poolKey(p.name, p.team);
        counts[key] = (counts[key] || 0) + 1;
      });
    });
    return counts;
  }, [lineups]);

  const excludedKeys = useMemo(() => new Set(excluded.map(e => poolKey(e.name, e.team))), [excluded]);
  const lockedKeys = useMemo(() => new Set(locked.map(e => poolKey(e.name, e.team))), [locked]);
  const hasPendingChanges = useMemo(() => {
    const sameSet = (a, b) => a.size === b.size && [...a].every(k => b.has(k));
    const appliedExcludedKeys = new Set(appliedExcluded.map(e => poolKey(e.name, e.team)));
    const appliedLockedKeys = new Set(appliedLocked.map(e => poolKey(e.name, e.team)));
    return !sameSet(excludedKeys, appliedExcludedKeys) || !sameSet(lockedKeys, appliedLockedKeys);
  }, [excludedKeys, lockedKeys, appliedExcluded, appliedLocked]);

  const toggleExclude = (name, team) => {
    const key = poolKey(name, team);
    setExcluded(prev => (
      excludedKeys.has(key) ? prev.filter(e => poolKey(e.name, e.team) !== key) : [...prev, { name, team }]
    ));
    // Excluding and locking the same player at once is a contradiction --
    // excluding always wins and clears any lock.
    if (lockedKeys.has(key)) setLocked(prev => prev.filter(e => poolKey(e.name, e.team) !== key));
  };
  const toggleLock = (name, team) => {
    const key = poolKey(name, team);
    setLocked(prev => (
      lockedKeys.has(key) ? prev.filter(e => poolKey(e.name, e.team) !== key) : [...prev, { name, team }]
    ));
    if (excludedKeys.has(key)) setExcluded(prev => prev.filter(e => poolKey(e.name, e.team) !== key));
  };

  const handleRerun = async () => {
    setRegenerating(true);
    setRegenError('');
    try {
      const data = await ApiService.regenerateCashLineups(selectedWeek, { excluded, locked });
      setLineups(data.lineups || []);
      setAppliedExcluded(data.excluded || excluded);
      setAppliedLocked(data.locked || locked);
    } catch (err) {
      setRegenError(err.message || 'Rerun failed');
    } finally {
      setRegenerating(false);
    }
  };

  const handleSort = (field) => {
    if (sortField === field) {
      setSortAsc(a => !a);
    } else {
      setSortField(field);
      setSortAsc(field === 'name' || field === 'pos' || field === 'team');
    }
  };

  const visiblePool = useMemo(() => {
    const q = playerSearch.trim().toLowerCase();
    let rows = allPlayers;
    if (posFilter !== 'ALL') rows = rows.filter(p => p.pos === posFilter);
    if (q) rows = rows.filter(p => p.name && p.name.toLowerCase().includes(q));
    const sorted = [...rows].sort((a, b) => {
      let av, bv;
      if (sortField === 'median') { av = a.dk_score ?? -Infinity; bv = b.dk_score ?? -Infinity; }
      else if (sortField === 'salary') { av = a.salary ?? -Infinity; bv = b.salary ?? -Infinity; }
      else { av = (a[sortField] || '').toString(); bv = (b[sortField] || '').toString(); }
      if (av < bv) return sortAsc ? -1 : 1;
      if (av > bv) return sortAsc ? 1 : -1;
      return 0;
    });
    return sorted;
  }, [allPlayers, posFilter, playerSearch, sortField, sortAsc]);

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: '16px', flexWrap: 'wrap', gap: '10px' }}>
        <div>
          <h1 style={{ marginBottom: '2px', fontSize: '1.6rem' }}>💰 Cash Lineups</h1>
          <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', margin: 0 }}>
            This slate's own top {lineups.length || 10} cash-optimal builds — pure median projection, zero variance, the same objective the Optimizer uses for contest_type "Cash". A player highlighted across most/all of these is the industry's likely "solved" cash chalk by Thursday.
          </p>
        </div>
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <span style={{ fontWeight: 600, fontSize: '0.85rem', color: 'var(--text-white)' }}>Week:</span>
          <select
            value={selectedWeek}
            onChange={e => setSelectedWeek(parseInt(e.target.value))}
            style={{
              background: 'rgba(11, 17, 38, 0.6)', border: '1px solid var(--border-glass)',
              color: 'var(--text-white)', borderRadius: '8px', padding: '6px 12px', fontWeight: 600, cursor: 'pointer',
            }}
          >
            {(weeks.length > 0 ? weeks : Array.from({ length: 18 }, (_, i) => i + 1)).map(w => (
              <option key={w} value={w}>{w}</option>
            ))}
          </select>
        </div>
      </div>

      {/* Player pool -- lock/exclude anyone the solver would otherwise treat
          purely on salary/value math, same interaction as the Optimizer's
          pool minus everything tournament-specific. */}
      <div style={{ ...cardStyle, marginBottom: '16px' }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '10px', marginBottom: poolOpen ? '12px' : 0 }}>
          <button
            onClick={() => setPoolOpen(o => !o)}
            style={{
              background: 'transparent', border: 'none', color: 'var(--text-white)',
              fontWeight: 700, fontSize: '0.95rem', cursor: 'pointer', padding: 0,
              display: 'flex', alignItems: 'center', gap: '6px',
            }}
          >
            <span style={{ transform: poolOpen ? 'rotate(90deg)' : 'none', transition: 'transform 0.15s', display: 'inline-block' }}>▶</span>
            Player Pool
            {locked.length > 0 && (
              <span style={{ fontSize: '0.72rem', fontWeight: 700, color: '#eab308', background: 'rgba(234,179,8,0.12)', borderRadius: '10px', padding: '2px 8px' }}>
                {locked.length} locked
              </span>
            )}
            {excluded.length > 0 && (
              <span style={{ fontSize: '0.72rem', fontWeight: 700, color: '#ef4444', background: 'rgba(239,68,68,0.12)', borderRadius: '10px', padding: '2px 8px' }}>
                {excluded.length} excluded
              </span>
            )}
          </button>
          <div style={{ display: 'flex', alignItems: 'center', gap: '10px', flexWrap: 'wrap' }}>
            {hasPendingChanges && (
              <span style={{ fontSize: '0.78rem', color: '#eab308', fontWeight: 600 }}>Pool changed — rerun to apply</span>
            )}
            {regenError && (
              <span style={{ fontSize: '0.78rem', color: '#ef4444', fontWeight: 600 }}>{regenError}</span>
            )}
            <button
              onClick={handleRerun}
              disabled={regenerating || loading}
              style={{
                background: hasPendingChanges ? 'linear-gradient(135deg, #06b6d4, #9d4edd)' : 'rgba(255,255,255,0.06)',
                border: '1px solid var(--border-glass)', color: '#fff', fontWeight: 700,
                fontSize: '0.82rem', padding: '7px 14px', borderRadius: '8px',
                cursor: regenerating || loading ? 'default' : 'pointer',
                opacity: regenerating || loading ? 0.7 : 1, whiteSpace: 'nowrap',
              }}
            >
              {regenerating ? 'Rerunning…' : '🔄 Rerun Cash Lineups'}
            </button>
          </div>
        </div>

        {poolOpen && (
          <>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '8px', marginBottom: '10px' }}>
              <div style={{ display: 'flex', gap: '4px' }}>
                {['ALL', 'QB', 'RB', 'WR', 'TE', 'DST'].map(pos => (
                  <button key={pos} onClick={() => setPosFilter(pos)} style={{
                    ...pillBtnBase,
                    background: posFilter === pos ? (POS_COLORS[pos] ? POS_COLORS[pos] + '22' : 'rgba(0,242,254,0.15)') : 'rgba(255,255,255,0.04)',
                    color: posFilter === pos ? (POS_COLORS[pos] || 'var(--accent-primary)') : 'var(--text-muted)',
                    borderColor: posFilter === pos ? (POS_COLORS[pos] || 'rgba(0,242,254,0.4)') + '88' : 'rgba(255,255,255,0.08)',
                  }}>{pos}</button>
                ))}
              </div>
              <input
                type="text" placeholder="Search player…"
                value={playerSearch} onChange={e => setPlayerSearch(e.target.value)}
                style={{ ...inputStyle, width: '180px' }}
              />
            </div>

            <div style={{ maxHeight: '50vh', overflowY: 'auto', border: '1px solid rgba(255,255,255,0.05)', borderRadius: '8px' }}>
              <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.8rem' }}>
                <thead>
                  <tr style={{ position: 'sticky', top: 0, background: '#0c122b', zIndex: 1 }}>
                    <th style={{ width: '28px', textAlign: 'center', padding: '7px 4px', userSelect: 'none' }}>🔒</th>
                    <th style={{ width: '28px', textAlign: 'center', padding: '7px 4px', userSelect: 'none' }}>✕</th>
                    {[
                      ['name', 'Player'], ['pos', 'Pos'], ['team', 'Team'], ['salary', 'Salary'], ['median', 'Median'],
                    ].map(([field, label]) => (
                      <th key={field} style={{ cursor: 'pointer', padding: '7px 8px', whiteSpace: 'nowrap' }} onClick={() => handleSort(field)}>
                        {label} {sortField === field ? (sortAsc ? '↑' : '↓') : ''}
                      </th>
                    ))}
                    <th style={{ padding: '7px 8px', fontSize: '0.7rem', color: 'var(--text-muted)' }}>Opp</th>
                  </tr>
                </thead>
                <tbody>
                  {visiblePool.length === 0 ? (
                    <tr><td colSpan={7} style={{ textAlign: 'center', padding: '30px', color: 'var(--text-muted)' }}>No players match your filters.</td></tr>
                  ) : visiblePool.map(p => {
                    const key = poolKey(p.name, p.team);
                    const isExcluded = excludedKeys.has(key);
                    const isLocked = lockedKeys.has(key);
                    return (
                      <tr key={key} style={{
                        opacity: isExcluded ? 0.35 : 1,
                        background: isLocked ? 'rgba(234,179,8,0.06)' : 'transparent',
                        borderBottom: '1px solid rgba(255,255,255,0.04)',
                        transition: 'opacity 0.15s',
                      }}>
                        <td style={{ textAlign: 'center', padding: '5px 4px' }}>
                          <button onClick={() => toggleLock(p.name, p.team)} title={isLocked ? 'Unlock' : 'Lock into every build'} style={{
                            background: isLocked ? 'rgba(234,179,8,0.2)' : 'rgba(255,255,255,0.04)',
                            border: `1px solid ${isLocked ? 'rgba(234,179,8,0.5)' : 'rgba(255,255,255,0.08)'}`,
                            color: isLocked ? '#eab308' : 'var(--text-muted)',
                            borderRadius: '4px', padding: '2px 5px', cursor: 'pointer', fontSize: '0.68rem',
                          }}>🔒</button>
                        </td>
                        <td style={{ textAlign: 'center', padding: '5px 4px' }}>
                          <button onClick={() => toggleExclude(p.name, p.team)} title={isExcluded ? 'Re-include' : 'Exclude from pool'} style={{
                            background: isExcluded ? 'rgba(239,68,68,0.15)' : 'rgba(255,255,255,0.04)',
                            border: `1px solid ${isExcluded ? 'rgba(239,68,68,0.4)' : 'rgba(255,255,255,0.08)'}`,
                            color: isExcluded ? '#ef4444' : 'var(--text-muted)',
                            borderRadius: '4px', padding: '2px 5px', cursor: 'pointer', fontSize: '0.68rem',
                          }}>✕</button>
                        </td>
                        <td style={{ fontWeight: 600, padding: '5px 8px', whiteSpace: 'nowrap' }}>
                          <span style={{ display: 'inline-block', width: '7px', height: '7px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '6px', verticalAlign: 'middle' }} />
                          {p.pos === 'DST' ? `${p.team} DST` : p.name}
                        </td>
                        <td style={{ padding: '5px 6px' }}>
                          <span style={{
                            padding: '2px 5px', borderRadius: '4px', fontSize: '0.68rem', fontWeight: 700,
                            background: (POS_COLORS[p.pos] || '#888') + '22',
                            color: POS_COLORS[p.pos] || '#888',
                            border: `1px solid ${POS_COLORS[p.pos] || '#888'}44`,
                          }}>{p.pos}</span>
                        </td>
                        <td style={{ padding: '5px 6px', fontWeight: 600 }}>{p.team}</td>
                        <td style={{ padding: '5px 6px', fontWeight: 600 }}>{p.salary == null ? '—' : `$${p.salary.toLocaleString()}`}</td>
                        <td style={{ padding: '5px 6px', fontWeight: 700, color: 'var(--accent-primary)' }}>{p.dk_score ?? '—'}</td>
                        <td style={{ padding: '5px 6px', fontSize: '0.75rem', color: 'var(--text-muted)', whiteSpace: 'nowrap' }}>{p.opponent}</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </>
        )}
      </div>

      {loading ? (
        <div style={{ ...cardStyle, textAlign: 'center', padding: '40px', color: 'var(--text-muted)' }}>
          Generating cash-optimal builds… (first load of the week can take a bit — 10 ILP solves)
        </div>
      ) : lineups.length === 0 ? (
        <div style={{ ...cardStyle, textAlign: 'center', padding: '40px', color: 'var(--text-muted)' }}>
          No cash lineups available for this week yet.
        </div>
      ) : (
        <div style={{ ...cardStyle, overflowX: 'auto' }}>
          <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.8rem' }}>
            <thead>
              <tr style={{ borderBottom: '1px solid var(--border-glass)' }}>
                <th style={{ padding: '7px 6px', fontSize: '0.7rem', color: 'var(--text-muted)' }}>#</th>
                {SLOT_ORDER.map((slot, i) => (
                  <th key={`${slot}${i}`} style={{ padding: '7px 6px', fontSize: '0.68rem', color: POS_COLORS[slot] || 'var(--text-muted)' }}>{slot}</th>
                ))}
                <th style={{ padding: '7px 6px', fontSize: '0.7rem', color: 'var(--text-muted)' }}>Median</th>
                <th style={{ padding: '7px 6px', fontSize: '0.7rem', color: 'var(--text-muted)' }}>Salary</th>
              </tr>
            </thead>
            <tbody>
              {lineups.map((lu, idx) => {
                const cols = getSlottedColumns(lu.slots);
                return (
                  <tr key={idx} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                    <td style={{ padding: '6px 6px', fontWeight: 700, color: 'var(--text-muted)' }}>{idx + 1}</td>
                    {cols.map((p, i) => {
                      if (!p) return <td key={i} style={{ padding: '5px 6px', color: 'rgba(255,255,255,0.2)' }}>—</td>;
                      const key = poolKey(p.name, p.team);
                      const count = appearanceCounts[key] || 0;
                      const isConsensus = count >= Math.max(2, Math.ceil(lineups.length * 0.6));
                      const isLocked = lockedKeys.has(key);
                      const displayName = p.pos === 'DST' ? `${p.team} DST` : p.name.split(' ').slice(-1)[0];
                      return (
                        <td key={i} style={{
                          padding: '5px 6px', whiteSpace: 'nowrap',
                          background: isConsensus ? 'rgba(34,197,94,0.08)' : 'transparent',
                          borderRadius: '4px',
                        }} title={`In ${count}/${lineups.length} of this slate's top cash builds`}>
                          <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '4px', verticalAlign: 'middle' }} />
                          <span style={{ fontWeight: isConsensus ? 700 : 600, color: isConsensus ? '#22c55e' : 'var(--text-white)' }}>{displayName}</span>
                          {isLocked && <span title="Locked" style={{ marginLeft: '3px', fontSize: '0.65rem' }}>🔒</span>}
                        </td>
                      );
                    })}
                    <td style={{ padding: '6px 6px', color: 'var(--accent-primary)', fontWeight: 600 }}>{lu.projected_score}</td>
                    <td style={{ padding: '6px 6px', color: 'var(--text-muted)' }}>${lu.total_salary?.toLocaleString()}</td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}
