import { useState, useMemo } from 'react';

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

const POS_COLORS = { QB: '#ef4444', RB: '#22c55e', WR: '#3b82f6', TE: '#a855f7', DST: '#f97316' };

const cardStyle = {
  background: 'rgba(11, 17, 38, 0.6)',
  border: '1px solid var(--border-glass)',
  borderRadius: '12px',
  padding: '16px',
};

const inputStyle = {
  background: 'rgba(11, 17, 38, 0.6)',
  border: '1px solid var(--border-glass)',
  color: 'var(--text-white)',
  borderRadius: '8px',
  padding: '6px 10px',
  fontSize: '0.82rem',
};

const pillBtnBase = {
  border: '1px solid transparent',
  borderRadius: '8px',
  fontWeight: 600,
  fontSize: '0.8rem',
  transition: 'all 0.15s',
};

/** Every player this week's median/ceiling projection, optimal-lineup rate
 * (traditional multi-game slate, from /api/week_projections -- NOT the
 * 2-team showdown-scoped optimal_pct in /api/week_sim_results), and
 * projected ownership (from allSimResults, the only place it's computed --
 * see get_week_sim_results()'s slate-wide _compute_ownership() pass).
 * Leverage = optimal% - own% (this page's whole point). */
export default function Leverage({ weekProjections, allSimResults }) {
  const [platform, setPlatform] = useState('DK');
  const [posFilter, setPosFilter] = useState('ALL');
  const [search, setSearch] = useState('');
  const [sortField, setSortField] = useState('leverage');
  const [sortAsc, setSortAsc] = useState(false);

  // Ownership only lives in allSimResults' per-game projections today --
  // flatten once into a (name, team) -> ownership_proj lookup.
  const ownershipByKey = useMemo(() => {
    const map = {};
    Object.values(allSimResults || {}).forEach(g => {
      (g?.projections || []).forEach(p => {
        map[`${p.name}_${p.team}`] = p.ownership_proj;
      });
    });
    return map;
  }, [allSimResults]);

  const rows = useMemo(() => {
    const wpList = Array.isArray(weekProjections) ? weekProjections : (weekProjections?.players || []);
    return wpList
      .filter(p => p.name && p.pos && p.is_main !== false) // Main Slate only, same as the Optimizer's player pool
      .map(p => {
        const isFD = platform === 'FD';
        const median = isFD ? (p.fd_p50 ?? p.fd_score ?? 0) : (p.dk_p50 ?? p.dk_score ?? 0);
        const ceiling = isFD ? (p.fd_p95 ?? median) : (p.dk_p95 ?? median);
        const optimalPct = p.optimal_pct ?? 0;
        const ownPct = ownershipByKey[`${p.name}_${p.team}`];
        const leverage = ownPct != null ? parseFloat((optimalPct - ownPct).toFixed(1)) : null;
        const pos = (p.pos || '').replace(/\d/g, '').toUpperCase();
        return {
          id: `${p.name}_${p.team}`,
          name: pos === 'DST' ? `${p.team} DST` : p.name, pos, team: p.team,
          median: parseFloat(median.toFixed(1)),
          ceiling: parseFloat(ceiling.toFixed(1)),
          optimalPct,
          ownPct: ownPct != null ? ownPct : null,
          leverage,
        };
      })
      .filter(p => ['QB', 'RB', 'WR', 'TE', 'DST'].includes(p.pos));
  }, [weekProjections, platform, ownershipByKey]);

  const filtered = useMemo(() => {
    let list = rows;
    if (posFilter !== 'ALL') list = list.filter(p => p.pos === posFilter);
    if (search.trim()) {
      const q = search.trim().toLowerCase();
      list = list.filter(p => p.name.toLowerCase().includes(q));
    }
    const sorted = [...list].sort((a, b) => {
      const av = a[sortField] ?? -Infinity;
      const bv = b[sortField] ?? -Infinity;
      return sortAsc ? av - bv : bv - av;
    });
    return sorted;
  }, [rows, posFilter, search, sortField, sortAsc]);

  const handleSort = (field) => {
    if (sortField === field) setSortAsc(!sortAsc);
    else { setSortField(field); setSortAsc(false); }
  };

  const thStyle = (field) => ({
    padding: '8px 10px', fontSize: '0.72rem', textTransform: 'uppercase', letterSpacing: '0.04em',
    color: sortField === field ? 'var(--accent-primary)' : 'var(--text-muted)',
    cursor: 'pointer', whiteSpace: 'nowrap', userSelect: 'none',
  });

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      <div style={{ marginBottom: '16px' }}>
        <h1 style={{ marginBottom: '2px', fontSize: '1.6rem' }}>🎯 Ownership Leverage</h1>
        <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', margin: 0 }}>
          Leverage = optimal-lineup rate − projected ownership. Positive means the field is likely under-rostering a player relative to how often they show up in the slate's own optimal builds; negative means they're probably overowned relative to their true rate.
        </p>
      </div>

      <div style={{ ...cardStyle, display: 'flex', flexWrap: 'wrap', gap: '12px', alignItems: 'center', marginBottom: '14px' }}>
        <div style={{ display: 'flex', gap: '6px' }}>
          {['DK', 'FD'].map(plat => (
            <button key={plat} onClick={() => setPlatform(plat)} style={{
              ...pillBtnBase, padding: '6px 14px', cursor: 'pointer',
              background: platform === plat ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.04)',
              color: platform === plat ? 'var(--accent-primary)' : 'var(--text-white)',
              borderColor: platform === plat ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.1)',
            }}>{plat}</button>
          ))}
        </div>
        <div style={{ display: 'flex', gap: '4px' }}>
          {['ALL', 'QB', 'RB', 'WR', 'TE', 'DST'].map(pos => (
            <button key={pos} onClick={() => setPosFilter(pos)} style={{
              ...pillBtnBase, padding: '6px 12px', cursor: 'pointer',
              background: posFilter === pos ? 'rgba(0,242,254,0.15)' : 'rgba(255,255,255,0.04)',
              color: posFilter === pos ? 'var(--accent-primary)' : 'var(--text-white)',
              borderColor: posFilter === pos ? 'rgba(0,242,254,0.4)' : 'rgba(255,255,255,0.1)',
            }}>{pos}</button>
          ))}
        </div>
        <input
          type="text" placeholder="Search player…" value={search}
          onChange={e => setSearch(e.target.value)}
          style={{ ...inputStyle, flex: 1, minWidth: '160px' }}
        />
        <div style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>{filtered.length} players</div>
      </div>

      <div style={{ ...cardStyle, overflowX: 'auto' }}>
        <table style={{ width: '100%', borderCollapse: 'collapse', fontSize: '0.85rem' }}>
          <thead>
            <tr style={{ borderBottom: '1px solid var(--border-glass)' }}>
              <th style={thStyle('name')} onClick={() => handleSort('name')}>Player</th>
              <th style={thStyle('pos')} onClick={() => handleSort('pos')}>Pos</th>
              <th style={thStyle('team')} onClick={() => handleSort('team')}>Team</th>
              <th style={thStyle('median')} onClick={() => handleSort('median')}>Median</th>
              <th style={thStyle('ceiling')} onClick={() => handleSort('ceiling')}>Ceiling</th>
              <th style={thStyle('optimalPct')} onClick={() => handleSort('optimalPct')}>Optimal%</th>
              <th style={thStyle('ownPct')} onClick={() => handleSort('ownPct')}>Own%</th>
              <th style={thStyle('leverage')} onClick={() => handleSort('leverage')}>Leverage</th>
            </tr>
          </thead>
          <tbody>
            {filtered.map(p => (
              <tr key={p.id} style={{ borderBottom: '1px solid rgba(255,255,255,0.04)' }}>
                <td style={{ padding: '6px 10px', fontWeight: 600 }}>
                  <span style={{ display: 'inline-block', width: '7px', height: '7px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '6px' }} />
                  {p.name}
                </td>
                <td style={{ padding: '6px 10px' }}>
                  <span style={{ color: POS_COLORS[p.pos] || 'var(--text-muted)', fontWeight: 700, fontSize: '0.75rem' }}>{p.pos}</span>
                </td>
                <td style={{ padding: '6px 10px', color: 'var(--text-muted)' }}>{p.team}</td>
                <td style={{ padding: '6px 10px' }}>{p.median}</td>
                <td style={{ padding: '6px 10px', color: 'var(--accent-primary)' }}>{p.ceiling}</td>
                <td style={{ padding: '6px 10px' }}>{p.optimalPct}%</td>
                <td style={{ padding: '6px 10px' }}>{p.ownPct != null ? `${p.ownPct}%` : '—'}</td>
                <td style={{
                  padding: '6px 10px', fontWeight: 700,
                  color: p.leverage == null ? 'var(--text-muted)' : p.leverage > 0 ? '#22c55e' : p.leverage < 0 ? '#ef4444' : 'var(--text-white)',
                }}>
                  {p.leverage != null ? (p.leverage > 0 ? `+${p.leverage}` : p.leverage) : '—'}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
