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

const SLOT_ORDER = ['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'TE', 'FLEX', 'DST'];

function getSlottedColumns(slots) {
  const bySlot = { QB: [], RB: [], WR: [], TE: [], FLEX: [], DST: [] };
  slots.forEach(p => { if (bySlot[p.slot]) bySlot[p.slot].push(p); });
  return [bySlot.QB[0], bySlot.RB[0], bySlot.RB[1], bySlot.WR[0], bySlot.WR[1], bySlot.WR[2], bySlot.TE[0], bySlot.FLEX[0], bySlot.DST[0]];
}

/** This slate's own top cash-optimal (pure median, zero-variance) builds --
 * the same output _generate_cash_consensus_lineups() feeds into ownership
 * as a chalk signal, surfaced directly here so overlap across the set is
 * visible at a glance (see WORKLOG discussion: real-world cash consensus
 * is normally "solved" to 1-2 builds by Thursday, occasionally 3-5). */
export default function CashLineups() {
  const [weeks, setWeeks] = useState([]);
  const [selectedWeek, setSelectedWeek] = useState(1);
  const [lineups, setLineups] = useState([]);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    ApiService.getWeeks().then(data => setWeeks(data.weeks || [1])).catch(() => {});
  }, []);

  useEffect(() => {
    setLoading(true);
    ApiService.getWeekCashLineups(selectedWeek)
      .then(data => setLineups(data.lineups || []))
      .catch(() => setLineups([]))
      .finally(() => setLoading(false));
  }, [selectedWeek]);

  // How many of these builds each player appears in -- the overlap signal
  // itself, computed the same way the backend does for ownership.
  const appearanceCounts = useMemo(() => {
    const counts = {};
    lineups.forEach(lu => {
      lu.slots.forEach(p => {
        const key = `${p.name}_${p.team}`;
        counts[key] = (counts[key] || 0) + 1;
      });
    });
    return counts;
  }, [lineups]);

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
                      const key = `${p.name}_${p.team}`;
                      const count = appearanceCounts[key] || 0;
                      const isConsensus = count >= Math.max(2, Math.ceil(lineups.length * 0.6));
                      const displayName = p.pos === 'DST' ? `${p.team} DST` : p.name.split(' ').slice(-1)[0];
                      return (
                        <td key={i} style={{
                          padding: '5px 6px', whiteSpace: 'nowrap',
                          background: isConsensus ? 'rgba(34,197,94,0.08)' : 'transparent',
                          borderRadius: '4px',
                        }} title={`In ${count}/${lineups.length} of this slate's top cash builds`}>
                          <span style={{ display: 'inline-block', width: '6px', height: '6px', borderRadius: '50%', background: TEAM_COLORS[p.team] || '#888', marginRight: '4px', verticalAlign: 'middle' }} />
                          <span style={{ fontWeight: isConsensus ? 700 : 600, color: isConsensus ? '#22c55e' : 'var(--text-white)' }}>{displayName}</span>
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
