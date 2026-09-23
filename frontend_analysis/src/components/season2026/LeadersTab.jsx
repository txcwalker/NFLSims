import { useState, useMemo } from 'react';
import { useSortableData, SortableTh } from './shared';

const QB_COLS = [
  ['Player', 'Player'], ['Team', 'Team'], ['pAtt_avg', 'Att'], ['pCmp_avg', 'Cmp'], ['cmp_pct', 'Cmp %'], ['pYds_avg', 'Pass Yds'],
  ['pTD_avg', 'Pass TD'], ['rYds_avg', 'Rush Yds'], ['rTD_avg', 'Rush TD'], ['totalTD', 'Total TD'], ['fumbles_avg', 'Fum'],
  ['sacks_taken_avg', 'Sacks'], ['int_avg', 'INT'], ['air_yards_avg', 'Air Yds'], ['adot', 'ADOT'],
  ['std_score_avg', 'Std'],
];
const WR_TE_COLS = [
  ['Player', 'Player'], ['Team', 'Team'], ['Slot', 'Slot'], ['rAtt_avg', 'Rush Att'], ['rYds_avg', 'Rush Yds'],
  ['rTD_avg', 'Rush TD'], ['targets_avg', 'Targets'], ['rec_avg', 'Rec'], ['recYds_avg', 'Rec Yds'],
  ['recTD_avg', 'Rec TD'], ['totalTD', 'Total TD'], ['fumbles_avg', 'Fum'], ['air_yards_avg', 'Air Yds'], ['adot', 'ADOT'],
  ['std_score_avg', 'Std'],
];
const RB_COLS = WR_TE_COLS.filter(([key]) => key !== 'air_yards_avg' && key !== 'adot');

function LeadersTable({ rows, cols, defaultSort }) {
  const { sorted, sortKey, sortDir, toggleSort } = useSortableData(rows, defaultSort);
  if (!rows || rows.length === 0) return <div style={{ padding: '20px', color: 'var(--text-muted)' }}>No players in this category.</div>;
  return (
    <div style={{ overflowX: 'auto' }}>
      <table className="tactical-table">
        <thead><tr>{cols.map(([key, label]) => <SortableTh key={key} label={label} sortKeyName={key} activeKey={sortKey} dir={sortDir} onClick={toggleSort} />)}</tr></thead>
        <tbody>
          {sorted.map((r, i) => (
            <tr key={r.Player + r.Team + i}>
              {cols.map(([key]) => (
                <td key={key} style={{ fontWeight: key === 'Player' ? 600 : 400 }}>
                  {typeof r[key] === 'number' ? r[key].toFixed(1) : r[key]}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function LeadersTab({ leaders }) {
  const [pos, setPos] = useState('qb');
  const [scope, setScope] = useState('overall');
  const positions = [['qb', 'QB'], ['rb', 'RB'], ['wr', 'WR'], ['te', 'TE']];
  const rawData = leaders?.[scope === 'overall' ? 'overall' : 'rookies']?.[pos] || [];
  const data = useMemo(() => rawData.map(r => ({
    ...r,
    totalTD: pos === 'qb' ? (r.pTD_avg || 0) + (r.rTD_avg || 0) : (r.rTD_avg || 0) + (r.recTD_avg || 0),
  })), [rawData, pos]);
  const cols = pos === 'qb' ? QB_COLS : (pos === 'rb' ? RB_COLS : WR_TE_COLS);
  const defaultSort = pos === 'qb' ? 'pYds_avg' : (pos === 'rb' ? 'rYds_avg' : 'recYds_avg');

  return (
    <div>
      <div style={{ display: 'flex', gap: '12px', marginBottom: '16px', flexWrap: 'wrap' }}>
        <div className="tabs-container" style={{ marginBottom: 0 }}>
          {positions.map(([id, label]) => (
            <button key={id} className={`tab-btn ${pos === id ? 'active' : ''}`} onClick={() => setPos(id)}>{label}</button>
          ))}
        </div>
        <div className="tabs-container" style={{ marginBottom: 0 }}>
          <button className={`tab-btn ${scope === 'overall' ? 'active' : ''}`} onClick={() => setScope('overall')}>Overall</button>
          <button className={`tab-btn ${scope === 'rookies' ? 'active' : ''}`} onClick={() => setScope('rookies')}>Rookies</button>
        </div>
      </div>
      <LeadersTable rows={data} cols={cols} defaultSort={defaultSort} />
    </div>
  );
}

export default LeadersTab;
