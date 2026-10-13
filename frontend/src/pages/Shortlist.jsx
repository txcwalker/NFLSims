// Shortlist (Classic): the lineups hand-saved from the DFS Optimizer during the
// week, cut down to your final entries with the Optimizer's own analysis tools
// (ClassicLineupAnalysis -- exposure panel, drill-down, FLEX split, lineup table) and the
// same DK upload export.
//
// Inputs   weekProjections / allSimResults / games : same App-level props the Optimizer gets,
//          used only to rebuild the player pool so ownership shown here is LIVE, not the value
//          frozen into each lineup when it was saved.
//          selectedWeek : the week whose shortlist is loaded.
// Outputs  edits the shortlist through /api/shortlist (add happens in the Optimizer;
//          remove / bulk-remove / clear happen here); downloads DK upload / summary CSVs.
// Ownership precedence = the Optimizer's: manual override from its saved overlay > live weekly sim.
import { useState, useEffect, useMemo } from 'react';
import { ApiService } from '../api';
import ClassicLineupAnalysis from '../components/ClassicLineupAnalysis';
import { buildPlayerPool, exportSummaryCSV, exportDkUploadCSV, cardStyle, pillBtnBase } from '../optimizerShared';

const btn = (accent) => ({
  padding: '7px 14px', background: `${accent}1a`, color: accent,
  border: `1px solid ${accent}4d`, borderRadius: '8px', cursor: 'pointer',
  fontWeight: 600, fontSize: '0.82rem',
});

export default function Shortlist({ allSimResults = {}, weekProjections = [], games = [], weeks = [], selectedWeek = 1, setSelectedWeek = () => {} }) {
  const [entries, setEntries] = useState([]);        // [{entry_id, saved_at, source_build_id, lineup}]
  const [loaded, setLoaded] = useState(false);
  const [overlayOwn, setOverlayOwn] = useState({});  // player id -> manual ownership override from the Optimizer
  const [toast, setToast] = useState('');

  // Load this week's shortlist + the Optimizer's manual ownership overrides.
  // App fires several week changes at startup (default week, then the real one), so responses can
  // arrive out of order -- `cancelled` makes a superseded request drop its result instead of
  // overwriting the current week's lineups (the bug that showed an empty Week 5).
  useEffect(() => {
    let cancelled = false;
    setLoaded(false);
    (async () => {
      const [port, state] = await Promise.all([
        ApiService.getShortlist(selectedWeek, 'classic'),
        ApiService.getOptimizerState(selectedWeek),
      ]);
      if (cancelled) return;
      setEntries(port.lineups || []);
      const own = {};
      Object.entries(state?.overlay?.players || {}).forEach(([id, ov]) => {
        if (ov && ov.ownershipPct != null) own[id] = ov.ownershipPct;
      });
      setOverlayOwn(own);
      setLoaded(true);
    })();
    return () => { cancelled = true; };
  }, [selectedWeek]);
  useEffect(() => {
    if (!toast) return undefined;
    const t = setTimeout(() => setToast(''), 4000);
    return () => clearTimeout(t);
  }, [toast]);

  // Same slate restriction the Optimizer applies (main-slate teams only).
  const slateTeams = useMemo(() => {
    const s = new Set();
    (games || []).forEach(g => { if (g.dk_main) { s.add(g.away_team); s.add(g.home_team); } });
    return s;
  }, [games]);

  // Live pool: weekly-sim ownership, with the Optimizer's manual overrides on top.
  const pool = useMemo(() => buildPlayerPool(weekProjections, allSimResults, games, slateTeams)
    .map(p => (overlayOwn[p.id] != null ? { ...p, ownershipPct: overlayOwn[p.id] } : p)),
  [weekProjections, allSimResults, games, slateTeams, overlayOwn]);

  // Lineups carry their entry_id so a row can be removed; everything else is the saved optimizer lineup.
  const lineups = useMemo(() => entries.map(e => ({ ...e.lineup, _entry_id: e.entry_id, _saved_at: e.saved_at })), [entries]);

  const remove = async (entryId) => {
    const res = await ApiService.removeFromShortlist(selectedWeek, { entry_ids: [entryId] }, 'classic');
    if (res) setEntries(res.lineups || []); else setToast('Remove failed (backend unreachable)');
  };
  // Move out every lineup currently shown (respects the exposure drill-down / FLEX filter):
  // e.g. click a player you've soured on, then "Remove N shown".
  const removeMany = async (shown) => {
    const ids = shown.map(l => l._entry_id).filter(Boolean);
    if (!ids.length) return;
    if (!window.confirm(`Move ${ids.length} lineup${ids.length === 1 ? '' : 's'} out of the shortlist?`)) return;
    const res = await ApiService.removeFromShortlist(selectedWeek, { entry_ids: ids }, 'classic');
    if (res) { setEntries(res.lineups || []); setToast(`Removed ${ids.length}`); } else setToast('Remove failed (backend unreachable)');
  };
  const clearAll = async () => {
    if (!window.confirm(`Remove all ${entries.length} saved lineups from this week's shortlist?`)) return;
    const res = await ApiService.removeFromShortlist(selectedWeek, { clear: true }, 'classic');
    if (res) setEntries([]); else setToast('Clear failed (backend unreachable)');
  };

  // Same week picker the Optimizer has -- each week keeps its own shortlist.
  const weekPicker = weeks.length > 0 && (
    <div style={{ display: 'flex', alignItems: 'center', marginBottom: '12px' }}>
      <span style={{ fontWeight: 700, color: 'var(--text-white)', marginRight: '10px' }}>Week:</span>
      <select value={selectedWeek} onChange={e => setSelectedWeek(Number(e.target.value))}
        style={{ ...pillBtnBase, padding: '6px 10px', background: 'rgba(255,255,255,0.04)', color: 'var(--text-white)' }}>
        {weeks.map(w => <option key={w} value={w}>Week {w}</option>)}
      </select>
    </div>
  );

  if (loaded && entries.length === 0) {
    return (
      <div>
      {weekPicker}
      <div style={{ ...cardStyle, textAlign: 'center', padding: '40px 20px' }}>
        <h2 style={{ marginTop: 0 }}>Shortlist — Week {selectedWeek}</h2>
        <p style={{ color: 'var(--text-muted)', margin: 0 }}>
          No saved lineups yet. In the DFS Optimizer's results, click <strong>＋</strong> on a lineup (or
          <strong> ＋ Save … to Shortlist</strong> to save everything shown) and they will collect here.
        </p>
      </div>
      </div>
    );
  }

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      {weekPicker}
      <ClassicLineupAnalysis
        lineups={lineups}
        pool={pool}
        preferPoolOwnership
        salaryCap={50000}
        summaryTitle={`Shortlist — Week ${selectedWeek}`}
        summarySubtitle={`${lineups.length} saved lineup${lineups.length === 1 ? '' : 's'} · ownership is live (manual overrides from the Optimizer apply)`}
        subtitleExtra={toast ? <span style={{ marginLeft: '8px', color: 'var(--accent-green)' }}>{toast}</span> : null}
        tableTitle="Shortlisted Lineups"
        exposureTitle="Shortlist Exposure"
        emptyMessage="Loading saved lineups…"
        headerActions={({ sortedLineups, displayedLineups }) => (
          <>
            {displayedLineups.length < sortedLineups.length && (
              <button onClick={() => removeMany(displayedLineups)} style={btn('#f59e0b')}
                title="Remove every lineup matching the current filter from the shortlist">Remove {displayedLineups.length} shown</button>
            )}
            <button onClick={clearAll} style={btn('#ef4444')} title="Remove every lineup from this week's shortlist">Clear all</button>
            <button onClick={() => exportSummaryCSV(sortedLineups, `shortlist_week${selectedWeek}_summary.csv`)} style={btn('#9ca3af')}
              title="Human-readable summary -- not accepted by DK's upload form">↓ Export Summary</button>
            <button onClick={() => exportDkUploadCSV(sortedLineups, `dk_upload_shortlist_week${selectedWeek}.csv`)} style={btn('#00f2fe')}
              title="DK's real Bulk Upload format (player IDs saved with each lineup -- re-save if salaries refreshed)">↓ Export for DK Upload</button>
          </>
        )}
        rowActionHeader=""
        rowAction={(lu) => (
          <button onClick={() => remove(lu._entry_id)} title="Move out of the shortlist" style={{
            border: '1px solid rgba(239,68,68,0.3)', background: 'rgba(239,68,68,0.08)', color: '#ef4444',
            borderRadius: '6px', cursor: 'pointer', fontSize: '0.7rem', fontWeight: 700, padding: '2px 7px',
          }}>✕</button>
        )}
      />
    </div>
  );
}
