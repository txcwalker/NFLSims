import { useState, useEffect, useCallback } from 'react';

const API_BASE = '/api';

function BotFeed() {
  const [plays, setPlays] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [autoRefresh, setAutoRefresh] = useState(true);

  const load = useCallback(async () => {
    try {
      const res = await fetch(`${API_BASE}/live-bot-feed?limit=200`);
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const data = await res.json();
      setPlays(data.plays || []);
      setError(null);
    } catch (err) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    load();
  }, [load]);

  useEffect(() => {
    if (!autoRefresh) return;
    const id = setInterval(load, 15000);
    return () => clearInterval(id);
  }, [autoRefresh, load]);

  const fmtPct = (v) => (v === null || v === undefined ? '—' : `${Math.round(v * 100)}%`);
  const fmtYardline = (y) => {
    if (y === null || y === undefined) return '—';
    const v = Math.round(y);
    return v <= 50 ? `Opp ${v}` : `Own ${100 - v}`;
  };

  return (
    <div style={{ padding: '24px', maxWidth: '1200px', margin: '0 auto' }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '8px' }}>
        <h1 style={{ margin: 0 }}>Bot Test Feed</h1>
        <label style={{ fontSize: '13px', color: 'var(--text-muted)', display: 'flex', alignItems: 'center', gap: '6px' }}>
          <input
            type="checkbox"
            checked={autoRefresh}
            onChange={(e) => setAutoRefresh(e.target.checked)}
          />
          Auto-refresh (15s)
        </label>
      </div>
      <p style={{ color: 'var(--text-muted)', fontSize: '13px', marginTop: 0 }}>
        Every 4th down the live bot has evaluated (DRY_RUN mode) &mdash; posted or skipped, with the model's
        recommendation vs. the actual call and the exact post text it would have sent.
      </p>

      {loading && <p>Loading...</p>}
      {error && <p style={{ color: 'var(--accent-red, #e05252)' }}>Failed to load: {error}</p>}
      {!loading && !error && plays.length === 0 && (
        <p style={{ color: 'var(--text-muted)' }}>No evaluated 4th downs yet. This fills in as the daemon polls live games.</p>
      )}

      <div style={{ display: 'flex', flexDirection: 'column', gap: '12px' }}>
        {plays.map((p, i) => (
          <div
            key={`${p.game_id}-${p.play_id}-${i}`}
            style={{
              border: '1px solid var(--border-color)',
              borderRadius: '6px',
              padding: '14px 16px',
              backgroundColor: 'var(--bg-secondary)',
            }}
          >
            <div style={{ display: 'flex', justifyContent: 'space-between', flexWrap: 'wrap', gap: '8px', marginBottom: '6px' }}>
              <strong>
                {p.off} vs {p.def} &middot; Q{p.qtr} {p.clock} &middot; 4th & {p.ydstogo} at {fmtYardline(p.yardline_100)}
              </strong>
              <span
                style={{
                  fontSize: '11px',
                  fontWeight: 700,
                  padding: '2px 8px',
                  borderRadius: '4px',
                  backgroundColor: p.should_post ? 'var(--accent-green, #2e7d32)' : 'var(--bg-tertiary)',
                  color: p.should_post ? '#fff' : 'var(--text-muted)',
                }}
              >
                {p.should_post ? 'POSTED (dry-run)' : `SKIPPED — ${p.post_reason}`}
              </span>
            </div>
            <div style={{ fontSize: '13px', color: 'var(--text-muted)', marginBottom: '8px' }}>
              Called: <strong>{(p.called_action || '?').toUpperCase()}</strong> &nbsp;|&nbsp;
              Model recommends: <strong>{(p.best_action || '?').toUpperCase()}</strong> &nbsp;|&nbsp;
              GO {fmtPct(p.wp_go)} &nbsp;PUNT {fmtPct(p.wp_punt)} &nbsp;FG {fmtPct(p.wp_fg)}
              {p.wp_gap ? <> &nbsp;|&nbsp; WP gap {fmtPct(p.wp_gap)}</> : null}
            </div>
            {p.post_text && (
              <pre
                style={{
                  whiteSpace: 'pre-wrap',
                  fontFamily: 'inherit',
                  fontSize: '13px',
                  margin: 0,
                  padding: '10px 12px',
                  backgroundColor: 'var(--bg-tertiary)',
                  borderRadius: '4px',
                }}
              >
                {p.post_text}
              </pre>
            )}
          </div>
        ))}
      </div>
    </div>
  );
}

export default BotFeed;
