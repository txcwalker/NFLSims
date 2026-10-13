import { Fragment, useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';
import PickemSlip from '../components/PickemSlip';

/**
 * Underdog Pick'em lines -- paste the board, grade it against our sims (Pickems > Underdog Lines).
 *
 * Flow: paste -> POST /api/pickems/grade (src/pickems/: parser -> ledger -> grader). The board
 * ACCUMULATES: each paste adds only new/changed legs to the week's ledger (data/pickems/{year}/
 * line_history.csv), so you can paste one market at a time and re-paste to pick up moved lines.
 * The page loads the accumulated board from GET /api/pickems/board on open / week change.
 *
 * Every multiplier is the leg's odds: it is +EV only when our sim probability beats 1/multiplier,
 * so the headline numbers are EV and the MINIMUM multiplier worth taking -- compare that one number
 * with what the Underdog app shows when you enter the pick (multipliers move).
 *
 * Layout: top plays per market (ranked by EV, thin players hidden), everything else via search.
 * "Thin" = sim opportunities per game (rush attempts + targets; pass attempts for QBs) below the
 * adjustable cutoff -- they stay searchable. First/Last TD Scorer are ignored by design.
 * Any filter (text, Game dropdown, Team dropdown) switches to one flat results table with every
 * leg for the selection, thin players and -EV legs included; text accepts a team code or a
 * team pair ("det car") for a game.
 *
 * Underdog's Terms of Use (6.ix) allow only manual browser access: no scraper or poller.
 * Click any row to open the line panel: slide the line up/down and see our probability and the
 * minimum multiplier each side needs at that line (GET /api/pickems/ladder).
 * "EV" assumes our sim is right; nothing here has been graded against results yet.
 *
 * Inputs (props): weeks (App's week list), selectedWeek / setSelectedWeek.
 */
const inputStyle = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '5px 8px', fontSize: '0.82rem' };
const card = { padding: '16px', borderRadius: '12px', border: '1px solid var(--border-glass)', marginBottom: '14px' };
const th = { padding: '6px 8px', textAlign: 'left', whiteSpace: 'nowrap', fontWeight: 600, fontSize: '0.72rem', textTransform: 'uppercase', color: 'var(--text-muted)' };
const td = { padding: '5px 8px', whiteSpace: 'nowrap', fontSize: '0.84rem' };
const TOP_N = 10;
const SEARCH_CAP = 500;
const MARKET_ORDER = ['Rush + Rec TDs', 'Passing Yards', 'Passing TDs', 'Pass Attempts', 'Pass Completions', 'Interceptions',
  'Rushing Yards', 'Rush Attempts', 'Receiving Yards', 'Receptions', 'Rush + Rec Yards'];

const pct = (v) => (v == null ? '—' : `${(v * 100).toFixed(1)}%`);
const mult = (v) => (v == null ? '—' : `${v.toFixed(2)}x`);
/** Decimal payout multiplier (stake included) -> American odds string: 1.96x = -104, 3.70x = +270. */
const american = (m) => {
  if (m == null || m <= 1) return '—';
  const a = m >= 2 ? Math.round(100 * (m - 1)) : -Math.round(100 / (m - 1));
  return a > 0 ? `+${a}` : `${a}`;
};

/** Game id "AWAY@HOME" for a graded row (home=true means the row's team is the home side). */
const gameKey = (r) => (r.home ? `${r.opp}@${r.team}` : `${r.team}@${r.opp}`);

const COLS = 14;
const legId = (r) => `${r.team}|${r.player}|${r.market}|${r.line}|${r.side}`;

/**
 * Click-a-player panel: for this player + market, slide the line up or down and see our
 * probability and the minimum multiplier (1 / probability) each side needs to be +EV there.
 * Inputs: r (graded row), week. Data: GET /api/pickems/ladder (lines +-12 around the board line).
 */
function LadderPanel({ r, week }) {
  const [data, setData] = useState(null);
  const [err, setErr] = useState(null);
  const [i, setI] = useState(0);   // index into data.lines
  useEffect(() => {
    let alive = true;
    ApiService.getPickemLadder(week, r.team, r.player, r.market, r.line).then(res => {
      if (!alive) return;
      if (!res.ok) { setErr(res.error); return; }
      setData(res.data);
      setI(Math.max(0, res.data.lines.findIndex(l => l.line === r.line)));
    });
    return () => { alive = false; };
  }, [week, r.team, r.player, r.market, r.line]);

  if (err) return <div style={{ color: '#f87171', fontSize: '0.82rem' }}>{err}</div>;
  if (!data) return <div style={{ color: 'var(--text-muted)', fontSize: '0.82rem' }}>Loading…</div>;
  const L = data.lines[i];
  const side = (label, p, m) => (
    <div style={{ minWidth: 150 }}>
      <div style={{ color: 'var(--text-muted)', fontSize: '0.72rem', textTransform: 'uppercase' }}>{label} {L.line}</div>
      <div style={{ fontSize: '0.84rem' }}>Sim {pct(p)}</div>
      <div style={{ fontWeight: 700, fontSize: '1.05rem' }}>Min mult {mult(m)} <span style={{ color: 'var(--text-muted)', fontWeight: 400, fontSize: '0.8rem' }}>({american(m)})</span></div>
    </div>
  );
  return (
    <div style={{ padding: '8px 4px' }}>
      <div style={{ fontSize: '0.82rem', marginBottom: 8 }}>
        <b>{r.player}</b> · {r.market} · sim mean {data.mean.toFixed(1)}, median {data.median}
        <span style={{ color: 'var(--text-muted)' }}> · board line {r.line} ({r.side} pays {mult(r.multiplier)})</span>
      </div>
      <div style={{ display: 'flex', alignItems: 'center', gap: 16, flexWrap: 'wrap' }}>
        <button style={{ ...inputStyle, cursor: 'pointer' }} disabled={i === 0} onClick={() => setI(i - 1)}>▼ Lower line</button>
        <input type="range" min={0} max={data.lines.length - 1} value={i} onChange={e => setI(Number(e.target.value))} style={{ width: 220 }} />
        <button style={{ ...inputStyle, cursor: 'pointer' }} disabled={i === data.lines.length - 1} onClick={() => setI(i + 1)}>Higher line ▲</button>
        {side('Higher', L.p_higher, L.min_mult_higher)}
        {side('Lower', L.p_lower, L.min_mult_lower)}
        {L.line !== r.line && <button style={{ ...inputStyle, cursor: 'pointer' }} onClick={() => setI(data.lines.findIndex(l => l.line === r.line))}>Back to {r.line}</button>}
      </div>
      <div style={{ color: 'var(--text-muted)', fontSize: '0.74rem', marginTop: 6 }}>
        Take a side at the shifted line only if Underdog pays at least its Min mult.
      </div>
    </div>
  );
}

/** One graded-leg table. rows: graded rows to show; week: for the click-to-expand line panel. */
function LegTable({ rows, week, onAdd }) {
  const [openId, setOpenId] = useState(null);
  return (
    <div style={{ overflowX: 'auto' }}>
      <table style={{ width: '100%', borderCollapse: 'collapse' }}>
        <thead><tr>
          {['Player', 'Game', 'Pick', 'Line', 'Sim avg', 'Mult', 'Odds', 'Open', 'Market', 'Sim', 'Min mult', 'EV', 'Usage', ''].map(h => <th key={h} style={th}>{h}</th>)}
        </tr></thead>
        <tbody>
          {rows.map((r, i) => {
            const moved = r.prev_multiplier != null && r.prev_multiplier !== r.multiplier;
            const id = legId(r);
            return (
              <Fragment key={i}>
              <tr onClick={() => setOpenId(openId === id ? null : id)} title="Click to see min multipliers at other lines"
                style={{ borderTop: '1px solid rgba(255,255,255,0.06)', cursor: 'pointer', background: openId === id ? 'rgba(255,255,255,0.06)' : undefined }}>
                <td style={td}>{r.player} <span style={{ color: 'var(--text-muted)' }}>{r.team} {r.pos}</span></td>
                <td style={{ ...td, color: 'var(--text-muted)' }}>{r.home ? 'vs' : '@'} {r.opp}</td>
                <td style={td}>{r.side}</td>
                <td style={{ ...td, fontWeight: 600 }} title={r.market}>{r.line} <span style={{ color: 'var(--text-muted)', fontWeight: 400, fontSize: '0.72rem' }}>{r.market}</span></td>
                <td style={{ ...td, color: 'var(--text-muted)' }} title="Our simulated mean for this stat">{r.sim_mean.toFixed(1)}</td>
                <td style={td}>{mult(r.multiplier)}
                  {moved && <span title={`was ${mult(r.prev_multiplier)} before the latest paste`}
                    style={{ marginLeft: 4, color: r.multiplier > r.prev_multiplier ? '#34d399' : '#f87171' }}>{r.multiplier > r.prev_multiplier ? '▲' : '▼'}</span>}
                </td>
                <td style={{ ...td, color: 'var(--text-muted)' }} title="American odds equivalent of the multiplier">{american(r.multiplier)}</td>
                <td style={{ ...td, color: 'var(--text-muted)' }}>{mult(r.open_multiplier)}</td>
                <td style={td} title="Two-sided odds with Underdog's margin removed; blank-star when only one side is offered">{pct(r.market_prob ?? r.implied_prob)}{r.market_prob == null ? '*' : ''}</td>
                <td style={td}>{pct(r.sim_prob)}{r.flag && <span title="Sim and market differ by 25+ pts — check roles/injury news before trusting this" style={{ marginLeft: 4 }}>⚠️</span>}</td>
                <td style={{ ...td, fontWeight: 600 }}>{mult(r.min_multiplier)} <span style={{ color: 'var(--text-muted)', fontWeight: 400, fontSize: '0.74rem' }}>({american(r.min_multiplier)})</span></td>
                <td style={{ ...td, fontWeight: 700, color: r.ev_pct > 0 ? '#34d399' : '#f87171' }}>{r.ev_pct > 0 ? '+' : ''}{r.ev_pct.toFixed(1)}%</td>
                <td style={{ ...td, color: 'var(--text-muted)' }} title="Sim rush attempts + targets per game (pass attempts for QBs)">{r.usage.toFixed(1)}</td>
                <td style={td}><button onClick={(e) => { e.stopPropagation(); onAdd(r); }} title="Add this leg to the entry calculator"
                  style={{ ...inputStyle, cursor: 'pointer', padding: '1px 8px' }}>+</button></td>
              </tr>
              {openId === id && (
                <tr><td colSpan={COLS} style={{ background: 'rgba(0,0,0,0.2)', padding: '6px 10px' }}><LadderPanel r={r} week={week} /></td></tr>
              )}
              </Fragment>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

export default function PickemLines({ weeks = [], selectedWeek, setSelectedWeek }) {
  const [paste, setPaste] = useState('');
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState(null);
  const [board, setBoard] = useState(null);       // graded accumulated board
  const [lastPaste, setLastPaste] = useState(null); // {parsed,new,changed,unchanged} of the latest paste this session
  const [evOnly, setEvOnly] = useState(true);
  const [minUsage, setMinUsage] = useState(3);
  const [query, setQuery] = useState('');
  const [game, setGame] = useState('');           // "AWAY@HOME" or '' = any
  const [team, setTeam] = useState('');           // player's team code or '' = any
  const [slip, setSlip] = useState([]);           // entry-calculator legs (cleared on week change)
  const [expanded, setExpanded] = useState({});   // market -> show all non-thin legs

  // Load the accumulated board whenever the week changes.
  useEffect(() => {
    let alive = true;
    setBoard(null); setError(null); setLastPaste(null); setGame(''); setTeam(''); setSlip([]);
    ApiService.getPickemBoard(selectedWeek).then(res => {
      if (!alive) return;
      if (res.ok) setBoard(res.data); else setError(res.error);
    });
    return () => { alive = false; };
  }, [selectedWeek]);

  const grade = async () => {
    setBusy(true); setError(null);
    const res = await ApiService.gradePickemBoard(paste, selectedWeek);
    setBusy(false);
    if (res.ok) { setBoard(res.data); setLastPaste(res.data.paste); setPaste(''); } else setError(res.error);
  };

  /** Add a board row to the slip; a second side of the same player+market replaces the first. */
  const addToSlip = (r) => setSlip(ls => [...ls.filter(l => !(l.team === r.team && l.player === r.player && l.market === r.market)),
    { id: legId(r), team: r.team, player: r.player, market: r.market, line: r.line, side: r.side, multiplier: r.multiplier, pos: r.pos }]);

  const q = query.trim().toLowerCase();
  const searching = Boolean(q || game || team);   // any filter -> flat results table, thin players included

  // Dropdown options: every game and team that has a priced leg on the board.
  const { games, teams } = useMemo(() => {
    const g = new Map(), t = new Set();
    for (const r of board?.rows || []) {
      g.set(gameKey(r), `${gameKey(r).replace('@', ' @ ')} · ${r.kickoff}`);
      t.add(r.team);
    }
    return { games: [...g.entries()].sort((a, b) => a[1].localeCompare(b[1])), teams: [...t].sort() };
  }, [board]);

  const { sections, searchRows, thinHidden } = useMemo(() => {
    if (!board) return { sections: [], searchRows: [], thinHidden: 0 };
    // Text: player / market substring, a team code (that team's players), or two team codes
    // ("det car", "det @ car", "det vs car") = that game.
    const codes = q.split(/[^a-z]+/).filter(Boolean).map(s => s.toUpperCase());
    const textOk = (r) => {
      if (!q) return true;
      if (codes.length === 2 && codes.every(c => c === r.team || c === r.opp)) return true;
      return r.player.toLowerCase().includes(q) || r.team.toLowerCase() === q || r.market.toLowerCase().includes(q);
    };
    const matches = (r) => textOk(r) && (!game || gameKey(r) === game) && (!team || r.team === team);
    const searchRows = searching ? board.rows.filter(matches) : [];
    const byMarket = {};
    let thin = 0;
    for (const r of board.rows) {
      if (r.usage < minUsage) { thin += 1; continue; }
      if (evOnly && r.ev_pct <= 0) continue;
      (byMarket[r.market] ||= []).push(r);
    }
    const rank = (m) => { const i = MARKET_ORDER.indexOf(m); return i < 0 ? 99 : i; };
    const sections = Object.entries(byMarket).sort((a, b) => rank(a[0]) - rank(b[0]) || a[0].localeCompare(b[0]));
    return { sections, searchRows, thinHidden: thin };
  }, [board, q, game, team, searching, evOnly, minUsage]);

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px', width: '100%' }}>
      <div className="glass-panel" style={{ ...card, marginBottom: '18px', padding: '12px 20px', display: 'flex', gap: '18px', alignItems: 'center', flexWrap: 'wrap' }}>
        <span style={{ fontWeight: 700, color: 'var(--text-white)' }}>🐕 Underdog Lines</span>
        {weeks.length > 0 && (
          <label style={{ display: 'flex', alignItems: 'center', gap: '6px', fontSize: '0.85rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>Week</span>
            <select value={selectedWeek} onChange={e => setSelectedWeek?.(Number(e.target.value))} style={{ ...inputStyle, width: 'auto' }}>
              {weeks.map(w => <option key={w} value={w}>{w}</option>)}
            </select>
          </label>
        )}
      </div>

      <div className="glass-panel" style={card}>
        <p style={{ margin: '0 0 10px', color: 'var(--text-muted)', fontSize: '0.85rem' }}>
          Open the Underdog Pick'em board, select all, copy, paste here — one market or several at a time. Each paste only
          adds new lines and updates ones whose multiplier moved; everything pasted so far stays on the board.
          A leg is +EV only when our probability beats 1 / multiplier.
        </p>
        <textarea value={paste} onChange={e => setPaste(e.target.value)} rows={6}
          placeholder="Paste the Underdog board here..."
          style={{ ...inputStyle, width: '100%', boxSizing: 'border-box', fontFamily: 'monospace' }} />
        <button onClick={grade} disabled={busy || !paste.trim()}
          style={{ ...inputStyle, marginTop: '10px', cursor: busy || !paste.trim() ? 'not-allowed' : 'pointer', opacity: busy || !paste.trim() ? 0.5 : 1 }}>
          {busy ? 'Grading…' : `Add to Week ${selectedWeek} board`}
        </button>
        {lastPaste && <span style={{ marginLeft: '12px', color: 'var(--text-muted)', fontSize: '0.82rem' }}>
          Last paste: {lastPaste.parsed} legs — {lastPaste.new} new, {lastPaste.changed} changed, {lastPaste.unchanged} unchanged</span>}
        {error && <span style={{ marginLeft: '12px', color: '#f87171', fontSize: '0.85rem' }}>{error}</span>}
      </div>

      <PickemSlip legs={slip} setLegs={setSlip} week={selectedWeek} />

      {board && (
        <div className="glass-panel" style={card}>
          <div style={{ display: 'flex', gap: '14px', alignItems: 'center', flexWrap: 'wrap', marginBottom: '10px', fontSize: '0.82rem' }}>
            <span style={{ color: 'var(--text-muted)' }}>
              {board.board_legs} legs on board · {board.rows.length} priced · {board.rows.filter(r => r.ev_pct > 0).length} +EV
              · sims updated {new Date(board.sims_updated_at * 1000).toLocaleString([], { month: 'short', day: 'numeric', hour: 'numeric', minute: '2-digit' })}
            </span>
            <input value={query} onChange={e => setQuery(e.target.value)} placeholder="Search player, team, market or game (e.g. DET CAR)" style={{ ...inputStyle, width: '300px' }} />
            <select value={game} onChange={e => setGame(e.target.value)} style={{ ...inputStyle, width: 'auto' }} title="Show every leg in one game">
              <option value="">All games</option>
              {games.map(([k, label]) => <option key={k} value={k}>{label}</option>)}
            </select>
            <select value={team} onChange={e => setTeam(e.target.value)} style={{ ...inputStyle, width: 'auto' }} title="Show every leg for one team's players">
              <option value="">All teams</option>
              {teams.map(t => <option key={t} value={t}>{t}</option>)}
            </select>
            {searching && <button onClick={() => { setQuery(''); setGame(''); setTeam(''); }} style={{ ...inputStyle, cursor: 'pointer' }}>Clear</button>}
            <label><input type="checkbox" checked={evOnly} onChange={e => setEvOnly(e.target.checked)} /> +EV only</label>
            <label title="Players whose sim opportunities per game (rush attempts + targets; pass attempts for QBs) fall below this are hidden from the top plays but stay searchable">
              Min usage <input type="number" min="0" step="0.5" value={minUsage} onChange={e => setMinUsage(Number(e.target.value) || 0)} style={{ ...inputStyle, width: '60px' }} />
            </label>
          </div>
          {Object.keys(board.unsupported).length > 0 && (
            <div style={{ color: 'var(--accent-gold)', fontSize: '0.8rem', marginBottom: '8px' }}>
              Not priced yet (exact Underdog market names): {Object.entries(board.unsupported).map(([m, n]) => `${m} (${n} legs)`).join(', ')}.
            </div>
          )}
          {board.unmatched.length > 0 && (
            <div style={{ color: 'var(--accent-gold)', fontSize: '0.8rem', marginBottom: '8px' }}>
              No sim player for: {board.unmatched.map(u => `${u.player} (${u.team})`).join(', ')} — not on a simmed roster this week.
            </div>
          )}

          {board.board_legs === 0 && <div style={{ color: 'var(--text-muted)', padding: '12px 4px', fontSize: '0.85rem' }}>Nothing on the Week {selectedWeek} board yet — paste a market above.</div>}

          {searching ? (
            <>
              <h4 style={{ margin: '6px 0', color: 'var(--text-white)' }}>Results: {searchRows.length} legs, best EV first (thin players and −EV legs included){searchRows.length > SEARCH_CAP ? ` — showing first ${SEARCH_CAP}` : ''}</h4>
              <LegTable rows={searchRows.slice(0, SEARCH_CAP)} week={selectedWeek} onAdd={addToSlip} />
            </>
          ) : (
            <>
              {sections.map(([market, rows]) => {
                const open = expanded[market];
                return (
                  <div key={market} style={{ marginBottom: '18px' }}>
                    <h4 style={{ margin: '6px 0', color: 'var(--text-white)' }}>
                      {market} <span style={{ color: 'var(--text-muted)', fontWeight: 400, fontSize: '0.78rem' }}>
                        top {Math.min(TOP_N, rows.length)} of {rows.length}</span>
                      {rows.length > TOP_N && (
                        <button onClick={() => setExpanded(e => ({ ...e, [market]: !open }))} style={{ ...inputStyle, marginLeft: '10px', padding: '2px 8px', cursor: 'pointer' }}>
                          {open ? 'Show top 10' : `Show all ${rows.length}`}
                        </button>
                      )}
                    </h4>
                    <LegTable rows={open ? rows : rows.slice(0, TOP_N)} week={selectedWeek} onAdd={addToSlip} />
                  </div>
                );
              })}
              {board.board_legs > 0 && sections.length === 0 && (
                <div style={{ color: 'var(--text-muted)', padding: '12px 4px', fontSize: '0.85rem' }}>No legs pass the filters — try un-ticking +EV only or lowering Min usage.</div>
              )}
              {thinHidden > 0 && <div style={{ color: 'var(--text-muted)', fontSize: '0.78rem' }}>{thinHidden} legs hidden as thin (usage under {minUsage}) — search a name to see them.</div>}
            </>
          )}
          <div style={{ color: 'var(--text-muted)', fontSize: '0.74rem', marginTop: '8px' }}>
            * one-sided leg: Market shows 1/multiplier, which still includes Underdog's margin. Take a leg only at or above its Min mult — multipliers move.
            ▲/▼ = multiplier moved on the latest paste; Open = first multiplier we captured.
          </div>
        </div>
      )}
    </div>
  );
}
