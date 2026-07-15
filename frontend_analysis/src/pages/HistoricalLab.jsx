/**
 * HistoricalLab.jsx
 * =================
 * Testing lab: chess-engine-style evaluation for Week 1 2025 NFL games.
 *
 * Layout mirrors Lichess analysis board:
 *   - Top:    KEP evaluation bar (home = positive/right, away = negative/left)
 *   - Middle: Suggested play lines (principal variations, depth 2)
 *   - Bottom: Full-game KEP timeline (home-team reference, no sign-flipping)
 *
 * NOTE: Remove this page before production launch — for internal metric evaluation only.
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import {
  LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip,
  ReferenceLine, ResponsiveContainer,
} from 'recharts';

// ─── team colors ───────────────────────────────────────────────────────────────

const TEAM_COLORS = {
  ARI: '#97233F', ATL: '#A71930', BAL: '#241773', BUF: '#00338D',
  CAR: '#0085CA', CHI: '#0B162A', CIN: '#FB4F14', CLE: '#311D00',
  DAL: '#003594', DEN: '#FB4F14', DET: '#0076B6', GB:  '#203731',
  HOU: '#03202F', IND: '#002C5F', JAX: '#006778', KC:  '#E31837',
  LAC: '#0080C6', LAR: '#003594', LV:  '#A5ACAF', MIA: '#008E97',
  MIN: '#4F2683', NE:  '#002244', NO:  '#D3BC8D', NYG: '#0B2265',
  NYJ: '#125740', PHI: '#004C54', PIT: '#FFB612', SEA: '#002244',
  SF:  '#AA0000', TB:  '#D50A0A', TEN: '#4B92DB', WAS: '#5A1414',
};

function teamColor(abbr) {
  return TEAM_COLORS[abbr] ?? '#888888';
}

// ─── helpers ───────────────────────────────────────────────────────────────────

function formatClock(gameSec) {
  const totalSec = Math.max(0, Math.round(gameSec));
  const qtr = Math.min(4, Math.floor((3600 - totalSec) / 900) + 1);
  const secsInQ = totalSec - (4 - qtr) * 900;
  const mm = Math.floor(secsInQ / 60);
  const ss = secsInQ % 60;
  return `Q${qtr} ${mm}:${ss.toString().padStart(2, '0')}`;
}

function kepLabel(homeKep, homeTeam, awayTeam) {
  const abs = Math.abs(homeKep).toFixed(2);
  if (Math.abs(homeKep) < 0.5) return 'Position is even';
  const team = homeKep > 0 ? homeTeam : awayTeam;
  return `${team} +${abs} KEP advantage`;
}

// ─── sub-components ─────────────────────────────────────────────────────────────

/** Horizontal evaluation bar — home team on right (positive), away on left (negative). */
function EvalBar({ homeKep, homeTeam, awayTeam, homeScore, awayScore }) {
  const clampedKep = Math.max(-24, Math.min(24, homeKep ?? 0));
  const rightPct = ((clampedKep + 24) / 48) * 100;
  const homeColor = teamColor(homeTeam);
  const awayColor = teamColor(awayTeam);
  const leadColor = homeKep >= 0 ? homeColor : awayColor;

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
      {/* Scoreboard */}
      <div style={{
        display: 'flex', justifyContent: 'space-between', alignItems: 'center',
        background: 'var(--bg-tertiary)', borderRadius: 6, padding: '10px 16px',
        border: '1px solid var(--border-color)',
      }}>
        <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
          <span style={{ width: 12, height: 12, borderRadius: 2, background: awayColor, display: 'inline-block', flexShrink: 0 }} />
          <span style={{ fontFamily: 'var(--font-heading)', fontWeight: 700, fontSize: 15, color: 'var(--text-primary)' }}>{awayTeam}</span>
          <span style={{ fontSize: 11, color: 'var(--text-muted)' }}>away</span>
        </div>
        <div style={{ display: 'flex', alignItems: 'center', gap: 12 }}>
          <span style={{
            fontFamily: 'var(--font-heading)', fontWeight: 800, fontSize: 28,
            color: (awayScore ?? 0) > (homeScore ?? 0) ? awayColor : 'var(--text-primary)',
            minWidth: 32, textAlign: 'right',
          }}>{awayScore ?? '—'}</span>
          <span style={{ color: 'var(--text-muted)', fontSize: 16 }}>–</span>
          <span style={{
            fontFamily: 'var(--font-heading)', fontWeight: 800, fontSize: 28,
            color: (homeScore ?? 0) > (awayScore ?? 0) ? homeColor : 'var(--text-primary)',
            minWidth: 32, textAlign: 'left',
          }}>{homeScore ?? '—'}</span>
        </div>
        <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
          <span style={{ fontSize: 11, color: 'var(--text-muted)' }}>home</span>
          <span style={{ fontFamily: 'var(--font-heading)', fontWeight: 700, fontSize: 15, color: 'var(--text-primary)' }}>{homeTeam}</span>
          <span style={{ width: 12, height: 12, borderRadius: 2, background: homeColor, display: 'inline-block', flexShrink: 0 }} />
        </div>
      </div>

      {/* Team labels + KEP value */}
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <span style={{ display: 'flex', alignItems: 'center', gap: 6, fontSize: 13, fontWeight: 600, color: 'var(--text-primary)' }}>
          <span style={{ width: 10, height: 10, borderRadius: 2, background: awayColor, display: 'inline-block' }} />
          {awayTeam}
        </span>
        <span style={{
          fontFamily: 'var(--font-heading)', fontWeight: 700, fontSize: 22,
          color: leadColor,
        }}>
          {homeKep >= 0 ? '+' : ''}{homeKep?.toFixed(2) ?? '—'}
        </span>
        <span style={{ display: 'flex', alignItems: 'center', gap: 6, fontSize: 13, fontWeight: 600, color: 'var(--text-primary)' }}>
          {homeTeam}
          <span style={{ width: 10, height: 10, borderRadius: 2, background: homeColor, display: 'inline-block' }} />
        </span>
      </div>

      {/* Bar */}
      <div style={{
        position: 'relative', height: 20, borderRadius: 4,
        background: 'var(--bg-tertiary)', overflow: 'hidden',
        border: '1px solid var(--border-color)',
      }}>
        {/* Away (left) fill */}
        <div style={{
          position: 'absolute', left: 0, top: 0, bottom: 0,
          width: `${100 - rightPct}%`,
          background: clampedKep < 0
            ? `linear-gradient(to right, ${awayColor}, ${awayColor}44)`
            : 'transparent',
          transition: 'width 0.4s ease',
        }} />
        {/* Home (right) fill */}
        <div style={{
          position: 'absolute', right: 0, top: 0, bottom: 0,
          width: `${rightPct}%`,
          background: clampedKep > 0
            ? `linear-gradient(to left, ${homeColor}, ${homeColor}44)`
            : 'transparent',
          transition: 'width 0.4s ease',
        }} />
        {/* Center line */}
        <div style={{
          position: 'absolute', left: '50%', top: 0, bottom: 0,
          width: 1, background: 'var(--border-color)',
        }} />
        {/* Divider dot */}
        <div style={{
          position: 'absolute', left: `${rightPct}%`, top: '50%',
          transform: 'translate(-50%, -50%)',
          width: 12, height: 12, borderRadius: '50%',
          background: leadColor,
          border: '2px solid var(--bg-primary)',
          transition: 'left 0.4s ease',
          zIndex: 2,
        }} />
      </div>

      <div style={{ textAlign: 'center', fontSize: 12, color: 'var(--text-muted)' }}>
        {kepLabel(homeKep, homeTeam, awayTeam)}
      </div>
    </div>
  );
}


/** Suggested play lines panel (principal variations).
 *  metric: 'kep' or 'efsd' — selects which value axis to display.
 *  Lines are shown in HOME-TEAM reference:
 *    home possession → optimizing for + (higher is better)
 *    away possession → optimizing for − (lower is better)
 */
function LinesPanel({ lines, loading, homeTeam, awayTeam, posteam, metric = 'kep' }) {
  if (loading) {
    return <div style={{ color: 'var(--accent-cyan)', fontSize: 13, padding: '8px 0' }}>Computing lines…</div>;
  }
  if (!lines?.length) {
    return <div style={{ color: 'var(--text-muted)', fontSize: 12, padding: '8px 0' }}>Select a play on the timeline to see suggested lines.</div>;
  }

  const isHomePoss = posteam === homeTeam;
  const posColor   = teamColor(posteam ?? '');
  const CONCEPT_ICONS = { Run: '🏃', Screen: '↔', Short: '→', Medium: '↗', Deep: '⬆' };
  const metricLabel = metric === 'efsd' ? 'EFSD' : 'KEP';

  // Pull the right keys based on active metric.
  const deltaKey = metric === 'efsd' ? 'delta_efsd' : 'delta_kep';
  const stepsKey = metric === 'efsd' ? 'efsd_steps' : 'kep_steps';

  // Convert to home-team reference, re-sort so BEST for the possessing team is first.
  const homeRefLines = [...lines]
    .filter(line => line[deltaKey] != null && line[stepsKey] != null)
    .map(line => ({
      ...line,
      delta_home: isHomePoss ? line[deltaKey] : -line[deltaKey],
      steps_home: (line[stepsKey] ?? []).map(k => isHomePoss ? k : -k),
    }))
    .sort((a, b) => isHomePoss
      ? b.delta_home - a.delta_home
      : a.delta_home - b.delta_home
    );

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
      {/* Possession context */}
      <div style={{ fontSize: 11, color: 'var(--text-muted)', display: 'flex', alignItems: 'center', gap: 6 }}>
        <span style={{ width: 8, height: 8, borderRadius: 2, background: posColor, display: 'inline-block' }} />
        <span><strong style={{ color: 'var(--text-primary)' }}>{posteam}</strong> possesses — optimizing for <strong style={{ color: posColor }}>{isHomePoss ? '+' : '−'}</strong></span>
      </div>

      {homeRefLines.map((line, i) => {
        const isTop = i === 0;
        const d = line.delta_home;
        const dSign = d >= 0 ? '+' : '';
        const dColor = isHomePoss
          ? (d >= 0 ? teamColor(homeTeam) : teamColor(awayTeam))
          : (d <= 0 ? teamColor(awayTeam) : teamColor(homeTeam));
        return (
          <div key={i} style={{
            padding: '10px 12px', borderRadius: 6,
            background: isTop ? `${posColor}11` : 'var(--bg-tertiary)',
            border: `1px solid ${isTop ? posColor : 'var(--border-color)'}`,
          }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
              <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
                {isTop && (
                  <span style={{
                    background: posColor, color: '#000',
                    fontSize: 10, fontWeight: 700, padding: '1px 5px', borderRadius: 3,
                  }}>BEST</span>
                )}
                <span style={{ fontFamily: 'var(--font-heading)', fontSize: 13, color: 'var(--text-primary)' }}>
                  {line.concepts.map((c, ci) => (
                    <span key={ci}>
                      {ci > 0 && <span style={{ color: 'var(--text-muted)', margin: '0 4px' }}>→</span>}
                      <span style={{ color: isTop && ci === 0 ? posColor : 'var(--text-primary)' }}>
                        {CONCEPT_ICONS[c] || ''} {c}
                      </span>
                    </span>
                  ))}
                </span>
              </div>
              <span style={{ fontFamily: 'var(--font-heading)', fontWeight: 700, fontSize: 13, color: dColor }}>
                {dSign}{d.toFixed(2)}
              </span>
            </div>
            <div style={{ display: 'flex', gap: 6, marginTop: 6, alignItems: 'center', fontSize: 11, color: 'var(--text-muted)' }}>
              {line.steps_home.map((val, ki) => (
                <span key={ki} style={{ display: 'flex', alignItems: 'center', gap: 3 }}>
                  {ki > 0 && <span>→</span>}
                  <span style={{ color: val >= 0 ? teamColor(homeTeam) : teamColor(awayTeam) }}>
                    {val >= 0 ? '+' : ''}{val.toFixed(2)}
                  </span>
                </span>
              ))}
              <span style={{ marginLeft: 4 }}>home {metricLabel}</span>
              <span style={{ marginLeft: 'auto' }}>n={line.n}</span>
            </div>
          </div>
        );
      })}
    </div>
  );
}


/** Custom tooltip for KEP timeline chart. */
function KEPTooltip({ active, payload, homeTeam, awayTeam }) {
  if (!active || !payload?.[0]) return null;
  const d = payload[0].payload;
  const homeKep = d.home_kep;
  const leadTeam = homeKep >= 0 ? homeTeam : awayTeam;
  const homeColor = teamColor(homeTeam);
  const awayColor = teamColor(awayTeam);
  return (
    <div style={{
      background: 'var(--bg-secondary)', border: '1px solid var(--border-color)',
      borderRadius: 6, padding: '10px 14px', fontSize: 12, minWidth: 180,
    }}>
      {/* Running score at this play */}
      {(() => {
        const isHome = d.posteam === homeTeam;
        const hScore = isHome ? d.posteam_score : d.defteam_score;
        const aScore = isHome ? d.defteam_score : d.posteam_score;
        return (
          <div style={{ display: 'flex', justifyContent: 'center', gap: 10, marginBottom: 8, fontFamily: 'var(--font-heading)' }}>
            <span style={{ color: awayColor, fontWeight: 700 }}>{awayTeam} {aScore ?? '—'}</span>
            <span style={{ color: 'var(--text-muted)' }}>–</span>
            <span style={{ color: homeColor, fontWeight: 700 }}>{hScore ?? '—'} {homeTeam}</span>
          </div>
        );
      })()}
      <div style={{ color: 'var(--text-muted)', marginBottom: 6, borderTop: '1px solid var(--border-color)', paddingTop: 6 }}>
        {formatClock(d.game_seconds_remaining)} · {d.posteam} {d.down}&{d.ydstogo} at {d.yardline_100} yds
      </div>
      <div style={{
        fontFamily: 'var(--font-heading)', fontWeight: 700,
        color: homeKep >= 0 ? homeColor : awayColor,
      }}>
        {leadTeam} +{Math.abs(homeKep).toFixed(2)} KEP
      </div>
      <div style={{ color: 'var(--text-muted)', marginTop: 2 }}>EP: {d.ep?.toFixed(2) ?? '—'}</div>
    </div>
  );
}


// ─── main page ──────────────────────────────────────────────────────────────────

export default function HistoricalLab() {
  const [games, setGames]               = useState([]);
  const [selectedGame, setSelectedGame] = useState(null);
  const [plays, setPlays]               = useState([]);
  const [playsLoading, setPlaysLoading] = useState(false);
  const [selectedPlay, setSelectedPlay] = useState(null);
  const [lines, setLines]               = useState(null);
  const [linesLoading, setLinesLoading] = useState(false);
  const [metric, setMetric]             = useState('kep');
  const topRef = useRef(null);

  // Load game list on mount.
  useEffect(() => {
    fetch('/api/historical/week1-2025')
      .then(r => r.json())
      .then(d => {
        setGames(d.games || []);
        if (d.games?.length) setSelectedGame(d.games[0]);
      })
      .catch(() => {});
  }, []);

  // Load plays whenever selected game changes.
  useEffect(() => {
    if (!selectedGame) return;
    setPlays([]);
    setSelectedPlay(null);
    setLines(null);
    setPlaysLoading(true);
    fetch(`/api/historical/plays/${selectedGame.game_id}`)
      .then(r => r.json())
      .then(d => {
        setPlays(d.plays || []);
        // Default: last play
        if (d.plays?.length) setSelectedPlay(d.plays[d.plays.length - 1]);
      })
      .catch(() => {})
      .finally(() => setPlaysLoading(false));
  }, [selectedGame]);

  // Fetch suggested lines when selected play or metric changes.
  const fetchLines = useCallback((play, activeMetric) => {
    if (!play) return;
    setLines(null);
    setLinesLoading(true);
    const p = new URLSearchParams({
      down:               play.down,
      distance:           play.ydstogo,
      yardline_100:       play.yardline_100,
      clock:              Math.round(play.game_seconds_remaining),
      score_differential: play.score_differential,
      posteam_timeouts:   play.posteam_timeouts_remaining,
      defteam_timeouts:   play.defteam_timeouts_remaining,
      n_sims:             150,
      depth:              2,
      n_lines:            3,
      metric:             activeMetric ?? 'kep',
    });
    fetch(`/api/historical/suggest-lines?${p}`)
      .then(r => r.json())
      .then(d => setLines(d))
      .catch(() => {})
      .finally(() => setLinesLoading(false));
  }, []);

  // Fetch lines whenever selected play changes.
  useEffect(() => {
    if (selectedPlay) fetchLines(selectedPlay, metric);
  }, [selectedPlay, fetchLines]); // eslint-disable-line react-hooks/exhaustive-deps

  // Re-fetch lines when metric toggle changes (keep same play).
  useEffect(() => {
    if (selectedPlay) fetchLines(selectedPlay, metric);
  }, [metric]); // eslint-disable-line react-hooks/exhaustive-deps

  const homeTeam = selectedGame?.home_team ?? '—';
  const awayTeam = selectedGame?.away_team ?? '—';
  const homeKep  = selectedPlay?.home_kep ?? 0;

  // Derive running score from posteam/defteam scores (home_score in nfl_data_py is the FINAL score, not running).
  const isHomePoss   = selectedPlay?.posteam === homeTeam;
  const homeScoreNow = selectedPlay ? (isHomePoss ? selectedPlay.posteam_score : selectedPlay.defteam_score) : null;
  const awayScoreNow = selectedPlay ? (isHomePoss ? selectedPlay.defteam_score : selectedPlay.posteam_score) : null;

  // Select a play and scroll the eval bar + lines panel into view.
  const selectPlay = useCallback((play) => {
    setSelectedPlay(play);
    topRef.current?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }, []);

  // Chart click handler — fires from LineChart onClick (fallback).
  const handleChartClick = (data) => {
    if (data?.activePayload?.[0]?.payload) {
      selectPlay(data.activePayload[0].payload);
    }
  };

  return (
    <div style={{ padding: '24px', maxWidth: 1400, margin: '0 auto' }}>

      {/* ── Header ── */}
      <div style={{ marginBottom: 20, display: 'flex', justifyContent: 'space-between', alignItems: 'flex-end' }}>
        <div>
          <div style={{
            display: 'inline-block', background: 'rgba(249,115,22,0.15)',
            border: '1px solid var(--accent-orange)', borderRadius: 4,
            padding: '2px 8px', fontSize: 11, color: 'var(--accent-orange)',
            fontFamily: 'var(--font-heading)', marginBottom: 6,
          }}>
            ⚗ TESTING LAB — REMOVE BEFORE LAUNCH
          </div>
          <h1 style={{ fontSize: 24, fontFamily: 'var(--font-heading)', color: 'var(--text-primary)', margin: 0 }}>
            Chess Evaluator — Week 1 2025
          </h1>
          <p style={{ color: 'var(--text-muted)', fontSize: 13, margin: '4px 0 0' }}>
            Home-team KEP reference · Suggested lines (depth 2) · 16 games
          </p>
        </div>

        {/* Game selector */}
        <select
          value={selectedGame?.game_id ?? ''}
          onChange={e => {
            const g = games.find(g => g.game_id === e.target.value);
            if (g) setSelectedGame(g);
          }}
          style={{
            background: 'var(--bg-secondary)', color: 'var(--text-primary)',
            border: '1px solid var(--border-color)', borderRadius: 6,
            padding: '8px 12px', fontSize: 13, fontFamily: 'var(--font-heading)',
            cursor: 'pointer', minWidth: 260,
          }}
        >
          {games.map(g => (
            <option key={g.game_id} value={g.game_id}>
              {g.away_team} @ {g.home_team} — {g.away_score}–{g.home_score} ({g.gameday})
            </option>
          ))}
        </select>
      </div>

      {/* ── Top panels: eval bar + lines ── */}
      <div ref={topRef} style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 16, marginBottom: 20 }}>

        {/* Left: eval bar + play info */}
        <div style={{
          background: 'var(--bg-secondary)', border: '1px solid var(--border-color)',
          borderRadius: 8, padding: 20,
        }}>
          <div style={{ fontSize: 11, color: 'var(--text-muted)', fontFamily: 'var(--font-heading)', marginBottom: 14, letterSpacing: 1 }}>
            ♟ POSITIONAL EVALUATION
          </div>
          <EvalBar
            homeKep={homeKep}
            homeTeam={homeTeam}
            awayTeam={awayTeam}
            homeScore={homeScoreNow}
            awayScore={awayScoreNow}
          />

          {selectedPlay && (
            <div style={{ marginTop: 20, borderTop: '1px solid var(--border-color)', paddingTop: 16 }}>
              <div style={{ fontSize: 11, color: 'var(--text-muted)', marginBottom: 10, fontFamily: 'var(--font-heading)', letterSpacing: 1 }}>
                SELECTED PLAY
              </div>
              <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '8px 16px', fontSize: 13 }}>
                {[
                  ['Clock',      formatClock(selectedPlay.game_seconds_remaining)],
                  ['Possession', selectedPlay.posteam],
                  ['Situation',  `${selectedPlay.down}&${selectedPlay.ydstogo}`],
                  ['Field',      `${selectedPlay.yardline_100} yds out`],
                  ['EP',         selectedPlay.ep?.toFixed(3) ?? '—'],
                  ['KEP (off)',  selectedPlay.kep_off?.toFixed(3) ?? '—'],
                  ['EFSD (off)', selectedPlay.efsd_off?.toFixed(3) ?? '—'],
                  ['EFSD (home)', selectedPlay.home_efsd?.toFixed(3) ?? '—'],
                ].map(([label, val]) => (
                  <div key={label}>
                    <div style={{ color: 'var(--text-muted)', fontSize: 11 }}>{label}</div>
                    <div style={{ color: 'var(--text-primary)', fontWeight: 600 }}>{val}</div>
                  </div>
                ))}
              </div>
              <div style={{
                marginTop: 12, fontSize: 12, color: 'var(--text-muted)',
                borderLeft: '2px solid var(--border-color)', paddingLeft: 8, lineHeight: 1.5,
              }}>
                {selectedPlay.desc?.slice(0, 120)}{(selectedPlay.desc?.length ?? 0) > 120 ? '…' : ''}
              </div>
            </div>
          )}
        </div>

        {/* Right: suggested lines */}
        <div style={{
          background: 'var(--bg-secondary)', border: '1px solid var(--border-color)',
          borderRadius: 8, padding: 20,
        }}>
          <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 14 }}>
            <div style={{ fontSize: 11, color: 'var(--text-muted)', fontFamily: 'var(--font-heading)', letterSpacing: 1 }}>
              ♜ SUGGESTED LINES — ONE-STEP {metric.toUpperCase()}
            </div>
            {/* Metric toggle */}
            <div style={{ display: 'flex', gap: 4 }}>
              {['kep', 'efsd'].map(m => (
                <button
                  key={m}
                  onClick={() => setMetric(m)}
                  style={{
                    padding: '3px 10px', borderRadius: 4, fontSize: 11,
                    fontFamily: 'var(--font-heading)', fontWeight: 700,
                    cursor: 'pointer', border: '1px solid',
                    borderColor: metric === m ? 'var(--accent-cyan)' : 'var(--border-color)',
                    background: metric === m ? 'rgba(0,242,254,0.12)' : 'var(--bg-tertiary)',
                    color: metric === m ? 'var(--accent-cyan)' : 'var(--text-muted)',
                    transition: 'all 0.15s',
                  }}
                >
                  {m.toUpperCase()}
                </button>
              ))}
            </div>
          </div>
          <LinesPanel
            lines={lines?.lines}
            loading={linesLoading}
            homeTeam={homeTeam}
            awayTeam={awayTeam}
            posteam={selectedPlay?.posteam}
            metric={metric}
          />
        </div>
      </div>

      {/* ── KEP Timeline chart ── */}
      <div style={{
        background: 'var(--bg-secondary)', border: '1px solid var(--border-color)',
        borderRadius: 8, padding: 20, marginBottom: 16,
      }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 16 }}>
          <div style={{ fontSize: 11, color: 'var(--text-muted)', fontFamily: 'var(--font-heading)', letterSpacing: 1 }}>
            📈 KEP GAME TIMELINE — HOME-TEAM REFERENCE
          </div>
          <div style={{ fontSize: 12, color: 'var(--text-muted)' }}>
            {awayTeam} @ {homeTeam}
            {selectedGame && ` — ${selectedGame.away_score}–${selectedGame.home_score} final`}
          </div>
        </div>

        {playsLoading ? (
          <div style={{ textAlign: 'center', padding: 60, color: 'var(--accent-cyan)' }}>Loading plays…</div>
        ) : plays.length === 0 ? (
          <div style={{ textAlign: 'center', padding: 60, color: 'var(--text-muted)' }}>No data</div>
        ) : (
          <ResponsiveContainer width="100%" height={240}>
            <LineChart data={plays} margin={{ top: 8, right: 24, bottom: 8, left: 8 }} onClick={handleChartClick} style={{ cursor: 'pointer' }}>
              <CartesianGrid strokeDasharray="3 3" stroke="rgba(255,255,255,0.05)" />
              <XAxis
                dataKey="play_id"
                tick={false}
                axisLine={{ stroke: 'var(--border-color)' }}
                tickLine={false}
              />
              <YAxis
                domain={[-24, 24]}
                ticks={[-24, -18, -12, -6, 0, 6, 12, 18, 24]}
                tick={{ fill: 'var(--text-muted)', fontSize: 11 }}
                axisLine={{ stroke: 'var(--border-color)' }}
                tickLine={false}
                tickFormatter={v => v === 0 ? 'EVEN' : (v > 0 ? `+${v}` : v)}
                label={{ value: `↑ ${homeTeam}  KEP  ${awayTeam} ↓`, angle: -90, position: 'insideLeft', fill: 'var(--text-muted)', fontSize: 10 }}
              />
              <Tooltip content={<KEPTooltip homeTeam={homeTeam} awayTeam={awayTeam} />} />
              <ReferenceLine y={0} stroke="var(--border-color)" strokeDasharray="4 4" />
              {selectedPlay && (
                <ReferenceLine x={selectedPlay.play_id} stroke="rgba(255,255,255,0.3)" strokeDasharray="3 3" />
              )}
              <Line
                type="monotone"
                dataKey="home_kep"
                stroke="var(--accent-cyan)"
                strokeWidth={2}
                dot={(props) => {
                  const isSelected = props.payload?.play_id === selectedPlay?.play_id;
                  return (
                    <circle
                      key={props.key}
                      cx={props.cx} cy={props.cy}
                      r={isSelected ? 6 : 2.5}
                      fill={isSelected ? 'var(--accent-cyan)' : 'rgba(0,242,254,0.35)'}
                      stroke={isSelected ? 'var(--bg-primary)' : 'none'}
                      strokeWidth={isSelected ? 2 : 0}
                      style={{ cursor: 'pointer' }}
                      onClick={() => selectPlay(props.payload)}
                    />
                  );
                }}
                activeDot={{ r: 7, fill: 'var(--accent-cyan)', stroke: 'var(--bg-primary)', strokeWidth: 2, cursor: 'pointer', onClick: (_, payload) => selectPlay(payload.payload) }}
                name={`${homeTeam} KEP advantage`}
              />
            </LineChart>
          </ResponsiveContainer>
        )}

        <div style={{ fontSize: 11, color: 'var(--text-muted)', marginTop: 8 }}>
          KEP is bounded ±24 pts — inverted from the WP model. Click any point to load suggested lines.
          Positive = {homeTeam} leads positionally. Negative = {awayTeam}.
        </div>
      </div>

      {/* ── EFSD Timeline chart ── */}
      <div style={{
        background: 'var(--bg-secondary)', border: '1px solid var(--border-color)',
        borderRadius: 8, padding: 20,
      }}>
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 16 }}>
          <div style={{ fontSize: 11, color: 'var(--text-muted)', fontFamily: 'var(--font-heading)', letterSpacing: 1 }}>
            📊 EFSD GAME TIMELINE — HOME-TEAM REFERENCE
          </div>
          <div style={{ fontSize: 12, color: 'var(--text-muted)' }}>
            Expected Final Score Differential · unbounded axis
          </div>
        </div>

        {playsLoading ? (
          <div style={{ textAlign: 'center', padding: 60, color: 'var(--accent-cyan)' }}>Loading plays…</div>
        ) : plays.length === 0 ? (
          <div style={{ textAlign: 'center', padding: 60, color: 'var(--text-muted)' }}>No data</div>
        ) : (
          <ResponsiveContainer width="100%" height={240}>
            <LineChart data={plays} margin={{ top: 8, right: 24, bottom: 8, left: 8 }} onClick={handleChartClick} style={{ cursor: 'pointer' }}>
              <CartesianGrid strokeDasharray="3 3" stroke="rgba(255,255,255,0.05)" />
              <XAxis
                dataKey="play_id"
                tick={false}
                axisLine={{ stroke: 'var(--border-color)' }}
                tickLine={false}
                label={{ value: '← Game Progression →', position: 'insideBottom', offset: 4, fill: 'var(--text-muted)', fontSize: 11 }}
              />
              <YAxis
                tick={{ fill: 'var(--text-muted)', fontSize: 11 }}
                axisLine={{ stroke: 'var(--border-color)' }}
                tickLine={false}
                tickFormatter={v => v === 0 ? 'EVEN' : (v > 0 ? `+${v}` : v)}
                label={{ value: `↑ ${homeTeam}  EFSD  ${awayTeam} ↓`, angle: -90, position: 'insideLeft', fill: 'var(--text-muted)', fontSize: 10 }}
              />
              <Tooltip
                content={({ active, payload }) => {
                  if (!active || !payload?.[0]) return null;
                  const d = payload[0].payload;
                  const homeEfsd = d.home_efsd;
                  const leadTeam = (homeEfsd ?? 0) >= 0 ? homeTeam : awayTeam;
                  const homeColor = teamColor(homeTeam);
                  const awayColor = teamColor(awayTeam);
                  return (
                    <div style={{ background: 'var(--bg-secondary)', border: '1px solid var(--border-color)', borderRadius: 6, padding: '10px 14px', fontSize: 12, minWidth: 180 }}>
                      <div style={{ color: 'var(--text-muted)', marginBottom: 6 }}>
                        {formatClock(d.game_seconds_remaining)} · {d.posteam} {d.down}&{d.ydstogo} at {d.yardline_100} yds
                      </div>
                      <div style={{ fontFamily: 'var(--font-heading)', fontWeight: 700, color: (homeEfsd ?? 0) >= 0 ? homeColor : awayColor }}>
                        {leadTeam} +{Math.abs(homeEfsd ?? 0).toFixed(2)} EFSD
                      </div>
                      <div style={{ color: 'var(--text-muted)', marginTop: 2 }}>KEP: {d.home_kep?.toFixed(2) ?? '—'}</div>
                    </div>
                  );
                }}
              />
              <ReferenceLine y={0} stroke="var(--border-color)" strokeDasharray="4 4" />
              {selectedPlay && (
                <ReferenceLine x={selectedPlay.play_id} stroke="rgba(255,255,255,0.3)" strokeDasharray="3 3" />
              )}
              <Line
                type="monotone"
                dataKey="home_efsd"
                stroke="#a78bfa"
                strokeWidth={2}
                dot={(props) => {
                  const isSelected = props.payload?.play_id === selectedPlay?.play_id;
                  return (
                    <circle
                      key={props.key}
                      cx={props.cx} cy={props.cy}
                      r={isSelected ? 6 : 2.5}
                      fill={isSelected ? '#a78bfa' : 'rgba(167,139,250,0.35)'}
                      stroke={isSelected ? 'var(--bg-primary)' : 'none'}
                      strokeWidth={isSelected ? 2 : 0}
                      style={{ cursor: 'pointer' }}
                      onClick={() => selectPlay(props.payload)}
                    />
                  );
                }}
                activeDot={{ r: 7, fill: '#a78bfa', stroke: 'var(--bg-primary)', strokeWidth: 2, cursor: 'pointer', onClick: (_, payload) => selectPlay(payload.payload) }}
                name={`${homeTeam} EFSD`}
              />
            </LineChart>
          </ResponsiveContainer>
        )}

        <div style={{ fontSize: 11, color: 'var(--text-muted)', marginTop: 8 }}>
          EFSD is unbounded — directly predicts the expected final score margin. Unlike KEP, it can
          exceed ±24 in blowouts. Both charts share the same x-axis and play selection.
        </div>
      </div>

    </div>
  );
}
