import { useState, useEffect, useCallback, useRef } from 'react';
import { ApiService } from '../api';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, BarChart, Bar, Cell } from 'recharts';
import { Play, TrendingUp, Info, Activity, ShieldAlert, Cpu, ArrowLeft, Layers } from 'lucide-react';
import { teamColor } from '../teamColors';

// Minutes elapsed since kickoff for a play-by-play or positional-eval row.
// Prefers the backend's game_seconds_remaining (regulation kickoff = 3600);
// falls back to deriving it from qtr + clock text for older/mock records.
function elapsedMinutesFor(play) {
  if (play.game_seconds_remaining != null) {
    return (3600 - play.game_seconds_remaining) / 60;
  }
  const [mm, ss] = (play.time || play.clock || '15:00').split(':').map(Number);
  const secsRemainingInQtr = (mm || 0) * 60 + (ss || 0);
  const qtr = play.qtr || 1;
  return ((qtr - 1) * 15 * 60 + (15 * 60 - secsRemainingInQtr)) / 60;
}

// Builds <linearGradient> stop offsets (0-1, fractions of the LINE'S OWN
// t-range -- matching SVG's default objectBoundingBox units) that color a
// single Line's stroke by which side of `threshold` each point falls on,
// with a hard transition exactly at each crossing.
//
// This exists instead of rendering one <Line> per color segment because
// Recharts' click/hover hit-testing (activePayload) only works reliably
// against ONE chart-level `data` array shared by every series -- splitting
// into multiple <Line data={partial}> segments (each with a different
// partial dataset) silently breaks onClick, since Recharts can no longer
// map a mouse x-position to a single canonical data index. Painting one
// <Line data={fullData}> with a gradient stroke keeps click/hover intact
// while still getting the color-by-leader look.
function buildGradientStops(data, key, threshold, aboveColor, belowColor) {
  if (!data.length) return [{ offset: 0, color: aboveColor }, { offset: 1, color: aboveColor }];
  const tMin = data[0].t;
  const tMax = data[data.length - 1].t;
  const span = (tMax - tMin) || 1;
  const colorFor = (above) => (above ? aboveColor : belowColor);

  const stops = [{ offset: 0, color: colorFor(data[0][key] >= threshold) }];
  for (let i = 1; i < data.length; i++) {
    const prev = data[i - 1];
    const curr = data[i];
    const prevAbove = prev[key] >= threshold;
    const currAbove = curr[key] >= threshold;
    if (prevAbove !== currAbove && curr[key] !== prev[key]) {
      const frac = (threshold - prev[key]) / (curr[key] - prev[key]);
      const crossT = prev.t + frac * (curr.t - prev.t);
      const offset = Math.max(0, Math.min(1, (crossT - tMin) / span));
      stops.push({ offset, color: colorFor(prevAbove) });
      stops.push({ offset, color: colorFor(currAbove) });
    }
  }
  stops.push({ offset: 1, color: colorFor(data[data.length - 1][key] >= threshold) });
  return stops;
}

// Lichess/chess.com-style evaluation bar. Always home-perspective (home
// fills from the leading edge -- right in horizontal, top in vertical --
// away from the trailing edge) -- never flips sign on a possession change,
// so it reads as "board state," not "whoever has the ball."
//
// mode: 'points' (EFSD -- signed, symmetric around 0, needs a clamp `cap`)
//    or 'probability' (WP -- already 0-100, home value IS the fill percent).
// orientation: 'horizontal' (default) or 'vertical'.
function EvalBar({ homeVal, homeTeam, awayTeam, homeColor, awayColor, cap, label, mode = 'points', orientation = 'horizontal' }) {
  let homePct, leadTeam, leadColor, valueText;
  if (mode === 'probability') {
    const h = Math.max(0, Math.min(100, homeVal ?? 50));
    const a = 100 - h;
    homePct = h;
    leadTeam = h >= a ? homeTeam : awayTeam;
    leadColor = h >= a ? homeColor : awayColor;
    valueText = `${Math.max(h, a).toFixed(1)}% ${leadTeam}`;
  } else {
    const val = homeVal ?? 0;
    const clamped = Math.max(-cap, Math.min(cap, val));
    homePct = ((clamped + cap) / (2 * cap)) * 100;
    leadTeam = val >= 0 ? homeTeam : awayTeam;
    leadColor = val >= 0 ? homeColor : awayColor;
    valueText = `+${Math.abs(val).toFixed(1)} ${leadTeam}`;
  }

  if (orientation === 'vertical') {
    return (
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 6, height: '100%', width: 44 }}>
        <span style={{ fontSize: 10, fontWeight: 800, color: homeColor }}>{homeTeam}</span>
        <div style={{ position: 'relative', flex: 1, width: 18, borderRadius: 4, background: 'var(--bg-tertiary)', overflow: 'hidden', border: '1px solid var(--border-color)' }}>
          <div style={{ position: 'absolute', top: 0, left: 0, width: '100%', height: `${homePct}%`, background: homeColor, transition: 'height 0.4s ease' }} />
          <div style={{ position: 'absolute', bottom: 0, left: 0, width: '100%', height: `${100 - homePct}%`, background: awayColor, transition: 'height 0.4s ease' }} />
          <div style={{ position: 'absolute', left: 0, top: '50%', width: '100%', height: 1, background: 'rgba(255,255,255,0.3)' }} />
        </div>
        <span style={{ fontSize: 10, fontWeight: 800, color: awayColor }}>{awayTeam}</span>
        <span style={{ fontSize: 10, fontWeight: 700, color: leadColor, textAlign: 'center', lineHeight: 1.3 }}>{valueText}</span>
        {label && <span style={{ fontSize: 9, color: 'var(--text-muted)', textAlign: 'center' }}>{label}</span>}
      </div>
    );
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', fontSize: 12, fontWeight: 700 }}>
        <span style={{ color: awayColor }}>{awayTeam}</span>
        <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 1 }}>
          <span style={{ fontFamily: 'var(--font-heading)', fontWeight: 800, fontSize: 15, color: leadColor }}>
            {valueText}
          </span>
          {label && <span style={{ fontSize: 10, fontWeight: 400, color: 'var(--text-muted)' }}>{label}</span>}
        </div>
        <span style={{ color: homeColor }}>{homeTeam}</span>
      </div>
      <div style={{ position: 'relative', height: 16, borderRadius: 4, background: 'var(--bg-tertiary)', overflow: 'hidden', border: '1px solid var(--border-color)' }}>
        <div style={{ position: 'absolute', top: 0, left: 0, height: '100%', width: `${100 - homePct}%`, background: awayColor, transition: 'width 0.4s ease' }} />
        <div style={{ position: 'absolute', top: 0, right: 0, height: '100%', width: `${homePct}%`, background: homeColor, transition: 'width 0.4s ease' }} />
        <div style={{ position: 'absolute', top: 0, left: '50%', height: '100%', width: 1, background: 'rgba(255,255,255,0.3)' }} />
      </div>
    </div>
  );
}

function GameSummary({ gameId, gameDate, navigateTo }) {
  const [games, setGames] = useState([]);
  const [playByPlay, setPlayByPlay] = useState([]);
  const [selectedPlayId, setSelectedPlayId] = useState(null);
  const [fourthDowns, setFourthDowns] = useState([]);
  const [stats, setStats] = useState(null);
  const [playerStats, setPlayerStats] = useState(null);
  const [chessEval, setChessEval] = useState(null);
  const [conceptResult, setConceptResult] = useState(null);
  const [conceptLoading, setConceptLoading] = useState(false);

  const [activeTab, setActiveTab] = useState('center'); // 'center', 'fourth', 'chess'
  const [loading, setLoading] = useState(true);

  // Tracked so a background auto-refresh can tell whether the user was
  // "following live" (keep tracking the new latest play after a refresh) or
  // had pinned an earlier play to inspect (leave their selection alone --
  // a poll shouldn't yank them away from what they're looking at).
  const playByPlayRef = useRef([]);
  const selectedPlayIdRef = useRef(null);
  useEffect(() => { playByPlayRef.current = playByPlay; }, [playByPlay]);
  useEffect(() => { selectedPlayIdRef.current = selectedPlayId; }, [selectedPlayId]);

  const loadAllGameDetails = useCallback(async (isBackgroundRefresh = false) => {
    if (!gameId) return;
    if (!isBackgroundRefresh) setLoading(true);
    try {
      const [gameList, plays, fourths, gameStats, playerBoxScore, evaluator] = await Promise.all([
        ApiService.getLiveGames(gameDate || undefined),
        ApiService.getPlayByPlay(gameId),
        ApiService.getFourthDowns(gameId),
        ApiService.getGameStats(gameId),
        ApiService.getPlayerStats(gameId),
        ApiService.getChessEvaluator(gameId)
      ]);

      setGames(gameList || []);
      setFourthDowns(fourths || []);
      setStats(gameStats);
      setPlayerStats(playerBoxScore);
      setChessEval(evaluator);

      const prevPlays = playByPlayRef.current;
      const wasFollowingLive = !isBackgroundRefresh
        || prevPlays.length === 0
        || selectedPlayIdRef.current === prevPlays[prevPlays.length - 1]?.play_id;
      setPlayByPlay(plays || []);
      if (plays && plays.length > 0 && wasFollowingLive) {
        setSelectedPlayId(plays[plays.length - 1].play_id);
      }
    } catch (err) {
      console.error('Failed to load game summary metrics', err);
    } finally {
      if (!isBackgroundRefresh) setLoading(false);
    }
  }, [gameId, gameDate]);

  useEffect(() => {
    loadAllGameDetails(false);
  }, [loadAllGameDetails]);

  // Fetch concept recommendations from the slider API when a chess play is selected
  useEffect(() => {
    if (activeTab !== 'chess' || !chessEval) return;
    const evals = chessEval.evaluations;
    if (!evals?.length) return;
    const ev = evals.find(e => String(e.play_id) === String(selectedPlayId)) ?? evals[evals.length - 1];
    if (!ev) return;

    const [mm, ss] = ev.clock.split(':').map(Number);
    const gameSec = (4 - ev.qtr) * 900 + mm * 60 + ss;

    const params = new URLSearchParams({
      down: ev.down, distance: ev.ydstogo, yardline_100: ev.yardline_100,
      clock: gameSec, score_differential: ev.score_differential,
      posteam_timeouts: 3, defteam_timeouts: 3, n_sims: 300,
    });

    const controller = new AbortController();
    setConceptLoading(true);
    setConceptResult(null);
    fetch(`/api/positional-evaluator?${params}`, { signal: controller.signal })
      .then(r => r.ok ? r.json() : Promise.reject())
      .then(data => { setConceptResult(data); setConceptLoading(false); })
      .catch(err => { if (err.name !== 'AbortError') setConceptLoading(false); });

    return () => controller.abort();
  }, [selectedPlayId, activeTab, chessEval]);

  const activeGame = games.find(g => g.game_id === gameId);
  const activePlay = playByPlay.find(p => p.play_id === selectedPlayId);
  const homeColor = teamColor(activeGame?.home_team);
  const awayColor = teamColor(activeGame?.away_team);

  // Auto-refresh every 30s, but ONLY while the game is actually in progress --
  // never for Pregame (nothing to refresh) or Final (nothing changes). The
  // effect re-evaluates isGameInProgress after every load, so polling starts
  // the moment a pregame game goes live and stops the instant it ends.
  const isGameInProgress = !!activeGame
    && activeGame.time_remaining
    && activeGame.time_remaining !== 'Pregame'
    && activeGame.time_remaining !== 'Final';

  useEffect(() => {
    if (!isGameInProgress) return;
    const intervalId = setInterval(() => loadAllGameDetails(true), 30000);
    return () => clearInterval(intervalId);
  }, [isGameInProgress, loadAllGameDetails]);

  // WP graph: x-axis is real elapsed game time (minutes since kickoff), not
  // play sequence, so bunched hurry-up plays and long clock stoppages read
  // at their true pace. One line (home WP) -- away WP is always 100 - home,
  // so a second line would be pure redundancy.
  const chartData = playByPlay.map((play) => ({
    t: Math.round(elapsedMinutesFor(play) * 10) / 10,
    wp: play.home_wp,
    play_id: play.play_id,
    desc: play.desc,
  }));
  const lastElapsed = chartData.length ? chartData[chartData.length - 1].t : 0;
  const xDomainMax = Math.max(60, Math.ceil(lastElapsed / 5) * 5); // reserve full-game width; extends for OT
  const latestPlay = playByPlay[playByPlay.length - 1];
  // selectedPlayId defaults to the latest play on load (see loadAllGameDetails
  // below), so this reads as "live" until the user clicks an earlier point on
  // the graph or a card in the Play Log -- same selection state both drive.
  const selectedWpPoint = playByPlay.find(p => p.play_id === selectedPlayId) || latestPlay;
  const isLatestWpSelected = !selectedWpPoint || !latestPlay || selectedWpPoint.play_id === latestPlay.play_id;

  // EFSD chart data (expected final score differential) over elapsed game time,
  // reframed to home-team perspective -- same convention as the WP graph.
  // Hoisted out of the Chess tab so both it and the Game Center tab's vertical
  // bar preview can read the same current/selected EFSD value.
  const chessEvals = chessEval?.evaluations || [];
  const chessHomeTeam = chessEval?.home_team;
  const chessChartData = chessEvals.map((ev) => ({
    t: Math.round(elapsedMinutesFor(ev) * 10) / 10,
    homeEfsd: ev.off === chessHomeTeam ? ev.efsd : -ev.efsd,
    play_id: ev.play_id,
    label: `Q${ev.qtr} ${ev.clock} | ${ev.off} ${ev.down}&${ev.ydstogo}`,
  }));
  const latestChessPoint = chessChartData[chessChartData.length - 1];
  const chessLastElapsed = chessChartData.length ? chessChartData[chessChartData.length - 1].t : 0;
  const chessXDomainMax = Math.max(60, Math.ceil(chessLastElapsed / 5) * 5);
  const efsdCap = Math.max(24, ...chessChartData.map(d => Math.ceil(Math.abs(d.homeEfsd))), 0);
  const selectedEval = chessEvals.find(e => String(e.play_id) === String(selectedPlayId))
    || chessEvals[chessEvals.length - 1]
    || null;
  const selectedChessPoint = chessChartData.find(d => String(d.play_id) === String(selectedPlayId))
    || latestChessPoint;
  const isLatestChessSelected = !selectedChessPoint || !latestChessPoint
    || selectedChessPoint.play_id === latestChessPoint.play_id;

  const getRecommendationBadge = (rec) => {
    if (!rec) return null;
    if (rec.includes('GO')) return <span className="badge badge-green">👉 {rec}</span>;
    if (rec.includes('PUNT')) return <span className="badge badge-cyan">🏈 {rec}</span>;
    return <span className="badge badge-orange">🎯 {rec}</span>;
  };

  return (
    <div>
      {/* Return button */}
      <button 
        className="btn btn-outline" 
        onClick={() => navigateTo('home')}
        style={{ marginBottom: '16px', fontSize: '12px', padding: '6px 12px' }}
      >
        <ArrowLeft size={14} style={{ marginRight: '4px' }} /> Return to Dashboard
      </button>

      {loading || !activeGame ? (
        <div style={{ textAlign: 'center', padding: '40px', color: 'var(--accent-cyan)' }}>SYNCING GAME WORKSPACE STATE...</div>
      ) : (
        <div style={{ display: 'flex', flexDirection: 'column', gap: '20px' }}>
          
          {/* Sticky Status Banner */}
          <div className="panel" style={{ 
            background: 'linear-gradient(90deg, var(--bg-secondary) 0%, rgba(0, 242, 254, 0.02) 100%)',
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
            padding: '16px 24px',
            borderLeft: '4px solid var(--accent-cyan)'
          }}>
            <div style={{ display: 'flex', alignItems: 'center', gap: '24px' }}>
              <div style={{ display: 'flex', gap: '16px', alignItems: 'center' }}>
                <span style={{ fontSize: '20px', fontWeight: '800', color: activeGame.possession === activeGame.away_team ? 'var(--accent-cyan)' : 'inherit' }}>
                  {activeGame.away_team} {activeGame.away_score}
                </span>
                <span style={{ color: 'var(--text-muted)' }}>@</span>
                <span style={{ fontSize: '20px', fontWeight: '800', color: activeGame.possession === activeGame.home_team ? 'var(--accent-cyan)' : 'inherit' }}>
                  {activeGame.home_team} {activeGame.home_score}
                </span>
              </div>
              <div className="badge badge-cyan" style={{ fontSize: '10px' }}>
                Q{activeGame.quarter} | {activeGame.time_remaining}
              </div>
            </div>

            <div style={{ display: 'flex', gap: '20px', fontSize: '13px' }}>
              {activeGame.down && (
                <div style={{ backgroundColor: 'var(--bg-tertiary)', padding: '6px 12px', borderRadius: '4px' }}>
                  State: <strong style={{ color: 'var(--accent-orange)' }}>{activeGame.down}{activeGame.down === 1 ? 'st' : activeGame.down === 2 ? 'nd' : activeGame.down === 3 ? 'rd' : 'th'} & {activeGame.distance}</strong> at {activeGame.yardline}
                </div>
              )}
              <div style={{ backgroundColor: 'var(--bg-tertiary)', padding: '6px 12px', borderRadius: '4px' }}>
                Leverage Index: <strong style={{ color: 'var(--accent-cyan)' }}>{activeGame.leverage}</strong>
              </div>
            </div>
          </div>

          {/* Workspace Menu Tabs */}
          <div className="tabs-container" style={{ margin: '0' }}>
            <button className={`tab-btn ${activeTab === 'center' ? 'active' : ''}`} onClick={() => setActiveTab('center')}>
              <TrendingUp size={14} style={{ marginRight: '6px', display: 'inline' }} /> Game Center (WP & Stats)
            </button>
            <button className={`tab-btn ${activeTab === 'fourth' ? 'active' : ''}`} onClick={() => setActiveTab('fourth')}>
              <ShieldAlert size={14} style={{ marginRight: '6px', display: 'inline' }} /> 4th Down Decisions
            </button>
            <button className={`tab-btn ${activeTab === 'chess' ? 'active' : ''}`} onClick={() => setActiveTab('chess')}>
              <Cpu size={14} style={{ marginRight: '6px', display: 'inline' }} /> Chess Tactical Evaluator
            </button>
          </div>

          {/* Tab 1: Game Center */}
          {activeTab === 'center' && (
            <>
            <div className="dashboard-grid" style={{ margin: '0' }}>
              {/* Vertical eval bar + Left Column: Recharts and Stats, side by side */}
              <div style={{ display: 'flex', gap: '16px' }}>
                {selectedWpPoint && (
                  <EvalBar
                    homeVal={selectedWpPoint.home_wp}
                    homeTeam={activeGame.home_team}
                    awayTeam={activeGame.away_team}
                    homeColor={homeColor}
                    awayColor={awayColor}
                    mode="probability"
                    orientation="vertical"
                    label={isLatestWpSelected ? 'live' : `Q${selectedWpPoint.qtr} ${selectedWpPoint.time}`}
                  />
                )}
                {/* Preview: EFSD vertical bar next to WP, for side-by-side comparison */}
                {selectedChessPoint && (
                  <EvalBar
                    homeVal={selectedChessPoint.homeEfsd}
                    homeTeam={activeGame.home_team}
                    awayTeam={activeGame.away_team}
                    homeColor={homeColor}
                    awayColor={awayColor}
                    cap={efsdCap}
                    mode="points"
                    orientation="vertical"
                    label={isLatestChessSelected ? 'live' : `Q${selectedEval?.qtr} ${selectedEval?.clock}`}
                  />
                )}
              <div style={{ display: 'flex', flexDirection: 'column', gap: '20px', flex: 1, minWidth: 0 }}>
                {/* Line Chart */}
                <div className="panel" style={{ flex: 1, minHeight: '340px' }}>
                  <div className="panel-header" style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '8px' }}>
                    <span className="panel-title"><TrendingUp size={16} /> Win Expectancy Graph</span>
                    {selectedWpPoint && (
                      <div style={{ display: 'flex', alignItems: 'center', gap: '10px', fontFamily: 'var(--font-heading)', fontWeight: 800, fontSize: '16px' }}>
                        <span style={{ color: awayColor }}>{activeGame.away_team} {selectedWpPoint.away_wp.toFixed(1)}%</span>
                        <span style={{ color: 'var(--text-muted)', fontWeight: 400, fontSize: '12px' }}>
                          {isLatestWpSelected ? 'live WP' : `Q${selectedWpPoint.qtr} ${selectedWpPoint.time}`}
                        </span>
                        <span style={{ color: homeColor }}>{activeGame.home_team} {selectedWpPoint.home_wp.toFixed(1)}%</span>
                      </div>
                    )}
                  </div>

                  <div style={{ height: '240px', width: '100%', marginTop: '12px', position: 'relative' }}>
                    <span style={{ position: 'absolute', top: 4, left: 4, fontSize: '10px', fontWeight: 800, color: homeColor, zIndex: 1 }}>{activeGame.home_team}</span>
                    <span style={{ position: 'absolute', bottom: 20, left: 4, fontSize: '10px', fontWeight: 800, color: awayColor, zIndex: 1 }}>{activeGame.away_team}</span>
                    <ResponsiveContainer width="100%" height="100%">
                      <LineChart
                        data={chartData}
                        margin={{ top: 10, right: 10, left: -20, bottom: 0 }}
                        onClick={(data) => {
                          const pid = data?.activePayload?.[0]?.payload?.play_id;
                          if (pid != null) setSelectedPlayId(pid);
                        }}
                        style={{ cursor: 'pointer' }}
                      >
                        <defs>
                          <linearGradient id="wpLineGradient" x1="0" y1="0" x2="1" y2="0">
                            {buildGradientStops(chartData, 'wp', 50, homeColor, awayColor).map((s, i) => (
                              <stop key={i} offset={s.offset} stopColor={s.color} />
                            ))}
                          </linearGradient>
                        </defs>
                        <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                        <XAxis
                          type="number" dataKey="t" domain={[0, xDomainMax]}
                          ticks={[0, 15, 30, 45, 60]}
                          tickFormatter={(v) => v === 0 ? 'Kick' : v === 60 ? 'End' : `Q${Math.floor(v / 15) + 1}`}
                          stroke="var(--text-muted)" fontSize={11}
                        />
                        {/* Ticks relabeled as advantage magnitude (100/75/50/75/100) so both
                            ends of the axis read as "100% for whichever team is on that side",
                            with the two corner badges above showing which side is which. */}
                        <YAxis
                          domain={[0, 100]} ticks={[0, 25, 50, 75, 100]}
                          tickFormatter={(v) => Math.round(Math.abs(v - 50) + 50)}
                          stroke="var(--text-muted)" fontSize={11}
                        />
                        <Tooltip
                          contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)' }}
                          labelFormatter={(t) => `Q${Math.min(4, Math.floor(t / 15) + 1)} — ${Math.round(t)} min elapsed`}
                          formatter={(value) => [
                            `${activeGame.home_team} ${value.toFixed(1)}% / ${activeGame.away_team} ${(100 - value).toFixed(1)}%`,
                            'Win Probability',
                          ]}
                        />
                        <ReferenceLine y={50} stroke="var(--text-muted)" strokeDasharray="3 3" />
                        <Line
                          type="monotone" dataKey="wp" stroke="url(#wpLineGradient)" strokeWidth={2}
                          dot={{ r: 2, cursor: 'pointer' }} activeDot={{ r: 5 }}
                          isAnimationActive={false}
                        />
                      </LineChart>
                    </ResponsiveContainer>
                  </div>
                  <div style={{ fontSize: '11px', color: 'var(--text-muted)', marginTop: '4px' }}>
                    {!isLatestWpSelected && selectedWpPoint ? (
                      <>
                        Selected: {selectedWpPoint.desc}{' '}
                        <a href="#" onClick={(e) => { e.preventDefault(); setSelectedPlayId(latestPlay?.play_id); }} style={{ color: 'var(--accent-cyan)' }}>
                          back to live
                        </a>
                      </>
                    ) : (
                      <>Click a point on the graph (or a play in the log) to see WP at that moment. Line color follows whoever's favored.</>
                    )}
                  </div>
                </div>

                {/* Team Stats */}
                {stats && (
                  <div className="panel">
                    <div className="panel-header">
                      <span className="panel-title"><Layers size={16} /> Team Matchup Stats</span>
                    </div>
                    <table className="tactical-table">
                      <thead>
                        <tr>
                          <th>Stat Category</th>
                          <th>{stats.away.team} (Away)</th>
                          <th>{stats.home.team} (Home)</th>
                        </tr>
                      </thead>
                      <tbody>
                        <tr>
                          <td>First Downs</td>
                          <td>{stats.away.first_downs}</td>
                          <td>{stats.home.first_downs}</td>
                        </tr>
                        <tr>
                          <td>Total Yards</td>
                          <td style={{ fontWeight: '600' }}>{stats.away.total_yds}</td>
                          <td style={{ fontWeight: '600' }}>{stats.home.total_yds}</td>
                        </tr>
                        <tr>
                          <td>Passing Yards</td>
                          <td>{stats.away.pass_yds}</td>
                          <td>{stats.home.pass_yds}</td>
                        </tr>
                        <tr>
                          <td>Rushing Yards</td>
                          <td>{stats.away.rush_yds}</td>
                          <td>{stats.home.rush_yds}</td>
                        </tr>
                        <tr>
                          <td>Turnovers</td>
                          <td style={{ color: stats.away.turnovers > 0 ? 'var(--accent-orange)' : 'inherit' }}>{stats.away.turnovers}</td>
                          <td style={{ color: stats.home.turnovers > 0 ? 'var(--accent-orange)' : 'inherit' }}>{stats.home.turnovers}</td>
                        </tr>
                        <tr>
                          <td>EPA per Play</td>
                          <td style={{ color: stats.away.epa_play > 0 ? 'var(--accent-cyan)' : 'inherit' }}>{stats.away.epa_play}</td>
                          <td style={{ color: stats.home.epa_play > 0 ? 'var(--accent-cyan)' : 'inherit' }}>{stats.home.epa_play}</td>
                        </tr>
                      </tbody>
                    </table>
                  </div>
                )}
              </div>
              </div>

              {/* Right Column: Interactive Play Feed */}
              <div className="panel feed-container" style={{ maxHeight: '550px' }}>
                <div className="panel-header">
                  <span className="panel-title"><Info size={16} /> Play Log</span>
                </div>
                <div style={{ marginTop: '12px' }}>
                  {playByPlay.map(play => (
                    <div 
                      key={play.play_id}
                      className={`play-card ${selectedPlayId === play.play_id ? 'active' : ''}`}
                      onClick={() => setSelectedPlayId(play.play_id)}
                    >
                      <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', color: 'var(--text-muted)', marginBottom: '4px' }}>
                        <span>Q{play.qtr} | {play.time}</span>
                        <span style={{ color: play.possession === activeGame.away_team ? 'var(--accent-cyan)' : 'inherit', fontWeight: '700' }}>{play.possession} Ball</span>
                      </div>
                      <div style={{ fontSize: '12px', lineHeight: '1.4' }}>{play.desc}</div>
                    </div>
                  ))}
                </div>
              </div>
            </div>

            {/* Individual Stats -- full width, straight from ESPN's boxscore.players */}
            {playerStats && (
              <div className="panel" style={{ marginTop: '20px' }}>
                <div className="panel-header">
                  <span className="panel-title"><Layers size={16} /> Individual Stats</span>
                </div>
                <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '24px', marginTop: '12px' }}>
                  {['away', 'home'].map(side => {
                    const teamData = playerStats[side];
                    if (!teamData) return null;
                    return (
                      <div key={side}>
                        <div style={{ fontWeight: 700, fontSize: '14px', marginBottom: '10px', color: 'var(--text-primary)' }}>{teamData.team}</div>
                        {['passing', 'rushing', 'receiving'].map(cat => {
                          const catData = teamData.categories[cat];
                          if (!catData || !catData.rows.length) return null;
                          return (
                            <div key={cat} style={{ marginBottom: '16px', overflowX: 'auto' }}>
                              <div style={{ fontSize: '11px', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.05em', marginBottom: '4px' }}>{cat}</div>
                              <table className="tactical-table">
                                <thead>
                                  <tr>
                                    <th>Player</th>
                                    {catData.labels.map(l => <th key={l}>{l}</th>)}
                                  </tr>
                                </thead>
                                <tbody>
                                  {catData.rows.map((r, i) => (
                                    <tr key={i}>
                                      <td style={{ fontWeight: 600 }}>{r.name}</td>
                                      {r.stats.map((s, j) => <td key={j}>{s}</td>)}
                                    </tr>
                                  ))}
                                </tbody>
                              </table>
                            </div>
                          );
                        })}
                      </div>
                    );
                  })}
                </div>
              </div>
            )}
            </>
          )}

          {/* Tab 2: 4th Down Decisions */}
          {activeTab === 'fourth' && (
            <div className="dashboard-grid" style={{ margin: '0' }}>
              {/* Left Column: 4th downs list */}
              <div className="panel feed-container" style={{ maxHeight: '450px' }}>
                <div className="panel-header">
                  <span className="panel-title"><ShieldAlert size={16} /> Game 4th Downs</span>
                </div>
                <div style={{ marginTop: '12px' }}>
                  {fourthDowns.length === 0 ? (
                    <div style={{ padding: '24px', textAlign: 'center', color: 'var(--text-muted)' }}>
                      No fourth-down plays analyzed in this matchup.
                    </div>
                  ) : (
                    fourthDowns.map(play => (
                      <div
                        key={play.play_id}
                        className={`play-card ${selectedPlayId === play.play_id ? 'active' : ''}`}
                        onClick={() => setSelectedPlayId(play.play_id)}
                      >
                        <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', color: 'var(--text-muted)', marginBottom: '4px' }}>
                          <span style={{ color: play.possession === activeGame.away_team ? 'var(--accent-cyan)' : 'inherit', fontWeight: '700' }}>
                            {play.possession} Ball
                          </span>
                          <span>{activeGame.away_team} {play.away_score} — {activeGame.home_team} {play.home_score}</span>
                        </div>
                        <div style={{ fontWeight: '600', color: 'var(--accent-orange)' }}>{play.desc}</div>
                        <div style={{ fontSize: '11px', color: 'var(--text-secondary)', marginTop: '4px' }}>Actual: {play.actual}</div>
                      </div>
                    ))
                  )}
                </div>
              </div>

              {/* Right Column: Comparative bar chart stats */}
              <div className="panel">
                <div className="panel-header">
                  <span className="panel-title"><Cpu size={16} /> EV Decision Analytics</span>
                </div>
                
                {fourthDowns.find(d => d.play_id === selectedPlayId) ? (
                  (() => {
                    const play = fourthDowns.find(d => d.play_id === selectedPlayId);
                    return (
                      <div style={{ display: 'flex', flexDirection: 'column', gap: '16px', marginTop: '12px' }}>
                        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                          <div style={{ fontWeight: '700' }}>{play.desc}</div>
                          <div>{getRecommendationBadge(play.recharts_data.find(d => d.wp === Math.max(...play.recharts_data.map(x => x.wp)))?.name)}</div>
                        </div>
                        <div style={{ fontSize: '12px', color: 'var(--text-secondary)' }}>
                          <span style={{ color: play.possession === activeGame.away_team ? 'var(--accent-cyan)' : 'inherit', fontWeight: '700' }}>
                            {play.possession}
                          </span>
                          {' '}ball · {activeGame.away_team} {play.away_score} — {activeGame.home_team} {play.home_score}
                        </div>

                        <div style={{ height: '180px', width: '100%' }}>
                          <ResponsiveContainer width="100%" height="100%">
                            <BarChart data={play.recharts_data} margin={{ top: 10, right: 10, left: -20, bottom: 0 }}>
                              <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                              <XAxis dataKey="label" stroke="var(--text-muted)" fontSize={11} />
                              <YAxis domain={[0, 100]} stroke="var(--text-muted)" fontSize={11} />
                              <Tooltip contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)' }} />
                              <Bar dataKey="wp" name="Win Probability">
                                {play.recharts_data.map((entry, index) => {
                                  let color = 'var(--text-muted)';
                                  if (entry.name === 'GO') color = 'var(--accent-green)';
                                  if (entry.name === 'FG') color = 'var(--accent-orange)';
                                  if (entry.name === 'PUNT') color = 'var(--accent-cyan)';
                                  return <Cell key={`cell-${index}`} fill={color} />;
                                })}
                              </Bar>
                            </BarChart>
                          </ResponsiveContainer>
                        </div>

                        <table className="tactical-table">
                          <thead>
                            <tr>
                              <th>Option</th>
                              <th>Success Rate</th>
                              <th>Expected Win Probability</th>
                            </tr>
                          </thead>
                          <tbody>
                            {play.recharts_data.map(opt => (
                              <tr key={opt.name}>
                                <td style={{ fontWeight: '600' }}>{opt.label}</td>
                                <td>{opt.name === 'PUNT' ? '-' : `${opt.success_rate}%`}</td>
                                <td style={{ color: 'var(--accent-cyan)', fontWeight: '600' }}>{opt.wp}%</td>
                              </tr>
                            ))}
                          </tbody>
                        </table>
                      </div>
                    );
                  })()
                ) : (
                  <div style={{ textAlign: 'center', padding: '40px', color: 'var(--text-muted)' }}>
                    Select a fourth down play log card to examine the mathematical utilities.
                  </div>
                )}
              </div>
            </div>
          )}

          {/* Tab 3: Chess Tactical Evaluator */}
          {activeTab === 'chess' && (() => {
            return (
              <div className="dashboard-grid" style={{ margin: '0' }}>
                {/* Left: live eval bar + EFSD timeline chart */}
                <div className="panel" style={{ display: 'flex', flexDirection: 'column', gap: '14px' }}>
                  <div className="panel-header">
                    <span className="panel-title"><Activity size={16} /> EFSD Game Timeline</span>
                    {chessEval && (
                      <span style={{ fontSize: '11px', color: 'var(--text-muted)' }}>
                        {chessEval.away_team} @ {chessEval.home_team} — {chessEval.n_plays} evaluated plays
                      </span>
                    )}
                  </div>

                  {selectedChessPoint && (
                    <EvalBar
                      homeVal={selectedChessPoint.homeEfsd}
                      homeTeam={activeGame.home_team}
                      awayTeam={activeGame.away_team}
                      homeColor={homeColor}
                      awayColor={awayColor}
                      cap={efsdCap}
                      label={isLatestChessSelected ? 'live' : `Q${selectedEval?.qtr} ${selectedEval?.clock}`}
                    />
                  )}

                  {chessChartData.length > 0 ? (
                    <div style={{ height: '200px', width: '100%' }}>
                      <ResponsiveContainer width="100%" height="100%">
                        <LineChart
                          data={chessChartData}
                          margin={{ top: 10, right: 10, left: -10, bottom: 0 }}
                          onClick={(data) => {
                            const pid = data?.activePayload?.[0]?.payload?.play_id;
                            if (pid != null) setSelectedPlayId(pid);
                          }}
                          style={{ cursor: 'pointer' }}
                        >
                          <defs>
                            <linearGradient id="efsdLineGradient" x1="0" y1="0" x2="1" y2="0">
                              {buildGradientStops(chessChartData, 'homeEfsd', 0, homeColor, awayColor).map((s, i) => (
                                <stop key={i} offset={s.offset} stopColor={s.color} />
                              ))}
                            </linearGradient>
                          </defs>
                          <CartesianGrid strokeDasharray="3 3" stroke="var(--border-color)" />
                          <XAxis
                            type="number" dataKey="t" domain={[0, chessXDomainMax]}
                            ticks={[0, 15, 30, 45, 60]}
                            tickFormatter={(v) => v === 0 ? 'Kick' : v === 60 ? 'End' : `Q${Math.floor(v / 15) + 1}`}
                            stroke="var(--text-muted)" fontSize={11}
                          />
                          <YAxis domain={[-efsdCap, efsdCap]} stroke="var(--text-muted)" fontSize={11} />
                          <Tooltip
                            contentStyle={{ backgroundColor: 'var(--bg-secondary)', borderColor: 'var(--border-color)', fontSize: '11px' }}
                            labelFormatter={(t) => `${Math.round(t)} min elapsed`}
                            formatter={(value) => [
                              `+${Math.abs(value).toFixed(1)} ${value >= 0 ? activeGame.home_team : activeGame.away_team}`,
                              'Projected Final Margin',
                            ]}
                          />
                          <ReferenceLine y={0} stroke="var(--text-muted)" strokeDasharray="4 4" />
                          <Line
                            type="monotone" dataKey="homeEfsd" stroke="url(#efsdLineGradient)" strokeWidth={2}
                            dot={{ r: 2, cursor: 'pointer' }} activeDot={{ r: 5 }}
                            isAnimationActive={false}
                          />
                        </LineChart>
                      </ResponsiveContainer>
                    </div>
                  ) : (
                    <div style={{ padding: '40px', textAlign: 'center', color: 'var(--text-muted)', fontSize: '12px' }}>
                      No positional evaluations available for this game.
                    </div>
                  )}

                  <div style={{ fontSize: '11px', color: 'var(--text-muted)' }}>
                    EFSD = Expected Final Score Differential (projected final margin if the game played out from this point). Bar and line are always {activeGame.home_team} vs {activeGame.away_team} — no sign flip on possession change. Click a chart point to move the eval bar and detail panel to that play.
                    {!isLatestChessSelected && latestChessPoint && (
                      <>
                        {' '}
                        <a href="#" onClick={(e) => { e.preventDefault(); setSelectedPlayId(latestChessPoint.play_id); }} style={{ color: 'var(--accent-cyan)' }}>
                          back to live
                        </a>
                      </>
                    )}
                  </div>
                </div>

                {/* Right: selected play detail */}
                <div className="panel">
                  <div className="panel-header">
                    <span className="panel-title"><Cpu size={16} /> Play Positional Detail</span>
                  </div>

                  {selectedEval ? (
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '14px', marginTop: '12px' }}>
                      {/* Play context */}
                      <div style={{
                        backgroundColor: 'var(--bg-tertiary)',
                        padding: '10px 14px',
                        borderRadius: '6px',
                        borderLeft: '3px solid var(--accent-cyan)',
                        fontSize: '12px'
                      }}>
                        <div style={{ color: 'var(--text-muted)', marginBottom: '4px', fontSize: '11px' }}>
                          Q{selectedEval.qtr} {selectedEval.clock}
                        </div>
                        <strong style={{ color: 'var(--text-primary)' }}>
                          {selectedEval.off} {selectedEval.down}&{selectedEval.ydstogo} at {selectedEval.yardline_100} yds out
                        </strong>
                        <div style={{ color: 'var(--text-muted)', marginTop: '4px' }}>
                          vs {selectedEval.def} | margin {selectedEval.score_differential > 0 ? `+${selectedEval.score_differential}` : selectedEval.score_differential}
                        </div>
                      </div>

                      {/* EP + KEP stats */}
                      <table className="tactical-table">
                        <tbody>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>Expected Points (EP)</td>
                            <td style={{ color: 'var(--accent-orange)', fontWeight: '700', fontFamily: 'monospace' }}>
                              {selectedEval.ep >= 0 ? '+' : ''}{selectedEval.ep.toFixed(3)}
                            </td>
                          </tr>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>EFSD (projected margin)</td>
                            <td style={{
                              color: selectedEval.efsd >= 0 ? 'var(--accent-cyan)' : 'var(--accent-red)',
                              fontWeight: '700',
                              fontFamily: 'monospace'
                            }}>
                              {selectedEval.efsd >= 0 ? '+' : ''}{selectedEval.efsd.toFixed(3)}
                            </td>
                          </tr>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>Field Position</td>
                            <td style={{ fontWeight: '600' }}>{selectedEval.yardline_100} yds to end zone</td>
                          </tr>
                          <tr>
                            <td style={{ color: 'var(--text-muted)' }}>Situation</td>
                            <td style={{ fontWeight: '600' }}>{selectedEval.down}&{selectedEval.ydstogo}</td>
                          </tr>
                        </tbody>
                      </table>

                      {/* EFSD interpretation */}
                      <div style={{
                        padding: '10px 14px',
                        borderRadius: '6px',
                        background: selectedEval.efsd >= 0 ? 'rgba(0,242,254,0.05)' : 'rgba(255,56,56,0.05)',
                        border: `1px solid ${selectedEval.efsd >= 0 ? 'rgba(0,242,254,0.15)' : 'rgba(255,56,56,0.15)'}`,
                        fontSize: '12px',
                        color: 'var(--text-secondary)'
                      }}>
                        <strong style={{ color: selectedEval.efsd >= 0 ? 'var(--accent-cyan)' : 'var(--accent-red)' }}>
                          {selectedEval.off}
                        </strong>{' '}
                        {selectedEval.efsd >= 0
                          ? `is projected to finish the game up +${selectedEval.efsd.toFixed(1)} pts from this state.`
                          : `is projected to finish the game down ${Math.abs(selectedEval.efsd).toFixed(1)} pts from this state.`}
                      </div>

                      {/* Concept recommendation (requires live backend) */}
                      <div>
                        <div style={{ fontSize: '11px', color: 'var(--text-muted)', textTransform: 'uppercase', letterSpacing: '0.05em', marginBottom: '8px', display: 'flex', justifyContent: 'space-between' }}>
                          <span>Play Concept Recommendation</span>
                          {conceptLoading && <span style={{ color: 'var(--accent-cyan)' }}>running sims…</span>}
                        </div>
                        {conceptResult?.concepts ? (() => {
                          const sorted = [...conceptResult.concepts].sort((a, b) => (b.delta_efsd ?? -Infinity) - (a.delta_efsd ?? -Infinity));
                          const maxAbs = sorted.reduce((m, c) => Math.max(m, Math.abs(c.delta_efsd ?? 0)), 0.5);
                          return (
                            <div style={{ display: 'flex', flexDirection: 'column', gap: '7px' }}>
                              {sorted.map((c, i) => {
                                const dk = c.delta_efsd;
                                const isNull = dk === null || c.n < 5;
                                const barPct = isNull ? 0 : Math.min(100, (Math.abs(dk) / maxAbs) * 100);
                                const barColor = isNull ? 'rgba(255,255,255,0.12)' : dk > 0 ? 'var(--accent-green)' : 'var(--accent-red)';
                                return (
                                  <div key={c.concept}>
                                    <div style={{ display: 'flex', justifyContent: 'space-between', fontSize: '11px', marginBottom: '3px' }}>
                                      <span style={{ display: 'flex', gap: '5px', alignItems: 'center' }}>
                                        {i === 0 && !isNull && (
                                          <span style={{ padding: '1px 5px', borderRadius: '3px', background: 'rgba(0,245,212,0.14)', color: 'var(--accent-green)', fontSize: '9px', fontWeight: 800 }}>BEST</span>
                                        )}
                                        <span style={{ color: 'var(--text-primary)', fontWeight: 600 }}>{c.concept}</span>
                                        <span style={{ color: 'var(--text-muted)' }}>({c.n})</span>
                                      </span>
                                      <span style={{ fontFamily: 'monospace', color: barColor }}>
                                        {isNull ? 'N/A' : (dk > 0 ? `+${dk.toFixed(3)}` : dk.toFixed(3))}
                                      </span>
                                    </div>
                                    <div style={{ width: '100%', height: '4px', background: 'rgba(255,255,255,0.05)', borderRadius: '2px', overflow: 'hidden' }}>
                                      <div style={{ width: `${barPct}%`, height: '100%', background: barColor, transition: 'width 0.3s ease' }} />
                                    </div>
                                  </div>
                                );
                              })}
                            </div>
                          );
                        })() : !conceptLoading && (
                          <div style={{ fontSize: '11px', color: 'var(--text-muted)', fontStyle: 'italic' }}>
                            Start the backend to see play recommendations.
                          </div>
                        )}
                      </div>
                    </div>
                  ) : (
                    <div style={{ textAlign: 'center', padding: '40px', color: 'var(--text-muted)', fontSize: '12px' }}>
                      {chessChartData.length > 0
                        ? 'Click a point on the EFSD chart or select a play from the Play Log to see positional detail.'
                        : 'No evaluations available for this game.'}
                    </div>
                  )}
                </div>
              </div>
            );
          })()}

        </div>
      )}
    </div>
  );
}

export default GameSummary;
