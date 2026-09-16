import { useState, useEffect } from 'react'
import { PAGES } from './pagesConfig'
import { ApiService } from './api'
import Navbar from './components/Navbar'
import ProgressFooter from './components/ProgressFooter'
import Home from './pages/Home'
import Simulator from './pages/Simulator'
import SlateLeaders from './pages/SlateLeaders'
import About from './pages/About'
import Roadmap from './pages/Roadmap'
import InDevelopment from './pages/InDevelopment'
import DfsSummary from './pages/DfsSummary'
import Optimizer from './pages/Optimizer'
import ShowdownOptimizer from './pages/ShowdownOptimizer'
import EvaluationTab from './pages/EvaluationTab'
import SimReplays from './pages/SimReplays'
import CashLineups from './pages/CashLineups'
import Bankroll from './pages/Bankroll'
import Leverage from './pages/Leverage'
import './App.css'

export default function App() {
  const [currentPage, setCurrentPage] = useState('home');

  // Lifted shared simulator states
  const [selectedSlate, setSelectedSlate] = useState('TRADITIONAL');
  const [weeks, setWeeks] = useState([]);
  const [selectedWeek, setSelectedWeek] = useState(1);
  const [games, setGames] = useState([]);
  const [weekProjections, setWeekProjections] = useState([]);
  const [selectedGame, setSelectedGame] = useState(null);
  const [hasSimResults, setHasSimResults] = useState(false);
  const [allSimResults, setAllSimResults] = useState({});
  const [simResults, setSimResults] = useState(null);
  const [generatedLineups, setGeneratedLineups] = useState([]);
  const [optimizerLineups, setOptimizerLineups] = useState([]);
  const [optimizerSettings, setOptimizerSettings] = useState(null);

  // DK slate selection (Main Slate today; more slate types once DK has them
  // live -- e.g. Showdown/split-Sunday). Lifted here rather than kept local
  // to the Optimizer page so week_projections/games/rosters can all reflect
  // whichever slate is selected as this grows beyond just the Optimizer.
  const [dkSlates, setDkSlates] = useState([]);
  const [selectedDraftGroupId, setSelectedDraftGroupId] = useState(null);

  useEffect(() => {
    // Passing selectedWeek lets the backend apply its sticky per-week
    // "main slate" pin (see dk_scraper.resolve_main_slate_draft_group_id)
    // instead of DK's raw live "most open contests" pick, which silently
    // flips to a small leftover slate once the real main slate's contests
    // lock. This effect only re-fires on a WEEK change (its only dependency),
    // never on an in-week manual slate pick, so always adopting that week's
    // resolved default here is safe -- it can't clobber a same-week user
    // choice. (Previously used `prev ?? ...`, which meant the very first
    // week's resolved default stuck forever across every later week switch --
    // e.g. mounting on Week 1 then switching to Week 2 kept serving Week 1's
    // pinned slate.)
    // Guards against a stale response landing after a newer one -- e.g. the
    // Week 1 request (fired on initial mount) resolving AFTER the Week 2
    // request that supersedes it moments later (selectedWeek now defaults to
    // the most recent week, so both fire in quick succession on every load).
    // Without this, network timing alone decides which week's slate wins.
    let cancelled = false;
    ApiService.getDkSlates(selectedWeek)
      .then(data => {
        if (cancelled) return;
        setDkSlates(data.slates || []);
        setSelectedDraftGroupId(data.default_draft_group_id ?? null);
      })
      .catch(err => console.error("Error fetching DK slates:", err));
    return () => { cancelled = true; };
  }, [selectedWeek]);

  // Global data fetching for weeks, games, and week projections. Also
  // defaults selectedWeek to the most recent available week (max of
  // /api/weeks) instead of always starting on Week 1 -- runs once on
  // mount, before the user could have picked a week themselves.
  useEffect(() => {
    ApiService.getWeeks()
      .then(data => {
        const ws = data.weeks && data.weeks.length ? data.weeks : [1];
        setWeeks(ws);
        setSelectedWeek(Math.max(...ws));
      })
      .catch(err => console.error("Error fetching weeks:", err));
  }, []);

  // DK-salary-dependent fetches -- these alone need to react to a slate
  // switch. selectedWeek/selectedDraftGroupId both change in quick
  // succession on mount (week defaults forward, then the slate resolves),
  // so this fires several times before settling -- `cancelled` drops any
  // response that isn't for the most recent request, kept separate from the
  // sim-results effect below so that harmless double-fire doesn't also
  // re-trigger an expensive Monte Carlo re-simulation.
  useEffect(() => {
    let cancelled = false;
    ApiService.getGames(selectedWeek, selectedDraftGroupId)
      .then(data => {
        if (cancelled) return;
        setGames(data.games || []);
      })
      .catch(err => console.error("Error fetching games:", err));

    ApiService.getWeekProjections(selectedWeek, selectedDraftGroupId)
      .then(data => {
        if (cancelled) return;
        setWeekProjections(data.players || []);
      })
      .catch(err => console.error("Error fetching week projections:", err));
    return () => { cancelled = true; };
  }, [selectedWeek, selectedDraftGroupId]);

  useEffect(() => {
    // Prepopulate every game's baseline sim results from the parquet cache so
    // the Simulator doesn't need a per-game "Run Engine" click to show data.
    // Unrelated to DK salaries/slate -- must not re-fire on a slate switch.
    let cancelled = false;
    ApiService.getWeekSimResults(selectedWeek)
      .then(data => {
        if (cancelled) return;
        const gameResults = data.games || {};
        if (Object.keys(gameResults).length > 0) {
          setAllSimResults(prev => ({ ...gameResults, ...prev }));
          setHasSimResults(true);
        }
      })
      .catch(err => console.error("Error fetching week sim results:", err));
    return () => { cancelled = true; };
  }, [selectedWeek]);

  // Handle URL location hash sync for navigation bookmarks and browser history support
  useEffect(() => {
    const handleHashChange = () => {
      const hash = window.location.hash.replace('#', '');
      const baseHash = hash.split('?')[0];
      const validPages = PAGES.map(p => p.id);
      if (validPages.includes(baseHash)) {
        setCurrentPage(baseHash);
      } else {
        setCurrentPage('home');
      }
    };

    // Run on initial mount
    handleHashChange();

    window.addEventListener('hashchange', handleHashChange);
    return () => window.removeEventListener('hashchange', handleHashChange);
  }, []);

  const [slateOverrides, setSlateOverrides] = useState({});

  // Determine active component to render
  const renderPageContent = () => {
    const page = PAGES.find(p => p.id === currentPage);
    if (!page) return <Home setCurrentPage={setCurrentPage} />;

    if (page.isDevelopment) {
      return <InDevelopment currentPage={currentPage} />;
    }

    switch (currentPage) {
      case 'home':
        return <Home setCurrentPage={setCurrentPage} />;
      case 'simulator':
        return (
          <Simulator 
            slateOverrides={slateOverrides} 
            setSlateOverrides={setSlateOverrides}
            selectedSlate={selectedSlate}
            setSelectedSlate={setSelectedSlate}
            weeks={weeks}
            setWeeks={setWeeks}
            selectedWeek={selectedWeek}
            setSelectedWeek={setSelectedWeek}
            games={games}
            setGames={setGames}
            weekProjections={weekProjections}
            setWeekProjections={setWeekProjections}
            selectedGame={selectedGame}
            setSelectedGame={setSelectedGame}
            hasSimResults={hasSimResults}
            setHasSimResults={setHasSimResults}
            allSimResults={allSimResults}
            setAllSimResults={setAllSimResults}
            simResults={simResults}
            setSimResults={setSimResults}
            generatedLineups={generatedLineups}
            setGeneratedLineups={setGeneratedLineups}
            setCurrentPage={setCurrentPage}
          />
        );
      case 'dfs_summary':
        return (
          <DfsSummary
            selectedWeek={selectedWeek}
            selectedSlate={selectedSlate}
            weekProjections={weekProjections}
            allSimResults={allSimResults}
            simResults={simResults}
            generatedLineups={generatedLineups}
            setCurrentPage={setCurrentPage}
          />
        );
      case 'optimizer':
        return (
          <Optimizer
            allSimResults={allSimResults}
            simResults={simResults}
            weekProjections={weekProjections}
            games={games}
            weeks={weeks}
            selectedWeek={selectedWeek}
            setSelectedWeek={setSelectedWeek}
            optimizerLineups={optimizerLineups}
            setOptimizerLineups={setOptimizerLineups}
            optimizerSettings={optimizerSettings}
            setOptimizerSettings={setOptimizerSettings}
            setCurrentPage={setCurrentPage}
            dkSlates={dkSlates}
            selectedDraftGroupId={selectedDraftGroupId}
            setSelectedDraftGroupId={setSelectedDraftGroupId}
          />
        );
      case 'showdown_optimizer':
        return (
          <ShowdownOptimizer
            allSimResults={allSimResults}
            games={games}
            weeks={weeks}
            selectedWeek={selectedWeek}
            setSelectedWeek={setSelectedWeek}
          />
        );
      case 'evaluation':
        return (
          <EvaluationTab
            allSimResults={allSimResults}
            games={games}
            weeks={weeks}
            selectedWeek={selectedWeek}
            setSelectedWeek={setSelectedWeek}
          />
        );
      case 'sim_replays':
        return <SimReplays />;
      case 'slate_leaders':
        return <SlateLeaders />;
      case 'cash_lineups':
        return <CashLineups />;
      case 'bankroll':
        return <Bankroll />;
      case 'leverage':
        return <Leverage weekProjections={weekProjections} allSimResults={allSimResults} />;
      case 'about':
        return <About />;
      case 'roadmap':
        return <Roadmap />;
      default:
        return <Home setCurrentPage={setCurrentPage} />;
    }
  };

  return (
    <div style={{
      display: 'flex',
      flexDirection: 'column',
      minHeight: '100vh',
      maxWidth: (currentPage === 'optimizer' || currentPage === 'showdown_optimizer' || currentPage === 'evaluation') ? '1680px' : '1280px',
      margin: '0 auto',
      padding: '20px',
      boxSizing: 'border-box'
    }}>
      {/* Global Header Navigation */}
      <Navbar currentPage={currentPage} setCurrentPage={setCurrentPage} />

      {/* Page Content viewport */}
      <main style={{ flexGrow: 1, display: 'flex', flexDirection: 'column' }}>
        {renderPageContent()}
      </main>

      {/* Progress tracking footer */}
      <ProgressFooter setCurrentPage={setCurrentPage} />
    </div>
  )
}
