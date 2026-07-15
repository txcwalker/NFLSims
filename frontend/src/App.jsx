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

  // Global data fetching for weeks, games, and week projections
  useEffect(() => {
    ApiService.getWeeks()
      .then(data => {
        setWeeks(data.weeks || [1]);
      })
      .catch(err => console.error("Error fetching weeks:", err));
  }, []);

  useEffect(() => {
    ApiService.getGames(selectedWeek)
      .then(data => {
        setGames(data.games || []);
      })
      .catch(err => console.error("Error fetching games:", err));

    ApiService.getWeekProjections(selectedWeek)
      .then(data => {
        setWeekProjections(data.players || []);
      })
      .catch(err => console.error("Error fetching week projections:", err));

    // Prepopulate every game's baseline sim results from the parquet cache so
    // the Simulator doesn't need a per-game "Run Engine" click to show data.
    ApiService.getWeekSimResults(selectedWeek)
      .then(data => {
        const gameResults = data.games || {};
        if (Object.keys(gameResults).length > 0) {
          setAllSimResults(prev => ({ ...gameResults, ...prev }));
          setHasSimResults(true);
        }
      })
      .catch(err => console.error("Error fetching week sim results:", err));
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
          />
        );
      case 'slate_leaders':
        return <SlateLeaders />;
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
      maxWidth: currentPage === 'optimizer' ? '1680px' : '1280px',
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
