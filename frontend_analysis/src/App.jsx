import { useState, useEffect } from 'react';
import { PAGES } from './pagesConfig';
import Home from './pages/Home';
import FourthDowns from './pages/FourthDowns';
import Standings from './pages/Standings';
import Season2026 from './pages/Season2026';
import GameSummary from './pages/GameSummary';
import HistoricalLab from './pages/HistoricalLab';
import BotFeed from './pages/BotFeed';
import './App.css';

function App() {
  const [currentPage, setCurrentPage] = useState('home');
  const [activeGameId, setActiveGameId] = useState('');
  const [activeGameDate, setActiveGameDate] = useState('');

  useEffect(() => {
    // Basic hash router supporting simple query params e.g. #game-summary?id=live_game_1
    const handleHashChange = () => {
      const fullHash = window.location.hash.replace('#', '');
      const [path, queryStr] = fullHash.split('?');
      
      // Parse query params into key-value pairs
      const params = {};
      if (queryStr) {
        queryStr.split('&').forEach(pair => {
          const [k, v] = pair.split('=');
          params[k] = decodeURIComponent(v);
        });
      }

      if (params.id) {
        setActiveGameId(params.id);
      }
      // Games from a non-today slate (yesterday, a picked date, or the
      // backend's "no games today" fallback) only exist on that date's ESPN
      // scoreboard -- GameSummary needs it to re-query the right day instead
      // of silently searching today's (empty) slate and hanging forever.
      setActiveGameDate(params.date || '');

      const validPage = PAGES.find(p => p.id === path);
      if (validPage) {
        setCurrentPage(validPage.id);
      } else {
        setCurrentPage('home');
      }
    };

    window.addEventListener('hashchange', handleHashChange);
    // Trigger on initial load
    handleHashChange();

    return () => window.removeEventListener('hashchange', handleHashChange);
  }, []);

  const navigateTo = (pageId, params = null) => {
    let hash = `#${pageId}`;
    if (params) {
      const queryStr = Object.entries(params)
        .map(([k, v]) => `${k}=${encodeURIComponent(v)}`)
        .join('&');
      hash += `?${queryStr}`;
    }
    window.location.hash = hash;
  };

  const renderActivePage = () => {
    switch (currentPage) {
      case 'home':
        return <Home navigateTo={navigateTo} />;
      case 'standings':
        return <Standings />;
      case 'season-2026':
        return <Season2026 />;
      case 'fourth-downs':
        return <FourthDowns />;
      case 'game-summary':
        return <GameSummary gameId={activeGameId} gameDate={activeGameDate} navigateTo={navigateTo} />;
      case 'historical-lab':
        return <HistoricalLab />;
      case 'bot-feed':
        return <BotFeed />;
      default:
        return <Home navigateTo={navigateTo} />;
    }
  };

  return (
    <div className="app-container">
      <header className="header">
        <div className="logo" style={{ cursor: 'pointer' }} onClick={() => navigateTo('home')}>
          <div className="pulse-dot"></div>
          NFLSIMS <span style={{ color: 'var(--text-muted)', fontSize: '12px', fontWeight: '500' }}>TACTICAL ANALYST</span>
        </div>
        <nav className="nav-links">
          {PAGES.filter(page => page.showInNavbar).map(page => (
            <button
              key={page.id}
              className={`nav-btn ${currentPage === page.id ? 'active' : ''}`}
              onClick={() => navigateTo(page.id)}
            >
              <span style={{ fontSize: '14px' }}>{page.icon}</span> {page.label}
            </button>
          ))}
        </nav>
      </header>

      <main className="main-content">
        {renderActivePage()}
      </main>

      <footer style={{ 
        padding: '16px 24px', 
        borderTop: '1px solid var(--border-color)', 
        backgroundColor: 'var(--bg-secondary)',
        display: 'flex',
        justifyContent: 'space-between',
        fontSize: '12px',
        color: 'var(--text-muted)',
        fontFamily: 'var(--font-heading)'
      }}>
        <div>NFLSims Analytics Engine v0.2.0-beta</div>
        <div style={{ display: 'flex', gap: '16px' }}>
          <span>Theme: Control Room</span>
          <span style={{ color: 'var(--accent-cyan)' }}>● Core Engine Operational</span>
        </div>
      </footer>
    </div>
  );
}

export default App;
