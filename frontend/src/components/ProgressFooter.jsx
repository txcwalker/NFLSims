import { useState, useEffect } from 'react'

export default function ProgressFooter({ setCurrentPage }) {
  const [tickerIndex, setTickerIndex] = useState(0);

  const tickerMessages = [
    "Currently Hardening: Clock Physics & Punting models",
    "Up Next: DFS Lineup Optimizer (Integer Knapsack Solver)",
    "Model standard locked: 10/10 engine models audited",
    "Methodology updated: Play lifecycle visualizer live",
    "Next Milestone: Tier 1 validation vs. 2024 season actuals"
  ];

  useEffect(() => {
    const timer = setInterval(() => {
      setTickerIndex((prev) => (prev + 1) % tickerMessages.length);
    }, 4500);
    return () => clearInterval(timer);
  }, [tickerMessages.length]);

  const handleFooterClick = () => {
    setCurrentPage('roadmap');
    window.location.hash = 'roadmap';
  };

  return (
    <footer 
      onClick={handleFooterClick}
      style={{
        display: 'flex',
        justifyContent: 'space-between',
        alignItems: 'center',
        padding: '12px 24px',
        background: 'var(--bg-glass)',
        backdropFilter: 'blur(16px)',
        border: '1px solid var(--border-glass)',
        borderRadius: '12px',
        marginTop: '30px',
        boxShadow: '0 -4px 20px rgba(0, 0, 0, 0.4)',
        cursor: 'pointer',
        flexWrap: 'wrap',
        gap: '15px',
        transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)',
        userSelect: 'none'
      }}
      className="progress-footer"
    >
      {/* Version Tag */}
      <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
        <div style={{
          width: '6px',
          height: '6px',
          borderRadius: '50%',
          background: 'var(--accent-primary)',
          boxShadow: 'var(--glow-cyan)'
        }}></div>
        <span style={{ fontSize: '0.8rem', fontWeight: 700, color: 'var(--text-white)' }}>
          NFLSims <span style={{ color: 'var(--accent-primary)', fontFamily: 'monospace' }}>v0.1.0-alpha</span>
        </span>
      </div>

      {/* Progress Bars */}
      <div style={{
        display: 'flex',
        alignItems: 'center',
        gap: '24px',
        flexWrap: 'wrap'
      }}>
        {/* Tier 1 */}
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <span style={{ fontSize: '0.75rem', fontWeight: 600, color: 'var(--text-muted)' }}>Tier 1:</span>
          <div style={{ width: '80px', height: '6px', background: 'rgba(255,255,255,0.06)', borderRadius: '3px', overflow: 'hidden' }}>
            <div style={{ width: '75%', height: '100%', background: 'var(--accent-primary)', boxShadow: 'var(--glow-cyan)' }}></div>
          </div>
          <span style={{ fontSize: '0.72rem', fontWeight: 700, color: 'var(--accent-primary)', fontFamily: 'monospace' }}>75%</span>
        </div>

        {/* Tier 2 */}
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <span style={{ fontSize: '0.75rem', fontWeight: 600, color: 'var(--text-muted)' }}>Tier 2:</span>
          <div style={{ width: '80px', height: '6px', background: 'rgba(255,255,255,0.06)', borderRadius: '3px', overflow: 'hidden' }}>
            <div style={{ width: '10%', height: '100%', background: 'var(--accent-secondary)', boxShadow: 'var(--glow-purple)' }}></div>
          </div>
          <span style={{ fontSize: '0.72rem', fontWeight: 700, color: 'var(--accent-secondary)', fontFamily: 'monospace' }}>10%</span>
        </div>

        {/* Tier 3 */}
        <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
          <span style={{ fontSize: '0.75rem', fontWeight: 600, color: 'var(--text-muted)' }}>Tier 3:</span>
          <div style={{ width: '80px', height: '6px', background: 'rgba(255,255,255,0.06)', borderRadius: '3px', overflow: 'hidden' }}>
            <div style={{ width: '0%', height: '100%', background: 'var(--text-muted)' }}></div>
          </div>
          <span style={{ fontSize: '0.72rem', fontWeight: 700, color: 'var(--text-muted)', fontFamily: 'monospace' }}>0%</span>
        </div>
      </div>

      {/* Marquee Ticker */}
      <div style={{
        display: 'flex',
        alignItems: 'center',
        gap: '6px',
        overflow: 'hidden',
        minWidth: '280px',
        maxWidth: '380px'
      }}>
        <span style={{ fontSize: '0.75rem', fontWeight: 800, color: 'var(--accent-gold)', textTransform: 'uppercase', letterSpacing: '0.5px' }}>
          [STATUS]:
        </span>
        <span 
          style={{
            fontSize: '0.78rem',
            color: 'var(--text-main)',
            fontWeight: 500,
            transition: 'opacity 0.3s ease-in-out',
            whiteSpace: 'nowrap',
            textOverflow: 'ellipsis',
            overflow: 'hidden'
          }}
          className="ticker-text"
        >
          {tickerMessages[tickerIndex]}
        </span>
      </div>
    </footer>
  )
}
