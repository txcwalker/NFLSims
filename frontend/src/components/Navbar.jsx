import { useState, useEffect } from 'react'
import { PAGES } from '../pagesConfig'
import { ApiService } from '../api'

export default function Navbar({ currentPage, setCurrentPage }) {
  const [dropdownOpen, setDropdownOpen] = useState(false);
  const [isOnline, setIsOnline] = useState(true);

  useEffect(() => {
    let active = true;
    const verifyHealth = () => {
      ApiService.checkHealth().then(online => {
        if (active) setIsOnline(online);
      });
    };
    verifyHealth();
    const interval = setInterval(verifyHealth, 30000);
    return () => {
      active = false;
      clearInterval(interval);
    };
  }, []);

  // Group pages by category
  const coreLinks = PAGES.filter(p => p.category === 'core' && p.showInNavbar);
  const toolLinks = PAGES.filter(p => p.category === 'tool' && p.showInNavbar);
  const metaLinks = PAGES.filter(p => p.category === 'meta' && p.showInNavbar);

  const handleNav = (pageId) => {
    setCurrentPage(pageId);
    window.location.hash = pageId === 'home' ? '' : pageId;
    setDropdownOpen(false);
  };

  return (
    <nav style={{
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      padding: '16px 24px',
      background: 'var(--bg-glass)',
      backdropFilter: 'blur(16px)',
      border: '1px solid var(--border-glass)',
      borderRadius: '16px',
      marginBottom: '24px',
      boxShadow: 'var(--shadow-main)',
      position: 'relative',
      zIndex: 100
    }}>
      {/* Brand Logo / Home trigger */}
      <div 
        onClick={() => handleNav('home')}
        style={{
          display: 'flex',
          alignItems: 'center',
          gap: '10px',
          cursor: 'pointer',
          userSelect: 'none'
        }}
      >
        <div className="glow-dot" style={{ animation: 'pulse-glow 1.5s infinite alternate' }}></div>
        <span style={{
          fontSize: '1.4rem',
          fontWeight: 800,
          background: 'linear-gradient(135deg, #ffffff 30%, var(--accent-primary) 100%)',
          WebkitBackgroundClip: 'text',
          WebkitTextFillColor: 'transparent',
          letterSpacing: '-0.5px'
        }}>
          NFLSims
        </span>
        <div 
          title={isOnline ? "Connected to live API server" : "API offline - sandbox fallback active"}
          style={{
            width: '8px',
            height: '8px',
            borderRadius: '50%',
            backgroundColor: isOnline ? '#10b981' : '#f59e0b',
            boxShadow: isOnline ? '0 0 8px #10b981' : '0 0 8px #f59e0b',
            transition: 'all 0.3s ease',
            marginLeft: '-2px'
          }}
        ></div>
      </div>

      {/* Nav Menu Links */}
      <div style={{
        display: 'flex',
        alignItems: 'center',
        gap: '20px'
      }}>
        {/* Core Links */}
        {coreLinks.map(p => (
          <button
            key={p.id}
            onClick={() => handleNav(p.id)}
            style={{
              background: 'transparent',
              border: 'none',
              color: currentPage === p.id ? 'var(--accent-primary)' : 'var(--text-main)',
              fontWeight: 600,
              fontSize: '0.95rem',
              cursor: 'pointer',
              padding: '6px 12px',
              borderRadius: '8px',
              transition: 'all 0.2s',
              textShadow: currentPage === p.id ? 'var(--glow-cyan)' : 'none'
            }}
          >
            {p.label}
          </button>
        ))}

        {/* Dropdown Tools Link */}
        <div 
          className="nav-dropdown-wrapper"
          onMouseEnter={() => setDropdownOpen(true)}
          onMouseLeave={() => setDropdownOpen(false)}
          style={{ position: 'relative' }}
        >
          <button
            style={{
              background: 'transparent',
              border: 'none',
              color: toolLinks.some(t => t.id === currentPage) ? 'var(--accent-secondary)' : 'var(--text-main)',
              fontWeight: 600,
              fontSize: '0.95rem',
              cursor: 'pointer',
              padding: '6px 12px',
              borderRadius: '8px',
              transition: 'all 0.2s',
              display: 'flex',
              alignItems: 'center',
              gap: '4px'
            }}
          >
            Tools (Beta)
            <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3" style={{
              transform: dropdownOpen ? 'rotate(180deg)' : 'rotate(0deg)',
              transition: 'transform 0.2s'
            }}>
              <polyline points="6 9 12 15 18 9"></polyline>
            </svg>
          </button>

          {/* Dropdown Card */}
          {dropdownOpen && (
            <div style={{
              position: 'absolute',
              top: '100%',
              left: '50%',
              transform: 'translateX(-50%)',
              paddingTop: '8px',
              width: '240px',
              zIndex: 101
            }}>
              <div style={{
                background: '#0c122b',
                border: '1px solid var(--border-glass)',
                borderRadius: '12px',
                padding: '8px',
                boxShadow: '0 10px 25px -5px rgba(0, 0, 0, 0.6)',
                display: 'flex',
                flexDirection: 'column',
                gap: '4px'
              }}>
                {toolLinks.map(t => (
                  <button
                    key={t.id}
                    onClick={() => handleNav(t.id)}
                    style={{
                      background: currentPage === t.id ? 'rgba(157, 78, 221, 0.15)' : 'transparent',
                      border: 'none',
                      color: currentPage === t.id ? 'var(--text-white)' : 'var(--text-main)',
                      textAlign: 'left',
                      padding: '10px 12px',
                      borderRadius: '8px',
                      fontSize: '0.88rem',
                      fontWeight: 600,
                      cursor: 'pointer',
                      transition: 'all 0.2s',
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'space-between',
                      gap: '8px'
                    }}
                    className="dropdown-item"
                  >
                    <span style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
                      <span style={{ fontSize: '1.1rem' }}>{t.icon}</span>
                      {t.label}
                    </span>
                    {t.isDevelopment && (
                      <span style={{
                        fontSize: '0.7rem',
                        padding: '2px 6px',
                        background: 'rgba(255, 170, 0, 0.12)',
                        color: 'var(--accent-gold)',
                        borderRadius: '4px',
                        border: '1px solid rgba(255, 170, 0, 0.2)',
                        fontWeight: 700
                      }}>
                        DEV
                      </span>
                    )}
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>

        {/* Meta Links */}
        {metaLinks.map(p => (
          <button
            key={p.id}
            onClick={() => handleNav(p.id)}
            style={{
              background: 'transparent',
              border: 'none',
              color: currentPage === p.id ? 'var(--text-white)' : 'var(--text-main)',
              fontWeight: 600,
              fontSize: '0.95rem',
              cursor: 'pointer',
              padding: '6px 12px',
              borderRadius: '8px',
              transition: 'all 0.2s',
              borderBottom: currentPage === p.id ? '2px solid var(--accent-secondary)' : '2px solid transparent'
            }}
          >
            {p.label}
          </button>
        ))}
      </div>
    </nav>
  )
}
