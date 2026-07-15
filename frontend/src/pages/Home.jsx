import { PAGES } from '../pagesConfig'

export default function Home({ setCurrentPage }) {
  const handleNav = (pageId) => {
    setCurrentPage(pageId);
    window.location.hash = pageId === 'home' ? '' : pageId;
  };

  const featurePages = PAGES.filter(p => p.id !== 'home');

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      {/* HERO SECTION */}
      <section style={{
        textAlign: 'center',
        padding: '50px 20px',
        background: 'radial-gradient(circle at 50% 50%, rgba(157, 78, 221, 0.12) 0%, transparent 60%)',
        borderRadius: '24px',
        border: '1px solid rgba(255, 255, 255, 0.03)',
        marginBottom: '40px',
        position: 'relative',
        overflow: 'hidden'
      }}>
        <div style={{
          position: 'absolute',
          top: '-10%',
          left: '50%',
          transform: 'translateX(-50%)',
          width: '300px',
          height: '300px',
          background: 'rgba(0, 242, 254, 0.1)',
          borderRadius: '50%',
          filter: 'blur(80px)',
          zIndex: 0,
          pointerEvents: 'none'
        }}></div>

        <h1 style={{
          fontSize: '3rem',
          fontWeight: 900,
          marginBottom: '16px',
          justifyContent: 'center',
          letterSpacing: '-1px',
          zIndex: 1,
          position: 'relative'
        }}>
          Simulate the Gridiron. <span style={{
            background: 'linear-gradient(135deg, var(--accent-primary) 0%, var(--accent-secondary) 100%)',
            WebkitBackgroundClip: 'text',
            WebkitTextFillColor: 'transparent'
          }}>Own the Slates.</span>
        </h1>

        <p style={{
          fontSize: '1.2rem',
          color: 'var(--text-main)',
          maxWidth: '800px',
          margin: '0 auto 32px auto',
          lineHeight: '1.6',
          zIndex: 1,
          position: 'relative'
        }}>
          NFLSims runs <strong>10,000 play-by-play simulations</strong> in parallel to model full game slates. Custom-tune tempo, coaching aggressiveness (PROE), rosters, and catch rates to project player statistics and find DraftKings and FanDuel value edges.
        </p>

        <div style={{
          display: 'flex',
          justifyContent: 'center',
          gap: '16px',
          flexWrap: 'wrap',
          zIndex: 1,
          position: 'relative'
        }}>
          <button 
            onClick={() => handleNav('simulator')}
            className="btn-primary" 
            style={{ width: 'auto', padding: '14px 32px' }}
          >
            Launch DFS Simulator
          </button>
          <button 
            onClick={() => handleNav('roadmap')}
            style={{
              background: 'rgba(255, 255, 255, 0.05)',
              border: '1px solid var(--border-glass)',
              color: 'var(--text-white)',
              borderRadius: '12px',
              padding: '14px 32px',
              fontWeight: 700,
              fontSize: '1.05rem',
              cursor: 'pointer',
              transition: 'all 0.3s'
            }}
            className="btn-secondary-glow"
          >
            View Project Roadmap
          </button>
        </div>
      </section>

      {/* THREE-COLUMN DOCK / STATUS SUMMARY */}
      <section style={{
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(320px, 1fr))',
        gap: '24px',
        marginBottom: '40px'
      }}>
        {/* Playbook Engine Status */}
        <div className="glass-panel" style={{ display: 'flex', flexDirection: 'column' }}>
          <h2 style={{ marginBottom: '16px', display: 'flex', alignItems: 'center', gap: '8px' }}>
            <span style={{ color: 'var(--accent-primary)' }}>⚙️</span> Simulator Diagnostics
          </h2>
          <div style={{ display: 'flex', flexDirection: 'column', gap: '12px', flexGrow: 1 }}>
            <div style={{ display: 'flex', justifyContent: 'space-between', borderBottom: '1px solid rgba(255,255,255,0.04)', paddingBottom: '8px' }}>
              <span style={{ fontSize: '0.9rem' }}>Play Call Model (PROE Ratio)</span>
              <span style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--accent-green)' }}>HARDENED 🟢</span>
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between', borderBottom: '1px solid rgba(255,255,255,0.04)', paddingBottom: '8px' }}>
              <span style={{ fontSize: '0.9rem' }}>Yardage Engines (Air yds, Rush, YAC)</span>
              <span style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--accent-green)' }}>ACTIVE 🟢</span>
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between', borderBottom: '1px solid rgba(255,255,255,0.04)', paddingBottom: '8px' }}>
              <span style={{ fontSize: '0.9rem' }}>Post-Snap Chaos (Pressures, Sacks, TOs)</span>
              <span style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--accent-green)' }}>ACTIVE 🟢</span>
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between', borderBottom: '1px solid rgba(255,255,255,0.04)', paddingBottom: '8px' }}>
              <span style={{ fontSize: '0.9rem' }}>Clock Physics Engine</span>
              <span style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--accent-gold)' }}>HARDENING 🟡</span>
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between', paddingBottom: '4px' }}>
              <span style={{ fontSize: '0.9rem' }}>Lineup Optimizer Solver</span>
              <span style={{ fontSize: '0.85rem', fontWeight: 700, color: 'var(--accent-secondary)' }}>PLANNING 🟣</span>
            </div>
          </div>
        </div>

        {/* Engine Performance Stats */}
        <div className="glass-panel" style={{ display: 'flex', flexDirection: 'column' }}>
          <h2 style={{ marginBottom: '16px', display: 'flex', alignItems: 'center', gap: '8px' }}>
            <span style={{ color: 'var(--accent-secondary)' }}>⚡</span> Processing Power
          </h2>
          <div style={{ display: 'flex', flexDirection: 'column', gap: '16px', flexGrow: 1, justifyContent: 'center' }}>
            <div style={{ textAlign: 'center' }}>
              <div style={{ fontSize: '2.2rem', fontWeight: 800, color: 'var(--accent-primary)' }}>1.2s</div>
              <div style={{ fontSize: '0.8rem', color: 'var(--text-muted)', textTransform: 'uppercase' }}>Avg Run Time (10,000 simulations)</div>
            </div>
            <div style={{ height: '1px', background: 'rgba(255,255,255,0.06)' }}></div>
            <div style={{ display: 'flex', justifyContent: 'space-around' }}>
              <div style={{ textAlign: 'center' }}>
                <div style={{ fontWeight: 700, color: 'var(--text-white)' }}>Multi-Core</div>
                <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>Parallel CPU Cores</span>
              </div>
              <div style={{ textAlign: 'center' }}>
                <div style={{ fontWeight: 700, color: 'var(--text-white)' }}>Client-Side</div>
                <span style={{ fontSize: '0.75rem', color: 'var(--text-muted)' }}>Memory Sandbox</span>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* CORE & FUTURE CAPABILITIES GRID */}
      <h2 style={{ marginBottom: '24px', fontSize: '1.6rem', borderBottom: '1px solid rgba(255,255,255,0.06)', paddingBottom: '12px' }}>
        Interactive Portal & Living Map
      </h2>

      <div style={{
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
        gap: '24px'
      }}>
        {featurePages.map(p => (
          <div 
            key={p.id} 
            className="glass-panel"
            style={{
              display: 'flex',
              flexDirection: 'column',
              justifyContent: 'space-between',
              position: 'relative',
              overflow: 'hidden',
              cursor: 'pointer',
              borderLeft: p.isDevelopment ? '1px solid var(--border-glass)' : '3px solid var(--accent-primary)',
              transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)'
            }}
            onClick={() => handleNav(p.id)}
            className="home-card"
          >
            {p.isDevelopment && (
              <div style={{
                position: 'absolute',
                top: '12px',
                right: '12px',
                background: 'rgba(255, 170, 0, 0.08)',
                border: '1px solid rgba(255, 170, 0, 0.2)',
                color: 'var(--accent-gold)',
                padding: '2px 8px',
                borderRadius: '6px',
                fontSize: '0.72rem',
                fontWeight: 700
              }}>
                LOCK • {p.targetDate}
              </div>
            )}

            <div>
              <div style={{ fontSize: '2.5rem', marginBottom: '12px' }}>{p.icon || '🛠️'}</div>
              <h3 style={{ fontSize: '1.25rem', fontWeight: 700, marginBottom: '8px', color: 'var(--text-white)' }}>
                {p.label}
              </h3>
              <p style={{ fontSize: '0.9rem', color: 'var(--text-main)', lineHeight: '1.5', marginBottom: '24px' }}>
                {p.description}
              </p>
            </div>

            <div>
              {p.isDevelopment ? (
                <div style={{
                  display: 'flex',
                  alignItems: 'center',
                  gap: '8px',
                  color: 'var(--accent-gold)',
                  fontWeight: 600,
                  fontSize: '0.9rem'
                }}>
                  <span>Preview Wireframe</span>
                  <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3">
                    <line x1="5" y1="12" x2="19" y2="12"></line>
                    <polyline points="12 5 19 12 12 19"></polyline>
                  </svg>
                </div>
              ) : (
                <div style={{
                  display: 'flex',
                  alignItems: 'center',
                  gap: '8px',
                  color: 'var(--accent-primary)',
                  fontWeight: 600,
                  fontSize: '0.9rem'
                }}>
                  <span>Launch Tool</span>
                  <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3">
                    <line x1="5" y1="12" x2="19" y2="12"></line>
                    <polyline points="12 5 19 12 12 19"></polyline>
                  </svg>
                </div>
              )}
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}
