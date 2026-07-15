import { useState } from 'react'

export default function About() {
  const [activeStep, setActiveStep] = useState(0);

  const steps = [
    {
      title: "1. Pre-Snap Check",
      subtitle: "Penalties & Alignment",
      description: "Before the snap, the engine calculates pre-snap penalty frequencies (e.g., False Start, Offside) based on team discipline ratings and offensive tempo. Personnel groupings (11 vs. 12 vs. 21 personnel) are set based on team coach DNA profiles.",
      parameters: "Team Penalty Rates, Coach Personnel Maps, Stadium Noise Factor"
    },
    {
      title: "2. Play Selection",
      subtitle: "Pass vs. Run Choice",
      description: "The engine determines the play call (Pass, Rush, or Play-Action). This decision is state-dependent, factoring in the current quarter, score differential, down, distance-to-gain, field position, and team coach PROE (Pass Rate Over Expected) settings.",
      parameters: "PROE Sliders, Score Differential, Down & Distance, Quarter, Time Remaining"
    },
    {
      title: "3. Execution & Depth",
      subtitle: "Throw Depth & Target",
      description: "For passing plays, the quarterback selects a target based on player target shares. The throw depth is simulated using the team's Deep Shot tendency and the QB's baseline depth traits (Air Yards model). For rushing plays, the carry is allocated based on team carry shares.",
      parameters: "Target Share %, Carry Share %, Deep Shot Sliders, Air Yards Coefficients"
    },
    {
      title: "4. Tackle & YAC",
      subtitle: "Evasion & YAC Physics",
      description: "Once the ball is caught or carried, the engine simulates yards gained. Rushing plays calculate yards-after-contact, while passing plays simulate completion odds (using player Catch Rate overrides) and Yards After Catch (YAC model) based on defender positioning.",
      parameters: "Catch Rate %, Yards-after-Contact, YAC Probability Density"
    },
    {
      title: "5. Post-Snap Chaos",
      subtitle: "Sacks, Pressures & TOs",
      description: "During the play, the engine checks for chaos events. Defensive pressure (affected by team Pressure Allowed settings) can result in sacks, throwaways, or forced turnovers (interceptions, fumbles) modeled using player and team defensive coefficients.",
      parameters: "Pressure Allowed Slider, Sacks/Interceptions Baselines, Player Fumble Rates"
    },
    {
      title: "6. State Update",
      subtitle: "Clock Physics & Downs",
      description: "After the play terminates, the clock ticks down based on whether the play ended in-bounds, incomplete, or with a timeout. The ball is spotted, down & distance are recalculated, and the engine evaluates 4th-down decisions (Go, Punt, FG) if applicable.",
      parameters: "Tempo (Plays/Game) Slider, 4th Down Bot Math, Kickoff/Punt Models"
    }
  ];

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      {/* METHODOLOGY INTRO */}
      <section className="glass-panel" style={{ marginBottom: '30px' }}>
        <h1 style={{ marginBottom: '16px' }}>Simulation Methodology</h1>
        <p style={{ lineHeight: '1.6', fontSize: '1.05rem', color: 'var(--text-main)' }}>
          Traditional NFL projections rely on static seasonal averages. They predict a player will score 14.5 points, but they cannot show you how often they score 30+ points or drop to zero. 
        </p>
        <p style={{ lineHeight: '1.6', fontSize: '1.05rem', color: 'var(--text-main)', marginTop: '12px' }}>
          NFLSims solves this by building a **state-dependent play-by-play Monte Carlo simulator**. The engine models the game sequentially—calculating downs, clocks, yards, and touchdowns play-by-play. By executing this loop **10,000 times** for every matchup, we generate full probability distributions for every player and game outcome.
        </p>
      </section>

      {/* PLAY LIFECYCLE FLOWCHART EXPLORER */}
      <section className="glass-panel" style={{ marginBottom: '30px' }}>
        <h2 style={{ marginBottom: '8px' }}>Interactive Play Lifecycle</h2>
        <p style={{ fontSize: '0.9rem', color: 'var(--text-muted)', marginBottom: '24px' }}>
          Click on any phase below to inspect the variables and mathematical rules that govern a single play's simulation.
        </p>

        {/* Timeline Flow Steps Grid */}
        <div style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(auto-fit, minmax(140px, 1fr))',
          gap: '12px',
          marginBottom: '24px'
        }}>
          {steps.map((step, idx) => (
            <div 
              key={idx}
              onClick={() => setActiveStep(idx)}
              style={{
                background: activeStep === idx ? 'var(--bg-glass-active)' : 'rgba(11, 17, 38, 0.4)',
                border: activeStep === idx ? '1px solid var(--accent-primary)' : '1px solid var(--border-glass)',
                boxShadow: activeStep === idx ? 'var(--glow-cyan)' : 'none',
                borderRadius: '12px',
                padding: '16px 12px',
                textAlign: 'center',
                cursor: 'pointer',
                transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)',
                userSelect: 'none'
              }}
              className="lifecycle-step-card"
            >
              <div style={{
                fontSize: '0.8rem',
                fontWeight: 800,
                color: activeStep === idx ? 'var(--accent-primary)' : 'var(--text-muted)',
                marginBottom: '6px'
              }}>
                STEP {idx + 1}
              </div>
              <h3 style={{ fontSize: '0.95rem', fontWeight: 700, color: 'var(--text-white)', marginBottom: '4px' }}>
                {step.subtitle}
              </h3>
            </div>
          ))}
        </div>

        {/* Expanded Step Detail Box */}
        <div style={{
          background: 'rgba(11, 17, 38, 0.6)',
          border: '1px solid var(--border-glass)',
          borderRadius: '12px',
          padding: '24px',
          boxShadow: 'inset 0 4px 10px rgba(0,0,0,0.3)',
          transition: 'all 0.3s'
        }}>
          <h3 style={{ fontSize: '1.2rem', fontWeight: 700, color: 'var(--accent-primary)', marginBottom: '10px' }}>
            {steps[activeStep].title} — {steps[activeStep].subtitle}
          </h3>
          <p style={{ lineHeight: '1.6', color: 'var(--text-main)', marginBottom: '16px' }}>
            {steps[activeStep].description}
          </p>
          <div style={{
            display: 'flex',
            alignItems: 'center',
            gap: '8px',
            fontSize: '0.85rem',
            color: 'var(--accent-gold)',
            fontWeight: 700,
            background: 'rgba(255, 170, 0, 0.05)',
            border: '1px solid rgba(255, 170, 0, 0.1)',
            padding: '8px 16px',
            borderRadius: '6px',
            width: 'fit-content'
          }}>
            <span>Influencing Parameters:</span>
            <span style={{ color: 'var(--text-white)', fontFamily: 'monospace' }}>{steps[activeStep].parameters}</span>
          </div>
        </div>
      </section>

      {/* DFS SCORING RULES */}
      <section className="glass-panel">
        <h2 style={{ marginBottom: '16px' }}>DFS Scoring Systems</h2>
        <p style={{ fontSize: '0.9rem', color: 'var(--text-main)', marginBottom: '20px' }}>
          Simulated outcomes are converted into fantasy points at the end of each run according to standard sportsbook rule sets.
        </p>

        <div className="table-container">
          <table>
            <thead>
              <tr>
                <th>Stat Category</th>
                <th>DraftKings Points</th>
                <th>FanDuel Points</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Passing Yard</td>
                <td style={{ fontFamily: 'monospace' }}>0.04 pts / yd</td>
                <td style={{ fontFamily: 'monospace' }}>0.04 pts / yd</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Passing Touchdown</td>
                <td style={{ fontFamily: 'monospace' }}>4.0 pts</td>
                <td style={{ fontFamily: 'monospace' }}>4.0 pts</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Passing Interception</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-red)' }}>-1.0 pts</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-red)' }}>-1.0 pts</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Reception (PPR)</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-green)' }}>1.0 pt (Full PPR)</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-gold)' }}>0.5 pt (Half PPR)</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Rushing / Receiving Yard</td>
                <td style={{ fontFamily: 'monospace' }}>0.1 pts / yd</td>
                <td style={{ fontFamily: 'monospace' }}>0.1 pts / yd</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Rushing / Receiving TD</td>
                <td style={{ fontFamily: 'monospace' }}>6.0 pts</td>
                <td style={{ fontFamily: 'monospace' }}>6.0 pts</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Fumble Lost</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-red)' }}>-1.0 pts</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-red)' }}>-2.0 pts</td>
              </tr>
              <tr>
                <td style={{ fontWeight: 700, color: 'var(--text-white)' }}>Yardage Milestone Bonuses</td>
                <td style={{ fontFamily: 'monospace', color: 'var(--accent-primary)' }}>
                  +3.0 pts (300+ Pass Yds)<br />
                  +3.0 pts (100+ Rush/Rec Yds)
                </td>
                <td style={{ fontFamily: 'monospace', color: 'var(--text-muted)' }}>None</td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>
    </div>
  )
}
