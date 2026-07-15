import { useState } from 'react'

export default function Roadmap() {
  const [expandedItems, setExpandedItems] = useState({});

  const toggleExpand = (id) => {
    setExpandedItems(prev => ({
      ...prev,
      [id]: !prev[id]
    }));
  };

  const timelineData = [
    {
      tier: "Tier 1: Core Simulation Engine",
      progress: 75,
      accent: "var(--accent-primary)",
      description: "Everything in the platform is downstream of a validated season simulator. This engine models physical football games play-by-play.",
      items: [
        {
          id: "t1_event",
          title: "Play-by-play Event Loop",
          status: "COMPLETED",
          badge: "🟢",
          description: "Calculates downs, yards, scoring events, and play clock intervals. Serves as the master scheduler for the simulation execution.",
          details: "Built in Python. Handles transitions between drives, turnovers, kickoffs, punts, and standard offensive downs. Validated for loop-termination and deadlock-free runs."
        },
        {
          id: "t1_yardage",
          title: "Yardage Models (Air Yards, Rush, YAC)",
          status: "COMPLETED",
          badge: "🟢",
          description: "Generates distributions of yards gained per rush attempt, throw distance, and yards-after-catch/contact.",
          details: "Incorporates random forest regressions and kernel density estimations trained on historical NextGen stats. Fully integrated into player DNA overrides."
        },
        {
          id: "t1_dna",
          title: "Player & Coach DNA Dictionaries",
          status: "COMPLETED",
          badge: "🟢",
          description: "Maintains files mapping offensive tendencies, player skills, and coach decision rates.",
          details: "Dictionaries are loaded on server boot. Controls base characteristics for team tempo, pass vs run ratio, catch rates, and touchdown distributions."
        },
        {
          id: "t1_clock",
          title: "Clock Physics & Kickoff Hardening",
          status: "IN_PROGRESS",
          badge: "🟡",
          description: "Refining timing equations for end-of-quarter spikes, dynamic tempo clocks, and special teams bounds.",
          details: "Eliminates edge-case time leaks. Hardening the transition between the two-minute warning and hurry-up offenses to ensure realistic play counts."
        },
        {
          id: "t1_season",
          title: "Season-long Simulator (Week 1 -> 18)",
          status: "COMPLETED",
          badge: "🟢",
          description: "Simulates full 18-week schedules, tracking divisional standings and playoff seedings dynamically.",
          details: "Calculates cumulative team wins, divisional win rates, and wildcard structures. Ready to receive initial standing overrides for Season-from-Week-X simulation."
        },
        {
          id: "t1_validation",
          title: "System Validation vs. 2024 Actuals",
          status: "PENDING",
          badge: "⚪",
          description: "Running back-tests using 2024 starting rosters to evaluate simulated season records against actual 2024 outcomes.",
          details: "Final gate before public release. Calibrates standard deviation bounds to ensure simulated distributions cover actual historic outliers."
        }
      ]
    },
    {
      tier: "Tier 2: DFS & Betting Integration",
      progress: 10,
      accent: "var(--accent-secondary)",
      description: "Applies simulated game distributions to fantasy projections, book lines, and lineup builders.",
      items: [
        {
          id: "t2_4th",
          title: "4th Down Bot Decision Logic",
          status: "COMPLETED",
          badge: "🟢",
          description: "Calculates mathematically optimal choices (Go, FG, Punt) at any spot on the field.",
          details: "Integrates expected points (EP) and simulated win probability (WP) models. Ready for live game integration and bot auto-posting."
        },
        {
          id: "t2_wp",
          title: "Win Probability Live Tracker",
          status: "IN_PROGRESS",
          badge: "🟡",
          description: "Renders in-game win probability curves by feeding real-time situation metrics into the simulator.",
          details: "Currently training the neural-net evaluator on historical game situations. Designed to update after every play snap in under 50ms."
        },
        {
          id: "t2_proj",
          title: "DFS Projections Generator",
          status: "PENDING",
          badge: "⚪",
          description: "Aggregates Monte Carlo runs into fantasy point distributions for DraftKings and FanDuel.",
          details: "Calculates average points, standard deviation, and floor/ceiling percentages (e.g., odds of scoring 20+ points)."
        },
        {
          id: "t2_opt",
          title: "DFS Lineup Optimizer (Knapsack Solver)",
          status: "PENDING",
          badge: "⚪",
          description: "Builds mathematical models to select optimal lineups within budget constraints.",
          details: "Integrates DraftKings and FanDuel salary restrictions. Will support CSV lineup exporting directly for DFS portal uploads."
        },
        {
          id: "t2_odds",
          title: "Sportsbook Odds Live API Integration",
          status: "PENDING",
          badge: "⚪",
          description: "Connects to active sportsbook feeds to retrieve live spreads, totals, and player props.",
          details: "Queries book feeds dynamically. Enables comparing bookmaker odds to simulated probabilities in real-time."
        },
        {
          id: "t2_edge",
          title: "Sim vs. Book Value Edge Finder",
          status: "PENDING",
          badge: "⚪",
          description: "Highlights odds gaps where simulated probabilities indicate sportsbook lines are mispriced.",
          details: "Sortable dashboard displaying positive expected value (+EV) percentages for game totals, spreads, and player prop limits."
        }
      ]
    },
    {
      tier: "Tier 3: Live Analytics & Contracts",
      progress: 0,
      accent: "var(--text-muted)",
      description: "Advanced metrics and analytics wrappers for roster evaluation, coaches, and contract evaluation.",
      items: [
        {
          id: "t3_war",
          title: "Wins Above Replacement (WAR) Metric",
          status: "PENDING",
          badge: "⚪",
          description: "Defines positional replacement baselines and calculates individual player win contributions.",
          details: "Runs simulations with and without selected players to calculate the direct impact of marginal performance on season wins."
        },
        {
          id: "t3_contract",
          title: "Roster Contract Value Evaluator",
          status: "PENDING",
          badge: "⚪",
          description: "Weighs simulated player WAR metrics against salary cap figures to score contract efficiencies.",
          details: "Identifies overpaid/underpaid athletes and drafts contract recommendations for franchise rosters."
        },
        {
          id: "t3_chess",
          title: "Chess Situational Evaluator",
          status: "PENDING",
          badge: "⚪",
          description: "Interactive chess-like builder scoring expected value based on play selection under specific down & distance constraints.",
          details: "Allows users to construct arbitrary situations and test outcomes independent of team score differences."
        },
        {
          id: "t3_awards",
          title: "Award Odds Simulator (MVP, OPOY)",
          status: "PENDING",
          badge: "⚪",
          description: "Projects seasonal award probabilities by feeding simulation outputs into award historical voting models.",
          details: "Updates awards grids (MVP, Rookie of the Year, Coach of the Year) dynamically as simulated season weeks progress."
        }
      ]
    }
  ];

  return (
    <div style={{ flexGrow: 1, paddingBottom: '20px' }}>
      <section className="glass-panel" style={{ marginBottom: '30px' }}>
        <h1 style={{ marginBottom: '16px' }}>Project Roadmap & Living Map</h1>
        <p style={{ lineHeight: '1.6', color: 'var(--text-main)' }}>
          Follow the progress of the NFLSims platform. Each item is sequenced by physical engine dependencies. Below is the living record of completed work, active tasks, and upcoming milestones. Click on any item to view detailed technical specifications and status parameters.
        </p>
      </section>

      {/* TIER SECTIONS */}
      {timelineData.map((tierData, idx) => (
        <div key={idx} style={{ marginBottom: '40px' }}>
          {/* Tier Header with Progress */}
          <div className="glass-panel" style={{
            borderLeft: `4px solid ${tierData.accent}`,
            marginBottom: '20px',
            padding: '20px 24px'
          }}>
            <div className="flex-between" style={{ flexWrap: 'wrap', gap: '10px' }}>
              <div>
                <h2 style={{ color: 'var(--text-white)' }}>{tierData.tier}</h2>
                <p style={{ fontSize: '0.9rem', color: 'var(--text-muted)', marginTop: '4px' }}>
                  {tierData.description}
                </p>
              </div>
              <div style={{ display: 'flex', alignItems: 'center', gap: '12px' }}>
                <span style={{ fontSize: '1rem', fontWeight: 800, color: tierData.accent, fontFamily: 'monospace' }}>
                  {tierData.progress}%
                </span>
                <div style={{ width: '120px', height: '10px', background: 'rgba(255,255,255,0.06)', borderRadius: '5px', overflow: 'hidden' }}>
                  <div style={{ width: `${tierData.progress}%`, height: '100%', background: tierData.accent }}></div>
                </div>
              </div>
            </div>
          </div>

          {/* Timeline Cards Grid */}
          <div style={{
            display: 'flex',
            flexDirection: 'column',
            gap: '16px',
            paddingLeft: '20px',
            borderLeft: `2px solid ${tierData.accent}`,
            marginLeft: '10px'
          }}>
            {tierData.items.map(item => {
              const isOpen = !!expandedItems[item.id];
              const getStatusColor = () => {
                if (item.status === 'COMPLETED') return 'var(--accent-green)';
                if (item.status === 'IN_PROGRESS') return 'var(--accent-primary)';
                return 'var(--text-muted)';
              };

              return (
                <div 
                  key={item.id}
                  className="glass-panel"
                  style={{
                    padding: '16px 20px',
                    cursor: 'pointer',
                    background: isOpen ? 'var(--bg-glass-active)' : 'var(--bg-glass)',
                    borderColor: isOpen ? tierData.accent : 'var(--border-glass)',
                    transition: 'all 0.3s'
                  }}
                  onClick={() => toggleExpand(item.id)}
                >
                  <div className="flex-between">
                    <h3 style={{
                      fontSize: '1.05rem',
                      fontWeight: 700,
                      color: 'var(--text-white)',
                      display: 'flex',
                      alignItems: 'center',
                      gap: '8px'
                    }}>
                      <span>{item.badge}</span>
                      {item.title}
                    </h3>
                    <span style={{
                      fontSize: '0.75rem',
                      fontWeight: 800,
                      color: getStatusColor(),
                      fontFamily: 'monospace',
                      letterSpacing: '0.5px'
                    }}>
                      {item.status.replace('_', ' ')}
                    </span>
                  </div>

                  <p style={{
                    fontSize: '0.88rem',
                    color: 'var(--text-main)',
                    marginTop: '8px',
                    lineHeight: '1.4'
                  }}>
                    {item.description}
                  </p>

                  {/* Collapsible details drawer */}
                  {isOpen && (
                    <div style={{
                      marginTop: '16px',
                      paddingTop: '16px',
                      borderTop: '1px solid rgba(255,255,255,0.06)',
                      fontSize: '0.85rem',
                      color: 'var(--text-muted)',
                      lineHeight: '1.5',
                      animation: 'fadeIn 0.2s'
                    }}>
                      <div style={{ color: 'var(--text-white)', fontWeight: 600, marginBottom: '4px' }}>Technical Parameters:</div>
                      {item.details}
                    </div>
                  )}
                </div>
              );
            })}
          </div>
        </div>
      ))}
    </div>
  )
}
