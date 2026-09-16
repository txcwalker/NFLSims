// src/pagesConfig.js
export const PAGES = [
  {
    id: 'home',
    label: 'Home',
    showInNavbar: false,
    category: 'core',
    description: 'Welcome portal for NFLSims'
  },
  {
    id: 'simulator',
    label: 'DFS Simulator',
    showInNavbar: true,
    category: 'core',
    icon: '🏈',
    description: 'Run 10,000 parallel Monte Carlo simulations to find weekly game outcomes and player statistics.'
  },
  {
    id: 'slate_leaders',
    label: 'Weekly Summary',
    showInNavbar: true,
    category: 'core',
    icon: '📊',
    description: 'Weekly summary of simulation averages and percentiles across all games and slates.'
  },
  {
    id: 'dfs_summary',
    label: 'Slate DFS Summary',
    showInNavbar: false,
    category: 'core',
    icon: '📊',
    description: 'Projections & target summary for the slate.'
  },
  {
    id: 'optimizer',
    label: 'DFS Optimizer',
    showInNavbar: true,
    category: 'tool',
    icon: '⚡',
    description: 'Build optimal DraftKings and FanDuel lineups using mathematical constraints (knapsack solver) on simulated projections.'
  },
  {
    id: 'showdown_optimizer',
    label: 'Showdown Optimizer',
    showInNavbar: true,
    category: 'tool',
    icon: '✧',
    description: 'Single-game Showdown (CPT + 5 FLEX) lineup builder with a leverage dial that trades ceiling for lower total lineup ownership.'
  },
  {
    id: 'evaluation',
    label: 'Evaluation',
    showInNavbar: true,
    category: 'tool',
    icon: '🧪',
    description: 'Field analysis for settled contests (winner, percentile cutoffs, hindsight-optimal, top-finisher ownership) and paper-trade results (predicted vs actual for lineups you flagged as playing).'
  },
  {
    id: 'sim_replays',
    label: 'Sim Replays',
    showInNavbar: true,
    category: 'tool',
    icon: '🔁',
    description: 'Pick a contest you actually entered and see your submitted lineup(s) -- real points and rank -- next to our sim\'s own pre-game projected distribution for that exact lineup.'
  },
  {
    id: 'bankroll',
    label: 'Bankroll',
    showInNavbar: true,
    category: 'tool',
    icon: '🏦',
    description: 'Bankroll tracking across every paper and real account -- cost, settled winnings, net P&L, and ROI rolled up from the Builds tagged to each.'
  },
  {
    id: 'cash_lineups',
    label: 'Cash Lineups',
    showInNavbar: true,
    category: 'tool',
    icon: '💰',
    description: "This slate's own top cash-optimal (pure median, zero-variance) lineup builds -- also the consensus signal feeding ownership projections."
  },
  {
    id: 'leverage',
    label: 'Ownership Leverage',
    showInNavbar: true,
    category: 'tool',
    icon: '🎯',
    description: 'Median, ceiling, optimal-lineup rate, and projected ownership for every player -- leverage = optimal% - own%.'
  },
  {
    id: 'props',
    label: 'Prop Bet Finder',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'June 2027',
    icon: '🎯',
    description: 'Identify value edges by comparing simulated player performance thresholds with live sportsbook odds.'
  },
  {
    id: 'war',
    label: 'WAR Metric',
    showInNavbar: true,
    category: 'tool',
    isDevelopment: true,
    targetDate: 'Jan 2027',
    icon: '🛡️',
    description: 'Evaluate individual player impact on team wins compared to a replacement-level baseline in the NFL.'
  },
  {
    id: 'about',
    label: 'Methodology',
    showInNavbar: true,
    category: 'meta',
    icon: '📚',
    description: 'Deep dive into the underlying physics of the simulation engine.'
  },
  {
    id: 'roadmap',
    label: 'Roadmap',
    showInNavbar: true,
    category: 'meta',
    icon: '🗺️',
    description: 'Living map and progress log of all project components.'
  }
];
