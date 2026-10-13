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
    label: 'Game Explorer',
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
    id: 'shortlist',
    label: 'Shortlist',
    showInNavbar: true,
    category: 'tool',
    icon: '🗂️',
    description: 'Lineups you saved from the Optimizer during the week, cut down into your final entries with the same exposure and drill-down tools and a DK upload export.'
  },
  {
    id: 'showdown_optimizer',
    label: 'Showdown Optimizer',
    showInNavbar: true,
    category: 'tool',
    icon: '✧',
    description: 'Single-game Showdown (CPT + 5 FLEX) lineup builder with a leverage dial that trades ceiling for lower total lineup ownership.'
  },
  // Evaluation dropdown (2026-09-29): one page per model-accuracy grader,
  // all rendered by pages/EvaluationPage.jsx. DFS contest review lives on
  // Sim Replays; paper-trade results on Bankroll.
  {
    id: 'eval_games',
    label: 'Game Lines',
    showInNavbar: true,
    category: 'eval',
    icon: '🎲',
    description: 'Sim spreads, totals and win probabilities graded against opening/closing Vegas lines and the actual results -- accuracy, ATS/total/moneyline records, calibration and CLV.'
  },
  {
    id: 'eval_players',
    label: 'Player Projections',
    showInNavbar: true,
    category: 'eval',
    icon: '🏃',
    description: 'Every QB/RB/WR/TE projection graded against the real stat line -- interval coverage, bias, error and where each player finished inside their own sim distribution.'
  },
  {
    id: 'eval_rankings',
    label: 'Rankings',
    showInNavbar: true,
    category: 'eval',
    icon: '🏆',
    description: 'Weekly positional rankings graded against actual finishes in season-long scoring formats -- rank correlation, top-12 hit rates and probability skill.'
  },
  {
    id: 'eval_pool',
    label: 'Lineup Pool',
    showInNavbar: true,
    category: 'eval',
    icon: '🎰',
    description: 'Every lineup the Optimizer generated pre-lock, played or not, graded against the real $1 First Down field -- calibration of predicted ITM/top-1%/EV, stale-lineup decay, exposure gaps and build-by-build results.'
  },
  // Pickems dropdown (2026-10-03): Underdog Pick'em lines pasted by hand (their
  // ToS only allows manual access, so no scraper) and graded against our sims.
  {
    id: 'pickem_lines',
    label: 'Underdog Lines',
    showInNavbar: true,
    category: 'pickems',
    icon: '🐕',
    description: "Paste the Underdog Pick'em board and see which legs our sims say are +EV, with the minimum multiplier worth taking."
  },
  {
    id: 'pickem_results',
    label: 'Underdog Grading',
    showInNavbar: true,
    category: 'pickems',
    icon: '🧾',
    description: 'Underdog legs settled against real stat lines at the opening and closing multipliers -- do our +EV legs pay, and is our probability better than the market\'s?'
  },
  {
    id: 'sim_replays',
    label: 'Sim Replays',
    showInNavbar: true,
    category: 'tool',
    icon: '🔁',
    description: 'Pick a contest you actually entered and see your submitted lineup(s) -- real points and rank -- next to our sim\'s own pre-game projected distribution for that exact lineup. Also holds each showdown game\'s field analysis and field sim replay.'
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
    description: 'Median, ceiling, optimal-lineup rate, projected ownership, and box-score volume stats (attempts/yards/TDs/catches) for every player -- leverage = optimal% - own%.'
  },
  {
    id: 'props',
    label: 'Prop Bet Finder',
    showInNavbar: true,
    category: 'tool',
    // Draft 2026-09-26: live Polymarket US prop ladders vs. our sims (pages/PropMarkets.jsx).
    // The old isDevelopment mock preview in InDevelopment.jsx no longer renders for this id.
    icon: '🎯',
    description: 'Compare our simulated player-stat probabilities with live prediction-market prop prices (Polymarket US first; Kalshi later).'
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
