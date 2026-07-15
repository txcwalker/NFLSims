// src/pagesConfig.js
export const PAGES = [
  {
    id: 'home',
    label: 'Home',
    showInNavbar: true,
    icon: '📊',
    description: 'Tactical Dashboard showing live game metrics, season standings projections, and analytical highlights.'
  },
  {
    id: 'standings',
    label: 'Full Standings',
    showInNavbar: true,
    icon: '🏆',
    description: 'Simulated records and playoff probability outcomes for all 32 NFL teams divided by division.'
  },
  {
    id: 'live-wp',
    label: 'Live Win Probability',
    showInNavbar: true,
    icon: '📈',
    description: 'Dynamic play-by-play line charts and leverage index streams tracking live game momentum.'
  },
  {
    id: 'fourth-downs',
    label: '4th Down Explorer',
    showInNavbar: true,
    icon: '🎯',
    description: 'Situational calculator and live feed matching decisions against conversion models.'
  },
  {
    id: 'game-summary',
    label: 'Game Center',
    showInNavbar: false,
    icon: '🏈',
    description: 'Integrated match dashboard containing play graphs, stats, 4th down history, and next-play recommendations.'
  },
  {
    id: 'historical-lab',
    label: '⚗ Testing Lab',
    showInNavbar: true,
    icon: '⚗',
    description: 'Internal testing lab — chess evaluator and suggested lines over Week 1 2025 games. Remove before launch.'
  }
];
