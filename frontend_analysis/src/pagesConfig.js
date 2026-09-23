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
    label: 'Current Season',
    showInNavbar: true,
    icon: '🏆',
    description: 'Real, actual 2026 stats to date -- standings, league leaders, team stats, and the current week\'s matchup(s).'
  },
  {
    id: 'season-2026',
    label: '2026 Rest of Season',
    showInNavbar: true,
    icon: '🏈',
    description: 'Additive 2026 projection: real results through the last completed week plus simulated projections for the rest -- standings, league leaders, team stats, matchup win probabilities, and per-team usage/matchups.'
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
  },
  {
    id: 'bot-feed',
    label: 'Bot Test Feed',
    showInNavbar: true,
    icon: '🤖',
    description: 'Live 4th-down bot review feed — every evaluated decision (posted or skipped) in DRY_RUN mode, for pre-launch verification.'
  }
];
