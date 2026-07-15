// src/dfsRules.js

export const DFS_RULES = {
  DK_TRADITIONAL: {
    label: 'DraftKings Traditional',
    salaryCap: 50000,
    ppr: 1.0,
    slots: ['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'TE', 'FLEX', 'DST'],
    flexEligible: ['RB', 'WR', 'TE'],
    captainMultiplier: 1.0
  },
  DK_SHOWDOWN: {
    label: 'DraftKings Showdown',
    salaryCap: 50000,
    ppr: 1.0,
    slots: ['CPT', 'FLEX', 'FLEX', 'FLEX', 'FLEX', 'FLEX'],
    flexEligible: ['QB', 'RB', 'WR', 'TE', 'DST'], // DST can be flex, but not Captain
    captainMultiplier: 1.5
  },
  FD_TRADITIONAL: {
    label: 'FanDuel Traditional (Placeholder)',
    salaryCap: 60000,
    ppr: 0.5,
    slots: ['QB', 'RB', 'RB', 'WR', 'WR', 'WR', 'TE', 'FLEX', 'D'],
    flexEligible: ['RB', 'WR', 'TE'],
    captainMultiplier: 1.0
  }
};

export function validateLineup(lineup, rulesKey) {
  const rules = DFS_RULES[rulesKey];
  if (!rules) return { valid: false, error: 'Invalid platform rules' };

  const totalSalary = lineup.reduce((sum, p) => sum + p.salary, 0);
  if (totalSalary > rules.salaryCap) {
    return { valid: false, error: `Salary exceeds limit of $${rules.salaryCap}` };
  }

  // Ensure no duplicate players in the lineup
  const playerNames = lineup.map(p => p.name);
  const uniqueNames = new Set(playerNames);
  if (playerNames.length !== uniqueNames.size) {
    return { valid: false, error: 'Duplicate players selected' };
  }

  return { valid: true, salarySpent: totalSalary };
}
