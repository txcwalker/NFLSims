// Shared betting-line formatting helpers for the DFS UI.
//
// Convention used everywhere on the site: a point spread is always stated
// FROM THE HOME TEAM'S SIDE.
//   negative -> home favored     ("CAR -2.5")
//   positive -> home underdog    ("CAR +5.7")
//   zero     -> pick'em          ("PK")
//
// The nflverse `spread_line` field on the schedule is the *home margin*
// (positive when the home team is favored), so the home-side line the UI
// wants is simply its negative: homeSideLine = -spread_line.
//
// The sim produces average team scores; its home-side line is
// (away_avg_score - home_avg_score), which is the same framing.

/**
 * Format a home-side point spread.
 * Input: number (points, home side) or null/NaN.
 * Output: display string ("+5.7", "-2.5", "PK", or "--").
 */
export function fmtSpreadNum(n) {
  if (n == null || !isFinite(n)) return '--';
  const r = Math.round(n * 10) / 10;
  if (r === 0) return 'PK';
  return r > 0 ? `+${r.toFixed(1)}` : r.toFixed(1);
}

/**
 * American moneyline implied by a win probability.
 * Input: p in (0, 1). Favorite (p >= .5) -> negative, underdog -> positive.
 * Rounded to the nearest 5 the way a sportsbook posts it.
 * Output: integer, or null for degenerate probabilities.
 */
export function probToAmericanML(p) {
  if (p == null || !isFinite(p) || p <= 0 || p >= 1) return null;
  const ml = p >= 0.5 ? (-100 * p) / (1 - p) : (100 * (1 - p)) / p;
  return Math.round(ml / 5) * 5;
}

/**
 * Format an American moneyline for display.
 * Input: number (e.g. -148, 124) or null.
 * Output: "-148", "+124", or "--".
 */
export function fmtML(ml) {
  if (ml == null || !isFinite(ml)) return '--';
  const v = Math.round(ml);
  return v > 0 ? `+${v}` : `${v}`;
}
