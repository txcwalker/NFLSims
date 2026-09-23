// Shared helpers for the season2026 tab components -- used by both the
// "2026 Rest of Season" page (Season2026.jsx, simulated/additive) and the
// "Current Season" page (CurrentSeason.jsx, real stats), which render the
// exact same 5 tabs off different data sources.
import { useState, useMemo } from 'react';

// Column-sum of a usage table. Volume stats are additive across players, so a
// plain sum of the per-player medians is a fine at-a-glance team total (the
// median-vs-mean gap on these counting stats is <1%). Ratio stats (Cmp %) are
// recomputed from the summed numerator/denominator, never summed directly.
export const sumKey = (rows, k) => (rows || []).reduce((s, r) => s + (Number(r[k]) || 0), 0);
export const fmt1 = (n) => (Math.round(n * 10) / 10).toFixed(1);

// --- Betting-line helpers (all lines rendered home-team-relative) ----------
// A point spread stated FROM THE HOME TEAM'S SIDE: negative = home favored
// ("CAR -2.5"), positive = home underdog ("CAR +5.7"), 0 = pick'em.
export const fmtSpread = (n) => {
  if (n == null || !isFinite(n)) return null;
  const r = Math.round(n * 10) / 10;
  if (r === 0) return 'PK';
  return r > 0 ? `+${r.toFixed(1)}` : r.toFixed(1);
};
// American moneyline implied by a win probability p (0..1). Favorite (p >= .5)
// comes back negative, underdog positive, rounded to the nearest 5 the way a
// book posts it. null for degenerate probabilities.
export const probToAmericanML = (p) => {
  if (p == null || !isFinite(p) || p <= 0 || p >= 1) return null;
  const ml = p >= 0.5 ? (-100 * p) / (1 - p) : (100 * (1 - p)) / p;
  return Math.round(ml / 5) * 5;
};
export const fmtML = (ml) => {
  if (ml == null || !isFinite(ml)) return '—';
  const v = Math.round(ml);
  return v > 0 ? `+${v}` : `${v}`;
};

export function useSortableData(rows, initialKey, initialDir = 'desc') {
  const [sortKey, setSortKey] = useState(initialKey);
  const [sortDir, setSortDir] = useState(initialDir);

  const sorted = useMemo(() => {
    if (!rows) return [];
    const copy = [...rows];
    copy.sort((a, b) => {
      const av = a[sortKey], bv = b[sortKey];
      if (typeof av === 'string') return sortDir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
      return sortDir === 'asc' ? (av ?? 0) - (bv ?? 0) : (bv ?? 0) - (av ?? 0);
    });
    return copy;
  }, [rows, sortKey, sortDir]);

  const toggleSort = (key) => {
    if (key === sortKey) setSortDir(d => (d === 'asc' ? 'desc' : 'asc'));
    else { setSortKey(key); setSortDir('desc'); }
  };

  return { sorted, sortKey, sortDir, toggleSort };
}

export function SortableTh({ label, sortKeyName, activeKey, dir, onClick }) {
  return (
    <th style={{ cursor: 'pointer', userSelect: 'none' }} onClick={() => onClick(sortKeyName)}>
      {label}{activeKey === sortKeyName ? (dir === 'asc' ? ' ▲' : ' ▼') : ''}
    </th>
  );
}
