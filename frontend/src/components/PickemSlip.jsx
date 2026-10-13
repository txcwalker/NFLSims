import { useEffect, useMemo, useState } from 'react';
import { ApiService } from '../api';

/**
 * Underdog Pick'em entry calculators (Pickems > Underdog Lines).
 *
 * Inputs (props): legs -- the slip [{id, team, player, market, line, side, multiplier, pos}] (owned by
 *   PickemLines; "+" on a board row adds a leg); setLegs; week.
 * Data: POST /api/pickems/slip -> joint probability that every leg hits. Same-game legs are counted
 *   together per sim iteration (correlation is real); different games multiply as independent.
 *
 * Calculator 1 -- Entry EV: stake, payout, boost, P(all hit), EV in $ and %.
 * Calculator 2 -- Minimum multiplier: the total payout multiplier the slip needs to break even, and
 *   (product mode) what each leg's own multiplier must be given the others.
 *
 * Payout model: "product" = total multiplier is the product of the legs' multipliers (editable per leg;
 *   fits Underdog's per-leg multiplier display). "override" = type the total the slip shows (use this
 *   when it differs, e.g. a fixed payout table). Boost = X% extra on WINNINGS (payout minus stake);
 *   Underdog's wording can differ -- check a boost's fine print. Flex/insured entries are not modeled.
 */
const box = { padding: '14px', borderRadius: '12px', border: '1px solid var(--border-glass)', flex: '1 1 340px', minWidth: 300 };
const inp = { background: 'rgba(0,0,0,0.25)', border: '1px solid rgba(255,255,255,0.14)', borderRadius: '6px', color: 'var(--text-white)', padding: '4px 7px', fontSize: '0.82rem' };
const muted = { color: 'var(--text-muted)' };

const pct = (v) => (v == null ? '—' : `${(v * 100).toFixed(1)}%`);
const mult = (v) => (v == null || !isFinite(v) ? '—' : `${v.toFixed(2)}x`);
const money = (v) => `${v < 0 ? '-' : ''}$${Math.abs(v).toFixed(2)}`;
/** Decimal multiplier (stake included) -> American odds string. */
const american = (m) => {
  if (m == null || !isFinite(m) || m <= 1) return '—';
  const a = m >= 2 ? Math.round(100 * (m - 1)) : -Math.round(100 / (m - 1));
  return a > 0 ? `+${a}` : `${a}`;
};

export default function PickemSlip({ legs, setLegs, week }) {
  const [stake, setStake] = useState(5);
  const [boost, setBoost] = useState(0);
  const [override, setOverride] = useState('');   // '' = total follows the product of the legs; typed = your number
  const mode = override !== '' ? 'override' : 'product';
  const [prob, setProb] = useState(null);
  const [err, setErr] = useState(null);

  // Re-price the joint probability whenever the set of legs (not their multipliers) changes.
  const key = legs.map(l => `${l.team}|${l.player}|${l.market}|${l.line}|${l.side}`).join(';');
  useEffect(() => {
    if (legs.length === 0) { setProb(null); setErr(null); return; }
    let alive = true;
    ApiService.getPickemSlip(week, legs.map(({ team, player, market, line, side }) => ({ team, player, market, line, side }))).then(res => {
      if (!alive) return;
      if (res.ok) { setProb(res.data); setErr(null); } else { setProb(null); setErr(res.error); }
    });
    return () => { alive = false; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, week]);

  const calc = useMemo(() => {
    if (!prob || legs.length === 0) return null;
    const P = prob.joint;
    const b = (Number(boost) || 0) / 100;
    const s = Number(stake) || 0;
    const prod = legs.reduce((a, l) => a * (Number(l.multiplier) || 0), 1);
    const M = mode === 'override' ? Number(override) || 0 : prod;
    const payout = s * (1 + (1 + b) * (M - 1));          // boost lifts the winnings (payout - stake)
    const ev = P * payout - s;
    const mMin = P > 0 ? 1 + (1 / P - 1) / (1 + b) : null;  // total multiplier where EV = 0
    return { P, b, s, prod, M, payout, ev, mMin };
  }, [prob, legs, stake, boost, mode, override]);

  const setMult = (id, v) => setLegs(ls => ls.map(l => (l.id === id ? { ...l, multiplier: v } : l)));
  const remove = (id) => setLegs(ls => ls.filter(l => l.id !== id));

  return (
    <div className="glass-panel" style={{ padding: '16px', borderRadius: '12px', border: '1px solid var(--border-glass)', marginBottom: '14px' }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 12, marginBottom: 10, flexWrap: 'wrap' }}>
        <b style={{ color: 'var(--text-white)' }}>🧮 Entry calculator</b>
        <span style={{ ...muted, fontSize: '0.8rem' }}>Add legs with “+” on any board row; edit multipliers to match your slip.</span>
        {legs.length > 0 && <button style={{ ...inp, cursor: 'pointer' }} onClick={() => setLegs([])}>Clear slip</button>}
      </div>

      {legs.length === 0 ? (
        <div style={{ ...muted, fontSize: '0.85rem' }}>Slip is empty.</div>
      ) : (
        <>
          <table style={{ borderCollapse: 'collapse', marginBottom: 12, width: '100%', maxWidth: 760 }}>
            <thead><tr>{['Leg', 'Mult (edit)', 'Odds', 'Sim hit %', 'Min mult', ''].map(h => <th key={h} style={{ textAlign: 'left', fontSize: '0.72rem', textTransform: 'uppercase', ...muted, padding: '4px 8px' }}>{h}</th>)}</tr></thead>
            <tbody>
              {legs.map((l, i) => {
                const others = legs.reduce((a, o) => (o.id === l.id ? a : a * (Number(o.multiplier) || 0)), 1);
                const legMin = calc && mode === 'product' && others > 0 ? calc.mMin / others : null;
                return (
                  <tr key={l.id} style={{ borderTop: '1px solid rgba(255,255,255,0.06)' }}>
                    <td style={{ padding: '4px 8px', fontSize: '0.84rem' }}>{l.player} <span style={muted}>{l.side} {l.line} {l.market}</span></td>
                    <td style={{ padding: '4px 8px' }}>
                      <input type="number" step="0.01" min="0" value={l.multiplier} onChange={e => setMult(l.id, e.target.value)} style={{ ...inp, width: 70 }} />
                    </td>
                    <td style={{ padding: '4px 8px', ...muted, fontSize: '0.82rem' }}>{american(Number(l.multiplier))}</td>
                    <td style={{ padding: '4px 8px', fontSize: '0.84rem' }}>{pct(prob?.singles?.[i])}</td>
                    <td style={{ padding: '4px 8px', fontSize: '0.84rem', fontWeight: 600 }} title="Multiplier this leg needs for the whole slip to break even, given the other legs' multipliers">
                      {mode === 'product' ? <>{mult(legMin)} <span style={{ ...muted, fontWeight: 400 }}>({american(legMin)})</span></> : '—'}
                    </td>
                    <td style={{ padding: '4px 8px' }}><button style={{ ...inp, cursor: 'pointer', padding: '1px 7px' }} onClick={() => remove(l.id)}>✕</button></td>
                  </tr>
                );
              })}
            </tbody>
          </table>

          {err && <div style={{ color: '#f87171', fontSize: '0.82rem', marginBottom: 8 }}>{err}</div>}

          <div style={{ display: 'flex', gap: 14, flexWrap: 'wrap' }}>
            {/* Calculator 1: Entry EV */}
            <div style={box}>
              <div style={{ fontWeight: 700, marginBottom: 8, color: 'var(--text-white)' }}>Entry EV</div>
              <div style={{ display: 'flex', gap: 10, flexWrap: 'wrap', alignItems: 'center', fontSize: '0.82rem', marginBottom: 8 }}>
                <label>Stake $ <input type="number" min="0" step="1" value={stake} onChange={e => setStake(e.target.value)} style={{ ...inp, width: 64 }} /></label>
                <label>Boost % <input type="number" min="0" step="5" value={boost} onChange={e => setBoost(e.target.value)} style={{ ...inp, width: 58 }} /></label>
              </div>
              <div style={{ display: 'flex', gap: 10, flexWrap: 'wrap', alignItems: 'center', fontSize: '0.82rem', marginBottom: 10 }}>
                <label>Total multiplier (before boost){' '}
                  <input type="number" step="0.01" min="0" value={override !== '' ? override : (calc ? calc.prod.toFixed(2) : '')}
                    onChange={e => setOverride(e.target.value)} style={{ ...inp, width: 80 }} />
                </label>
                {override !== '' ? <button style={{ ...inp, cursor: 'pointer' }} onClick={() => setOverride('')}>Reset to product of legs</button>
                  : <span style={muted}>auto = product of legs; type to override</span>}
              </div>
              {calc ? (
                <div style={{ fontSize: '0.88rem', lineHeight: 1.7 }}>
                  <div>Total multiplier: <b>{mult(calc.M)}</b> <span style={muted}>({american(calc.M)})</span></div>
                  <div>Payout if all hit: <b>{money(calc.payout)}</b>{calc.b > 0 && <span style={muted}> (with {boost}% boost)</span>}</div>
                  <div>P(all hit): <b>{pct(calc.P)}</b> <span style={muted}>
                    {prob.games < legs.length ? `same-game legs counted together; independent would be ${pct(prob.independent)}` : 'all different games'}</span></div>
                  <div style={{ fontSize: '1.05rem', fontWeight: 700, color: calc.ev > 0 ? '#34d399' : '#f87171' }}>
                    EV {calc.ev >= 0 ? '+' : ''}{money(calc.ev)} ({calc.s > 0 ? `${calc.ev >= 0 ? '+' : ''}${(calc.ev / calc.s * 100).toFixed(1)}%` : '—'})
                  </div>
                </div>
              ) : <div style={muted}>{err ? '' : 'Pricing…'}</div>}
            </div>

            {/* Calculator 2: minimum multiplier */}
            <div style={box}>
              <div style={{ fontWeight: 700, marginBottom: 8, color: 'var(--text-white)' }}>Minimum multiplier for +EV</div>
              {calc ? (
                <div style={{ fontSize: '0.88rem', lineHeight: 1.7 }}>
                  <div>P(all hit): <b>{pct(calc.P)}</b></div>
                  <div>Slip must pay at least: <b style={{ fontSize: '1.05rem' }}>{mult(calc.mMin)}</b> <span style={muted}>({american(calc.mMin)})</span>
                    {calc.b > 0 && <span style={muted}> after the {boost}% boost</span>}</div>
                  <div>Slip pays: <b>{mult(calc.M)}</b> —
                    <b style={{ color: calc.M >= calc.mMin ? '#34d399' : '#f87171' }}> {calc.M >= calc.mMin ? 'clears it' : 'short of it'}</b>
                  </div>
                  <div style={{ ...muted, fontSize: '0.76rem', marginTop: 6 }}>
                    {mode === 'product'
                      ? 'Per-leg “Min mult” in the table above = what that leg alone must pay, holding the other legs’ multipliers fixed.'
                      : 'Per-leg minimums need “Product of legs” mode (a typed slip total has no per-leg split).'}
                  </div>
                </div>
              ) : <div style={muted}>{err ? '' : 'Pricing…'}</div>}
            </div>
          </div>
          <div style={{ ...muted, fontSize: '0.74rem', marginTop: 8 }}>
            EV assumes our sims are right. Whole-number lines count a push as a miss. Boost = extra % on winnings (check its fine print). Flex/insured entries are not modeled.
          </div>
        </>
      )}
    </div>
  );
}
