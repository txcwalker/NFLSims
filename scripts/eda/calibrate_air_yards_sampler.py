"""A2 Phase 1: recalibrate the air-yards sampler's post-regression noise and
clip bounds against the real 2021-2025 target-depth distribution.

The tri-gate sampler (models/air_yards_v_0_1_1/inference.py) predicts a point
air-yards per level (screen/std/deep) then adds Gaussian noise and clips. The
noise stds were hand-set (1.5/4.0/12.0) and don't match the regressors' real
residual spread (RMSE 2.2/4.6/7.8); the hard clips ([1,19] / >=20) pile mass at
the 19/20 boundary. Phase 0 measured the damage: sim depth mix too peaked at
5-12 yds, fat 30+ tail, deep completion 15% vs 29% real.

Two passes:
  --capture    run a slate with the sampler's capture hook on, dump the exact
               feature matrices the engine feeds it -> scratchpad/ay_capture.npz
  --calibrate  replay those feature rows through the gate+regressors under a
               grid of (noise_std x clip) candidates, score each against the
               real bucket distribution, print the best. Writes nothing.

Then hand-edit metadata.json's "sampling" block to the chosen params and re-run
scripts/eda/measure_sim_completion_by_depth.py to confirm in a live sim.

Usage:
    venv\\Scripts\\python.exe scripts/eda/calibrate_air_yards_sampler.py --capture [weeks] [iters]
    venv\\Scripts\\python.exe scripts/eda/calibrate_air_yards_sampler.py --calibrate
"""
import os
import sys
import numpy as np

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

SCRATCH = os.environ.get("CLAUDE_SCRATCH", "scratchpad")
CAP_PATH = os.path.join(SCRATCH, "ay_capture.npz")

# Real 2021-2025, from analyze_completion_by_depth.py (att %, buckets:
# <=0, 0-5, 5-10, 10-15, 15-20, 20-30, 30+)
REAL_ATT_PCT = np.array([21.9, 30.5, 18.2, 11.5, 7.7, 6.2, 4.0])
REAL_MEAN_AY = 8.0            # attempt-weighted, roughly
BUCKET_EDGES = np.array([0, 5, 10, 15, 20, 30], dtype=np.float32)
# per-bucket objective weight -- the tails are where it's broken
W = np.array([1.0, 1.0, 1.2, 1.2, 1.5, 1.5, 2.0])

# The real conditional air-yards distribution within the std and deep levels is
# right-skewed (far more short throws than long), so a symmetric Gaussian around
# the regressor's (correct) mean prediction over-fills the middle buckets. We
# sample instead from a shifted Gamma:  sample = floor + (pred - floor) * G
# where G ~ Gamma(k, 1/k)  =>  E[sample] = pred  (mean preserved), and k sets
# the skew: k=1 -> Exponential (max right-skew), k large -> ~Gaussian.
# Screen stays a clipped Gaussian (its distribution is near-symmetric, small).
GRID_SCREEN_SD = [1.5, 2.2]
GRID_STD_K     = [1.0, 1.5, 2.0, 3.0, 6.0]
GRID_STD_FLOOR = [-3.0, -1.0, 1.0]
GRID_DEEP_K    = [1.0, 1.5, 2.0, 3.0]
GRID_DEEP_FLOOR = [14.0, 17.0, 20.0]


def buckets(ay):
    b = np.digitize(ay, BUCKET_EDGES, right=True)
    return np.bincount(b, minlength=7)[:7]


def do_capture(weeks, iters):
    import pandas as pd
    from src.nfl_sim.batch import BatchSimulator
    from src.nfl_sim.model_registry import ModelRegistry

    reg = ModelRegistry()
    reg.air_yards_sampler.capture = True
    reg.air_yards_sampler.captured = []

    sched = pd.read_csv("data/external/schedule_2026.csv")
    games = sched[(sched["game_type"] == "REG") & (sched["week"] <= weeks)]
    print(f"capture: {len(games)} matchups x {iters} iters")
    for _, g in games.iterrows():
        BatchSimulator(g["away_team"], g["home_team"], year=2026).run_batch(
            iterations=iters, vectorized=True)

    cap = reg.air_yards_sampler.captured
    by_zone = {}
    for X, zone in cap:
        by_zone.setdefault(zone, []).append(X)
    os.makedirs(SCRATCH, exist_ok=True)
    save = {f"X_{z}": np.vstack(v) for z, v in by_zone.items()}
    np.savez_compressed(CAP_PATH, **save)
    tot = sum(v.shape[0] for v in save.values())
    print(f"saved {tot:,} feature rows -> {CAP_PATH}  ({ {z: v.shape[0] for z,v in save.items()} })")


def do_calibrate():
    from src.nfl_sim.model_registry import ModelRegistry
    s = ModelRegistry().air_yards_sampler

    d = np.load(CAP_PATH)
    zones = [k[2:] for k in d.files]

    # precompute, per zone: gate level assignment (fixed) + per-level base preds
    rng = np.random.default_rng(2026)
    pre = {}
    for z in zones:
        X = d[f"X_{z}"].astype(np.float32)
        if z not in s._gate_boosters:
            continue
        probs = s._gate_boosters[z].inplace_predict(X)
        probs = probs.reshape(len(X), -1)
        cum = np.cumsum(probs, axis=1)
        r = rng.random(len(X))
        levels = (r[:, None] > cum).sum(axis=1)
        base = np.zeros(len(X))
        for lvl in (0, 1, 2):
            m = levels == lvl
            if m.any() and lvl in s._reg_boosters[z]:
                base[m] = s._reg_boosters[z][lvl].inplace_predict(X[m])
            elif m.any():
                # zone has no deep regressor (goalline) -> treat as std level
                levels[m] = 1
                base[m] = s._reg_boosters[z][1].inplace_predict(X[m])
        pre[z] = (levels, base, rng.standard_normal(len(X)))

    # gate mix vs real, for context
    all_lv = np.concatenate([pre[z][0] for z in pre])
    print(f"gate level mix (captured): screen {np.mean(all_lv==0):.3f}  "
          f"std {np.mean(all_lv==1):.3f}  deep {np.mean(all_lv==2):.3f}")
    print(f"real                     : screen ~0.219  std ~0.679  deep ~0.102\n")

    rng2 = np.random.default_rng(7)

    def gamma_draw(pred, floor, k, n):
        # sample = floor + (pred-floor) * Gamma(k, 1/k); mean == pred
        scale = np.maximum(pred - floor, 0.25)
        return floor + scale * rng2.gamma(k, 1.0 / k, size=n)

    def eval_combo(screen_sd, std_k, std_floor, deep_k, deep_floor):
        allsamp = []
        for z, (levels, base, gn) in pre.items():
            samp = base.copy()
            m0 = levels == 0
            if m0.any():
                samp[m0] = np.minimum(base[m0] + gn[m0] * screen_sd, 0.0)
            m1 = levels == 1
            if m1.any():
                samp[m1] = gamma_draw(base[m1], std_floor, std_k, m1.sum())
            m2 = levels == 2
            if m2.any():
                samp[m2] = gamma_draw(base[m2], deep_floor, deep_k, m2.sum())
            allsamp.append(samp)
        allsamp = np.concatenate(allsamp)
        cnt = buckets(allsamp).astype(float)
        pct = 100 * cnt / cnt.sum()
        err = np.sum(W * np.abs(pct - REAL_ATT_PCT))
        mean_pen = 2.0 * abs(allsamp.mean() - REAL_MEAN_AY)
        return err + mean_pen, pct, allsamp.mean()

    results = []
    for ssd in GRID_SCREEN_SD:
        for sk in GRID_STD_K:
            for sf in GRID_STD_FLOOR:
                for dk in GRID_DEEP_K:
                    for df in GRID_DEEP_FLOOR:
                        score, pct, mn = eval_combo(ssd, sk, sf, dk, df)
                        results.append((score, ssd, sk, sf, dk, df, pct, mn))
    results.sort(key=lambda t: t[0])

    # current baseline (symmetric Gaussian 1.5/4.0/12.0, clips [1,19]/>=20)
    def eval_current():
        allsamp = []
        for z, (levels, base, gn) in pre.items():
            samp = base.copy()
            for lvl, sd, lo, hi in ((0, 1.5, None, 0), (1, 4.0, 1, 19), (2, 12.0, 20, None)):
                m = levels == lvl
                if not m.any():
                    continue
                v = base[m] + gn[m] * sd
                if lvl == 0: v = np.minimum(v, 0)
                elif lvl == 1: v = np.clip(v, 1, 19)
                else: v = np.maximum(v, 20)
                samp[m] = v
            allsamp.append(samp)
        a = np.concatenate(allsamp)
        c = buckets(a).astype(float); p = 100 * c / c.sum()
        return np.sum(W * np.abs(p - REAL_ATT_PCT)) + 2.0 * abs(a.mean() - REAL_MEAN_AY), p, a.mean()

    base_score, base_pct, base_mn = eval_current()

    hdr = (f"{'score':>6}  {'scrSD':>5} {'stdK':>4} {'stdF':>5} {'dpK':>4} {'dpF':>4} {'mean':>5}  | " +
           "  ".join(f"{x:>5}" for x in ["<=0", "0-5", "5-10", "1015", "1520", "2030", "30+"]))
    print(hdr)
    print("-" * len(hdr))
    print(f"{base_score:6.1f}  {'CURRENT (symmetric gaussian)':>40}  {base_mn:5.1f}  | " +
          "  ".join(f"{p:5.1f}" for p in base_pct))
    print(f"{'':6}  {'REAL':>40}  {REAL_MEAN_AY:5.1f}  | " +
          "  ".join(f"{p:5.1f}" for p in REAL_ATT_PCT))
    print("-" * len(hdr))
    for score, ssd, sk, sf, dk, df, pct, mn in results[:12]:
        print(f"{score:6.1f}  {ssd:5} {sk:4} {sf:5} {dk:4} {df:4} {mn:5.1f}  | " +
              "  ".join(f"{p:5.1f}" for p in pct))


if __name__ == "__main__":
    if "--capture" in sys.argv:
        w = int(sys.argv[sys.argv.index("--capture") + 1]) if len(sys.argv) > sys.argv.index("--capture") + 1 and sys.argv[sys.argv.index("--capture") + 1].isdigit() else 2
        it = 250
        rest = [x for x in sys.argv[sys.argv.index("--capture") + 1:] if x.isdigit()]
        if len(rest) >= 1: w = int(rest[0])
        if len(rest) >= 2: it = int(rest[1])
        do_capture(w, it)
    elif "--calibrate" in sys.argv:
        do_calibrate()
    else:
        print(__doc__)
