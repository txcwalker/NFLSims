"""A2 Phase 2: refit the completion model's depth curve so deep completion
stops diving.

The completion prob for a non-screen throw is
    logit(P) = logit(zone_baseline) + [g(air_yards) - g(adot)] + sep_bonus
    P        = sigmoid(logit(P)) + qb_cpoe - 0.075   (open; no sep/offset for contested)
where g(x) = b0 + b1*x is a single line (b1 = -0.08). Because b0 and the
linearity both cancel in `g(ay) - g(adot)`, the only thing that matters is the
*shape* of g. Phase 0 showed the real completion-vs-depth curve is convex --
steep 5-20 yds, nearly flat past 25 -- so one slope can't fit it: the sim
over-completes 5-15 and the 30+ bucket craters to ~14% vs 29% real.

This replaces g with a **two-slope piecewise line**: slope `b1a` up to `knee`
air yards, a flatter `b1b` beyond. Calibrated here against the real by-depth
completion curve, replaying the exact formula on captured throw inputs.

    --capture [weeks] [iters]   dump completion-model inputs -> scratchpad/completion_cap.npz
    --calibrate                 Phase 2: grid-search the depth curve (b1a, b1b, knee)
    --calibrate-adot            Phase 3 (2a): grid-search the ADOT soft-cap (knee, compress)
                                against the real completion-by-(receiver ADOT x throw depth) table
"""
import os
import sys
import numpy as np

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

SCRATCH = os.environ.get("CLAUDE_SCRATCH", "scratchpad")
CAP = os.path.join(SCRATCH, "completion_cap.npz")

# Real 2021-2025 completion % by air-yards bucket (analyze_completion_by_depth.py).
# Bucket 0 (<=0, screen) is handled by a separate hardcoded path, not this curve.
# REAL_CMP = 2021-2025 completion % on REAL TARGETS (receiver_player_id not null),
# which is what the sim's completion model actually fires on. NOT the box-score
# number (that includes throwaways). Buckets 0-5..30+ (idx 0 = screen, unused).
BUCKET_EDGES = np.array([0, 5, 10, 15, 20, 30], dtype=np.float32)
REAL_CMP = np.array([81.4, 76.4, 65.9, 58.8, 52.8, 39.2, 30.1])
REAL_ATT_W = np.array([21.5, 30.8, 18.1, 11.5, 7.7, 6.3, 4.1])

GRID_B1A  = [-0.135, -0.12, -0.11, -0.10, -0.09]
GRID_B1B  = [-0.045, -0.035, -0.025, -0.015, -0.005]
GRID_KNEE = [14, 16, 18, 20, 23]

# Phase 4 joint re-cal grids (new model: interp real curve + skill shrink)
_DEPTH_AY  = np.array([1.0, 3.4, 7.6, 12.9, 17.8, 24.7, 39.0, 55.0])
_DEPTH_CMP = np.array([0.800, 0.764, 0.659, 0.588, 0.528, 0.392, 0.301, 0.210])
_DEPTH_LG  = np.log(_DEPTH_CMP / (1 - _DEPTH_CMP))
GRID_SHRINK    = [0.4, 0.55, 0.7, 0.85, 1.0]
GRID_SEP_SCALE = [0.10, 0.13, 0.16]
GRID_OFFSET    = [-0.03, -0.02, -0.01, 0.0, 0.02, 0.04]


def _sig(x): return 1.0 / (1.0 + np.exp(-x))
def _logit(p):
    p = np.clip(p, 1e-4, 1 - 1e-4)
    return np.log(p / (1 - p))


def do_capture(weeks, iters):
    import pandas as pd
    from src.nfl_sim.batch import BatchSimulator
    sched = pd.read_csv("data/external/schedule_2026.csv")
    games = sched[(sched["game_type"] == "REG") & (sched["week"] <= weeks)]
    print(f"capture: {len(games)} matchups x {iters} iters")
    rows = []
    for _, g in games.iterrows():
        b = BatchSimulator(g["away_team"], g["home_team"], year=2026)
        b.run_batch(iterations=iters, vectorized=True, capture_completion=True)
        if b.last_completion_cap is not None:
            rows.append(b.last_completion_cap)
    X = np.vstack(rows)
    os.makedirs(SCRATCH, exist_ok=True)
    # cols: ay, adot, zone_baseline, contested_wr_rate, sep_roll, qb_cpoe_frac, is_contested
    np.savez_compressed(CAP, X=X)
    print(f"saved {len(X):,} throws -> {CAP}")


def g_piece(x, b1a, b1b, knee):
    return b1a * np.minimum(x, knee) + b1b * np.maximum(x - knee, 0.0)


def do_calibrate():
    d = np.load(CAP)["X"]
    ay, adot, zb, cwr, sep, cpoe, isc = [d[:, i] for i in range(7)]
    isc = isc > 0.5
    sep_bonus = 0.15 * (sep - 1.0)
    b = np.digitize(ay, BUCKET_EDGES, right=True)

    def eval_curve(b1a, b1b, knee):
        D = g_piece(ay, b1a, b1b, knee) - g_piece(adot, b1a, b1b, knee)
        lp_open = _logit(zb) + D + sep_bonus
        p_open = _sig(lp_open) + cpoe - OFFSET
        lp_con = _logit(cwr) + D
        p_con = _sig(lp_con) + cpoe
        p = np.where(isc, p_con, p_open)
        p = np.clip(p, 0.01, 0.99)
        cmp_by = np.array([p[b == k].mean() * 100 if np.any(b == k) else np.nan
                           for k in range(7)])
        w = np.array([1, 1.2, 1.2, 1.3, 1.5, 2.0, 2.5])
        err = np.nansum(w[1:] * np.abs(cmp_by[1:] - REAL_CMP[1:]))
        overall = p.mean() * 100
        return err, cmp_by, overall

    cur_err, cur_by, cur_ov = eval_curve(-0.08, -0.08, 999)  # current single slope

    res = []
    for a in GRID_B1A:
        for bb in GRID_B1B:
            for k in GRID_KNEE:
                e, cby, ov = eval_curve(a, bb, k)
                res.append((e, a, bb, k, cby, ov))
    res.sort(key=lambda t: t[0])

    lbl = ["<=0", "0-5", "5-10", "10-15", "15-20", "20-30", "30+"]
    hdr = f"{'err':>6} {'b1a':>6} {'b1b':>6} {'knee':>4} {'ovr%':>5}  | " + "  ".join(f"{x:>5}" for x in lbl[1:])
    print(hdr); print("-" * len(hdr))
    print(f"{cur_err:6.1f} {-0.08:6} {-0.08:6} {'--':>4} {cur_ov:5.1f}  | " +
          "  ".join(f"{v:5.1f}" for v in cur_by[1:]) + "   <- CURRENT")
    print(f"{'':6} {'':6} {'':6} {'REAL':>4} {'':5}  | " +
          "  ".join(f"{v:5.1f}" for v in REAL_CMP[1:]))
    print("-" * len(hdr))
    for e, a, bb, k, cby, ov in res[:12]:
        print(f"{e:6.1f} {a:6} {bb:6} {k:4} {ov:5.1f}  | " +
              "  ".join(f"{v:5.1f}" for v in cby[1:]))


# --- Phase 3 (2a): ADOT soft-cap --------------------------------------------
# Real completion % by (receiver ADOT quartile) x (throw air_yards), 2021-2025,
# from analyze_completion_by_receiver_adot.py. ADOT quartile edges 5.3/8.3/11.4.
# '*' cells (thin sample) down-weighted. Columns 0-5..30+.
REAL_ADOT_TABLE = {
    # adot band lo/hi : [0-5, 5-10, 10-15, 15-20, 20-30, 30+]
    (0.0, 5.3):   [77.7, 60.1, 53.6, 45.5, 35.2, 31.1],
    (5.3, 8.3):   [78.8, 68.6, 59.7, 58.0, 42.2, 30.9],
    (8.3, 11.4):  [75.5, 66.9, 61.0, 52.9, 42.5, 30.8],
    (11.4, 99.0): [72.7, 66.4, 58.9, 53.4, 37.0, 31.1],
}
REAL_ADOT_WT = {   # per (band, ay bucket) weight; thin-sample cells ~0.3
    (0.0, 5.3):   [1.0, 0.3, 0.3, 0.3, 0.3, 0.2],
    (5.3, 8.3):   [1.0, 1.0, 1.0, 1.0, 0.8, 0.4],
    (8.3, 11.4):  [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
    (11.4, 99.0): [1.5, 1.5, 1.5, 1.5, 1.2, 1.0],   # deep receivers = what 2a targets
}
GRID_ADOT_KNEE     = [10, 11, 12, 13, 14]
GRID_ADOT_COMPRESS = [0.35, 0.45, 0.55, 0.65, 0.75]


def do_calibrate_adot():
    d = np.load(CAP)["X"]
    ay, adot, zb, cwr, sep, cpoe, isc = [d[:, i] for i in range(7)]
    isc = isc > 0.5
    sep_bonus = 0.15 * (sep - 1.0)
    ayb = np.digitize(ay, BUCKET_EDGES, right=True)  # 0..6 ; bucket 0 = screen (skip)

    # current live depth curve (game_engine._depth_logit), Phase 2 values
    B1A, B1B, KNEE = -0.09, -0.025, 23.0
    def g(x):
        return B1A * np.minimum(x, KNEE) + B1B * np.maximum(x - KNEE, 0.0)

    def eff_adot(a, knee, comp):
        return np.minimum(a, knee) + np.maximum(a - knee, 0.0) * (1.0 - comp)

    def realized(knee, comp):
        ea = eff_adot(adot, knee, comp) if knee is not None else adot
        D = g(ay) - g(ea)
        p_open = _sig(_logit(zb) + D + sep_bonus) + cpoe - OFFSET
        p_con = _sig(_logit(cwr) + D) + cpoe
        p = np.clip(np.where(isc, p_con, p_open), 0.01, 0.99)
        return p

    def score(knee, comp):
        p = realized(knee, comp)
        err = 0.0
        cells = {}
        for (lo, hi), row in REAL_ADOT_TABLE.items():
            wrow = REAL_ADOT_WT[(lo, hi)]
            bandm = (adot >= lo) & (adot < hi)
            for j in range(6):  # ay buckets 0-5 .. 30+  => ayb 1..6
                m = bandm & (ayb == j + 1)
                if m.sum() < 50:
                    continue
                simv = p[m].mean() * 100
                err += wrow[j] * abs(simv - row[j])
                cells[((lo, hi), j)] = simv
        return err, cells

    cur_err, cur_cells = score(None, None)
    res = []
    for k in GRID_ADOT_KNEE:
        for c in GRID_ADOT_COMPRESS:
            e, cells = score(k, c)
            res.append((e, k, c, cells))
    res.sort(key=lambda t: t[0])

    lbl = ["0-5", "5-10", "10-15", "15-20", "20-30", "30+"]
    def show(cells, tag):
        print(f"\n  {tag}")
        for (lo, hi), row in REAL_ADOT_TABLE.items():
            sim = [cells.get(((lo, hi), j)) for j in range(6)]
            s = "  ".join(f"{v:5.1f}" if v is not None else "   -- " for v in sim)
            r = "  ".join(f"{v:5.1f}" for v in row)
            print(f"   adot {lo:>4}-{hi:<4} sim | {s}")
            print(f"   {'':>13} real| {r}")

    print(f"CURRENT (no cap): err {cur_err:.1f}")
    show(cur_cells, "current")
    for e, k, c, cells in res[:4]:
        print(f"\n=== knee={k}  compress={c}  err {e:.1f} ===")
        show(cells, f"knee {k} / compress {c}")


def do_calibrate_p4():
    """Phase 4: joint re-cal of SKILL_SHRINK x sep_bonus scale x flat offset,
    against the corrected real-target completion curve. New model: interp the
    real completion-vs-depth curve, add a shrunk per-receiver skill term, add
    the zero-mean separation deviation."""
    d = np.load(CAP)["X"]
    ay, adot, zb, cwr, sep, cpoe, isc, avgsep = [d[:, i] for i in range(8)]
    isc = isc > 0.5
    ayb = np.digitize(ay, BUCKET_EDGES, right=True)
    ea = np.minimum(adot, 16.0) + np.maximum(adot - 16.0, 0.0) * 0.25  # Phase 3 anchor cap

    curve_ay = np.interp(ay, _DEPTH_AY, _DEPTH_LG)
    curve_anchor = np.interp(ea, _DEPTH_AY, _DEPTH_LG)

    def eval_combo(shrink, sep_scale, offset):
        skill = shrink * (_logit(zb) - curve_anchor)
        skill_c = shrink * (_logit(cwr) - curve_anchor)
        sb = sep_scale * (sep - avgsep)
        p_open = _sig(curve_ay + skill + sb) + cpoe - offset
        p_con = _sig(curve_ay + skill_c) + cpoe
        p = np.clip(np.where(isc, p_con, p_open), 0.01, 0.99)
        by = np.array([p[ayb == k].mean() * 100 if np.any(ayb == k) else np.nan for k in range(7)])
        w = np.array([0, 1.3, 1.0, 1.0, 1.3, 1.3, 1.6])
        tgt = REAL_CMP + 1.7          # replay runs ~1.5-2pp hot vs live
        err = np.nansum(w[1:] * np.abs(by[1:] - tgt[1:])) + 1.5 * abs(p.mean() * 100 - (67.52 + 1.7))
        return err, by, p.mean() * 100

    res = []
    for sh in GRID_SHRINK:
        for s in GRID_SEP_SCALE:
            for o in GRID_OFFSET:
                e, by, ov = eval_combo(sh, s, o)
                res.append((e, sh, s, o, by, ov))
    res.sort(key=lambda t: t[0])
    cur_e, cur_by, cur_ov = eval_combo(1.0, 0.13, 0.0)

    lbl = ["0-5", "5-10", "10-15", "15-20", "20-30", "30+"]
    hdr = f"{'err':>5} {'shrk':>5} {'sepS':>5} {'off':>6} {'ovr':>5}  | " + "  ".join(f"{x:>5}" for x in lbl)
    print(hdr); print("-" * len(hdr))
    print(f"{'':5} {'':5} {'':5} {'REAL':>6} {67.5:5.1f}  | " + "  ".join(f"{v:5.1f}" for v in REAL_CMP[1:]) + "   (replay aim +1.7)")
    print(f"{cur_e:5.1f} {1.0:5} {0.13:5} {0.0:6} {cur_ov:5.1f}  | " + "  ".join(f"{v:5.1f}" for v in cur_by[1:]) + "  <- shrink=1 (old behaviour)")
    print("-" * len(hdr))
    for e, sh, s, o, by, ov in res[:12]:
        print(f"{e:5.1f} {sh:5} {s:5} {o:6} {ov:5.1f}  | " + "  ".join(f"{v:5.1f}" for v in by[1:]))


if __name__ == "__main__":
    if "--capture" in sys.argv:
        nums = [int(x) for x in sys.argv if x.isdigit()]
        do_capture(nums[0] if nums else 2, nums[1] if len(nums) > 1 else 300)
    elif "--calibrate-adot" in sys.argv:
        do_calibrate_adot()
    elif "--calibrate-p4" in sys.argv:
        do_calibrate_p4()
    elif "--calibrate" in sys.argv:
        do_calibrate()
    else:
        print(__doc__)
