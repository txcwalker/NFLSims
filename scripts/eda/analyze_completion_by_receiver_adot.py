"""A2 Phase 3 design check: does a receiver's own ADOT change how well they
catch a throw of a GIVEN depth?

The completion model anchors each receiver's catch rate at their ADOT and then
adjusts along the depth curve for the actual throw -- so a deep specialist
thrown a short pass gets a big completion boost (Thornton: sim ~75% on a 10-yd
throw). Phase 3 (2c) dampens that. But how much personal-ADOT effect is real?

This bins real 2021-2025 throws by (receiver ADOT quartile) x (throw air_yards
bucket) and prints completion %. If completion at a given air_yards is roughly
flat across receiver-ADOT quartiles, the personal-ADOT re-scaling is mostly
spurious and Phase 3 should shrink it hard.

Usage: venv\\Scripts\\python.exe scripts/eda/analyze_completion_by_receiver_adot.py
"""
import os
import numpy as np
import pandas as pd
import nfl_data_py as nfl

SEASONS = list(range(2021, 2026))
OUT = "docs/eda_outputs/completion_by_depth/receiver_adot_check.md"
AY_EDGES = np.array([0, 5, 10, 15, 20, 30], dtype=float)
AY_LABELS = ["0-5", "5-10", "10-15", "15-20", "20-30", "30+"]


def main():
    print(f"loading PBP {SEASONS} ...")
    df = nfl.import_pbp_data(SEASONS, downcast=True, cache=False)
    df = df[df["season_type"] == "REG"]
    p = df[
        (df["pass_attempt"] == 1) & (df["sack"] == 0) & (df["qb_spike"] != 1)
        & (df["two_point_attempt"] == 0)
        & df["air_yards"].notna() & df["complete_pass"].notna()
        & df["receiver_player_id"].notna()
    ].copy()
    p["cp"] = p["complete_pass"].astype(float)

    # receiver ADOT over the whole window, min 100 targets to be stable
    g = p.groupby("receiver_player_id").agg(n=("cp", "size"), adot=("air_yards", "mean"))
    g = g[g["n"] >= 100]
    p = p[p["receiver_player_id"].isin(g.index)]
    p["rec_adot"] = p["receiver_player_id"].map(g["adot"])

    # ADOT quartiles of the throws (target-weighted view of receiver types)
    q = g["adot"].quantile([0.25, 0.5, 0.75]).values
    def adot_band(a):
        return ("Q1 <%.1f" % q[0] if a < q[0] else
                "Q2 %.1f-%.1f" % (q[0], q[1]) if a < q[1] else
                "Q3 %.1f-%.1f" % (q[1], q[2]) if a < q[2] else
                "Q4 >%.1f" % q[2])
    p["adot_band"] = p["rec_adot"].map(adot_band)
    p["ay_bucket"] = pd.cut(p["air_yards"], bins=[*AY_EDGES, 999], labels=AY_LABELS, right=True)

    tbl = p.pivot_table(index="adot_band", columns="ay_bucket", values="cp",
                        aggfunc="mean", observed=True) * 100
    cnt = p.pivot_table(index="adot_band", columns="ay_bucket", values="cp",
                        aggfunc="size", observed=True)

    pd.set_option("display.width", 200)
    print("\nCompletion % by receiver-ADOT band x throw air_yards (real 2021-2025):\n")
    print(tbl.round(1).to_string())
    print("\nthrow counts:\n")
    print(cnt.to_string())

    # the key number: spread across ADOT bands within each air_yards bucket
    print("\nspread (max-min completion %) across ADOT bands, per air_yards bucket:")
    for c in AY_LABELS:
        if c in tbl.columns:
            col = tbl[c].dropna()
            print(f"  {c:>6}: {col.max() - col.min():.1f}pp   ({col.min():.1f} - {col.max():.1f})")

    lines = ["# Phase 3 check: completion by receiver ADOT x throw depth\n",
             "Real 2021-2025 REG, receivers with >=100 targets. If completion within an "
             "air_yards column is flat across receiver-ADOT rows, the model's personal-ADOT "
             "re-anchoring is mostly spurious.\n",
             "## Completion %\n", "```", tbl.round(1).to_string(), "```\n",
             "## Throw counts\n", "```", cnt.to_string(), "```\n"]
    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with open(OUT, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    print(f"\nwrote {OUT}")


if __name__ == "__main__":
    main()
