"""Phase 0 of the completion-rate calibration ("A2", see
docs/implementation_plans/completion_rate_calibration_plan.md).

Builds the REAL completion-rate-by-target-depth baseline the sim's completion
model is calibrated against, from 2021-2025 nfl_data_py PBP. The sim's depth
curve (`logit(P) = b0 + b1*air_yards + delta_wr`, game_engine.py ~1638) is a
single log-linear slope; real completion vs air-yards is convex (falls off
faster deep). This gives us the shape to fit.

Outputs:
  - console table + docs/eda_outputs/completion_by_depth/README.md
  - real 2024+2025 catch rate & ADOT for the receivers Cam flagged, as the
    per-player Phase 2 anchor targets.

Usage: venv\\Scripts\\python.exe scripts/eda/analyze_completion_by_depth.py
"""
import os
import numpy as np
import pandas as pd
import nfl_data_py as nfl

SEASONS = list(range(2021, 2026))
OUT_DIR = "docs/eda_outputs/completion_by_depth"

# air_yards buckets (the throw's depth, which is what b1 operates on)
BUCKETS = [(-99, 0), (0, 5), (5, 10), (10, 15), (15, 20), (20, 30), (30, 99)]
BUCKET_LABELS = ["<=0 (screen/behind LOS)", "0-5", "5-10", "10-15", "15-20", "20-30", "30+"]

FLAGGED = [
    "C.Olave", "A.St. Brown", "A.Brown", "M.Evans", "M.Nabers", "W.Robinson",
    "S.Diggs", "M.Harrison", "G.Pickens", "T.Thornton", "K.Boutte", "A.Pierce",
    "J.Waddle", "D.Smith", "C.Sutton", "N.Collins", "J.Chase", "P.Nacua",
]


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    print(f"Loading PBP {SEASONS} ...")
    df = nfl.import_pbp_data(SEASONS, downcast=True, cache=False)
    df = df[df["season_type"] == "REG"]

    # Real pass attempts with a measured air_yards. Exclude sacks, spikes,
    # throwaways (nfl marks many as incomplete with NA air_yards anyway), 2pt.
    p = df[
        (df["pass_attempt"] == 1)
        & (df["sack"] == 0)
        & (df["qb_spike"] != 1)
        & (df["two_point_attempt"] == 0)
        & df["air_yards"].notna()
        & df["complete_pass"].notna()
        # a real target to a real receiver -- excludes throwaways / batted balls,
        # which otherwise drag every bucket ~1-2pp low (2026-09-06). The sim's
        # completion model only fires on real targets, so this is the matched set.
        & df["receiver_player_id"].notna()
    ].copy()
    p["cp"] = p["complete_pass"].astype(float)
    print(f"{len(p):,} qualifying pass attempts\n")

    # -- by depth bucket -------------------------------------------------------
    rows = []
    for (lo, hi), lab in zip(BUCKETS, BUCKET_LABELS):
        m = (p["air_yards"] > lo) & (p["air_yards"] <= hi)
        sub = p[m]
        rows.append({
            "bucket": lab,
            "attempts": len(sub),
            "att_pct": round(100 * len(sub) / len(p), 1),
            "cmp_pct": round(100 * sub["cp"].mean(), 1),
            "mean_air_yards": round(sub["air_yards"].mean(), 1),
        })
    tbl = pd.DataFrame(rows)
    overall = round(100 * p["cp"].mean(), 2)

    # sanity: attempt-weighted overall should be ~64-65 (Cam's target 64.40)
    print("REAL completion rate by target depth (2021-2025 REG):\n")
    print(tbl.to_string(index=False))
    print(f"\nOverall (all qualifying attempts): {overall}%")
    print("Cam's stated real 2025 target (weighted CMP/ATT, 50 QBs): 64.40%")

    # -- last-2-year cut (closer to what a 2026 projection should target) -----
    p2 = p[p["season"] >= 2024]
    rows2 = []
    for (lo, hi), lab in zip(BUCKETS, BUCKET_LABELS):
        sub = p2[(p2["air_yards"] > lo) & (p2["air_yards"] <= hi)]
        rows2.append({"bucket": lab, "cmp_pct_24_25": round(100 * sub["cp"].mean(), 1)})
    tbl = tbl.merge(pd.DataFrame(rows2), on="bucket")

    # -- flagged receivers: real 2024+2025 catch rate + ADOT -----------------
    rec = p2[p2["receiver_player_name"].notna()].copy()
    g = rec.groupby("receiver_player_name").agg(
        targets=("cp", "size"), rec=("cp", "sum"),
        adot=("air_yards", "mean"),
        deep_rate=("air_yards", lambda s: (s >= 20).mean()),
    ).reset_index()
    g["catch_rate"] = (g["rec"] / g["targets"]).round(3)
    g["adot"] = g["adot"].round(1)
    g["deep_rate"] = g["deep_rate"].round(3)
    flagged = g[g["receiver_player_name"].isin(FLAGGED) & (g["targets"] >= 40)]
    flagged = flagged.sort_values("adot", ascending=False)

    print("\n\nFlagged receivers -- REAL 2024+2025 (the Phase 2 per-player targets):\n")
    print(flagged[["receiver_player_name", "targets", "catch_rate", "adot", "deep_rate"]].to_string(index=False))

    # -- write doc ----------------------------------------------------------
    def md_table(df):
        cols = list(df.columns)
        out = ["| " + " | ".join(cols) + " |",
               "| " + " | ".join("---" for _ in cols) + " |"]
        for _, r in df.iterrows():
            out.append("| " + " | ".join(str(r[c]) for c in cols) + " |")
        return "\n".join(out)

    md = ["# Real completion rate by target depth (2021-2025)\n",
          "Phase 0 baseline for the completion-rate calibration "
          "(`docs/implementation_plans/completion_rate_calibration_plan.md`). "
          "Source: `nfl_data_py` PBP, REG only, pass attempts with a measured "
          "`air_yards`, sacks/spikes/2pt excluded.\n",
          f"**Overall completion (2021-2025): {overall}%** "
          "(Cam's real 2025 target: 64.40%).\n",
          md_table(tbl),
          "\n## Flagged receivers -- real 2024+2025\n",
          "Per-player anchor targets for Phase 2. `catch_rate` = rec/targets, "
          "`deep_rate` = share of targets 20+ air yards.\n",
          md_table(flagged[["receiver_player_name", "targets", "catch_rate", "adot", "deep_rate"]]),
          ]
    with open(os.path.join(OUT_DIR, "README.md"), "w", encoding="utf-8") as f:
        f.write("\n".join(md) + "\n")
    print(f"\nWrote {OUT_DIR}/README.md")


if __name__ == "__main__":
    main()
