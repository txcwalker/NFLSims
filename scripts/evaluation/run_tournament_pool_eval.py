"""Run the tournament-pool evaluation for one or more settled weeks and print a
short report. Writes data/eval/{year}/tournament_pool/week_NN.json.

Usage: python scripts/evaluation/run_tournament_pool_eval.py 1 2 3 4 [--year 2026]
See src/evaluation/tournament_pool_eval.py for what is graded and how.
"""
import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from src.evaluation.tournament_pool_eval import write_week_eval  # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("weeks", nargs="+", type=int)
    ap.add_argument("--year", type=int, default=2026)
    args = ap.parse_args()
    for wk in args.weeks:
        res = write_week_eval(args.year, wk)
        if not res.get("gradable"):
            print(f"Week {wk}: not gradable -- {res.get('reason')}")
            continue
        f, s, c = res["field"], res["summary"], res["calibration"]
        print(f"\n=== Week {wk} | {f['contest']} field {f['size']:,} | winner {f['winner_score']} | payouts: {f['payout_source']}")
        print(f"builds {s['n_builds']} | generated {s['n_generated']:,} | unique {s['n_unique']:,} | "
              f"mean regen {s['mean_regen']} (max {s["max_regen"]}) | {s["share_gen_from_repeats_5plus"]}% of generations are 5+x repeats | post-lock builds excluded: {len(s["post_lock_builds_excluded"])}")
        u = s["valid"]
        print(f"valid: best rank {u['best_rank']} (score {u['best_score']}) | top10 {s['n_top10']} top100 {s['n_top100']} | "
              f"paid {u['paid_pct']}% top1% {u['top1_pct']}% top0.1% {u['top01_pct']}% | mean finish pct {u['mean_pct_finish']} | ROI {u['roi_pct']}%")
        st = res["stale"]
        print(f"stale (unavailable-by-lock player): {st['n_stale']} of {s['n_unique']} unique lineups ({st['pct_stale']}%) -> "
              + ", ".join(f"{p['name']} [{p['reason']}] x{p['n_lineups']}" for p in st["players"][:6]))
        for a in res["by_age"]:
            print(f"  age {a['label']}: n={a['n']} stale {a['pct_stale']}% | valid paid {a.get('paid_pct')}% mean finish {a.get('mean_pct_finish')}")
        for k in ("itm", "top1", "top01"):
            if k in c:
                print(f"  calib {k}: predicted {c[k]['pred_mean']}% vs realized {c[k]['realized']}% (n={c[k]['n']})")
        if "ev" in c:
            print(f"  EV: predicted {c['ev']['pred_mean_roi_pct']}% vs realized {c['ev']['realized_roi_pct']}%")
        print(f"  field check: rows with unscored player {f['n_missing_rows']}, max |rescore - CSV| {f['max_rescore_vs_csv_diff']}")


if __name__ == "__main__":
    main()
