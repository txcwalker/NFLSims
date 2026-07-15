# RETIRED — evaluates the old single-model air-yards trainer (train.py in this
# same legacy folder), not the deployed zone-split model. DO NOT RUN as a
# current-model check. Kept for historical reference only. See AGENTS.md §11.7-11.9.

import joblib
import json
import numpy as np
import pandas as pd
import os
import nfl_data_py as nfl
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import (
    log_loss, mean_squared_error, mean_absolute_error, 
    accuracy_score, brier_score_loss, roc_auc_score, precision_recall_curve, auc,
    confusion_matrix
)
from sklearn.preprocessing import label_binarize
from train import preprocess_data, load_dna_atlases
from inference import AirYardsDoubleHurdleSampler

# =============================================================================
# Air Yards Evaluation (Mathematical Rigor) — V.0.1.1
# =============================================================================

MODEL_DIR = "src/nfl_sim/models/air_yards_v_0_1_1"
REPORT_DIR = "docs/eda_outputs/air_yards/evaluation_v_0_1_1"
os.makedirs(REPORT_DIR, exist_ok=True)

def calculate_pr_auc(y_true, y_prob):
    precision, recall, _ = precision_recall_curve(y_true, y_prob)
    return auc(recall, precision)

def run_rigorous_evaluation():
    print("Loading test data (2024)...")
    pbp_2024 = nfl.import_pbp_data([2024])
    atlases = load_dna_atlases()
    df_2024, features = preprocess_data(pbp_2024, atlases)
    
    X = df_2024[features]
    y_true = df_2024['air_yards']
    y_true_level = df_2024['ay_level']
    
    sampler = AirYardsDoubleHurdleSampler(MODEL_DIR)
    y_sim = sampler.sample(X)
    
    # 1. GATE EVALUATION (CLASSIFICATION)
    probs = sampler.gate.predict_proba(X) # [N, 3]
    y_true_bin = label_binarize(y_true_level, classes=[0, 1, 2])
    
    gate_results = {
        "log_loss": float(log_loss(y_true_level, probs)),
        "accuracy": float(accuracy_score(y_true_level, np.argmax(probs, axis=1))),
        "roc_auc_ovr": float(roc_auc_score(y_true_level, probs, multi_class='ovr')),
        "pr_auc": {
            "screen": float(calculate_pr_auc(y_true_bin[:, 0], probs[:, 0])),
            "standard": float(calculate_pr_auc(y_true_bin[:, 1], probs[:, 1])),
            "deep": float(calculate_pr_auc(y_true_bin[:, 2], probs[:, 2]))
        }
    }
    
    # 2. REGRESSOR EVALUATION (PER BUCKET)
    reg_results = {}
    for lvl, name in [(0, "screen"), (1, "standard"), (2, "deep")]:
        mask = (y_true_level == lvl)
        if any(mask):
            y_sub = y_true[mask]
            preds_sub = sampler.regs[lvl].predict(X[mask])
            reg_results[name] = {
                "rmse": float(np.sqrt(mean_squared_error(y_sub, preds_sub))),
                "mae": float(mean_absolute_error(y_sub, preds_sub))
            }

    # 3. DISTRIBUTION / SIMULATION MATCHING
    rates = {
        "empirical": {
            "screen": float((y_true_level == 0).mean()),
            "standard": float((y_true_level == 1).mean()),
            "deep": float((y_true_level == 2).mean())
        },
        "simulated": {
            "screen": float((y_sim <= 0).mean()),
            "standard": float(((y_sim > 0) & (y_sim < 20)).mean()),
            "deep": float((y_sim >= 20).mean())
        }
    }

    # --- PRINT REPORT ---
    print("\n" + "="*50)
    print("AIR YARDS V.0.1.1 RIGOROUS EVALUATION")
    print("="*50)
    print(f"GATE: Log Loss = {gate_results['log_loss']:.4f} | Accuracy = {gate_results['accuracy']:.1%}")
    print(f"GATE: PR-AUC (Deep) = {gate_results['pr_auc']['deep']:.4f} (Target: >0.20)")
    print(f"GATE: PR-AUC (Screen) = {gate_results['pr_auc']['screen']:.4f}")
    print("-" * 50)
    print("REGRESSOR ACCURACY (Within Bucket):")
    for name, m in reg_results.items():
        print(f"  {name.capitalize():8}: RMSE = {m['rmse']:.2f} | MAE = {m['mae']:.2f}")
    print("-" * 50)
    print("DISTRIBUTION MATCHING:")
    print(f"  Deep Shot Rate: Sim {rates['simulated']['deep']:.1%} vs Emp {rates['empirical']['deep']:.1%}")
    print(f"  Screen Rate:    Sim {rates['simulated']['screen']:.1%} vs Emp {rates['empirical']['screen']:.1%}")
    print("="*50)

    # Save outputs
    all_results = {
        "gate": gate_results,
        "regressors": reg_results,
        "rates": rates
    }
    with open(os.path.join(REPORT_DIR, "rigorous_results.json"), 'w') as f:
        json.dump(all_results, f, indent=2)

    # Confusion Matrix Visualization
    plt.figure(figsize=(8, 6))
    cm = confusion_matrix(y_true_level, np.argmax(probs, axis=1), normalize='true')
    sns.heatmap(cm, annot=True, fmt='.2f', cmap='Blues', xticklabels=['Screen', 'Std', 'Deep'], yticklabels=['Screen', 'Std', 'Deep'])
    plt.title("Gate Confusion Matrix (Tactical Decision)")
    plt.ylabel("Actual")
    plt.xlabel("Predicted")
    plt.savefig(os.path.join(REPORT_DIR, "gate_confusion_matrix.png"))

    return all_results

if __name__ == "__main__":
    run_rigorous_evaluation()
