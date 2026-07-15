import pandas as pd
import numpy as np
import xgboost as xgb
import os
import json
import joblib
import nfl_data_py as nfl
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import log_loss, brier_score_loss, roc_auc_score
from sklearn.calibration import calibration_curve

def train_fd_conversion_model():
    print("==================================================")
    print("NFLSims Fourth Down Conversion Model V.0.1.0 Training")
    print("==================================================")
    
    # Create directories if they do not exist
    os.makedirs("docs/eda_outputs/fd_conversion_v010", exist_ok=True)
    os.makedirs("src/nfl_sim/models/fourth_down_conversion_v_0_1_0", exist_ok=True)
    
    # 1. Load Data
    seasons = list(range(2015, 2026)) # 2015 to 2025
    print(f"Loading NFL play-by-play data for seasons {seasons[0]} to {seasons[-1]} via nfl_data_py...")
    df_raw = nfl.import_pbp_data(seasons)
    print(f"Loaded {len(df_raw)} raw play records.")
    
    # 2. Derive Target and Filter Dataset
    print("\nFiltering and preparing dataset...")
    # Select plays: 3rd and 4th downs, active conversion attempts
    df = df_raw[
        (df_raw['down'].isin([3, 4])) &
        ((df_raw['rush'] == 1) | (df_raw['pass'] == 1) | (df_raw['qb_scramble'] == 1)) &
        ((df_raw['qb_spike'] != 1) | (df_raw['qb_spike'].isna())) &
        ((df_raw['qb_kneel'] != 1) | (df_raw['qb_kneel'].isna())) &
        (df_raw['first_down'].notna())
    ].copy()
    
    # Convert target and clean features
    df['first_down'] = df['first_down'].astype(int)
    
    features = [
        'ydstogo',
        'yardline_100',
        'score_differential',
        'game_seconds_remaining'
    ]
    
    df['ydstogo'] = df['ydstogo'].fillna(10.0).astype(float)
    df['yardline_100'] = df['yardline_100'].fillna(50.0).astype(float)
    df['score_differential'] = df['score_differential'].fillna(0.0).astype(float)
    df['game_seconds_remaining'] = df['game_seconds_remaining'].fillna(1800.0).astype(float)
    
    df = df.dropna(subset=features + ['first_down', 'season'])
    print(f"Filtered to {len(df)} active 3rd/4th down conversion attempts.")
    
    # Split training window (2015-2024) and out-of-time evaluation window (2025)
    df_train_val_test = df[df['season'] <= 2024].copy()
    df_2025 = df[df['season'] == 2025].copy()
    
    print(f"Training/Validation/Test sample size (2015-2024): {len(df_train_val_test)}")
    print(f"Out-of-time evaluation sample size (2025): {len(df_2025)}")
    
    # 3. Stratified Split 70% / 15% / 15% on training window
    X = df_train_val_test[features]
    y = df_train_val_test['first_down']
    
    X_train, X_temp, y_train, y_temp = train_test_split(
        X, y, test_size=0.30, random_state=42, stratify=y
    )
    X_val, X_test, y_val, y_test = train_test_split(
        X_temp, y_temp, test_size=0.50, random_state=42, stratify=y_temp
    )
    
    print(f"  Train size:      {len(X_train)}")
    print(f"  Validation size: {len(X_val)}")
    print(f"  Holdout Test size: {len(X_test)}")
    
    # 4. Train Baseline Model (Logistic Regression)
    print("\nTraining Baseline Logistic Regression Model...")
    lr_model = LogisticRegression(solver='lbfgs', max_iter=1000, random_state=42)
    lr_model.fit(X_train, y_train)
    
    # 5. Train Primary Model (XGBoost with Monotonic Constraint on ydstogo)
    # ydstogo is the first feature, so constraint is -1. Others are 0 (no constraint).
    print("Training Primary XGBoost Classifier with negative monotonic constraint on ydstogo...")
    xgb_model = xgb.XGBClassifier(
        n_estimators=1000,
        max_depth=4,
        learning_rate=0.03,
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=42,
        tree_method='hist',
        early_stopping_rounds=50,
        monotone_constraints=(-1, 0, 0, 0)
    )
    
    xgb_model.fit(
        X_train, y_train,
        eval_set=[(X_val, y_val)],
        verbose=100
    )
    
    # 6. Evaluation on Holdout Test Set (2015-2024)
    print("\n================ EVALUATION ON HOLDOUT TEST SET (2015-2024) ================")
    lr_preds = lr_model.predict_proba(X_test)[:, 1]
    xgb_preds = xgb_model.predict_proba(X_test)[:, 1]
    
    baseline_prior = y_train.mean()
    naive_preds = np.full(y_test.shape, baseline_prior)
    
    metrics = {
        "Naive Prior": {
            "Log Loss": log_loss(y_test, naive_preds),
            "Brier Score": brier_score_loss(y_test, naive_preds),
            "ROC-AUC": 0.5000
        },
        "Logistic Regression (Baseline)": {
            "Log Loss": log_loss(y_test, lr_preds),
            "Brier Score": brier_score_loss(y_test, lr_preds),
            "ROC-AUC": roc_auc_score(y_test, lr_preds)
        },
        "XGBoost Classifier (Primary)": {
            "Log Loss": log_loss(y_test, xgb_preds),
            "Brier Score": brier_score_loss(y_test, xgb_preds),
            "ROC-AUC": roc_auc_score(y_test, xgb_preds)
        }
    }
    
    for name, met in metrics.items():
        print(f"{name}:")
        print(f"  Log Loss:    {met['Log Loss']:.6f}")
        print(f"  Brier Score: {met['Brier Score']:.6f}")
        print(f"  ROC-AUC:     {met['ROC-AUC']:.6f}")
        
    # Out-of-time evaluation on 2025 Season
    print("\n================ OUT-OF-TIME EVALUATION ON 2025 SEASON ================")
    X_2025 = df_2025[features]
    y_2025 = df_2025['first_down']
    xgb_preds_2025 = xgb_model.predict_proba(X_2025)[:, 1]
    
    print("XGBoost Classifier on 2025:")
    print(f"  Log Loss:    {log_loss(y_2025, xgb_preds_2025):.6f}")
    print(f"  Brier Score: {brier_score_loss(y_2025, xgb_preds_2025):.6f}")
    print(f"  ROC-AUC:     {roc_auc_score(y_2025, xgb_preds_2025):.6f}")

    # 7. Calibration Checks
    print("\nGenerating calibration tables...")
    df_cal = pd.DataFrame({'actual': y_test, 'predicted': xgb_preds})
    df_cal['bin'] = pd.cut(df_cal['predicted'], bins=np.arange(0.0, 1.1, 0.1))
    cal_table = df_cal.groupby('bin').agg(
        total_plays=('actual', 'count'),
        observed_wins=('actual', 'sum'),
        observed_win_rate=('actual', 'mean'),
        expected_win_rate=('predicted', 'mean')
    ).reset_index()
    
    print("\nCalibration Table (Test Set):")
    print(cal_table.to_string(index=False, formatters={
        'observed_win_rate': lambda x: f"{x*100:5.2f}%" if pd.notna(x) else "N/A",
        'expected_win_rate': lambda x: f"{x*100:5.2f}%" if pd.notna(x) else "N/A"
    }))
    
    # 8. Generate Plots
    print("\nGenerating evaluation plots...")
    # Plot 1: Calibration Curve
    prob_true, prob_pred = calibration_curve(y_test, xgb_preds, n_bins=10)
    plt.figure(figsize=(8, 6))
    plt.plot(prob_pred, prob_true, marker='o', label='XGBoost Classifier', color='#2b5c8f')
    plt.plot([0, 1], [0, 1], linestyle='--', color='gray', label='Perfect Calibration')
    plt.title('Fourth Down Conversion Model Calibration Curve (Test Set)')
    plt.xlabel('Mean Predicted Probability')
    plt.ylabel('Fraction of Positives (Observed Success Rate)')
    plt.legend(loc='lower right')
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig('docs/eda_outputs/fd_conversion_v010/calibration_curve.png', dpi=150)
    plt.close()
    
    # Plot 2: Prediction curve vs distance
    # Sweep distances from 1 to 20 yards, holding yardline_100=50, score_diff=0, game_sec=1800
    distances = np.arange(1, 21)
    sweep_df = pd.DataFrame({
        'ydstogo': distances,
        'yardline_100': np.full(distances.shape, 50.0),
        'score_differential': np.full(distances.shape, 0.0),
        'game_seconds_remaining': np.full(distances.shape, 1800.0)
    })
    xgb_sweep_preds = xgb_model.predict_proba(sweep_df)[:, 1]
    lr_sweep_preds = lr_model.predict_proba(sweep_df)[:, 1]
    
    # Empirical success rates by distance (using training window)
    emp_rates = df_train_val_test.groupby('ydstogo')['first_down'].agg(['mean', 'count']).reset_index()
    emp_rates = emp_rates[emp_rates['ydstogo'] <= 20]
    
    plt.figure(figsize=(10, 6))
    plt.plot(distances, xgb_sweep_preds * 100, label='XGBoost Model (Monotonic)', color='#2b5c8f', linewidth=2.5, marker='o')
    plt.plot(distances, lr_sweep_preds * 100, label='Logistic Regression Baseline', color='#d95f02', linestyle='--', marker='x')
    plt.scatter(emp_rates['ydstogo'], emp_rates['mean'] * 100, color='black', alpha=0.7, label='Empirical NFL Success Rate', zorder=5)
    plt.title('Fourth Down Conversion Probability by Distance (Yardline 50)')
    plt.xlabel('Yards to Go (ydstogo)')
    plt.ylabel('Conversion Success Probability (%)')
    plt.xticks(np.arange(1, 21))
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig('docs/eda_outputs/fd_conversion_v010/prediction_curve.png', dpi=150)
    plt.close()
    
    # 9. Perform EDA calculations for docs/models/fourth_down_conversion_v_0_1_0.md
    print("\n================ EDA CALCULATIONS FOR DOCUMENTATION ================")
    # Total sample statistics
    df_eda = df[(df['season'] >= 2016) & (df['season'] <= 2025)].copy()
    print(f"EDA Window (2016-2025) Total Attempts: {len(df_eda)}")
    print(f"Overall Success Rate: {df_eda['first_down'].mean() * 100:.2f}%")
    
    # Conversion by distance bins (1, 2, 3-5, 6-9, 10+)
    bins = [0, 1, 2, 5, 9, 100]
    labels = ['1 yd', '2 yds', '3-5 yds', '6-9 yds', '10+ yds']
    df_eda['dist_bin'] = pd.cut(df_eda['ydstogo'], bins=bins, labels=labels)
    dist_breakdown = df_eda.groupby('dist_bin')['first_down'].agg(['count', 'sum', 'mean']).reset_index()
    print("\nSuccess Rate by Distance Bin (2016-2025):")
    print(dist_breakdown.to_string(index=False))
    
    # Goal line vs open field (yardline_100 <= 10 vs > 10)
    df_eda['is_goal_line'] = np.where(df_eda['yardline_100'] <= 10, 'Goal Line (<=10 yds)', 'Open Field (>10 yds)')
    gl_breakdown = df_eda.groupby('is_goal_line')['first_down'].agg(['count', 'sum', 'mean']).reset_index()
    print("\nSuccess Rate Goal Line vs Open Field:")
    print(gl_breakdown.to_string(index=False))
    
    # 3rd Down vs 4th Down comparison
    down_breakdown = df_eda.groupby('down')['first_down'].agg(['count', 'sum', 'mean']).reset_index()
    print("\nSuccess Rate by Down:")
    print(down_breakdown.to_string(index=False))
    
    # Breakdown by down and distance bin (3rd down vs 4th down selection bias check)
    down_dist_breakdown = df_eda.groupby(['dist_bin', 'down'])['first_down'].agg(['count', 'mean']).reset_index()
    print("\nDown and Distance Conversion Rate Comparison:")
    print(down_dist_breakdown.to_string(index=False))
    
    # 10. Save Model and Metadata
    model_dir = "src/nfl_sim/models/fourth_down_conversion_v_0_1_0"
    model_path = os.path.join(model_dir, "fd_conversion_model.joblib")
    joblib.dump(xgb_model, model_path)
    print(f"\nModel successfully saved to {model_path}")
    
    # Save baseline model (Logistic Regression) too for comparison/fallback
    joblib.dump(lr_model, os.path.join(model_dir, "lr_baseline_model.joblib"))
    
    metadata = {
        "version": "V.0.1.0",
        "model_type": "xgb.XGBClassifier",
        "features": features,
        "fallbacks": {
            "ydstogo": 2.0,
            "yardline_100": 50.0,
            "score_differential": 0.0,
            "game_seconds_remaining": 1800.0
        },
        "training_seasons": [seasons[0], 2024],
        "split": {
            "train": 0.7,
            "validation": 0.15,
            "test": 0.15
        },
        "metrics": {
            "xgb": {
                "test_log_loss": float(metrics["XGBoost Classifier (Primary)"]["Log Loss"]),
                "test_brier_score": float(metrics["XGBoost Classifier (Primary)"]["Brier Score"]),
                "test_roc_auc": float(metrics["XGBoost Classifier (Primary)"]["ROC-AUC"]),
                "2025_log_loss": float(log_loss(y_2025, xgb_preds_2025)),
                "2025_brier_score": float(brier_score_loss(y_2025, xgb_preds_2025)),
                "2025_roc_auc": float(roc_auc_score(y_2025, xgb_preds_2025))
            },
            "lr": {
                "test_log_loss": float(metrics["Logistic Regression (Baseline)"]["Log Loss"]),
                "test_brier_score": float(metrics["Logistic Regression (Baseline)"]["Brier Score"]),
                "test_roc_auc": float(metrics["Logistic Regression (Baseline)"]["ROC-AUC"])
            }
        }
    }
    
    meta_path = os.path.join(model_dir, "metadata.json")
    with open(meta_path, 'w') as f:
        json.dump(metadata, f, indent=4)
    print(f"Metadata saved to {meta_path}")

if __name__ == "__main__":
    train_fd_conversion_model()
