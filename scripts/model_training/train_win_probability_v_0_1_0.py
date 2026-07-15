import pandas as pd
import numpy as np
import xgboost as xgb
import os
import json
import joblib
import nfl_data_py as nfl
from sklearn.model_selection import train_test_split
from sklearn.metrics import log_loss, brier_score_loss, roc_auc_score

def train_win_probability_model():
    print("==================================================")
    print("NFLSims Win Probability Model V.0.1.0 Training")
    print("==================================================")
    
    # 1. Load Data
    seasons = list(range(2015, 2025))
    print(f"Loading NFL play-by-play data for seasons {seasons[0]} to {seasons[-1]} via nfl_data_py...")
    df_raw = nfl.import_pbp_data(seasons)
    print(f"Loaded {len(df_raw)} raw play records.")
    
    # 2. Derive Target and Filter Dataset
    print("\nFiltering and preparing dataset...")
    # Winner: 1 if posteam wins, 0 if they lose. Exclude ties (result == 0)
    df = df_raw[(df_raw['result'].notna()) & (df_raw['result'] != 0) & (df_raw['posteam_type'].notna())].copy()
    df['Winner'] = np.where(
        ((df['result'] > 0) & (df['posteam_type'] == 'home')) |
        ((df['result'] < 0) & (df['posteam_type'] == 'away')),
        1, 0
    )
    
    # Keep only active plays to filter out timeout/quarter end noise
    valid_plays = ["run", "pass", "punt", "kickoff", "field_goal", "extra_point", "qb_kneel", "qb_spike"]
    df = df[df['play_type'].isin(valid_plays)].copy()
    print(f"Filtered to {len(df)} active plays.")
    
    # Derive receive_2h_ko kickoff possession flag
    df['receive_2h_ko'] = np.where(
        (df['qtr'] <= 2) & (
            ((df['home_opening_kickoff'] == 1.0) & (df['posteam_type'] == 'home')) |
            ((df['home_opening_kickoff'] == 0.0) & (df['posteam_type'] == 'away'))
        ),
        1.0,
        0.0
    ).astype(float)
    
    # 3. Clean features
    features = [
        'score_differential',
        'game_seconds_remaining',
        'down',
        'ydstogo',
        'yardline_100',
        'posteam_timeouts_remaining',
        'defteam_timeouts_remaining',
        'receive_2h_ko'
    ]
    
    # Clean up NaNs in timeouts and kickoff flag
    df['receive_2h_ko'] = df['receive_2h_ko'].fillna(0.0).astype(float)
    df['posteam_timeouts_remaining'] = df['posteam_timeouts_remaining'].fillna(3.0).astype(float)
    df['defteam_timeouts_remaining'] = df['defteam_timeouts_remaining'].fillna(3.0).astype(float)
    df['down'] = df['down'].fillna(1.0).astype(float) # Fallback for kickoffs/PATs
    df['ydstogo'] = df['ydstogo'].fillna(10.0).astype(float)
    df['score_differential'] = df['score_differential'].fillna(0.0).astype(float)
    
    # Drop rows with any remaining NAs in features or target
    df = df.dropna(subset=features + ['Winner'])
    
    X = df[features].astype(float)
    y = df['Winner'].astype(int)
    
    print(f"Final cleaned samples for training: {len(X)}")
    
    # 4. Stratified Split 70% / 15% / 15%
    print("\nSplitting dataset (70% Train, 15% Val, 15% Test)...")
    X_train, X_temp, y_train, y_temp = train_test_split(
        X, y, test_size=0.30, random_state=42, stratify=y
    )
    X_val, X_test, y_val, y_test = train_test_split(
        X_temp, y_temp, test_size=0.50, random_state=42, stratify=y_temp
    )
    
    print(f"  Train size:      {len(X_train)}")
    print(f"  Validation size: {len(X_val)}")
    print(f"  Holdout Test size: {len(X_test)}")
    
    # 5. Model Selection & Hyperparameter Training
    print("\nTraining XGBoost Classifier with binary:logistic objective...")
    model = xgb.XGBClassifier(
        n_estimators=1000,
        max_depth=5,
        learning_rate=0.03,
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=42,
        tree_method='hist',
        early_stopping_rounds=50
    )
    
    model.fit(
        X_train, y_train,
        eval_set=[(X_val, y_val)],
        verbose=100
    )
    
    # 6. Evaluation on Holdout Test Set
    print("\nEvaluating on Holdout Test Set...")
    preds_proba = model.predict_proba(X_test)[:, 1]
    
    test_logloss = log_loss(y_test, preds_proba)
    test_brier = brier_score_loss(y_test, preds_proba)
    test_auc = roc_auc_score(y_test, preds_proba)
    
    # Baseline comparison (historical win rate prior)
    baseline_win_rate = y_train.mean()
    baseline_preds = np.full(y_test.shape, baseline_win_rate)
    baseline_logloss = log_loss(y_test, baseline_preds)
    baseline_brier = brier_score_loss(y_test, baseline_preds)
    
    print(f"\n--- Model Performance on Holdout Test ---")
    print(f"Model Log Loss:    {test_logloss:.4f}  (Baseline: {baseline_logloss:.4f})")
    print(f"Model Brier Score: {test_brier:.4f}  (Baseline: {baseline_brier:.4f})")
    print(f"Model ROC-AUC:     {test_auc:.4f}")
    
    # 7. Calibration Audit Table
    print("\n--- Calibration Check Bins ---")
    df_cal = pd.DataFrame({'actual': y_test, 'predicted': preds_proba})
    df_cal['bin'] = pd.cut(df_cal['predicted'], bins=np.arange(0.0, 1.1, 0.1))
    
    cal_table = df_cal.groupby('bin').agg(
        total_plays=('actual', 'count'),
        observed_wins=('actual', 'sum'),
        observed_win_rate=('actual', 'mean'),
        expected_win_rate=('predicted', 'mean')
    ).reset_index()
    
    print(cal_table.to_string(index=False, formatters={
        'observed_win_rate': lambda x: f"{x*100:5.2f}%" if pd.notna(x) else "N/A",
        'expected_win_rate': lambda x: f"{x*100:5.2f}%" if pd.notna(x) else "N/A"
    }))
    
    # 8. Save Model and Metadata
    model_dir = "src/nfl_sim/models/win_probability_v_0_1_0"
    os.makedirs(model_dir, exist_ok=True)
    
    model_path = os.path.join(model_dir, "wp_model.joblib")
    joblib.dump(model, model_path)
    print(f"\nModel successfully saved to {model_path}")
    
    # Serialize metadata
    metadata = {
        "version": "V.0.1.0",
        "model_type": "xgb.XGBClassifier",
        "features": features,
        "fallbacks": {
            "score_differential": 0.0,
            "game_seconds_remaining": 1800.0,
            "down": 1.0,
            "ydstogo": 10.0,
            "yardline_100": 75.0,
            "posteam_timeouts_remaining": 3.0,
            "defteam_timeouts_remaining": 3.0,
            "receive_2h_ko": 0.0
        },
        "training_seasons": [seasons[0], seasons[-1]],
        "split": {
            "train": 0.7,
            "validation": 0.15,
            "test": 0.15
        },
        "total_active_plays": len(df),
        "overall_prior_win_rate": float(baseline_win_rate),
        "metrics": {
            "test_log_loss": float(test_logloss),
            "test_brier_score": float(test_brier),
            "test_roc_auc": float(test_auc),
            "baseline_log_loss": float(baseline_logloss),
            "baseline_brier_score": float(baseline_brier)
        }
    }
    
    meta_path = os.path.join(model_dir, "metadata.json")
    with open(meta_path, 'w') as f:
        json.dump(metadata, f, indent=4)
    print(f"Metadata saved to {meta_path}")

if __name__ == "__main__":
    train_win_probability_model()
