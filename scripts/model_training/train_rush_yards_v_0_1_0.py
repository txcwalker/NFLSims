import pandas as pd
import numpy as np
import xgboost as xgb
import os
import json
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_absolute_error, mean_squared_error, roc_auc_score

def train_rush_yards_model():
    print("--------------------------------------------------")
    print("NFLSims Rushing Yards Model V.0.1.0 Training Script")
    print("--------------------------------------------------")
    
    # 1. Load Data
    data_path = 'data/hardened_run_training_master.csv'
    if not os.path.exists(data_path):
        raise FileNotFoundError(f"Dataset not found at {data_path}")
        
    print("Loading dataset...")
    df = pd.read_csv(data_path, low_memory=False)
    print(f"Total raw plays loaded: {len(df)}")
    
    # Filter for rushes
    df_rush = df[df['play_type'] == 'run'].copy()
    print(f"Filtered for rushing plays: {len(df_rush)}")
    
    # Define features and target
    features = [
        'yardline_100', 'ydstogo', 'game_seconds_remaining', 'score_differential',
        'yac_att_z', 'btk_rate_z', 'run_block_z', 'box_density_z', 'efficiency_z'
    ]
    target = 'yards_gained'
    
    X = df_rush[features].fillna(0.0)
    y = df_rush[target]
    
    # 2. Train/Validation/Test Split (70 / 15 / 15)
    print("\nSplitting dataset (70% Train, 15% Val, 15% Holdout Test)...")
    X_train, X_temp, y_train, y_temp = train_test_split(
        X, y, test_size=0.30, random_state=42
    )
    X_val, X_test, y_val, y_test = train_test_split(
        X_temp, y_temp, test_size=0.50, random_state=42
    )
    
    print(f"Train samples: {len(X_train)}")
    print(f"Validation samples: {len(X_val)}")
    print(f"Holdout Test samples: {len(X_test)}")
    
    # 3. Log-Shift Transformation (log(y + 30))
    y_train_log = np.log(y_train + 30.0)
    y_val_log = np.log(y_val + 30.0)
    y_test_log = np.log(y_test + 30.0)
    
    # 4. Model Configuration & Training
    print("\nTraining XGBoost Regressor with reg:squarederror on log(y+30)...")
    model = xgb.XGBRegressor(
        n_estimators=500,
        max_depth=6,
        learning_rate=0.03,
        subsample=0.8,
        colsample_bytree=0.8,
        objective='reg:squarederror',
        tree_method='hist',
        random_state=42
    )
    
    model.fit(
        X_train, 
        y_train_log,
        eval_set=[(X_val, y_val_log)],
        verbose=100
    )
    
    # 5. Evaluation on Holdout Test
    print("\nEvaluating on Holdout Test Set...")
    preds_log_scaled = model.predict(X_test)
    preds_unshifted = np.exp(preds_log_scaled) - 30.0
    preds_bounded = np.clip(preds_unshifted, -10.0, X_test['yardline_100'])
    
    mae = mean_absolute_error(y_test, preds_bounded)
    rmse = np.sqrt(mean_squared_error(y_test, preds_bounded))
    
    baseline_mean = y_train.mean()
    baseline_preds = np.full(shape=y_test.shape, fill_value=baseline_mean)
    baseline_mae = mean_absolute_error(y_test, baseline_preds)
    baseline_rmse = np.sqrt(mean_squared_error(y_test, baseline_preds))
    
    y_test_binary = (y_test >= 4.0).astype(int)
    preds_scaled = (preds_bounded - preds_bounded.min()) / (preds_bounded.max() - preds_bounded.min() + 1e-8)
    auc = roc_auc_score(y_test_binary, preds_scaled)
    
    print(f"\n--- Holdout Evaluation Metrics ---")
    print(f"Baseline (Hist Avg) MAE: {baseline_mae:.4f} yards")
    print(f"Baseline (Hist Avg) RMSE: {baseline_rmse:.4f} yards")
    print(f"Model MAE:               {mae:.4f} yards ({((baseline_mae - mae)/baseline_mae)*100:.2f}% improvement)")
    print(f"Model RMSE:              {rmse:.4f} yards ({((baseline_rmse - rmse)/baseline_rmse)*100:.2f}% improvement)")
    print(f"Success (>=4 yds) AUC:   {auc:.4f}")
    
    # 6. Partition-wise breakdown
    print("\n--- Partition-wise MAE Breakdown ---")
    df_eval = X_test.copy()
    df_eval['actual'] = y_test
    df_eval['predicted'] = preds_bounded
    
    partitions = {
        "Short Yardage / Goal Line (ydstogo <= 2)": df_eval[df_eval['ydstogo'] <= 2],
        "Standard Rushes (ydstogo 3-8)": df_eval[(df_eval['ydstogo'] > 2) & (df_eval['ydstogo'] < 9)],
        "Long Rushes (ydstogo >= 9)": df_eval[df_eval['ydstogo'] >= 9]
    }
    
    for name, part_df in partitions.items():
        if len(part_df) > 0:
            part_mae = mean_absolute_error(part_df['actual'], part_df['predicted'])
            print(f"  {name:40s} | MAE: {part_mae:.4f} yards | N: {len(part_df)}")
            
    # 7. Extract Empirical Residuals Pool
    # We calculate errors on the full training set
    print("\nCalculating empirical residuals on training set...")
    train_preds_log = model.predict(X_train)
    train_preds = np.exp(train_preds_log) - 30.0
    train_preds_bounded = np.clip(train_preds, -10.0, X_train['yardline_100'])
    train_residuals = y_train - train_preds_bounded
    
    # Subsample 2,000 representative residuals to store in JSON
    np.random.seed(42)
    residuals_pool = np.random.choice(train_residuals, size=2000, replace=False).tolist()
    print(f"Derived {len(residuals_pool)} empirical residuals (Mean error: {np.mean(residuals_pool):.4f} yards)")
    
    # 8. Scenario Distribution Test WITH Residual Noise
    print("\nRunning Scenario Distribution Testing with Empirical Residual Noise (500 Scenarios)...")
    scenarios = []
    for _ in range(500):
        yd_100 = np.random.randint(1, 99)
        down = np.random.choice([1, 2, 3, 4], p=[0.5, 0.3, 0.15, 0.05])
        dist = 10 if down == 1 else np.random.randint(1, 15)
        if yd_100 < dist: dist = yd_100
        sec_rem = np.random.randint(10, 3600)
        score_diff = np.random.randint(-24, 24)
        
        yac_z = np.random.normal(0, 1.0)
        btk_z = np.random.normal(0, 1.0)
        block_z = np.random.normal(0, 1.0)
        box_z = np.random.normal(0, 1.0)
        eff_z = np.random.normal(0, 1.0)
        
        scenarios.append({
            'yardline_100': yd_100, 'ydstogo': dist, 'game_seconds_remaining': sec_rem,
            'score_differential': score_diff, 'yac_att_z': yac_z, 'btk_rate_z': btk_z,
            'run_block_z': block_z, 'box_density_z': box_z, 'efficiency_z': eff_z
        })
        
    df_scenarios = pd.DataFrame(scenarios)
    sim_log = model.predict(df_scenarios[features])
    sim_unshifted = np.exp(sim_log) - 30.0
    
    # Add empirical noise
    noise = np.random.choice(residuals_pool, size=len(df_scenarios), replace=True)
    sim_noisy = sim_unshifted + noise
    sim_bounded = np.clip(sim_noisy, -10.0, df_scenarios['yardline_100'])
    
    print("Noisy Scenario Rushing Distribution Summary:")
    print(f"  Mean Gained:   {sim_bounded.mean():.2f} yards (Target: 4.52)")
    print(f"  Median Gained: {np.median(sim_bounded):.2f} yards (Target: 3.00)")
    print(f"  Min Gained:    {sim_bounded.min():.2f} yards (Target: -28.0)")
    print(f"  Max Gained:    {sim_bounded.max():.2f} yards (Target: 98.0)")
    print(f"  TFL Rate (<0): {(sim_bounded < 0).mean()*100:.2f}% (Target: 8.60%)")
    print(f"  Explosive Rate (>=20): {(sim_bounded >= 20).mean()*100:.2f}% (Target: 2.48%)")
    
    # 9. Save Model & Metadata
    model_dir = 'src/nfl_sim/models/rush_yards_v_0_1_0'
    os.makedirs(model_dir, exist_ok=True)
    
    model_path = os.path.join(model_dir, 'rush_yards_model.json')
    model.save_model(model_path)
    print(f"\nModel successfully saved to {model_path}")
    
    # Save metadata JSON (fallbacks, features list, residuals_pool)
    metadata = {
        "version": "V.0.1.0",
        "features": features,
        "fallbacks": {
            "yardline_100": 75.0,
            "ydstogo": 10.0,
            "game_seconds_remaining": 1800.0,
            "score_differential": 0.0,
            "yac_att_z": 0.0,
            "btk_rate_z": 0.0,
            "run_block_z": 0.0,
            "box_density_z": 0.0,
            "efficiency_z": 0.0
        },
        "residuals_pool": residuals_pool,
        "metrics": {
            "holdout_mae": float(mae),
            "holdout_rmse": float(rmse),
            "success_auc": float(auc),
            "baseline_mae": float(baseline_mae)
        }
    }
    
    meta_path = os.path.join(model_dir, 'metadata.json')
    with open(meta_path, 'w') as f:
        json.dump(metadata, f, indent=4)
    print(f"Metadata saved to {meta_path}")

if __name__ == "__main__":
    train_rush_yards_model()
