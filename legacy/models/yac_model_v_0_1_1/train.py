# RETIRED — superseded by src/nfl_sim/models/yac_model_v_0_1_1/train_zone_split.py.
# DO NOT RUN. This is the old single-model (non-zone-split) YAC trainer; its
# output has a different feature set/shape than the deployed
# {primary,redzone,goalline} artifacts, so running it would silently replace the
# live model with an incompatible one. Kept for historical reference only.
# See AGENTS.md §11.7.

import os
import json
import joblib
import pandas as pd
import numpy as np
import xgboost as xgb
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import KFold
from sklearn.metrics import mean_absolute_error, mean_squared_error

# Config
DATA_PATH = "data/hardened_pass_training_master_v2_5.csv"
MODEL_DIR = "src/nfl_sim/models/yac_model_v_0_1_1"
EDA_DIR = "docs/eda_outputs/yac/evaluation_v_0_1_1"

os.makedirs(MODEL_DIR, exist_ok=True)
os.makedirs(EDA_DIR, exist_ok=True)

def run_training_pipeline():
    print("==================================================")
    print("YAC MODEL V.0.1.1 DEVELOPMENT PIPELINE")
    print("==================================================")
    
    # 1. Load Data
    print(f"Loading master passing dataset from {DATA_PATH}...")
    df = pd.read_csv(DATA_PATH, low_memory=False)
    
    # Filter for completions
    df_comp = df[df['complete_pass'] == 1].copy()
    print(f"Total completed passes loaded: {len(df_comp)}")
    
    # 2. Temporal Split
    # Training: 2020-2023, Validation: 2024
    train_df = df_comp[df_comp['season'] < 2024].copy()
    val_df = df_comp[df_comp['season'] == 2024].copy()
    
    print(f"Training samples (2020-2023): {len(train_df)}")
    print(f"Validation samples (2024): {len(val_df)}")
    
    # 3. Compute Player DNA Traits on Training Split
    print("\nComputing Receiver and QB DNA from training window...")
    
    # Receiver DNA: elusiveness (YAC over expected) and broken tackle rate
    # Expected YAC is from xyac_mean_yardage
    train_df['yac_over_expected'] = train_df['yac_actual'] - train_df['xyac_mean_yardage']
    train_df['high_yac_play'] = (train_df['yac_over_expected'] > 3.0).astype(int)
    
    receiver_dna = {}
    receiver_groups = train_df.groupby('receiver_player_name')
    for name, group in receiver_groups:
        if len(group) >= 10:  # Min targets threshold for stable DNA
            receiver_dna[name] = {
                "elusiveness": float(group['yac_over_expected'].mean()),
                "broken_tackle_rate": float(group['high_yac_play'].mean()),
                "catch_rate": float(group['complete_pass'].mean()),  # Proxy or standard
                "avg_separation_yds": float(group['avg_separation'].mean())
            }
            
    # QB DNA: pressure rate
    train_df['is_pressured'] = train_df['was_pressure'].fillna(0).astype(int)
    qb_dna = {}
    qb_groups = train_df.groupby('passer_player_name')
    for name, group in qb_groups:
        if len(group) >= 15:
            qb_dna[name] = {
                "pressure_rate": float(group['is_pressured'].mean()),
                "cpoe": float(group['cpoe'].mean()) if 'cpoe' in group.columns else 0.0,
                "avg_time_to_throw_sec": float(group['time_to_throw'].mean()) if 'time_to_throw' in group.columns else 2.7
            }
            
    # Save DNA mapping to use at inference
    fallbacks = {
        "elusiveness": 0.0,
        "broken_tackle_rate": 0.15,
        "catch_rate": 0.65,
        "avg_separation_yds": 2.9,
        "pressure_rate": 0.20,
        "cpoe": 0.0,
        "avg_time_to_throw_sec": 2.7
    }
    
    # 4. Map DNA Features to Train and Val Sets
    def map_dna(target_df):
        # Receiver DNA
        target_df['elusiveness'] = target_df['receiver_player_name'].map(
            lambda x: receiver_dna.get(x, {}).get('elusiveness', fallbacks['elusiveness'])
        )
        target_df['broken_tackle_rate'] = target_df['receiver_player_name'].map(
            lambda x: receiver_dna.get(x, {}).get('broken_tackle_rate', fallbacks['broken_tackle_rate'])
        )
        target_df['catch_rate'] = target_df['receiver_player_name'].map(
            lambda x: receiver_dna.get(x, {}).get('catch_rate', fallbacks['catch_rate'])
        )
        target_df['avg_separation_yds'] = target_df['receiver_player_name'].map(
            lambda x: receiver_dna.get(x, {}).get('avg_separation_yds', fallbacks['avg_separation_yds'])
        )
        
        # QB DNA
        target_df['pressure_rate'] = target_df['passer_player_name'].map(
            lambda x: qb_dna.get(x, {}).get('pressure_rate', fallbacks['pressure_rate'])
        )
        target_df['cpoe'] = target_df['passer_player_name'].map(
            lambda x: qb_dna.get(x, {}).get('cpoe', fallbacks['cpoe'])
        )
        target_df['avg_time_to_throw_sec'] = target_df['passer_player_name'].map(
            lambda x: qb_dna.get(x, {}).get('avg_time_to_throw_sec', fallbacks['avg_time_to_throw_sec'])
        )
        
        # Play Partition
        target_df['is_screen'] = (target_df['air_yards'] <= 0).astype(int)
        target_df['is_std'] = ((target_df['air_yards'] > 0) & (target_df['air_yards'] < 20)).astype(int)
        target_df['is_deep'] = (target_df['air_yards'] >= 20).astype(int)
        
        # Additional contextual cleanups
        target_df['score_differential'] = target_df['score_differential'].fillna(0)
        target_df['game_seconds_remaining'] = target_df['game_seconds_remaining'].fillna(1800)
        target_df['timeouts_pos'] = target_df['temp_timeouts_remaining_pos'].fillna(3) if 'temp_timeouts_remaining_pos' in target_df.columns else 3
        target_df['timeouts_def'] = target_df['temp_timeouts_remaining_def'].fillna(3) if 'temp_timeouts_remaining_def' in target_df.columns else 3
        
        return target_df

    print("Mapping DNA and contextual features to dataframes...")
    train_df = map_dna(train_df)
    val_df = map_dna(val_df)
    
    # 5. Define Feature Sets
    features = [
        # Spatial
        'air_yards', 'yardline_100', 'ydstogo',
        # Receiver DNA
        'elusiveness', 'broken_tackle_rate', 'catch_rate', 'avg_separation_yds',
        # QB DNA
        'cpoe', 'avg_time_to_throw_sec', 'pressure_rate',
        # Contextual
        'score_differential', 'game_seconds_remaining', 'timeouts_pos', 'timeouts_def',
        # Partition
        'is_screen', 'is_std', 'is_deep'
    ]
    
    X_train = train_df[features]
    y_train = train_df['yac_actual']
    
    X_val = val_df[features]
    y_val = val_df['yac_actual']
    
    # 6. Hyperparameter Tuning (Manual Grid Search)
    print("\nStarting Hyperparameter Grid Search...")
    grid = {
        'max_depth': [4, 6, 8],
        'learning_rate': [0.03, 0.05, 0.1],
        'subsample': [0.8, 1.0]
    }
    
    best_mae = float('inf')
    best_params = {}
    
    # Define a simple 3-fold cross validation on training split
    kf = KFold(n_splits=3, shuffle=True, random_state=42)
    
    for depth in grid['max_depth']:
        for lr in grid['learning_rate']:
            for subs in grid['subsample']:
                print(f" Evaluating max_depth={depth}, lr={lr}, subsample={subs}...")
                maes = []
                for train_idx, val_idx in kf.split(X_train):
                    X_tr, X_va = X_train.iloc[train_idx], X_train.iloc[val_idx]
                    y_tr, y_va = y_train.iloc[train_idx], y_train.iloc[val_idx]
                    
                    model = xgb.XGBRegressor(
                        objective='reg:absoluteerror',
                        n_estimators=300,
                        max_depth=depth,
                        learning_rate=lr,
                        subsample=subs,
                        random_state=42,
                        tree_method='hist'
                    )
                    model.fit(X_tr, y_tr)
                    preds = model.predict(X_va)
                    maes.append(mean_absolute_error(y_va, preds))
                    
                mean_mae = np.mean(maes)
                print(f"  CV Mean MAE = {mean_mae:.4f}")
                if mean_mae < best_mae:
                    best_mae = mean_mae
                    best_params = {'max_depth': depth, 'learning_rate': lr, 'subsample': subs}
                    
    print(f"\nBest Hyperparameters found: {best_params} (MAE: {best_mae:.4f})")
    
    # 7. Train Final Model
    print("\nTraining final model on full 2020-2023 dataset...")
    best_model = xgb.XGBRegressor(
        objective='reg:absoluteerror',
        n_estimators=600,
        max_depth=best_params['max_depth'],
        learning_rate=best_params['learning_rate'],
        subsample=best_params['subsample'],
        random_state=42,
        tree_method='hist'
    )
    best_model.fit(X_train, y_train)
    
    # 8. Evaluate on Holdout (2024)
    print("\nEvaluating on 2024 Holdout Season...")
    preds_val = best_model.predict(X_val)
    mae_val = mean_absolute_error(y_val, preds_val)
    rmse_val = np.sqrt(mean_squared_error(y_val, preds_val))
    
    # Baseline comparison (naive baseline = average YAC)
    baseline_mae = mean_absolute_error(y_val, np.full_like(y_val, y_train.mean()))
    baseline_rmse = np.sqrt(mean_squared_error(y_val, np.full_like(y_val, y_train.mean())))
    
    print("--------------------------------------------------")
    print(f"YAC V.0.1.1 HOLDOUT PERFORMANCE (2024):")
    print(f"  MODEL MAE    : {mae_val:.4f} yds  (vs Baseline MAE: {baseline_mae:.4f} yds)")
    print(f"  MODEL RMSE   : {rmse_val:.4f} yds  (vs Baseline RMSE: {baseline_rmse:.4f} yds)")
    print("--------------------------------------------------")
    
    # 9. Generate and Save EDA/Evaluation Plots
    print("\nGenerating evaluation visualizations...")
    
    # Plot 1: YAC Distribution by Play Partition
    plt.figure(figsize=(10, 6))
    partitions = {
        "Screen (≤0 yds)": val_df[val_df['is_screen'] == 1]['yac_actual'],
        "Standard (0-20 yds)": val_df[val_df['is_std'] == 1]['yac_actual'],
        "Deep (≥20 yds)": val_df[val_df['is_deep'] == 1]['yac_actual']
    }
    for label, Y in partitions.items():
        sns.kdeplot(Y, label=f"{label} (N={len(Y)})", fill=True, alpha=0.3)
    plt.xlim(-5, 30)
    plt.title("Distribution of Yards After Catch (YAC) by Pass Type (2024 Holdout)")
    plt.xlabel("Yards After Catch")
    plt.ylabel("Density")
    plt.legend()
    plt.savefig(os.path.join(EDA_DIR, "yac_by_partition.png"))
    plt.close()
    
    # Plot 2: Correlation Heatmap
    plt.figure(figsize=(12, 10))
    corr_cols = ['yac_actual', 'air_yards', 'yardline_100', 'ydstogo', 'elusiveness', 'broken_tackle_rate', 'avg_separation_yds', 'avg_time_to_throw_sec', 'pressure_rate']
    sns.heatmap(val_df[corr_cols].corr(), annot=True, cmap='coolwarm', fmt='.2f', vmin=-1, vmax=1)
    plt.title("YAC & DNA Feature Correlations (2024 Holdout)")
    plt.tight_layout()
    plt.savefig(os.path.join(EDA_DIR, "yac_correlations.png"))
    plt.close()
    
    # Plot 3: Feature Importance
    plt.figure(figsize=(10, 8))
    importances = best_model.feature_importances_
    feat_imp = pd.Series(importances, index=features).sort_values(ascending=True)
    feat_imp.plot(kind='barh', color='teal')
    plt.title("YAC Model V.0.1.1 - Feature Importance")
    plt.xlabel("Relative Importance")
    plt.tight_layout()
    plt.savefig(os.path.join(EDA_DIR, "feature_importance.png"))
    plt.close()
    
    # Plot 4: Calibration Plot (Mean Predicted vs Mean Observed in 5-yard bins)
    plt.figure(figsize=(8, 6))
    eval_df = pd.DataFrame({'actual': y_val, 'predicted': preds_val})
    eval_df['bin'] = pd.cut(eval_df['predicted'], bins=np.arange(-5, 25, 2))
    binned = eval_df.groupby('bin', observed=False).mean()
    plt.scatter(binned['predicted'], binned['actual'], color='blue', s=80, label='Bins (width=2 yds)')
    plt.plot([-5, 20], [-5, 20], color='red', linestyle='--', label='Perfect Calibration')
    plt.xlabel("Mean Predicted YAC")
    plt.ylabel("Mean Actual YAC")
    plt.title("YAC Model Calibration (2024 Holdout)")
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.savefig(os.path.join(EDA_DIR, "yac_calibration.png"))
    plt.close()
    
    # 10. Save Model Artifacts
    print(f"\nSaving model joblib to {MODEL_DIR}...")
    joblib.dump(best_model, os.path.join(MODEL_DIR, "yac_reg_v_0_1_1.joblib"))
    
    # Save metadata.json
    metadata = {
        "version": "V.0.1.1",
        "features": features,
        "hyperparameters": best_params,
        "performance": {
            "mae": float(mae_val),
            "rmse": float(rmse_val),
            "baseline_mae": float(baseline_mae),
            "baseline_rmse": float(baseline_rmse)
        },
        "fallbacks": fallbacks,
        "dna": {
            "receiver": receiver_dna,
            "qb": qb_dna
        }
    }
    
    with open(os.path.join(MODEL_DIR, "metadata.json"), 'w') as f:
        json.dump(metadata, f, indent=4)
        
    print("YAC Model and Metadata successfully saved!")
    print("==================================================")

if __name__ == "__main__":
    run_training_pipeline()
