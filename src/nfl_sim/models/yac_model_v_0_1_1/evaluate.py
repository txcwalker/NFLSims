import os
import json
import pandas as pd
import numpy as np
from sklearn.metrics import mean_absolute_error, mean_squared_error
from inference import YACModelV011

DATA_PATH = "data/hardened_pass_training_master_v2_5.csv"
MODEL_DIR = "src/nfl_sim/models/yac_model_v_0_1_1"

def run_evaluation():
    print("==================================================")
    print("YAC MODEL V.0.1.1 STANDALONE EVALUATOR")
    print("==================================================")
    
    # 1. Load Model & Metadata
    print("Loading YAC V.0.1.1 inference engine...")
    yac_model = YACModelV011(model_dir=MODEL_DIR)
    
    # 2. Load Holdout Data (2024)
    print(f"Loading holdout dataset from {DATA_PATH}...")
    df = pd.read_csv(DATA_PATH, low_memory=False)
    val_df = df[(df['complete_pass'] == 1) & (df['season'] == 2024)].copy()
    
    print(f"Holdout Samples (Season 2024): {len(val_df)}")
    if len(val_df) == 0:
        print("Error: No holdout samples found for 2024!")
        return
        
    # 3. Generate Predictions using Inference Engine
    print("Running predictions over holdout plays...")
    predictions = []
    actuals = []
    
    for idx, row in val_df.iterrows():
        feat_dict = dict(row)
        pred = yac_model.predict(feat_dict)
        predictions.append(pred)
        actuals.append(row['yac_actual'])
        
    predictions = np.array(predictions)
    actuals = np.array(actuals)
    
    # 4. Compute Metrics
    mae = mean_absolute_error(actuals, predictions)
    rmse = np.sqrt(mean_squared_error(actuals, predictions))
    
    # Naive baseline (historical average YAC)
    historical_avg = yac_model.fallbacks['elusiveness'] + 4.5  # Approximate mean
    baseline_mae = mean_absolute_error(actuals, np.full_like(actuals, historical_avg))
    baseline_rmse = np.sqrt(mean_squared_error(actuals, np.full_like(actuals, historical_avg)))
    
    print("\n" + "="*50)
    print("EVALUATION RESULTS:")
    print("="*50)
    print(f"YAC V.0.1.1 Model Performance:")
    print(f"  Mean Absolute Error (MAE) : {mae:.4f} yards")
    print(f"  Root Mean Squared Error    : {rmse:.4f} yards")
    print("-"*50)
    print(f"Naive Baseline Performance (Historical Mean):")
    print(f"  Mean Absolute Error (MAE) : {baseline_mae:.4f} yards")
    print(f"  Root Mean Squared Error    : {baseline_rmse:.4f} yards")
    print("-"*50)
    print(f"Relative Improvement:")
    print(f"  MAE Reduction: {((baseline_mae - mae) / baseline_mae) * 100.0:.2f}%")
    print(f"  RMSE Reduction: {((baseline_rmse - rmse) / baseline_rmse) * 100.0:.2f}%")
    print("="*50)
    
    # 5. Partition-wise Analysis
    print("\nPartition-wise MAE Analysis:")
    partitions = {
        "Screen (Air Yards <= 0)": val_df[val_df['air_yards'] <= 0].index,
        "Standard (0 < Air Yards < 20)": val_df[(val_df['air_yards'] > 0) & (val_df['air_yards'] < 20)].index,
        "Deep (Air Yards >= 20)": val_df[val_df['air_yards'] >= 20].index
    }
    
    # Map index to positions in predictions
    val_indices = val_df.index
    for name, indices in partitions.items():
        if len(indices) == 0: continue
        # Find positions of indices in actuals/predictions
        positions = [val_indices.get_loc(idx) for idx in indices]
        part_act = actuals[positions]
        part_pred = predictions[positions]
        part_mae = mean_absolute_error(part_act, part_pred)
        print(f"  - {name:<30}: MAE = {part_mae:.4f} yards (N={len(indices)})")
        
    print("==================================================")

if __name__ == "__main__":
    run_evaluation()
