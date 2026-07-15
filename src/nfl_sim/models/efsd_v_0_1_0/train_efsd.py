import os
import json
import numpy as np
import pandas as pd
import xgboost as xgb
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score

# =============================================================================
# EFSD Model Training — V.0.1.0 (Expected Final Score Differential)
#
# Trains an XGBoost regressor that predicts the expected final margin
# (posteam final score minus defteam final score) from the current game state.
# Unlike KEP (which inverts a WP model and is bounded ±24), EFSD is a direct
# regression target and is unbounded.
#
# Stop condition: run this script, verify metrics + plots look sane, then
# proceed to efsd_inference.py.
# =============================================================================

CSV_PATH    = "data/processed/hardened_pass_training_master_v2_5.csv"
MODEL_DIR   = "src/nfl_sim/models/efsd_v_0_1_0"

TRAIN_SEASONS       = [2020, 2021, 2022, 2023]
TEST_SEASON         = 2024
INCLUDED_PLAY_TYPES = {'pass', 'run', 'kickoff', 'punt', 'field_goal',
                       'extra_point', 'qb_kneel', 'qb_spike'}

FEATURES = [
    'score_differential',
    'game_seconds_remaining',
    'down',
    'ydstogo',
    'yardline_100',
    'posteam_timeouts_remaining',
    'defteam_timeouts_remaining',
    'receive_2h_ko',
]

os.makedirs(MODEL_DIR, exist_ok=True)


# ---------------------------------------------------------------------------
# Pre-flight
# ---------------------------------------------------------------------------

def verify_columns(df):
    """
    Inputs:  raw DataFrame loaded from CSV
    Outputs: raises AssertionError with specific missing columns if schema is wrong
    Purpose: fail loudly before any computation if the CSV schema drifts
    """
    required = [
        'game_seconds_remaining', 'posteam_score', 'defteam_score',
        'home_opening_kickoff', 'home_score', 'away_score',
        'score_differential', 'down', 'ydstogo', 'yardline_100',
        'posteam_timeouts_remaining', 'defteam_timeouts_remaining',
        'play_type', 'epa', 'season', 'qtr',
        'posteam', 'home_team', 'away_team',
    ]
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise AssertionError(f"Pre-flight failed — missing columns: {missing}")
    print(f"  [OK] All {len(required)} required columns present.")


# ---------------------------------------------------------------------------
# Feature engineering
# ---------------------------------------------------------------------------

def build_features(df):
    """
    Inputs:  raw play-by-play DataFrame (post play_type filter)
    Outputs: DataFrame with target variable and all 8 model features populated
    Purpose: construct the target, receive_2h_ko, OT handling, and null fills
    """
    df = df.copy()

    # Target: final score margin from posteam's perspective
    df['final_posteam_score'] = np.where(
        df['posteam'] == df['home_team'], df['home_score'], df['away_score']
    )
    df['final_defteam_score'] = np.where(
        df['posteam'] == df['home_team'], df['away_score'], df['home_score']
    )
    df['target'] = df['final_posteam_score'] - df['final_defteam_score']

    # receive_2h_ko: 1 = posteam receives 2nd-half kickoff, 0 = they don't
    # 0.5 for OT plays (coin flip; outcome unknown at simulation time)
    df['receive_2h_ko_team'] = np.where(
        df['home_opening_kickoff'] == 1, df['home_team'], df['away_team']
    )
    df['receive_2h_ko'] = 0.0
    first_half_mask = (df['game_seconds_remaining'] > 1800) & \
                      (df['posteam'] == df['receive_2h_ko_team'])
    df.loc[first_half_mask, 'receive_2h_ko'] = 1.0
    df.loc[df['qtr'] == 5, 'receive_2h_ko'] = 0.5   # OT: 50/50 possession uncertainty

    # Null fills
    df['down']                       = df['down'].fillna(0)
    df['ydstogo']                    = df['ydstogo'].fillna(0)
    df['posteam_timeouts_remaining'] = df['posteam_timeouts_remaining'].fillna(3)
    df['defteam_timeouts_remaining'] = df['defteam_timeouts_remaining'].fillna(3)

    # Drop rows where spatial anchor or target are missing
    df = df.dropna(subset=['yardline_100', 'target'])

    return df


# ---------------------------------------------------------------------------
# Data load
# ---------------------------------------------------------------------------

def load_data():
    """
    Inputs:  CSV_PATH (hardened play-by-play master)
    Outputs: feature-engineered DataFrame filtered to included play types
    Purpose: single entry point for data ingestion and preparation
    """
    print("Loading CSV...")
    df = pd.read_csv(CSV_PATH, low_memory=False)
    print(f"  Loaded {len(df):,} raw rows.")

    verify_columns(df)

    df = df[df['play_type'].isin(INCLUDED_PLAY_TYPES)].copy()
    print(f"  After play type filter: {len(df):,} rows.")

    df = build_features(df)
    print(f"  After null drops:       {len(df):,} rows.")

    return df


# ---------------------------------------------------------------------------
# Training
# ---------------------------------------------------------------------------

def train(df):
    """
    Inputs:  feature-engineered DataFrame
    Outputs: fitted XGBRegressor, test features array, test target array, test DataFrame
    Purpose: season-stratified train/test split + XGBoost training with early stopping
    """
    train_df = df[df['season'].isin(TRAIN_SEASONS)].copy()
    test_df  = df[df['season'] == TEST_SEASON].copy()

    print(f"\nTrain rows: {len(train_df):,} | Test rows: {len(test_df):,}")

    X_full = train_df[FEATURES].values.astype(np.float32)
    y_full = train_df['target'].values.astype(np.float32)

    X_test = test_df[FEATURES].values.astype(np.float32)
    y_test = test_df['target'].values.astype(np.float32)

    # 85/15 split within training for early stopping validation
    split   = int(len(X_full) * 0.85)
    X_train, X_val = X_full[:split], X_full[split:]
    y_train, y_val = y_full[:split], y_full[split:]

    model = xgb.XGBRegressor(
        objective             = 'reg:squarederror',
        learning_rate         = 0.05,
        n_estimators          = 500,
        early_stopping_rounds = 50,
        max_depth             = 6,
        tree_method           = 'hist',
        random_state          = 42,
    )
    model.fit(
        X_train, y_train,
        eval_set  = [(X_val, y_val)],
        verbose   = 50,
    )

    return model, X_test, y_test, test_df


# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------

def evaluate(model, X_test, y_test, test_df):
    """
    Inputs:  fitted model, test arrays, test DataFrame (with epa, qtr, pred columns)
    Outputs: metrics dict, annotated test DataFrame, EPA correlation subset DataFrame
    Purpose: compute RMSE/MAE/R²/sign-accuracy/time-bucket/EPA-correlation metrics
    """
    preds = model.predict(X_test)

    rmse     = float(np.sqrt(mean_squared_error(y_test, preds)))
    mae      = float(mean_absolute_error(y_test, preds))
    r2       = float(r2_score(y_test, preds))
    sign_acc = float(np.mean(np.sign(preds) == np.sign(y_test)))

    print(f"\n{'='*50}")
    print("Test Set Metrics (Season 2024 holdout)")
    print(f"{'='*50}")
    print(f"  RMSE:          {rmse:.4f}")
    print(f"  MAE:           {mae:.4f}")
    print(f"  R²:            {r2:.4f}")
    print(f"  Sign Accuracy: {sign_acc:.4f}")

    # Time-bucket by quarter
    test_df       = test_df.copy()
    test_df['pred'] = preds
    print(f"\n--- Time-Bucket Metrics by Quarter ---")
    bucket_metrics = {}
    for q in [1, 2, 3, 4]:
        mask = test_df['qtr'] == q
        if mask.sum() == 0:
            continue
        q_rmse = float(np.sqrt(mean_squared_error(
            test_df.loc[mask, 'target'], test_df.loc[mask, 'pred']
        )))
        q_r2   = float(r2_score(test_df.loc[mask, 'target'], test_df.loc[mask, 'pred']))
        q_sign = float(np.mean(
            np.sign(test_df.loc[mask, 'pred']) == np.sign(test_df.loc[mask, 'target'])
        ))
        bucket_metrics[f'Q{q}'] = {
            'rmse': q_rmse, 'r2': q_r2, 'sign_accuracy': q_sign
        }
        print(f"  Q{q}: RMSE={q_rmse:.4f}  R²={q_r2:.4f}  Sign Acc={q_sign:.4f}")

    # EPA correlation: Δ EFSD (play-over-play change) vs nflfastR EPA
    # Sort within games to ensure meaningful diffs
    sort_cols = ['game_id', 'play_id'] if 'play_id' in test_df.columns else ['game_id']
    test_df_sorted = test_df.sort_values(sort_cols).copy()
    test_df_sorted['delta_efsd'] = test_df_sorted.groupby('game_id')['pred'].diff()
    epa_corr_df = test_df_sorted.dropna(subset=['delta_efsd', 'epa'])
    epa_corr    = float(epa_corr_df['delta_efsd'].corr(epa_corr_df['epa']))
    print(f"\n  EPA Correlation (delta EFSD vs epa): r = {epa_corr:.4f}")

    metrics = {
        'test_rmse':           rmse,
        'test_mae':            mae,
        'test_r2':             r2,
        'test_sign_accuracy':  sign_acc,
        'test_epa_correlation': epa_corr,
        'time_bucket':         bucket_metrics,
    }
    return metrics, test_df_sorted, epa_corr_df


# ---------------------------------------------------------------------------
# Diagnostic plots
# ---------------------------------------------------------------------------

def save_plots(model, test_df, epa_corr_df):
    """
    Inputs:  fitted model, annotated test DataFrame, EPA correlation subset
    Outputs: 4 PNG files saved to MODEL_DIR
    Purpose: visual diagnostics — calibration, time-decay, EPA alignment, feature importance
    """

    # 1. Calibration curve
    fig, ax = plt.subplots(figsize=(8, 6))
    pred_min = test_df['pred'].min()
    pred_max = test_df['pred'].max()
    bins = np.arange(np.floor(pred_min / 2) * 2, np.ceil(pred_max / 2) * 2 + 2, 2)
    test_df['pred_bin'] = pd.cut(test_df['pred'], bins=bins)
    cal = (
        test_df.groupby('pred_bin', observed=True)
        .agg(mean_pred=('pred', 'mean'), mean_actual=('target', 'mean'))
        .dropna()
    )
    ax.scatter(cal['mean_pred'], cal['mean_actual'], alpha=0.7, s=30, color='steelblue')
    lim = max(abs(cal['mean_pred'].min()), abs(cal['mean_actual'].max())) * 1.15
    ax.plot([-lim, lim], [-lim, lim], 'r--', lw=1.5, label='Perfect calibration (y=x)')
    ax.set_xlabel('Mean Predicted EFSD')
    ax.set_ylabel('Mean Actual Final Margin')
    ax.set_title('EFSD Calibration Curve (2-pt bins) — Season 2024')
    ax.legend()
    fig.tight_layout()
    fig.savefig(os.path.join(MODEL_DIR, 'efsd_calibration.png'), dpi=120)
    plt.close(fig)
    print("  Saved: efsd_calibration.png")

    # 2. Time-decay: RMSE and sign accuracy across game clock (120-second buckets)
    test_df['sec_bucket'] = (test_df['game_seconds_remaining'] // 120) * 120
    decay = (
        test_df.groupby('sec_bucket')
        .apply(lambda g: pd.Series({
            'rmse':     np.sqrt(mean_squared_error(g['target'], g['pred'])),
            'sign_acc': np.mean(np.sign(g['pred']) == np.sign(g['target'])),
        }))
        .reset_index()
        .sort_values('sec_bucket')
    )
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
    ax1.plot(decay['sec_bucket'], decay['rmse'], color='steelblue', lw=1.5)
    ax1.set_ylabel('RMSE (points)')
    ax1.set_title('EFSD Time-Decay Analysis — Season 2024')
    ax2.plot(decay['sec_bucket'], decay['sign_acc'], color='darkorange', lw=1.5)
    ax2.axhline(0.5, color='gray', linestyle='--', lw=1, label='Random baseline')
    ax2.set_ylabel('Sign Accuracy')
    ax2.set_xlabel('Game Seconds Remaining')
    ax2.legend()
    fig.tight_layout()
    fig.savefig(os.path.join(MODEL_DIR, 'efsd_time_decay.png'), dpi=120)
    plt.close(fig)
    print("  Saved: efsd_time_decay.png")

    # 3. EPA correlation scatter (sampled to 5k points for readability)
    fig, ax = plt.subplots(figsize=(8, 6))
    sample = epa_corr_df.sample(min(5000, len(epa_corr_df)), random_state=42)
    ax.scatter(sample['epa'], sample['delta_efsd'], alpha=0.15, s=8, color='mediumseagreen')
    ax.set_xlabel('nflfastR EPA')
    ax.set_ylabel('Δ EFSD (play-over-play)')
    ax.set_title('Δ EFSD vs EPA Correlation — Season 2024')
    r_val = float(epa_corr_df['delta_efsd'].corr(epa_corr_df['epa']))
    ax.annotate(f'r = {r_val:.4f}', xy=(0.05, 0.92), xycoords='axes fraction', fontsize=11)
    fig.tight_layout()
    fig.savefig(os.path.join(MODEL_DIR, 'efsd_epa_correlation.png'), dpi=120)
    plt.close(fig)
    print("  Saved: efsd_epa_correlation.png")

    # 4. Feature importance (gain-based)
    # Watch for score_differential dominating — if it dwarfs clock/timeout signal,
    # the model may be underfitting situational leverage.
    importances = model.get_booster().get_score(importance_type='gain')
    imp_df = (
        pd.DataFrame(list(importances.items()), columns=['feature', 'gain'])
        .sort_values('gain', ascending=True)
    )
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.barh(imp_df['feature'], imp_df['gain'], color='cornflowerblue')
    ax.set_xlabel('Gain')
    ax.set_title('EFSD Feature Importance (Gain) — V.0.1.0')
    fig.tight_layout()
    fig.savefig(os.path.join(MODEL_DIR, 'efsd_feature_importance.png'), dpi=120)
    plt.close(fig)
    print("  Saved: efsd_feature_importance.png")


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    print("=" * 60)
    print("EFSD Model Training — V.0.1.0")
    print("=" * 60)

    df                          = load_data()
    model, X_test, y_test, test_df = train(df)
    metrics, test_df_sorted, epa_corr_df = evaluate(model, X_test, y_test, test_df)

    print("\nSaving model and metadata...")
    model.save_model(os.path.join(MODEL_DIR, 'efsd_model.json'))

    metadata = {
        'version':           'V.0.1.0',
        'features':          FEATURES,
        'train_seasons':     TRAIN_SEASONS,
        'test_season':       TEST_SEASON,
        'included_play_types': sorted(INCLUDED_PLAY_TYPES),
        'hyperparameters': {
            'objective':             'reg:squarederror',
            'learning_rate':         0.05,
            'n_estimators':          500,
            'early_stopping_rounds': 50,
            'max_depth':             6,
            'tree_method':           'hist',
        },
        'metrics': metrics,
    }
    with open(os.path.join(MODEL_DIR, 'metadata.json'), 'w') as f:
        json.dump(metadata, f, indent=2)
    print(f"  Saved: metadata.json")

    print("\nGenerating diagnostic plots...")
    save_plots(model, test_df_sorted, epa_corr_df)

    print("\n[Done] EFSD V.0.1.0 training complete.")
    print(f"  Model:    {MODEL_DIR}/efsd_model.json")
    print(f"  Metadata: {MODEL_DIR}/metadata.json")
    print(f"  Plots:    {MODEL_DIR}/efsd_*.png")
