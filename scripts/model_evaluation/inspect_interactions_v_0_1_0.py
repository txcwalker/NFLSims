import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.metrics import accuracy_score, roc_auc_score

# 1. Load Data
df = pd.read_csv('data/model_1_training_data_with_macro.csv')
df = df[df['play_type'].isin(['run', 'pass'])].copy()
df['target'] = (df['play_type'] == 'pass').astype(int)

df = df.dropna(subset=['down', 'ydstogo', 'game_seconds_remaining', 'score_differential', 'wp']).copy()

# 2. Advanced Feature Engineering: Rhythm, Pace, and Interactions
df['drive_pass_count'] = df.groupby(['game_id', 'drive'])['target'].transform(
    lambda x: x.cumsum().shift(1).fillna(0)
)
df['game_pass_count'] = df.groupby(['game_id', 'posteam'])['target'].transform(
    lambda x: x.cumsum().shift(1).fillna(0)
)
df['previous_play_pass'] = (df['previous_play_type'] == 'pass').astype(int)

df['leverage_interaction'] = df['score_differential'] * df['game_seconds_remaining']
df['momentum_interaction'] = df['down'] * df['ydstogo']
df['redzone_interaction'] = df['ydstogo'] * df['yardline_100']

# Advanced Game State Feature: Play Call of the Previous 1st & 10 Play
df_1_10 = df[(df['down'] == 1) & (df['ydstogo'] == 10)].copy()
df_1_10 = df_1_10.sort_values(by=['game_id', 'play_id'])
df_1_10['prev_1_10_play_type'] = df_1_10.groupby(['game_id', 'posteam'])['target'].shift(1)
df = df.merge(
    df_1_10[['game_id', 'play_id', 'prev_1_10_play_type']], 
    on=['game_id', 'play_id'], 
    how='left'
)
df['prev_1_10_play_type'] = df['prev_1_10_play_type'].fillna(df_1_10['target'].mean())

def assign_bucket(row):
    d = int(row['down'])
    yd = int(row['ydstogo'])
    if d == 1:
        return "1st & 10" if yd == 10 else "1st & Off-Schedule"
    elif d in [2, 3, 4]:
        dist = "Short" if yd <= 3 else "Medium" if yd <= 6 else "Long"
        return f"{d}nd & {dist}" if d == 2 else f"{d}rd & {dist}" if d == 3 else f"{d}th & {dist}"
    return "Other"

df['bucket'] = df.apply(assign_bucket, axis=1)

def evaluate_features_for_bucket(bucket_name):
    print(f"\nEvaluating with + Without Interactions for Bucket: {bucket_name}")
    sub = df[df['bucket'] == bucket_name].copy()
    
    unique_games = sub['game_id'].unique()
    np.random.seed(42)
    np.random.shuffle(unique_games)
    train_end = int(0.7 * len(unique_games))
    val_end = int(0.85 * len(unique_games))
    train_games = unique_games[:train_end]
    test_games = unique_games[val_end:]
    
    train_df = sub[sub['game_id'].isin(train_games)].copy()
    test_df = sub[sub['game_id'].isin(test_games)].copy()
    
    # Simple encode coach proe
    league_avg_last_2 = train_df[train_df['season'].isin([2023, 2024])]['pass_oe'].mean()
    if pd.isna(league_avg_last_2):
        league_avg_last_2 = train_df['pass_oe'].mean()
        
    encoded_coach_proe = train_df.groupby('posteam_coach')['pass_oe'].mean().to_dict()
    
    train_df['posteam_coach_proe'] = train_df['posteam_coach'].map(encoded_coach_proe).fillna(league_avg_last_2)
    test_df['posteam_coach_proe'] = test_df['posteam_coach'].map(encoded_coach_proe).fillna(league_avg_last_2)
    
    # Setup baseline numeric features
    numeric_features = [
        'yardline_100', 'qtr', 'game_seconds_remaining', 'half_seconds_remaining', 
        'posteam_timeouts_remaining', 'score_differential', 'wp', 
        'l4_off_proe', 'previous_play_pass', 'drive_pass_count', 'posteam_coach_proe'
    ]
    if bucket_name == "1st & 10":
        numeric_features.append('prev_1_10_play_type')

    # Without interactions
    X_train_no = train_df[numeric_features].fillna(train_df[numeric_features].median())
    y_train = train_df['target']
    X_test_no = test_df[numeric_features].fillna(train_df[numeric_features].median())
    
    model_no = xgb.XGBClassifier(n_estimators=150, max_depth=6, learning_rate=0.1, random_state=42, n_jobs=-1)
    model_no.fit(X_train_no, y_train)
    preds_no = model_no.predict_proba(X_test_no)[:, 1]
    
    acc_no = accuracy_score(test_df['target'], (preds_no > 0.5).astype(int))
    auc_no = roc_auc_score(test_df['target'], preds_no)
    print(f"BASELINE:  Accuracy={acc_no:.4f}, AUROC={auc_no:.4f}")

    # With interactions & rhythm
    adv_features = numeric_features + ['leverage_interaction', 'momentum_interaction', 'redzone_interaction', 'game_pass_count']
    X_train_adv = train_df[adv_features].fillna(train_df[adv_features].median())
    X_test_adv = test_df[adv_features].fillna(train_df[adv_features].median())
    
    model_adv = xgb.XGBClassifier(n_estimators=150, max_depth=6, learning_rate=0.1, random_state=42, n_jobs=-1)
    model_adv.fit(X_train_adv, y_train)
    preds_adv = model_adv.predict_proba(X_test_adv)[:, 1]
    
    acc_adv = accuracy_score(test_df['target'], (preds_adv > 0.5).astype(int))
    auc_adv = roc_auc_score(test_df['target'], preds_adv)
    print(f"ADVANCED:  Accuracy={acc_adv:.4f}, AUROC={auc_adv:.4f}")

evaluate_features_for_bucket("1st & 10")
evaluate_features_for_bucket("2nd & Medium")
evaluate_features_for_bucket("2nd & Short")
