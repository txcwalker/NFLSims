import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.metrics import accuracy_score, roc_auc_score

# 1. Load Data
df = pd.read_csv('data/model_1_training_data_with_macro.csv')
df = df[df['play_type'].isin(['run', 'pass'])].copy()
df['target'] = (df['play_type'] == 'pass').astype(int)

# Filter for missing fundamentals
df = df.dropna(subset=['down', 'ydstogo', 'game_seconds_remaining', 'score_differential', 'wp']).copy()

# Add prev_1_10_play_type
df_1_10 = df[(df['down'] == 1) & (df['ydstogo'] == 10)].copy()
df_1_10 = df_1_10.sort_values(by=['game_id', 'play_id'])
df_1_10['prev_1_10_play_type'] = df_1_10.groupby(['game_id', 'posteam'])['target'].shift(1)
df = df.merge(
    df_1_10[['game_id', 'play_id', 'prev_1_10_play_type']], 
    on=['game_id', 'play_id'], 
    how='left'
)
df['prev_1_10_play_type'] = df['prev_1_10_play_type'].fillna(df_1_10['target'].mean())

# Features created in notebook
df['drive_pass_count'] = df.groupby(['game_id', 'drive'])['target'].transform(
    lambda x: x.cumsum().shift(1).fillna(0)
)
df['leverage_interaction'] = df['score_differential'] * df['game_seconds_remaining']
df['momentum_interaction'] = df['down'] * df['ydstogo']
df['redzone_interaction'] = df['ydstogo'] * df['yardline_100']
df['previous_play_pass'] = (df['previous_play_type'] == 'pass').astype(int)

# Set base features
numeric_features = [
    'yardline_100', 'qtr', 'game_seconds_remaining', 'half_seconds_remaining', 
    'posteam_timeouts_remaining', 'score_differential', 'wp', 
    'l4_off_proe', 'previous_play_pass', 'drive_pass_count'
]

# Advanced Coach PROE encoding function exactly as in notebook
def train_and_eval_bucket(bucket_name):
    print(f"\n======================================")
    print(f"Analyzing Bucket: {bucket_name}")
    print(f"======================================")
    
    # 2. Bucket Assignment
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
    
    sub = df[df['bucket'] == bucket_name].copy()
    if len(sub) == 0:
        print(f"No samples for {bucket_name}")
        return

    # Check target balance
    print(f"Samples count: {len(sub)}")
    print(f"Pass rate (target = 1): {sub['target'].mean():.4f}")

    # Split Game ID
    unique_games = sub['game_id'].unique()
    np.random.seed(42)
    np.random.shuffle(unique_games)
    
    train_end = int(0.7 * len(unique_games))
    val_end = int(0.85 * len(unique_games))
    
    train_games = unique_games[:train_end]
    test_games = unique_games[val_end:]
    
    train_df = sub[sub['game_id'].isin(train_games)].copy()
    test_df = sub[sub['game_id'].isin(test_games)].copy()
    
    # Coach PROE Encoding
    league_avg_last_2 = train_df[train_df['season'].isin([2023, 2024])]['pass_oe'].mean()
    if pd.isna(league_avg_last_2):
        league_avg_last_2 = train_df['pass_oe'].mean()
        
    entire_proe = train_df.groupby('posteam_coach')['pass_oe'].mean()
    last_2_years_proe = train_df[train_df['season'].isin([2023, 2024])].groupby('posteam_coach')['pass_oe'].mean()
    
    encoded_coach_proe = {}
    for coach in train_df['posteam_coach'].unique():
        years_exp = train_df[train_df['posteam_coach'] == coach]['season'].nunique()
        if years_exp >= 2 and coach in last_2_years_proe:
            encoded_coach_proe[coach] = 0.5 * entire_proe[coach] + 0.5 * last_2_years_proe[coach]
        else:
            encoded_coach_proe[coach] = entire_proe[coach]
            
    for coach in test_df['posteam_coach'].unique():
        if coach not in encoded_coach_proe:
            team = test_df[test_df['posteam_coach'] == coach]['posteam'].iloc[0]
            team_train = train_df[train_df['posteam'] == team]
            if len(team_train) > 0:
                encoded_coach_proe[coach] = team_train['pass_oe'].mean()
            else:
                encoded_coach_proe[coach] = league_avg_last_2
                
    train_df['posteam_coach_proe'] = train_df['posteam_coach'].map(encoded_coach_proe).fillna(league_avg_last_2)
    test_df['posteam_coach_proe'] = test_df['posteam_coach'].map(encoded_coach_proe).fillna(league_avg_last_2)
    
    bucket_features = numeric_features + ['posteam_coach_proe']
    if bucket_name == "1st & 10":
        bucket_features.append('prev_1_10_play_type')

    X_train = train_df[bucket_features].copy()
    y_train = train_df['target']
    X_test = test_df[bucket_features].copy()
    y_test = test_df['target']
    
    for col in bucket_features:
        X_train[col] = X_train[col].fillna(X_train[col].median())
        X_test[col] = X_test[col].fillna(X_train[col].median())
        
    # XGBoost training
    model = xgb.XGBClassifier(n_estimators=150, max_depth=6, learning_rate=0.1, random_state=42, n_jobs=-1)
    model.fit(X_train, y_train)
    preds = model.predict_proba(X_test)[:, 1]
    
    acc = accuracy_score(y_test, (preds > 0.5).astype(int))
    auc = roc_auc_score(y_test, preds)
    
    print(f"XGBoost Test Accuracy: {acc:.4f}")
    print(f"XGBoost Test AUROC: {auc:.4f}")
    
    # Feature Importances
    importances = pd.Series(model.feature_importances_, index=bucket_features).sort_values(ascending=False)
    print("\nFeature Importances:")
    print(importances.to_string())

train_and_eval_bucket("2nd & Short")
train_and_eval_bucket("1st & 10")
train_and_eval_bucket("2nd & Medium")
