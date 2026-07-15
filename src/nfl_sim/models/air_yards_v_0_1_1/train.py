import os
import json
import pandas as pd
import numpy as np
import xgboost as xgb
import nfl_data_py as nfl
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, mean_squared_error
import joblib

# =============================================================================
# Air Yards Model Training — V.0.1.1 (Double Hurdle Model)
# =============================================================================

# Config
SEASONS = [2020, 2021, 2022, 2023, 2024]
DNA_DIR = "data/dna"
MODEL_DIR = "src/nfl_sim/models/air_yards_v_0_1_1"
SCREEN_MAX = 0
DEEP_MIN = 20

os.makedirs(MODEL_DIR, exist_ok=True)

def load_dna_atlases():
    atlases = {}
    for name in ['qb_dna', 'skill_dna', 'coach_dna', 'trench_dna']:
        path = os.path.join(DNA_DIR, f"{name}.json")
        with open(path, 'r') as f:
            data = json.load(f)
            atlases[name] = {k: v for k, v in data.items() if not k.startswith('_')}
    return atlases

def preprocess_data(pbp, atlases):
    df = pbp[
        (pbp['play_type'] == 'pass') &
        (pbp['qb_spike'] == 0) &
        (pbp['aborted_play'] == 0) &
        (pbp['air_yards'].notna())
    ].copy()

    df['off_coach'] = np.where(df['posteam'] == df['home_team'], df['home_coach'], df['away_coach'])
    
    # DNA Joins
    qb_features_raw = ['cpoe', 'avg_air_yards_per_att', 'deep_ball_rate', 'scramble_rate', 'sack_rate', 'play_action_rate', 'under_pressure_cpoe', 'ngs_aggressiveness_index', 'avg_time_to_throw_sec']
    qb_df = pd.DataFrame.from_dict(atlases['qb_dna'], orient='index')[qb_features_raw].rename(columns={c: f"{c}_qb" for c in qb_features_raw})
    df = df.merge(qb_df, left_on='passer_player_name', right_index=True, how='left')

    skill_features_raw = ['target_share', 'catch_rate', 'avg_target_depth_yds', 'deep_target_rate', 'air_yards_share', 'yac_per_reception', 'avg_separation_yds']
    skill_df = pd.DataFrame.from_dict(atlases['skill_dna'], orient='index')[skill_features_raw].rename(columns={c: f"{c}_recv" for c in skill_features_raw})
    df = df.merge(skill_df, left_on='receiver_player_name', right_index=True, how='left')

    coach_features_raw = ['air_yards_tendency', 'deep_shot_rate', 'screen_rate', 'play_action_rate', 'no_huddle_rate', 'rpo_rate', 'conservative_score_bias']
    coach_df = pd.DataFrame.from_dict(atlases['coach_dna'], orient='index')[coach_features_raw].rename(columns={c: f"{c}_coach" for c in coach_features_raw})
    df = df.merge(coach_df, left_on='off_coach', right_index=True, how='left')

    trench_rows = []
    for season, teams in atlases['trench_dna'].items():
        for team, metrics in teams.items():
            metrics['season'], metrics['team'] = int(season), team
            trench_rows.append(metrics)
    trench_flat = pd.DataFrame(trench_rows)
    df = df.merge(trench_flat, left_on=['season', 'posteam'], right_on=['season', 'team'], how='left', suffixes=('', '_off_trench'))
    df = df.merge(trench_flat, left_on=['season', 'defteam'], right_on=['season', 'team'], how='left', suffixes=('', '_def_trench'))

    situational_cols = ['down', 'ydstogo', 'yardline_100', 'qtr', 'score_differential', 'half_seconds_remaining', 'game_seconds_remaining']
    feature_cols = situational_cols + [f"{c}_qb" for c in qb_features_raw] + [f"{c}_recv" for c in skill_features_raw] + [f"{c}_coach" for c in coach_features_raw] + ['def_pressure_rate_def_trench', 'def_sack_rate_def_trench']

    for col in feature_cols:
        df[col] = pd.to_numeric(df[col], errors='coerce')
        df[col] = df[col].fillna(df[col].median())

    df['ay_level'] = 1 # default standard
    df.loc[df['air_yards'] <= SCREEN_MAX, 'ay_level'] = 0
    df.loc[df['air_yards'] >= DEEP_MIN, 'ay_level'] = 2
    
    return df, feature_cols

def train_double_hurdle(df, features):
    X = df[features]
    y_class = df['ay_level']
    X_train, X_test, y_c_train, y_c_test = train_test_split(X, y_class, test_size=0.2, random_state=42)
    
    # 1. Gate
    gate_model = xgb.XGBClassifier(objective='multi:softprob', num_class=3, n_estimators=500, learning_rate=0.05, max_depth=5, random_state=42)
    gate_model.fit(X_train, y_c_train, eval_set=[(X_test, y_c_test)], verbose=False)
    
    # 2. Regressors
    regressors = {}
    for level, name, mask in [(0, "Screen", df['ay_level'] == 0), (1, "Standard", df['ay_level'] == 1), (2, "Deep", df['ay_level'] == 2)]:
        X_sub, y_sub = df.loc[mask, features], df.loc[mask, 'air_yards']
        reg_model = xgb.XGBRegressor(objective='reg:squarederror', n_estimators=500, learning_rate=0.05, max_depth=(6 if level==1 else 4), random_state=42)
        reg_model.fit(X_sub, y_sub, verbose=False)
        regressors[level] = reg_model
    return gate_model, regressors, X_test, y_c_test

if __name__ == "__main__":
    pbp = nfl.import_pbp_data(SEASONS)
    atlases = load_dna_atlases()
    df, features = preprocess_data(pbp, atlases)
    gate, regressors, X_test, y_c_test = train_double_hurdle(df, features)
    joblib.dump(gate, os.path.join(MODEL_DIR, "tri_gate_v_0_1_1.joblib"))
    joblib.dump(regressors[0], os.path.join(MODEL_DIR, "screen_reg_v_0_1_1.joblib"))
    joblib.dump(regressors[1], os.path.join(MODEL_DIR, "std_reg_v_0_1_1.joblib"))
    joblib.dump(regressors[2], os.path.join(MODEL_DIR, "deep_reg_v_0_1_1.joblib"))
    with open(os.path.join(MODEL_DIR, "metadata.json"), 'w') as f:
        json.dump({"version": "V.0.1.1", "features": features, "levels": {"0": "screen", "1": "standard", "2": "deep"}, "screen_max": SCREEN_MAX, "deep_min": DEEP_MIN}, f, indent=2)
