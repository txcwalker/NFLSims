import os
import pandas as pd
import numpy as np
import nfl_data_py as nfl
from scipy.stats import lognorm, gamma, expon
import statsmodels.api as sm

def fit_distributions():
    print("Loading data...")
    seasons = list(range(2016, 2026))
    df_raw = nfl.import_pbp_data(seasons)
    
    # Filter to REG
    if 'season_type' in df_raw.columns:
        pbp = df_raw[df_raw['season_type'] == 'REG'].copy()
    else:
        pbp = df_raw[df_raw['game_type'] == 'REG'].copy()
        
    print(f"Loaded {len(pbp)} regular season rows.")
    
    # ----------------------------------------------------
    # 1. Kickoffs (2024-2025)
    # ----------------------------------------------------
    print("\n--- Fitting Kickoffs (2024-2025) ---")
    ko = pbp[(pbp['play_type'] == 'kickoff') & (pbp['season'].isin([2024, 2025]))].copy()
    ko_returns = ko[(ko['touchback'] == 0) & (ko['kickoff_fair_catch'] != 1) & (ko['return_yards'].notna())]
    ko_ret_yds = ko_returns['return_yards'].values
    
    # Fit Shifted Log-Normal
    # Shifted distribution is Y = X + theta, where X ~ LogNormal
    # Fit using lognorm.fit: returns (shape/s, loc/theta, scale/exp(mu))
    # We want to fit with a lower bound shift. Let's find the minimum return yardage:
    min_ko_ret = np.min(ko_ret_yds)
    print(f"Minimum kickoff return yards: {min_ko_ret}")
    # Fit: we specify loc=min_ko_ret - 1 to allow some buffer, or let it fit loc freely.
    s_ko, loc_ko, scale_ko = lognorm.fit(ko_ret_yds)
    mu_ko = np.log(scale_ko)
    print(f"Shifted Log-Normal: shape (sigma) = {s_ko:.4f}, loc (theta) = {loc_ko:.4f}, scale = {scale_ko:.4f} (mu = {mu_ko:.4f})")
    
    # Check 2025 specific touchback rate
    ko_2025 = ko[ko['season'] == 2025]
    tb_rate_2025 = ko_2025['touchback'].mean()
    print(f"2025 Touchback Rate: {tb_rate_2025:.4f}")
    
    # ----------------------------------------------------
    # 2. Punts (2016-2025)
    # ----------------------------------------------------
    print("\n--- Fitting Punts (2016-2025) ---")
    punts = pbp[pbp['play_type'] == 'punt'].copy()
    
    # Logistic regression for touchbacks: P(touchback) ~ yardline_100
    # Filter to valid touchback and yardline_100
    punts_tb_data = punts[['yardline_100', 'touchback']].dropna()
    punts_tb_data['intercept'] = 1.0
    logit_mod = sm.Logit(punts_tb_data['touchback'], punts_tb_data[['intercept', 'yardline_100']])
    logit_res = logit_mod.fit()
    print("Logistic Regression for Punt Touchbacks:")
    print(logit_res.summary())
    intercept_pb, coef_pb = logit_res.params
    print(f"Punt Touchback Logit: intercept = {intercept_pb:.4f}, coef = {coef_pb:.4f}")
    
    # Fair catch rate of non-touchbacks
    non_tb_punts = punts[punts['touchback'] == 0]
    fc_rate = non_tb_punts['punt_fair_catch'].mean()
    print(f"Fair catch rate (of non-touchbacks): {fc_rate:.4f}")
    
    # Punt return yards
    # Filter returns: non-touchback, non-fair catch, returner name not null, yards not null
    p_returns = punts[(punts['touchback'] == 0) & (punts['punt_fair_catch'] == 0) & (punts['return_yards'].notna()) & (punts['punt_returner_player_name'].notna())]
    p_ret_yds = p_returns['return_yards'].values
    min_p_ret = np.min(p_ret_yds)
    print(f"Minimum punt return yards: {min_p_ret}")
    
    # Fit Shifted Exponential: Y = X + theta, where X ~ Exponential
    # expon.fit returns (loc/theta, scale/beta) where mean of X is scale/beta.
    loc_pe, scale_pe = expon.fit(p_ret_yds)
    print(f"Shifted Exponential: loc (theta) = {loc_pe:.4f}, scale (beta/mean) = {scale_pe:.4f}")
    
    # Punt TD rate of returns
    p_td_count = p_returns['return_touchdown'].sum()
    p_td_rate = p_td_count / len(p_returns)
    print(f"Punt Return TD Rate: {p_td_rate:.6f} ({p_td_count} TDs out of {len(p_returns)} returns)")
    
    # ----------------------------------------------------
    # 3. Interceptions (2016-2025)
    # ----------------------------------------------------
    print("\n--- Fitting Interceptions (2016-2025) ---")
    ints = pbp[pbp['interception'] == 1].copy()
    
    # TD Rate
    int_td_count = ints['return_touchdown'].sum()
    int_td_rate = int_td_count / len(ints)
    print(f"Interception TD Rate: {int_td_rate:.6f} ({int_td_count} TDs out of {len(ints)} INTs)")
    
    # Slide/Tackle rate: proportion of returns == 0
    non_td_ints = ints[ints['return_touchdown'] == 0].dropna(subset=['return_yards'])
    slide_rate = (non_td_ints['return_yards'] == 0).mean()
    print(f"Interception slide/tackle rate (return yards == 0): {slide_rate:.4f}")
    
    # Active returns (yards > 0 or yards < 0)
    act_ints = non_td_ints[non_td_ints['return_yards'] != 0]
    act_int_yds = act_ints['return_yards'].values
    min_act_int = np.min(act_int_yds)
    print(f"Minimum active interception return yards: {min_act_int}")
    
    # Fit Shifted Gamma: Y = X + theta, where X ~ Gamma
    # gamma.fit returns (shape/alpha, loc/theta, scale/beta)
    a_int, loc_int, scale_int = gamma.fit(act_int_yds)
    print(f"Shifted Gamma: shape (alpha) = {a_int:.4f}, loc (theta) = {loc_int:.4f}, scale = {scale_int:.4f}")
    
    # ----------------------------------------------------
    # 4. Fumbles (2016-2025)
    # ----------------------------------------------------
    print("\n--- Fitting Fumbles (Lost, 2016-2025) ---")
    fumbs = pbp[pbp['fumble_lost'] == 1].copy()
    
    # TD Rate
    fumb_td_count = fumbs['return_touchdown'].sum()
    fumb_td_rate = fumb_td_count / len(fumbs)
    print(f"Fumble TD Rate: {fumb_td_rate:.6f} ({fumb_td_count} TDs out of {len(fumbs)} lost fumbles)")
    
    # securing the ball rate (return yards == 0)
    non_td_fumbs = fumbs[fumbs['return_touchdown'] == 0].dropna(subset=['return_yards'])
    securing_rate = (non_td_fumbs['return_yards'] == 0).mean()
    print(f"Fumble securing rate (return yards == 0): {securing_rate:.4f}")
    
    # Active returns
    act_fumbs = non_td_fumbs[non_td_fumbs['return_yards'] != 0]
    act_fumb_yds = act_fumbs['return_yards'].values
    if len(act_fumb_yds) > 0:
        min_act_fumb = np.min(act_fumb_yds)
        print(f"Minimum active fumble return yards: {min_act_fumb}")
        # Fit Shifted Exponential
        loc_fe, scale_fe = expon.fit(act_fumb_yds)
        print(f"Shifted Exponential: loc (theta) = {loc_fe:.4f}, scale (beta) = {scale_fe:.4f}")
    else:
        print("No active fumble return yards recorded.")

if __name__ == '__main__':
    fit_distributions()
