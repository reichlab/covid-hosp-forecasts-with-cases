from sarix import sarix
import itertools
import covidcast
from datetime import date
from scipy.stats import norm
import pandas as pd
import numpy as np
from pathlib import Path
import os
import argparse

import sys
sys.path.insert(0, '..')

from sarixutil.sarixutil import load_data, save_forecast_file, save_fit_samples, build_model_name, expand_grid



if __name__ == "__main__":
    # parse arguments
    def boolean_string(s):
        if s not in {'False', 'True'}:
            raise ValueError('Not a valid boolean string')
        return s == 'True'
    
    parser = argparse.ArgumentParser(description="hierarchicalGP")
    parser.add_argument("--state", nargs="?", default='ma', type=str)
    parser.add_argument("--forecast_date", nargs="?",
                        default='2020-12-07', type=str)
    parser.add_argument("--case_type", nargs="?", default='test', type=str)
    parser.add_argument("--case_source", nargs="?", default='jhu-csse', type=str)
    parser.add_argument("--smooth_case", nargs="?", default='True', type=boolean_string)
    parser.add_argument("--data_timing", nargs="?", default='final', type=str)
    parser.add_argument("--transform", nargs="?",
                        default='fourth_rt', type=str)
    args = parser.parse_args()
    state = args.state
    forecast_date = args.forecast_date
    case_type = args.case_type
    case_source = args.case_source
    smooth_case = args.smooth_case
    data_timing = args.data_timing
    transform = args.transform
    # state = 'ma'
    # state = 'ca'
    # forecast_date = '2020-12-07'
    # case_type = 'test'
    # case_timing = 'final'
    # transform = 'fourth_rt'
    
    # define model variations to fit
    if forecast_date <= '2021-06-07':
        # validation phase
        sari_variations = expand_grid({
            'p': [p for p in range(5)],
            'P': [P for P in range(3)],
            'd': [d for d in range(2)],
            'D': [D for D in range(2)]
        })
        
        # keep only variations with some kind of lag
        sari_variations = sari_variations[(
            sari_variations.p != 0) | (sari_variations.P != 0)]
    else:
        # prospective test set evaluation phase
        # settings were chosen based on validation set performance; see eval-validation-forecasts.R
        if state == 'ma':
            if case_type == 'none':
                sari_variations = pd.DataFrame({
                    'p': [1],
                    'd': [0],
                    'P': [1],
                    'D': [1]
                })
            elif case_type == 'report':
                sari_variations = pd.DataFrame({
                    'p': [2, 1],
                    'd': [0, 0],
                    'P': [1, 1],
                    'D': [1, 1]
                })
            else:
                sari_variations = pd.DataFrame({
                    'p': [2, 2],
                    'd': [0, 0],
                    'P': [1, 0],
                    'D': [1, 1]
                })
        elif state == 'ca':
            if case_type == 'none':
                sari_variations = pd.DataFrame({
                    'p': [4],
                    'd': [1],
                    'P': [1],
                    'D': [0]
                })
            elif case_type == 'report':
                sari_variations = pd.DataFrame({
                    'p': [3, 3],
                    'd': [1, 1],
                    'P': [1, 0],
                    'D': [0, 0]
                })
            else:
                sari_variations = pd.DataFrame({
                    'p': [3, 0],
                    'd': [1, 1],
                    'P': [1, 1],
                    'D': [0, 0]
                })
    
    # keep only variations without a model fit file
    model_names = [build_model_name(case_type,
                                    case_source,
                                    smooth_case,
                                    sari_variations.p.values[i],
                                    sari_variations.d.values[i],
                                    sari_variations.P.values[i],
                                    sari_variations.D.values[i])
                   for i in range(sari_variations.shape[0])]
    file_paths = [
        Path("forecasts") / state / model_name / f"{forecast_date}-{model_name}.csv"
        for model_name in model_names]
    file_doesnt_exist = [not file_path.exists() for file_path in file_paths]
    sari_variations = sari_variations.loc[file_doesnt_exist]
    
    # only proceed if there are models to fit
    if sari_variations.shape[0] > 0:
        # load data
        # as_of is set to the value of the finalized data date
        if data_timing == 'final':
            as_of = '2022-07-22'
        else:
            as_of = forecast_date
        
        if smooth_case:
            outlier_correction = 'redist_zero_runs'
        else:
            outlier_correction = 'none'
        
        data = load_data(as_of=as_of, end_day=forecast_date,
                         case_type=case_type, case_source=case_source,
                         state=state, outlier_correction=outlier_correction)
        
        # add 7 day rolling mean of cases
        if smooth_case:
            data['case'] = data.rolling(7)[['case']].mean()
        
        # data transform
        if transform == "sqrt":
            data.case[data.case <= 0] = 1.0
            data.case = np.sqrt(data.case)
            data.hosp[data.hosp <= 0] = 1.0
            data.hosp = np.sqrt(data.hosp)
        elif transform == "fourth_rt":
            data.case[data.case <= 0] = 1.0
            data.case = np.power(data.case, 0.25)
            data.hosp[data.hosp <= 0] = 1.0
            data.hosp = np.power(data.hosp, 0.25)
        elif transform == "log":
            data.case[data.case <= 0] = 1.0
            data.case = np.log(data.case)
            data.hosp[data.hosp <= 0] = 1.0
            data.hosp = np.log(data.hosp)
        
        # figure out horizons
        # last date with observed data
        last_obs_date = pd.to_datetime(data.iloc[-1].date)
        # how far out to forecast to get to 28 days after due date
        due_date = pd.to_datetime(forecast_date)
        extra_horizons_rel_obs = (due_date - last_obs_date).days
        effective_horizon_rel_obs = 28 + extra_horizons_rel_obs
        # how many forecasts to keep relative to forecast_date
        extra_horizons_rel_forecast_date = (
            due_date - pd.to_datetime(forecast_date)).days
        effective_horizon_rel_forecast_date = int(
            28 + extra_horizons_rel_forecast_date)
        
        # quantile levels at which to generate predictions
        q_levels = np.array([0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35,
                             0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80,
                             0.85, 0.90, 0.95, 0.975, 0.99])
        # q_levels = np.array([0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975])
        
        # fit models
        for i in range(sari_variations.shape[0]):
            p = sari_variations.p.values[i]
            P = sari_variations.P.values[i]
            d = sari_variations.d.values[i]
            D = sari_variations.D.values[i]
            
            if case_type == 'none':
                modeled_vars = ['hosp']
            else:
                modeled_vars = ['case', 'hosp']
            
            sarix_fit = sarix.SARIX(
                xy=data[modeled_vars].dropna().values,
                p=p,
                d=d,
                P=P,
                D=D,
                season_period=7,
                transform="none",
                forecast_horizon=effective_horizon_rel_obs,
                num_warmup=1000,
                num_samples=1000,
                num_chains=1)
            
            pred_samples = sarix_fit.predictions
            
            # extract predictive quantiles for response variable
            hosp_pred_qs = np.percentile(
                pred_samples[:, :, -1], q_levels * 100.0, axis=0)
            
            # subset to those we want to keep
            hosp_pred_qs = hosp_pred_qs[:, extra_horizons_rel_obs:]
            
            # invert data transform
            if transform == "log":
                hosp_pred_qs = np.exp(hosp_pred_qs)
            elif transform == "fourth_rt":
                hosp_pred_qs = np.maximum(0.0, hosp_pred_qs)**4
            elif transform == "sqrt":
                hosp_pred_qs = np.maximum(0.0, hosp_pred_qs)**2
            
            if case_type == 'none':
                case_pred_qs = None
            else:
                # extract predictive quantiles for cases
                case_pred_qs = np.percentile(
                    pred_samples[:, :, -2], q_levels * 100.0, axis=0)
                
                # subset to those we want to keep
                case_pred_qs = case_pred_qs[:, extra_horizons_rel_obs:]
                
                # invert data transform
                if transform == "log":
                    case_pred_qs = np.exp(case_pred_qs)
                elif transform == "fourth_rt":
                    case_pred_qs = np.maximum(0.0, case_pred_qs)**4
                elif transform == "sqrt":
                    case_pred_qs = np.maximum(0.0, case_pred_qs)**2
            
            model_name = build_model_name(
                case_type, case_source, smooth_case, p, d, P, D)
            save_forecast_file(state=state,
                               forecast_date=forecast_date,
                               hosp_pred_qs=hosp_pred_qs,
                               case_pred_qs=case_pred_qs,
                               q_levels=q_levels,
                               model_name=model_name)
            param_samples = {k: v for k, v in sarix_fit.samples.items()
                             if k in ['betas_update_var', 'theta']}
            save_fit_samples(state=state,
                             forecast_date=forecast_date,
                             param_samples=param_samples,
                             pred_samples=pred_samples,
                             model_name=model_name)
