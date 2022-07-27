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
sys.path.insert(0, '.')


def expand_grid(data_dict):
    """Create a dataframe from every combination of given values."""
    rows = itertools.product(*data_dict.values())
    return pd.DataFrame.from_records(rows, columns=data_dict.keys())


def count_zeros(values):
    '''Count the number of entries in values that are zero'''
    return sum(values == 0.)


def prop_zeros(values):
    '''Calculate the proportion of entries in values that are zero'''
    return sum(values == 0.) / len(values)


def clean_outliers(values):
    result = values.copy()
    
    # set all trailing zeros to nan; will be filled with the last nonzero value later
    num_trailing_zeros = len(result.values) - np.max(np.nonzero(values.values)) - 1
    if num_trailing_zeros > 0:
        result[-num_trailing_zeros:] = np.nan
    
    # count number of zero values in a centered rolling 7 day window
    # fill in zero counts at end with last value
    zero_counts = result.rolling(7, center = True).apply(count_zeros)
    zero_counts = zero_counts.fillna(method='ffill')
    
    zero_props = result.rolling(7, center = True).apply(prop_zeros)
    zero_props = zero_props.fillna(method='ffill')
    
    # if more than 2 days of zeros in rolling window, replace with rolling mean
    inds_to_replace = (zero_counts > 2) & (zero_props < 1.0)
    replace_vals = result.rolling(7, center = True).mean()
    replace_vals = replace_vals.fillna(method='ffill')
    result[inds_to_replace] = replace_vals[inds_to_replace]
    
    # if 1 or 2 days of zeros in rolling window, set to nan
    inds_to_replace = (((zero_counts > 0) & (zero_counts <= 2)) | zero_props == 1.0) & (result == 0.)
    result[inds_to_replace] = np.nan
    
    # detect outliers as rolling median +/- 3 IQR, set to nan
    rm = result.rolling(15, center = True, min_periods=1).median()
    resids = result - rm
    riqr = resids.rolling(15, center = True, min_periods=1).quantile(0.75) - \
        resids.rolling(15, center = True, min_periods=1).quantile(0.25)
    lower = (rm - 3 * riqr).fillna(method = 'ffill')
    upper = (rm + 3 * riqr).fillna(method = 'ffill')
    is_outlr = (result < lower) | (result > upper)
    result[is_outlr] = np.nan
    
    # linearly interpolate nan's (corresponding to zeros and detected outliers)
    result.interpolate(inplace=True, limit_direction='both')
    
    return result


def redist_zero_runs(values, include_leading = False):
    # adapted from https://stackoverflow.com/questions/1066758/find-length-of-sequences-of-identical-values-in-a-numpy-array-run-length-encodi
    values = values.copy()
    lead_adj = -1 * include_leading
    n = len(values)
    zero_ind = (values == 0.)
    y = zero_ind[1:] != zero_ind[:-1]   # pairwise unequal
    i = np.append(np.where(y), n - 1)   # must include last element posi
    z = np.diff(np.append(-1, i))       # run lengths
    p = np.cumsum(np.append(0, z))[:-1] # positions
    if values[0] != 0.:
        i = i[1:]
        z = z[1:]
        p = p[1:]
    
    # following line throws an error if last value is a zero
    ranges = p.reshape((len(p) // 2, 2))
    for j in range(ranges.shape[0]):
        values[(ranges[j, 0] + lead_adj):(ranges[j,1]+1)] = np.mean(values[(ranges[j, 0] + lead_adj):(ranges[j,1]+1)])
    
    # set trailing zeros to nan and then fill with the last nonzero value
    num_trailing_zeros = len(values) - np.max(np.nonzero(values)) - 1
    if num_trailing_zeros > 0:
        values[-num_trailing_zeros:] = np.nan
    
    values.interpolate(inplace=True, limit_direction='both')
    
    return values


def load_data(as_of=None,
              end_day="2021-07-01",
              case_type='report',
              state='ma',
              case_source = 'jhu-csse',
              outlier_correction = 'none'):
    """
    Load data for cases and hosps from covidcast or state DPH records
    Parameters
    ----------
    as_of: string of date in YYYY-MM-DD format.
            Default to None.
    end_day: string of date in YYYY-MM-DD format.
            Default to "2021-07-01"
    case_type: string of recording method for Covid-19 cases
            Default to 'report'
    state: string of a 2-letter state abbreviation.
            Default to 'ma'
    case_source: string specifying data source for case data
            Default to 'jhu-csse', other option is 'dph' for state dph records
    outlier_correction: string specifying method for outlier correction.
            Correction is done only for outliers in report date cases.
            Default to 'none', in which case the original data are returned.
            'interp_zeros' linearly interpolates zeros.
            'interp_zeros_outliers' linearly interpolates zeros and outliers
            detected as being more than 3 IQR's away from a rolling median
            'redist_zero_runs' replaces runs of zeros and the adjacent days with
            the mean over that time span.
    
    Returns
    -------
    df: data frame
            It has columns location, date, inc_hosp, population and rate.
            It is sorted by location and date columns in ascending order.
    """
    # override as_of = None to use the as_of for "finalized data"
    if as_of is None:
        as_of = '2022-04-29'
    
    # load hospitalizations
    hosp_df = covidcast.signal(data_source="hhs",
                               signal="confirmed_admissions_covid_1d",
                               start_day=date.fromisoformat("2020-10-01"),
                               end_day=date.fromisoformat(end_day),
                               geo_type="state",
                               geo_values=state,
                               as_of=date.fromisoformat(as_of))
    hosp_df = hosp_df[["geo_value", "time_value", "value"]]
    hosp_df.columns = ["location", "date", "hosp"]
    
    # load cases
    if case_type == 'report':
        # if case_timing == 'final':
        #     case_as_of = '2022-04-29'
        # else:
        #     case_as_of = as_of
        
        if case_source == 'jhu-csse':
            case_df = covidcast.signal(data_source="jhu-csse",
                                    signal="confirmed_incidence_num",
                                    start_day=date.fromisoformat("2020-10-01"),
                                    end_day=date.fromisoformat(end_day),
                                    geo_type="state",
                                    geo_values=state,
                                    as_of=date.fromisoformat(as_of))
            case_df = case_df[["geo_value", "time_value", "value"]]
            case_df.columns = ["location", "date", "case"]
        elif case_source == 'dph':
            if state == 'ca':
                # if case_timing == 'final':
                #     case_as_of = '2022-04-29'
                # else:
                #     raise ValueError("For case_source 'dph', only case_timing 'final' is supported")
                
                case_df_path = 'csv-data/CA-DPH-reportdate-covid-' + as_of + '.csv'
                case_df = pd.read_csv(case_df_path)
                case_df['location'] = state
                case_df = case_df[['location', 'report_date', 'new_positive']]
                case_df.columns = ['location', 'date', 'case']
                
                case_df = case_df[(case_df.date >= '2020-10-01')
                                & (case_df.date <= end_day)]
                case_df.date = pd.to_datetime(case_df.date)
                
            else:
                raise ValueError("For case_source 'dph', only state 'ca' is supported")
        
        # quick fix to zero values; replace with nan and then interpolate missing
        if outlier_correction == 'interp_zeros':
            zero_inds = np.where(case_df.case == 0)
            case_df['case'].iloc[zero_inds] = np.nan
            case_df['case'] = case_df['case'].interpolate()
        elif outlier_correction == 'interp_zeros_outliers':
            case_df.case = clean_outliers(case_df.case)
        elif outlier_correction == 'redist_zero_runs':
            case_df.case = redist_zero_runs(case_df.case.values, include_leading=True)
        elif outlier_correction != 'none':
            raise ValueError("outlier_correction must be 'none', 'interp_zeros', 'interp_zeros_outliers', or 'redist_zero_runs'")
    else:
        # csv_files = os.listdir('csv-data')
        # as_ofs = [f[21:-4] for f in csv_files]
        # if case_timing == 'final':
        #     if state == 'ma':
        #         case_as_of = '2022-04-28'
        #     elif state == 'ca':
        #         case_as_of = '2022-04-29'
        # else:
        #     subset_as_ofs = [ao for ao in as_ofs if ao <= as_of]
        #     case_as_of = max(subset_as_ofs)
        
        if state == 'ma':
            case_df_path = 'csv-data/MA-DPH-csvdata-covid-' + as_of + '.csv'
        elif state == 'ca':
            case_df_path = 'csv-data/CA-DPH-testdate-covid-' + as_of + '.csv'
        
        case_df = pd.read_csv(case_df_path)
        case_df['location'] = state
        case_df = case_df[['location', 'test_date', 'new_positive']]
        case_df.columns = ['location', 'date', 'case']
        
        case_df = case_df[(case_df.date >= '2020-10-01')
                          & (case_df.date <= end_day)]
        case_df.date = pd.to_datetime(case_df.date)
    
    # merge
    df = case_df.merge(hosp_df, on=["location", "date"], how="left")
    
    # sort by date
    df = df.sort_values(by = 'date')
    
    # ensure float data type
    df[['case', 'hosp']] = df[['case', 'hosp']].astype('float64')
    
    # drop missing values; assumed to be trailing (dangerous?)
    print(df)
    df = df.dropna()
    
    return df



def construct_forecast_df(location, forecast_date, pred_qs, q_levels, base_target):
    # format predictions for one target variable as a data frame with required columns
    horizons_str = [str(i + 1) for i in range(28)]
    preds_df = pd.DataFrame(pred_qs, columns=horizons_str)
    preds_df['forecast_date'] = forecast_date
    preds_df['location'] = location
    preds_df['quantile'] = q_levels
    preds_df = pd.melt(preds_df,
                       id_vars=['forecast_date', 'location', 'quantile'],
                       var_name='horizon')
    preds_df['target_end_date'] = pd.to_datetime(preds_df['forecast_date']).values + \
        pd.to_timedelta(preds_df['horizon'].astype(int), 'days')
    preds_df['base_target'] = base_target
    preds_df['target'] = preds_df['horizon'] + preds_df['base_target']
    preds_df['type'] = 'quantile'
    preds_df = preds_df[['location', 'forecast_date', 'target',
                         'target_end_date', 'type', 'quantile', 'value']]
    return preds_df


def save_forecast_file(state, forecast_date, hosp_pred_qs, case_pred_qs, q_levels, model_name):
    if state == 'ma':
        location = '25'
    elif state == 'ca':
        location = '06'
    else:
        raise ValueError("Only support states ma and ca")
    
    hosp_pred_df = construct_forecast_df(location,
                                         forecast_date,
                                         hosp_pred_qs,
                                         q_levels,
                                         ' day ahead inc hosp')
    if case_pred_qs is None:
        preds_df = hosp_pred_df
    else:
        case_pred_df = construct_forecast_df(location,
                                             forecast_date,
                                             case_pred_qs,
                                             q_levels,
                                             ' day ahead inc case')
        preds_df = pd.concat([hosp_pred_df, case_pred_df], axis=0)
    
    # save predictions
    model_dir = Path("forecasts") / state / model_name
    model_dir.mkdir(mode=0o775, parents=True, exist_ok=True)
    file_path = model_dir / f"{forecast_date}-{model_name}.csv"
    preds_df.to_csv(file_path, index=False)


def save_fit_samples(state, forecast_date, param_samples, pred_samples, model_name):
    model_dir = Path("fit_samples") / state / model_name
    model_dir.mkdir(mode=0o775, parents=True, exist_ok=True)
    file_path = model_dir / f"{forecast_date}-{model_name}.npz"
    np.savez_compressed(file_path,
                        param_samples=param_samples,
                        pred_samples=pred_samples)


def build_model_name(case_type, case_timing, smooth_case, p, d, P, D):
    return f"{case_type}_" + \
        f"{case_timing}_" +\
        f"smooth_case_{smooth_case}_" +\
        "SARIX_" +\
        f"p_{p}_" +\
        f"d_{d}_" +\
        f"P_{P}_" +\
        f"D_{D}"


if __name__ == "__main__":
    # parse arguments
    parser = argparse.ArgumentParser(description="hierarchicalGP")
    parser.add_argument("--state", nargs="?", default='ma', type=str)
    parser.add_argument("--forecast_date", nargs="?",
                        default='2020-12-07', type=str)
    parser.add_argument("--case_type", nargs="?", default='test', type=str)
    parser.add_argument("--case_source", nargs="?", default='jhu-csse', type=str)
    parser.add_argument("--data_timing", nargs="?", default='final', type=str)
    parser.add_argument("--outlier_correction", nargs="?", default='final', type=str)
    parser.add_argument("--transform", nargs="?",
                        default='fourth_rt', type=str)
    args = parser.parse_args()
    state = args.state
    forecast_date = args.forecast_date
    case_type = args.case_type
    case_source = args.case_source
    data_timing = args.data_timing
    outlier_correction = args.outlier_correction
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
        if case_type == 'none':
            smooth_case_options = [False]
        else:
            smooth_case_options = [True, False]
        
        sari_variations = expand_grid({
            'smooth_case': smooth_case_options,
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
                    'smooth_case': [False],
                    'p': [1],
                    'd': [0],
                    'P': [1],
                    'D': [1]
                })
            elif case_type == 'report':
                sari_variations = pd.DataFrame({
                    'smooth_case': [False, True],
                    'p': [2, 1],
                    'd': [0, 0],
                    'P': [1, 1],
                    'D': [1, 1]
                })
            else:
                sari_variations = pd.DataFrame({
                    'smooth_case': [False, True],
                    'p': [2, 2],
                    'd': [0, 0],
                    'P': [1, 0],
                    'D': [1, 1]
                })
        elif state == 'ca':
            if case_type == 'none':
                sari_variations = pd.DataFrame({
                    'smooth_case': [False],
                    'p': [4],
                    'd': [1],
                    'P': [1],
                    'D': [0]
                })
            elif case_type == 'report':
                sari_variations = pd.DataFrame({
                    'smooth_case': [False, True],
                    'p': [3, 3],
                    'd': [1, 1],
                    'P': [1, 0],
                    'D': [0, 0]
                })
            else:
                sari_variations = pd.DataFrame({
                    'smooth_case': [False, True],
                    'p': [3, 0],
                    'd': [1, 1],
                    'P': [1, 1],
                    'D': [0, 0]
                })

    # keep only variations without a model fit file
    model_names = [build_model_name(case_type,
                                    data_timing,
                                    sari_variations.smooth_case.values[i],
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
            as_of = '' # TODO
        else:
            as_of = forecast_date
        
        data = load_data(as_of=as_of, end_day=forecast_date,
                         case_type=case_type, case_source=case_source,
                         state=state, outlier_correction=outlier_correction)
        
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

        # add 7 day rolling mean of cases
        data['case_rm'] = data.rolling(7)[['case']].mean()

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
            smooth_case = sari_variations.smooth_case.values[i]
            p = sari_variations.p.values[i]
            P = sari_variations.P.values[i]
            d = sari_variations.d.values[i]
            D = sari_variations.D.values[i]

            if case_type == 'none':
                modeled_vars = ['hosp']
            elif smooth_case:
                modeled_vars = ['case_rm', 'hosp']
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
                case_type, case_timing, smooth_case, p, d, P, D)
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
