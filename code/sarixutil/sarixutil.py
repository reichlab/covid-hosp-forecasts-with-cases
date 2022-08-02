import itertools
import covidcast
from datetime import date, timedelta
import pandas as pd
import numpy as np
from pathlib import Path

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


def redist_zero_runs(values, include_leading = False):
    # make a copy of values and ensure data type
    result = values.copy().astype('float64')
    
    # find the number of trailing zeros
    num_trailing_zeros = len(result.values) - np.max(np.nonzero(result.values)) - 1
    
    # adapted from https://stackoverflow.com/questions/1066758/find-length-of-sequences-of-identical-values-in-a-numpy-array-run-length-encodi
    values = result.values
    lead_adj = -1 * include_leading
    n = len(values)
    zero_ind = (values == 0.)
    y = zero_ind[1:] != zero_ind[:-1]   # pairwise unequal
    i = np.append(np.where(y), n - 1)   # must include last element posi
    z = np.diff(np.append(-1, i))       # run lengths
    p = np.cumsum(np.append(0, z))[:-1] # positions
    
    # if the first run is not a run of zeros, drop it; corrections below deal with runs of zeros
    if values[0] != 0.:
        i = i[1:]
        z = z[1:]
        p = p[1:]
    
    # if there are trailing zeros, drop the last run start point; we'll handle this separately below
    if num_trailing_zeros > 0:
        i = i[:-1]
        z = z[:-1]
        p = p[:-1]
    
    # reshape ranges to an array of shape (n, 2) where n is the number of runs
    # of zeros, the first column contains the start index of the run, and the
    # second column is the index of the first non-zero value following the run
    # this line throws an error if last value is a zero
    # this is intentional, because we're not expecting this to occur in our data
    ranges = p.reshape((len(p) // 2, 2))
    
    # for each run, replace zeros and adjacent with the mean for that span
    for j in range(ranges.shape[0]):
        values[(ranges[j, 0] + lead_adj):(ranges[j,1]+1)] = np.mean(values[(ranges[j, 0] + lead_adj):(ranges[j,1]+1)])
    
    # set trailing zeros and negative values to the last nonzero value
    # easiest to do this by working with the pandas series, whose values were updated
    # by reference in the previous steps
    if num_trailing_zeros > 0:
        result[-num_trailing_zeros:] = np.nan
    
    result[result < 0.0] = np.nan
    result = result.fillna(method='ffill')
    
    return result.values


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
        as_of = '2022-07-22'
    
    # load hospitalizations
    if as_of == '2022-07-22':
        hosp_df_path = f'csv-data/{state.upper()}-JHU-reportdate-hospitalizations-2022-07-22.csv'
        hosp_df = pd.read_csv(hosp_df_path)
        hosp_df = hosp_df[["location", "date", "inc"]]
        hosp_df.columns = ["location", "date", "hosp"]
        hosp_df['location'] = state
        hosp_df = hosp_df[(hosp_df.date >= '2020-10-01')
                        & (hosp_df.date <= end_day)]
        hosp_df.date = pd.to_datetime(hosp_df.date)
    else:
        hosp_df = covidcast.signal(data_source="hhs",
                                signal="confirmed_admissions_covid_1d",
                                start_day=date.fromisoformat("2020-10-01"),
                                end_day=date.fromisoformat(end_day),
                                geo_type="state",
                                geo_values=state,
                                as_of=date.fromisoformat(as_of))
        hosp_df = hosp_df[["geo_value", "time_value", "value"]]
        hosp_df.columns = ["location", "date", "hosp"]
    
    # sort by date
    hosp_df = hosp_df.sort_values(by = 'date')
    
    # load cases
    if case_type == 'report':
        # if case_timing == 'final':
        #     case_as_of = '2022-04-29'
        # else:
        #     case_as_of = as_of
        
        if case_source == 'jhu-csse':
            if as_of == '2022-07-22':
                case_df_path = f'csv-data/{state.upper()}-JHU-reportdate-cases-2022-07-22.csv'
                case_df = pd.read_csv(case_df_path)
                case_df = case_df[["location", "date", "inc"]]
                case_df.columns = ["location", "date", "case"]
                case_df['location'] = state
                case_df = case_df[(case_df.date >= '2020-10-01')
                                & (case_df.date <= end_day)]
                case_df.date = pd.to_datetime(case_df.date)
            else:
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
        
        # sort by date
        case_df = case_df.sort_values(by = 'date')
        
        # fix to zero values
        # first, one-off manual adjustment for MA on Saturday, 2022-07-22
        # reported cases were an abnormally low 85, but not 0 as typical for
        # weekend days. we manully relocate those 85 cases to the following
        # Monday so that the saturday value will have a 0 and zero-handling
        # rules will apply.
        if state == 'ma':
            case_df.loc[case_df.date == '2022-01-24', 'case'] += case_df.loc[case_df.date == '2022-01-22', 'case'].values
            case_df.loc[case_df.date == '2022-01-22', 'case'] = 0.0
        
        if outlier_correction == 'interp_zeros':
            zero_inds = np.where(case_df.case == 0)
            case_df['case'].iloc[zero_inds] = np.nan
            case_df['case'] = case_df['case'].interpolate()
        elif outlier_correction == 'interp_zeros_outliers':
            case_df.case = clean_outliers(case_df.case)
        elif outlier_correction == 'redist_zero_runs':
            case_df.case = redist_zero_runs(case_df.case, include_leading=False)
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
        
        # for MA test date cases, latest available data file is off by one
        if state == 'ma' and as_of == '2022-07-22':
            as_of = '2022-07-21'
        
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
        
        # sort by date
        case_df = case_df.sort_values(by = 'date')
    
    # merge
    df = case_df.merge(hosp_df, on=["location", "date"], how="left")
    
    # ensure float data type
    df[['case', 'hosp']] = df[['case', 'hosp']].astype('float64')
    
    # drop missing values; assumed to be trailing (dangerous?)
    # print(df)
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


def build_model_name(case_type, case_source, smooth_case, p, d, P, D):
    return f"{case_type}_" + \
        f"{case_source}_" +\
        f"smooth_case_{smooth_case}_" +\
        "SARIX_" +\
        f"p_{p}_" +\
        f"d_{d}_" +\
        f"P_{P}_" +\
        f"D_{D}"


