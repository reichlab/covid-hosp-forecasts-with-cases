import sys
sys.path.insert(0, '.')

import argparse

import os
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm

from datetime import date

import covidcast
import itertools

from sarix import sarix

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt



def expand_grid(data_dict):
	"""Create a dataframe from every combination of given values."""
	rows = itertools.product(*data_dict.values())
	return pd.DataFrame.from_records(rows, columns=data_dict.keys())


def load_data(as_of = None, end_day = "2021-07-01", case_type = 'report', case_timing = 'final', impute_zeros = True):
	"""
	Load data for MA cases and hosps from covidcast
	Parameters
	----------
	as_of: string of date in YYYY-MM-DD format. 
		Default to None.
	end_day: string of date in YYYY-MM-DD format. 
		Default to "2021-07-01"
	Returns
	-------
	df: data frame
		It has columns location, date, inc_hosp, population and rate. 
		It is sorted by location and date columns in ascending order.
	"""
	# override as_of = None to use the same as_of as is used for cases
	if as_of is None:
		hosp_as_of = '2022-03-14'
	else:
		hosp_as_of = as_of
	
	# load hospitalizations
	hosp_df = covidcast.signal(data_source="hhs",
							   signal="confirmed_admissions_covid_1d",
							   start_day=date.fromisoformat("2020-10-01"),
							   end_day=date.fromisoformat(end_day),
							   geo_type="state",
							   geo_values="ma",
							   as_of=date.fromisoformat(hosp_as_of))
	hosp_df = hosp_df[["geo_value", "time_value", "value"]]
	hosp_df.columns = ["location", "date", "hosp"]
	
	# load cases
	if case_type == 'report':
		if case_timing == 'final':
			case_as_of = '2022-03-14'
		else:
			case_as_of = as_of
		
		case_df = covidcast.signal(data_source="jhu-csse",
								   signal="confirmed_incidence_num",
								   start_day=date.fromisoformat("2020-10-01"),
								   end_day=date.fromisoformat(end_day),
								   geo_type="state",
								   geo_values="ma",
								   as_of=date.fromisoformat(case_as_of))
		case_df = case_df[["geo_value", "time_value", "value"]]
		case_df.columns = ["location", "date", "case"]
		
		# quick fix to zero values; replace with nan and then interpolate missing
		if impute_zeros:
			zero_inds = np.where(case_df.case == 0)
			case_df['case'].iloc[zero_inds] = np.nan
			case_df.interpolate(inplace=True)
	else:
		csv_files = os.listdir('csv-data')
		as_ofs = [f[21:-4] for f in csv_files]
		if case_timing == 'final':
			case_as_of = '2022-03-14'
		else:
			subset_as_ofs = [ao for ao in as_ofs if ao <= as_of]
			case_as_of = max(subset_as_ofs)
		
		case_df = pd.read_csv('csv-data/MA-DPH-csvdata-covid-' + case_as_of + '.csv')
		case_df['location'] = 'ma'
		case_df = case_df[['location', 'test_date', 'new_positive']]
		case_df.columns = ['location', 'date', 'case']
		
		case_df = case_df[(case_df.date >= '2020-10-01') & (case_df.date <= end_day)]
		case_df.date = pd.to_datetime(case_df.date)
	
	# merge
	df = case_df.merge(hosp_df, on=["location", "date"], how = "left")
	
	# ensure float data type
	df[['case', 'hosp']] = df[['case', 'hosp']].astype('float64')
	
	# drop missing values; assumed to be trailing (dangerous?)
	df = df.dropna()
	
	return df


def build_model_name(case_type, case_timing, smooth_case, p, d, P, D):
	return f"{case_type}_" + \
		f"{case_timing}_" +\
		f"smooth_case_{smooth_case}_" +\
		"SARIX_" +\
		f"p_{p}_" +\
		f"d_{d}_" +\
		f"P_{P}_" +\
		f"D_{D}"



forecast_date = '2021-07-26'
case_type = 'report'
case_timing = 'final'
transform = 'fourth_rt'

# load forecasts
sari_variations = pd.DataFrame({
	'smooth_case': [False, True],
	'p': [2, 2],
	'd': [0, 0],
	'P': [1, 0],
	'D': [1, 1]
})

model_names = [build_model_name(case_type,
																case_timing,
																sari_variations.smooth_case.values[i],
																sari_variations.p.values[i],
																sari_variations.d.values[i],
																sari_variations.P.values[i],
																sari_variations.D.values[i]) \
								for i in range(sari_variations.shape[0])]
file_paths = [
	Path("forecasts") / model_name / f"{forecast_date}-{model_name}.csv" \
		for model_name in model_names]

forecasts = [
	pd.read_csv(file_path) for file_path in file_paths
]


# load modeled data
# should end_day be forecast_date - 1?
data = load_data(as_of=forecast_date, end_day=forecast_date,
				case_type=case_type, case_timing=case_timing)
data_with_zeros = load_data(as_of=forecast_date, end_day=forecast_date,
				case_type=case_type, case_timing=case_timing, impute_zeros=False)


orig_data = data.copy()

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
	data_with_zeros.case[data_with_zeros.case <= 0] = 1.0
	data_with_zeros.case = np.power(data_with_zeros.case, 0.25)
	data_with_zeros.hosp[data_with_zeros.hosp <= 0] = 1.0
	data_with_zeros.hosp = np.power(data_with_zeros.hosp, 0.25)
elif transform == "log":
	data.case[data.case <= 0] = 1.0
	data.case = np.log(data.case)
	data.hosp[data.hosp <= 0] = 1.0
	data.hosp = np.log(data.hosp)

# add 7 day rolling mean of cases
data['case_rm'] = data.rolling(7)[['case']].mean()
data_with_zeros['case_rm'] = data_with_zeros.rolling(7)[['case']].mean()

# plot
fc_to_plot = forecasts[0]
fc_to_plot = fc_to_plot[fc_to_plot['target'].str[-4:] == 'case']
fc_to_plot.target_end_date = pd.to_datetime(fc_to_plot.target_end_date)
fc_to_plot['fourth_rt_value'] = fc_to_plot.value ** 0.25

plt.plot(data.date, data.case, '-', color="black", label="cases, zeros interpolated")
plt.plot(data.date, data.case_rm, '--', label="rolling mean cases, zeros interpolated")
plt.plot(data_with_zeros.date, data_with_zeros.case, '-.', label="cases")
plt.plot(data_with_zeros.date, data_with_zeros.case_rm, ':', label="rolling mean cases")
plt.plot(fc_to_plot[fc_to_plot['quantile'] == 0.5].target_end_date, fc_to_plot[fc_to_plot['quantile'] == 0.5].fourth_rt_value, color = "purple", label = "predictive median")
plt.plot(fc_to_plot[fc_to_plot['quantile'] == 0.025].target_end_date, fc_to_plot[fc_to_plot['quantile'] == 0.025].fourth_rt_value, '--', color = "purple", label = "95% PI")
plt.plot(fc_to_plot[fc_to_plot['quantile'] == 0.975].target_end_date, fc_to_plot[fc_to_plot['quantile'] == 0.975].fourth_rt_value, '--', color = "purple", label = "95% PI")
plt.tight_layout()
plt.legend()
plt.show()

