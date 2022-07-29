# run this script with the repository root as your working directory
# python3 code/plot-clean-data.py

from datetime import date, timedelta

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

from sarixutil.sarixutil import load_data


first_forecast_date = date.fromisoformat("2020-12-07")
last_forecast_date = date.fromisoformat("2022-05-30")
num_forecast_dates = (last_forecast_date - first_forecast_date).days // 7 + 1
forecast_dates = [str(first_forecast_date + i * timedelta(days=7)) \
    for i in range(num_forecast_dates)]

for state in ['ca', 'ma']:
    for case_type in ['report', 'test']:
        if state == 'ca' and case_type == 'report':
            # in CA, report date cases from both jhu-csse and state dph
            case_source_options = ['jhu-csse', 'dph']
        elif state == 'ma' and case_type == 'report':
            # in MA, only source of report date cases is jhu-csse
            case_source_options = ['jhu-csse']
        else:
            # for both states, test date cases come from state dph
            case_source_options = ['dph']
        
        for case_source in case_source_options:
            with PdfPages(f'figures/{case_type}_{case_source}_cases_{state}.pdf') as pdf:
                for forecast_date in forecast_dates:
                    # load data
                    data_uncorrected = load_data(
                        as_of='2022-07-22',
                        end_day=forecast_date,
                        case_type=case_type,
                        state=state,
                        case_source = case_source,
                        outlier_correction = 'none')
                    
                    data_corrected = load_data(
                        as_of='2022-07-22',
                        end_day=forecast_date,
                        case_type=case_type,
                        state=state,
                        case_source = case_source,
                        outlier_correction = 'redist_zero_runs')
                    data_corrected['case_7da'] = data_corrected.case.rolling(7).mean()
                    
                    plt.figure(figsize=(24, 8))
                    plt.plot(data_uncorrected.case.values, label = "orig")
                    plt.plot(data_corrected.case.values, label = "corrected")
                    plt.plot(data_corrected.case_7da.values, label = "corrected, 7da")
                    plt.title(forecast_date)
                    plt.legend()
                    plt.tight_layout()
                    pdf.savefig()
                    plt.close()

