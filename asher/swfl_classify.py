import numpy as np
import pandas as pd

from functools import partial
from plotnine import *
from stepmix.stepmix import StepMix

from utils import (
    categorize_daily,
    convert_categorical_to_numeric,
    load_asher_data,
    SWFL_KEYS,
)


data = load_asher_data()

# Convert SWFL columns to numeric, reverse code as needed, and add an averaged column
data = convert_categorical_to_numeric(data, ['SWFL1', 'SWFL2'], negative=True)
data = convert_categorical_to_numeric(data, ['SWFL3', 'SWFL4', 'SWFL5'])
data['SWFLMEAN'] = data.apply(lambda df: sum([df[k] for k in SWFL_KEYS]) / 5, axis=1)

# Categorize total meat consumption (MEATDAILY is a summed numeric column)
data[f'MEATDAILY_cat'] = data.apply(partial(categorize_daily, key='MEATDAILY'), axis=1)

# Convert SWFL to categories
for key in SWFL_KEYS:
    swfl_lookup = [np.nan, 0, 0, 1, 2, 2]  # collapse into three categories instead of 5
    data[f"{key}_cat"] = data.apply(lambda df: df[key] if df[key] != df[key] else swfl_lookup[int(df[key])], axis=1)

# Get subset with SWFL data
all_semis = data.loc[data['PREVALENCES'] == 'Reducers',:]
analytic_semis = all_semis.loc[all_semis['SWFL1'] > -1,:]


# Perhaps oddly, the 5 SWFL items are not correlated particularly well, among either the reducers or the whole sample
def swfl_corr(df):
    records = []
    for i in range(1, 6):
        record = []
        for j in range(1, 6):
            record.append(df[f"SWFL{i}"].corr(df[f"SWFL{j}"]))
        records.append(record)
    return pd.DataFrame.from_records(records)

data_swfl = swfl_corr(data.loc[data['SWFL1'] > -1,:])
semis_swfl = swfl_corr(analytic_semis)

# Visualize those correlations
for i in range(1, 6):
    for j in range(1, i):
        trends = (
            ggplot(analytic_semis, aes(x = f'SWFL{i}', y = f'SWFL{j}'))
            + geom_point(alpha = 0.3)
            + stat_smooth(method = "lm", alpha = 0.5)
            + labs(x = f'SWFL{i}', y = f'SWFL{j}', title = "")
        )
        #trends.show()


def fit_and_plot_model(data, num_classes, measurement="continuous", plot=False, categories=Ellipsis):
    data = data.copy()
    model = StepMix(n_components=num_classes, measurement=measurement, verbose=1, random_state=23)
    model.fit(data)
    data['pred'] = model.predict(data)

    print("How many people are in each class?")
    print(data.groupby('pred').count())

    if measurement == "continuous" and plot:
        scatter_data = data.loc[data['MEATDAILY'] <= 5,:]
        trends = (
            ggplot(scatter_data, aes(x = 'MEATDAILY', y = 'SWFLMEAN', fill = 'factor(pred)', shape = 'factor(pred)'))
            + geom_point(alpha = 0.3)
            + scale_y_continuous(limits = [1, 5])
            + labs(x = 'MEATDAILY', y = 'SWFLMEAN', title = "")
        )
        trends.show()
    if categories is not Ellipsis:
        print("How many people are in each possible cell?")
        print(data.groupby(categories).count())
        print("How did each cell get split up?")
        print(data.reset_index().groupby(categories + ['pred']).count())

    return (model, data)


continuous = analytic_semis.loc[:,['SWFLMEAN', 'MEATDAILY']]
fit_and_plot_model(continuous, 2)   # Not good: classification appears to depend almost entirely on consumption, groups are 90%/10%
fit_and_plot_model(continuous, 3)   # Not good: very similar to previous, but with an additional group with only 4 observations
fit_and_plot_model(continuous, 4)   # Not good: still basically just depending on consumption, and one group is a single observation

trimmed_outliers = continuous.loc[data['MEATDAILY'] <= 5,:]
fit_and_plot_model(trimmed_outliers, 2)     # Not good: similar to the 2-class model without outliers trimmed
fit_and_plot_model(trimmed_outliers, 3)     # Not good: similar to 4-class model without outliers trimmed, just depends on consumption
fit_and_plot_model(trimmed_outliers, 4)     # Not good: same general pattern as 4-class model without outliers

categorical2 = analytic_semis.loc[:,['MEATDAILY_cat'] + ["SWFL3_cat", "SWFL4_cat"]]
fit_and_plot_model(categorical2, 2, measurement="categorical", categories=['MEATDAILY_cat', 'SWFL3_cat', 'SWFL4_cat'])
fit_and_plot_model(categorical2, 3, measurement="categorical", categories=['MEATDAILY_cat', 'SWFL3_cat', 'SWFL4_cat'])
fit_and_plot_model(categorical2, 4, measurement="categorical", categories=['MEATDAILY_cat', 'SWFL3_cat', 'SWFL4_cat'])

continuous2 = analytic_semis.loc[:,['MEATDAILY'] + ["SWFL3", "SWFL4"]]
fit_and_plot_model(continuous2, 2)
fit_and_plot_model(continuous2, 3)
fit_and_plot_model(continuous2, 4)
fit_and_plot_model(continuous2, 5)
fit_and_plot_model(continuous2, 6)
fit_and_plot_model(continuous2, 7)
fit_and_plot_model(continuous2, 8)
fit_and_plot_model(continuous2, 9)
fit_and_plot_model(continuous2, 10)

# TODO: mixed model, with continuous consumption but categorical three-level SWFL3 and SWFL4
# TODO pick the best categorical2 model and test its sensitivity
