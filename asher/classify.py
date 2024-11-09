import numpy as np
import pandas as pd

from plotnine import *
from stepmix.stepmix import StepMix

from utils import (
    convert_categorical_to_numeric,
    load_asher_data,
    CONSUMPTION_OPTIONS,
    SWFL_KEYS,
    SPECIES_KEYS,
)


data = load_asher_data()
item_counts = pd.DataFrame(data.count())
item_counts.loc[item_counts[0] > 2000,:].index.to_list()    # items in the 26K sample


# Convert SWFL columns to numeric, reverse code as needed, and add an averaged column
data = convert_categorical_to_numeric(data, ['SWFL1', 'SWFL2'], negative=True)
data = convert_categorical_to_numeric(data, ['SWFL3', 'SWFL4', 'SWFL5'])
data['SWFLMEAN'] = data.apply(lambda df: sum([df[k] for k in SWFL_KEYS]) / 5, axis=1)

# Convert consumption columns to numeric, and note MEATDAILY is a summer numeric column
data = convert_categorical_to_numeric(data, SPECIES_KEYS, options=CONSUMPTION_OPTIONS)
#data['meatdaily_numeric'] = data.apply(lambda df: sum([df[k] for k in SPECIES_KEYS]), axis=1)  3 equivalent to MEATDAILY

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

for i in range(1, 6):
    for j in range(1, i):
        trends = (
            ggplot(analytic_semis, aes(x = f'SWFL{i}', y = f'SWFL{j}'))
            + geom_point(alpha = 0.3)
            + stat_smooth(method = "lm", alpha = 0.5)
            + labs(x = f'SWFL{i}', y = f'SWFL{j}', title = "")
        )
        #trends.show()

trends = (
    ggplot(analytic_semis, aes(x = 'MEATDAILY', y = 'SWFLMEAN'))
    + geom_point(alpha = 0.3)
            + stat_smooth(method = "lm", alpha = 0.5)
    + labs(x = 'MEATDAILY', y = 'SWFLMEAN', title = "")
)
trends.show()

subset = analytic_semis.loc[:,['ID', 'SWFLMEAN', 'MEATDAILY']]


'''
How many reducers are there in the whole set? Ought to be more than 300. But I still only have consumption data for them.

I don't have SWFL for the whole set. Some options:
- Do the same plan, but only on the ~300 meat reducers for whom I have SWFL.
- Classify the entire set by consumption plus region.
- Classify the 1800 based on SWFL and diet.
- Classify the 1800 based on motivation, see if that predicts diet.
- Classify the 1800 based on barriers, see if that predicts diet.
- Classify the meat eaters alone, by consumption
'''



num_classes = 3
model = StepMix(n_components=num_classes, measurement="continuous", verbose=1, random_state=23)
model.fit(subset)

pass
