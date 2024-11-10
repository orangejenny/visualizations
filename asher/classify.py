import numpy as np
import pandas as pd

from functools import partial
from plotnine import *
from stepmix.stepmix import StepMix

from utils import (
    categorize_daily,
    convert_categorical_to_numeric,
    load_asher_data,
    CONSUMPTION_OPTIONS,
    SPECIES_KEYS,
)


# Load data
data = load_asher_data()
item_counts = pd.DataFrame(data.count())
item_counts.loc[item_counts[0] > 2000,:].index.to_list()    # items in the 26K sample

# Convert consumption columns to numeric, and recall MEATDAILY is a summed numeric column
data = convert_categorical_to_numeric(data, SPECIES_KEYS, options=CONSUMPTION_OPTIONS)

# Add species groupings: by class (mammal / bird / fish) and perception (red meat / white meat / 'blue' meat)
data['MAMMALDAILY'] = data.apply(lambda df: sum([df[k] for k in ['BEEFDAILY', 'PORKDAILY']]), axis=1)
data['BIRDDAILY'] = data.apply(lambda df: sum([df[k] for k in ['CHICKENDAILY', 'TURKEYDAILY']]), axis=1)
data['REDDAILY'] = data.apply(lambda df: sum([df[k] for k in ['BEEFDAILY']]), axis=1)
data['WHITEDAILY'] = data.apply(lambda df: sum([df[k] for k in ['PORKDAILY', 'CHICKENDAILY', 'TURKEYDAILY']]), axis=1)
data['BLUEDAILY'] = data.apply(lambda df: sum([df[k] for k in ['FISHDAILY', 'SHELLFISHDAILY']]), axis=1)

# (ggplot(data.loc[data['MEATDAILY'] <= 5,:], aes(x = 'MEATDAILY')) + geom_histogram()).show()  # histogram of meat consumption
# (ggplot(data.loc[np.logical_and(data['MEATDAILY'] <= 3, data['PREVALENCES'] == 'Reducers'),:], aes(x = 'MEATDAILY')) + geom_histogram()).show()  # same, for semis only
# Among omnis, the percentages 0/1/2/3 are 13/26/48/12
# Among semis, the percentages 0/1/2/3 are 19/27/37/16
for key in ['MEATDAILY', 'MAMMALDAILY', 'BIRDDAILY', 'REDDAILY', 'WHITEDAILY', 'BLUEDAILY']:
    data[f'{key}_cat'] = data.apply(partial(categorize_daily, key=key), axis=1)


all_semis = data.loc[data['PREVALENCES'] == 'Reducers',:]






def fit_model(data, num_classes, measurement="continuous", categories=Ellipsis):
    data = data.copy()
    model = StepMix(n_components=num_classes, measurement=measurement, verbose=1, random_state=23)
    model.fit(data)
    data['pred'] = model.predict(data)

    print("How many people are in each class?")
    print(data.groupby('pred').count())

    if categories is not Ellipsis:
        print("How many people are in each possible cell?")
        print(data.groupby(categories).count())
        print("How did each cell get split up?")
        print(data.reset_index().groupby(categories + ['pred']).count())

    return (model, data)

