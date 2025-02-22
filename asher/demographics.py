import numpy as np
import pandas as pd
import re

from plotnine import (
    aes,
    coord_flip,
    geom_point,
    ggplot,
    labs,
    scale_y_continuous,
    theme,
    theme_minimal,
)

from utils import load_asher_data

# Load all data
data = load_asher_data()

# Create samples: overall, flexitarians, unrestricted, analytics samples of flexitarians and unrestricted
samples = {
    'All': data,
    #'Flexitarians': data.loc[data['PREVALENCES'] == "Reducers",:].copy(),
    #'Unrestricted meat eaters': data.loc[data['PREVALENCES'] == "Non-Reducing Omnivores",:].copy(),
}
samples.update({
    'All (regression data)': data.loc[
        np.logical_or(
            np.logical_or(
                np.logical_or(
                    np.logical_or(data[f'orMOTIVATIONS_ANIMAL'] == 'No', data[f'orMOTIVATIONS_ANIMAL'] == 'Yes'),
                    np.logical_or(data[f'ocMOTIVATIONS_ANIMAL'] == 'No', data[f'ocMOTIVATIONS_ANIMAL'] == 'Yes')
                ),
                np.logical_or(
                    np.logical_or(data[f'ovMOTIVATIONS_ANIMAL'] == 'No', data[f'ovMOTIVATIONS_ANIMAL'] == 'Yes'),
                    np.logical_or(data[f'rMOTIVATIONS_ANIMAL'] == 'No', data[f'rMOTIVATIONS_ANIMAL'] == 'Yes')
                )
            ),
            np.logical_or(
                np.logical_or(data[f'cMOTIVATIONS_ANIMAL'] == 'No', data[f'cMOTIVATIONS_ANIMAL'] == 'Yes'),
                np.logical_or(data[f'vMOTIVATIONS_ANIMAL'] == 'No', data[f'vMOTIVATIONS_ANIMAL'] == 'Yes')
            )
        )
    ,:].copy(),
    #'Flexitarians (regression data)': data.loc[np.logical_or(data[f'rMOTIVATIONS_ANIMAL'] == 'No', data[f'rMOTIVATIONS_ANIMAL'] == 'Yes'),:].copy(),
    #'Unrestricted meat eaters (regression data)': data.loc[
    #    np.logical_or(
    #        np.logical_or(
    #            np.logical_or(data[f'orMOTIVATIONS_ANIMAL'] == 'No', data[f'orMOTIVATIONS_ANIMAL'] == 'Yes'),
    #            np.logical_or(data[f'ocMOTIVATIONS_ANIMAL'] == 'No', data[f'ocMOTIVATIONS_ANIMAL'] == 'Yes')
    #        ),
    #        np.logical_or(data[f'ovMOTIVATIONS_ANIMAL'] == 'No', data[f'ovMOTIVATIONS_ANIMAL'] == 'Yes')
    #    )
    #,:].copy(),
})
print("Totals per smaple: ")
print([(name, len(sample)) for name, sample in samples.items()])

by_attr = {
    #'USA': {
        # age by sex, education, race, income, and region
    #},
}
records = []

# Note these are unweighted
def demo_props(df, attr):
    counts = df.groupby(attr, observed=True).count()['ID'].to_dict()
    total = sum(counts.values())

    def _key(key):
        key = re.sub(r'\(.*', '', key)
        key = re.sub(r', .*', '', key)
        return key

    def _value(value):
        #return value
        return round(value * 100 / total, 1)

    return {f'{attr}: {_key(key)}': _value(value) for key, value in counts.items()}

def add_demographics_for_sample(df):
    # TODO: double check & cite these, Asher's table ^ also has US characteristics
    attrs = {
        'SEX': [51.1, 48.9],
        'Age_Group': [9.5, 13.7, 12.6, 13.4, 12.9, 14.9],   # youngest one might be off, not sure if that's percentage of adults or whole population
        'RACE': [13.7, 6.4, 4.6, 75.3],
        'region4': [20.7, 17.2, 38.4, 23.7],
        'INCOME': [38.9, 8.8, 6.4, 6.8, 10.3, 16.1, 12.7],  # https://data.census.gov/table?q=United%20States&t=Income%20(Households,%20Families,%20Individuals)&g=010XX00US
        'EDUCATION': [48.4, 27.9, 8.9, 14.9],
    }
    demographics = {}

    if df is None:
        # US population, hard code based on Asher p129
        current_attr = None
        current_index = 0
        for index, key in enumerate(by_attr['All'].keys()):
            attr = key.split(":")[0]
            if attr != current_attr:
                current_attr = attr
                current_index = 0
            demographics[key] = attrs[current_attr][current_index]
            current_index += 1
        return demographics

    for attr in attrs.keys():
        demographics.update(demo_props(df, attr))
    return demographics

for name, sample in list(samples.items()) + [('US Population', None)]:
    sample_demographics = add_demographics_for_sample(sample)
    by_attr[name] = sample_demographics

    def _attr(attr):
        attr = attr.replace('_', ' ')
        attr = attr.title()
        return attr

    records.extend([
        {
            'Sample': name,
            'demographic': _attr(attr),
            'value': value,
        } for attr, value in sample_demographics.items()
    ])

wide_demographics = pd.DataFrame.from_dict(by_attr)
long_demographics = pd.DataFrame.from_records(records)
print(wide_demographics)

# Visualize!
demographic_plot = (
    ggplot(long_demographics, aes(x="factor(demographic)", y="value", fill="Sample"))
    + geom_point(alpha = 0.5, size=5, shape='^')
    + coord_flip()
    + scale_y_continuous(limits=[0, 100])
    + theme_minimal()
    + theme(legend_position="bottom", legend_title_position=None)
    + labs(x="", y="% of sample", title="Demographic comparison of samples (unweighted)")
)
demographic_plot.show()
#demographic_plot.save(filename="demographic_plot.png", width=12, height=5)
