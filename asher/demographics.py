'''
    This looks at the demographics of the entire sample and diet groups, not at specific classes within flexitarians.
'''
import numpy as np
import pandas as pd
import re

from plotnine import (
    aes,
    coord_flip,
    element_rect,
    geom_point,
    ggplot,
    labs,
    scale_y_continuous,
    theme,
    theme_minimal,
)

from utils import add_race_dummy, load_asher_data, proportions

do_weight = True
flexitarians_only = False
classes_only = True

# Load all data
data = load_asher_data()
class_data = pd.read_csv("reducers_3_classes_with_pred_weighted.csv")

# Recode race and non-Hispanic white and non-white, due to sample size
data = add_race_dummy(data, 'RACEETHNICITY', 'RACEETHNICITY_dummy')
class_data = add_race_dummy(class_data, 'RACEETHNICITY', 'RACEETHNICITY_dummy')

# Create samples: overall, flexitarians, unrestricted, analytics samples of flexitarians and unrestricted
samples = {}
#'Unrestricted meat eaters': data.loc[data['PREVALENCES'] == "Non-Reducing Omnivores",:].copy(),

if flexitarians_only:
    samples['Latent class analysis sample (n=8081)'] = data.loc[data['PREVALENCES'] == "Reducers",:].copy()
    samples['Regression sample (n=286)'] = data.loc[np.logical_or(data[f'rMOTIVATIONS_ANIMAL'] == 'No', data[f'rMOTIVATIONS_ANIMAL'] == 'Yes'),:].copy()
elif classes_only:
    #samples['American adults'] = None
    samples['   All flexitarians (n=8081)'] = class_data
    samples[' Superficial (n=5691)'] = class_data.loc[class_data['pred'] == 0,:]
    samples['  Successful (n=1606)'] = class_data.loc[class_data['pred'] == 1,:]
    samples['Struggling (n=784)'] = class_data.loc[class_data['pred'] == 2,:]
else:
    samples['American adults'] = None
    #samples['Cleaned sample'] = data
    samples['Analytic sample'] = data.loc[
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
    ,:].copy()

#'Unrestricted meat eaters (analytic sample)': data.loc[
#    np.logical_or(
#        np.logical_or(
#            np.logical_or(data[f'orMOTIVATIONS_ANIMAL'] == 'No', data[f'orMOTIVATIONS_ANIMAL'] == 'Yes'),
#            np.logical_or(data[f'ocMOTIVATIONS_ANIMAL'] == 'No', data[f'ocMOTIVATIONS_ANIMAL'] == 'Yes')
#        ),
#        np.logical_or(data[f'ovMOTIVATIONS_ANIMAL'] == 'No', data[f'ovMOTIVATIONS_ANIMAL'] == 'Yes')
#    )
#,:].copy(),
print("Totals per sample: ")
print([(name, len(sample)) for name, sample in samples.items() if sample is not None])

by_attr = { }
records = []

def _race_label(label):
    if 'other' in label.lower():
        label = 'Other'
    else:
        label = re.sub(r'.* ', '', label)
        label = label.title()
    return f'Race: {label}'


def _race_dummy_label(label):
    label = "Non-Hispanic white" if label == 0 else "Other"
    return (' ' * 20) + f'Race: {label}'

def _education_label(label):
    label = label.lower()
    if 'no diploma' in label:
        label = ' No high school diploma'
    elif 'diploma' in label:
        label = '  High school diploma'
    elif 'associate' in label:
        label = '    College degree'
    else:
        label = '   Some college'
    return (' ' * 30) + re.sub(r'^( *)(.*)$', r'\1Education: \2', label)

def _income_label(label):
    label = label.replace('$', '\\$')
    first_num = re.search(r'(\d+),', label).group(1)
    index = [14, 15, 25, 35, 50, 75, 100].index(int(first_num))
    return (' ' * 40) + (' ' * index) + f'Income: {label}'

def add_demographics_for_sample(df, weight=False):
    attrs = {
        #'RACEETHNICITY': _race_label,
        'RACEETHNICITY_dummy': _race_dummy_label,
        'region4': lambda x: (' ' * 50) + f'Region: {x}',
        'INCOME': _income_label,
        'EDUCATION': _education_label,
    }
    demographics = {}

    '''
    if df is None:
        # US population, hard code
        current_attr = None
        current_index = 0
        demographics.update({
            # 2020: https://www.census.gov/popclock/data_tables.php?component=pyramid
            'Female 18 to 24': 5.7,
            'Female 25 to 34': 8.8,
            'Female 35 to 44': 8.3,
            'Female 45 to 54': 8.0,
            'Female 55 to 64': 8.5,
            'Female 65+': 11.6,
            'Male 18 to 24': 6.0,
            'Male 25 to 34': 9.0,
            'Male 35 to 44': 8.4,
            'Male 45 to 54': 8.0,
            'Male 55 to 64': 8.2,
            'Male 65+': 9.5,
            # 2023: https://data.census.gov/table?q=United%20States&t=Income%20(Households,%20Families,%20Individuals)&g=010XX00US
            'Income: $100,000 or over': 38.9,
            'Income: $14,999 or less': 8.8,
            'Income: $15,000 to $24,999': 6.4,
            'Income: $25,000 to $34,999': 6.8,
            'Income: $35,000 to $49,999': 10.3,
            'Income: $50,000 to $74,999': 16.1,
            'Income: $75,000 to $99,999': 12.7,
            # 2023: https://datacenter.aecf.org/data/tables/6539-adult-population-by-race-and-ethnicity
            'Race: White': 61,
            'Race: Hispanic': 18,
            'Race: Black': 12,
            'Race: Asian': 6,
            'Race: Other': 3,
            # 2023: https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-detail.html
            'Region: Midwest': 20,
            'Region: Northeast': 17,
            'Region: South': 39,
            'Region: West': 24,
            # 2021: https://nces.ed.gov/programs/digest/d21/tables/dt21_104.30.asp
            'Education: Less than high school diploma': 10,
            'Education: High school diploma': 28,
            'Education: Some college': 17,
            'Education: College degree': 45,
        })
        return demographics
    '''

    # Age by sex
    if do_weight:
        counts = df.loc[:,['SEX', 'Age_Group', 'Wts']].groupby(['SEX', 'Age_Group'], observed=True).sum()['Wts'].to_dict()
    else:
        counts = df.groupby(['SEX', 'Age_Group'], observed=True).count()['ID'].to_dict()
    total = sum(counts.values())
    demographics.update({
        (' ' * 10 if 'Male' in label else '')
        + (' ' * [18, 25, 35, 45, 55, 65].index(int(re.search(r'(\d+\b)', label[1]).group(1))))
        + ('Men ' if 'Male' in label else 'Women ')
        + label[1]
        : round(value * 100 / total, 1)
        for label, value in counts.items()
    })

    # Other attributes
    for attr, _format in attrs.items():
        demographics.update(proportions(df, attr, _format, weight=do_weight))
    return demographics

for name, sample in samples.items():
    sample_demographics = add_demographics_for_sample(sample)
    by_attr[name] = sample_demographics

    records.extend([
        {
            ' ': name,      # This is the sample, label with " " so the legend isn't titled
            'demographic': attr,
            'value': value,
        } for attr, value in sample_demographics.items()
    ])

wide_demographics = pd.DataFrame.from_dict(by_attr)
long_demographics = pd.DataFrame.from_records(records)
print(wide_demographics)

# Visualize!
weight_label = "Weighted demographic" if do_weight else "Demographic"
group_label = "classes" if classes_only else "samples"
demographic_plot = (
    ggplot(long_demographics, aes(x="factor(demographic)", y="value", shape=" ", fill=" "))
    + geom_point(alpha = 0.5, size=5)
    + coord_flip()
    + scale_y_continuous(limits=[0, 100])
    + theme_minimal()
    + theme(legend_position="bottom", legend_title_position=None,
            panel_background=element_rect(fill="white"),
            plot_background=element_rect(fill="white"))
    + labs(x="", y="% of sample", title=f"{weight_label} comparison of {group_label}")
)
demographic_plot.show()
#demographic_plot.save(filename="demographic_plot.png", width=12, height=5)
