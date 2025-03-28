import numpy as np
import pandas as pd


WORKING_DIRECTORY = "~/Dropbox/2024 Fall/thesis"

AG_CSV = "ag_data/ag_data.csv"
STATES_CSV = "ag_data/states.csv"

BARRIER_KEYS = [f'BARRIERS_{x}' for x in ['COST', 'FOODSATISFACTION', 'HEALTH', 'IDENTITY', 'INCONVENIENCE', 'MOTIVATION', 'SOCIALISSUES']]
MOTIVATION_KEYS = [f'MOTIVATIONS_{x}' for x in ['ANIMAL', 'COST', 'DISGUST', 'ENVIRO', 'HEALTH', 'JUSTICE', 'RELIGION', 'SOCIAL', 'TASTE', 'TREND']]
SPECIES_KEYS = [f"{s}DAILY" for s in ['BEEF', 'PORK', 'CHICKEN', 'TURKEY', 'FISH', 'SHELLFISH', 'OTHERMEATS']]
SWFL_KEYS = [f"SWFL{i}" for i in range(1, 6)]

LIKERT = {
    'Strongly disagree': 1,
    'Disagree': 2,
    'Neither agree nor disagree': 3,
    'Agree': 4,
    'Strongly agree': 5,
}

CONSUMPTION_OPTIONS = {
    "Never (0.000)": 0,
    "1-11 times per YEAR or less frequently (0.016)": 0.016,
    "1 time per MONTH (0.033)": 0.033,
    "2-3 times per MONTH (0.082)": 0.082,
    "1 time per WEEK (0.142)": 0.142,
    "2-4 times per WEEK (0.427)": 0.427,
    "5-6 times per WEEK (0.784)": 0.784,
    "1 time per DAY (1.000)": 1,
    "2 or more times per DAY (2.500)": 2.5,
}

GENERIC_OPTIONS = {
    "Answer 1": 1,
    "Answer 2": 2,
    "Answer 3": 3,
    "Answer 4": 4,
    "Answer 5": 5,
}

WILLINGNESS_OPTIONS = {
    "Not willing": 1,
    "Likely not willing": 2,
    "Unsure": 3,
    "Likely willing": 4,
    "Willing": 5,
}

COMPARISON_OPTIONS = {
    "Much worse": 1,
    "Somewhat worse": 2,
    "About the same": 3,
    "Somewhat better": 4,
    "Much better": 5,
}


def load_asher_data():
    print("Loading Asher data, takes about 3 seconds...")
    screened_sample = pd.read_spss(f"{WORKING_DIRECTORY}/asher_data/Dissertation Kathryn Asher (Weighted Cleaned Sample).sav")

    # Add geographic data to sample
    states = pd.read_csv(f"{WORKING_DIRECTORY}/{STATES_CSV}")
    geo_sample = pd.merge(screened_sample, states, how='left', left_on='STATE', right_on='state')

    return geo_sample


def counts_table(df, label):
    if type(label) != list:
        label = [label]
    return df.loc[:, ['ID'] + label].groupby(label).count()

def counts_dict(df, label):
    return counts_table(df, label).to_dict()['ID']


def convert_categorical_to_numeric(data, labels, options=LIKERT, overwrite=True, negative=False):
    if negative:
        keys = list(options.keys())
        values = list(options.values())
        values.reverse()
        lookup = dict(zip(keys, values))
    else:
        lookup = options

    for label in labels:
        label_numeric = label if overwrite else f"{label}_numeric"
        data[label_numeric] = data.apply(lambda df: lookup.get(df[label], np.nan), axis=1)

    return data


def categorize_daily(df, key="", levels=4):
    if levels == 0:
        return df[key]

    if df[key] > 1:         # most meals (more than once a day)
        return levels - 1
    if df[key] > 0.5:       # most days (more than once every other day)
        return levels - 2
    if df[key] >= 0.142:     # sometimes (once a week or less)
        return levels - 3
    if levels == 5 and df[key] > 0:
        return 1
    return 0                # seldom (once a month or less)

def response_count_for_question(data, key):
    return data.groupby(key, observed=True).count()['ID']
