import numpy as np
import pandas as pd
import re


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

EXTENT_OPTIONS = {
    'Moderate extent': 3,
    'Not at all': 1,
    'Great extent': 4,
    'Very great extent': 5,
    'Small extent': 2,
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


# Params: df, column label, formatter for key values
# Returns a dict of formatted keys => percentage of df with that value
def proportions(df, attr, _format=None, weight=False):
    if _format is None:
        _format = lambda x: x

    if weight:
        counts = df.loc[:,[attr, 'Wts']].groupby(attr, observed=True).sum()['Wts'].to_dict()
    else:
        counts = df.groupby(attr, observed=True).count()['ID'].to_dict()
    total = sum(counts.values())

    def _value(value):
        return round(value * 100 / total, 1)

    return {_format(key): _value(value) for key, value in counts.items()}


def response_count_for_question(data, key):
    return data.groupby(key, observed=True).count()['ID']


### Recoding utilities
def recode_by_dict(df, old_label, new_label, values):
    df[new_label] = df.apply(lambda df: values.get(df[old_label], np.nan), axis=1)
    return df


race_values = {
    'Other race/ethnicity (including two or more)': 1,
    'Asian': 1,
    'African American or Black': 1,
    'HIspanic': 1,
    'White': 0,
}
def add_race_dummy(df, label, new_label):
    return recode_by_dict(df, label, new_label, race_values)

income_values = {
    '$14,999 or less': 7500,
    '$15,000 to $24,999': 20_000,
    '$25,000 to $34,999': 30_000,
    '$35,000 to $49,999': 42_500,
    '$50,000 to $74,999': 62_500,
    '$75,000 to $99,999': 87_500,
    '$100,000 or over': 150_000,
}
def add_income_continuous(df):
    return recode_by_dict(df, 'INCOME', 'INCOME_cont', income_values)


### Display utilities
def display_motivation(label):
    label = re.sub(r'^[rov]*', '', label)
    label = label.replace("MOTIVATIONS_", "")
    return {
        'ANIMAL': 'Animal protection',
        'COST': 'Cost',
        'DISGUST': 'Feelings of disgust about meat',
        'ENVIRO': 'Concern for the environment',
        'HEALTH': 'Health',
        'JUSTICE': 'Social justice or world hunger',
        'RELIGION': 'Religious/spiritual beliefs',
        'SOCIAL': 'Social influence',
        'TASTE': 'Taste preferences',
        'TREND': 'Wanting to follow a food trend',
        'INTERNAL': 'Any internal motivation',
        'EXTERNAL': 'Any external motivation',
    }.get(label, "unknown")


def display_barrier(label):
    label = re.sub(r'^[rov]*', '', label)
    label = label.replace("BARRIERS_", "")
    return {
        'COST': 'Costs too much',
        'FOODSATISFACTION': 'Satisfied with food options',
        'HEALTH': 'Good for my health',
        'IDENTITY': 'A reduced-meat diet is part of my identity',
        'INCONVENIENCE': 'Inconvenient',
        'MOTIVATION': 'Difficult to stay motivated',
        'SOCIALISSUES': 'Creates issues in my social life',
    }.get(label, "unknown")


def display_other(label):
    return {
        "PASTVEG": "Has been vegetarian in the past (yes/no)",
        "length_total": "Length of time following a reduced-meat diet",
        "rINTENTIONS": "Intent to continue with reduced-meat diet",
        "rREDUCEFURTHER": "Willingness to further reduce meat in diet",
        "rVEGWILLING": "Willingness to go vegetarian",
        "rTIES": "Strong ties to other flexitarians",
    }.get(label, "unknown")
