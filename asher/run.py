import numpy as np
import pandas as pd
import re
import requests

import utils

from collections import Counter, defaultdict
from io import StringIO
from plotnine import (
    aes,
    element_text,
    facet_wrap,
    geom_col,
    geom_histogram,
    geom_jitter,
    geom_line,
    geom_point,
    ggplot,
    labs,
    scale_colour_manual,
    scale_x_continuous,
    scale_x_reverse,
    scale_y_continuous,
    scale_y_reverse,
    stat_smooth,
    theme,
)



### LOAD DATA ###
# Load Asher data
screened_sample = pd.read_spss(f"{utils.WORKING_DIRECTORY}/asher_data/Dissertation Kathryn Asher (Weighted Cleaned Sample).sav")  # 3 seconds

# Add geographic data to sample
states = pd.read_csv(f"{utils.WORKING_DIRECTORY}/{utils.STATES_CSV}")
geo_sample = pd.merge(screened_sample, states, how='left', left_on='STATE', right_on='state')

# See how many respondents from each state are in each sample
counter = Counter(geo_sample['STATE'])
n = 3
print(f"Largest state samples: {counter.most_common(n)}")
print(f"Smallest state samples: {counter.most_common()[:-(len(counter))-1:-1][:n]}\n")

# Histograms of agricultural metrics
def show_histogram(metric="population", bins=20, per_capita=False):
    df = states.copy()
    if per_capita:
        df[f"{metric}_per_capita"] = df.apply(lambda df: df[metric] / df.population if df[metric] else 0, axis=1)
        metric = f"{metric}_per_capita"
    trends = (
        ggplot(df, aes(x = metric))
        + geom_histogram(bins=bins)
        + labs(x = "", y = "", title = f"{metric} histogram")
    )
    trends.show()


# Scatter plots of animal count (or log) vs animal inventory
def show_scatter(df, x, y):
    trends = (
        ggplot(df, aes(x = x, y = y))
        + geom_point(alpha = 0.3)
        + stat_smooth(method = "lm", alpha = 0.5)
        + labs(x = x, y = y, title = f"Livestock inventory vs consumption")
    )
    trends.show()

chicken_options = {
    "1 time per DAY (1.000)": 1,
    "1 time per MONTH (0.033)": 0.033,
    "1 time per WEEK (0.142)": 0.142,
    "1-11 times per YEAR or less frequently (0.016)": 0.016,
    "2 or more times per DAY (2.500)": 2.5,
    "2-3 times per MONTH (0.082)": 0.082,
    "2-4 times per WEEK (0.427)": 0.427,
    "5-6 times per WEEK (0.784)": 0.784,
    "Never (0.000)": 0,
}
df = screened_sample.loc[:,['STATE', 'CHICKENDAILY']]
df['chicken_numeric_old'] = df.apply(lambda L: chicken_options.get(L.CHICKENDAILY, np.nan), axis=1)
df['chicken_numeric'] = df.apply(lambda L: float(re.sub(r'.* \(([\d.]+)\)$', r"\1", L.CHICKENDAILY)), axis=1)

national_prevalences = geo_sample.groupby(['PREVALENCES'], observed=True).count()['ID']
print(national_prevalences)

STATE = 'state'
REGION9 = 'region9'
REGION4 = 'region4'
LEVELS = [STATE, REGION9, REGION4]

def geo_levels(df, process_df):
    levels = {}
    for level in LEVELS:
        levels[level] = process_df(df, level)
    return levels


total_by_level = geo_levels(geo_sample, lambda df, level: df.groupby([level], observed=True, as_index=False).count().loc[:,[level, 'ID']])


### Prevalence of diets ###
# There's lots of prevalence data! All 20-somethingK
prevalence_by_level = geo_levels(geo_sample, lambda df, level: df.groupby([level, 'PREVALENCES'], observed=True, as_index=False).count().loc[:,[level, 'PREVALENCES', 'ID']])
for level in LEVELS:
    prevalence_by_level[level] = pd.merge(prevalence_by_level[level], total_by_level[level], how='left', left_on=level, right_on=level)
    prevalence_by_level[level]['proportion'] = np.round(prevalence_by_level[level]['ID_x'] * 100 / prevalence_by_level[level]['ID_y'], decimals=0)

def ranked_prevalence(level, diet):
    return prevalence_by_level[level].loc[prevalence_by_level[level]['PREVALENCES'] == diet,:].sort_values('proportion')

# Non-reducing omni proportion varies from 55% (Hawaii) to 80% (Vermont)
# Region9 varies from 62% to 75%
# Region4 varies from 64% to 72%
ranked_prevalence(STATE, "Non-Reducing Omnivores")
ranked_prevalence(REGION9, "Non-Reducing Omnivores")
ranked_prevalence(REGION4, "Non-Reducing Omnivores")

# Reducer proportion varies from 16% (Wyoming) to 42% (Delaware)
# Region9 is 24% to 35%
# Region4 is 27% to 34%
ranked_prevalence(STATE, "Reducers")
ranked_prevalence(REGION9, "Reducers")
ranked_prevalence(REGION4, "Reducers")

# Ignore vegetarians. At the state level, n < 5 often, and there isn't significant variation at either regional level
#ranked_prevalence(STATE, "Reducers")


### Thermometers ###
thermometer_metrics = {'ovTHERMOMETER_1': 'vegetarian', 'ocTHERMOMETER_1': 'chicken-free', 'orTHERMOMETER_1': 'reducer'}
thermometer_data = geo_sample.loc[:,LEVELS + list(thermometer_metrics.keys())]
thermometer_data.rename(columns=thermometer_metrics, inplace=True)

def ranked_thermometer(level, diet):
    return thermometer_data.loc[:, [level, 'vegetarian', 'chicken-free', 'reducer']].groupby([level], as_index=False).mean().sort_values(diet)

# n is too small for state-level data
#ranked_thermometer(STATE, 'vegetarian')
#ranked_thermometer(STATE, 'chicken-free')
#ranked_thermometer(STATE, 'reducer')

# region9s have some variation, 10-15% depending on the diet
# All regions are the warmest towards vegetarians, most are coldest towards chicken-free
ranked_thermometer(REGION4, 'vegetarian')
ranked_thermometer(REGION4, 'chicken-free')
ranked_thermometer(REGION4, 'reducer')

# All region4s are most positive towards vegetarians, least towards chicken-avoiders, only 2-7% variation
ranked_thermometer(REGION4, 'vegetarian')
ranked_thermometer(REGION4, 'chicken-free')
ranked_thermometer(REGION4, 'reducer')


### SWFL ###
# Interestingly, on SWFL1 and 2, which are negative coded, reducers don't disagree as strongly (though they still disagree)
# So reducers are more likely than omnis to Cconsider food negative elements in their lives
# This is consistent across major regions and looks mostly consistent across minor regions.
# Do meat reducers feel like they're reducing their meat intake involuntarily?
likert = {
    'Strongly agree': 5,
    'Agree': 4,
    'Neither agree nor disagree': 3,
    'Disagree': 2,
    'Strongly disagree': 1,
}
meaters = geo_sample.loc[np.logical_or(geo_sample['PREVALENCES'] == "Non-Reducing Omnivores", geo_sample['PREVALENCES'] == "Reducers"),:].copy()
swfl_means = {}
swfl_counts = {}
for index in range(1, 6):
    key = f"SWFL{index}"
    key2 = f"{key}_numeric"
    meaters[key2] = meaters.apply(lambda df: likert.get(df[key], np.nan), axis=1)
    # motivations['HEALTH']['r'][REGION4]
    swfl_means[key] = geo_levels(meaters, lambda df, level: df.loc[:,[level, 'PREVALENCES', key2]].groupby([level, 'PREVALENCES'], observed=True, as_index=False).mean())
    swfl_counts[key] = geo_levels(meaters, lambda df, level: df.loc[:,[level, 'PREVALENCES', key2]].groupby([level, 'PREVALENCES', key2], observed=True, as_index=False).count())

# SWFL distributions
for metric in [f'SWFL{i}_numeric' for i in range(1, 6)]:
    swfl_plot = (
        ggplot(meaters.loc[np.logical_not(np.isnan(meaters[metric])),:], aes(x = metric))
            + geom_histogram()
            + facet_wrap("PREVALENCES")
            + labs(x = "", y = "", title = metric)
    )
    #swfl_plot.show()


### Age - are reducers just older? ###
# No. This is an ugly histogram, but there isn't an obvious difference in the distributions
age_plot = (
    ggplot(meaters, aes(x = "AGE"))
    + geom_histogram(binwidth = 5)
    + facet_wrap("PREVALENCES")
    + labs(x = "", y = "", title = f"Age histogram")
)
age_plot.show()


### Consumption: daily chicken ###
# Not obvious regional trends here. In general, by major or minor region, reducers eat a tiny bit more chicken than omnis
meaters['chicken_numeric'] = meaters.apply(lambda df: float(re.sub(r'.* \(([\d.]+)\)$', r"\1", df.CHICKENDAILY)), axis=1)
chicken = geo_levels(meaters, lambda df, level: df.loc[:,[level, 'PREVALENCES', 'chicken_numeric']].groupby([level, 'PREVALENCES'], observed=True, as_index=False).mean())
import pdb; pdb.set_trace()

# TODO: look at different species, and at the deltas between omnis and reducers
'''
BEEFDAILY
PORKDAILY
CHICKENDAILY
TURKEYDAILY
FISHDAILY
SHELLFISHDAILY
OTHERMEATSDAILY
MEAT
LANDMEAT
MEATDAILY
LANDMEATDAILY
'''

### Motivations (yes/no) ###
# There just aren't enough people to analyze here. As an example:
#   geo_sample.groupby(['rMOTIVATIONS_ANIMAL', REGION9]).count()
#       6 people in New England
#   geo_sample.groupby(['rMOTIVATIONS_ANIMAL', REGION4]).count()
#       only 44 people in the Northeast
motivations = defaultdict(dict)
yes_no = {"Yes": 1, "No": 0}
motivation_keys = ['ANIMAL', 'COST', 'DISGUST', 'ENVIRO', 'HEALTH', 'JUSTICE', 'RELIGION', 'SOCIAL', 'TASTE', 'TREND']
diets = ['vegetarian', 'chicken-free', 'reducing meat']
for motivation in motivation_keys:
    for diet in diets:
        key = f"{diet[0]}MOTIVATIONS_{motivation}"
        key2 = f"{key}_numeric"
        geo_sample[key2] = geo_sample.apply(lambda df: yes_no.get(df[key], np.nan), axis=1)
        motivations[motivation][diet] = geo_levels(geo_sample, lambda df, level: df.loc[:,[level, key2]].groupby([level], observed=True, as_index=False).mean())

records = []
for motivation in motivation_keys:
    for diet in diets:
        key = f"{diet[0]}MOTIVATIONS_{motivation}_numeric"
        (no, yes) = geo_sample.groupby(key).count()['PREVALENCES']
        prop = yes * 100 / (yes + no)
        records.append({
            'motivation': motivation,
            'diet': diet,
            'proportion': prop,
        })
motivation_plot_data = pd.DataFrame.from_records(records)
palette = {
    'vegetarian v': "#0072B2",
    'no chicken c': "#339933",
    'reducing meat r': "#FF9933",
}
motivation_plot = (
    ggplot(motivation_plot_data, aes(x = "factor(motivation)", y = "proportion", fill = "factor(diet)"))
        + geom_col(position = "dodge2")
        + scale_y_continuous(limits = [0, 100])
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = "", y = "", title = "Motivations")
)
#motivation_plot.show()

### Barriers (1-5) ###
# Same problem:
#   geo_sample.groupby(['cBARRIERS_INCONVENIENCE', REGION4]).count()
#       Only 51 people im the entire Northast
barrier_keys = ["INCONVENIENCE", "FOODSATISFACTION", "SOCIALISSUES", "HEALTH", "COST", "MOTIVATION", "IDENTITY"]
for barrier in barrier_keys:
    for diet in diets:
        for omni in ["o", ""]:
            key = f"{omni}{diet[0]}BARRIERS_{barrier}"
            key2 = f"{key}_numeric"
            geo_sample[key2] = geo_sample.apply(lambda df: likert.get(df[key], np.nan), axis=1)

records = []
for barrier in barrier_keys:
    for diet in diets:
        for omni in ["o", ""]:
            key = f"{omni}{diet[0]}BARRIERS_{barrier}_numeric"
            values = geo_sample.groupby(key).count()['ID']
            for index, value in enumerate(values):
                records.append({
                    'barrier': barrier,
                    'diet': diet + (" (omni)" if omni else ""),
                    'value':  index + 1,
                    'proportion': value * 100 / sum(values),
                })
barrier_plot_data = pd.DataFrame.from_records(records)

barrier_plot = (
    ggplot(barrier_plot_data, aes(x = "factor(diet)", y = "proportion", fill = "value"))
        + geom_col(position = "dodge2")
        + facet_wrap("barrier")
        + scale_y_continuous(limits = [0, 100])
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = "", y = "", title = "Barriers")
)
#barrier_plot.show()
pass
