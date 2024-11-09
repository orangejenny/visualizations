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
geo_sample = load_asher_data()
geo_sample = convert_categorical_to_numeric(geo_sample, [f"SWFL{i}" for i in range(1, 6)], overwrite=False)



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



# See how many respondents from each state are in each sample
counter = Counter(geo_sample['STATE'])
n = 3
print(f"Largest state samples: {counter.most_common(n)}")
print(f"Smallest state samples: {counter.most_common()[:-(len(counter))-1:-1][:n]}\n")

# Histograms of agricultural metrics
ag_data = pd.read_csv(f"{utils.WORKING_DIRECTORY}/{utils.AG_CSV}")
ag_metrics = set(ag_data.columns) - {REGION4, REGION9, STATE, 'state_upper', 'id', 'Unnamed: 0'}
ag_metrics = [m for m in ag_metrics if not m.startswith("rank_")]
transformed_ag_data = ag_data.copy()
transformed_ag_data['transformation'] = 'identity'
for transformation in [np.log, np.log10]:
    for_transform = ag_data.copy()
    for_transform['transformation'] = str(transformation)
    for metric in ag_metrics:
        for_transform[metric] = transformation(ag_data[metric])
    transformed_ag_data = pd.concat([transformed_ag_data, for_transform])

for metric in ag_metrics:
    for t in ["identity", "<ufunc 'log'>"]:   # , "<ufunc 'log10'>"]:
        trends = (
            ggplot(transformed_ag_data.loc[transformed_ag_data["transformation"] == t,:], aes(x = metric))
            + geom_histogram(bins=20)
            + labs(x = "", y = "", title = f"{metric} histogram, {t}")
            #+ facet_wrap("transformation")
        )
        #trends.show()
'''
    METRIC                  IDENTITY    LOG
    acres                      N         N
    acres_adjusted             N         Y
    broilers                   N         N
    broilers_adjusted          N         N
    cows                       N         Y
    cows_adjusted              N         Y
    employees                  N         Y
    employees_adjusted         N         Y
    farmers                    N         Y
    farmers_adjusted           N         Y
    farms                      N         Y
    farms_adjusted             N         Y
    hogs                       N         N
    hogs_adjusted              N         N
    layers                     N         Y
    layers_adjusted            N         Y
    population

'''

# Scatter plots of animal count (or log) vs animal inventory
def show_scatter(df, x, y, transform=None, filter_low_pop=False):
    df = df.copy()
    if transform:
        df[x] = transform(df[x])
    if filter_low_pop:
        df = pd.merge(df, total_by_level[STATE], how='left', left_on=STATE, right_on=STATE)
        df = df.loc[df['ID'] >= 100,:]
    trends = (
        ggplot(df, aes(x = x, y = y))
        + geom_point(alpha = 0.3)
        + stat_smooth(method = "lm", alpha = 0.5)
        + labs(x = x, y = y, title = "")
    )
    trends.show()

data = convert_categorical_to_numeric(data, SPECIES_KEYS, options=CONSUMPTION_OPTIONS, overwrite=False)
geo_sample['allmeatdaily'] = geo_sample.apply(lambda df: sum([df[f"{k}_numeric"] for k in utils.SPECIES_KEYS]), axis=1)

# Histograms of allmeatdaily, by diet
def _serving_histogram(metric):
    serving_plot = (
        ggplot(geo_sample.loc[np.logical_or(geo_sample['PREVALENCES'] == "Non-Reducing Omnivores", geo_sample['PREVALENCES'] == "Reducers"),:], aes(x = metric))
        + geom_histogram()   #binwidth = 5)
        + facet_wrap("PREVALENCES")
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = metric, y = "")
    )
    serving_plot.show()

_serving_histogram("allmeatdaily")


# Difference between omnis and reducers in daily servings of meat
def _allmeatdaily_diffs(df, level):
    omnis = geo_sample.loc[geo_sample['PREVALENCES'] == "Non-Reducing Omnivores",[level,'allmeatdaily']].groupby(level, observed=True, as_index=False).mean()
    reducers = geo_sample.loc[geo_sample['PREVALENCES'] == "Reducers",[level,'allmeatdaily']].groupby(level, observed=True, as_index=False).mean()
    combined = pd.merge(omnis, reducers, how='left', left_on=level, right_on=level, suffixes=("_omnis", "_reducers"))
    combined['diff'] = combined['allmeatdaily_omnis'] - combined['allmeatdaily_reducers']
    return combined

# Not obvious regional trends here. In general, by major or minor region, reducers eat a tiny bit more chicken than omnis
# At the state level, there's some variation and some states where omnis eat more meat than semis
servings_by_level = geo_levels(geo_sample, lambda df, level: df.loc[:, ['PREVALENCES', level, 'allmeatdaily']].groupby([level, 'PREVALENCES'], observed=True, as_index=False).mean())
diffs_by_level = geo_levels(geo_sample, _allmeatdaily_diffs)
pd.merge(diffs_by_level[STATE], total_by_level[STATE], how='left', left_on=STATE, right_on=STATE).sort_values('diff')

# Compare allmeatdaily with ag metrics: nope, nothing meaningful is jumping out
scatter_data = pd.merge(servings_by_level[STATE], ag_data, how='left', left_on=STATE, right_on=STATE)
#show_scatter(scatter_data, 'employees_adjusted', 'allmeatdaily', transform=np.log)

### Prevalence of diets ###
# There's lots of prevalence data! All 20-somethingK
national_prevalences = geo_sample.groupby(['PREVALENCES'], observed=True).count()['ID']
print(national_prevalences)

prevalence_by_level = geo_levels(geo_sample, lambda df, level: df.groupby([level, 'PREVALENCES'], observed=True, as_index=False).count().loc[:,[level, 'PREVALENCES', 'ID']])
for level in LEVELS:
    prevalence_by_level[level] = pd.merge(prevalence_by_level[level], total_by_level[level], how='left', left_on=level, right_on=level)
    prevalence_by_level[level]['proportion'] = np.round(prevalence_by_level[level]['ID_x'] * 100 / prevalence_by_level[level]['ID_y'], decimals=0)

def ranked_prevalence(level, diet):
    return prevalence_by_level[level].loc[prevalence_by_level[level]['PREVALENCES'] == diet,:].sort_values('proportion')


def _meater_prop(df, level):
    df = df.loc[np.logical_or(df['PREVALENCES'] == "Non-Reducing Omnivores", df['PREVALENCES'] == "Reducers")].copy()
    df = df.groupby([level, 'PREVALENCES'], observed=True, as_index=False).count().loc[:,[level, 'PREVALENCES', 'ID']]
    omnis = df.loc[df['PREVALENCES'] == "Non-Reducing Omnivores",:].copy()
    reducers = df.loc[df['PREVALENCES'] == "Reducers",:].copy()
    both = pd.merge(omnis, reducers, how='left', left_on=level, right_on=level, suffixes=("_omnis", "_reducers"))
    both['reducer_prop'] = both['ID_reducers'] * 100 / (both['ID_reducers'] + both['ID_omnis'])
    return both

meater_prop_by_level = geo_levels(geo_sample, _meater_prop)
# Compare reducer_prop with ag metrics
# Adjusted metrics for employees, farms, and farmers all positively correlated with proportion of reducers
# Adjusted metrics for cows, layers, and hogs (but not broilers) all negatively correlated with proportion of reducers
# Both of these indicate that as the influence of farming increases, the proportion of reducers decreases
scatter_data = pd.merge(meater_prop_by_level[STATE], ag_data, how='left', left_on=STATE, right_on=STATE)
#show_scatter(scatter_data, 'employees_adjusted', 'reducer_prop', transform=np.log, filter_low_pop=False)

# Compare diff with ag_metrics
# As the influence of farming increases, reducers eat more meat as compared to non-reducers
# This is a weaker relationship than the relationship between farming and proportion of meat reducers
scatter_data = pd.merge(diffs_by_level[STATE], ag_data, how='left', left_on=STATE, right_on=STATE)
show_scatter(scatter_data, 'employees_adjusted', 'diff', transform=np.log)

# Non-reducing omni proportion varies from 55% (Hawaii) to 80% (Vermont)
# Region9 varies from 62% to 75%
# Region4 varies from 64% to 72%
ranked_prevalence(STATE, "Non-Reducing Omnivores")
ranked_prevalence(REGION9, "Non-Reducing Omnivores")
ranked_prevalence(REGION4, "Non-Reducing Omnivores")

# Scatter plot of proportion vs difference in states
# No relationship
'''
scatter_data = pd.merge(meater_prop_by_level[STATE], diffs_by_level[STATE], how='left', left_on=STATE, right_on=STATE)
scatter_data = pd.merge(scatter_data, total_by_level[STATE], how='left', left_on=STATE, right_on=STATE)
scatter_data = scatter_data.loc[scatter_data['ID'] > 100,:]
scatter_plot = (
    ggplot(scatter_data, aes(x = 'reducer_prop', y = 'diff', size = 'ID'))
    + geom_point(alpha = 0.3)
    + stat_smooth(method = "lm", alpha = 0.5)
    + labs(x = "Proportion of omnivores who are reducing their meat consumption", y = "Difference in meat servings per day", title = f"Anything meaningful?")
)
scatter_plot.show()
'''

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
# This is also corroborated by reducers being more likely to be motivated by cost (in motivation analysis below)
meaters = geo_sample.loc[np.logical_or(geo_sample['PREVALENCES'] == "Non-Reducing Omnivores", geo_sample['PREVALENCES'] == "Reducers"),:].copy()
swfl_means = {}
swfl_counts = {}
for index in range(1, 6):
    key = f"SWFL{index}"
    key2 = f"{key}_numeric"
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
#age_plot.show()

# Other reducer demographics
meaters.groupby(['SEX', 'PREVALENCES'], observed=True).count()['ID']     # men & women almost identical, which is a little interesting given the veg skews female
meaters.groupby(['RACE', 'PREVALENCES'], observed=True).count()['ID']    # fewer reducers in white population, more in Asian population
meaters.groupby(['EDUCATION', 'PREVALENCES'], observed=True).count()['ID']
meaters.groupby(['INCOME', 'PREVALENCES'], observed=True).count()['ID']


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
regional_records = []
for motivation in motivation_keys:
    for diet in diets:
        key = f"{diet[0]}MOTIVATIONS_{motivation}_numeric"
        (no, yes) = geo_sample.groupby(key).count()['PREVALENCES']
        records.append({
            'motivation': motivation,
            'diet': diet,
            'proportion': yes * 100 / (yes + no),
        })

        regional_counts = geo_sample.groupby([REGION4, key]).count()['ID']
        for region, value in regional_counts.index:
            if value:
                continue
            (no, yes) = regional_counts[region]
            regional_records.append({
                'region': region,
                'motivation': motivation,
                'diet': diet,
                'proportion': yes * 100 / (yes + no),
            })
motivation_plot_data = pd.DataFrame.from_records(records)

motivation_plot = (
    ggplot(motivation_plot_data, aes(x = "factor(motivation)", y = "proportion", fill = "factor(diet)"))
        + geom_col(position = "dodge2")
        + scale_y_continuous(limits = [0, 100])
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = "", y = "", title = "Motivations")
)
#motivation_plot.show()

# There's a fair amount of region4 variation in motivations for meat reducers
# For example, ANIMAL ranges from 31% (Midwets) to 50% (Northeast), and ENVIRO is 31%-55%
# But COST is more consistent, 43%-49%
# Motivations also vary regionally for vegetarisn, though perhaps not quite as much
regional_motivations = pd.DataFrame.from_records(regional_records)
regional_motivations.loc[regional_motivations['diet'] == "reducing meat",:]

### Barriers (1-5) ###
# Same problem:
#   geo_sample.groupby(['cBARRIERS_INCONVENIENCE', REGION4]).count()
#       Only 51 people im the entire Northast
barrier_keys = ["INCONVENIENCE", "FOODSATISFACTION", "SOCIALISSUES", "HEALTH", "COST", "MOTIVATION", "IDENTITY"]
for barrier in barrier_keys:
    for diet in diets:
        for omni in ["o", ""]:
            key = f"{omni}{diet[0]}BARRIERS_{barrier}"
            geo_sample = convert_categorical_to_numeric(geo_sample, [key], overwrite=False)

records = []
regional_means = None
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
            diet_means = geo_sample.loc[:,[REGION4, key]].groupby([REGION4]).mean()
            diet_means.rename({key: 'metric'}, axis=1, inplace=True)
            match = re.match(r'(.*)BARRIERS_(.*)_numeric', key)
            diet_means['diet'] = match.group(1)
            diet_means['barrier'] = match.group(2)
            if regional_means is None:
                regional_means = diet_means
            else:
                regional_means = pd.concat([diet_means, regional_means])

# These don't vary so much by region, like 0.3 or 0.5
# One of the bigger differences is IDENTITY, from 2.6 in the midwest to 3.1 in the northeast
regional_means.loc[regional_means['diet'] == 'r',:]

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
