import numpy as np
import pandas as pd

from pandas import DataFrame
from scipy.stats import chi2_contingency, ttest_ind

from ces import CESData

CONTINUOUS_PREFIXES = set()
CATEGORICAL_PREFIXES = set()

ces_data = CESData()
panel = ces_data.get_panel()
three_years = ces_data.get_all_waves()
two_years = ces_data.get_paired_waves()

########################
# Functions: utilities #
########################
def filter_na(df, label):
    return df.loc[pd.notna(df[label]),:].copy()

def count_flippers(df, before_label, after_label, lower_bound, upper_bound):
    valid_rows = df.loc[
        np.greater_equal(df[before_label], lower_bound) & np.greater_equal(df[after_label], lower_bound)
        &
        np.less_equal(df[before_label], upper_bound) & np.less_equal(df[after_label], upper_bound),
        [before_label, after_label]
    ]
    flippers = valid_rows.loc[np.not_equal(valid_rows[before_label], valid_rows[after_label]), :]
    return round(len(flippers) * 100 / len(valid_rows), 1)


def t_test(df, issue_label, demographic_label='new_child', a_value=0, b_value=1):
    filtered = filter_na(filter_na(df, demographic_label), issue_label)
    group_a = filtered.loc[np.equal(filtered[demographic_label], a_value), issue_label]
    group_b = filtered.loc[np.equal(filtered[demographic_label], b_value), issue_label]
    return ttest_ind(group_a, group_b, equal_var=False)


def pvalue_stars(pvalue):
    if pvalue < 0.001:
        return '***'
    if pvalue < 0.01:
        return '**'
    if pvalue < 0.05:
        return '*'
    return ''


def t_tests(df, issue_suffix, demographic_label='new_child', a_value=0, b_value=1):
    results = {
        'metric': [],
        'statistic': [],
        'df': [],
        'pvalue': [],
    }
    for label in [f'{p}_{issue_suffix}' for p in CONTINUOUS_PREFIXES]:
        result = t_test(df, label, demographic_label, a_value, b_value)
        results['metric'].append(label)
        results['statistic'].append(result.statistic)
        results['df'].append(result.df)
        results['pvalue'].append(str(round(result.pvalue, 4)) + pvalue_stars(result.pvalue))
    df = DataFrame.from_dict(results)
    df.sort_values('metric', inplace=True)
    return df


def chisq(df, factor1, factor2='new_child'):
    return chi2_contingency(pd.crosstab(df[factor1], df[factor2]))


def chisqs(df, issue_suffix, demographic_label='new_child'):
    results = {
        'metric': [],
        'statistic': [],
        'dof': [],
        'pvalue': [],
    }
    for label in [f'{p}_{issue_suffix}' for p in CATEGORICAL_PREFIXES]:
        result = chisq(df, label, demographic_label)
        results['metric'].append(label)
        results['statistic'].append(result.statistic)
        results['dof'].append(result.dof)
        results['pvalue'].append(str(round(result.pvalue, 4)) + pvalue_stars(result.pvalue))
    df = DataFrame.from_dict(results)
    df.sort_values('metric', inplace=True)
    return df


def summarize_continuous(df, group_by_labels, issue):
    if type(group_by_labels) == type(''):
        group_by_labels = [group_by_labels]
    return df.loc[
        :,
        group_by_labels + [f'{issue}_before', f'{issue}_after', f'{issue}_delta', f'{issue}_delta_abs']
    ].groupby(group_by_labels, as_index=False).mean()


def continuous_persists(df, issue):
    flags = filter_na(df, f'{issue}_persists')
    flags[f'{issue}_persistence_flag'] = np.bool_(flags[f'{issue}_persists'])
    flags.groupby(['new_child', f'{issue}_persistence_flag']).count()
    return count_percentages(flags, 'new_child', f'{issue}_persistence_flag')


def categorical_persists(df, issue):
    flags = filter_na(df, f'{issue}_persists')
    flags[f'{issue}_persistence_flag'] = np.bool_(flags[f'{issue}_persists'])
    flags.groupby(['new_child', f'{issue}_persistence_flag']).count()
    return count_percentages(filter_na(df, f'{issue}_persists'), 'new_child', f'{issue}_persists')


def count_percentages(df, group_by_label, metric_label):
    counts = df.loc[:,['caseid', group_by_label, metric_label]].groupby([group_by_label, metric_label], as_index=False).count() # roughly pd.crosstab
    totals = filter_na(df, metric_label).loc[:,['caseid', group_by_label]].groupby([group_by_label], as_index=False).count()
    results = counts.merge(totals, on=group_by_label)
    results['percent'] = np.round(results['caseid_x'] * 100 / results['caseid_y'], decimals=1)
    return results


###################
# Functions: data #
###################
def add_parenting(df):
    df = df.assign(
        new_child=lambda x:np.where(x.cycle == 1214, x.child18num_12 < x.child18num_14, x.child18num_10 < x.child18num_12),
        firstborn=lambda x:np.where(
            x.new_child == False, False,
            np.where(x.cycle == 1214, x.child18num_12 == 0, x.child18num_10 == 0)
        )
    )
    return df.drop([f'child18num_{year}' for year in [10, 12, 14]], axis=1)


def _nan_out_of_bounds(df, label, lower_bound=None, upper_bound=None):
    if lower_bound is None or upper_bound is None:
        return df

    df = df.assign(lower_bound=lower_bound, upper_bound=upper_bound)

    df.loc[np.logical_or(
        np.less(df[label], df.lower_bound),
        np.greater(df[label], df.upper_bound)
    ), label] = np.nan

    df.drop(['lower_bound', 'upper_bound'], axis=1)

    return df


def _add_before_after(df, before_pattern, prefix, lower_bound=None, upper_bound=None):
    kwargs = {
        f'{prefix}_before': np.where(df.cycle == 1214, df[before_pattern.replace('XX', '12')], df[before_pattern.replace('XX', '10')]),
        f'{prefix}_after': np.where(df.cycle == 1214, df[before_pattern.replace('XX', '14')], df[before_pattern.replace('XX', '12')]),
    }
    df = df.assign(**kwargs)
    df = _nan_out_of_bounds(df, f'{prefix}_before', lower_bound, upper_bound)
    df = _nan_out_of_bounds(df, f'{prefix}_after', lower_bound, upper_bound)
    return df


def _drop_pattern(df, pattern):
    for year in (10, 12, 14):
        df.pop(pattern.replace('XX', str(year)))


def add_continuous(df, before_pattern, prefix, lower_bound=None, upper_bound=None, drop=True):
    df = _add_before_after(df, before_pattern, prefix, lower_bound, upper_bound)

    kwargs = {
        f'{prefix}_delta': lambda x: x[f'{prefix}_after'] - x[f'{prefix}_before'],
        f'{prefix}_delta_abs': lambda x: abs(x[f'{prefix}_delta']),
        f'{prefix}_delta_sq': lambda x: x[f'{prefix}_delta'] * x[f'{prefix}_delta'],
        f'{prefix}_direction': lambda x: np.sign(x[f'{prefix}_delta']),
    }
    df = df.assign(**kwargs)
    df.loc[np.isnan(df[f'{prefix}_delta']), f'{prefix}_direction'] = np.nan # because some of the 0s should be NaN

    # Only relevant in three_years
    kwargs = {
        f'{prefix}_persists': lambda x: np.where(
            np.logical_and(
                x[f'{prefix}_delta'] != 0,
                np.logical_not(x[f'{prefix}_delta'] * (x[before_pattern.replace('XX', '14')] - x[before_pattern.replace('XX', '12')]) < 0)
            ),
            x[before_pattern.replace('XX', '14')] - x[before_pattern.replace('XX', '10')], 0
        ),
    }
    df = df.assign(**kwargs)
    df.loc[np.isnan(df[before_pattern.replace('XX', '10')]), f'{prefix}_persists'] = np.nan
    df.loc[np.isnan(df[before_pattern.replace('XX', '12')]), f'{prefix}_persists'] = np.nan
    df.loc[np.isnan(df[before_pattern.replace('XX', '14')]), f'{prefix}_persists'] = np.nan
    df[f'{prefix}_persists_abs'] = np.abs(df[f'{prefix}_persists'])

    CONTINUOUS_PREFIXES.add(prefix)
    if drop:
        _drop_pattern(df, before_pattern)

    return df


def add_categorical(df, before_pattern, prefix, lower_bound=None, upper_bound=None, drop=True):
    df = _add_before_after(df, before_pattern, prefix)

    df[f'{prefix}_change'] = np.where(df[f'{prefix}_before'] == df[f'{prefix}_after'], 0, 1)
    # distinguish between False and NaN
    for suffix in ('before', 'after'):
        df.loc[np.isnan(df[f'{prefix}_{suffix}']), f'{prefix}_change'] = np.nan

    df[f'{prefix}_persists'] = np.where(np.logical_and(
        df[before_pattern.replace('XX', '10')] != df[before_pattern.replace('XX', '12')], # change in 2010 vs 2012
        df[before_pattern.replace('XX', '12')] == df[before_pattern.replace('XX', '14')]  # kept 2012 value in 2014
    ), 1, 0)
    df.loc[np.isnan(df[before_pattern.replace('XX', '10')]), f'{prefix}_persists'] = np.nan
    df.loc[np.isnan(df[before_pattern.replace('XX', '12')]), f'{prefix}_persists'] = np.nan
    df.loc[np.isnan(df[before_pattern.replace('XX', '14')]), f'{prefix}_persists'] = np.nan
    df[f'{prefix}_persists_abs'] = np.abs(df[f'{prefix}_persists'])

    CATEGORICAL_PREFIXES.add(prefix)
    if drop:
        _drop_pattern(df, before_pattern)

    return df


def add_continuous_opinions(df):
    df = add_continuous(df, 'CCXX_321', 'climate_change', 1, 5, drop=False)
    df = add_continuous(df, 'CCXX_325', 'jobs_env', 1, 5)
    df = add_continuous(df, 'CCXX_327', 'aff_action', 1, 4)
    df = add_continuous(df, 'CCXX_320', 'guns', 1, 3)
    df = add_continuous(df, 'CCXX_415r', 'tax_or_spend', 0, 100)
    df = add_continuous(df, 'CCXX_416r', 'sales_or_inc', 0, 100)
    return df


def add_categorical_opinions(df):
    df = add_categorical(df, 'CCXX_326', 'gay_marriage', 1, 2, drop=False)
    df = add_categorical(df, 'CCXX_330B', 'schip', 1, 2)
    df = add_categorical(df, 'CCXX_328', 'budget', 1, 3)
    df = add_categorical(df, 'CCXX_329', 'budget_avoid', 1, 3)
    return df

def add_composite_opinions(df):
    for year in (10, 12, 14):
        # TODO: add in the jobs/environment question to this composite?
        # CC10_321 is climate change: 1-5 with 1 liberal
        # CC10_330C is clean energy act, with 1 support, 2, oppose, and other values invalid
        # Composite is 1-5, with lower values more liberal
        df = _nan_out_of_bounds(df, f'CC{year}_330C', 1, 2)
        df[f'climate_composite_20{year}'] = (df[f'CC{year}_321'] * 2.5 + df[f'CC{year}_330C']) / 2

        # CC10_326 is gay marriage ban: 1 support, 2 oppose
        # CC10_330G is ending don't ask don't tell: 1 support, 2 oppose, others invalid
        df = _nan_out_of_bounds(df, f'CC{year}_330G', 1, 2)
        df[f'gay_composite_20{year}'] = (df[f'CC{year}_326'] + df[f'CC{year}_330G']) / 2

        # yes/no questions on military force usage
        df[f'military_composite_20{year}'] = np.sum(df.loc[:, df.columns.str.startswith(f'CC{year}_414_')], axis=1) / 7

        # Ideology composite that combines ideo and pid
        df[f'ideo_composite_20{year}'] = (df[f'ideo5_{year}'] * 5 + 2.5 * df[f'pid7_{year}']) / 7 / 2  # 5-point composite scale

    # CC10_322_1-CC10_322_7 are all yes/no immigration questions, 8 and 9 are "nothing"/"none of the above" which aren't clearly liberal or conservative
    # 2010 asked 1 2 3 4 7, 2012 asked 1 2 3 4 5 6, 2014 asked 1 2 3 4 5 6
    df[f'immigration_composite_2010'] = (np.sum(df.loc[:, df.columns.str.contains('CC10_322_[1-4]')], axis=1) + df['CC10_322_7']) / 5
    df[f'immigration_composite_2012'] = np.sum(df.loc[:, df.columns.str.contains('CC12_322_[1-6]')], axis=1) / 6
    df[f'immigration_composite_2014'] = np.sum(df.loc[:, df.columns.str.contains('CC14_322_[1-6]')], axis=1) / 6

    df = add_continuous(df, 'climate_composite_20XX', 'climate_composite')
    df = add_continuous(df, 'gay_composite_20XX', 'gay_composite')
    df = add_continuous(df, 'ideo_composite_20XX', 'ideo_composite')
    df = add_continuous(df, 'military_composite_20XX', 'military_composite')
    df = add_continuous(df, 'immigration_composite_20XX', 'immigration_composite')

    return df


##################
# Transform data #
##################
three_years = add_parenting(three_years)
two_years = add_parenting(two_years)

three_years = add_continuous(three_years, 'ideo5_XX', 'ideo', 1, 5, drop=False)
two_years = add_continuous(two_years, 'ideo5_XX', 'ideo', 1, 5, drop=False)

three_years = add_continuous(three_years, 'pid7_XX', 'pid', 1, 7, drop=False)
two_years = add_continuous(two_years, 'pid7_XX', 'pid', 1, 7, drop=False)

three_years = add_continuous_opinions(three_years)
two_years = add_continuous_opinions(two_years)

three_years = add_categorical_opinions(three_years)
two_years = add_categorical_opinions(two_years)

three_years = add_composite_opinions(three_years)
two_years = add_composite_opinions(two_years)

# De-fragment frames
two_years = two_years.copy()
three_years = three_years.copy()

##########################
# Analysis: Demographics #
##########################

# In two cycles: 420 with new child, 18580 without
counts = two_years.groupby('new_child', as_index=False).count()
assert (counts.loc[:, 'caseid'] == [18580, 420]).all()

# In three cycles: 229 with new child in 2012, 9271 without
counts = three_years.groupby('new_child', as_index=False).count()
assert (counts.loc[:, 'caseid'] == [9271, 229]).all()

############################
# Analysis: Ideology/Party #
############################

### Exploratory: How often do people change ideology/party between two waves?
# For pid3, 0.8%, too coarse to be useful
assert 0.8 == count_flippers(three_years, "pid3_10", "pid3_12", 1, 2)

# For pid7, 20-25% each 2 years
assert 20.9 == count_flippers(three_years, "pid7_10", "pid7_12", 1, 7)
assert 18.9 == count_flippers(three_years, "pid7_12", "pid7_14", 1, 7)
assert 25.2 == count_flippers(three_years, "pid7_10", "pid7_14", 1, 7)
assert 24.9 == count_flippers(three_years, "ideo5_10", "ideo5_12", 1, 5)
assert 20.1 == count_flippers(three_years, "ideo5_12", "ideo5_14", 1, 5)
assert 27.8 == count_flippers(three_years, "ideo5_10", "ideo5_14", 1, 5)

### Exploratory: Ideology distribution across panel: roughly normal, skewing conservative
panel.groupby("ideo5_10").count().loc[:,'weight']

### Exploratory: Party distribution across panel
# Not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
panel.groupby("pid7_10").count().loc[:,'weight']
# Parents are still U-shaped, a little more liberal, also looks like more moderates
three_years.loc[np.equal(three_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'weight']

### Testing: ideological change: nothing significant
assert 0.4108 == round(t_test(two_years, 'ideo_delta').pvalue, 4)
assert 0.6008 == round(t_test(two_years, 'ideo_delta_abs').pvalue, 4)
assert 0.7221 == round(t_test(two_years, 'ideo_composite_delta').pvalue, 4)
assert 0.0143 == round(t_test(two_years, 'ideo_composite_delta_abs').pvalue, 4)

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
assert 0.6761 == round(t_test(young_adults, 'ideo_delta').pvalue, 4)
assert 0.6028 == round(t_test(young_adults, 'ideo_delta_abs').pvalue, 4)
assert 0.1845 == round(t_test(young_adults, 'ideo_composite_delta').pvalue, 4)
assert 0.3203 == round(t_test(young_adults, 'ideo_composite_delta_abs').pvalue, 4)

### Descriptive: ideological change
# Average ideological change over two years: trivially liberal, moreso for non-new-parents
summarize_continuous(filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude
# New parents: 12% more liberal, 11% more conservative
# Non-new-parents: 12% more liberal, 10% more conservative
count_percentages(filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo_direction')

# Using firstborn instead of new_child is still trivial, but new parents more conservative
summarize_continuous(filter_na(two_years, 'ideo_delta'), 'firstborn', 'ideo')

# Younger adults look about the same as the whole cohort, trivially more liberal
summarize_continuous(filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude, for younger adults
# New parents: 20% more liberal, 9% more conservative
# Non-new-parents: 14% more liberal, 10% more conservative
count_percentages(filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo_direction')

### Testing: party change: nothing significant
young_adults = two_years.loc[np.less(two_years['age'], 30),:]
assert 0.4663 == round(t_test(young_adults, 'pid_delta').pvalue, 4)
assert 0.6051 == round(t_test(young_adults, 'pid_delta_abs').pvalue, 4)
assert 0.4348 == round(t_test(two_years, 'pid_delta').pvalue, 4)
assert 0.0747 == round(t_test(two_years, 'pid_delta_abs').pvalue, 4)
 
### Descriptive: party change
# Average party change over two years: bigger than ideology, but still small
# Vaguely interesting that it's a bigger change. Could just be that it's a bigger scale.
summarize_continuous(filter_na(two_years, 'pid_delta'), 'new_child', 'pid')
summarize_continuous(filter_na(two_years, 'pid_delta'), 'firstborn', 'pid')
summarize_continuous(filter_na(young_adults, 'pid_delta'), 'firstborn', 'pid')

####################
# Analysis: Issues #
####################

### Testing: continuous & composite issues
# "After" views: nothing
t_tests(two_years, 'after')
assert 0.9979 == round(t_test(two_years, 'climate_composite_after').pvalue, 4)

# Change, incorporating direction: nothing
t_tests(two_years, 'delta')
assert 0.787 == round(t_test(two_years, 'immigration_composite_delta').pvalue, 4)

# Change, absolute value: climate change, gay, guns: climate change & climate composite persist, and oddly so does sales or inc
t_tests(two_years, 'delta_abs')
t_tests(two_years, 'delta_sq')
assert 0.5486 == round(t_test(two_years, 'military_composite_delta').pvalue, 4)

# Persistent change: nothing
t_tests(three_years, 'persists')

# Persistent absolute change: climate change, tax/spend, climate composite, gay rights composite
t_tests(three_years, 'persists_abs')

# Switching to firstborn and looking at change: for absolute change, climate change, climate composite and gay rights composite
t_tests(two_years, 'delta', 'firstborn')
assert 0.4092 == round(t_test(two_years, 'jobs_env_delta', 'firstborn').pvalue, 4)
t_tests(two_years, 'delta_abs', 'firstborn')
t_tests(two_years, 'delta_sq', 'firstborn')
assert 0.1119 == round(t_test(two_years, 'jobs_env_delta_abs', 'firstborn').pvalue, 4)

# Summary of continuous & composite issues
summarize_continuous(two_years, "new_child", "climate_change")
summarize_continuous(two_years, "new_child", "jobs_env")
summarize_continuous(two_years, "new_child", "aff_action")
guns = summarize_continuous(two_years, "new_child", "guns")
assert ([round(v, 2) for v in guns.iloc[1, 1:].values] == [1.78, 1.72, -0.06, 0.29])
summarize_continuous(two_years, "new_child", "tax_or_spend")
summarize_continuous(two_years, "new_child", "sales_or_inc")
summarize_continuous(two_years, "new_child", "climate_composite")
summarize_continuous(two_years, "new_child", "gay_composite")
military = summarize_continuous(two_years, "new_child", "military_composite")
assert ([round(v, 2) for v in military.iloc[1, 1:].values] == [1.46, 1.49, 0.02, 0.16])
summarize_continuous(two_years, "new_child", "immigration_composite")

# Non-response rates, continuous & composite issues
# Do non-response rates differ for parents and non-parents?
total = len(two_years)
for prefixes, suffix in ((CONTINUOUS_PREFIXES, 'delta'), (CATEGORICAL_PREFIXES, 'change')):
    for issue in prefixes:
        missing = len(two_years.loc[np.isnan(two_years[f'{issue}_{suffix}']),:])
        #print(f"Non-response for {issue}_{suffix}: {round(missing * 100 / total, 2)}%")

### Testing: categorical variables: both budget questions, but neither persists
assert 0.8664 == round(chisq(two_years, 'ideo_direction').pvalue, 4)
assert 0.3215 == round(chisq(two_years, 'pid_direction').pvalue, 4)
chisqs(two_years, 'after')
chisqs(two_years, 'change')
chisqs(three_years, 'persists')

# Descriptive statistics on categorical issues
count_percentages(two_years, 'new_child', 'gay_marriage_before')
after_counts = count_percentages(two_years, 'new_child', 'gay_marriage_after')
assert (after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].values == [39.4, 60.6]).all()

count_percentages(two_years, 'new_child', 'schip_before')
count_percentages(two_years, 'new_child', 'schip_after')

count_percentages(two_years, 'new_child', 'budget_before')
count_percentages(two_years, 'new_child', 'budget_after')

count_percentages(two_years, 'new_child', 'budget_avoid_before')
after_counts = count_percentages(two_years, 'new_child', 'budget_avoid_after')
assert (after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].values == [20, 35.4, 44.6]).all()


# Persistence: how common is persistent change?
# Of the new parents who changed, how many keep that change?
# New parents often slightly more likely to experience persistent change than others
continuous_persists(three_years, "climate_change") # 25% vs 18%
continuous_persists(three_years, "jobs_env") # 23% vs 23%
continuous_persists(three_years, "aff_action") # 16% vs 16%
continuous_persists(three_years, "guns") # 16% vs 14%
continuous_persists(three_years, "tax_or_spend") # 29% vs 35%
continuous_persists(three_years, "sales_or_inc") # 39% vs 36%
continuous_persists(three_years, "climate_composite") # 30% vs 21%
continuous_persists(three_years, "gay_composite") # 17% vs 11%
continuous_persists(three_years, "military_composite") # 32% vs 30%
continuous_persists(three_years, "immigration_composite") # 50% vs 48%
 
categorical_persists(three_years, "gay_marriage") # 10% vs 7%
categorical_persists(three_years, "schip") # 15% vs 12%
categorical_persists(three_years, "budget") # 15% vs 12%
categorical_persists(three_years, "budget_avoid") # 16% vs 16%

####################
# Analysis: Gender #
####################
two_years_new_parents = two_years.loc[np.equal(two_years['new_child'], 1),:]
three_years_new_parents = three_years.loc[np.equal(three_years['new_child'], 1),:]
two_years_men = two_years.loc[np.equal(two_years['gender'], 1),:]
three_years_men = three_years.loc[np.equal(three_years['gender'], 1),:]
two_years_women = two_years.loc[np.equal(two_years['gender'], 2),:]
three_years_women = three_years.loc[np.equal(three_years['gender'], 2),:]

### Ideology/party description
summarize_continuous(filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'ideo')
summarize_continuous(filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'pid')

### Ideology/party + continuous issues
# Compare new fathers to new mothers: tax vs spend; absolute aff action, jobs env,, climate composite, military composite
t_tests(two_years_new_parents, 'delta', 'gender', a_value=1, b_value=2)
t_tests(two_years_new_parents, 'delta_abs', 'gender', a_value=1, b_value=2)

# Compare new fathers to other men: tax or spend with direction...doesn't persist
t_tests(two_years_men, 'delta')
t_tests(two_years_men, 'delta_abs')
t_test(three_years_men, 'tax_or_spend_persists')
t_test(three_years_men, 'tax_or_spend_persists_abs')

# Compare new mothers to other women: absolute climate, gay, guns
t_tests(two_years_women, 'delta')
t_tests(two_years_women, 'delta_abs')
t_test(three_years_women, 'climate_change_persists_abs')
t_test(three_years_women, 'guns_persists')

### Describe continuous issues
for prefix in CONTINUOUS_PREFIXES:
    summarize_continuous(two_years_new_parents, "gender", prefix)
    summarize_continuous(two_years, ["new_child", "gender"], prefix)

# New parents vs other: persistent abs: sales or inc
t_tests(three_years_new_parents, 'persists', 'gender', a_value=1, b_value=2)
t_tests(three_years_new_parents, 'persists_abs', 'gender', a_value=1, b_value=2)

summarize_continuous(two_years_women, "new_child", "climate_change")
summarize_continuous(two_years_women, "new_child", "guns")
summarize_continuous(two_years_women, "new_child", "climate_composite")
summarize_continuous(two_years_women, "new_child", "gay_composite")

# Mothers vs non-mothers: persists absolute value: gay composite
t_tests(three_years_women, 'persists')
t_tests(three_years_women, 'persists_abs')

### Categorical issues
# Compare new fathers to new mothers: both budget questions: budget persists
assert 0.595 == round(chisq(two_years_new_parents, 'ideo_direction', 'gender').pvalue, 4)
assert 0.1408 == round(chisq(two_years_new_parents, 'pid_direction', 'gender').pvalue, 4)
chisqs(three_years_new_parents, "persists", "gender")

# Comparing new fathers to new mothers on budget_change
count_percentages(two_years_new_parents, "gender", "budget_before")
count_percentages(two_years_new_parents, "gender", "budget_after")
counts = count_percentages(two_years_new_parents, "gender", "budget_change")
assert (counts['caseid_x'].values == [160,  42, 122,  86]).all()
assert (counts['percent'].values == [79.2, 20.8, 58.7, 41.3]).all()

# Compare new fathers to other men: nothing
assert 0.8416 == round(chisq(two_years_men, 'ideo_direction').pvalue, 4)
assert 0.2836 == round(chisq(two_years_men, 'pid_direction').pvalue, 4)
chisqs(two_years_men, 'change')

# Compare new mothers to other women: both budget questions: budget persists
assert 0.7833 == round(chisq(two_years_women, 'ideo_direction').pvalue, 4)
assert 0.1103 == round(chisq(two_years_women, 'pid_direction').pvalue, 4)
chisqs(two_years_women, 'change')
chisqs(three_years_women, "persists")

# Comparing new mothers to other women on budget_change and budget_change_avoid
count_percentages(two_years_women, "new_child", "budget_before")
count_percentages(two_years_women, "new_child", "budget_after")
count_percentages(two_years_women, "new_child", "budget_avoid_before")
count_percentages(two_years_women, "new_child", "budget_avoid_after")


####################
# Analysis: Income #
####################
# Exploratory: what does the income distribution look like across the panel?
panel.loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count()

# Exploratory: what does the income distribution look like for new parents?
three_years.loc[np.equal(three_years['new_child'], 1),['income', 'new_child', 'caseid']].groupby('income').count()
three_years.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count()
three_years.loc[:,['high_income', 'caseid']].groupby('high_income').count()
three_years.loc[:,['low_income', 'caseid']].groupby('low_income').count()

two_years_high_income = two_years.loc[np.equal(two_years['high_income'], 1),:]
three_years_high_income = three_years.loc[np.equal(three_years['high_income'], 1),:]
two_years_low_income = two_years.loc[np.equal(two_years['high_income'], 0),:]
three_years_low_income = three_years.loc[np.equal(three_years['high_income'], 0),:]
assert 0.7833 == round(chisq(two_years_women, 'ideo_direction').pvalue, 4)
assert 0.1103 == round(chisq(two_years_women, 'pid_direction').pvalue, 4)
chisqs(two_years_women, 'change')

# Ideology & party: nothing
summarize_continuous(filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'ideo')
summarize_continuous(filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'pid')
assert 0.517 == round(chisq(two_years_new_parents, 'ideo_direction', 'high_income').pvalue, 4)
assert 0.5566 == round(chisq(two_years_new_parents, 'pid_direction', 'high_income').pvalue, 4)

# Comparing high-income new parents with other new parents: continuous: climate change
t_tests(two_years_new_parents, 'delta', 'high_income')
t_tests(two_years_new_parents, 'delta_abs', 'high_income')

summarize_continuous(two_years_new_parents, "high_income", "climate_change")
summarize_continuous(two_years_new_parents, "high_income", "climate_composite")

# Comparing high-income new parents with other high-income people: nothing
t_tests(two_years_high_income, 'delta')
t_tests(two_years_high_income, 'delta_abs')

# Comparing low-income new parents with other low-income people: climate change, jobs env, guns
t_tests(two_years_low_income, 'delta')
t_tests(two_years_low_income, 'delta_abs')

for prefix in CONTINUOUS_PREFIXES:
    summarize_continuous(two_years, ["new_child", "high_income"], prefix)

t_test(three_years_new_parents, "climate_change_persists", "high_income")
t_test(three_years_new_parents, "climate_change_persists_abs", "high_income")
# TODO: T tests for climate_composite return NaN
t_test(three_years_new_parents, "climate_composite_persists", "high_income")
t_test(three_years_new_parents, "climate_composite_persists_abs", "high_income")

# Chi square tests within new parents: high_income, low_income: nothing
chisqs(two_years_new_parents, 'change', 'high_income')

# Chi square tests within high income: nothing
chisqs(two_years_high_income, 'change')

# Chi square tests within low income: budget
chisqs(two_years_low_income, 'change')
