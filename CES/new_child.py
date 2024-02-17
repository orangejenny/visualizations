import numpy as np
import pandas as pd

from pandas import DataFrame
from scipy.stats import chi2_contingency, ttest_ind

CONTINUOUS_PREFIXES = set()
CATEGORICAL_PREFIXES = set()

panel = pd.read_stata("~/Documents/visualizations/midterm/CCES_Panel_Full3waves_VV_V4.dta", convert_categoricals=False)  # n=9500

# Drop most columns
three_years = panel.loc[
    :,
    panel.columns.str.contains('caseid') +
    panel.columns.str.contains('weight') +   # TODO

    # Ideology and partisanship
    panel.columns.str.startswith('ideo5_') + 
    panel.columns.str.contains('^pid3_1[024]', regex=True) +
    panel.columns.str.startswith('pid7_') + 

    # Policy issues: categorical
    panel.columns.str.contains("CC1[024]_320", regex=True) + # gun control (1-3 more strict, less strict, same)
    panel.columns.str.contains("CC1[024]_326", regex=True) + # gay marriage (1/2 no/yes): note issue was very active during this time, with Obergefell in 2015
    panel.columns.str.contains("CC1[024]_328", regex=True) + # budget (1 cut defense, 2 cut domestic, 3 raise taxes)
    panel.columns.str.contains("CC1[024]_329", regex=True) + # budget move to avoid (1 cut defense, 2 cut domestic, 3 raise taxes)
    panel.columns.str.contains("CC1[024]_330B", regex=True) + # SCHIP (1 renew, 2 expire)

    # Policy issues: continuous
    panel.columns.str.contains("CC1[024]_321", regex=True) + # climate change (1-5 real to not real)
    panel.columns.str.contains("CC1[024]_325", regex=True) + # job vs environment (1-5 favor environment to favor jobs)
    panel.columns.str.contains("CC1[024]_327", regex=True) + # affirmative action (1-4 support to oppose)
    panel.columns.str.contains("CC1[024]_415r", regex=True) + # taxes vs spending (examples given are of domestic spending) (0 to 100)
    panel.columns.str.contains("CC1[024]_416r", regex=True) + # raise sales vs income tax (0 to 100)
    
    # Policy issues: additional issues for composites
    panel.columns.str.contains("CC1[024]_330C", regex=True) + # clean energy act (1/2 support/oppose, discard other values)
    panel.columns.str.contains("CC1[024]_330G", regex=True) + # end don't ask don't tell (1/2 support/oppose, discard other values)
    panel.columns.str.contains("CC1[024]_322_[1-7]", regex=True) + # immigration policies (1/2 support/oppose)
    panel.columns.str.contains("CC1[024]_414_[1-7]", regex=True) + # use of military force for various purposes (1/2 yes/no)

    # Parenthood
    panel.columns.str.startswith("gender_") +
    panel.columns.str.startswith("child18_") +
    panel.columns.str.startswith("child18num_") +

    # Demographics for controls/filters
    panel.columns.str.startswith("birthyr_") +
    panel.columns.str.startswith("faminc_") +
    panel.columns.str.startswith("investor_") + # 1/2 yes/no
    panel.columns.str.startswith("newsint_") + # Limit to 1-4, 1 is "high"
    panel.columns.str.startswith("race_") + # Limit to 1-8, categorical
    panel.columns.str.startswith("educ_") + # Limit to 1-6, categorical
    panel.columns.str.startswith("marstat_") + # Limit to 1-6, categorical
    panel.columns.str.startswith("pew_religimp_") # Limit to 1-4, 1 is "very important"
]

# Add cycle and age
three_years = three_years.assign(
    cycle=101214,
    age=lambda x: 2010 - x.birthyr_10,
)

# Recode a few columns to streamline later calculations
for year in (10, 12, 14):
    # Recode guns to be continuous (swapping 2 and 3 so that "no change" is in the middle of "less strict" and "more strict")
    label = f'CC{year}_320'
    three_years.loc[:, label] = np.where(three_years[label] == 2, 3, np.where(three_years[label] == 3, 2, np.where(three_years[label] == 1, 1, np.nan)))

    # Replace NA with 0 for child18num columns - seems this question was skipped if child18_{year} was No
    label = f'child18num_{year}'
    three_years.loc[np.isnan(three_years[label]), label] = 0

    # CC10_414_1-CC10_414_6 are all usage of military for for different reasons: 1 yes, 2 no
    # CC10_414_7 is a "none of the above" for the previous six: 1 yes, 2 no
    three_years[f'CC{year}_414_7'] = np.where(three_years[f'CC{year}_414_7'] == 1, 2, 1)

    # Flip the 2 yes/no immigration questions that are opposite polarity of the other 5
    # For 1,7, 1 is more liberal and 2 is more conservative
    # For 2,3,4,5,6, 1 is more conservative and 2 is more liberal
    three_years[f'CC{year}_322_1'] = np.where(three_years[f'CC{year}_322_1'] == 1, 2, 1)
    if year == 10:  # only asked in 2010
        three_years[f'CC{year}_322_7'] = np.where(three_years[f'CC{year}_322_7'] == 1, 2, np.where(three_years[f'CC{year}_322_7'] == 2, 1, np.nan))

# Consolidate demographics, arbitrarily using later data if there are differences
for demo in ('gender', 'race', 'investor', 'newsint', 'educ', 'marstat', 'pew_religimp'):
    old_labels = [f'{demo}_10', f'{demo}_12', f'{demo}_14']
    three_years[demo] = three_years[old_labels].bfill(axis=1).iloc[:, 0]
    three_years.drop(old_labels, axis=1, inplace=True)

# Income: Start with faminc_14 because the buckets vary by year, and the 2014 buckets are more granular
# Income brackets are approximate, since incomes are given in ranges.
three_years.rename(columns={'faminc_14': 'income'}, inplace=True)
three_years = three_years.assign(
    income_quintile=lambda x:np.select(
        [
            x.income == 1,
            x.income == 2,
            x.income == 3,
            x.income == 4,
            x.income == 5,
            x.income == 6,
            x.income == 7,
            x.income == 8,
            x.income == 9,
            x.income == 10, # note the 10 response could go into either 4th or 5th quintile
            x.income == 11,
            x.income == 12,
            x.income == 13,
            x.income == 14,
            x.income == 15,
            x.income == 16,
        ],
        [1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5],
        default=np.NAN
    ),
    # "High income" is top 20% to match Reeves
    high_income=lambda x: np.where(np.isnan(x.income_quintile), np.NAN, np.where(x.income_quintile == 5, 1, 0)),
    # "Low income" is bottom 40%, to very roughly correspond with SCHIP eligibility
    low_income=lambda x: np.select(
        [
            np.isnan(x.income_quintile),
            x.income_quintile == 1,
            x.income_quintile == 2,
        ],
        [np.NAN, 1, 1],
        default=0
    ),
)

two_years = pd.concat([
    three_years.assign(cycle=1012), # contains all data, but only look at 2010/2012
    three_years.assign(cycle=1214)  # contains all data, but only look at 2012/2014
])


########################
# Functions: utilities #
########################
def filter_na(df, label):
    return df.loc[np.logical_not(np.isnan(df[label])),:].copy()

def count_flippers(df, before_label, after_label, lower_bound, upper_bound):
    valid_rows = df.loc[
        np.logical_and(
            np.logical_and(
                np.greater_equal(df[before_label], lower_bound),
                np.greater_equal(df[after_label], lower_bound),
            ),
            np.logical_and(
                np.less_equal(df[before_label], upper_bound),
                np.less_equal(df[after_label], upper_bound),
            )
        ), [before_label, after_label]
    ]
    flippers = valid_rows.loc[np.not_equal(valid_rows[before_label], valid_rows[after_label]), :]
    return round(len(flippers) * 100 / len(valid_rows), 1)


def t_test(df, independent_label, dependent_label, a_value=0, b_value=1):
    na_filtered = filter_na(df, independent_label)
    group_a = na_filtered.loc[np.equal(na_filtered[dependent_label], a_value), independent_label]
    group_b = na_filtered.loc[np.equal(na_filtered[dependent_label], b_value), independent_label]
    return ttest_ind(group_a, group_b, equal_var=False)


def pvalue_stars(pvalue):
    if pvalue < 0.001:
        return '***'
    if pvalue < 0.01:
        return '**'
    if pvalue < 0.05:
        return '*'
    return ''


def t_tests(df, independent_suffix, dependent_label, a_value=0, b_value=1):
    results = {
        'metric': [],
        'statistic': [],
        'df': [],
        'pvalue': [],
    }
    for label in [f'{p}_{independent_suffix}' for p in CONTINUOUS_PREFIXES]:
        result = t_test(df, label, dependent_label, a_value, b_value)
        results['metric'].append(label)
        results['statistic'].append(result.statistic)
        results['df'].append(result.df)
        results['pvalue'].append(str(round(result.pvalue, 4)) + pvalue_stars(result.pvalue))
    df = DataFrame.from_dict(results)
    df.sort_values('metric', inplace=True)
    return df


def chisq(df, factor1, factor2):
    return chi2_contingency(pd.crosstab(df[factor1], df[factor2]))


def chisqs(df, independent_suffix, dependent_label, a_value=0, b_value=1):
    results = {
        'metric': [],
        'statistic': [],
        'dof': [],
        'pvalue': [],
    }
    for label in [f'{p}_{independent_suffix}' for p in CATEGORICAL_PREFIXES]:
        result = chisq(df, label, dependent_label)
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

'''
# TODO: delete
continuous_persists <- function(data, issue) {
  numbers <- data %>% 
      filter(!is.na(eval(parse(text=paste(c(issue, "_persists"), collapse=""))))) %>% 
      mutate(has_issue = if_else(eval(parse(text=paste(c(issue, "_persists"), collapse=""))) == 0, 0, 1)) %>% 
      group_by(new_child, has_issue) %>% 
      summarise(count = n())
  counts <- as.list(numbers$count)
  return(
    paste("New parents: ", two_percents(counts[[3]], counts[[4]]),
          ";",
          "Others: ", two_percents(counts[[1]], counts[[2]]))
  )
}

categorical_persists <- function(data, issue) {
  numbers <- data %>% 
      filter(!is.na(eval(parse(text=paste(c(issue, "_persists"), collapse=""))))) %>% 
      group_by(new_child, eval(parse(text=paste(c(issue, "_persists"), collapse="")))) %>% 
      summarise(count = n())
  counts <- as.list(numbers$count)
  return(
    paste("New parents: ", two_percents(counts[[3]], counts[[4]]),
          ";",
          "Others: ", two_percents(counts[[1]], counts[[2]]))
  )
}

'''
def count_percentages(df, group_by_label, metric_label):
    counts = df.loc[:,['caseid', group_by_label, metric_label]].groupby([group_by_label, metric_label], as_index=False).count() # roughly pd.crosstab
    totals = filter_na(df, metric_label).loc[:,['caseid', group_by_label]].groupby([group_by_label], as_index=False).count()
    results = counts.merge(totals, on=group_by_label)
    results['percent'] = np.round(np.divide(np.multiply(results['caseid_x'], 100), results['caseid_y']), decimals=1)
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
        f'{prefix}_after': np.where(df.cycle == 1012, df[before_pattern.replace('XX', '12')], df[before_pattern.replace('XX', '14')]),
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
        f'{prefix}_direction': lambda x: np.where(x[f'{prefix}_delta'] > 0, 1, np.where(x[f'{prefix}_delta'] < 0, -1, 0)),
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
        f'{prefix}_persists_abs': lambda x: abs(x[f'{prefix}_persists']),
    }
    df = df.assign(**kwargs)

    CONTINUOUS_PREFIXES.add(prefix)
    if drop:
        _drop_pattern(df, before_pattern)

    return df


def add_categorical(df, before_pattern, prefix, lower_bound=None, upper_bound=None, drop=True):
    df = _add_before_after(df, before_pattern, prefix)

    df[f'{prefix}_change'] = np.where(np.equal(df[f'{prefix}_before'], df[f'{prefix}_after']), 0, 1)
    # distinguish between False and NaN
    for suffix in ('before', 'after'):
        df.loc[np.isnan(df[f'{prefix}_{suffix}']), f'{prefix}_change'] = np.nan

    df[f'{prefix}_persists'] = np.where(np.logical_and(
        df[before_pattern.replace('XX', '10')] != df[before_pattern.replace('XX', '12')], # change in 2010 vs 2012
        df[before_pattern.replace('XX', '12')] == df[before_pattern.replace('XX', '14')]  # kept 2012 value in 2014
    ), 1, 0)
    df.loc[np.logical_not(df.cycle == 101214), f'{prefix}_persists'] = np.nan

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
        df[f'climate_composite_20{year}'] = np.divide(np.add(np.multiply(df[f'CC{year}_321'], 2.5), df[f'CC{year}_330C']), 2)

        # CC10_326 is gay marriage ban: 1 support, 2 oppose
        # CC10_330G is ending don't ask don't tell: 1 support, 2 oppose, others invalid
        df = _nan_out_of_bounds(df, f'CC{year}_330G', 1, 2)
        df[f'gay_composite_20{year}'] = np.divide(np.add(df[f'CC{year}_326'], df[f'CC{year}_330G']), 2)

        # yes/no questions on military force usage
        df[f'military_composite_20{year}'] = np.divide(np.sum(df.loc[:, df.columns.str.startswith(f'CC{year}_414_')], axis=1), 7)

    # CC10_322_1-CC10_322_7 are all yes/no immigration questions, 8 and 9 are "nothing"/"none of the above" which aren't clearly liberal or conservative
    # 2010 asked 1 2 3 4 7, 2012 asked 1 2 3 4 5 6, 2014 asked 1 2 3 4 5 6
    df[f'immigration_composite_2010'] = np.divide(np.add(np.sum(df.loc[:, df.columns.str.contains('CC10_322_[1-4]')], axis=1), df['CC10_322_7']), 5)
    df[f'immigration_composite_2012'] = np.divide(np.sum(df.loc[:, df.columns.str.contains('CC12_322_[1-6]')], axis=1), 6)
    df[f'immigration_composite_2014'] = np.divide(np.sum(df.loc[:, df.columns.str.contains('CC14_322_[1-6]')], axis=1), 6)

    df = add_continuous(df, 'climate_composite_20XX', 'climate_composite')
    df = add_continuous(df, 'gay_composite_20XX', 'gay_composite')
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
assert 0.4108 == round(t_test(two_years, 'ideo_delta', 'new_child').pvalue, 4)
assert 0.6008 == round(t_test(two_years, 'ideo_delta_abs', 'new_child').pvalue, 4)

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
assert 0.6761 == round(t_test(young_adults, 'ideo_delta', 'new_child').pvalue, 4)
assert 0.6028 == round(t_test(young_adults, 'ideo_delta_abs', 'new_child').pvalue, 4)

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
assert 0.4663 == round(t_test(young_adults, 'pid_delta', 'new_child', 0, 1).pvalue, 4)
assert 0.6051 == round(t_test(young_adults, 'pid_delta_abs', 'new_child', 0, 1).pvalue, 4)
assert 0.4348 == round(t_test(two_years, 'pid_delta', 'new_child', 0, 1).pvalue, 4)
assert 0.0747 == round(t_test(two_years, 'pid_delta_abs', 'new_child', 0, 1).pvalue, 4)
 
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
t_tests(two_years, 'after', 'new_child')
assert 0.9979 == round(t_test(two_years, 'climate_composite_after', 'new_child').pvalue, 4)

# Change, incorporating direction: nothing
t_tests(two_years, 'delta', 'new_child')
assert 0.787 == round(t_test(two_years, 'immigration_composite_delta', 'new_child').pvalue, 4)

# Change, absolute value: climate change, gay, guns: climate change & climate composite persist, and oddly so does sales or inc
t_tests(two_years, 'delta_abs', 'new_child')
assert 0.5486 == round(t_test(two_years, 'military_composite_delta', 'new_child').pvalue, 4)

# Persistent change: nothing
t_tests(three_years, 'persists', 'new_child')
# TODO: None of these match
#t.test(aff_action_persists~new_child, data=filter_na(three_years, "aff_action_persists")) # p=0.3376
#t.test(climate_change_persists~new_child, data=filter_na(three_years, "climate_change_persists")) # p=0.1511
#t.test(climate_composite_persists~new_child, data=filter_na(three_years, "climate_composite_persists")) # p=0.4749
#t.test(gay_composite_persists~new_child, data=filter_na(three_years, "gay_composite_persists")) # p=0.8711
#t.test(guns_persists~new_child, data=filter_na(three_years, "guns_persists")) # p=0.5596
#t.test(immigration_composite_persists~new_child, data=filter_na(three_years, "immigration_composite_persists")) # p=0.3216
#t.test(jobs_env_persists~new_child, data=filter_na(three_years, "jobs_env_persists")) # p=0.6261
#t.test(military_composite_persists~new_child, data=filter_na(three_years, "military_composite_persists")) # p=0.2531
#t.test(sales_or_inc_persists~new_child, data=filter_na(three_years, "sales_or_inc_persists")) # p=0.5886
#t.test(tax_or_spend_persists~new_child, data=filter_na(three_years, "tax_or_spend_persists")) # p=0.1892


# Persistent absolute change: climate change, tax/spend, climate composite, gay rights composite
t_tests(three_years, 'persists_abs', 'new_child')
# TODO: none of these match
#t.test(aff_action_persists_abs~new_child, data=filter_na(three_years, "aff_action_persists_abs")) # p=0.8902
#t.test(climate_change_persists_abs~new_child, data=filter_na(three_years, "climate_change_persists_abs")) # p=0.01737*
#t.test(climate_composite_persists_abs~new_child, data=filter_na(three_years, "climate_composite_persists_abs")) # p=0.03602*
#t.test(gay_composite_persists_abs~new_child, data=filter_na(three_years, "gay_composite_persists_abs")) # p=0.02934*
#t.test(guns_persists_abs~new_child, data=filter_na(three_years, "guns_persists_abs")) # p=0.3267
#t.test(immigration_composite_persists_abs~new_child, data=filter_na(three_years, "immigration_composite_persists_abs")) # p=0.4937
#t.test(jobs_env_persists_abs~new_child, data=filter_na(three_years, "jobs_env_persists_abs")) # p=0.5739
#t.test(military_composite_persists_abs~new_child, data=filter_na(three_years, "military_composite_persists_abs")) # p=0.2671
#t.test(sales_or_inc_persists_abs~new_child, data=filter_na(three_years, "sales_or_inc_persists_abs")) # p=0.5647
#t.test(tax_or_spend_persists_abs~new_child, data=filter_na(three_years, "tax_or_spend_persists_abs")) # p=0.03717*

# Switching to firstborn and looking at change: for absolute change, climate change, climate composite and gay rights composite
t_tests(two_years, 'delta', 'firstborn')
assert 0.4092 == round(t_test(two_years, 'jobs_env_delta', 'firstborn').pvalue, 4)
t_tests(two_years, 'delta_abs', 'firstborn')
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
 
'''
TODO
# Non-response rates, continuous & composite issues
non_response = two_years.loc[:, two_years.columns.str.endswith('_delta') + two_years.columns.str.endswith('_change') + two_years.columns.str.contains('new_child')]
for prefixes, suffix in ((CONTINUOUS_PREFIXES, 'delta'), (CATEGORICAL_PREFIXES, 'change')):
    for issue in prefixes:
        non_response.loc[:, f'has_{issue}'] = np.where(np.isnan(non_response[f'{issue}_{suffix}']), 0, 1)
#count_percentages(two_years, 'new_child', 'gay_marriage_before')
two_years %>% mutate(has_climate_change = if_else(is.na(climate_change_delta), 0, 1)) %>% group_by(new_child, has_climate_change) %>% summarise(count = n())
two_years %>% mutate(has_jobs_env = if_else(is.na(jobs_env_delta), 0, 1)) %>% group_by(new_child, has_jobs_env) %>% summarise(count = n())
two_years %>% mutate(has_aff_action = if_else(is.na(aff_action_delta), 0, 1)) %>% group_by(new_child, has_aff_action) %>% summarise(count = n())
two_years %>% mutate(has_guns = if_else(is.na(guns_delta), 0, 1)) %>% group_by(new_child, has_guns) %>% summarise(count = n())
two_years %>% mutate(has_tax_or_spend = if_else(is.na(tax_or_spend_delta), 0, 1)) %>% group_by(new_child, has_tax_or_spend) %>% summarise(count = n())
two_years %>% mutate(has_sales_or_inc = if_else(is.na(sales_or_inc_delta), 0, 1)) %>% group_by(new_child, has_sales_or_inc) %>% summarise(count = n())
two_years %>% mutate(has_climate_composite = if_else(is.na(climate_composite_delta), 0, 1)) %>% group_by(new_child, has_climate_composite) %>% summarise(count = n())
two_years %>% mutate(has_gay_composite = if_else(is.na(gay_composite_delta), 0, 1)) %>% group_by(new_child, has_gay_composite) %>% summarise(count = n())
two_years %>% mutate(has_military_composite = if_else(is.na(military_composite_delta), 0, 1)) %>% group_by(new_child, has_military_composite) %>% summarise(count = n())
two_years %>% mutate(has_immigration_composite = if_else(is.na(immigration_composite_delta), 0, 1)) %>% group_by(new_child, has_immigration_composite) %>% summarise(count = n())

# Non-response rates, categorical issues
two_years %>% group_by(new_child, gay_marriage_change) %>% summarise(count = n())
two_years %>% group_by(new_child, schip_change) %>% summarise(count = n())
two_years %>% group_by(new_child, budget_change) %>% summarise(count = n())
two_years %>% group_by(new_child, budget_avoid_change) %>% summarise(count = n()) # highest NA responses, at 3%
'''

### Testing: categorical variables: both budget questions, but neither persists
assert 0.8664 == round(chisq(two_years, 'new_child', 'ideo_direction').pvalue, 4)
assert 0.3215 == round(chisq(two_years, 'new_child', 'pid_direction').pvalue, 4)
chisqs(two_years, 'after', 'new_child')
chisqs(two_years, 'change', 'new_child')
# TODO: this one doesn't work because the '_persists' columns are NaN
#chisqs(two_years, 'persists', 'new_child')
#run_chisq(three_years, "new_child", "gay_marriage_persists") # p=0.06199
#run_chisq(three_years, "new_child", "schip_persists") # p=0.6948
#run_chisq(three_years, "new_child", "budget_persists") # p=0.32
#run_chisq(three_years, "new_child", "budget_avoid_persists") # p=1

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

'''
TODO
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

'''
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

# Compare new fathers to other men: tax or spend with direction
t_tests(two_years_men, 'delta', 'new_child')
t_tests(two_years_men, 'delta_abs', 'new_child')

# Compare new mothers to other women: absolute climate, gay, guns
t_tests(two_years_women, 'delta', 'new_child')
t_tests(two_years_women, 'delta_abs', 'new_child')

### Describe continuous issues
for prefix in CONTINUOUS_PREFIXES:
    summarize_continuous(two_years_new_parents, "gender", prefix)
    summarize_continuous(two_years, ["new_child", "gender"], prefix)

'''
TODO
t.test(climate_change_persists~gender, data=filter_na(three_years_new_parents, "climate_change_persists")) # p=0.9324
t.test(tax_or_spend_persists~gender, data=filter_na(three_years_new_parents, "tax_or_spend_persists")) # p=0.3928
t.test(jobs_env_persists~gender, data=filter_na(three_years_new_parents, "jobs_env_persists")) # p=0.7585
t.test(aff_action_persists~gender, data=filter_na(three_years_new_parents, "aff_action_persists")) # p=0.6927
t.test(climate_composite_persists~gender, data=filter_na(three_years_new_parents, "climate_composite_persists")) # p=0.7354
t.test(military_composite_persists~gender, data=filter_na(three_years_new_parents, "military_composite_persists")) # p=0.7544

t.test(climate_change_persists_abs~gender, data=filter_na(three_years_new_parents, "climate_change_persists_abs")) # p=0.7848
t.test(tax_or_spend_persists_abs~gender, data=filter_na(three_years_new_parents, "tax_or_spend_persists_abs")) # p=0.426
t.test(jobs_env_persists_abs~gender, data=filter_na(three_years_new_parents, "jobs_env_persists_abs")) # p=0.5003
t.test(aff_action_persists_abs~gender, data=filter_na(three_years_new_parents, "aff_action_persists_abs")) # p=0.1995
t.test(climate_composite_persists_abs~gender, data=filter_na(three_years_new_parents, "climate_composite_persists_abs")) # p=0.7691
t.test(military_composite_persists_abs~gender, data=filter_na(three_years_new_parents, "military_composite_persists_abs")) # p=0.7647
 
t.test(tax_or_spend_persists~new_child, data=filter_na(three_years_men, "tax_or_spend_persists")) # p=0.07183
t.test(tax_or_spend_persists_abs~new_child, data=filter_na(three_years_men, "tax_or_spend_persists_abs")) # p=0.0003767

t.test(climate_change_persists~high_income, data=filter_na(three_years_new_parents, "climate_change_persists")) # p=0.5825
'''

summarize_continuous(two_years_women, "new_child", "climate_change")
summarize_continuous(two_years_women, "new_child", "guns")
summarize_continuous(two_years_women, "new_child", "climate_composite")
summarize_continuous(two_years_women, "new_child", "gay_composite")

'''
TODO
t.test(climate_change_persists~new_child, data=filter_na(three_years_women, "climate_change_persists")) # p=0.2937
t.test(climate_change_persists_abs~new_child, data=filter_na(three_years_women, "climate_change_persists_abs")) # p=0.1275
t.test(guns_persists~new_child, data=filter_na(three_years_women, "guns_persists")) # p=0.7097
t.test(guns_persists_abs~new_child, data=filter_na(three_years_women, "guns_persists_abs")) # p=0.4326
t.test(climate_composite_persists~new_child, data=filter_na(three_years_women, "climate_composite_persists")) # p=0.798
t.test(gay_composite_persists~new_child, data=filter_na(three_years_women, "gay_composite_persists")) # p=0.6172
t.test(military_composite_persists~new_child, data=filter_na(three_years_women, "military_composite_persists")) # p=0.4658
t.test(immigration_composite_persists~new_child, data=filter_na(three_years_women, "immigration_composite_persists")) # p=0.5404
t.test(climate_composite_persists_abs~new_child, data=filter_na(three_years_women, "climate_composite_persists_abs")) # p=0.2395
t.test(gay_composite_persists_abs~new_child, data=filter_na(three_years_women, "gay_composite_persists_abs")) # p=0.01103*
t.test(military_composite_persists_abs~new_child, data=filter_na(three_years_women, "military_composite_persists_abs")) # p=0.4897
t.test(immigration_composite_persists_abs~new_child, data=filter_na(three_years_women, "immigration_composite_persists_abs")) # p=0.6124
'''

### Categorical issues
# Compare new fathers to new mothers: both budget questions: budget persists
assert 0.595 == round(chisq(two_years_new_parents, 'gender', 'ideo_direction').pvalue, 4)
assert 0.1408 == round(chisq(two_years_new_parents, 'gender', 'pid_direction').pvalue, 4)
chisqs(two_years_new_parents, 'change', 'gender')
#TODO: these are wrong becuase persists is bad
#run_chisq(three_years_new_parents, "gender", "budget_persists") # p=0.002303**
#run_chisq(three_years_new_parents, "gender", "budget_avoid_persists") # p=0.516
#run_chisq(three_years_high_income, "new_child", "schip_persists") # p=0.? "Chi-squared approximation may be incorrect"
#run_chisq(three_years_low_income, "new_child", "budget_persists") # p=0.09921

# Comparing new fathers to new mothers on budget_change
count_percentages(two_years_new_parents, "gender", "budget_before")
count_percentages(two_years_new_parents, "gender", "budget_after")
counts = count_percentages(two_years_new_parents, "gender", "budget_change")
assert (counts['caseid_x'].values == [160,  42, 122,  86]).all()
assert (counts['percent'].values == [79.2, 20.8, 58.7, 41.3]).all()

# Compare new fathers to other men: nothing
assert 0.8416 == round(chisq(two_years_men, 'new_child', 'ideo_direction').pvalue, 4)
assert 0.2836 == round(chisq(two_years_men, 'new_child', 'pid_direction').pvalue, 4)
chisqs(two_years_men, 'change', 'new_child')

# Compare new mothers to other women: both budget questions: both persist
assert 0.7833 == round(chisq(two_years_women, 'new_child', 'ideo_direction').pvalue, 4)
assert 0.1103 == round(chisq(two_years_women, 'new_child', 'pid_direction').pvalue, 4)
chisqs(two_years_women, 'change', 'new_child')
# TODO: persists isn't working
#run_chisq(three_years_women, "gender", "budget_persists") # p < 0.00000000000000022***
#run_chisq(three_years_women, "gender", "budget_avoid_persists") # p < 0.00000000000000022***

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
assert 0.7833 == round(chisq(two_years_women, 'new_child', 'ideo_direction').pvalue, 4)
assert 0.1103 == round(chisq(two_years_women, 'new_child', 'pid_direction').pvalue, 4)
chisqs(two_years_women, 'change', 'new_child')

# Ideology & party: nothing
summarize_continuous(filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'ideo')
summarize_continuous(filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'pid')
assert 0.517 == round(chisq(two_years_new_parents, 'high_income', 'ideo_direction').pvalue, 4)
assert 0.5566 == round(chisq(two_years_new_parents, 'high_income', 'pid_direction').pvalue, 4)

# Comparing high-income new parents with other new parents: continuous: climate change
t_tests(two_years_new_parents, 'delta', 'high_income')
t_tests(two_years_new_parents, 'delta_abs', 'high_income')

summarize_continuous(two_years_new_parents, "high_income", "climate_change")
summarize_continuous(two_years_new_parents, "high_income", "climate_composite")

# Comparing high-income new parents with other high-income people: nothing
t_tests(two_years_high_income, 'delta', 'new_child')
t_tests(two_years_high_income, 'delta_abs', 'new_child')

# Comparing low-income new parents with other low-income people: climate change, jobs env, guns
t_tests(two_years_low_income, 'delta', 'new_child')
t_tests(two_years_low_income, 'delta_abs', 'new_child')

for prefix in CONTINUOUS_PREFIXES:
    summarize_continuous(two_years, ["new_child", "high_income"], prefix)

# TODO: persists
#t.test(climate_change_persists~high_income, data=filter_na(three_years_new_parents, "climate_change_persists")) # p=0.5825
#t.test(climate_change_persists_abs~high_income, data=filter_na(three_years_new_parents, "climate_change_persists_abs")) # p=0.5661
#t.test(climate_composite_persists~high_income, data=filter_na(three_years_new_parents, "climate_composite_persists")) # p=0.6485
#t.test(climate_composite_persists_abs~high_income, data=filter_na(three_years_new_parents, "climate_composite_persists_abs")) # p=0.7648

#t.test(climate_change_persists_abs~new_child, data=filter_na(three_years_low_income, "climate_change_persists_abs")) # p=0.05492
#t.test(climate_composite_persists_abs~new_child, data=filter_na(three_years_low_income, "climate_composite_persists_abs")) # p=0.1125
#t.test(jobs_env_persists_abs~new_child, data=filter_na(three_years_low_income, "jobs_env_persists_abs")) # p=0.4577

# Chi square tests within new parents: high_income, low_income: nothing
chisqs(two_years_new_parents, 'change', 'high_income')

# Chi square tests within high income: nothing
chisqs(two_years_high_income, 'change', 'new_child')

# Chi square tests within low income: budget
chisqs(two_years_low_income, 'change', 'new_child')
