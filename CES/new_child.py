import logging
import numpy as np

from argparse import ArgumentParser
from ces import CESPanel
from datetime import datetime
from yougov import YouGovPanel


def log_info(data, description=''):
    data = str(data)
    if description:
        data = "\n" + description + "\n" + data
    logging.info(data + "\n")


logging.basicConfig(filename='new_child.log', filemode='w', encoding='utf-8', level=logging.INFO)
log_info(f"Run {datetime.now()}")

ces = CESPanel()
panel = ces.get_panel()
three_years = ces.get_all_waves()
two_years = ces.get_paired_waves()


log_info('''
#########################
# Analysis: Exploratory #
#########################''')

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (paired waves)")

counts = ces.get_all_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (all waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (paired waves)")

counts = ces.get_all_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (all waves)")

# Ideology distribution across panel: roughly normal, skewing conservative
log_info(panel.groupby("ideo5_10").count().loc[:,'weight'], "Overall distribution of ideo5_10")

# Party distribution across panel: not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
log_info(panel.groupby("pid7_10").count().loc[:,'weight'],  "Overall distribution of pid7_10")

# Party distribution among parents: still U-shaped, a little more liberal, also looks like more moderates
log_info(three_years.loc[np.equal(three_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'weight'], "Distribution of pid7_10 among new parents")

log_info('''
##########################################################################################
# Analysis: Count flippers: How often do people change ideology/party between two waves? #
##########################################################################################''')

def log_flippers(prefix, start_wave, end_wave, lower_bound, upper_bound):
    log_info(f"Percentage of {prefix} changing from 20{start_wave} to 20{end_wave}: "
             + str(ces.count_flippers(f"{prefix}_{start_wave}", f"{prefix}_{end_wave}", lower_bound, upper_bound)))  # 0.8%, too coarse to be useful

log_flippers("pid3", 10, 12, 1, 3)  # 8.9%, fairly coarse

# For pid7, 20-25% each 2 years
log_flippers("pid7", 10, 12, 1, 7)
log_flippers("pid7", 12, 14, 1, 7)
log_flippers("ideo5", 10, 12, 1, 5)
log_flippers("ideo5", 12, 14, 1, 5)
log_flippers("ideo5", 10, 14, 1, 5)

log_info('''
#######################
# Analysis: Attitudes #
#######################''')

log_info(ces.all_t_test_pvalues(ces.paired_waves), "T test p values, all paired data")
log_info(ces.all_chisq_pvalues(ces.paired_waves), "Chi square p values, all paired data")

log_info(ces.all_t_test_pvalues(ces.paired_waves, demographic_label="firstborn"), "T test p values, all paired data, firstborn")
log_info(ces.all_chisq_pvalues(ces.paired_waves, demographic_label="firstborn"), "Chi square p values, all paired data, firstborn")

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
#log_info(ces.all_t_test_pvalues(young_adults), "T test p values, respondents under 30 years old")   # TODO: RuntimeWarning: invalid value encountered in scalar multiply
log_info(ces.all_chisq_pvalues(young_adults), "Chi square p values, respondents under 30 years old")


### Descriptive: ideological change
# Average ideological change over two years: trivially liberal, moreso for non-new-parents
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude
# New parents: 12% more liberal, 11% more conservative
# Non-new-parents: 12% more liberal, 10% more conservative
ces.count_percentages(ces.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo_direction')

# Using firstborn instead of new_child is still trivial, but new parents more conservative
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), 'firstborn', 'ideo')

# Younger adults look about the same as the whole cohort, trivially more liberal
ces.summarize_continuous(ces.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude, for younger adults
# New parents: 20% more liberal, 9% more conservative
# Non-new-parents: 14% more liberal, 10% more conservative
ces.count_percentages(ces.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo_direction')

### Descriptive: party change
# Average party change over two years: bigger than ideology, but still small
# Vaguely interesting that it's a bigger change. Could just be that it's a bigger scale.
ces.summarize_continuous(ces.filter_na(two_years, 'pid_delta'), 'new_child', 'pid')
ces.summarize_continuous(ces.filter_na(two_years, 'pid_delta'), 'firstborn', 'pid')
ces.summarize_continuous(ces.filter_na(young_adults, 'pid_delta'), 'firstborn', 'pid')


# Summary of continuous & composite issues
ces.summarize_continuous(two_years, "new_child", "climate_change")
ces.summarize_continuous(two_years, "new_child", "jobs_env")
ces.summarize_continuous(two_years, "new_child", "aff_action")
ces.summarize_continuous(two_years, "new_child", "tax_or_spend")
ces.summarize_continuous(two_years, "new_child", "sales_or_inc")
ces.summarize_continuous(two_years, "new_child", "climate_composite")
ces.summarize_continuous(two_years, "new_child", "gay_composite")
ces.summarize_continuous(two_years, "new_child", "immigration_composite")

# Non-response rates, continuous & composite issues
# Do non-response rates differ for parents and non-parents?
total = len(two_years)
for prefixes, suffix in ((ces.CONTINUOUS_PREFIXES, 'delta'), (ces.CATEGORICAL_PREFIXES, 'change')):
    for issue in prefixes:
        missing = len(two_years.loc[np.isnan(two_years[f'{issue}_{suffix}']),:])
        #print(f"Non-response for {issue}_{suffix}: {round(missing * 100 / total, 2)}%")

# Descriptive statistics on categorical issues
ces.count_percentages(two_years, 'new_child', 'gay_marriage_before')

ces.count_percentages(two_years, 'new_child', 'schip_before')
ces.count_percentages(two_years, 'new_child', 'schip_after')

ces.count_percentages(two_years, 'new_child', 'budget_before')
ces.count_percentages(two_years, 'new_child', 'budget_after')

ces.count_percentages(two_years, 'new_child', 'budget_avoid_before')

# Persistence: how common is persistent change?
# Of the new parents who changed, how many keep that change?
# New parents often slightly more likely to experience persistent change than others
ces.continuous_persists("climate_change") # 25% vs 18%
ces.continuous_persists("jobs_env") # 23% vs 23%
ces.continuous_persists("aff_action") # 16% vs 16%
ces.continuous_persists("guns") # 16% vs 14%
ces.continuous_persists("tax_or_spend") # 29% vs 35%
ces.continuous_persists("sales_or_inc") # 39% vs 36%
ces.continuous_persists("climate_composite") # 30% vs 21%
ces.continuous_persists("gay_composite") # 17% vs 11%
ces.continuous_persists("military_composite") # 32% vs 30%
ces.continuous_persists("immigration_composite") # 50% vs 48%
 
ces.categorical_persists("gay_marriage") # 10% vs 7%
ces.categorical_persists("schip") # 15% vs 12%
ces.categorical_persists("budget") # 15% vs 12%
ces.categorical_persists("budget_avoid") # 16% vs 16%

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
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'ideo')
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'pid')

### Ideology/party + continuous issues
# Compare new fathers to new mothers: tax vs spend; absolute aff action, jobs env,, climate composite, military composite
ces.t_tests(two_years_new_parents, 'delta', 'gender', a_value=1, b_value=2)
ces.t_tests(two_years_new_parents, 'delta_abs', 'gender', a_value=1, b_value=2)

# Compare new fathers to other men: tax or spend with direction...doesn't persist
ces.t_tests(two_years_men, 'delta')
ces.t_tests(two_years_men, 'delta_abs')
ces.t_test(three_years_men, 'tax_or_spend_persists')
ces.t_test(three_years_men, 'tax_or_spend_persists_abs')

# Compare new mothers to other women: absolute climate, gay, guns
ces.t_tests(two_years_women, 'delta')
ces.t_tests(two_years_women, 'delta_abs')
ces.t_test(three_years_women, 'climate_change_persists_abs')
ces.t_test(three_years_women, 'guns_persists')

### Describe continuous issues
for prefix in ces.CONTINUOUS_PREFIXES:
    ces.summarize_continuous(two_years_new_parents, "gender", prefix)
    ces.summarize_continuous(two_years, ["new_child", "gender"], prefix)

# New parents vs other: persistent abs: sales or inc
ces.t_tests(three_years_new_parents, 'persists', 'gender', a_value=1, b_value=2)
ces.t_tests(three_years_new_parents, 'persists_abs', 'gender', a_value=1, b_value=2)

ces.summarize_continuous(two_years_women, "new_child", "climate_change")
ces.summarize_continuous(two_years_women, "new_child", "guns")
ces.summarize_continuous(two_years_women, "new_child", "climate_composite")
ces.summarize_continuous(two_years_women, "new_child", "gay_composite")

# Mothers vs non-mothers: persists absolute value: gay composite
ces.t_tests(three_years_women, 'persists')
ces.t_tests(three_years_women, 'persists_abs')

### Categorical issues
# Compare new fathers to new mothers: both budget questions: budget persists
ces.chisqs(three_years_new_parents, "persists", "gender")

# Comparing new fathers to new mothers on budget_change
ces.count_percentages(two_years_new_parents, "gender", "budget_before")
ces.count_percentages(two_years_new_parents, "gender", "budget_after")
ces.count_percentages(two_years_new_parents, "gender", "budget_change")

# Compare new fathers to other men: nothing
ces.chisqs(two_years_men, 'change')

# Compare new mothers to other women: both budget questions: budget persists
ces.chisqs(two_years_women, 'change')
ces.chisqs(three_years_women, "persists")

# Comparing new mothers to other women on budget_change and budget_change_avoid
ces.count_percentages(two_years_women, "new_child", "budget_before")
ces.count_percentages(two_years_women, "new_child", "budget_after")
ces.count_percentages(two_years_women, "new_child", "budget_avoid_before")
ces.count_percentages(two_years_women, "new_child", "budget_avoid_after")


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
ces.chisqs(two_years_women, 'change')

# Ideology & party: nothing
ces.summarize_continuous(ces.filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'ideo')
ces.summarize_continuous(ces.filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'pid')

# Comparing high-income new parents with other new parents: continuous: climate change
ces.t_tests(two_years_new_parents, 'delta', 'high_income')
ces.t_tests(two_years_new_parents, 'delta_abs', 'high_income')

ces.summarize_continuous(two_years_new_parents, "high_income", "climate_change")
ces.summarize_continuous(two_years_new_parents, "high_income", "climate_composite")

# Comparing high-income new parents with other high-income people: nothing
ces.t_tests(two_years_high_income, 'delta')
ces.t_tests(two_years_high_income, 'delta_abs')

# Comparing low-income new parents with other low-income people: climate change, jobs env, guns
ces.t_tests(two_years_low_income, 'delta')
ces.t_tests(two_years_low_income, 'delta_abs')

for prefix in ces.CONTINUOUS_PREFIXES:
    ces.summarize_continuous(two_years, ["new_child", "high_income"], prefix)

ces.t_test(three_years_new_parents, "climate_change_persists", "high_income")
ces.t_test(three_years_new_parents, "climate_change_persists_abs", "high_income")
# TODO: T tests for climate_composite return NaN
ces.t_test(three_years_new_parents, "climate_composite_persists", "high_income")
ces.t_test(three_years_new_parents, "climate_composite_persists_abs", "high_income")

# Chi square tests within new parents: high_income, low_income: nothing
ces.chisqs(two_years_new_parents, 'change', 'high_income')

# Chi square tests within high income: nothing
ces.chisqs(two_years_high_income, 'change')

# Chi square tests within low income: budget
ces.chisqs(two_years_low_income, 'change')
