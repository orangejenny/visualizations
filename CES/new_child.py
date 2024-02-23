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
two_years = ces.get_paired_waves()


log_info('''
#########################
# Analysis: Exploratory #
#########################''')

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (paired waves)")

# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (all waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (paired waves)")

# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
log_info(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (all waves)")

# Ideology distribution across panel: roughly normal, skewing conservative
log_info(panel.groupby("ideo5_10").count().loc[:,'weight'], "Overall distribution of ideo5_10")

# Party distribution across panel: not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
log_info(panel.groupby("pid7_10").count().loc[:,'weight'],  "Overall distribution of pid7_10")

# Party distribution among parents: still U-shaped, a little more liberal, also looks like more moderates
# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
log_info(two_years.loc[np.equal(two_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'weight'], "Distribution of pid7_10 among new parents")

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

log_info(ces.summarize_all_continuous(two_years, 'new_child'), "Summary of continuous issues, all paired data")
log_info(ces.summarize_all_continuous(two_years, 'firstborn'), "Summary of continuous issues, all paired data, firstborn child versus all others")

log_info('''
#################
# Analysis: Age #
#################''')

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
#log_info(ces.all_t_test_pvalues(young_adults), "T test p values, respondents under 30 years old")   # TODO: RuntimeWarning: invalid value encountered in scalar multiply
log_info(ces.all_chisq_pvalues(young_adults), "Chi square p values, respondents under 30 years old")

log_info(ces.summarize_all_continuous(young_adults, 'new_child'), "Summary of continuous issues, respondents under 30 years old")

log_info('''
####################
# Analysis: Gender #
####################''')
two_years_new_parents = two_years.loc[np.equal(two_years['new_child'], 1),:]
two_years_men = two_years.loc[np.equal(two_years['gender'], 1),:]
two_years_women = two_years.loc[np.equal(two_years['gender'], 2),:]

log_info(ces.all_t_test_pvalues(two_years_men), "T test p values, new fathers versus other men")
log_info(ces.all_chisq_pvalues(two_years_men), "Chi square p values, new fathers versus other men")

log_info(ces.all_t_test_pvalues(two_years_women), "T test p values, new fathers versus other women")
log_info(ces.all_chisq_pvalues(two_years_women), "Chi square p values, new fathers versus other women")

log_info(ces.all_t_test_pvalues(two_years_new_parents, demographic_label='gender', a_value=1, b_value=2), "T test p values, new fathers versus new mothers")
log_info(ces.all_chisq_pvalues(two_years_new_parents, demographic_label='gender'), "Chi square p values, new fathers versus new mothers")

log_info(ces.summarize_all_continuous(two_years, ['new_child', 'gender']), "Summary of continuous issues by new_child and gender")

log_info('''
####################
# Analysis: Income #
####################''')
two_years_bottom_80 = two_years.loc[np.equal(two_years['high_income'], 0),:]  # "not high", not necessarily low
two_years_top_20 = two_years.loc[np.equal(two_years['high_income'], 1),:]

log_info(ces.all_t_test_pvalues(two_years_bottom_80), "T test p values, bottom 80% new parents versus other bottom 80% respondents")
log_info(ces.all_chisq_pvalues(two_years_bottom_80), "Chi square p values, bottom 80% new parents versus other bottom 80% respondents")

log_info(ces.all_t_test_pvalues(two_years_top_20), "T test p values, top 20% new parents versus other top 20% respondents")
log_info(ces.all_chisq_pvalues(two_years_top_20), "Chi square p values, top 20% new parents versus other top 20% respondents")

log_info(ces.all_t_test_pvalues(two_years_new_parents, demographic_label='high_income'), "T test p values, top 20% new parents versus bottom 80% new parents")
log_info(ces.all_chisq_pvalues(two_years_new_parents, demographic_label='high_income'), "Chi square p values, top 20% new parents versus bottom 80% new parents")

log_info(ces.summarize_all_continuous(two_years, ['new_child', 'high_income']), "Summary of continuous issues by new_child and income")


# Counts of liberal/conservative movement, ignoring magnitude
# New parents: 12% more liberal, 11% more conservative
# Non-new-parents: 12% more liberal, 10% more conservative
ces.count_percentages(ces.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo_direction')

# Counts of liberal/conservative movement, ignoring magnitude, for younger adults
# New parents: 20% more liberal, 9% more conservative
# Non-new-parents: 14% more liberal, 10% more conservative
ces.count_percentages(ces.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo_direction')

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

### Ideology/party + continuous issues

# Comparing new fathers to new mothers on budget_change
ces.count_percentages(two_years_new_parents, "gender", "budget_before")
ces.count_percentages(two_years_new_parents, "gender", "budget_after")
ces.count_percentages(two_years_new_parents, "gender", "budget_change")

# Comparing new mothers to other women on budget_change and budget_change_avoid
ces.count_percentages(two_years_women, "new_child", "budget_before")
ces.count_percentages(two_years_women, "new_child", "budget_after")
ces.count_percentages(two_years_women, "new_child", "budget_avoid_before")
ces.count_percentages(two_years_women, "new_child", "budget_avoid_after")

# Exploratory: what does the income distribution look like across the panel?
panel.loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count()

# Exploratory: what does the income distribution look like for new parents?
# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
two_years.loc[np.equal(two_years['new_child'], 1),['income', 'new_child', 'caseid']].groupby('income').count()
two_years.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count()
two_years.loc[:,['high_income', 'caseid']].groupby('high_income').count()
two_years.loc[:,['low_income', 'caseid']].groupby('low_income').count()
