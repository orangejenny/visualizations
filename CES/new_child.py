import numpy as np
import os
import pandas as pd

from ces import CESPanel
from datetime import datetime
from yougov import YouGovPanel

OUTPUT_DIR = 'output'
OUTPUT_FILES = ['significant.log', 'all.log', 'two_stars.log', 'three_stars.log']

def truncate_output():
    for filename in OUTPUT_FILES:
        with open(os.path.join(OUTPUT_DIR, filename), 'w') as fh:
            fh.write(f"Run started {datetime.now()}\n")

def _output(filename, data, description=''):
    if type(data) == pd.DataFrame:
        data = data.to_string(max_rows=100)
    else:
        data = str(data)
    if description:
        data = "\n" + description + "\n" + data
    with open(os.path.join(OUTPUT_DIR, filename), 'a') as fh:
        fh.write(data + "\n")

def log_verbose(data, description=''):
    _output('all.log', data, description)

def log_header(header):
    print(header)
    for filename in OUTPUT_FILES:
        _output(filename, header)

def log_findings(data, description=''):
    _output('all.log', data, description)
    _output('significant.log', _limit_to_significant(data), description)
    _output('two_stars.log', _limit_to_significant(data, level=2), description)
    _output('three_stars.log', _limit_to_significant(data, level=3), description)

def _limit_to_significant(data, level=1):
    key = '*' * level
    if 'pvalue' in data:
        data = data.astype({'pvalue': pd.StringDtype()})
        data['sig'] = data['pvalue'].str.find(key) != -1
    else:
        for col in data.columns:
            if col.endswith('*'):
                data[col.replace('*', '?')] = data[col].str.find(key) != -1
        data['sig'] = data.any(axis=1, bool_only=True)
        data.drop([col for col in data.columns if col.endswith("?")], axis=1, inplace=True)
    data = data.loc[data['sig'],:]
    data = data.drop(['sig'], axis=1)
    return data


truncate_output()

ces = CESPanel()
panel = ces.get_panel()
two_years = ces.get_paired_waves()

log_header('''
############
# Matching #
############''')

log_findings(ces.get_matched_outcomes(ces.get_paired_waves(), "age"), f"Comparison of outcomes between new parents and a control group matched on age")
log_findings(ces.get_matched_outcomes(ces.get_paired_waves(), "age + marstat"), f"Comparison of outcomes between new parents and a control group matched on age & marital status")

log_verbose('''
######################################
# Analysis: Exploratory (unweighted) #
######################################''')

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
log_verbose(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (paired waves)")

# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
log_verbose(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (all waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
log_verbose(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (paired waves)")

# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
log_verbose(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (all waves)")

# Ideology distribution across panel: roughly normal, skewing conservative
log_verbose(panel.groupby("ideo5_10").count().loc[:,'caseid'], "Overall distribution of ideo5_10")

# Party distribution across panel: not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
log_verbose(panel.groupby("pid7_10").count().loc[:,'caseid'],  "Overall distribution of pid7_10")

# Party distribution among parents: still U-shaped, a little more liberal, also looks like more moderates
# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
log_verbose(two_years.loc[np.equal(two_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'caseid'], "Distribution of pid7_10 among new parents")

# Counts of liberal/conservative movement, ignoring magnitude
# New parents: 12% more liberal, 11% more conservative
# Non-new-parents: 12% more liberal, 10% more conservative
# TODO: use ideo_composite instead of ideo
log_verbose(ces.count_percentages(ces.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo_direction'), "Ideological direction change")

log_verbose('''
#######################################################################################################
# Analysis: Count flippers: How often do people change ideology/party between two waves? (unweighted) #
#######################################################################################################''')

def log_flippers(issue, start_wave, end_wave, lower_bound, upper_bound):
    log_verbose(f"Percentage of {issue} changing from 20{start_wave} to 20{end_wave}: "
             + str(ces.count_flippers(f"{issue}_{start_wave}", f"{issue}_{end_wave}", lower_bound, upper_bound)))  # 0.8%, too coarse to be useful

log_flippers("pid3", 10, 12, 1, 3)  # 8.9%, fairly coarse

# For pid7, 20-25% each 2 years
log_flippers("pid7", 10, 12, 1, 7)
log_flippers("pid7", 12, 14, 1, 7)
log_flippers("ideo5", 10, 12, 1, 5)
log_flippers("ideo5", 12, 14, 1, 5)
log_flippers("ideo5", 10, 14, 1, 5)

log_header('''
#######################
# Analysis: Attitudes #
#######################''')

log_findings(ces.all_t_test_pvalues(ces.paired_waves), "T test p values, all paired data")
log_findings(ces.all_chisq_pvalues(ces.paired_waves), "Chi square p values, all paired data")

# How different do things look for a single pair of waves?
both = ces.paired_waves
first = both.loc[both['start_wave'] == 10,:]
second = both.loc[both['start_wave'] == 12,:]
log_findings(ces.all_t_test_pvalues(first), "T test p values, 2010/2012 only")
log_findings(ces.all_t_test_pvalues(second), "T test p values, 2012/2014 only")

log_findings(ces.all_t_test_pvalues(ces.paired_waves, demographic_label="firstborn"), "T test p values, all paired data, firstborn")
log_findings(ces.all_chisq_pvalues(ces.paired_waves, demographic_label="firstborn"), "Chi square p values, all paired data, firstborn")

log_verbose(ces.summarize_all_continuous(two_years, 'new_child'), "Summary of continuous issues, all paired data")
log_verbose(ces.summarize_all_continuous(two_years, 'firstborn'), "Summary of continuous issues, all paired data, firstborn child versus all others")

log_verbose(ces.summarize_all_categorical(two_years, 'new_child', 'before'), "Summary of categorical 'before' responses, all paired data")
log_verbose(ces.summarize_all_categorical(two_years, 'new_child', 'after'), "Summary of categorical 'after' responses, all paired data")
log_verbose(ces.summarize_all_categorical(two_years, 'new_child', 'change'), "Summary of categorical responses changing between waves, all paired data")
log_verbose(ces.summarize_all_categorical(two_years, 'new_child', 'persists'), "Summary of categorical responses that change and persist, all 2010/2012 data")

# (not logged) Persistence: how common is persistent change?
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


log_header('''
#################
# Analysis: Age #
#################''')

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
#log_findings(ces.all_t_test_pvalues(young_adults), "T test p values, respondents under 30 years old")   # TODO: RuntimeWarning: invalid value encountered in scalar multiply
log_findings(ces.all_chisq_pvalues(young_adults), "Chi square p values, respondents under 30 years old")

log_verbose(ces.summarize_all_continuous(young_adults, 'new_child'), "Summary of continuous issues, respondents under 30 years old")

log_header('''
####################
# Analysis: Gender #
####################''')
two_years_new_parents = two_years.loc[np.equal(two_years['new_child'], 1),:]
two_years_men = two_years.loc[np.equal(two_years['gender'], 1),:]
two_years_women = two_years.loc[np.equal(two_years['gender'], 2),:]

log_findings(ces.all_t_test_pvalues(two_years_men), "T test p values, new fathers versus other men")
log_findings(ces.all_chisq_pvalues(two_years_men), "Chi square p values, new fathers versus other men")

log_findings(ces.all_t_test_pvalues(two_years_women), "T test p values, new mothers versus other women")
log_findings(ces.all_chisq_pvalues(two_years_women), "Chi square p values, new mothers versus other women")

log_findings(ces.all_t_test_pvalues(two_years_new_parents, demographic_label='gender', a_value=1, b_value=2), "T test p values, new fathers versus new mothers")
log_findings(ces.all_chisq_pvalues(two_years_new_parents, demographic_label='gender'), "Chi square p values, new fathers versus new mothers")

log_verbose(ces.summarize_all_continuous(two_years, ['new_child', 'gender']), "Summary of continuous issues by new_child and gender")

log_verbose(ces.summarize_all_categorical(two_years_new_parents, 'gender', 'before'), "Categorical 'before' responses, new mothers vs new fathers")
log_verbose(ces.summarize_all_categorical(two_years_new_parents, 'gender', 'after'), "Categorical 'after' responses, new mothers vs new fathers")
log_verbose(ces.summarize_all_categorical(two_years_new_parents, 'gender', 'change'), "Categorical response change, new mothers vs new fathers")
log_verbose(ces.summarize_all_categorical(two_years_new_parents, 'gender', 'persists'), "Categorical response persistent change, new mothers vs new fathers")

log_verbose(ces.summarize_all_categorical(two_years_men, 'new_child', 'before'), "Categorical 'before' responses, new fathers vs other men")
log_verbose(ces.summarize_all_categorical(two_years_men, 'new_child', 'after'), "Categorical 'after' responses, new fathers vs other men")
log_verbose(ces.summarize_all_categorical(two_years_men, 'new_child', 'change'), "Categorical response change, new fathers vs other men")
log_verbose(ces.summarize_all_categorical(two_years_men, 'new_child', 'persists'), "Categorical response persistent change, new fathers vs other men")

log_verbose(ces.summarize_all_categorical(two_years_women, 'new_child', 'before'), "Categorical 'before' responses, new mothers vs other women")
log_verbose(ces.summarize_all_categorical(two_years_women, 'new_child', 'after'), "Categorical 'after' responses, new mothers vs other women")
log_verbose(ces.summarize_all_categorical(two_years_women, 'new_child', 'change'), "Categorical response change, new mothers vs other women")
log_verbose(ces.summarize_all_categorical(two_years_women, 'new_child', 'persists'), "Categorical response persistent change, new mothers vs other women")

log_header('''
####################
# Analysis: Income #
####################''')

log_verbose(panel.loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count(), "Income distribution across panel")
# TODO: Should this be looking at the whole panel, at people who became a parent in any wave?
log_verbose(two_years.loc[:,['income', 'new_child', 'caseid']].groupby(['new_child', 'income']).count(), "Income distribution, new parents and others")
log_verbose(two_years.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count(), "Income distribution by quintile")

two_years_bottom_80 = two_years.loc[np.equal(two_years['high_income'], 0),:]  # "not high", not necessarily low
two_years_bottom_40 = two_years.loc[np.equal(two_years['low_income'], 1),:]
two_years_top_20 = two_years.loc[np.equal(two_years['high_income'], 1),:]

log_findings(ces.all_t_test_pvalues(two_years_bottom_80), "T test p values, bottom 80% new parents versus other bottom 80% respondents")
log_findings(ces.all_chisq_pvalues(two_years_bottom_80), "Chi square p values, bottom 80% new parents versus other bottom 80% respondents")

log_findings(ces.all_t_test_pvalues(two_years_bottom_40), "T test p values, bottom 40% new parents versus other bottom 40% respondents")
log_findings(ces.all_chisq_pvalues(two_years_bottom_40), "Chi square p values, bottom 40% new parents versus other bottom 40% respondents")

log_findings(ces.all_t_test_pvalues(two_years_top_20), "T test p values, top 20% new parents versus other top 20% respondents")
log_findings(ces.all_chisq_pvalues(two_years_top_20), "Chi square p values, top 20% new parents versus other top 20% respondents")

log_findings(ces.all_t_test_pvalues(two_years_new_parents, demographic_label='high_income'), "T test p values, top 20% new parents versus bottom 80% new parents")
log_findings(ces.all_chisq_pvalues(two_years_new_parents, demographic_label='high_income'), "Chi square p values, top 20% new parents versus bottom 80% new parents")

log_verbose(ces.summarize_all_continuous(two_years, ['new_child', 'high_income']), "Summary of continuous issues by new_child and income")

log_verbose('''
##########################
# Analysis: Non-response #
##########################''')

# TODO: Do non-response rates differ for parents and non-parents?
# TODO: weight these?
log_verbose(ces.summarize_non_response(two_years), "Non-response rates")
