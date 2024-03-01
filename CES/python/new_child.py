import numpy as np
import pandas as pd

from ces import CESPanel


ces = CESPanel()
panel = ces.get_panel()
two_years = ces.get_paired_waves()

ces.log_header('''
############
# Matching #
############''')

waves_1012 = two_years.loc[two_years['start_wave'] == 10,:].copy()
waves_1214 = two_years.loc[two_years['start_wave'] == 12,:].copy()

ces.log_findings(ces.get_matched_outcomes(waves_1012, "age"), f"Comparison of outcomes between new parents and a control group matched on age")
ces.log_findings(ces.get_matched_outcomes(waves_1012, "age + marstat"), f"Comparison of outcomes between new parents and a control group matched on age & marital status")
ces.log_findings(ces.get_matched_outcomes(waves_1012, "age + marstat + ideo_before"), f"Comparison of outcomes between new parents and a control group matched on age & marital status & ideology")

ces.log_verbose(ces.consider_models(ces.get_paired_waves()), f"Comparison of models to predict new parenthood")

ces.log_verbose('''
######################################
# Analysis: Exploratory (unweighted) #
######################################''')

counts = ces.get_paired_waves().groupby('parenthood', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['new_child', 'total']], "Total number of each parenthood group (paired waves)")

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (paired waves)")

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['new_child', 'total']], "Total number of new parents and non-new-parents in sample (all waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (paired waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (all waves)")

# Ideology distribution across panel: roughly normal, skewing conservative
ces.log_verbose(panel.groupby("ideo5_10").count().loc[:,'caseid'], "Overall distribution of ideo5_10")

# Party distribution across panel: not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
ces.log_verbose(panel.groupby("pid7_10").count().loc[:,'caseid'],  "Overall distribution of pid7_10")

# Party distribution among parents: still U-shaped, a little more liberal, also looks like more moderates
ces.log_verbose(two_years.loc[np.equal(two_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'caseid'], "Distribution of pid7_10 among new parents")

# Counts of liberal/conservative movement, ignoring magnitude
# Maybe curiously, these numbers are a lot higher for the composite - people often change pid or ideo but not both
# New parents: 22% more liberal, 19% more conservative
# Non-new-parents: 21% more liberal, 17% more conservative
ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'ideo_composite_direction'), "Ideological composite direction change")
ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'ideo_direction'), "Ideological direction change")
ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'pid_direction'), "Party direction change")

ces.log_verbose('''
#######################################################################################################
# Analysis: Count flippers: How often do people change ideology/party between two waves? (unweighted) #
#######################################################################################################''')

def log_flippers(issue, start_wave, end_wave, lower_bound, upper_bound):
    ces.log_verbose(f"Percentage of {issue} changing from 20{start_wave} to 20{end_wave}: "
                    + str(ces.count_flippers(f"{issue}_{start_wave}", f"{issue}_{end_wave}", lower_bound, upper_bound)))  # 0.8%, too coarse to be useful

log_flippers("pid3", 10, 12, 1, 3)  # 8.9%, fairly coarse

# For pid7, 20-25% each 2 years
log_flippers("pid7", 10, 12, 1, 7)
log_flippers("pid7", 12, 14, 1, 7)
log_flippers("ideo5", 10, 12, 1, 5)
log_flippers("ideo5", 12, 14, 1, 5)
log_flippers("ideo5", 10, 14, 1, 5)

ces.log_header('''
#######################
# Analysis: Attitudes #
#######################''')

ces.log_findings(ces.all_t_test_pvalues(ces.paired_waves), "T test p values, all paired data")

# How different do things look for a single pair of waves? Should I treat these as two different data sets?
ces.log_findings(ces.all_t_test_pvalues(waves_1012), "T test p values, 2010/2012 only")
ces.log_findings(ces.all_t_test_pvalues(waves_1214), "T test p values, 2012/2014 only")

ces.log_findings(ces.all_t_test_pvalues(ces.paired_waves, demographic_label="firstborn"), "T test p values, all paired data, firstborn")

ces.log_verbose(ces.summarize_all_issues(two_years, 'new_child'), "Summary of issues, all paired data")
ces.log_verbose(ces.summarize_all_issues(two_years, 'firstborn'), "Summary of issues, all paired data, firstborn child versus all others")

# (not logged) Persistence: how common is persistent change?
# Of the new parents who changed, how many keep that change?
# New parents often slightly more likely to experience persistent change than others
ces.log_verbose(ces.summarize_all_persistence(), "Summary of persistent change frequency")

ces.log_header('''
#################
# Analysis: Age #
#################''')

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
ces.log_findings(ces.all_t_test_pvalues(young_adults), "T test p values, respondents under 30 years old")

ces.log_verbose(ces.summarize_all_issues(young_adults, 'new_child'), "Summary of issues, respondents under 30 years old")

ces.log_header('''
####################
# Analysis: Gender #
####################''')
two_years_new_parents = two_years.loc[np.equal(two_years['new_child'], 1),:]
two_years_men = two_years.loc[np.equal(two_years['gender'], 1),:]
two_years_women = two_years.loc[np.equal(two_years['gender'], 2),:]

ces.log_findings(ces.all_t_test_pvalues(two_years_men), "T test p values, new fathers versus other men")

ces.log_findings(ces.all_t_test_pvalues(two_years_women), "T test p values, new mothers versus other women")

ces.log_findings(ces.all_t_test_pvalues(two_years_new_parents, demographic_label='gender', a_value=1, b_value=2), "T test p values, new fathers versus new mothers")

ces.log_verbose(ces.summarize_all_issues(two_years, ['new_child', 'gender']), "Summary of issues by new_child and gender")

ces.log_header('''
####################
# Analysis: Income #
####################''')

ces.log_verbose(panel.loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count(), "Income distribution across panel")
ces.log_verbose(two_years.loc[:,['income', 'new_child', 'caseid']].groupby(['new_child', 'income']).count(), "Income distribution, new parents and others")
ces.log_verbose(two_years.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count(), "Income distribution by quintile")

two_years_bottom_80 = two_years.loc[np.equal(two_years['high_income'], 0),:]  # "not high", not necessarily low
two_years_bottom_40 = two_years.loc[np.equal(two_years['low_income'], 1),:]
two_years_top_20 = two_years.loc[np.equal(two_years['high_income'], 1),:]

ces.log_findings(ces.all_t_test_pvalues(two_years_bottom_80), "T test p values, bottom 80% new parents versus other bottom 80% respondents")

ces.log_findings(ces.all_t_test_pvalues(two_years_bottom_40), "T test p values, bottom 40% new parents versus other bottom 40% respondents")

ces.log_findings(ces.all_t_test_pvalues(two_years_top_20), "T test p values, top 20% new parents versus other top 20% respondents")

ces.log_findings(ces.all_t_test_pvalues(two_years_new_parents, demographic_label='high_income'), "T test p values, top 20% new parents versus bottom 80% new parents")

ces.log_verbose(ces.summarize_all_issues(two_years, ['new_child', 'high_income']), "Summary of issues by new_child and income")

ces.log_verbose('''
#######################################
# Analysis: Non-response (unweighted) #
#######################################''')

# TODO: Do non-response rates differ for parents and non-parents?
ces.log_verbose(ces.summarize_issues_non_response(two_years), "Non-response rates for issues")
ces.log_verbose(ces.summarize_demographics_non_response(two_years), "Non-response rates for demographics")  # income is 13%
