import argparse
import numpy as np
import pandas as pd

from ces import CESPanel


def _filter_dummy(df, dummy):
    return df.loc[df[dummy] == 1,:].copy()

def _filter_under_40(df):
    return df.loc[df['age'] < 40,:].copy()

def _filter_demographic(df, label, value):
    return df.loc[df[label] == value,:].copy()

 
parser = argparse.ArgumentParser(description="Analyze parenting political data")
parser.add_argument('-o', '--output', help='Suffix for output directory')
args = parser.parse_args()


ces = CESPanel(args.output)
panel = ces.get_panel()
two_years = ces.get_paired_waves()
under_40_two_years = _filter_under_40(two_years)

waves_1012 = two_years.loc[two_years['start_wave'] == 10,:].copy()
waves_1214 = two_years.loc[two_years['start_wave'] == 12,:].copy()

under_40_1012 = _filter_under_40(waves_1012)

parents_1012 = _filter_dummy(waves_1012, 'is_parent')
parents_under_40_1012 = _filter_under_40(parents_1012)

new_child_1012 = _filter_dummy(waves_1012, 'new_child')
new_child_under_40_1012 = _filter_under_40(new_child_1012)
firstborn_1012 = _filter_dummy(waves_1012, 'firstborn')
firstborn_under_40_1012 = _filter_under_40(firstborn_1012)

ces.log_header('''
############
# Matching #
############''')

formulas = [
    "marstat + pew_churatd + age + income_quintile + educ",
]

parenthood_01_1012 = waves_1012.loc[waves_1012['parenthood'] < 2,:].copy()
under_40_parenthood_01_1012 = parenthood_01_1012.loc[parenthood_01_1012['age'] < 40,:].copy()

for formula in formulas:
    for treatment in ces.treatments:
        ces.log_findings(ces.evaluate_scores(waves_1012, formula, treatment), f"Evaluate scoring of {treatment} ~ {formula}")

    # Treatment is firstborn, control is non-parents
    ces.log_findings(ces.get_matched_outcomes(parenthood_01_1012, formula, 'firstborn'), f"Comparison of outcomes between firstborn and non-parents, matched on {formula}")
    ces.log_findings(ces.get_matched_outcomes(under_40_parenthood_01_1012, formula, 'firstborn'), f"Same, but only respondents under 40")

    # Treatment is new_child, control is other parents
    ces.log_findings(ces.get_matched_outcomes(parents_1012, formula, 'new_child'), f"Comparison of outcomes between new_child and other parents, matched on {formula}")
    ces.log_findings(ces.get_matched_outcomes(parents_under_40_1012, formula, 'new_child'), f"Same, but only respondents under 40")

    # Treatment is is_parent, control is non-parents
    ces.log_findings(ces.get_matched_outcomes(waves_1012, formula, 'is_parent'), f"Comparison of outcomes between is_parent and non-parents, matched on {formula}")
    ces.log_findings(ces.get_matched_outcomes(under_40_1012, formula, 'is_parent'), f"Same, but only respondents under 40")

ces.log_header('''
####################
# Matching: Gender #
####################''')
def matching_for_subset(demo_label, demo_a, demo_b):
    # Split data by demographic
    demo_a_1012 = _filter_demographic(waves_1012, demo_label, demo_a)
    demo_a_under_40_1012 = _filter_under_40(demo_a_1012)
    demo_b_1012 = _filter_demographic(waves_1012, demo_label, demo_b)
    demo_b_under_40_1012 = _filter_under_40(demo_b_1012)

    # Build samples for matching: treatment group plus the relevant control, which may not be the full dataset
    sample_1012 = {}
    sample_1012['firstborn'] = pd.concat([_filter_dummy(waves_1012, 'firstborn'), _filter_dummy(waves_1012, 'childless')])  # Treatment is firstborn, control is non-parents
    sample_1012['new_child'] = _filter_dummy(waves_1012, 'is_parent')  # Treatment is new_child, control is other parents
    sample_1012['is_parent'] = waves_1012  # Treatment is is_parent, control is non-parents, which is the whole sample
    under_40_sample_1012 = {}
    for treatment in ces.treatments:
        under_40_sample_1012[treatment] = _filter_under_40(sample_1012[treatment])

    for formula in formulas:
        for treatment in ces.treatments:
            ces.log_findings(ces.get_matched_outcomes(sample_1012[treatment], f"{treatment} ~ {formula}", demo_label, demo_a, demo_b),
                             f"Comparison of outcomes when {treatment}=1, split by {demo_label}, matched on {formula}")
            ces.log_findings(ces.get_matched_outcomes(under_40_sample_1012[treatment], f"{treatment} ~ {formula}", demo_label, demo_a, demo_b),
                             f"Comparison of outcomes when {treatment}=1, respondents under 40, split by {demo_label}, matched on {formula}")

        for demo_value, demo_subset, demo_subset_under_40 in (
            (demo_a, demo_a_1012, demo_a_under_40_1012),
            (demo_b, demo_b_1012, demo_b_under_40_1012),
        ):
            for treatment in ces.treatments:
                ces.log_findings(ces.get_matched_outcomes(demo_subset, f"{treatment} ~ {formula}", treatment),
                                 f"Comparison of outcomes, {demo_label}={demo_value}, treatment={treatment}, matched on {formula}")
                ces.log_findings(ces.get_matched_outcomes(demo_subset_under_40, f"{treatment} ~ {formula}", treatment),
                                 f"Comparison of outcomes, {demo_label}={demo_value}, respondents under 40, treatment={treatment}, matched on {formula}")

matching_for_subset("gender", 1, 2)

ces.log_header('''
####################
# Matching: Income #
####################''')

matching_for_subset("high_income", 0, 1)    # Top 20% vs bottom 80%
matching_for_subset("low_income", 0, 1)     # Bottom 40% vs top 60%

ces.log_header('''
############
# Modeling #
############''')

for df, addendum in [
    (two_years, ""),
    (under_40_1012, ", limited to respondents under 40"),
]:
    for treatment in ces.treatments:
        models = ces.consider_models(df, treatment)
        ces.log_verbose(models, f"Comparison of models to predict {treatment}{addendum}")
        if len(models):
            top_formula = models['formula'][1]  # 1 because these are indexed pased on DataFrame.rank
            ces.log_verbose(ces.evaluate_scores(df, top_formula, treatment), f"Score evaluation for top model: {top_formula}")

ces.log_verbose('''
######################################
# Analysis: Exploratory (unweighted) #
######################################''')

counts = ces.get_paired_waves().groupby('parenthood', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['new_child', 'total']], "Total number of each parenthood group (paired waves)")

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['new_child', 'total']], "Total number of new_child and non-new_child in sample (paired waves)")

counts = ces.get_paired_waves().groupby('new_child', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['new_child', 'total']], "Total number of new_child and non-new_child in sample (all waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (paired waves)")

counts = ces.get_paired_waves().groupby('firstborn', as_index=False).count().rename(columns={'caseid': 'total'})
ces.log_verbose(counts.loc[:,['firstborn', 'total']], "Total number of new first-time parents and others in sample (all waves)")

# Ideology distribution across panel: roughly normal, skewing conservative
ces.log_verbose(panel.groupby("ideo5_10").count().loc[:,'caseid'], "Overall distribution of ideo5_10")

# Party distribution across panel: not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
ces.log_verbose(panel.groupby("pid7_10").count().loc[:,'caseid'],  "Overall distribution of pid7_10")

# Party distribution among parents: still U-shaped, a little more liberal, also looks like more moderates
ces.log_verbose(two_years.loc[np.equal(two_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'caseid'], "Distribution of pid7_10 among new_child")

# Counts of liberal/conservative movement, ignoring magnitude
# Maybe curiously, these numbers are a lot higher for the composite - people often change pid or ideo but not both
# new_child: 22% more liberal, 19% more conservative
# Non-new_child: 21% more liberal, 17% more conservative
ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'ideo_composite_direction'), "Ideological composite direction change")
ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'ideo_direction'), "Ideological direction change")
ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'pid_direction'), "Party direction change")

ces.log_verbose('''
#############################################################################################
# Count flippers: How often do people change ideology/party between two waves? (unweighted) #
#############################################################################################''')

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
#############################
# Panel analysis: Attitudes #
#############################''')
ces.log_findings(ces.all_t_test_pvalues(ces.paired_waves, "new_child"), "T test p values, all paired data")

# How different do things look for a single pair of waves? Should I treat these as two different data sets?
ces.log_findings(ces.all_t_test_pvalues(waves_1012, "new_child"), "T test p values, 2010/2012 only")
ces.log_findings(ces.all_t_test_pvalues(waves_1214, "new_child"), "T test p values, 2012/2014 only")

ces.log_findings(ces.all_t_test_pvalues(ces.paired_waves, "firstborn"), "T test p values, all paired data, firstborn")
ces.log_findings(ces.all_t_test_pvalues(waves_1012, "firstborn"), "T test p values, 2010/2012 only, firstborn")
ces.log_findings(ces.all_t_test_pvalues(waves_1214, "firstborn"), "T test p values, 2012/2014 only, firstborn")

ces.log_verbose(ces.summarize_all_issues(two_years, 'new_child'), "Summary of issues, all paired data")
ces.log_verbose(ces.summarize_all_issues(two_years, 'firstborn'), "Summary of issues, all paired data, firstborn child versus all others")

# (not logged) Persistence: how common is persistent change?
# Of the new_child who changed, how many keep that change?
# new_child often slightly more likely to experience persistent change than others
for treatment in ces.treatments:
    ces.log_verbose(ces.summarize_all_persistence(treatment), f"Summary of persistent change frequency: {treatment}")

ces.log_header('''
#######################
# Panel analysis: Age #
#######################''')

# Change to 40 to match matching?
young_adults = two_years.loc[np.less(two_years['age'], 30),:]
for treatment in ces.treatments:
    ces.log_findings(ces.all_t_test_pvalues(young_adults, treatment), f"T test p values, respondents under 30 years old: {treatment}")
    ces.log_verbose(ces.summarize_all_issues(young_adults, treatment), f"Summary of issues, respondents under 30 years old: {treatment}")

ces.log_header('''
##########################
# Panel analysis: Gender #
##########################''')
two_years_new_parents = two_years.loc[np.equal(two_years['new_child'], 1),:]
two_years_men = two_years.loc[np.equal(two_years['gender'], 1),:]
two_years_women = two_years.loc[np.equal(two_years['gender'], 2),:]

for treatment in ces.treatments:
    ces.log_findings(ces.all_t_test_pvalues(two_years_men, treatment), f"T test p values, new fathers versus other men: {treatment}")
    ces.log_findings(ces.all_t_test_pvalues(two_years_women, treatment), f"T test p values, new mothers versus other women: {treatment}")
    ces.log_findings(ces.all_t_test_pvalues(two_years_new_parents, 'gender', a_value=1, b_value=2), f"T test p values, new fathers versus new mothers: {treatment}")
    ces.log_verbose(ces.summarize_all_issues(two_years, [treatment, 'gender']), f"Summary of issues by new_child and gender: {treatment}")

ces.log_header('''
##########################
# Panel analysis: Income #
##########################''')

ces.log_verbose(panel.loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count(), "Income distribution across panel")
ces.log_verbose(two_years.loc[:,['income', 'new_child', 'caseid']].groupby(['new_child', 'income']).count(), "Income distribution, new_child and others")
ces.log_verbose(two_years.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count(), "Income distribution by quintile")

two_years_bottom_80 = two_years.loc[np.equal(two_years['high_income'], 0),:]  # "not high", not necessarily low
two_years_bottom_40 = two_years.loc[np.equal(two_years['low_income'], 1),:]
two_years_top_20 = two_years.loc[np.equal(two_years['high_income'], 1),:]

for treatment in ces.treatments:
    ces.log_findings(ces.all_t_test_pvalues(two_years_bottom_80, treatment), f"T test p values, bottom 80% {treatment} versus other bottom 80% respondents")
    ces.log_findings(ces.all_t_test_pvalues(two_years_bottom_40, treatment), f"T test p values, bottom 40% {treatment} versus other bottom 40% respondents")
    ces.log_findings(ces.all_t_test_pvalues(two_years_top_20, treatment), f"T test p values, top 20% {treatment} versus other top 20% respondents")
    ces.log_findings(ces.all_t_test_pvalues(two_years_new_parents, 'high_income'), f"T test p values, top 20% {treatment} versus bottom 80% {treatment}")
    ces.log_verbose(ces.summarize_all_issues(two_years, [treatment, 'high_income']), f"Summary of issues by {treatment} and income")

ces.log_verbose('''
#############################
# Non-response (unweighted) #
#############################''')

ces.log_verbose(ces.summarize_issues_non_response(two_years), "Non-response rates for issues")
ces.log_verbose(ces.summarize_demographics_non_response(two_years), "Non-response rates for demographics")  # income is 13%
