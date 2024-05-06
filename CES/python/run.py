import argparse
import numpy as np
import pandas as pd

from collections import Counter

from ces import CESPanel

#np.seterr(all='raise')

parser = argparse.ArgumentParser(description="Analyze parenting political data")
parser.add_argument('-o', '--output', help='Suffix for output directory')
parser.add_argument('-m', '--only-match', action='store_true')
parser.add_argument('-l', '--only-model', action='store_true')
parser.add_argument('-p', '--only-panel', action='store_true')
parser.add_argument('-e', '--only-explore', action='store_true')
parser.add_argument('-M', '--skip-match', action='store_true')
parser.add_argument('-L', '--skip-model', action='store_true')
parser.add_argument('-P', '--skip-panel', action='store_true')
parser.add_argument('-E', '--skip-explore', action='store_true')
args = parser.parse_args()

def _should_run(section, skip_header=False):
    sections = ['match', 'model', 'panel', 'explore']
    if section not in sections:
        raise ParentsPoliticsPanelException(f"Unrecognized section: {section}")

    should_run = True
    if getattr(args, f'skip_{section}'):
        should_run = False
    else:
        for s in sections:
            if getattr(args, f'only_{s}'):
                should_run = s == section

    if not skip_header:
        skip_label = ": SKIPPING" if not should_run else ""
        ces.log_header(f'''
        ############
        # {section.upper()}{skip_label}
        ############''')

    return should_run


ces = CESPanel(args.output)
two_years = ces.get_paired_waves()
waves_1012 = two_years.loc[two_years['start_wave'] == 10,:].copy()
waves_1214 = two_years.loc[two_years['start_wave'] == 12,:].copy()

exploratory_data = ces.get_paired_waves()
panel_data = waves_1012
matching_data = waves_1012

waves_1012 = ces.filter_age(waves_1012, 40)


### Build samples for matching: treatment group plus the relevant control
sample_1012 = {}
sample_1012['firstborn'] = pd.concat([
    ces.filter_dummy(waves_1012, 'firstborn'),
    ces.filter_dummy(waves_1012, 'childless'),
])
sample_1012['new_child'] = pd.concat([
    ces.filter_dummy(waves_1012, 'new_child'),
    ces.filter_dummy(waves_1012, 'childless'),
])
sample_1012['is_parent'] = waves_1012  # Treatment is is_parent, control is non-parents, which is the whole sample


if _should_run("match"):
    formula = 'C(gender) + C(race) + C(employ) + C(marstat) + pew_churatd + C(ownhome) + RUCC_2023 + C(division) + age + income'

    for treatment in ces.treatments - {'new_child'}:
        ces.add_score(waves_1012, f"{treatment} ~ {formula}", label=f'{treatment}_score')
        ces.add_score(sample_1012[treatment], f"{treatment} ~ {formula}", label=f'{treatment}_score')

    for treatment in ces.treatments - {'new_child'}:
        outcomes = ces.get_matched_outcomes(sample_1012[treatment], treatment, age_limit=40)
        ces.log_matching(outcomes, f"All respondents under 40, treatment={treatment}", viz_filename=f"matching_{treatment}")

    # diff is control - treatment, or a -b for t tests, so negatives mean treatment group (women, high_income, low_income) is more liberal
    def matching_for_subset(demo_label, demo_a, demo_b):
        # Within each treatment, compare demographic groups
        for treatment in ces.treatments - {'new_child'}:
            comparator_desc = f"{demo_label} ({demo_a} vs {demo_b})"
            outcomes = ces.get_matched_outcomes(ces.filter_dummy(waves_1012, treatment), demo_label, score_label=f"{treatment}_score",
                                                control_value=demo_a, treatment_value=demo_b, age_limit=40,
                                                comparator_treatment=treatment, comparator_desc=comparator_desc)
            ces.log_matching(outcomes, f"{demo_label}, {treatment}=1, respondents under 40, by {comparator_desc}, matched on {formula}")

        # Within each demographic group, compare those receiving and not receiving treatment
        for demo_value in (demo_a, demo_b):
            for treatment in ces.treatments - {'new_child'}:
                comparator_desc = f"{demo_label}={demo_value}"
                demo_subset = ces.filter_demographic(sample_1012[treatment], demo_label, demo_value)
                outcomes = ces.get_matched_outcomes(demo_subset, treatment, age_limit=40, comparator_desc=comparator_desc)
                ces.log_matching(outcomes, f"{comparator_desc}, respondents under 40, treatment={treatment}, matched on {formula}")

    ces.log_header('''
    ####################
    # Matching: Gender #
    ####################''')
    matching_for_subset("gender", 1, 2)

    ces.log_header('''
    ####################
    # Matching: Income #
    ####################''')
    matching_for_subset("high_income", 0, 1)    # Top 20% vs bottom 80%
    matching_for_subset("low_income", 0, 1)     # Bottom 40% vs top 60%

if _should_run("model"):
    top_formulas = {}
    for df, do_weight, addendum in [
        #(waves_1012, False, ""),
        #(waves_1012, True, ""),
        #(ces.filter_age(waves_1012, 40), False, ", limited to respondents under 40, unweighted"),
        (ces.filter_age(waves_1012, 40), True, ", limited to respondents under 40, weighted"),
    ]:
        for treatment in ces.treatments - {'new_child'}:
            tag = f"{treatment}{addendum}"
            print("Looking at " + tag)
            models = ces.consider_models(df, treatment, do_weight=do_weight)
            ces.log_verbose(models, f"Comparison of models to predict {treatment}{addendum}")
            if len(models):
                top_formula = models['formula'][1]  # 1 because these are indexed based on DataFrame.rank
                #ces.log_verbose(ces.scores_histogram_table(df, top_formula, treatment), f"Score histogram for top model: {top_formula}")
                top_formulas[tag] = [
                    models['formula'][1],
                    models['formula'][2],
                    models['formula'][3],
                    models['formula'][4],
                    models['formula'][5],
                ]

if _should_run("explore"):
    ces.log_verbose("### All of this is unweighted")

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

    ### Distributions across panel
    panel = ces.get_panel()
    # Ideology: roughly normal, skewing conservative
    ces.log_verbose(panel.groupby("ideo5_10").count().loc[:,'caseid'], "Overall distribution of ideo5_10")
    # Party: not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
    ces.log_verbose(panel.groupby("pid7_10").count().loc[:,'caseid'],  "Overall distribution of pid7_10")
    # Party, among parents: still U-shaped, a little more liberal, also looks like more moderates
    ces.log_verbose(two_years.loc[np.equal(two_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'caseid'], "Distribution of pid7_10 among new_child")

    # Counts of liberal/conservative movement, ignoring magnitude
    # Maybe curiously, these numbers are a lot higher for the composite - people often change pid or ideo but not both
    # new_child: 22% more liberal, 19% more conservative
    # Non-new_child: 21% more liberal, 17% more conservative
    ces.log_verbose(ces.count_percentages(two_years, 'new_child', '_ideo_composite_direction'), "Ideological composite direction change")
    ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'ideo_direction'), "Ideological direction change")
    ces.log_verbose(ces.count_percentages(two_years, 'new_child', 'pid_direction'), "Party direction change")

    ces.log_verbose("### Count flippers: How often do people change ideology/party between two waves?")

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

    ces.log_verbose(ces.summarize_issues_non_response(two_years), "Non-response rates for issues")
    ces.log_verbose(ces.summarize_demographics_non_response(two_years), "Non-response rates for demographics")  # income is 13%

if _should_run("panel"):
    for treatment in ces.treatments:
        #ces.log_findings(ces.all_t_test_pvalues(sample_1012[treatment], treatment), f"T test p values for {treatment}")
        #ces.log_verbose(ces.summarize_all_issues(sample_1012[treatment], treatment), f"Summary of issues for {treatment}")
        ces.log_panel(ces.all_t_test_pvalues(sample_1012[treatment], treatment, age_limit=40), f"T test p values, respondents under 40 years old: {treatment}", viz_filename=f"panel_{treatment}")
        ces.log_verbose(ces.summarize_all_issues(sample_1012[treatment], treatment, age_limit=40), f"Summary of issues, respondents under 40 years old: {treatment}")

    # (not logged) Persistence: how common is persistent change?
    # Of those who changed, how many keep that change?
    # Note this doesn't account for age limit
    for treatment in ces.treatments:
        ces.log_verbose(ces.summarize_all_persistence(treatment), f"Summary of persistent change frequency: {treatment}")

    ces.log_header('''
    ##########################
    # Panel analysis: Gender #
    ##########################''')
    for age_limit in [40]:  # (None, 40):
        for treatment in ces.treatments - {'new_child'}:
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_demographic(sample_1012[treatment], "gender", 1), treatment, age_limit=age_limit, comparator_desc="gender=1"),
                             f"T test p values, fathers ({treatment}) versus other men")
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_demographic(sample_1012[treatment], "gender", 2), treatment, age_limit=age_limit, comparator_desc="gender=2"),
                             f"T test p values, mothers ({treatment}) versus other women")
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_dummy(sample_1012[treatment], treatment), 'gender', a_value=1, b_value=2, age_limit=age_limit,
                                                    comparator_treatment=treatment, comparator_desc="gender (1 vs 2)"),
                             f"T test p values, fathers versus mothers: all {treatment}")
            ces.log_verbose(ces.summarize_all_issues(sample_1012[treatment], [treatment, 'gender']), f"Summary of issues by new_child and gender: {treatment}")

    ces.log_header('''
    ##########################
    # Panel analysis: Income #
    ##########################''')
    ces.log_verbose(ces.get_panel().loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count(), "Income distribution across panel")
    ces.log_verbose(waves_1012.loc[:,['income', 'new_child', 'caseid']].groupby(['new_child', 'income']).count(), "Income distribution, new_child and others")
    ces.log_verbose(waves_1012.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count(), "Income distribution by quintile")

    for age_limit in [40]:  # (None, 40):
        for treatment in ces.treatments - {'new_child'}:
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_demographic(sample_1012[treatment], 'low_income', 1), treatment, age_limit=age_limit, comparator_desc="low_income=1"),
                             f"T test p values, bottom 40% {treatment} versus other bottom 40% respondents")
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_demographic(sample_1012[treatment], 'low_income', 0), treatment, age_limit=age_limit, comparator_desc="low_income=0"),
                             f"T test p values, top 60% {treatment} versus other top 60% respondents")
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_demographic(sample_1012[treatment], 'high_income', 1), treatment, age_limit=age_limit, comparator_desc="high_income=1"),
                             f"T test p values, top 20% {treatment} versus other top 20% respondents")
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_demographic(sample_1012[treatment], 'high_income', 0), treatment, age_limit=age_limit, comparator_desc="high_income=0"),
                             f"T test p values, bottom 80% {treatment} versus other bottom 80% respondents")

            ces.log_panel(ces.all_t_test_pvalues(ces.filter_dummy(sample_1012[treatment], treatment), 'high_income', age_limit=age_limit,
                                                    comparator_treatment=treatment, comparator_desc="high_income (0 vs 1)"),
                             f"T test p values, top 20% {treatment} versus bottom 80% {treatment}")
            ces.log_panel(ces.all_t_test_pvalues(ces.filter_dummy(sample_1012[treatment], treatment), 'low_income', age_limit=age_limit,
                                                    comparator_treatment=treatment, comparator_desc="low_income (0 vs 1)"),
                             f"T test p values, bottom 40% {treatment} versus top 60% {treatment}")

            ces.log_verbose(ces.summarize_all_issues(sample_1012[treatment], [treatment, 'high_income']), f"Summary of issues by {treatment} and high_income")
            ces.log_verbose(ces.summarize_all_issues(sample_1012[treatment], [treatment, 'low_income']), f"Summary of issues by {treatment} and low_income")

if _should_run("match") and _should_run("panel"):
    ces.log_header('''
    #######################
    # Approach Comparison #
    #######################''')
    ces.log_verbose(ces.get_approach_comparison(), "Comparison of matching and panel analysis")
    findings = ces.filter_approach_comparison(0.1, 3, 100)
    ces.log_verbose(findings, "Findings with significance ***, at least 0.1 substantive difference, and at least 100 cases each in treatment and control groups")
    ces.log_verbose(Counter(findings['issue']), "Issue counts in the above table")

    ces.log_verbose(ces.get_core_approach_comparison(), "Compare matching's after value with panel analysis's delta, limited to young adults")
    core_findings = ces.filter_core_approach_comparison(10, 1, 10)
    ces.log_verbose(core_findings, "Core findings with significance *, at least 10% substantive difference, and at least 10 cases each in treatment and control groups")
    ces.log_verbose(Counter(core_findings['issue']), "Issue counts in the above table")
