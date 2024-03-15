import numpy as np
import os
import pandas as pd
import re
import statsmodels.api as sm
import statsmodels.formula.api as smf

from collections import defaultdict, namedtuple
from datetime import datetime
from itertools import combinations
from pandas import DataFrame
from statsmodels.stats.weightstats import ttest_ind

Result = namedtuple('Result', ['statistic', 'df', 'pvalue'])


class ParentsPoliticsPanelException(Exception):
    pass


class ParentsPoliticsPanel():
    OUTPUT_DIR = 'output'
    OUTPUT_FILES = ['significant.log', 'all.log', 'two_stars.log', 'three_stars.log', 'substantive.log']

    METRICS = ['before', 'after', 'delta', 'delta_abs', 'persists', 'persists_abs']
    waves = []
    demographics_with_bounds = []

    @property
    def start_waves(self):
        return self.waves[:-1]

    @property
    def end_waves(self):
        return self.waves[1:]

    @property
    def demographics(self):
        return [d[0] for d in self.demographics_with_bounds]

    def __init__(self, output_suffix=''):
        self.ISSUES = set()

        if output_suffix:
            self.OUTPUT_DIR = f'{self.OUTPUT_DIR}_{output_suffix}'
        if not os.path.isdir(self.OUTPUT_DIR):
            print("Making directory " + self.OUTPUT_DIR)
            os.mkdir(self.OUTPUT_DIR)
        self._truncate_output()

        self.panel = self._load_panel()
        self.paired_waves = self._build_paired_waves(self._trimmed_panel())

        self.paired_waves = self._add_parenting(self.paired_waves)
        self.paired_waves = self.paired_waves.astype({t: 'int32' for t in self.treatments})

        self.paired_waves = self._add_all_single_issues(self.paired_waves)
        self.paired_waves = self.add_all_composite_issues(self.paired_waves)

        # De-fragment frames
        self.paired_waves = self.paired_waves.copy()

    def _truncate_output(self):
        for filename in self.OUTPUT_FILES:
            with open(os.path.join(self.OUTPUT_DIR, filename), 'w') as fh:
                fh.write(f"Run started {datetime.now()}\n")

    def log_header(self, header):
        print(header)
        for filename in self.OUTPUT_FILES:
            self._output(filename, header)

    def log_verbose(self, data, description=''):
        self._output('all.log', data, description)

    def log_findings(self, data, description=''):
        self._output('all.log', data, description)
        significant = self._limit_to_significant(data)
        self._output('significant.log', significant, description)
        self._output('substantive.log', self._limit_to_substantive(significant), description)
        self._output('two_stars.log', self._limit_to_significant(significant, level=2), description)
        self._output('three_stars.log', self._limit_to_significant(significant, level=3), description)

    def _limit_to_significant(self, data, level=1):
        data = data.copy()
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

    def _limit_to_substantive(self, data, threshold=0.1):
        data = data.copy()
        if 'diff' in data:
            data['sub'] = np.abs(data['diff']) >= threshold
        else:
            for col in data.columns:
                if col.endswith('-'):
                    data[col.replace('-', '?')] = np.abs(data[col]) >= threshold
            data['sub'] = data.any(axis=1, bool_only=True)
            data.drop([col for col in data.columns if col.endswith("?")], axis=1, inplace=True)
        data = data.loc[data['sub'],:]
        data = data.drop(['sub'], axis=1)
        return data

    def _output(self, filename, data, description=''):
        if type(data) == pd.DataFrame:
            data = data.to_string(max_rows=100)
        else:
            data = str(data)
        if description:
            data = "\n" + description + "\n" + data
        with open(os.path.join(self.OUTPUT_DIR, filename), 'a') as fh:
            fh.write(data + "\n")

    def _load_panel(self):
        raise NotImplementedError()

    def _trimmed_panel(self):
        raise NotImplementedError()

    def _build_paired_waves(self, df):
        raise NotImplementedError()

    def _add_parenting(self, df):
        raise NotImplementedError()

    def _add_all_single_issues(self, df):
        raise NotImplementedError()

    def add_all_composite_issues(self, df):
        raise NotImplementedError()

    ################################
    # Data accessors and filtering #
    ################################
    # panel contains all original data
    def get_panel(self):
        return self.panel

    # paired_waves contains multiple rows per respondent, one row per pair of waves
    def get_paired_waves(self):
        return self.paired_waves

    def filter_dummy(self, df, dummy):
        return df.loc[df[dummy] == 1,:].copy()

    def filter_under_40(self, df):
        return df.loc[df['age'] < 40,:].copy()

    def filter_demographic(self, df, label, value):
        return df.loc[df[label] == value,:].copy()

    def filter_na(self, df, label):
        return df.loc[pd.notna(df[label]),:].copy()

    ######################
    # Analysis functions #
    ######################
    def count_flippers(self, before_label, after_label, lower_bound, upper_bound):
        valid_rows = self.panel.loc[
            np.greater_equal(self.panel[before_label], lower_bound) & np.greater_equal(self.panel[after_label], lower_bound)
            &
            np.less_equal(self.panel[before_label], upper_bound) & np.less_equal(self.panel[after_label], upper_bound),
            [before_label, after_label]
        ]
        flippers = valid_rows.loc[np.not_equal(valid_rows[before_label], valid_rows[after_label]), :]
        return round(len(flippers) * 100 / len(valid_rows), 1)

    def nan_out_of_bounds(self, df, label, lower_bound=None, upper_bound=None):
        if lower_bound is None or upper_bound is None:
            return df

        df = df.assign(lower_bound=lower_bound, upper_bound=upper_bound)

        df.loc[np.logical_or(
            np.less(df[label], df.lower_bound),
            np.greater(df[label], df.upper_bound)
        ), label] = np.nan

        df = df.drop(['lower_bound', 'upper_bound'], axis=1)

        return df

    def pvalue_stars(self, pvalue):
        if pvalue < 0.001:
            return '***'
        if pvalue < 0.01:
            return '**'
        if pvalue < 0.05:
            return '*'
        return ''

    def all_t_test_pvalues(self, df, demographic_label, **test_kwargs):
        issues = list(self.ISSUES)
        issues.sort()
        all_results = pd.DataFrame(data={'issue': issues})
        for metric in self.METRICS:
            issue_results = self.t_tests(df, metric, demographic_label, **test_kwargs)
            all_results = all_results.merge(issue_results.loc[:,['issue', 'diff', 'pvalue']], on='issue')
            all_results.rename(columns={'diff': f'{metric}-', 'pvalue': f'{metric}*'}, inplace=True)
        return all_results

    # TODO: Make weighting an option, not default
    def t_tests(self, df, metric, demographic_label, a_value=0, b_value=1):
        results = {
            'metric': [],
            'diff': [],
            'statistic': [],
            'df': [],
            'pvalue': [],
            'issue': [],
        }
        for issue in self.ISSUES:
            results['issue'].append(issue)
            label = f'{issue}_{metric}'
            result = self.t_test(df, label, demographic_label, a_value, b_value)

            filtered = self.filter_na(df, label)
            a_values = filtered.loc[filtered[demographic_label] == a_value, [label, 'weight']]
            b_values = filtered.loc[filtered[demographic_label] == b_value, [label, 'weight']]
            if np.isnan(result.statistic):
                results['diff'].append(np.nan)
            else:
                results['diff'].append(round(
                    np.average(a_values[label], weights=a_values['weight'])
                    - np.average(b_values[label], weights=b_values['weight']),
                2))

            results['metric'].append(label)
            results['statistic'].append(result.statistic)
            results['df'].append(result.df)
            results['pvalue'].append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))
        df = DataFrame.from_dict(results)
        df.sort_values('metric', inplace=True)
        return df

    def t_test(self, df, issue_label, demographic_label, a_value=0, b_value=1):
        filtered = self.filter_na(self.filter_na(df, demographic_label), issue_label)
        group_a = filtered.loc[np.equal(filtered[demographic_label], a_value), ['weight', issue_label]]
        group_b = filtered.loc[np.equal(filtered[demographic_label], b_value), ['weight', issue_label]]
        if group_a.empty or group_b.empty:
            (statistic, pvalue, df) = (np.nan, np.nan, np.nan)
        else:
            (statistic, pvalue, df) = ttest_ind(group_a[issue_label], group_b[issue_label],
                                                usevar='unequal',
                                                weights=(group_a.weight, group_b.weight))
        return Result(statistic=statistic, df=df, pvalue=pvalue)

    #####################
    # Summary functions #
    #####################
    def summarize_all_issues(self, df, group_by_labels):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]
        all_issues = pd.DataFrame({k: [] for k in ['issue'] + group_by_labels + self.METRICS})
        for issue in sorted(self.ISSUES):
            issue_summary = self.summarize_issue(self.filter_na(df, f'{issue}_delta'), group_by_labels, issue)
            issue_summary['issue'] = issue
            issue_summary.rename(columns={f'{issue}_{m}': m for m in self.METRICS}, inplace=True)
            all_issues = pd.concat([all_issues, issue_summary])
        return all_issues

    def summarize_issues_non_response(self, df):
        total = len(df)
        rates = defaultdict(list)

        # Note the rates for persists are artificially high because they only apply to the 10/12 pairs, not the 12/14 pairs
        for issue in sorted(self.ISSUES):
            rates['issue'].append(issue)
            for metric in set(self.METRICS):
                label = f'{issue}_{metric}'
                if label in df:
                    missing = len(df.loc[np.isnan(df[label]),:])
                    rates[metric].append(str(round(missing * 100 / total, 2)) + '%')
                else:
                    rates[metric].append('--')

        return pd.DataFrame(rates)

    def summarize_demographics_non_response(self, df):
        total = len(df)
        rates = defaultdict(list)

        for demographic in self.demographics:
            missing = len(df.loc[np.isnan(df[demographic]),:])
            rates['demographic'].append(demographic)
            rates['rate'].append(str(round(missing * 100 / total, 2)) + '%')

        return pd.DataFrame(rates)

    def summarize_issue(self, df, group_by_labels, issue):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]

        issue_columns = [f'{issue}_{m}' for m in self.METRICS]
        return self._weighted_averages(df, group_by_labels, issue_columns)

    # For each issue column, calculate (values * weights).groupby(by).sum() / weights.groupby(by).sum()
    def _weighted_averages(self, df, group_by_labels, columns):
        if group_by_labels is None:
            group_by_labels = []
        elif type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]

        summary = df.loc[:,['weight'] + group_by_labels + columns].copy()
        for col in columns:
            summary[col] = np.multiply(summary[col], summary['weight'])
        if group_by_labels:
            summary = summary.groupby(group_by_labels, as_index=False)
        summary = summary.sum()
        for col in columns:
            summary[col] = np.divide(summary[col], summary['weight'])
        summary = summary.drop(['weight'], axis=(1 if type(summary) == pd.DataFrame else 0))
        return summary

    def summarize_all_persistence(self, treatment):
        all_issues = pd.DataFrame({k: [] for k in ['issue', treatment, 'persistence_flag', 'count', 'total', 'percent']})
        for issue in sorted(self.ISSUES):
            issue_summary = self.summarize_persistence(issue, treatment)
            issue_summary['issue'] = issue
            issue_summary.rename(columns={f'{issue}_persistence_flag': 'persistence_flag'}, inplace=True)
            all_issues = pd.concat([all_issues, issue_summary])
        all_issues = all_issues.loc[all_issues['persistence_flag'] == 1,:]
        all_issues.drop(['persistence_flag'], axis=1)
        return all_issues

    # Note this is unweighted
    def summarize_persistence(self, issue, treatment):
        flags = self.filter_na(self.paired_waves, f'{issue}_persists')
        flags[f'{issue}_persistence_flag'] = np.int32(np.bool_(flags[f'{issue}_persists']))
        flags.groupby([treatment, f'{issue}_persistence_flag']).count()
        return self.count_percentages(flags, treatment, f'{issue}_persistence_flag')

    def count_percentages(self, df, group_by_label, metric_label):
        counts = df.loc[:,['caseid', group_by_label, metric_label]].groupby([group_by_label, metric_label], as_index=False).count() # roughly pd.crosstab
        totals = self.filter_na(df, metric_label).loc[:,['caseid', group_by_label]].groupby([group_by_label], as_index=False).count()
        results = counts.merge(totals, on=group_by_label)
        results['percent'] = np.round(results['caseid_x'] * 100 / results['caseid_y'], decimals=1)
        results.rename(columns={'caseid_x': 'count', 'caseid_y': 'total'}, inplace=True)
        return results

    ######################
    # Matching functions #
    ######################
    # TODO: Make weighting an option, not default
    def get_matched_outcomes(self, df, formula, treatment, control_value=0, treatment_value=1):
        outcomes = [
            f'{issue}_{metric}' for issue in self.ISSUES for metric in set(self.METRICS) - set(['persists', 'persists_abs'])
        ]
        columns = ['caseid', treatment, 'score', 'weight'] + outcomes

        if len(df['caseid'].unique()) != len(df['caseid']):
            raise ParentsPoliticsPanelException("Data frame gives to get_matched_outcomes does not have unique cases")

        if "~" not in formula:
            formula = f"{treatment} ~ {formula}"
        df = self._add_score(df, formula)
        treatment_cases = df.loc[df[treatment] == treatment_value, columns].copy()
        candidates = df.loc[df[treatment] == control_value, columns].copy()

        # Match up treatment and control groups
        matched_set = treatment_cases.merge(candidates, on='score', how='left', suffixes=('_treatment', ''))
        match_counts = matched_set.groupby('caseid_treatment').count().loc[:,['score']]
        match_counts.rename(columns={'score':'control_count'}, inplace=True)
        unmatched = match_counts.loc[match_counts['control_count'] == 0,:]
        # TODO: error/note if any of treatment_cases didn't match: ultimately implement nearest neighbor & record distance, noting bias
        # Currently not matching 5-15% of cases with exact matching
        if len(unmatched):
            print(f"Could not match {len(unmatched)} of {len(treatment_cases)} treatment cases ({round(len(unmatched) * 100 / len(treatment_cases))}%)")

        # Group on treatment caseid, averaging all relevant control matches
        matched_outcomes = self._weighted_averages(matched_set, 'caseid_treatment', outcomes)
        matched_outcomes = matched_outcomes.drop(['caseid_treatment'], axis=1)
        agg_matched_outcomes = matched_outcomes.mean()
        agg_treatment_outcomes = self._weighted_averages(treatment_cases, None, outcomes)

        # Reduce matches to a single control row per treatment to t test
        # TODO: matched_outcomes outcomes are weighted, but treatment_cases are not
        # Is doing weighting myself for t tests legit?
        matched_outcomes[treatment] = control_value
        matched_outcomes['weight'] = 1  # Outcomes have been weighted, so set weight to 1
        reduced_df = pd.concat([treatment_cases, matched_outcomes])
        pvalues = []
        for o in outcomes:
            result = self.t_test(reduced_df, o, treatment, a_value=control_value, b_value=treatment_value)
            pvalues.append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))

        summary = pd.DataFrame(data={
            'control': agg_matched_outcomes,
            'treatment': agg_treatment_outcomes,
            'diff': round(agg_matched_outcomes - agg_treatment_outcomes, 2),
            'pvalue': pvalues,
        })
        summary.sort_index(inplace=True)
        return summary

    def _add_score(self, df, formula):
        logit = smf.glm(formula=formula,
                        family=sm.families.Binomial(),
                        data=df).fit()
        df['score'] = logit.predict(df)
        return df

    def consider_models(self, df, treatment):
        models = {}
        for choose_count in range(1, len(self.demographics) + 1):
            for chosen in list(combinations(self.demographics, choose_count)):
                formula = treatment + " ~ " + " + ".join(chosen)
                logit = smf.glm(formula=formula,
                                family=sm.families.Binomial(),
                                data=df).fit()
                df['score'] = logit.predict(df)
                unscored_count = len(df.loc[np.isnan(df['score'])])
                unscored_percentage = f'{round(unscored_count * 100 / len(df))}%'
                models[formula] = (formula, logit.pseudo_rsquared(), logit.aic, unscored_percentage, logit)

        by_r_squared = sorted(models.values(), key=lambda t: t[1]) # higher is better
        by_aic = sorted(models.values(), key=lambda t: t[2]) # lower is better

        # Note two_years returns stuff, and waves_1214, but not waves_1012
        max_models = int(len(models) * 0.05)
        decent_r_squared = by_r_squared[-max_models:]
        decent_aic = by_aic[:max_models]
        decent_formulas = list(set([x[0] for x in decent_r_squared]) & set([x[0] for x in decent_aic]))[:10]

        summary = pd.DataFrame(data={
            'formula': [re.sub(r'.*~\s*', '', f) for f in decent_formulas],
            'r_squared': [models[f][1] for f in decent_formulas],
            'aic': [models[f][2] for f in decent_formulas],
            'unscored': [models[f][3] for f in decent_formulas],
        })
        summary['aic_rank'] = summary.rank()['aic']
        summary.set_index('aic_rank', inplace=True)
        summary.sort_values('aic_rank', inplace=True)
        return summary

    def scores_histogram_table(self, df, formula, treatment):
        self._add_score(df, f"{treatment} ~ {formula}")
        scores = df.loc[:,['score', treatment]].copy()
        scores.sort_values('score', inplace=True)
        scores = scores.loc[pd.notna(scores['score']),:]
        scores = scores.assign(rounded=lambda x: round(x['score'], 1))
        return scores.groupby([treatment, 'rounded']).count()
