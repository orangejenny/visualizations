import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

from collections import defaultdict
from pandas import DataFrame
from scipy.stats import chi2_contingency, ttest_ind


class ParentsPoliticsPanel():
    CONTINUOUS_METRICS = ['before', 'after', 'delta', 'delta_abs', 'persists', 'persists_abs']
    CATEGORICAL_METRICS = ['before', 'after', 'change', 'persists']
    waves = []

    @property
    def start_waves(self):
        return self.waves[:-1]

    @property
    def end_waves(self):
        return self.waves[1:]

    def __init__(self):
        self.CONTINUOUS_ISSUES = set()
        self.CATEGORICAL_ISSUES = set()

        self.panel = self._load_panel()
        self.paired_waves = self._build_paired_waves(self._trimmed_panel())

        self.paired_waves = self._add_parenting(self.paired_waves)
        self.paired_waves = self.paired_waves.astype({'new_child': 'int32', 'firstborn': 'int32'})

        self.paired_waves = self._add_all_continuous(self.paired_waves)
        self.paired_waves = self._add_all_categorical(self.paired_waves)
        self.paired_waves = self._add_all_composite(self.paired_waves)

        # De-fragment frames
        self.paired_waves = self.paired_waves.copy()

    def _load_panel(self):
        raise NotImplementedError()

    def _trimmed_panel(self):
        raise NotImplementedError()

    def _build_paired_waves(self, df):
        raise NotImplementedError()

    def _add_parenting(self, df):
        raise NotImplementedError()

    def _add_all_continuous(self, df):
        raise NotImplementedError()

    def _add_all_categorical(self, df):
        raise NotImplementedError()

    def _add_all_composite(self, df):
        raise NotImplementedError()

    ##################
    # Data accessors #
    ##################
    # panel contains all original data
    def get_panel(self):
        return self.panel

    # paired_waves contains multiple rows per respondent, one row per pair of waves
    def get_paired_waves(self):
        return self.paired_waves

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

        df.drop(['lower_bound', 'upper_bound'], axis=1)

        return df

    def pvalue_stars(self, pvalue):
        if pvalue < 0.001:
            return '***'
        if pvalue < 0.01:
            return '**'
        if pvalue < 0.05:
            return '*'
        return ''

    def filter_na(self, df, label):
        return df.loc[pd.notna(df[label]),:].copy()

    def all_chisq_pvalues(self, df, **test_kwargs):
        return self._all_test_pvalues(df, self.CATEGORICAL_ISSUES, self.CATEGORICAL_METRICS, self.chisqs, **test_kwargs)

    def all_t_test_pvalues(self, df, **test_kwargs):
        return self._all_test_pvalues(df, self.CONTINUOUS_ISSUES, self.CONTINUOUS_METRICS, self.t_tests, **test_kwargs)

    def _all_test_pvalues(self, df, issues, metrics, test, **test_kwargs):
        issues = list(issues)
        issues.sort()
        all_results = pd.DataFrame(data={'issue': issues})
        for metric in metrics:
            issue_results = test(df, metric, **test_kwargs)
            all_results = all_results.merge(issue_results.loc[:,['issue', 'diff', 'pvalue']], on='issue')
            all_results.rename(columns={'diff': f'{metric}-', 'pvalue': f'{metric}*'}, inplace=True)
        return all_results

    def t_tests(self, df, metric, demographic_label='new_child', a_value=0, b_value=1):
        results = {
            'metric': [],
            'diff': [],
            'statistic': [],
            'df': [],
            'pvalue': [],
            'issue': [],
        }
        for issue in self.CONTINUOUS_ISSUES:
            label = f'{issue}_{metric}'
            result = self.t_test(df, label, demographic_label, a_value, b_value)
            results['issue'].append(issue)
            results['diff'].append(
                round(df.loc[df[demographic_label] == a_value, label].mean()
                - df.loc[df[demographic_label] == b_value, label].mean(),
            2))
            results['metric'].append(label)
            results['statistic'].append(result.statistic)
            results['df'].append(result.df)
            results['pvalue'].append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))
        df = DataFrame.from_dict(results)
        df.sort_values('metric', inplace=True)
        return df

    def t_test(self, df, issue_label, demographic_label='new_child', a_value=0, b_value=1):
        filtered = self.filter_na(self.filter_na(df, demographic_label), issue_label)
        group_a = filtered.loc[np.equal(filtered[demographic_label], a_value), issue_label]
        group_b = filtered.loc[np.equal(filtered[demographic_label], b_value), issue_label]
        return ttest_ind(group_a, group_b, equal_var=False)

    def chisqs(self, df, metric, demographic_label='new_child'):
        results = {
            'metric': [],
            'diff': [],
            'statistic': [],
            'dof': [],
            'pvalue': [],
            'issue': [],
        }
        for issue in self.CATEGORICAL_ISSUES:
            label = f'{issue}_{metric}'
            result = self.chisq(df, label, demographic_label)
            results['issue'].append(issue)
            percentages = self.count_percentages(df, demographic_label, label)
            treatment = percentages.loc[percentages[demographic_label] == 1,:]
            control = percentages.loc[percentages[demographic_label] == 0,:]
            joined = treatment.merge(control, on=label, suffixes=('_treatment', '_control'))
            joined['diff'] = round(joined['percent_treatment'] - joined['percent_control'], 1)
            results['diff'].append("/".join([str(x) for x in joined['diff'].to_list()]))
            results['metric'].append(label)
            results['statistic'].append(result.statistic)
            results['dof'].append(result.dof)
            results['pvalue'].append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))
        df = DataFrame.from_dict(results)
        df.sort_values('metric', inplace=True)
        return df

    def chisq(self, df, factor1, factor2='new_child'):
        return chi2_contingency(pd.crosstab(df[factor1], df[factor2]))

    #####################
    # Summary functions #
    #####################
    def summarize_all_continuous(self, df, group_by_labels):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]
        all_issues = pd.DataFrame({k: [] for k in ['issue'] + group_by_labels + self.CONTINUOUS_METRICS})
        for issue in sorted(self.CONTINUOUS_ISSUES):
            # TODO: also filter_na for columns in group_by_labels?
            issue_summary = self.summarize_continuous(self.filter_na(df, f'{issue}_delta'), group_by_labels, issue)
            issue_summary['issue'] = issue
            issue_summary.rename(columns={f'{issue}_{m}': m for m in self.CONTINUOUS_METRICS}, inplace=True)
            all_issues = pd.concat([all_issues, issue_summary])
        return all_issues

    def summarize_non_response(self, df):
        total = len(df)
        rates = defaultdict(list)
        # Note the rates for persists are artificially high because they only apply to the 10/12 pairs, not the 12/14 pairs
        for issue in sorted(self.CONTINUOUS_ISSUES | self.CATEGORICAL_ISSUES):
            rates['issue'].append(issue)
            for metric in set(self.CONTINUOUS_METRICS) | set(self.CATEGORICAL_METRICS):
                label = f'{issue}_{metric}'
                if label in df:
                    missing = len(df.loc[np.isnan(df[label]),:])
                    rates[metric].append(str(round(missing * 100 / total, 2)) + '%')
                else:
                    rates[metric].append('--')
        return pd.DataFrame(rates)

    def summarize_continuous(self, df, group_by_labels, issue):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]
        return df.loc[
            :,
            group_by_labels + [f'{issue}_{m}' for m in self.CONTINUOUS_METRICS]
        ].groupby(group_by_labels, as_index=False).mean()

    def continuous_persists(self, issue):
        flags = self.filter_na(self.paired_waves, f'{issue}_persists')
        flags[f'{issue}_persistence_flag'] = np.bool_(flags[f'{issue}_persists'])
        flags.groupby(['new_child', f'{issue}_persistence_flag']).count()
        return self.count_percentages(flags, 'new_child', f'{issue}_persistence_flag')

    def categorical_persists(self, issue):
        flags = self.filter_na(self.paired_waves, f'{issue}_persists')
        flags[f'{issue}_persistence_flag'] = np.bool_(flags[f'{issue}_persists'])
        flags.groupby(['new_child', f'{issue}_persistence_flag']).count()
        return self.count_percentages(self.filter_na(self.paired_waves, f'{issue}_persists'), 'new_child', f'{issue}_persists')

    def summarize_all_categorical(self, df, group_by_label, metric):
        all_issues = pd.DataFrame({k: [] for k in ['issue', group_by_label, metric, 'count', 'total', 'percent']})
        for issue in sorted(self.CATEGORICAL_ISSUES):
            issue_summary = self.count_percentages(df, group_by_label, f'{issue}_{metric}')
            issue_summary['issue'] = issue
            issue_summary.rename(columns={f'{issue}_{metric}': metric}, inplace=True)
            all_issues = pd.concat([all_issues, issue_summary])
        return all_issues

    def count_percentages(self, df, group_by_label, metric_label):
        counts = df.loc[:,['caseid', group_by_label, metric_label]].groupby([group_by_label, metric_label], as_index=False).count() # roughly pd.crosstab
        # TODO: also filter_na for group_by_label?
        totals = self.filter_na(df, metric_label).loc[:,['caseid', group_by_label]].groupby([group_by_label], as_index=False).count()
        results = counts.merge(totals, on=group_by_label)
        results['percent'] = np.round(results['caseid_x'] * 100 / results['caseid_y'], decimals=1)
        results.rename(columns={'caseid_x': 'count', 'caseid_y': 'total'}, inplace=True)
        return results

    ######################
    # Matching functions #
    ######################
    def get_matched_outcomes(self, df, formula):
        # TODO: add tests
        outcomes = [
            f'{issue}_{metric}' for issue in self.CATEGORICAL_ISSUES for metric in set(self.CATEGORICAL_METRICS) - set(['persists'])
        ] + [
            f'{issue}_{metric}' for issue in self.CONTINUOUS_ISSUES for metric in set(self.CONTINUOUS_METRICS) - set(['persists', 'persists_abs'])
        ]
        columns = ['caseid', 'new_child', 'score'] + outcomes    # TODO: add weight, and verify all mean calls are handling missing data appropriately
        df = self._add_score(df, formula)
        new_parents = df.loc[df['new_child'] == 1, columns].copy()  # TODO: use parenthood status instead of new parenthood?
        candidates = df.loc[df['new_child'] == 0, columns].copy()

        # Match up treatment and control groups
        # TODO: error/note if any of new_parents didn't match: ultimately implement nearest neighbor & record distance, noting bias
        matched_set = new_parents.merge(candidates, on='score', how='left', suffixes=('_treatment', ''))

        # Group on treatment caseid, averaging all relevant control matches
        # TODO: why does this frame have fewer rows than new_parents? Did they not all match?
        matched_outcomes = matched_set.loc[:, ['caseid_treatment'] + outcomes].groupby('caseid_treatment').mean()
        agg_matched_outcomes = matched_outcomes.mean()
        agg_treatment_outcomes = new_parents.loc[:, outcomes].mean()

        # Reduce matches to a single control row per treatment to t test
        reduced_matches = matched_set.groupby('caseid_treatment').mean().loc[:,columns]
        reduced_df = pd.concat([new_parents, reduced_matches])
        pvalues = []
        for o in outcomes:
            result = self.t_test(reduced_df, o)
            pvalues.append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))

        return pd.DataFrame(data={
            'control': agg_matched_outcomes,
            'treatment': agg_treatment_outcomes,
            'diff': round(agg_matched_outcomes - agg_treatment_outcomes, 2),
            'pvalue': pvalues,
        })

    def _add_score(self, df, formula):
        logit = smf.glm(formula="new_child ~ " + formula,
                        family=sm.families.Binomial(),
                        data=df).fit()
        df['score'] = logit.predict(df)
        return df
