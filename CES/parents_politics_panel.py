import numpy as np
import pandas as pd

from pandas import DataFrame
from scipy.stats import chi2_contingency, ttest_ind


class ParentsPoliticsPanel():
    waves = []

    @property
    def start_waves(self):
        return self.waves[:-1]

    @property
    def end_waves(self):
        return self.waves[1:]

    def __init__(self):
        self.CONTINUOUS_PREFIXES = set()
        self.CATEGORICAL_PREFIXES = set()

        self.panel = self._load_panel()
        self.all_waves = self.panel.copy()

        self.all_waves = self._trimmed_panel()
        if not len(self.waves):
            raise Exception("Must contain at least one wave")
        self.all_waves = self.all_waves.assign(start_wave=self.waves[0], end_wave=self.waves[-1])

        self.paired_waves = self._build_paired_waves(self.all_waves)

        self.all_waves = self._add_parenting(self.all_waves)
        self.all_waves = self.all_waves.astype({'new_child': 'int32', 'firstborn': 'int32'})
        self.paired_waves = self._add_parenting(self.paired_waves)
        self.paired_waves = self.paired_waves.astype({'new_child': 'int32', 'firstborn': 'int32'})

        self.all_waves = self._add_all_continuous(self.all_waves)
        self.paired_waves = self._add_all_continuous(self.paired_waves)

        self.all_waves = self._add_all_categorical(self.all_waves)
        self.paired_waves = self._add_all_categorical(self.paired_waves)

        self.all_waves = self._add_all_composite(self.all_waves)
        self.paired_waves = self._add_all_composite(self.paired_waves)

        # De-fragment frames
        self.all_waves = self.all_waves.copy()
        self.paired_waves = self.paired_waves.copy()

    def _load_panel(self):
        raise NotImplementedError()

    def _trimmed_panel(self):
        raise NotImplementedError()

    def _build_paired_waves(self, all_waves):
        raise NotImplementedError()

    def _add_parenting(self, df):
        raise NotImplementedError()

    def _add_all_continuous(self, df):
        raise NotImplementedError()

    def _add_all_categorical(self, df):
        raise NotImplementedError()

    def _add_all_composite(self, df):
        raise NotImplementedError()

    ### Data accessors
    # panel contains all original data
    def get_panel(self):
        return self.panel

    # paired_waves contains multiple rows per respondent, one row per pair of waves
    def get_paired_waves(self):
        return self.paired_waves

    ### Analysis functions
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

    def t_test(self, df, issue_label, demographic_label='new_child', a_value=0, b_value=1):
        filtered = self.filter_na(self.filter_na(df, demographic_label), issue_label)
        group_a = filtered.loc[np.equal(filtered[demographic_label], a_value), issue_label]
        group_b = filtered.loc[np.equal(filtered[demographic_label], b_value), issue_label]
        return ttest_ind(group_a, group_b, equal_var=False)

    def filter_na(self, df, label):
        return df.loc[pd.notna(df[label]),:].copy()

    def all_chisq_pvalues(self, df, **test_kwargs):
        metrics = ['before', 'after', 'change', 'persists']
        return self._all_test_pvalues(df, self.CATEGORICAL_PREFIXES, metrics, self.chisqs, **test_kwargs)

    def all_t_test_pvalues(self, df, **test_kwargs):
        metrics = ['before', 'after', 'delta', 'delta_abs', 'persists', 'persists_abs']
        return self._all_test_pvalues(df, self.CONTINUOUS_PREFIXES, metrics, self.t_tests, **test_kwargs)

    def _all_test_pvalues(self, df, issues, metrics, test, **test_kwargs):
        issues = list(issues)
        issues.sort()
        all_results = pd.DataFrame(data={'issue': issues})
        for metric in metrics:
            issue_results = test(df, metric, **test_kwargs)
            all_results = all_results.merge(issue_results.loc[:,['issue', 'pvalue']], on='issue')
            all_results.rename(columns={'pvalue': metric}, inplace=True)
        return all_results

    def t_tests(self, df, issue_suffix, demographic_label='new_child', a_value=0, b_value=1):
        results = {
            'metric': [],
            'statistic': [],
            'df': [],
            'pvalue': [],
            'issue': [],
        }
        for prefix, label in [(p, f'{p}_{issue_suffix}') for p in self.CONTINUOUS_PREFIXES]:
            result = self.t_test(df, label, demographic_label, a_value, b_value)
            results['issue'].append(prefix)
            results['metric'].append(label)
            results['statistic'].append(result.statistic)
            results['df'].append(result.df)
            results['pvalue'].append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))
        df = DataFrame.from_dict(results)
        df.sort_values('metric', inplace=True)
        return df

    def chisq(self, df, factor1, factor2='new_child'):
        return chi2_contingency(pd.crosstab(df[factor1], df[factor2]))

    def chisqs(self, df, issue_suffix, demographic_label='new_child'):
        results = {
            'metric': [],
            'statistic': [],
            'dof': [],
            'pvalue': [],
            'issue': [],
        }
        for prefix, label in [(p, f'{p}_{issue_suffix}') for p in self.CATEGORICAL_PREFIXES]:
            result = self.chisq(df, label, demographic_label)
            results['issue'].append(prefix)
            results['metric'].append(label)
            results['statistic'].append(result.statistic)
            results['dof'].append(result.dof)
            results['pvalue'].append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))
        df = DataFrame.from_dict(results)
        df.sort_values('metric', inplace=True)
        return df

    ### Summary functions
    def summarize_continuous(self, df, group_by_labels, issue):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]
        return df.loc[
            :,
            group_by_labels + [f'{issue}_before', f'{issue}_after', f'{issue}_delta', f'{issue}_delta_abs']
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

    def count_percentages(self, df, group_by_label, metric_label):
        counts = df.loc[:,['caseid', group_by_label, metric_label]].groupby([group_by_label, metric_label], as_index=False).count() # roughly pd.crosstab
        totals = self.filter_na(df, metric_label).loc[:,['caseid', group_by_label]].groupby([group_by_label], as_index=False).count()
        results = counts.merge(totals, on=group_by_label)
        results['percent'] = np.round(results['caseid_x'] * 100 / results['caseid_y'], decimals=1)
        return results
