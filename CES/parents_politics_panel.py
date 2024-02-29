import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

from collections import defaultdict, namedtuple
from itertools import combinations
from pandas import DataFrame
from statsmodels.stats.weightstats import ttest_ind

Result = namedtuple('Result', ['statistic', 'df', 'pvalue'])


class ParentsPoliticsPanel():
    METRICS = ['before', 'after', 'delta', 'delta_abs', 'persists', 'persists_abs']
    waves = []

    @property
    def start_waves(self):
        return self.waves[:-1]

    @property
    def end_waves(self):
        return self.waves[1:]

    def __init__(self):
        self.ISSUES = set()

        self.panel = self._load_panel()
        self.paired_waves = self._build_paired_waves(self._trimmed_panel())

        self.paired_waves = self._add_parenting(self.paired_waves)
        self.paired_waves = self.paired_waves.astype({'new_child': 'int32', 'firstborn': 'int32'})

        self.paired_waves = self._add_all_single_issues(self.paired_waves)
        self.paired_waves = self._add_all_composite_issues(self.paired_waves)

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

    def _add_all_single_issues(self, df):
        raise NotImplementedError()

    def _add_all_composite_issues(self, df):
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

    def filter_na(self, df, label):
        return df.loc[pd.notna(df[label]),:].copy()

    def all_t_test_pvalues(self, df, **test_kwargs):
        return self._all_test_pvalues(df, self.ISSUES, self.METRICS, self.t_tests, **test_kwargs)

    def _all_test_pvalues(self, df, issues, metrics, test, **test_kwargs):
        issues = list(issues)
        issues.sort()
        all_results = pd.DataFrame(data={'issue': issues})
        for metric in metrics:
            issue_results = test(df, metric, **test_kwargs)
            all_results = all_results.merge(issue_results.loc[:,['issue', 'diff', 'pvalue']], on='issue')
            all_results.rename(columns={'diff': f'{metric}-', 'pvalue': f'{metric}*'}, inplace=True)
        return all_results

    # TODO: Make weighting an option, not default
    def t_tests(self, df, metric, demographic_label='new_child', a_value=0, b_value=1):
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

    def t_test(self, df, issue_label, demographic_label='new_child', a_value=0, b_value=1):
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
            # TODO: also filter_na for columns in group_by_labels?
            issue_summary = self.summarize_issue(self.filter_na(df, f'{issue}_delta'), group_by_labels, issue)
            issue_summary['issue'] = issue
            issue_summary.rename(columns={f'{issue}_{m}': m for m in self.METRICS}, inplace=True)
            all_issues = pd.concat([all_issues, issue_summary])
        return all_issues

    def summarize_non_response(self, df):
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

    def summarize_persistence(self, issue):
        flags = self.filter_na(self.paired_waves, f'{issue}_persists')
        flags[f'{issue}_persistence_flag'] = np.bool_(flags[f'{issue}_persists'])
        flags.groupby(['new_child', f'{issue}_persistence_flag']).count()
        return self.count_percentages(flags, 'new_child', f'{issue}_persistence_flag')

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
    # TODO: Make weighting an option, not default
    def get_matched_outcomes(self, df, formula):
        # TODO: add tests
        outcomes = [
            f'{issue}_{metric}' for issue in self.ISSUES for metric in set(self.METRICS) - set(['persists', 'persists_abs'])
        ]
        columns = ['caseid', 'new_child', 'score', 'weight'] + outcomes
        df = self._add_score(df, formula)
        new_parents = df.loc[df['new_child'] == 1, columns].copy()  # TODO: use parenthood status instead of new parenthood?
        candidates = df.loc[df['new_child'] == 0, columns].copy()

        # Match up treatment and control groups
        # TODO: error/note if any of new_parents didn't match: ultimately implement nearest neighbor & record distance, noting bias
        matched_set = new_parents.merge(candidates, on='score', how='left', suffixes=('_treatment', ''))

        # Group on treatment caseid, averaging all relevant control matches
        # TODO: why does this frame have fewer rows than new_parents? Did they not all match?
        matched_outcomes = self._weighted_averages(matched_set, 'caseid_treatment', outcomes)
        matched_outcomes = matched_outcomes.drop(['caseid_treatment'], axis=1)
        agg_matched_outcomes = matched_outcomes.mean()
        agg_treatment_outcomes = self._weighted_averages(new_parents, None, outcomes)

        # Reduce matches to a single control row per treatment to t test
        # TODO: matched_outcomes outcomes are weighted, but new_parents are not
        # Is doing weighting myself for t tests legit?
        matched_outcomes['new_child'] = 0
        matched_outcomes['weight'] = 1  # Outcomes have been weighted, so set weight to 1
        reduced_df = pd.concat([new_parents, matched_outcomes])
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

    def consider_models(self, df):
        '''
        TODO
        - Remove investor? It's just a yes/no
        - income_quintile instead of income?
        - Combine pew_religimp with pew_churatd and/or pew_prayer?
        - Add employment status and maybe home ownership
        - Urban/rural? Need to cross-reference zip code with some other dataset.
        - Z-score for age?
        '''
        demographics = ['gender', 'race', 'investor', 'educ', 'marstat', 'age', 'pew_religimp', 'income']
        models = {}
        for choose_count in range(1, len(demographics) + 1):
            for chosen in list(combinations(demographics, choose_count)):
                formula = "new_child ~ " + " + ".join(chosen)  # TODO: other parenting statuses
                logit = smf.glm(formula=formula,
                                family=sm.families.Binomial(),
                                data=df).fit()
                models[formula] = (formula, logit.pseudo_rsquared(), logit.aic, logit)

        by_r_squared = sorted(models.values(), key=lambda t: t[1]) # higher is better
        by_aic = sorted(models.values(), key=lambda t: t[2]) # lower is better

        max_models = int(len(models) * 0.05)
        decent_r_squared = by_r_squared[-max_models:]
        decent_aic = by_aic[:max_models]
        decent_formulas = set([x[0] for x in decent_r_squared]) & set([x[0] for x in decent_aic])
        return {
            k: v for k, v in models.items()
            if k in decent_formulas
        }
