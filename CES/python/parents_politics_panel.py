import csv
import numpy as np
import os
import pandas as pd
import re
import statsmodels.api as sm
import statsmodels.formula.api as smf

from collections import defaultdict, namedtuple
from enum import Enum
from datetime import datetime
from itertools import combinations
from pandas import DataFrame
from scipy.stats import zscore
from statsmodels.stats.weightstats import ttest_ind


DemographicType = Enum('DemographicType', ['CONTINUOUS', 'CATEGORICAL', 'ORDERED_CATEGORICAL'])

Demographic = namedtuple('Demographic', ['name', 'dtype', 'upper_bound', 'lower_bound', 'top_categories'])
Result = namedtuple('Result', ['statistic', 'df', 'pvalue'])
ComparatorKey = namedtuple('ComparatorKey', ['issue', 'metric', 'treatment', 'demo_desc'])
ComparatorValue = namedtuple('ComparatorValue', ['diff', 'norm', 'pvalue'])


class ParentsPoliticsPanelException(Exception):
    pass


class ParentsPoliticsApproachComparator():
    MATCH = 'match'
    PANEL = 'panel'
    approaches = {
        PANEL: 'delta',
        MATCH: 'after',
    }

    def __init__(self, ppp):
        self.data = {a: {} for a in self.approaches}
        self.smallest_n = {}
        self.ppp = ppp
        self.comparison = None
        self.core_comparison = None

    def add(self, approach, outcome, treatment, substance, pvalue, demo_desc=None):
        assert approach in self.approaches.keys(), f"{approach} is not an approach"

        # Matching doesn't use new_child
        if treatment == 'new_child':
            return

        (issue, metric) = self.ppp.parse_outcome(outcome)
        normalized_substance = self.ppp.normalize_diff(issue, substance)
        key = ComparatorKey(issue, metric, treatment, demo_desc)
        self.data[approach][key] = ComparatorValue(substance, normalized_substance, pvalue)

    def set_smallest_n(self, approach, outcome, treatment, smallest_n, demo_desc=None):
        (issue, metric) = self.ppp.parse_outcome(outcome)
        key = ComparatorKey(issue, metric, treatment, demo_desc)
        self.smallest_n[key] = smallest_n

    def get_comparison(self):
        if self.comparison is None:
            keys = {key for value in self.data.values() for key in value.keys()}
            data = {
                'issue': [k.issue for k in keys],
                'metric': [k.metric for k in keys],
                'treatment': [k.treatment for k in keys],
                'demographic': [k.demo_desc if k.demo_desc else "--" for k in keys],
                'smallest_n': [self.smallest_n.get(k, '?') for k in keys],
            }
            blank_value = ComparatorValue("--", "--", "--")
            for approach in self.approaches.keys():
                data.update({f'{approach}-': [self.data[approach].get(key, blank_value).diff for key in keys]})
            for approach in self.approaches.keys():
                data.update({f'{approach}%': [self.data[approach].get(key, blank_value).norm for key in keys]})
            for approach in self.approaches.keys():
                data.update({f'{approach}*': [self.data[approach].get(key, blank_value).pvalue for key in keys]})
            for approach in self.approaches.keys():
                data.update({f'{approach}*_level': [self._star_count(self.data[approach].get(key, blank_value).pvalue) for key in keys]})
            self.comparison = pd.DataFrame(data=data)
            self.comparison.sort_values(['demographic', 'issue', 'treatment', 'metric'], inplace=True)

        return self.comparison

    # Pairs the "core" metrics: the after value for matching and the delta for panel analysis
    # Also limit to overall sample (no subset), young adults, and either firstborn/is_parent
    def get_core_comparison(self):
        if self.core_comparison is None:
            full = self.get_comparison()
            core = full.loc[
                np.logical_or(full['treatment'] == 'firstborn', full['treatment'] == 'is_parent')
            ,:].copy()

            key_columns = ['issue', 'treatment', 'demographic']
            adata = {}
            acols = {}
            for approach, core_metric in self.approaches.items():
                acols[approach] = [f'{approach}-', f'{approach}%', f'{approach}*', f'{approach}*_level']
                adata[approach] = core.loc[core['metric'] == core_metric, key_columns + ['smallest_n'] + acols[approach]].copy()

            # Just access the approaches by name, since merge only supports two
            recombined = pd.merge(adata['panel'], adata['match'], on=key_columns, suffixes=('', '_match'))
            self.core_comparison = recombined.loc[:,key_columns + ['smallest_n'] + [val for pair in zip(acols['match'], acols['panel']) for val in pair]]

        return self.core_comparison

    def _star_count(self, string):
        return len(re.sub(r'[^*]', "", string))

    def filter(self, substance_threshold, pvalue_threshold, smallest_n_threshold=None):
        return self._filter(self.get_comparison(), substance_threshold, pvalue_threshold, smallest_n_threshold)

    def filter_core(self, substance_threshold, pvalue_threshold, smallest_n_threshold=None):
        return self._filter(self.get_core_comparison(), substance_threshold, pvalue_threshold, smallest_n_threshold)

    def _filter(self, matrix, substance_threshold, pvalue_threshold, smallest_n_threshold=None):
        matrix = matrix.loc[np.logical_and(
            np.logical_and(matrix['match*_level'] >= pvalue_threshold, matrix['panel*_level'] >= pvalue_threshold),
            np.logical_and(np.abs(matrix['match%']) >= substance_threshold, np.abs(matrix['panel%']) >= substance_threshold)
        )].copy()
        if smallest_n_threshold:
            matrix = matrix.loc[matrix['smallest_n'] >= smallest_n_threshold,:]
        return matrix


class ParentsPoliticsPanel():
    OUTPUT_DIR = 'output'
    OUTPUT_FILES = ['significant.log', 'all.log', 'two_stars.log', 'three_stars.log', 'substantive.log', 'paper.log']

    METRICS = ['before', 'after', 'delta', 'delta_abs', 'persists', 'persists_abs']
    waves = []

    _demographics = []
    _demographic_viz_boundaries = {}
    _demographic_viz_labels = {}
    _demographic_category_names = {}
    _issue_viz_labels = {}

    @property
    def demographics(self):
        return {d.name: d for d in self._demographics}

    def _parse_viz_demographic(self, dname):
        category = None
        match = re.match(r'^(\w+)_([1-9])$', dname)
        if match:
            dname = match.group(1)
            category = match.group(2)
        return (dname, category)

    def demographic_viz_boundaries(self, dname):
        (dname, category) = self._parse_viz_demographic(dname)
        if dname in self._demographic_viz_boundaries:
            return self._demographic_viz_boundaries[dname]
        elif category:
            return (0, 100)
        demo = self.demographics[dname]
        return (demo.lower_bound, demo.upper_bound)

    def demographic_viz_label(self, dname):
        (dname, category) = self._parse_viz_demographic(dname)
        label = self._demographic_viz_labels.get(dname, dname.title())
        if category is not None:
            label += ': % ' + self._demographic_category_names.get(dname, {}).get(int(category), '?')
        return label

    def issue_viz_label(self, issue, pvalue=None):
        label = self._issue_viz_labels.get(issue, issue.replace('_composite', '').title())
        if pvalue is not None:
            label = label + re.sub(r'[^*]', '', pvalue)
        return label

    @property
    def start_waves(self):
        return self.waves[:-1]

    @property
    def end_waves(self):
        return self.waves[1:]

    @property
    def VIZ_DIR(self):
        return os.path.join(self.OUTPUT_DIR, 'viz')

    def __init__(self, output_suffix='', no_output=False):
        self.ISSUES = set()
        self.ISSUE_BOUNDS = {}

        self.no_output = no_output
        if not self.no_output:
            if output_suffix:
                self.OUTPUT_DIR = f'{self.OUTPUT_DIR}_{output_suffix}'
            if not os.path.isdir(self.OUTPUT_DIR):
                print("Making directory " + self.OUTPUT_DIR)
                os.mkdir(self.OUTPUT_DIR)
            if not os.path.isdir(self.VIZ_DIR):
                print("Making directory " + self.VIZ_DIR)
                os.mkdir(self.VIZ_DIR)
            self._truncate_output()

        self.panel = self._load_panel()
        self.paired_waves = self.build_paired_waves(self._trimmed_panel())

        self.paired_waves = self.add_parenting(self.paired_waves)
        self.paired_waves = self.paired_waves.astype({t: 'int32' for t in self.treatments})

        self.paired_waves = self._add_all_single_issues(self.paired_waves)
        self.paired_waves = self.add_all_composite_issues(self.paired_waves)

        # De-fragment frames
        self.paired_waves = self.paired_waves.copy()

        # Compare findings from different approaches
        self.comparator = ParentsPoliticsApproachComparator(self)

    def add_parenting(self, df):
        df = self.add_parenthood_indicator(df)

        # TODO: this removes ~10k rows in YouGov, why?
        df = df.loc[pd.notna(df['parenthood']),:].copy() # remove any rows where parenthood cannot be determined

        '''
        Additional boolean columns based on parenthood
        - childless: 0     ...recall this is only about minor children, these people may have adult children (but also recall the age < 40 cutoff)
        - firstborn: 1
        - new_sibling: 2
        - new_child: 1 or 2
        - steady_parent: 3
        - is_parent: 1, 2, or 3
        '''
        df = df.assign(**{
            'childless': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 0, 1, 0) for w in self.start_waves],
            ),
            'firstborn': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 1, 1, 0) for w in self.start_waves],
            ),
            'new_sibling': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 2, 1, 0) for w in self.start_waves],
            ),
            'new_child': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_or(x.parenthood == 1, x.parenthood == 2) , 1, 0) for w in self.start_waves],
            ),
            'steady_parent': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 3, 1, 0) for w in self.start_waves],
            ),
            'is_parent': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood != 0, 1, 0) for w in self.start_waves],
            ),
        })

        df = self.add_parenting_dosage_indicators(df)

        return df

    # Add standardized columns for a particular issue
    def add_issue(self, df, before_pattern, issue, lower_bound=None, upper_bound=None, calc_only=False):
        df = self._add_before_after(df, before_pattern, issue, lower_bound, upper_bound)

        df = df.assign(**{
            f'{issue}_delta': lambda x: x[f'{issue}_after'] - x[f'{issue}_before'],
            f'{issue}_delta_abs': lambda x: abs(x[f'{issue}_delta']),
            f'{issue}_delta_sq': lambda x: x[f'{issue}_delta'] * x[f'{issue}_delta'],
            f'{issue}_direction': lambda x: np.sign(x[f'{issue}_delta']),
        })
        df.loc[np.isnan(df[f'{issue}_delta']), f'{issue}_direction'] = np.nan # because some of the 0s should be NaN

        df = df.assign(**{
            f'{issue}_persists': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves[:-1]],
                [np.where(np.logical_and(
                    x[f'{issue}_delta'] != 0, # change in start vs end
                    # change from start to final is either zero or the same direction as delta
                    x[f'{issue}_delta'] * (x[before_pattern.replace('XX', str(self.end_waves[-1]))] - x[before_pattern.replace('XX', str(self.end_waves[i]))]) >= 0
                ),
                x[before_pattern.replace('XX', str(self.end_waves[-1]))] - x[before_pattern.replace('XX', str(w))],
                0) for i, w in enumerate(self.start_waves[:-1])]
            )
        })
        for wave in self.waves:
            df.loc[df['start_wave'] == self.start_waves[-1], f'{issue}_persists'] = np.nan  # Can't calculate when there are only two waves
            df.loc[np.isnan(df[before_pattern.replace('XX', str(wave))]), f'{issue}_persists'] = np.nan  # Can't calculate unless all waves are available
        df[f'{issue}_persists_abs'] = np.abs(df[f'{issue}_persists'])

        if not calc_only:
            self.ISSUES.add(issue)
            self.ISSUE_BOUNDS[issue] = (lower_bound, upper_bound)

        return df

    # Helper for add_issue
    def _add_before_after(self, df, before_pattern, issue, lower_bound=None, upper_bound=None):
        df = df.assign(**{
            f'{issue}_before': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [x[before_pattern.replace('XX', str(w))] for w in self.start_waves],
            ),
            f'{issue}_after': lambda x:np.select(
                [x.start_wave == w for w in self.waves[:-1]],
                [x[before_pattern.replace('XX', str(w))] for w in self.end_waves],
            ),
        })
        df = self.nan_out_of_bounds(df, f'{issue}_before', lower_bound, upper_bound)
        df = self.nan_out_of_bounds(df, f'{issue}_after', lower_bound, upper_bound)
        return df


    def get_approach_comparison(self, matrix=None):
        return self.comparator.get_comparison()

    def filter_approach_comparison(self, substance_threshold, pvalue_threshold, smallest_n_threshold=None):
        return self.comparator.filter(substance_threshold, pvalue_threshold, smallest_n_threshold)

    def get_core_approach_comparison(self, matrix=None):
        return self.comparator.get_core_comparison()

    def filter_core_approach_comparison(self, substance_threshold, pvalue_threshold, smallest_n_threshold=None):
        return self.comparator.filter_core(substance_threshold, pvalue_threshold, smallest_n_threshold)

    #####################
    # Logging functions #
    #####################
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

    def log_for_paper(self, data, description=''):
        self._output('paper.log', data, description)

    def _log_viz_data(self, filename, headers, rows):
        filename = os.path.join(self.VIZ_DIR, f'{filename}.csv')
        with open(filename, 'w', newline='') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow(headers)
            for row in rows:
                writer.writerow(row)

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
        if self.no_output:
            return
        if type(data) == pd.DataFrame:
            data = data.to_string(max_rows=100)
        else:
            data = str(data)
        if description:
            data = "\n" + description + "\n" + data
        with open(os.path.join(self.OUTPUT_DIR, filename), 'a') as fh:
            fh.write(data + "\n")

    def parse_outcome(self, outcome):
        for m in self.METRICS:
            if outcome.endswith(m):
                return (outcome.replace(f"_{m}", ""), m)
        raise ParentsPoliticsPanelException(f"Could not parse outcome {outcome}")

    def normalize_diff(self, issue, amount):
        (lower_bound, upper_bound) = self.ISSUE_BOUNDS[issue]
        range_size = upper_bound - lower_bound
        return round(amount * 100 / range_size, 1)

    def normalize_issue(self, issue, value):
        (lower_bound, upper_bound) = self.ISSUE_BOUNDS[issue]
        return self.normalize_value(value, lower_bound, upper_bound) / 10

    def normalize_value(self, value, lower_bound, upper_bound):
        range_size = upper_bound - lower_bound
        value -= lower_bound
        value = round(value * 100 / range_size, 1)
        return value

    def log_matching(self, matching_output, description='', viz_filename=None):
        (outcomes, covariates, messages) = matching_output
        if messages:
            description = description + "\n" + "\n".join(messages)

        self.log_findings(outcomes, description)

        paper_outcomes = defaultdict(list)
        for index, row in outcomes.iterrows():
            (issue, metric) = self.parse_outcome(index)
            if metric == "after":
                paper_outcomes['issue'].append(issue)
                paper_outcomes['control'].append(round(row['control'], 2))
                paper_outcomes['treatment'].append(round(row['treatment'], 2))
                paper_outcomes['diff'].append(round(row['diff'], 2))
                paper_outcomes['norm'].append(self.normalize_diff(issue, row['diff']))
                paper_outcomes['pvalue'].append(row['pvalue'])

        paper_outcomes = pd.DataFrame(paper_outcomes)
        self.log_for_paper(paper_outcomes, description)

        if viz_filename:
            # Viz for results
            headers = ['issue', 'value', 'is_matched']
            csv_rows = []
            for row in paper_outcomes.itertuples():
                csv_rows.append([self.issue_viz_label(row.issue, row.pvalue), self.normalize_issue(row.issue, row.treatment), "parents"])
                csv_rows.append([self.issue_viz_label(row.issue, row.pvalue), self.normalize_issue(row.issue, row.control), "matched non-parents"])
            self._log_viz_data(viz_filename, headers, csv_rows)

            # Viz for covariates
            self.log_for_paper(covariates, "Covariate means")
            headers = ['demographic', 'value', 'is_matched']
            csv_rows = []
            groups = covariates['group'].to_list()
            for label in covariates.columns[1:]:
                values = covariates[label].to_list()
                for group, value, in zip(groups, values):
                    (lower_bound, upper_bound) = self.demographic_viz_boundaries(label)
                    csv_rows.append([
                        self.demographic_viz_label(label),
                        self.normalize_value(value, lower_bound, upper_bound),
                        group
                    ])
            self._log_viz_data('covariates_' + viz_filename, headers, csv_rows)

    def log_panel(self, issues_and_messages, description='', viz_filename=None):
        (issues, messages) = issues_and_messages
        if messages:
            description = description + "\n" + "\n".join(messages)

        self.log_findings(issues, description)

        paper_issues = defaultdict(list)
        for index, row in issues.iterrows():
            issue = row['issue']
            paper_issues['issue'].append(issue)
            for metric in ['before', 'after', 'delta', 'persists']:
                paper_issues[f'{metric}_a'].append(row[f'{metric}_a'])
                paper_issues[f'{metric}_b'].append(row[f'{metric}_b'])
                paper_issues[f'{metric}-'].append(row[f'{metric}-'])
                paper_issues[f'{metric}*'].append(row[f'{metric}*'])
                paper_issues[f'{metric}%'].append(self.normalize_diff(issue, row[f'{metric}-']))

        paper_issues = pd.DataFrame(paper_issues)
        self.log_for_paper(paper_issues, description)

        if viz_filename:
            paper_issues_labels = {label: i for i, label in enumerate(paper_issues.columns)}
            cols = ['issue', 'before_a', 'after_a', 'before_b', 'after_b']
            renames = {
                'before_a': 'control_before',
                'after_a': 'control_after',
                'before_b': 'treatment_before',
                'after_b': 'treatment_after',
            }
            headers = [renames.get(c, c) for c in cols]
            pvalue_label = f'_{paper_issues_labels["delta*"] + 1}'
            csv_rows = []
            for row in paper_issues.itertuples():
                csv_rows.append([self.issue_viz_label(row.issue, getattr(row, pvalue_label))] + [
                    self.normalize_issue(row.issue, getattr(row, c))
                    for c in cols[1:]
                ])
            self._log_viz_data(viz_filename, headers, csv_rows)

    def _load_panel(self):
        raise NotImplementedError()

    def _trimmed_panel(self):
        raise NotImplementedError()

    def build_paired_waves(self, df):
        '''
        This duplicates the dataset so there's a set of rows for each pair of waves.
        With three waves, this means doubling the length of the data.
        '''
        if not len(self.waves):
            raise Exception("Must contain at least one wave")
        df = df.assign(start_wave=self.waves[0], end_wave=self.waves[-1])
        df = self._recode_issues(df)
        df = self._recode_demographics(df)
        df = self._add_income_brackets(df)
        df = self.add_geography(df)

        df = pd.concat([
            df.assign(
                start_wave=w,
                end_wave=self.end_waves[i],
            ) for i, w in enumerate(self.start_waves)
        ], ignore_index=True)
        df = self._add_age(df)   # age depends on start_wave
        df = self._consolidate_demographics(df)

        return df

    def add_parenthood_indicator(self, df):
        '''
        - 0 no children
        - 1 new first child (same as firstborn)
        - 2 new additional child
        - 3 parent, no change in number of children
        '''
        raise NotImplementedError()

    def get_divisions(self):
        # https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
        states = [
            ['CT', 'ME', 'MA', 'NH', 'RI', 'VT'],
            ['NJ', 'NY', 'PA'],
            ['IN', 'IL', 'MI', 'OH', 'WI'],
            ['IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD'],
            ['DE', 'DC', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'WV'],
            ['AL', 'KY', 'MS', 'TN'],
            ['AR', 'LA', 'OK', 'TX'],
            ['AZ', 'CO', 'ID', 'NM', 'MY', 'UT', 'NV', 'WY'],
            ['AK', 'CA', 'HI', 'OR', 'WA'],
        ]
        divisions = pd.DataFrame(data=[(a, i + 1) for i, abbreviations in enumerate(states) for a in abbreviations])
        divisions.rename(columns={0: 'state', 1: 'division'}, inplace=True)
        return divisions

    def _add_income_brackets(self, df):
        # Income brackets are approximate, since incomes are given in ranges.
        df = self.add_income_quintile(df)
        df = df.assign(
            # "High income" is top 20% to match Reeves
            high_income=lambda x: np.where(np.isnan(x.income_quintile), np.NAN, np.where(x.income_quintile == 5, 1, 0)),
            # "Low income" is bottom 40%, to very roughly correspond with SCHIP eligibility
            low_income=lambda x: np.select(
                [
                    np.isnan(x.income_quintile),
                    x.income_quintile == 1,
                    x.income_quintile == 2,
                    x.income_quintile == 3,
                    x.income_quintile == 4,
                    x.income_quintile == 5,
                ],
                [np.NAN, 1, 1, 0, 0, 0],
                default=np.nan
            ),
        )
        return df

    def add_income_quintile(self, df):
        raise NotImplementedError()

    def _add_age(self, df):
        df = df.assign(age=lambda x: 2000 + x.start_wave - x[self.dob_column])
        df = self.nan_out_of_bounds(df, 'age', 1, 200)
        df['age_zscore'] = zscore(df['age'], nan_policy='omit')
        df = df.loc[np.less_equal(df['age'], 40),:]
        return df.copy()

    def _consolidate_demographics(self, df):
        for dname, demographic in self.demographics.items():
            if demographic.lower_bound is None and demographic.upper_bound is None:
                continue

            old_labels = [f'{dname}_{wave}' for wave in self.waves if f'{dname}_{wave}' in df]
            for old_label in old_labels:
                df = self.nan_out_of_bounds(df, old_label, demographic.lower_bound, demographic.upper_bound)

            # Use "after" data if available, else use most recent value
            if len(old_labels) == 1:
                # Needed when there isn't data for any of the end waves, as in YouGov
                # TODO: Test YouGov
                df = df.assign(**{dname: old_labels[0]})
            else:
                df = df.assign(**{dname: lambda x: np.select(
                    [x.end_wave == w for w in self.end_waves if f'{dname}_{w}' in df],
                    [np.where(
                        pd.notna(x[f'{dname}_{w}']),
                        x[f'{dname}_{w}'],
                        x[old_labels].bfill(axis=1).iloc[:, 0]
                    ) for w in self.end_waves if f'{dname}_{w}' in df],
                )})
            df.drop(old_labels, axis=1, inplace=True)
        return df

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

    def all_t_test_pvalues(self, df, demographic_label, comparator_treatment=None, comparator_desc=None, **test_kwargs):
        messages = []
        a_value = test_kwargs.get('a_value', 0)
        b_value = test_kwargs.get('b_value', 1)
        a_count = len(self.filter_demographic(df, demographic_label, a_value))
        b_count = len(self.filter_demographic(df, demographic_label, b_value))
        verified = a_count + b_count == len(df)
        messages.append(f"{demographic_label}={a_value}, n={a_count}; {demographic_label}={b_value}, n={b_count}; reasonable={verified}")

        issues = list(self.ISSUES)
        issues.sort()
        all_results = pd.DataFrame(data={'issue': issues})
        for metric in self.METRICS:
            issue_results = self.t_tests(df, metric, demographic_label,
                                         comparator_treatment=comparator_treatment, comparator_desc=comparator_desc, **test_kwargs)
            all_results = all_results.merge(issue_results.loc[:,['issue', 'a', 'b', 'diff', 'pvalue']], on='issue')
            all_results.rename(columns={'diff': f'{metric}-', 'pvalue': f'{metric}*', 'a': f'{metric}_a', 'b': f'{metric}_b'}, inplace=True)

        return (all_results, messages)

    def t_tests(self, df, metric, demographic_label, a_value=0, b_value=1, do_weight=True,
                comparator_treatment=None, comparator_desc=None):
        results = {
            'metric': [],
            'a': [],
            'b': [],
            'diff': [],
            'statistic': [],
            'df': [],
            'pvalue': [],
            'issue': [],
        }
        for issue in self.ISSUES:
            results['issue'].append(issue)
            label = f'{issue}_{metric}'
            result = self.t_test(df, label, demographic_label, a_value, b_value, do_weight=do_weight)

            filtered = self.filter_na(df, label)
            a_values = filtered.loc[filtered[demographic_label] == a_value, [label, 'weight']]
            b_values = filtered.loc[filtered[demographic_label] == b_value, [label, 'weight']]
            if np.isnan(result.statistic):
                results['a'].append(np.nan)
                results['b'].append(np.nan)
                results['diff'].append(np.nan)
            else:
                weights = (a_values['weight'], b_values['weight']) if do_weight else (None, None)
                results['a'].append(round(np.average(a_values[label], weights=weights[0]), 3))
                results['b'].append(round(np.average(b_values[label], weights=weights[1]), 3))
                results['diff'].append(results['a'][-1] - results['b'][-1])

            results['metric'].append(label)
            results['statistic'].append(result.statistic)
            results['df'].append(result.df)
            results['pvalue'].append(str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue))

            if 'persist' not in metric:
                self.comparator.set_smallest_n(self.comparator.PANEL, label, comparator_treatment or demographic_label,
                                               min([len(a_values), len(b_values)]), demo_desc=comparator_desc)
                self.comparator.add(self.comparator.PANEL, label, comparator_treatment or demographic_label,
                                    results['diff'][-1], results['pvalue'][-1], demo_desc=comparator_desc)

        df = DataFrame.from_dict(results)
        df.sort_values('metric', inplace=True)
        return df

    def t_test(self, df, issue_label, demographic_label, a_value=0, b_value=1, do_weight=True):
        filtered = self.filter_na(self.filter_na(df, demographic_label), issue_label)
        group_a = filtered.loc[np.equal(filtered[demographic_label], a_value), ['weight', issue_label]]
        group_b = filtered.loc[np.equal(filtered[demographic_label], b_value), ['weight', issue_label]]
        if group_a.empty or group_b.empty:
            (statistic, pvalue, df) = (np.nan, np.nan, np.nan)
        elif np.var(group_a[issue_label]) == 0 or np.var(group_b[issue_label]) == 0:  # can happen in very small groups
           (statistic, pvalue, df) = (np.nan, np.nan, np.nan)
        else:
            (statistic, pvalue, df) = ttest_ind(group_a[issue_label], group_b[issue_label],
                                                usevar='unequal',
                                                weights=(group_a.weight, group_b.weight) if do_weight else (None, None))
        return Result(statistic=statistic, df=df, pvalue=pvalue)

    #####################
    # Summary functions #
    #####################
    def summarize_all_issues(self, df, group_by_labels, do_weight=True):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]
        all_issues = pd.DataFrame({k: [] for k in ['issue'] + group_by_labels + self.METRICS})
        for issue in sorted(self.ISSUES):
            issue_summary = self.summarize_issue(self.filter_na(df, f'{issue}_delta'), group_by_labels, issue, do_weight=do_weight)
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

        for demographic in self.demographics.keys():
            missing = len(df.loc[np.isnan(df[demographic]),:])
            rates['demographic'].append(demographic)
            rates['rate'].append(str(round(missing * 100 / total, 2)) + '%')

        return pd.DataFrame(rates)

    def summarize_issue(self, df, group_by_labels, issue, do_weight=True):
        if type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]

        issue_columns = [f'{issue}_{m}' for m in self.METRICS]
        return self._weighted_averages(df, group_by_labels, issue_columns, do_weight=do_weight)

    # For each issue column, calculate (values * weights).groupby(by).sum() / weights.groupby(by).sum()
    def _weighted_averages(self, df, group_by_labels, columns, do_weight=True):
        if group_by_labels is None:
            group_by_labels = []
        elif type(group_by_labels) == type(''):
            group_by_labels = [group_by_labels]

        # Calculate each outcome separately, because they may have NAs and in that case we need to skip the corresponding weight
        summary = None
        for col in columns:
            col_data = df.loc[:,['weight', col] + group_by_labels].copy()
            col_data = self.filter_na(col_data, col)
            if not do_weight:
                col_data['weight'] = 1
            col_data[col] = np.multiply(col_data[col], col_data['weight'])
            if group_by_labels:
                col_data = col_data.groupby(group_by_labels, as_index=False)
            col_data = col_data.sum()
            col_data[col] = round(np.divide(col_data[col], col_data['weight']), 2)
            col_data = col_data.drop(['weight'], axis=(1 if type(col_data) == pd.DataFrame else 0))
            if summary is None:
                summary = col_data
            elif group_by_labels:
                summary = pd.merge(summary, col_data, left_index=True, right_index=True, suffixes=('', '_drop'))
                summary.drop([f'{l}_drop' for l in group_by_labels], axis=1, inplace=True)
            else:
                summary = pd.concat([summary, col_data], axis=0)

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
    def get_matched_outcomes(self, df, treatment, score_label=None, control_value=0, treatment_value=1, do_weight=True,
                             comparator_treatment=None, comparator_desc=None, covariates_for_viz=None):
        outcomes = [
            f'{issue}_{metric}' for issue in self.ISSUES for metric in set(self.METRICS) - set(['persists', 'persists_abs'])
        ]
        if score_label is None:
            score_label = f'{treatment}_score'
        if covariates_for_viz is None:
            covariates_for_viz = []
        messages = []

        if len(df['caseid'].unique()) != len(df['caseid']):
            raise ParentsPoliticsPanelException("Data frame given to get_matched_outcomes does not have unique cases")

        columns = ['caseid', treatment, score_label, f'{score_label}_copy', 'weight'] + outcomes
        columns = columns + covariates_for_viz
        treatment_cases = df.loc[df[treatment] == treatment_value, columns].copy()
        candidates = df.loc[df[treatment] == control_value, columns].copy()

        expanded_covariates = []
        for covariate in covariates_for_viz:
            demo = self.demographics[covariate]
            if demo.dtype == DemographicType.CATEGORICAL:
                if demo.top_categories:
                    expanded_covariates.extend([f'{covariate}_{c}' for c in reversed(demo.top_categories)])
            else:
                expanded_covariates.append(covariate)
        covariate_means = {k: [] for k in ['group'] + expanded_covariates}
        if covariates_for_viz:
            covariate_means = self.add_covariate_means(treatment_cases, covariate_means, covariates_for_viz, 'parents')
            covariate_means = self.add_covariate_means(candidates, covariate_means, covariates_for_viz, 'all non-parents')

        # Match up treatment and control groups
        treatment_cases.sort_values(score_label, inplace=True)
        candidates.sort_values(score_label, inplace=True)

        before_counts = (len(treatment_cases), len(candidates))
        treatment_cases = self.filter_na(treatment_cases, score_label)
        candidates = self.filter_na(candidates, score_label)
        after_counts = (len(treatment_cases), len(candidates))
        if any(np.subtract(before_counts, after_counts)):
            treatment_percent = round((before_counts[0] - after_counts[0]) * 100 / before_counts[0], 1) if before_counts[0] != after_counts[0] else 0
            candidate_percent = round((before_counts[1] - after_counts[1]) * 100 / before_counts[1], 1) if before_counts[1] != after_counts[1] else 0
            messages.append(f"Lost {treatment_percent}% of treatment cases and {candidate_percent}% of control cases due to missing score")

        tolerances = {
            'is_parent': 0.10,
            'firstborn': 0.04,
        }
        control_cases = pd.merge_asof(treatment_cases, candidates, on=score_label, suffixes=('_treatment', ''), tolerance=tolerances[comparator_treatment or treatment], direction='nearest')
        control_cases = self.filter_na(control_cases, 'caseid')
        if covariates_for_viz:
            covariate_means = self.add_covariate_means(treatment_cases, covariate_means, covariates_for_viz, 'matched non-parents')
            covariate_means = pd.DataFrame(covariate_means)
        treatment_cases.drop([col for col in covariates_for_viz], axis=1, inplace=True)
        control_cases.drop([col for col in covariates_for_viz], axis=1, inplace=True)

        if len(control_cases) < len(treatment_cases):
            percent = round((len(treatment_cases) - len(control_cases)) * 100 / len(treatment_cases), 1)
            messages.append(f"Lost {percent}% of cases ({len(treatment_cases) - len(control_cases)} cases) due to matching tolerance, leaving {len(control_cases)}")

            # Filter out treatment cases that weren't matched
            treatment_cases = pd.merge(treatment_cases, control_cases['caseid_treatment'], how='inner', left_on='caseid', right_on='caseid_treatment')
        messages.append(f"Final n: {len(control_cases)} control, {len(treatment_cases)} treatment cases")

        control_cases['score_diff'] = control_cases[f'{score_label}_copy'] - control_cases[f'{score_label}_copy_treatment']
        messages.append(f"Max score difference: {round(max(control_cases['score_diff']), 4)}")

        # Calculate average treatment effect for control & treatment groups
        agg_matched_outcomes = self._weighted_averages(control_cases, None, outcomes, do_weight=do_weight)
        agg_treatment_outcomes = self._weighted_averages(treatment_cases, None, outcomes, do_weight=do_weight)

        # Calculate difference in treatment effect for each outcome, and run t test
        control_cases[treatment] = control_value
        reduced_df = pd.concat([treatment_cases, control_cases])
        diffs = []
        pvalues = []
        for o in outcomes:
            diff = round(agg_matched_outcomes[o] - agg_treatment_outcomes[o], 2)
            diffs.append(diff)
            result = self.t_test(reduced_df, o, treatment, a_value=control_value, b_value=treatment_value, do_weight=do_weight)
            pvalue = str(round(result.pvalue, 4)) + self.pvalue_stars(result.pvalue)
            pvalues.append(pvalue)
            self.comparator.add(self.comparator.MATCH, o, comparator_treatment or treatment, diff, pvalue, demo_desc=comparator_desc)

        summary = pd.DataFrame(data={
            'control': agg_matched_outcomes,
            'treatment': agg_treatment_outcomes,
            'diff': diffs,
            'pvalue': pvalues,
        })
        summary.sort_index(inplace=True)
        return (summary, covariate_means, messages)

    def add_covariate_means(self, df, results, covariates, label):
        results['group'].append(label)
        for covariate in covariates:
            demo = self.demographics[covariate]
            if demo.dtype == DemographicType.CATEGORICAL:
                if demo.top_categories:
                    counts = df.groupby(covariate, as_index=False).count()
                    total = counts['caseid'].sum()
                    for category in demo.top_categories:
                        count = counts.loc[counts[covariate] == category, ['caseid']].iloc[0, 0]
                        prop = count * 100 / total
                        results[f'{covariate}_{category}'].append(round(prop, 2))
            else:
                mean = df[covariate].mean()
                results[covariate].append(round(mean, 2))
        return results

    def add_score(self, df, formula, label='score', do_weight=True):
        logit = smf.glm(formula=formula,
                        family=sm.families.Binomial(),
                        data=df,
                        freq_weights=(df['weight'] if do_weight else None)).fit()
        df[label] = logit.predict(df)
        df[f'{label}_copy'] = df[label]
        return df

    def consider_models(self, df, treatment, do_weight=True):
        models = {}
        for choose_count in range(1, len(self.demographics) + 1):
            print(f"Trying formulas with {choose_count} options")
            combos = list(combinations([d for d in self.demographics.keys()], choose_count))
            for index, chosen in enumerate(combos):
                print(f"Trying {index} of {len(combos)}: {chosen}")
                formula = treatment + " ~ " + " + ".join([
                    f'C({c})' if self.demographics[c].dtype == DemographicType.CATEGORICAL else c for c in chosen
                ])
                logit = smf.glm(formula=formula,
                                family=sm.families.Binomial(),
                                data=df,
                                freq_weights=(df['weight'] if do_weight else None)).fit()
                df['score'] = logit.predict(df)
                unscored_count = len(df.loc[np.isnan(df['score'])])
                unscored_percentage = f'{round(unscored_count * 100 / len(df))}%'
                models[formula] = (formula, logit.pseudo_rsquared(), logit.aic, unscored_percentage, logit)

        by_r_squared = sorted(models.values(), key=lambda t: t[1]) # higher is better
        by_aic = sorted(models.values(), key=lambda t: t[2]) # lower is better

        max_models = 20  #int(len(models) * 0.5)
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

    def scores_histogram_table(self, df, formula, treatment, weight_score=True):
        self.add_score(df, f"{treatment} ~ {formula}", do_weight=weight_score)
        scores = df.loc[:,['score', treatment]].copy()
        scores.sort_values('score', inplace=True)
        scores = scores.loc[pd.notna(scores['score']),:]
        scores = scores.assign(rounded=lambda x: round(x['score'], 1))
        return scores.groupby([treatment, 'rounded']).count()
