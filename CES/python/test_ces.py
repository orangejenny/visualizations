# python -m unittest test_ces.py

import numpy as np
import pandas as pd
import unittest

from ces import CESPanel


class TestCESPanel(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.data = CESPanel()

    def test_start_waves(self):
        self.assertListEqual(self.data.start_waves, [10, 12])

    def test_end_waves(self):
        self.assertListEqual(self.data.end_waves, [12, 14])

    def test_pvalue_stars(self):
        self.assertEqual(self.data.pvalue_stars(0.11), "")
        self.assertEqual(self.data.pvalue_stars(0.045), "*")
        self.assertEqual(self.data.pvalue_stars(0.008), "**")
        self.assertEqual(self.data.pvalue_stars(0.0009), "***")

    def test_nan_out_of_bounds(self):
        df = pd.DataFrame(data={'one_digit': [1, 4, 34, -2, 9, 15], 'two_digit': [5, 10, 12, 3, 40, 100]})

        updated = self.data.nan_out_of_bounds(df, 'one_digit', 1, 9)
        self.assertEqual(len(df), 6)
        self.assertEqual(len(updated), 6)
        self.assertTrue(all(np.equal(df['one_digit'], [1, 4, 34, -2, 9, 15])))
        self.assertTrue(all(np.equal(updated['two_digit'], [5, 10, 12, 3, 40, 100])))
        self.assertEqual(updated['one_digit'][0], 1)
        self.assertEqual(updated['one_digit'][1], 4)
        self.assertTrue(np.isnan(updated['one_digit'][2]))
        self.assertTrue(np.isnan(updated['one_digit'][3]))
        self.assertEqual(updated['one_digit'][4], 9)
        self.assertTrue(np.isnan(updated['one_digit'][5]))
        self.assertEqual(updated['two_digit'][0], 5)
        self.assertEqual(updated['two_digit'][1], 10)

        updated = self.data.nan_out_of_bounds(updated, 'two_digit', 10, 99)
        self.assertEqual(updated['one_digit'][1], 4)
        self.assertTrue(np.isnan(updated['one_digit'][2]))
        self.assertTrue(np.isnan(updated['two_digit'][0]))
        self.assertEqual(updated['two_digit'][1], 10)
        self.assertEqual(updated['two_digit'][2], 12)
        self.assertTrue(np.isnan(updated['two_digit'][3]))
        self.assertEqual(updated['two_digit'][4], 40)
        self.assertTrue(np.isnan(updated['two_digit'][5]))

    def test_filter_na(self):
        df = pd.DataFrame(data={'one': [1, np.nan, np.nan, 4], 'two': [8, 7, np.nan, 5], 'three': [5, 3, 2, 1]})
        filter_one = self.data.filter_na(df, 'one')
        filter_three = self.data.filter_na(df, 'three')

        self.assertEqual(len(df), 4)
        self.assertEqual(len(filter_one), 2)
        self.assertEqual(len(filter_three), 4)

        self.assertTrue(all(np.equal(df['three'], [5, 3, 2, 1])))
        self.assertTrue(all(np.equal(filter_one['one'], [1, 4])))
        self.assertTrue(all(np.equal(filter_one['two'], [8, 5])))
        self.assertEqual(filter_three['one'][0], 1)
        self.assertTrue(np.isnan(filter_three['one'][1]))
        self.assertTrue(np.isnan(filter_three['one'][2]))
        self.assertEqual(filter_three['one'][3], 4)

    def test_add_age(self):
        df = pd.DataFrame(data={
            'birthyr_10': [1990, 9999, 1978],
            'start_wave': [10, 10, 12],
        })
        df = self.data.add_age(df)
        self.assertEqual(len(df), 3)
        self.assertEqual(df['age'][0], 20)
        self.assertTrue(np.isnan(df['age'][1]))
        self.assertEqual(df['age'][2], 34)
        self.assertEqual(df['age_zscore'][0], -1)
        self.assertTrue(np.isnan(df['age_zscore'][1]))
        self.assertEqual(df['age_zscore'][2], 1)

    def test_add_income_brackets(self):
        df = pd.DataFrame(data={'faminc_14': [75, 1, 8, 2, 14]})
        df = self.data.add_income_brackets(df)
        self.assertEqual(len(df), 5)
        self.assertTrue(np.isnan(df['income'][0]))
        self.assertTrue(np.isnan(df['income_quintile'][0]))
        self.assertTrue(np.isnan(df['high_income'][0]))
        self.assertTrue(np.isnan(df['low_income'][0]))
        self.assertListEqual(df['income'][1:].to_list(), [1, 8, 2, 14])
        self.assertListEqual(df['income_quintile'][1:].to_list(), [1, 4, 1, 5])
        self.assertListEqual(df['high_income'][1:].to_list(), [0, 0, 0, 1])
        self.assertListEqual(df['low_income'][1:].to_list(), [1, 0, 1, 0])

    def test_add_before_after(self):
        df = pd.DataFrame(data={
            'start_wave': [10, 10, 12],
            'end_wave': [12, 12, 14],
            'abc10': [3, 5, 2],
            'abc12': [1, 3, 4],
            'abc14': [5, 3, 5],
            'def10': [4, 2, 2],
            'def12': [2, 1, 3],
            'def14': [1, 4, 1],
        })

        df = self.data.add_before_after(df, 'abcXX', 'gov_approval')
        self.assertListEqual(df['gov_approval_before'].to_list(), [3, 5, 4])
        self.assertListEqual(df['gov_approval_after'].to_list(), [1, 3, 5])

        df = self.data.add_before_after(df, 'defXX', 'senate_approval', 1, 3)
        self.assertTrue(np.isnan(df['senate_approval_before'][0]))
        self.assertListEqual(df['senate_approval_before'][1:].to_list(), [2, 3])
        self.assertListEqual(df['senate_approval_after'].to_list(), [2, 1, 1])

    # TODO: add tests
    #def _weighted_averages(self, df, group_by_labels, columns):
    #def _recode_issues(self, df): incorporate this into other tests, including the composites
    #def _consolidate_demographics(self, df):
    #def _add_parenting(self, df):
    #def _add_issue(self, df, before_pattern, issue, lower_bound=None, upper_bound=None):
    #def get_matched_outcomes(self, df, formula, treatment):
    #def summarize_issues_non_response(self, df):
    #def summarize_demographics_non_response(self, df):
    #def _limit_to_significant(self, data, level=1):
    #def _limit_to_substantive(self, data, threshold=0.1):
    # TODO: add tests for functions added in this week's PRs

    def test_add_budget_composite(self):
        df = pd.DataFrame(data={
            'CC10_415r': [100, 0, 50, 80, 25, 10, 60, 30, 0],
            'CC10_330B': [1, 2, 1, 2, 1, 2, 1, 1, 1],
            'CC10_328': [1, 1, 1, 2, 2, 2, 3, 3, 3],
            'CC10_329': [1, 2, 3, 1, 2, 3, 1, 2, 3],
        })
        df = self.data.add_budget_composite(df, 10)
        self.assertListEqual([round(x, 2) for x in df['budget_composite_2010'].to_list()], [
            1.5, 1.33, 1.33, 1.93, 1.25, 1.7, 1.37, 1.1, 1.17
        ])

    def test_add_climate_composite(self):
        df = pd.DataFrame(data={
            'CC10_321': [1, 3, 5],
            'CC10_325': [2, 1, 4],
            'CC10_330C': [1, 2, 1],
        })
        df = self.data.add_climate_composite(df, 10)
        self.assertListEqual([round(x, 2) for x in df['climate_composite_2010'].to_list()], [1.33, 3, 3.33])

    def test_add_gay_composite(self):
        #df[f'gay_composite_20{year}'] = (df[f'CC{year}_326'] + df[f'CC{year}_330G']) / 2
        df = pd.DataFrame(data={
            'CC10_326': [1, 2, 1],
            'CC10_330G': [2, 1, 3],
        })
        df = self.data.add_gay_composite(df, 10)
        self.assertListEqual([round(x, 2) for x in df['gay_composite_2010'].to_list()[:-1]], [1.5, 1.5])
        self.assertTrue(np.isnan(df['gay_composite_2010'][2]))

    def test_add_military_composite(self):
        df = pd.DataFrame(data={
            'CC10_414_1': [1, 2],
            'CC10_414_2': [1, 1],
            'CC10_414_3': [1, 1],
            'CC10_414_4': [1, 1],
            'CC10_414_5': [1, 2],
            'CC10_414_6': [1, 2],
            'CC10_414_7': [1, 1],
            'CC12_414_7': [1, 1],
        })
        df = self.data.add_military_composite(df, 10)
        self.assertListEqual([round(x, 2) for x in df['military_composite_2010'].to_list()], [1, 1.43])

    def test_add_ideo_composite(self):
        df = pd.DataFrame(data={
            'ideo5_10': [5, 1, 2, 3, 4],
            'pid7_10': [7, 1, 2, 4, 5],
        })
        df = self.data.add_ideo_composite(df, 10)
        self.assertListEqual([round(x, 2) for x in df['ideo_composite_2010'].to_list()], [5, 0.86, 1.71, 2.93, 3.79])

    def test_add_immigration_composite(self):
        df = pd.DataFrame(data={
            'CC10_322_1': [1, 2], 'CC10_322_2': [2, 1], 'CC10_322_3': [1, 1],
            'CC12_322_1': [1, 2], 'CC12_322_2': [1, 1], 'CC12_322_3': [1, 1],
            'CC12_322_4': [2, 2], 'CC12_322_5': [2, 2], 'CC12_322_6': [1, 2], 'CC12_322_8': [1, 1],
            'CC14_322_1': [2, 1], 'CC14_322_2': [2, 1], 'CC14_322_3': [2, 2],
            'CC14_322_4': [2, 2], 'CC14_322_5': [1, 2], 'CC14_322_6': [1, 1], 'CC14_322_7': [1, 1],
        })
        df = self.data.add_immigration_composite(df)
        self.assertListEqual([round(x, 2) for x in df['immigration_composite_2010'].to_list()], [1.33, 1.33])
        self.assertListEqual([round(x, 2) for x in df['immigration_composite_2012'].to_list()], [1.33, 1.67])
        self.assertListEqual([round(x, 2) for x in df['immigration_composite_2014'].to_list()], [1.67, 1.5])

    def test_parenting_counts(self):
        counts = self.data.get_paired_waves().groupby('new_child', as_index=False).count()
        self.assertListEqual(counts.loc[:, 'caseid'].to_list(), [18721, 214])

    def test_flippers(self):
        self.assertEqual(25.2, self.data.count_flippers("pid7_10", "pid7_14", 1, 7))
        self.assertEqual(24.9, self.data.count_flippers("ideo5_10", "ideo5_12", 1, 5))

    def _test_pvalue(self, expected, result):
        self.assertEqual(expected, round(result.pvalue, 4))

    def test_t_test(self):
        def _test_t_test(expected, data, label, demographic):
            self._test_pvalue(expected, self.data.t_test(data, label, demographic))

        data = self.data.get_paired_waves()
        _test_t_test(0.6841, data, 'ideo_delta', 'new_child')
        _test_t_test(0.0417, data, 'ideo_composite_delta', 'new_child')
        _test_t_test(0.0943, data, 'pid_delta', 'new_child')

        young_adults = data.loc[np.less(data['age'], 30),:]
        _test_t_test(0.0015, young_adults, 'pid_delta', 'new_child')
        _test_t_test(0.7775, young_adults, 'military_composite_delta_abs', 'new_child')

        _test_t_test(0.2216, data, 'climate_composite_after', 'new_child')
        _test_t_test(0.0213, data, 'immigration_composite_delta', 'new_child')
        _test_t_test(0.835, data, 'military_composite_delta', 'new_child')
        _test_t_test(0.8794, data, 'budget_composite_delta', 'firstborn')
        _test_t_test(0.136, data, 'budget_composite_delta_abs', 'firstborn')

    def test_t_tests(self):
        results = self.data.t_tests(self.data.get_paired_waves(), 'delta', 'new_child')

        budget_composite = {
            k: list(v.values())[0]  # v will have a single value
            for k, v in results.loc[results['issue'] == 'budget_composite',:].to_dict().items()
        }
        self.assertEqual(budget_composite['issue'], 'budget_composite')
        self.assertEqual(budget_composite['metric'], 'budget_composite_delta')
        self.assertEqual(round(float(budget_composite['statistic']), 3), 0.152)
        self.assertEqual(round(float(budget_composite['df']), 1), 334.0)
        self.assertEqual(round(float(budget_composite['pvalue']), 4), 0.8794)

        pid = {
            k: list(v.values())[0]
            for k, v in results.loc[results['issue'] == 'pid',:].to_dict().items()
        }
        self.assertEqual(pid['issue'], 'pid')
        self.assertEqual(pid['metric'], 'pid_delta')
        self.assertEqual(round(float(pid['statistic']), 3), -1.678)
        self.assertEqual(round(float(pid['df']), 1), 314.3)
        self.assertEqual(round(float(pid['pvalue']), 4), 0.0943)

    def test_summarize_issue(self):
        data = self.data.get_paired_waves()
        guns = self.data.summarize_issue(data, "new_child", "guns")
        self.assertListEqual([round(v, 2) for v in guns.iloc[1, 1:].values], [1.66, 1.56, -0.05, 0.32, -0.06, 0.11])
        military = self.data.summarize_issue(data, "new_child", "military_composite")
        self.assertListEqual([round(v, 2) for v in military.iloc[1, 1:].values], [1.48, 1.50, 0.020, 0.16, 0.05, 0.06])

    def test_count_percentages(self):
        data = self.data.get_paired_waves()

        counts = self.data.count_percentages(data, 'new_child', 'gender')
        self.assertListEqual(counts.loc[np.equal(counts['new_child'], True), 'percent'].to_list(), [49.1, 50.9])
        self.assertListEqual(counts.loc[np.equal(counts['new_child'], False), 'percent'].to_list(), [55.6, 44.4])
