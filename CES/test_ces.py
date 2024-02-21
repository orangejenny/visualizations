# python -m unittest test_ces.py

import numpy as np
import unittest

from ces import CESData

class TestCESData(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.data = CESData()

    def test_parenting_counts(self):
        counts = self.data.get_paired_waves().groupby('new_child', as_index=False).count()
        self.assertListEqual(counts.loc[:, 'caseid'].to_list(), [18580, 420])

        counts = self.data.get_all_waves().groupby('new_child', as_index=False).count()
        self.assertListEqual(counts.loc[:, 'caseid'].to_list(), [9271, 229])

    def test_flippers(self):
        self.assertEqual(25.2, self.data.count_flippers("pid7_10", "pid7_14", 1, 7))
        self.assertEqual(24.9, self.data.count_flippers("ideo5_10", "ideo5_12", 1, 5))

    def _test_pvalue(self, expected, result):
        self.assertEqual(expected, round(result.pvalue, 4))

    def test_t_test(self):
        def _test_t_test(expected, data, label, demographic='new_child'):
            self._test_pvalue(expected, self.data.t_test(data, label, demographic))

        data = self.data.get_paired_waves()
        _test_t_test(0.4108, data, 'ideo_delta')
        _test_t_test(0.7221, data, 'ideo_composite_delta')
        _test_t_test(0.4348, data, 'pid_delta')

        young_adults = data.loc[np.less(data['age'], 30),:]
        _test_t_test(0.6028, young_adults, 'ideo_delta_abs')
        _test_t_test(0.3203, young_adults, 'ideo_composite_delta_abs')

        _test_t_test(0.9979, data, 'climate_composite_after')
        _test_t_test(0.787, data, 'immigration_composite_delta')
        _test_t_test(0.5486, data, 'military_composite_delta')
        _test_t_test(0.4092, data, 'jobs_env_delta', 'firstborn')
        _test_t_test(0.1119, data, 'jobs_env_delta_abs', 'firstborn')

    def test_chisq(self):
        def _test_chiqsq(expected, data, label, demographic='new_child'):
            self._test_pvalue(expected, self.data.chisq(data, label, demographic))

        paired_waves = self.data.get_paired_waves()
        _test_chiqsq(0.8664, paired_waves, 'ideo_direction')
        _test_chiqsq(0.3215, paired_waves, 'pid_direction')

        data = paired_waves.loc[np.equal(paired_waves['new_child'], 1),:]
        _test_chiqsq(0.517, data, 'ideo_direction', 'high_income')
        _test_chiqsq(0.5566, data, 'pid_direction', 'high_income')
        _test_chiqsq(0.595, data, 'ideo_direction', 'gender')
        _test_chiqsq(0.1408, data, 'pid_direction', 'gender')

        data = paired_waves.loc[np.equal(paired_waves['gender'], 1),:]
        _test_chiqsq(0.8416, data, 'ideo_direction')
        _test_chiqsq(0.2836, data, 'pid_direction')

        data = paired_waves.loc[np.equal(paired_waves['gender'], 2),:]
        _test_chiqsq(0.7833, data, 'ideo_direction')
        _test_chiqsq(0.1103, data, 'pid_direction')
        _test_chiqsq(0.7833, data, 'ideo_direction')
        _test_chiqsq(0.1103, data, 'pid_direction')

        #self.assertListEqual(counts['caseid_x'].to_list(), [160,  42, 122,  86])
        #self.assertListEqual(counts['percent'].to_list(), [79.2, 20.8, 58.7, 41.3])

    def test_summarize_continuous(self):
        data = self.data.get_paired_waves()
        guns = self.data.summarize_continuous(data, "new_child", "guns")
        self.assertListEqual([round(v, 2) for v in guns.iloc[1, 1:].values], [1.78, 1.72, -0.06, 0.29])
        military = self.data.summarize_continuous(data, "new_child", "military_composite")
        self.assertListEqual([round(v, 2) for v in military.iloc[1, 1:].values], [1.46, 1.49, 0.02, 0.16])

    def test_count_percentages(self):
        data = self.data.get_paired_waves()

        after_counts = self.data.count_percentages(data, 'new_child', 'gay_marriage_after')
        self.assertListEqual(after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].to_list(), [39.4, 60.6])

        after_counts = self.data.count_percentages(data, 'new_child', 'budget_avoid_after')
        self.assertListEqual(after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].to_list(), [20, 35.4, 44.6])

        data = data.loc[np.equal(data['new_child'], 1),:]
        change_counts = self.data.count_percentages(data, "gender", "budget_change")
        self.assertListEqual(change_counts['caseid_x'].to_list(), [160,  42, 122,  86])
        self.assertListEqual(change_counts['percent'].to_list(), [79.2, 20.8, 58.7, 41.3])
