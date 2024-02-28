# python -m unittest test_ces.py

import numpy as np
import unittest

from ces import CESPanel
from yougov import YouGovPanel

class TestCESPanel(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.data = CESPanel()

    def test_parenting_counts(self):
        counts = self.data.get_paired_waves().groupby('new_child', as_index=False).count()
        self.assertListEqual(counts.loc[:, 'caseid'].to_list(), [18499, 436])

    def test_flippers(self):
        self.assertEqual(25.2, self.data.count_flippers("pid7_10", "pid7_14", 1, 7))
        self.assertEqual(24.9, self.data.count_flippers("ideo5_10", "ideo5_12", 1, 5))

    def _test_pvalue(self, expected, result):
        self.assertEqual(expected, round(result.pvalue, 4))

    def test_t_test(self):
        def _test_t_test(expected, data, label, demographic='new_child'):
            self._test_pvalue(expected, self.data.t_test(data, label, demographic))

        data = self.data.get_paired_waves()
        _test_t_test(0.0113, data, 'ideo_delta')
        _test_t_test(0.368, data, 'ideo_composite_delta')
        _test_t_test(0.8079, data, 'pid_delta')

        young_adults = data.loc[np.less(data['age'], 30),:]
        _test_t_test(0.8714, young_adults, 'pid_delta')
        _test_t_test(0.0451, young_adults, 'military_composite_delta_abs')

        _test_t_test(0.0114, data, 'climate_composite_after')
        _test_t_test(0.1355, data, 'immigration_composite_delta')
        _test_t_test(0.0035, data, 'military_composite_delta')
        _test_t_test(0.6504, data, 'jobs_env_delta', 'firstborn')
        _test_t_test(0.5963, data, 'jobs_env_delta_abs', 'firstborn')

    def test_t_tests(self):
        results = self.data.t_tests(self.data.get_paired_waves(), 'delta')

        jobs_env = {
            k: list(v.values())[0]  # v will have a single value
            for k, v in results.loc[results['issue'] == 'jobs_env',:].to_dict().items()
        }
        self.assertEqual(jobs_env['issue'], 'jobs_env')
        self.assertEqual(jobs_env['metric'], 'jobs_env_delta')
        self.assertEqual(round(float(jobs_env['statistic']), 3), -1.051)
        self.assertEqual(round(float(jobs_env['df']), 1), 759.4)
        self.assertEqual(round(float(jobs_env['pvalue']), 4), 0.2937)

        pid = {
            k: list(v.values())[0]
            for k, v in results.loc[results['issue'] == 'pid',:].to_dict().items()
        }
        self.assertEqual(pid['issue'], 'pid')
        self.assertEqual(pid['metric'], 'pid_delta')
        self.assertEqual(round(float(pid['statistic']), 3), 0.243)
        self.assertEqual(round(float(pid['df']), 1), 777.7)
        self.assertEqual(round(float(pid['pvalue']), 4), 0.8079)

    def test_chisq(self):
        def _test_chiqsq(expected, data, label, demographic='new_child'):
            self._test_pvalue(expected, self.data.chisq(data, label, demographic))

        paired_waves = self.data.get_paired_waves()
        _test_chiqsq(0.9465, paired_waves, 'ideo_direction')
        _test_chiqsq(0.4124, paired_waves, 'pid_direction')

        data = paired_waves.loc[np.equal(paired_waves['new_child'], 1),:]
        _test_chiqsq(0.8229, data, 'ideo_direction', 'high_income')
        _test_chiqsq(0.5296, data, 'pid_direction', 'high_income')
        _test_chiqsq(0.5551, data, 'ideo_direction', 'gender')
        _test_chiqsq(0.0956, data, 'pid_direction', 'gender')

        data = paired_waves.loc[np.equal(paired_waves['gender'], 1),:]
        _test_chiqsq(0.9348, data, 'ideo_direction')
        _test_chiqsq(0.2544, data, 'pid_direction')

        data = paired_waves.loc[np.equal(paired_waves['gender'], 2),:]
        _test_chiqsq(0.7973, data, 'ideo_direction')
        _test_chiqsq(0.1096, data, 'pid_direction')

    def test_chisqs(self):
        results = self.data.chisqs(self.data.get_paired_waves(), 'change')

        schip = {
            k: list(v.values())[0]  # v will have a single value
            for k, v in results.loc[results['issue'] == 'schip',:].to_dict().items()
        }
        self.assertEqual(schip['issue'], 'schip')
        self.assertEqual(schip['metric'], 'schip_change')
        self.assertEqual(round(float(schip['statistic']), 3), 0.821)
        self.assertEqual(round(float(schip['dof']), 1), 1)
        self.assertEqual(round(float(schip['pvalue']), 4), 0.3649)

    def test_summarize_continuous(self):
        data = self.data.get_paired_waves()
        guns = self.data.summarize_continuous(data, "new_child", "guns")
        self.assertListEqual([round(v, 2) for v in guns.iloc[1, 1:].values], [1.7, 1.68, -0.0, 0.31, -0.04, 0.09])
        military = self.data.summarize_continuous(data, "new_child", "military_composite")
        self.assertListEqual([round(v, 2) for v in military.iloc[1, 1:].values], [1.49, 1.53, 0.04, 0.16, 0.04, 0.07])

    def test_count_percentages(self):
        data = self.data.get_paired_waves()

        after_counts = self.data.count_percentages(data, 'new_child', 'gay_marriage_after')
        self.assertListEqual(after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].to_list(), [40.7, 59.3])

        after_counts = self.data.count_percentages(data, 'new_child', 'budget_avoid_after')
        self.assertListEqual(after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].to_list(), [19.2, 36.4, 44.4])

        data = data.loc[np.equal(data['new_child'], 1),:]
        change_counts = self.data.count_percentages(data, "gender", "budget_change")
        self.assertListEqual(change_counts['count'].to_list(), [164,  45, 129,  88])
        self.assertListEqual(change_counts['percent'].to_list(), [78.5, 21.5, 59.4, 40.6])
