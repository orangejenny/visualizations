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

    def test_t_test(self):
        def _test_pvalue(data, label, expected, demographic='new_child'):
            self.assertEqual(expected, round(self.data.t_test(data, label, demographic).pvalue, 4))

        data = self.data.get_paired_waves()
        _test_pvalue(data, 'ideo_delta', 0.4108)
        _test_pvalue(data, 'ideo_composite_delta', 0.7221)
        _test_pvalue(data, 'pid_delta', 0.4348)

        young_adults = data.loc[np.less(data['age'], 30),:]
        _test_pvalue(young_adults, 'ideo_delta_abs', 0.6028)
        _test_pvalue(young_adults, 'ideo_composite_delta_abs', 0.3203)

        _test_pvalue(data, 'climate_composite_after', 0.9979)
        _test_pvalue(data, 'immigration_composite_delta', 0.787)
        _test_pvalue(data, 'military_composite_delta', 0.5486)
        _test_pvalue(data, 'jobs_env_delta', 0.4092, 'firstborn')
        _test_pvalue(data, 'jobs_env_delta_abs', 0.1119, 'firstborn')
