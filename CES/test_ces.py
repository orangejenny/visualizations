# python -m unittest test_ces.py

import unittest

from ces import CESData

class TestCESData(unittest.TestCase):
    def setUp(self):
        super().setUp()
        self.data = CESData()

    def test_parenting_counts(self):
        counts = self.data.get_paired_waves().groupby('new_child', as_index=False).count()
        self.assertListEqual(counts.loc[:, 'caseid'].to_list(), [18580, 420])

        counts = self.data.get_all_waves().groupby('new_child', as_index=False).count()
        self.assertListEqual(counts.loc[:, 'caseid'].to_list(), [9271, 229])
