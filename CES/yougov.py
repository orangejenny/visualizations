import numpy as np
import pandas as pd
import pdb

from parents_politics_panel import ParentsPoliticsPanel

class YouGovPanel(ParentsPoliticsPanel):
    waves = [11, 16, 17]

    def _load_panel(cls):
        # TODO: revisit problems encountered in parsing (54 problems, relating to unexpected values in cols 180-191)
        return pd.read_csv("~/Downloads/VOTER Panel Data Files/voter_panel.csv", encoding="latin_1")

    def _trimmed_panel(self):
        return self.panel.copy()

    def _add_parenting(self, df):
        return df

    def _add_all_continuous(self, df):
        return df

    def _add_all_composite(self, df):
        return df
