import pandas as pd

from parents_politics_panel import ParentsPoliticsPanel

class YouGovPanel(ParentsPoliticsPanel):
    waves = [11, 16, 17]

    def _load_panel(cls):
        return pd.read_csv("~/Downloads/VOTER Panel Data Files/voter_panel.csv", encoding="latin_1")

    def _build_all_waves(self, panel):
        return panel

    def _build_paired_waves(self, all_waves):
        return all_waves
