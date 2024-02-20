class ParentsPoliticsData():
    def __init__(self):
        self.panel = self._load_panel()
        self.all_waves = self._build_all_waves(self.panel)
        self.paired_waves = self._build_paired_waves(self.all_waves)

    def _load_panel(self):
        raise NotImplementedError()

    def _build_all_waves(self, panel):
        raise NotImplementedError()

    def _build_paired_waves(self, panel):
        raise NotImplementedError()

    # panel contains all original data
    def get_panel(self):
        return self.panel

    # all_waves contains one row per respondent and has extraneous columns reused, values recoded, etc.
    def get_all_waves(self):
        return self.all_waves

    # paired_waves contains multiple rows per respondent, one row per pair of waves
    def get_paired_waves(self):
        return self.paired_waves
