import numpy as np

class ParentsPoliticsData():
    def __init__(self):
        self.CONTINUOUS_PREFIXES = set()
        self.CATEGORICAL_PREFIXES = set()

        self.panel = self._load_panel()
        self.all_waves = self._build_all_waves(self.panel)
        self.paired_waves = self._build_paired_waves(self.all_waves)

        self.all_waves = self._add_parenting(self.all_waves)
        self.paired_waves = self._add_parenting(self.paired_waves)

        self.all_waves = self._add_all_continuous(self.all_waves)
        self.paired_waves = self._add_all_continuous(self.paired_waves)

        self.all_waves = self._add_all_categorical(self.all_waves)
        self.paired_waves = self._add_all_categorical(self.paired_waves)

        self.all_waves = self._add_all_composite(self.all_waves)
        self.paired_waves = self._add_all_composite(self.paired_waves)

        # De-fragment frames
        self.all_waves = self.all_waves.copy()
        self.paired_waves = self.paired_waves.copy()

    def _load_panel(self):
        raise NotImplementedError()

    def _build_all_waves(self, panel):
        raise NotImplementedError()

    def _build_paired_waves(self, panel):
        raise NotImplementedError()

    def _add_parenting(self, df):
        raise NotImplementedError()

    def _add_all_continuous(self, df):
        raise NotImplementedError()

    def _add_all_categorical(self, df):
        raise NotImplementedError()

    def _add_all_composite(self, df):
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

    def count_flippers(self, before_label, after_label, lower_bound, upper_bound):
        valid_rows = self.all_waves.loc[
            np.greater_equal(self.all_waves[before_label], lower_bound) & np.greater_equal(self.all_waves[after_label], lower_bound)
            &
            np.less_equal(self.all_waves[before_label], upper_bound) & np.less_equal(self.all_waves[after_label], upper_bound),
            [before_label, after_label]
        ]
        flippers = valid_rows.loc[np.not_equal(valid_rows[before_label], valid_rows[after_label]), :]
        return round(len(flippers) * 100 / len(valid_rows), 1)

    @classmethod
    def nan_out_of_bounds(cls, df, label, lower_bound=None, upper_bound=None):
        if lower_bound is None or upper_bound is None:
            return df

        df = df.assign(lower_bound=lower_bound, upper_bound=upper_bound)

        df.loc[np.logical_or(
            np.less(df[label], df.lower_bound),
            np.greater(df[label], df.upper_bound)
        ), label] = np.nan

        df.drop(['lower_bound', 'upper_bound'], axis=1)

        return df

