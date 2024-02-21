import numpy as np
import pandas as pd

from parents_politics_panel import ParentsPoliticsPanel

class CESPanel(ParentsPoliticsPanel):
    waves = [10, 12, 14]

    def _load_panel(cls):
        return pd.read_stata("~/Documents/visualizations/midterm/CCES_Panel_Full3waves_VV_V4.dta", convert_categoricals=False)  # n=9500

    def _build_all_waves(self, panel):
        all_waves = self._trim_columns_from_panel(panel)
        all_waves = all_waves.assign(start_wave=10, end_wave=14)
        all_waves = self._add_age(all_waves)
        all_waves = self._recode_issues(all_waves)
        all_waves = self._consolidate_demographics(all_waves)
        all_waves = self._add_income_brackets(all_waves)
        return all_waves.copy()

    def _trim_columns_from_panel(self, panel):
        # Drop most columns
        return panel.loc[
            :,
            panel.columns.str.contains('caseid') +
            panel.columns.str.contains('weight') +   # TODO

            # Ideology and partisanship
            panel.columns.str.startswith('ideo5_') + 
            panel.columns.str.contains('^pid3_1[024]', regex=True) +
            panel.columns.str.startswith('pid7_') + 

            # Policy issues: categorical
            panel.columns.str.contains("CC1[024]_320", regex=True) + # gun control (1-3 more strict, less strict, same)
            panel.columns.str.contains("CC1[024]_326", regex=True) + # gay marriage (1/2 no/yes): note issue was very active during this time, with Obergefell in 2015
            panel.columns.str.contains("CC1[024]_328", regex=True) + # budget (1 cut defense, 2 cut domestic, 3 raise taxes)
            panel.columns.str.contains("CC1[024]_329", regex=True) + # budget move to avoid (1 cut defense, 2 cut domestic, 3 raise taxes)
            panel.columns.str.contains("CC1[024]_330B", regex=True) + # SCHIP (1 renew, 2 expire)

            # Policy issues: continuous
            panel.columns.str.contains("CC1[024]_321", regex=True) + # climate change (1-5 real to not real)
            panel.columns.str.contains("CC1[024]_325", regex=True) + # job vs environment (1-5 favor environment to favor jobs)
            panel.columns.str.contains("CC1[024]_327", regex=True) + # affirmative action (1-4 support to oppose)
            panel.columns.str.contains("CC1[024]_415r", regex=True) + # taxes vs spending (examples given are of domestic spending) (0 to 100)
            panel.columns.str.contains("CC1[024]_416r", regex=True) + # raise sales vs income tax (0 to 100)

            # Policy issues: additional issues for composites
            panel.columns.str.contains("CC1[024]_330C", regex=True) + # clean energy act (1/2 support/oppose, discard other values)
            panel.columns.str.contains("CC1[024]_330G", regex=True) + # end don't ask don't tell (1/2 support/oppose, discard other values)
            panel.columns.str.contains("CC1[024]_322_[1-7]", regex=True) + # immigration policies (1/2 support/oppose)
            panel.columns.str.contains("CC1[024]_414_[1-7]", regex=True) + # use of military force for various purposes (1/2 yes/no)

            # Parenthood
            panel.columns.str.startswith("gender_") +
            panel.columns.str.startswith("child18_") +
            panel.columns.str.startswith("child18num_") +

            # Demographics for controls/filters
            panel.columns.str.startswith("birthyr_") +
            panel.columns.str.startswith("faminc_") +
            panel.columns.str.startswith("investor_") + # 1/2 yes/no
            panel.columns.str.startswith("newsint_") + # Limit to 1-4, 1 is "high"
            panel.columns.str.startswith("race_") + # Limit to 1-8, categorical
            panel.columns.str.startswith("educ_") + # Limit to 1-6, categorical
            panel.columns.str.startswith("marstat_") + # Limit to 1-6, categorical
            panel.columns.str.startswith("pew_religimp_") # Limit to 1-4, 1 is "very important"
        ].copy()

    def _add_age(self, all_waves):
        return all_waves.assign(age=lambda x: 2010 - x.birthyr_10)

    def _recode_issues(self, all_waves):
        # Recode a few columns to streamline later calculations
        # TODO: Verify that all invalid vlues are NAed out
        for year in self.waves:
            # Recode guns to be continuous (swapping 2 and 3 so that "no change" is in the middle of "less strict" and "more strict")
            label = f'CC{year}_320'
            all_waves.loc[:, label] = np.where(all_waves[label] == 2, 3, np.where(all_waves[label] == 3, 2, np.where(all_waves[label] == 1, 1, np.nan)))

            # Recode don't ask don't tell, swapping so that 1 is the more conservative value, to match gay marriage ban question
            all_waves[f'CC{year}_330G'] = np.where(
                all_waves[f'CC{year}_330G'] == 1,
                2,
                np.where(all_waves[f'CC{year}_330G'] == 2, 1, np.nan)
            )

            # Replace NA with 0 for child18num columns - seems this question was skipped if child18_{year} was No
            label = f'child18num_{year}'
            all_waves.loc[np.isnan(all_waves[label]), label] = 0

            # CC10_414_1-CC10_414_6 are all usage of military for for different reasons: 1 yes, 2 no
            # CC10_414_7 is a "none of the above" for the previous six: 1 yes, 2 no
            all_waves[f'CC{year}_414_7'] = np.where(all_waves[f'CC{year}_414_7'] == 1, 2, 1)

            # Flip the 2 yes/no immigration questions that are opposite polarity of the other 5
            # For 1,7, 1 is more liberal and 2 is more conservative
            # For 2,3,4,5,6, 1 is more conservative and 2 is more liberal
            all_waves[f'CC{year}_322_1'] = np.where(all_waves[f'CC{year}_322_1'] == 1, 2, 1)
            if year == 10:  # only asked in 2010
                all_waves[f'CC{year}_322_7'] = np.where(all_waves[f'CC{year}_322_7'] == 1, 2, np.where(all_waves[f'CC{year}_322_7'] == 2, 1, np.nan))
        return all_waves

    # Consolidate demographics, arbitrarily using later data if there are differences
    def _consolidate_demographics(self, all_waves):
        for demo in ('gender', 'race', 'investor', 'newsint', 'educ', 'marstat', 'pew_religimp'):
            old_labels = [f'{demo}_{wave}' for wave in self.waves]
            all_waves[demo] = all_waves[old_labels].bfill(axis=1).iloc[:, 0]
            all_waves.drop(old_labels, axis=1, inplace=True)
        return all_waves

    def _add_income_brackets(self, all_waves):
        # Income: Start with faminc_14 because the buckets vary by year, and the 2014 buckets are more granular
        # Income brackets are approximate, since incomes are given in ranges.
        all_waves = all_waves.rename(columns={'faminc_14': 'income'})
        all_waves = all_waves.assign(
            income_quintile=lambda x:np.select(
                [
                    # note the 10 response could go into either 4th or 5th quintile
                    x.income == n for n in range(1, 17)
                ],
                [1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5],
                default=np.nan
            ),
            # "High income" is top 20% to match Reeves
            high_income=lambda x: np.where(np.isnan(x.income_quintile), np.NAN, np.where(x.income_quintile == 5, 1, 0)),
            # "Low income" is bottom 40%, to very roughly correspond with SCHIP eligibility
            low_income=lambda x: np.select(
                [
                    np.isnan(x.income_quintile),
                    x.income_quintile == 1,
                    x.income_quintile == 2,
                ],
                [np.NAN, 1, 1],
                default=np.nan
            ),
        )
        return all_waves

    def _add_parenting(self, df):
        df = df.assign(
            new_child=lambda x:np.select(
                [x.start_wave == w for w in self.start_waves],
                [x[f'child18num_{w}'] < x[f'child18num_{self.waves[i + 1]}'] for i, w in enumerate(self.start_waves)],
            ),  # TODO: error if default is assigned?
            firstborn=lambda x:np.where(
                x.new_child == False,
                False,  # TODO: should this be NA?
                np.select(
                    [x.start_wave == w for w in self.start_waves],
                    [x[f'child18num_{w}'] == 0 for w in self.start_waves],
                )
            )
        )
        return df.drop([f'child18num_{year}' for year in self.waves], axis=1)

    def _add_all_continuous(self, df):
        df = self._add_continuous(df, 'ideo5_XX', 'ideo', 1, 5)
        df = self._add_continuous(df, 'pid7_XX', 'pid', 1, 7)
        df = self._add_continuous(df, 'CCXX_321', 'climate_change', 1, 5)
        df = self._add_continuous(df, 'CCXX_325', 'jobs_env', 1, 5)
        df = self._add_continuous(df, 'CCXX_327', 'aff_action', 1, 4)
        df = self._add_continuous(df, 'CCXX_320', 'guns', 1, 3)
        df = self._add_continuous(df, 'CCXX_415r', 'tax_or_spend', 0, 100)
        df = self._add_continuous(df, 'CCXX_416r', 'sales_or_inc', 0, 100)
        return df

    def _add_all_categorical(self, df):
        df = self._add_categorical(df, 'CCXX_326', 'gay_marriage', 1, 2)
        df = self._add_categorical(df, 'CCXX_330B', 'schip', 1, 2)
        df = self._add_categorical(df, 'CCXX_328', 'budget', 1, 3)
        df = self._add_categorical(df, 'CCXX_329', 'budget_avoid', 1, 3)
        return df

    def _add_all_composite(self, df):
        for year in self.waves:
            # TODO: add in the jobs/environment question to this composite?
            # CC10_321 is climate change: 1-5 with 1 liberal
            # CC10_330C is clean energy act, with 1 support, 2, oppose, and other values invalid
            # Composite is 1-5, with lower values more liberal
            df = self.nan_out_of_bounds(df, f'CC{year}_330C', 1, 2)
            df[f'climate_composite_20{year}'] = (df[f'CC{year}_321'] * 2.5 + df[f'CC{year}_330C']) / 2

            # CC10_326 is gay marriage ban: 1 support, 2 oppose
            # CC10_330G is ending don't ask don't tell: 1 support, 2 oppose, others invalid
            df = self.nan_out_of_bounds(df, f'CC{year}_330G', 1, 2)
            df[f'gay_composite_20{year}'] = (df[f'CC{year}_326'] + df[f'CC{year}_330G']) / 2

            # yes/no questions on military force usage
            df[f'military_composite_20{year}'] = np.sum(df.loc[:, df.columns.str.startswith(f'CC{year}_414_')], axis=1) / 7

            # Ideology composite that combines ideo and pid
            df[f'ideo_composite_20{year}'] = (df[f'ideo5_{year}'] * 5 + 2.5 * df[f'pid7_{year}']) / 7 / 2  # 5-point composite scale

        # CC10_322_1-CC10_322_7 are all yes/no immigration questions, 8 and 9 are "nothing"/"none of the above" which aren't clearly liberal or conservative
        # 2010 asked 1 2 3 4 7, 2012 asked 1 2 3 4 5 6, 2014 asked 1 2 3 4 5 6
        df[f'immigration_composite_2010'] = (np.sum(df.loc[:, df.columns.str.contains('CC10_322_[1-4]')], axis=1) + df['CC10_322_7']) / 5
        df[f'immigration_composite_2012'] = np.sum(df.loc[:, df.columns.str.contains('CC12_322_[1-6]')], axis=1) / 6
        df[f'immigration_composite_2014'] = np.sum(df.loc[:, df.columns.str.contains('CC14_322_[1-6]')], axis=1) / 6

        df = self._add_continuous(df, 'climate_composite_20XX', 'climate_composite')
        df = self._add_continuous(df, 'gay_composite_20XX', 'gay_composite')
        df = self._add_continuous(df, 'ideo_composite_20XX', 'ideo_composite')
        df = self._add_continuous(df, 'military_composite_20XX', 'military_composite')
        df = self._add_continuous(df, 'immigration_composite_20XX', 'immigration_composite')

        return df

    def _add_before_after(self, df, before_pattern, prefix, lower_bound=None, upper_bound=None):
        df = df.assign(**{
            f'{prefix}_before': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [x[before_pattern.replace('XX', str(w))] for w in self.start_waves],
            ),
            f'{prefix}_after': lambda x:np.select(
                [x.start_wave == w for w in self.waves[:-1]],
                [x[before_pattern.replace('XX', str(w))] for w in self.end_waves],
            ),
        })
        df = self.nan_out_of_bounds(df, f'{prefix}_before', lower_bound, upper_bound)
        df = self.nan_out_of_bounds(df, f'{prefix}_after', lower_bound, upper_bound)
        return df

    def _add_continuous(self, df, before_pattern, prefix, lower_bound=None, upper_bound=None):
        df = self._add_before_after(df, before_pattern, prefix, lower_bound, upper_bound)

        df = df.assign(**{
            f'{prefix}_delta': lambda x: x[f'{prefix}_after'] - x[f'{prefix}_before'],
            f'{prefix}_delta_abs': lambda x: abs(x[f'{prefix}_delta']),
            f'{prefix}_delta_sq': lambda x: x[f'{prefix}_delta'] * x[f'{prefix}_delta'],
            f'{prefix}_direction': lambda x: np.sign(x[f'{prefix}_delta']),
        })
        df.loc[np.isnan(df[f'{prefix}_delta']), f'{prefix}_direction'] = np.nan # because some of the 0s should be NaN

        # Only relevant in all_waves
        df = df.assign(**{
            f'{prefix}_persists': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_and(
                    x[f'{prefix}_delta'] != 0, # change in start vs end
                    # change from start to final is either zero or the same direction as delta
                    x[f'{prefix}_delta'] * (x[before_pattern.replace('XX', str(self.end_waves[-1]))] - x[before_pattern.replace('XX', str(self.end_waves[i]))]) >= 0
                ),
                x[before_pattern.replace('XX', str(self.end_waves[-1]))] - x[before_pattern.replace('XX', str(w))],
                0) for i, w in enumerate(self.start_waves)]
            )
        })
        for wave in self.waves:
            df.loc[np.isnan(df[before_pattern.replace('XX', str(wave))]), f'{prefix}_persists'] = np.nan
        df[f'{prefix}_persists_abs'] = np.abs(df[f'{prefix}_persists'])

        self.CONTINUOUS_PREFIXES.add(prefix)

        return df

    def _add_categorical(self, df, before_pattern, prefix, lower_bound=None, upper_bound=None):
        df = self._add_before_after(df, before_pattern, prefix)

        df[f'{prefix}_change'] = np.where(df[f'{prefix}_before'] == df[f'{prefix}_after'], 0, 1)
        # distinguish between False and NaN
        for suffix in ('before', 'after'):
            df.loc[np.isnan(df[f'{prefix}_{suffix}']), f'{prefix}_change'] = np.nan

        # Only relevant in all_waves
        df = df.assign(**{f'{prefix}_persists': lambda x: np.select(
            [x.start_wave == w for w in self.start_waves],
            [np.where(np.logical_and(
                x[before_pattern.replace('XX', str(w))] != x[before_pattern.replace('XX', str(self.end_waves[i]))], # change in start vs end
                x[before_pattern.replace('XX', str(self.end_waves[i]))] == x[before_pattern.replace('XX', str(self.end_waves[-1]))]  # kept end value in final wave
            ), 1, 0) for i, w in enumerate(self.start_waves)]
        )})
        for wave in self.waves:
            df.loc[np.isnan(df[before_pattern.replace('XX', str(wave))]), f'{prefix}_persists'] = np.nan
        df[f'{prefix}_persists_abs'] = np.abs(df[f'{prefix}_persists'])

        self.CATEGORICAL_PREFIXES.add(prefix)

        return df
