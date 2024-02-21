import numpy as np
import pandas as pd

from parents_politics_data import ParentsPoliticsData

class CESData(ParentsPoliticsData):
    def _load_panel(cls):
        return pd.read_stata("~/Documents/visualizations/midterm/CCES_Panel_Full3waves_VV_V4.dta", convert_categoricals=False)  # n=9500

    def _build_all_waves(self, panel):
        all_waves = self._trim_columns_from_panel(panel)
        all_waves = self._add_cycle(all_waves)
        all_waves = self._add_age(all_waves)
        all_waves = self._recode_issues(all_waves)
        all_waves = self._consolidate_demographics(all_waves)
        all_waves = self._add_income_brackets(all_waves)
        return all_waves.copy()

    def _build_paired_waves(self, all_waves):
        return pd.concat([
            all_waves.assign(cycle=1012), # contains all data, but only look at 2010/2012
            all_waves.assign(cycle=1214)  # contains all data, but only look at 2012/2014
        ])

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

    def _add_cycle(self, all_waves):
        return all_waves.assign(cycle=101214)

    def _add_age(self, all_waves):
        return all_waves.assign(age=lambda x: 2010 - x.birthyr_10)

    def _recode_issues(self, all_waves):
        # Recode a few columns to streamline later calculations
        # TODO: Verify that all invalid vlues are NAed out
        for year in (10, 12, 14):
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
            old_labels = [f'{demo}_10', f'{demo}_12', f'{demo}_14']
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
                    x.income == 1,
                    x.income == 2,
                    x.income == 3,
                    x.income == 4,
                    x.income == 5,
                    x.income == 6,
                    x.income == 7,
                    x.income == 8,
                    x.income == 9,
                    x.income == 10, # note the 10 response could go into either 4th or 5th quintile
                    x.income == 11,
                    x.income == 12,
                    x.income == 13,
                    x.income == 14,
                    x.income == 15,
                    x.income == 16,
                ],
                [1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5],
                default=np.NAN
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
                default=0
            ),
        )
        return all_waves
