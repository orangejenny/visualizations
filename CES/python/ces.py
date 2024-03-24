import numpy as np
import pandas as pd

from scipy.stats import zscore

from parents_politics_panel import ParentsPoliticsPanel

class CESPanel(ParentsPoliticsPanel):
    waves = [10, 12, 14]
    treatments = {'firstborn', 'new_child', 'is_parent'}
    demographics_with_bounds = [
        ('gender', 1, 2),
        ('race', 1, 8),
        ('employ', 1, 8),
        #('investor', 1, 2),
        ('educ', 1, 6),
        ('marstat', 1, 6),
        ('pew_churatd', 1, 6),  # There are other religion questions, but this one is used in CES's own sample matching
        ('ownhome', 1, 3),

        # constructed
        ('RUCC_2023', None, None),  # From USDA codes: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
        ('division', None, None),  # Census division: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
        ('age', None, None),
        ('income', None, None),
        #('income_quintile', None, None),   # Duplicative with income and less granular
    ]

    def _load_panel(cls):
        return pd.read_stata("~/Documents/visualizations/midterm/CCES_Panel_Full3waves_VV_V4.dta", convert_categoricals=False)  # n=9500

    def _build_paired_waves(self, df):
        if not len(self.waves):
            raise Exception("Must contain at least one wave")
        df = df.assign(start_wave=self.waves[0], end_wave=self.waves[-1])
        df = self._recode_issues(df)
        df = self.add_income_brackets(df)

        df = pd.concat([
            df.assign(
                start_wave=w,
                end_wave=self.end_waves[i],
            ) for i, w in enumerate(self.start_waves)
        ], ignore_index=True)
        df = self.add_age(df)   # age depends on start_wave
        df = self.add_rural_urban(df)
        df = self._consolidate_demographics(df)

        return df

    def _trimmed_panel(self):
        # Drop most columns
        return self.panel.loc[
            :,
            self.panel.columns.str.contains('caseid') +
            self.panel.columns.str.contains('weight') +

            # Ideology and partisanship
            self.panel.columns.str.startswith('ideo5_') + 
            self.panel.columns.str.contains('^pid3_1[024]', regex=True) +
            self.panel.columns.str.startswith('pid7_') + 

            # Policy issues: single issues
            self.panel.columns.str.contains("CC1[024]_320", regex=True) + # gun control (1-3 more strict, less strict, same)
            self.panel.columns.str.contains("CC1[024]_321", regex=True) + # climate change (1-5 real to not real)
            self.panel.columns.str.contains("CC1[024]_325", regex=True) + # job vs environment (1-5 favor environment to favor jobs)
            self.panel.columns.str.contains("CC1[024]_327", regex=True) + # affirmative action (1-4 support to oppose)
            self.panel.columns.str.contains("CC1[024]_415r", regex=True) + # taxes vs spending (examples given are of domestic spending) (0 to 100)

            # Policy issues: additional issues for composites
            self.panel.columns.str.contains("CC1[024]_326", regex=True) + # gay marriage (1/2 no/yes): note issue was very active during this time, with Obergefell in 2015
            self.panel.columns.str.contains("CC1[024]_328", regex=True) + # budget (1 cut defense, 2 cut domestic, 3 raise taxes)
            self.panel.columns.str.contains("CC1[024]_329", regex=True) + # budget move to avoid (1 cut defense, 2 cut domestic, 3 raise taxes)
            self.panel.columns.str.contains("CC1[024]_330B", regex=True) + # SCHIP (1 renew, 2 expire)
            self.panel.columns.str.contains("CC1[024]_330C", regex=True) + # clean energy act (1/2 support/oppose, discard other values)
            self.panel.columns.str.contains("CC1[024]_330G", regex=True) + # end don't ask don't tell (1/2 support/oppose, discard other values)
            self.panel.columns.str.contains("CC1[024]_322_[1-7]", regex=True) + # immigration policies (1/2 support/oppose)
            self.panel.columns.str.contains("CC1[024]_414_[1-7]", regex=True) + # use of military force for various purposes (1/2 yes/no)

            # Parenthood
            self.panel.columns.str.startswith("gender_") +
            self.panel.columns.str.startswith("child18_") +
            self.panel.columns.str.startswith("child18num_") +

            # Demographics for controls/filters
            self.panel.columns.str.startswith("birthyr_") +
            self.panel.columns.str.startswith("faminc_") +
            self.panel.columns.str.startswith("investor_") + # 1/2 yes/no
            self.panel.columns.str.startswith("race_") + # Limit to 1-8, categorical
            self.panel.columns.str.startswith("educ_") + # Limit to 1-6, categorical
            self.panel.columns.str.startswith("marstat_") + # Limit to 1-6, categorical
            self.panel.columns.str.startswith("pew_churatd") + # Limit to 1-6, 1 is "more than once a week"
            self.panel.columns.str.startswith("employ_") + # Limit to 1-8, categorical
            self.panel.columns.str.startswith("ownhome_") + # Limit to 1-3, categorical
            self.panel.columns.str.startswith("countyfips_")
        ].copy()

    def add_age(self, df):
        df = df.assign(age=lambda x: 2000 + x.start_wave - x.birthyr_10)
        df = self.nan_out_of_bounds(df, 'age', 1, 200)
        df['age_zscore'] = zscore(df['age'], nan_policy='omit')
        return df

    def _recode_issues(self, df):
        # Recode a few columns to streamline later calculations
        # TODO: Verify that all invalid vlues are NAed out
        for year in self.waves:
            # Recode guns to be continuous (swapping 2 and 3 so that "no change" is in the middle of "less strict" and "more strict")
            label = f'CC{year}_320'
            df.loc[:, label] = np.where(df[label] == 2, 3, np.where(df[label] == 3, 2, np.where(df[label] == 1, 1, np.nan)))

            # Recode don't ask don't tell, swapping so that 1 is the more conservative value, to match gay marriage ban question
            df[f'CC{year}_330G'] = np.where(
                df[f'CC{year}_330G'] == 1,
                2,
                np.where(df[f'CC{year}_330G'] == 2, 1, np.nan)
            )

            # Replace NA with 0 for child18num columns - seems this question was skipped if child18_{year} was No
            label = f'child18num_{year}'
            df.loc[np.isnan(df[label]), label] = 0

            # CC10_414_1-CC10_414_6 are all usage of military for for different reasons: 1 yes, 2 no
            # CC10_414_7 is a "none of the above" for the previous six: 1 yes, 2 no
            df[f'CC{year}_414_7'] = np.where(df[f'CC{year}_414_7'] == 1, 2, 1)

            # Flip the 2 yes/no immigration questions that are opposite polarity of the other 5
            # For 1,7, 1 is more liberal and 2 is more conservative
            # For 2,3,4,5,6, 1 is more conservative and 2 is more liberal
            df[f'CC{year}_322_1'] = np.where(df[f'CC{year}_322_1'] == 1, 2, 1)
            if year == 10:  # only asked in 2010
                df[f'CC{year}_322_7'] = np.where(df[f'CC{year}_322_7'] == 1, 2, np.where(df[f'CC{year}_322_7'] == 2, 1, np.nan))
        return df

    def _consolidate_demographics(self, df):
        for demo, lower_bound, upper_bound in self.demographics_with_bounds:
            if lower_bound is None and upper_bound is None:
                continue

            old_labels = [f'{demo}_{wave}' for wave in self.waves]
            for old_label in old_labels:
                df = self.nan_out_of_bounds(df, old_label, lower_bound, upper_bound)

            # Use "after" data if available, else use most recent value
            df = df.assign(**{demo: lambda x: np.select(
                [x.end_wave == w for w in self.end_waves],
                [np.where(
                    pd.notna(x[f'{demo}_{w}']),
                    x[f'{demo}_{w}'],
                    x[old_labels].bfill(axis=1).iloc[:, 0]
                ) for i, w in enumerate(self.end_waves)],
            )})
            df.drop(old_labels, axis=1, inplace=True)
        return df

    def add_rural_urban(self, df):
        codes = pd.read_csv("~/Documents/visualizations/midterm/ruralurbancontinuumcodes2023/rural_urban.csv")
        df = df.assign(
            countyfips_before=lambda x:np.select(
                [x.start_wave == w for w in self.start_waves],
                [x[f'countyfips_{w}'] for w in self.start_waves],
            )
        )
        df = df.astype({'countyfips_before': 'int64'})
        df = df.merge(codes, how='left', left_on='countyfips_before', right_on='FIPS')
        # https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
        states = [
            ['CT', 'ME', 'MA', 'NH', 'RI', 'VT'],
            ['NJ', 'NY', 'PA'],
            ['IN', 'IL', 'MI', 'OH', 'WI'],
            ['IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD'],
            ['DE', 'DC', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'WV'],
            ['AL', 'KY', 'MS', 'TN'],
            ['AR', 'LA', 'OK', 'TX'],
            ['AZ', 'CO', 'ID', 'NM', 'MY', 'UT', 'NV', 'WY'],
            ['AK', 'CA', 'HI', 'OR', 'WA'],
        ]
        divisions = pd.DataFrame(data=[(a, i + 1) for i, abbreviations in enumerate(states) for a in abbreviations])
        divisions.rename(columns={0: 'state', 1: 'division'}, inplace=True)
        df = df.merge(divisions, how='left', left_on='State', right_on='state')
        return df

    def add_income_brackets(self, df):
        # Income: Start with faminc_14 because the buckets vary by year, and the 2014 buckets are more granular
        # Income brackets are approximate, since incomes are given in ranges.
        df = df.rename(columns={'faminc_14': 'income'})
        df = self.nan_out_of_bounds(df, 'income', 1, 50)  # good enough to get rid of 98/99
        df = df.assign(
            income_quintile=lambda x:np.select(
                [
                    # note the 10 response could go into either 4th or 5th quintile
                    x.income == n for n in [x for x in range(1, 17)] + [32]
                ],
                [1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5],
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
                    x.income_quintile == 3,
                    x.income_quintile == 4,
                    x.income_quintile == 5,
                ],
                [np.NAN, 1, 1, 0, 0, 0],
                default=np.nan
            ),
        )
        return df

    def _add_parenting(self, df):
        '''
        parenthood is based on child18 (parent of ninor children? yes/no) and child18num (number of minor children)
        - 0 no children
        - 1 new first child (same as firstborn)
        - 2 new additional child
        - 3 parent, no change in number of children
        '''
        df = df.assign(
            # Combination of parent yes/no question and number of children question, being cautious of invalid values
            parenthood=lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(
                    # child18_start is NA or child18_next is NA,
                    np.logical_or(
                        np.logical_and(x[f'child18_{w}'] != 1, x[f'child18_{w}'] != 2),
                        np.logical_and(x[f'child18_{self.waves[i + 1]}'] != 1, x[f'child18_{self.waves[i + 1]}'] != 2),
                    ),
                    np.nan,
                    np.where(
                        x[f'child18_{w}'] == 2,  # child18_start is no, so not a parent at the start
                        np.where(
                            x[f'child18_{self.waves[i + 1]}'] == 1,  # child18_next is yes, so had a child at the end
                            1, # first child
                            0  # no children
                        ),
                        np.where(   # child18_start is yes
                            # child18num_start or child18num_next is invalid
                            np.logical_or(
                                np.logical_or(pd.isna(x[f'child18num_{w}']), pd.isna(x[f'child18num_{self.waves[i + 1]}'])),
                                np.logical_or(x[f'child18num_{w}'] > 20, x[f'child18num_{self.waves[i + 1]}'] > 20),
                            ),
                            np.nan,
                            np.where(
                                x[f'child18num_{w}'] < x[f'child18num_{self.waves[i + 1]}'],
                                2,  # new additional child
                                3   # parent, but no change in number of children
                            )
                        )
                    )
                ) for i, w in enumerate(self.start_waves)],
            )
        )
        df = df.loc[pd.notna(df['parenthood']),:].copy() # remove any rows where parenthood cannot be determined
        '''
        Additional boolean columns based on parenthood
        - childless: 0     ...recall this is only about minor children
        - firstborn: 1
        - new_child: 2
        - steady_parent: 3
        - is_parent: 1, 2, or 3

        - 0 no children
        - 1 new first child (same as firstborn)
        - 2 new additional child
        - 3 parent, no change in number of children
        '''
        df = df.assign(**{
            'childless': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 0, 1, 0) for w in self.start_waves],
            ),
            'firstborn': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 1, 1, 0) for w in self.start_waves],
            ),
            'new_child': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 2, 1, 0) for w in self.start_waves],
            ),
            'steady_parent': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood == 3, 1, 0) for w in self.start_waves],
            ),
            'is_parent': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(x.parenthood != 0, 1, 0) for w in self.start_waves],
            ),

            # Dosage dummy variables
            'is_parent_1': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_and(x.is_parent == 1, x[f'child18num_{w}'] == 1), 1, 0) for w in self.end_waves],
            ),
            'is_parent_2': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_and(x.is_parent == 1, x[f'child18num_{w}'] == 2), 1, 0) for w in self.end_waves],
            ),
            'is_parent_3': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_and(x.is_parent == 1, x[f'child18num_{w}'] == 3), 1, 0) for w in self.end_waves],
            ),
            'is_parent_3_more': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_and(x.is_parent == 1, x[f'child18num_{w}'] > 2), 1, 0) for w in self.end_waves],
            ),
            'is_parent_4_more': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(np.logical_and(x.is_parent == 1, x[f'child18num_{w}'] > 3), 1, 0) for w in self.end_waves],
            ),
        })
        return df

    def _add_all_single_issues(self, df):
        df = self._add_issue(df, 'ideo5_XX', 'ideo', 1, 5, calc_only=True)
        df = self._add_issue(df, 'pid7_XX', 'pid', 1, 7, calc_only=True)
        df = self._add_issue(df, 'CCXX_327', 'aff_action', 1, 4)
        df = self._add_issue(df, 'CCXX_320', 'guns', 1, 3)
        df = self._add_issue(df, 'CCXX_321', 'climate_severity', 1, 5, calc_only=True)
        df = self._add_issue(df, 'CCXX_325', 'climate_jobs_env', 1, 5, calc_only=True)
        df = self._add_issue(df, 'CCXX_325', 'climate_clean_energy', 1, 2, calc_only=True)
        df = self._add_issue(df, 'CCXX_326', 'gay_marriage', 1, 2, calc_only=True)
        df = self._add_issue(df, 'CCXX_330G', 'gay_dadt', 1, 2, calc_only=True)
        return df

    def add_all_composite_issues(self, df):
        for year in self.waves:
            df = self.add_budget_composite(df, year)
            df = self.add_climate_composite(df, year)
            df = self.add_gay_composite(df, year)
            df = self.add_military_composite(df, year)
            df = self.add_ideo_composite(df, year)

        df = self.add_immigration_composite(df)

        df = self._add_issue(df, 'budget_composite_20XX', 'budget_composite', 1, 2)
        df = self._add_issue(df, 'climate_composite_20XX', 'climate_composite', 1, 5)
        df = self._add_issue(df, 'gay_composite_20XX', 'gay_composite', 1, 2)
        df = self._add_issue(df, 'ideo_composite_20XX', 'ideo_composite', 12 / 14, 5)
        df = self._add_issue(df, 'military_composite_20XX', 'military_composite', 1, 2)
        df = self._add_issue(df, 'immigration_composite_20XX', 'immigration_composite', 1, 2)

        return df

    def add_budget_composite(self, df, year):
        # CC10_415r is taxes vs spending (examples given are of domestic spending): 0 to 100 (0 is raise taxes, 100 is cut all spending)
        # CC10_330B is SCHIP renewal: 1 renew, 2 expire
        # CC10_328 and CC10_329 are budget actions to take/avoid, respectively: 1 cut defense spending, 2 cut domestic spending, 3 raise taxes
        # Final scale is 1-2, with 1 more liberal
        df = df.assign(**{
            f'budget_strategy_{year}': lambda x: np.select(
                [
                    np.logical_and(x[f'CC{year}_328'] == 2, x[f'CC{year}_329'] != 2),
                    np.logical_and(x[f'CC{year}_329'] == 2, x[f'CC{year}_328'] != 2),
                ],
                [
                    2,  # conservative: prefer cut domestic spending, avoid anything else
                    1,  # liberal: avoid cut domestic spending, prefer anything else
                ],
                default=1.5,  # not na to avoid discarding these rows
            ),
            f'temp{year}': (df[f'CC{year}_415r'] / 100 + 1),
        })
        df[f'budget_composite_20{year}'] = ((df[f'CC{year}_415r'] / 100 + 1) + df[f'CC{year}_330B'] + df[f'budget_strategy_{year}']) / 3
        return df

    def add_climate_composite(self, df, year):
        # CC10_321 is climate change: 1-5 with 1 liberal
        # CC10_325 is jobs vs environment: 1-5 with 1 liberal
        # CC10_330C is clean energy act, with 1 support, 2, oppose, and other values invalid: count 1 as 1, 2, as 5
        # Composite is 1-5, with lower values more liberal
        df = self.nan_out_of_bounds(df, f'CC{year}_330C', 1, 2)
        df[f'climate_composite_20{year}'] = (df[f'CC{year}_321'] + df[f'CC{year}_325'] + ((df[f'CC{year}_330C'] - 1) * 4 + 1)) / 3
        return df

    def add_gay_composite(self, df, year):
        # CC10_326 is gay marriage ban: 1 support, 2 oppose
        # CC10_330G is ending don't ask don't tell: 1 support, 2 oppose, others invalid
        df = self.nan_out_of_bounds(df, f'CC{year}_330G', 1, 2)
        df[f'gay_composite_20{year}'] = (df[f'CC{year}_326'] + df[f'CC{year}_330G']) / 2
        return df

    def add_military_composite(self, df, year):
        # yes/no questions on military force usage
        df[f'military_composite_20{year}'] = np.sum(df.loc[:, df.columns.str.startswith(f'CC{year}_414_')], axis=1) / 7
        return df

    def add_ideo_composite(self, df, year):
        # Ideology composite that combines ideo and pid
        df[f'ideo_composite_20{year}'] = (df[f'ideo5_{year}'] * 7 + df[f'pid7_{year}'] * 5) / 7 / 2  # ~5-point composite scale
        return df

    def add_immigration_composite(self, df):
        # CC10_322_1-CC10_322_7 are all yes/no immigration questions, 8 and 9 are "nothing"/"none of the above" which aren't clearly liberal or conservative
        # 2010 has data for 1-3, 2012 and 2014 have data for 1-6
        df[f'immigration_composite_2010'] = np.sum(df.loc[:, df.columns.str.contains('CC10_322_[1-3]')], axis=1) / 3
        df[f'immigration_composite_2012'] = np.sum(df.loc[:, df.columns.str.contains('CC12_322_[1-6]')], axis=1) / 6
        df[f'immigration_composite_2014'] = np.sum(df.loc[:, df.columns.str.contains('CC14_322_[1-6]')], axis=1) / 6
        return df

    def add_before_after(self, df, before_pattern, issue, lower_bound=None, upper_bound=None):
        df = df.assign(**{
            f'{issue}_before': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [x[before_pattern.replace('XX', str(w))] for w in self.start_waves],
            ),
            f'{issue}_after': lambda x:np.select(
                [x.start_wave == w for w in self.waves[:-1]],
                [x[before_pattern.replace('XX', str(w))] for w in self.end_waves],
            ),
        })
        df = self.nan_out_of_bounds(df, f'{issue}_before', lower_bound, upper_bound)
        df = self.nan_out_of_bounds(df, f'{issue}_after', lower_bound, upper_bound)
        return df

    def _add_issue(self, df, before_pattern, issue, lower_bound=None, upper_bound=None, calc_only=False):
        df = self.add_before_after(df, before_pattern, issue, lower_bound, upper_bound)

        df = df.assign(**{
            f'{issue}_delta': lambda x: x[f'{issue}_after'] - x[f'{issue}_before'],
            f'{issue}_delta_abs': lambda x: abs(x[f'{issue}_delta']),
            f'{issue}_delta_sq': lambda x: x[f'{issue}_delta'] * x[f'{issue}_delta'],
            f'{issue}_direction': lambda x: np.sign(x[f'{issue}_delta']),
        })
        df.loc[np.isnan(df[f'{issue}_delta']), f'{issue}_direction'] = np.nan # because some of the 0s should be NaN

        df = df.assign(**{
            f'{issue}_persists': lambda x: np.select(
                [x.start_wave == w for w in self.start_waves[:-1]],
                [np.where(np.logical_and(
                    x[f'{issue}_delta'] != 0, # change in start vs end
                    # change from start to final is either zero or the same direction as delta
                    x[f'{issue}_delta'] * (x[before_pattern.replace('XX', str(self.end_waves[-1]))] - x[before_pattern.replace('XX', str(self.end_waves[i]))]) >= 0
                ),
                x[before_pattern.replace('XX', str(self.end_waves[-1]))] - x[before_pattern.replace('XX', str(w))],
                0) for i, w in enumerate(self.start_waves[:-1])]
            )
        })
        for wave in self.waves:
            df.loc[df['start_wave'] == self.start_waves[-1], f'{issue}_persists'] = np.nan  # Can't calculate when there are only two waves
            df.loc[np.isnan(df[before_pattern.replace('XX', str(wave))]), f'{issue}_persists'] = np.nan  # Can't calculate unless all waves are available
        df[f'{issue}_persists_abs'] = np.abs(df[f'{issue}_persists'])

        if not calc_only:
            self.ISSUES.add(issue)
            self.ISSUE_BOUNDS[issue] = (lower_bound, upper_bound)

        return df
