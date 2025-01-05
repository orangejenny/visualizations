import numpy as np
import pandas as pd

from parents_politics_panel import Demographic, DemographicType, ParentsPoliticsPanel

class CESPanel(ParentsPoliticsPanel):
    waves = [10, 12, 14]
    treatments = {'firstborn', 'new_child', 'is_parent'}
    dob_column = 'birthyr_10'

    # These do change. Not gender, but even race, and definitely employment, marital status, education, church, even division.
    _demographics = [
        Demographic(name="gender", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=2, top_categories=[1,2]),
        Demographic(name="race", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,3]),
        Demographic(name="employ", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,4]),
        #Demographic(name="investor", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=2, top_categories=None),
        Demographic(name="educ", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),
        Demographic(name="marstat", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=[1,5]),
        # There are other religion questions, but this one is used in CES's own sample matching
        Demographic(name="pew_churatd", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),
        Demographic(name="ownhome", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=3, top_categories=[1,2]),

        # constructed
        # From USDA codes: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
        Demographic(name="RUCC_2023", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
        # Census division: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
        Demographic(name="division", dtype=DemographicType.CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
        Demographic(name="age", dtype=DemographicType.CONTINUOUS, lower_bound=None, upper_bound=None, top_categories=None),
        Demographic(name="income", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
        # Duplicative with income and less granular, although it's linear in some sense
        #Demographic(name="income_quintile", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
    ]
    # For constructed demographics; these can probably be moved into normal Demographic.lower_bound and Demographic.upper_bound
    _demographic_viz_boundaries = {
        'age': (18, 40),
        'income': (1, 17),
        'pew_churatd': (1, 6),
        'RUCC_2023': (1, 9),
    }
    _demographic_viz_labels = {
        'age': 'Age (18-40)',
        'employ': 'Employment',
        'educ': 'Education',
        'marstat': 'Marital Status',
        'pew_churatd': 'Religiosity',
        'ownhome': 'Homeowner',
        'RUCC_2023': 'Urbanization',
    }
    _demographic_category_names = {
        "gender": {1: 'male', 2: 'female'},
        "race": {1: 'white', 2: 'black', 3: 'hispanic'},
        "employ": {1: 'full-time', 2: 'part-time', 4: 'unemployed'},
        "marstat": {1: 'married', 5: 'single'},
        "ownhome": {1: 'own', 2: 'rent'},
    }
    _issue_viz_labels = {
        '_ideo_composite': 'Ideology',
        'aff_action': 'Affirmative action',
        'budget_composite': 'Budget priorities',
        'gay_composite': 'Gay rights',
        'guns': 'Gun control',
    }

    def _load_panel(cls):
        return pd.read_stata("~/Documents/visualizations/midterm/CCES_Panel_Full3waves_VV_V4.dta", convert_categoricals=False)  # n=9500

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
            self.panel.columns.str.contains("CC1[024]_324", regex=True) + # abortion (1-4 conservative to liberal)
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

    def _recode_issues(self, df):
        # Recode a few columns to streamline later calculations
        for year in self.waves:
            # Recode guns to be continuous (swapping 2 and 3 so that "no change" is in the middle of "less strict" and "more strict")
            label = f'CC{year}_320'
            df.loc[:, label] = np.where(df[label] == 2, 3, np.where(df[label] == 3, 2, np.where(df[label] == 1, 1, np.nan)))

            # Reverse abortion, so that 1 is liberal and 4 conservative
            label = f'CC{year}_324'
            df.loc[:, label] = np.where(df[label] == 1, 4, np.where(df[label] == 2, 3, np.where(df[label] == 3, 2, np.where(df[label] == 4, 1, np.nan))))

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
            df[f'CC{year}_414_7'] = np.where(df[f'CC{year}_414_7'] == 1, 2, np.where(df[f'CC{year}_414_7'] == 2, 1, np.nan))

            # Flip the 2 yes/no immigration questions that are opposite polarity of the other 5
            # For 1,7, 1 is more liberal and 2 is more conservative
            # For 2,3,4,5,6, 1 is more conservative and 2 is more liberal
            df[f'CC{year}_322_1'] = np.where(df[f'CC{year}_322_1'] == 1, 2, np.where(df[f'CC{year}_322_1'] == 2, 1, np.nan))
            if year == 10:  # only asked in 2010
                df[f'CC{year}_322_7'] = np.where(df[f'CC{year}_322_7'] == 1, 2, np.where(df[f'CC{year}_322_7'] == 2, 1, np.nan))
        return df

    def _recode_demographics(self, df):
        # Nothing to do here
        return df

    def _consolidate_demographics(self, df):
        for dname, demographic in self.demographics.items():
            if demographic.lower_bound is None and demographic.upper_bound is None:
                continue

            old_labels = [f'{dname}_{wave}' for wave in self.waves]
            for old_label in old_labels:
                df = self.nan_out_of_bounds(df, old_label, demographic.lower_bound, demographic.upper_bound)

            # Use "after" data if available, else use most recent value
            df = df.assign(**{dname: lambda x: np.select(
                [x.end_wave == w for w in self.end_waves],
                [np.where(
                    pd.notna(x[f'{dname}_{w}']),
                    x[f'{dname}_{w}'],
                    x[old_labels].bfill(axis=1).iloc[:, 0]
                ) for i, w in enumerate(self.end_waves)],
            )})
            df.drop(old_labels, axis=1, inplace=True)
        return df

    def add_rural_urban(self, df):
        df = df.assign(countyfips_14=np.logical_or(df['countyfips_14'], 0))
        df = df.astype({
            'countyfips_10': 'int64',
            'countyfips_12': 'int64',
            'countyfips_14': 'int64',
        })
        df = df.assign(
            countyfips_before=lambda x:np.select(
                [x.start_wave == w for w in self.start_waves],
                [x[f'countyfips_{w}'] for w in self.start_waves],
            )
        )
        codes = pd.read_csv("~/Documents/visualizations/midterm/ruralurbancontinuumcodes2023/rural_urban.csv")
        codes = codes.loc[:,['FIPS', 'State', 'RUCC_2023']] #, 'Description']]
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

        divisions = divisions.merge(codes, how='inner', left_on='state', right_on='State')
        df = df.merge(codes, how='left', left_on='countyfips_before', right_on='FIPS')

        divisions.drop(['state', 'State'], axis=1, inplace=True)
        df = df.merge(divisions, how='left', left_on='countyfips_10', right_on='FIPS', suffixes=('', '_10'))
        df = df.merge(divisions, how='left', left_on='countyfips_12', right_on='FIPS', suffixes=('', '_12'))
        df = df.merge(divisions, how='left', left_on='countyfips_14', right_on='FIPS', suffixes=('', '_14'))

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

    def add_parenthood_indicator(self, df):
        # parenthood is based on child18 (parent of minor children? yes/no) and child18num (number of minor children)
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

        return df

    def add_parenting_dosage_indicators(self, df):
        df = df.assign(**{
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
        df = self.add_issue(df, 'ideo5_XX', 'ideo', 1, 5, calc_only=True)
        df = self.add_issue(df, 'pid7_XX', 'pid', 1, 7, calc_only=True)
        df = self.add_issue(df, 'CCXX_327', 'aff_action', 1, 4)
        df = self.add_issue(df, 'CCXX_320', 'guns', 1, 3)
        df = self.add_issue(df, 'CCXX_321', 'climate_severity', 1, 5, calc_only=True)
        df = self.add_issue(df, 'CCXX_325', 'climate_jobs_env', 1, 5, calc_only=True)
        df = self.add_issue(df, 'CCXX_325', 'climate_clean_energy', 1, 2, calc_only=True)
        df = self.add_issue(df, 'CCXX_326', 'gay_marriage', 1, 2, calc_only=True)
        df = self.add_issue(df, 'CCXX_330G', 'gay_dadt', 1, 2, calc_only=True)
        df = self.add_issue(df, 'CCXX_324', 'abortion', 1, 4)
        return df

    def add_all_composite_issues(self, df):
        for year in self.waves:
            df = self.add_budget_composite(df, year)
            df = self.add_climate_composite(df, year)
            df = self.add_gay_composite(df, year)
            df = self.add_military_composite(df, year)
            df = self.add_ideo_composite(df, year)

        df = self.add_immigration_composite(df)

        df = self.add_issue(df, 'budget_composite_20XX', 'budget_composite', 1, 2)
        df = self.add_issue(df, 'climate_composite_20XX', 'climate_composite', 1, 5)
        df = self.add_issue(df, 'gay_composite_20XX', 'gay_composite', 1, 2)
        df = self.add_issue(df, '_ideo_composite_20XX', '_ideo_composite', 6, 35)
        df = self.add_issue(df, 'military_composite_20XX', 'military_composite', 1, 2)
        df = self.add_issue(df, 'immigration_composite_20XX', 'immigration_composite', 1, 2)

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
        df[f'_ideo_composite_20{year}'] = (df[f'ideo5_{year}'] * 7 + df[f'pid7_{year}'] * 5) / 2  # ~5-point composite scale
        return df

    def add_immigration_composite(self, df):
        # CC10_322_1-CC10_322_7 are all yes/no immigration questions, 8 and 9 are "nothing"/"none of the above" which aren't clearly liberal or conservative
        # 2010 has data for 1-3, 2012 and 2014 have data for 1-6
        df[f'immigration_composite_2010'] = np.sum(df.loc[:, df.columns.str.contains('CC10_322_[1-3]')], axis=1) / 3
        df[f'immigration_composite_2012'] = np.sum(df.loc[:, df.columns.str.contains('CC12_322_[1-6]')], axis=1) / 6
        df[f'immigration_composite_2014'] = np.sum(df.loc[:, df.columns.str.contains('CC14_322_[1-6]')], axis=1) / 6
        return df
