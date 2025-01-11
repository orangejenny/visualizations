import numpy as np
import pandas as pd
import pdb

from scipy.stats import zscore

from parents_politics_panel import Demographic, DemographicType, ParentsPoliticsPanel

class YouGovPanel(ParentsPoliticsPanel):
    waves = [2011, 2016, 2017]    # These are the years that have a parenting question, household_children_x
    treatments = {'firstborn', 'is_parent'}     # No new_child treatment because number of children (household_child_num_x) isn't available in the same waves
    dob_column = 'birthyr_2011'

    _demographics = [
        Demographic(name="gender", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=2, top_categories=[1,2]),  # 2011 only
        Demographic(name="race", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,3]),
        Demographic(name="marstat", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=[1, 5]),
        Demographic(name="educ", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),
        Demographic(name="employment", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,4]),

        # Chosen to be most similar to pew_churatd in CES
        Demographic(name="religservice", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),

        # constructed
        # From USDA codes: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
        Demographic(name="RUCC_2023", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
        # Census division: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
        Demographic(name="division", dtype=DemographicType.CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
        Demographic(name="age", dtype=DemographicType.CONTINUOUS, lower_bound=None, upper_bound=None, top_categories=None),
        Demographic(name="income", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=None, upper_bound=None, top_categories=None),
    ]
    # For constructed demographics; these can probably be moved into normal Demographic.lower_bound and Demographic.upper_bound
    _demographic_viz_boundaries = {
        'age': (18, 40),
    }
    _demographic_viz_labels = {}
    _demographic_category_names = {
        "gender": {1: 'male', 2: 'female'},
    }
    _issue_viz_labels = {}

    def _load_panel(cls):
        # TODO: revisit problems encountered in parsing (54 problems, relating to unexpected values in cols 180-191)
        return pd.read_csv("~/Documents/visualizations/midterm/VOTER Panel Data Files/voter_panel.csv", encoding="latin_1")

    def _trimmed_panel(self):
        # Drop most columns
        return self.panel.loc[
            :,
            self.panel.columns.str.startswith('weight_') +

            # Ideology and partisanship
            self.panel.columns.str.startswith('ideo5_') +
            self.panel.columns.str.startswith('pid7_') +

            # Policy issues
            self.panel.columns.str.startswith('issue_') +
            self.panel.columns.str.startswith('abortidentity_') +
            self.panel.columns.str.startswith('view_abortlegal_') +
            self.panel.columns.str.startswith('affirmativeaction_') +
            self.panel.columns.str.startswith('egalitarian_') +
            self.panel.columns.str.startswith('env') +
            self.panel.columns.str.startswith('view_gaymar') +
            self.panel.columns.str.startswith('view_transgender') +
            self.panel.columns.str.startswith('govt_') +
            self.panel.columns.str.startswith('govtreg_') +
            self.panel.columns.str.startswith('immi_') +
            self.panel.columns.str.startswith('healthreformbill_') +
            self.panel.columns.str.startswith('univhealthcov_') +
            self.panel.columns.str.startswith('immi_') +
            self.panel.columns.str.startswith('sexism_') +
            self.panel.columns.str.startswith('wealth_') +
            self.panel.columns.str.startswith('economicbias_') +
            self.panel.columns.str.startswith('income_redistribution_') +
            self.panel.columns.str.startswith('taxdoug_') +
            self.panel.columns.str.startswith('taxwealth_') +
            self.panel.columns.str.startswith('view_deathpen') +

            # Parenthood
            self.panel.columns.str.startswith("household_children_") +

            # Demographics for controls/filters
            self.panel.columns.str.startswith("birthyr_") +
            self.panel.columns.str.startswith("faminc_") +
            self.panel.columns.str.startswith("gender_") +
            self.panel.columns.str.startswith("race_") +
            self.panel.columns.str.startswith("educ_") +
            self.panel.columns.str.startswith("marstat_") +
            self.panel.columns.str.startswith("religservice_") +
            self.panel.columns.str.startswith("inputstate_") +
            self.panel.columns.str.startswith("employment_") +
            self.panel.columns.str.startswith("employmentnonstud_2011") +
            self.panel.columns.str.startswith("student2_2011")
        ].copy()

    def _recode_issues(self, df):
        for year in self.waves:
            if year != 2017:
                # Reverse view_deathpen, so that 1 is liberal and 2 conservative
                label = f'view_deathpen_{year}'
                df.loc[:, label] = np.where(df[label] == 2, 1, np.where(df[label] == 1, 2, np.nan))

        return df

    def _recode_demographics(self, df):
        # TODO: implement
        '''
        Employment options vary by wave
            2011
                student2_2011
                    1 Full-time student
                    2 Part-time student
                    3 Not a student
                employmentnonstud_2011
                    1 Full-time employed
                    2 Part-time employed
                    3 Self-employed
                    4 Unemployed or temporarily on layoff
                    5 Retired
                    6 Permanently disabled
                    7 Homemaker
            2016 (employment_2016)
                1 Full-time
                2 Part-time
                3 Temporarily laid off
                4 Unemployed
                5 Retired
                6 Permanently disabled
                7 Homemaker
                8 Student
            2017 (employment_2017)
                1 Employed full-time for 35 hours or more per week
                2 Employed part-time for less than 35 hours per week
                3 Unemployed for less than 6 months
                4 Unemployed for more than 6 months
                5 Homemaker
                6 Retired
                7 Student
        '''
        return df

    def add_geography(self, df):
        divisions = self.get_divisions()

        # TODO: implement? inputstate is an integer, need to map those to states
        #for wave in self.waves:
        #    df = df.merge(divisions, how='left', left_on=f'inputstate_{wave}', right_on='state', suffixes=('', f'_{wave}'))
        #    df.drop(['state'], axis=1, inplace=True)

        return df

    def add_income_quintile(self, df):
        df = df.rename(columns={'faminc_2011': 'income'})
        df = self.nan_out_of_bounds(df, 'income', 1, 50)  # good enough to get rid of 98/99
        df = df.assign(
            income_quintile=lambda x:np.select(
                [
                    # note the 10 response could go into either 4th or 5th quintile
                    x.income == n for n in [x for x in range(1, 12)] + [31]
                ],
                [1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5],
                default=np.nan
            ),
        )
        return df

    def add_parenthood_indicator(self, df):
        # parenthood is based on household_children (parent of minor children? yes/no)
        # This only assigns values 0, 1, and 3, because data about number of children is not available for all waves
        df = df.assign(
            parenthood=lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(
                    # household_children_start is NA or household_children_next is NA,
                    np.logical_or(
                        np.logical_and(x[f'household_children_{w}'] != 1, x[f'household_children_{w}'] != 2),
                        np.logical_and(x[f'household_children_{self.waves[i + 1]}'] != 1, x[f'household_children_{self.waves[i + 1]}'] != 2),
                    ),
                    np.nan,
                    np.where(
                        x[f'household_children_{w}'] == 2,  # household_children_start is no, so not a parent at the start
                        np.where(
                            x[f'household_children_{self.waves[i + 1]}'] == 1,  # household_children_next is yes, so had a child at the end
                            1, # first child
                            0  # no children
                        ),
                        3 # parent, unknown whether there was a change in the number of children
                    )
                ) for i, w in enumerate(self.start_waves)],
            )
        )
        return df

    def add_parenting_dosage_indicators(self, df):
        # YouGov doesn't have these, due to household_children and household_child_num not being in all the same waves
        return df

    def _add_all_single_issues(self, df):
        df = self.add_issue(df, 'ideo5_XX', 'ideo', 1, 5, calc_only=True)
        df = self.add_issue(df, 'pid7_XX', 'pid', 1, 7, calc_only=True)

        df = self.add_issue(df, 'view_abortlegal_XX', 'abortion', 1, 3, waves=[2011, 2016])
        df = self.add_issue(df, 'affirmativeaction_XX', 'aff_action', 1, 2, waves=[2011, 2016])
        df = self.add_issue(df, 'view_gaymar_XX', 'gay_marriage', 1, 2, waves=[2011, 2016])
        df = self.add_issue(df, 'govtreg_business_XX', 'govt_reg_business', 1, 3)
        df = self.add_issue(df, 'univhealthcov_XX', 'universal_health', 1, 2)

        for issue in ['abort', 'budget', 'econ', 'educ', 'envi', 'gayrights', 'healthcare', 'immig', 'medicare', 'socsec', 'taxes', 'terror']:
            df = self.add_issue(df, f'issue_{issue}_XX', f'imp_{issue}', 1, 4)

        for issue in ['climatechange', 'crime', 'econ', 'fml', 'gendereq', 'govsize', 'infra', 'jobs', 'moneypol', 'poverty', 'raceeq', 'religlib']:
            df = self.add_issue(df, f'issue_{issue}_XX', f'imp_{issue}', 1, 4, waves=[2016, 2017])

        return df

    # TODO: double-check all of these
    def add_all_composite_issues(self, df):
        for year in self.waves:
            df = self.add_immigration_composite(df, year)
            if year != 2017:
                df = self.add_climate_composite(df, year)
                df = self.add_deathpen_composite(df, year)

        df = self.add_issue(df, 'climate_composite_XX', 'climate_composite', 1, 4, waves=[2011, 2016])
        df = self.add_issue(df, 'deathpen_composite_XX', 'deathpen_composite', 1, 5, waves=[2011, 2016])
        df = self.add_issue(df, 'immigration_composite_XX', 'immigration_composite', 1, 5)

        return df

    def add_climate_composite(self, df, year):
        # envwarm is belief in global warming: 1-4 with 1 liberal - is it fair to call belief in climate change liberal?
        # envcause is cause of global warming: 1 for human, 2 for natural causes - same question as other indicator
        # Composite is 1-4, with lower values more liberal
        df = self.nan_out_of_bounds(df, f'envwarm_{year}', 1, 4)
        df = self.nan_out_of_bounds(df, f'envcause_{year}', 1, 2)
        df[f'climate_composite_{year}'] = (df[f'envwarm_{year}'] + ((df[f'envcause_{year}'] - 1) * 4 + 1)) / 2
        return df

    def add_deathpen_composite(self, df, year):
        # view_deathpen is favoring death penalty: 1 for favor, 2 for oppose
        # view_deathpenfreq is cause of global warming: 1 for human, 2 for natural causes - same question as other indicator
        # Composite is 1-5, with lower values more liberal
        df = self.nan_out_of_bounds(df, f'view_deathpen_{year}', 1, 2)
        df = self.nan_out_of_bounds(df, f'view_deathpenfreq_{year}', 1, 3)
        df[f'deathpen_composite_{year}'] = ((df[f'view_deathpen_{year}'] - 1) * 2 + df[f'view_deathpenfreq_{year}']) / 2
        return df

    def add_immigration_composite(self, df, year):
        # immi_naturalize is favoring a pathway to citizenship for uncodument immigrants: 1 favor, 2 oppose
        # immi_makedifficult is whether it should be easier or harder to migrate: 1-5 with 1 liberal
        # Composite is 1-5, with lower values more liberal
        df = self.nan_out_of_bounds(df, f'immi_naturalize_{year}', 1, 2)
        df = self.nan_out_of_bounds(df, f'immi_makedifficult_{year}', 1, 5)
        df[f'immigration_composite_{year}'] = ((df[f'immi_naturalize_{year}'] - 1) * 4 + df[f'immi_makedifficult_{year}']) / 2
        return df
