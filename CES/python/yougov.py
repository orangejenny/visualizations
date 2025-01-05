import numpy as np
import pandas as pd
import pdb

from scipy.stats import zscore

from parents_politics_panel import Demographic, DemographicType, ParentsPoliticsPanel

class YouGovPanel(ParentsPoliticsPanel):
    waves = [11, 16, 17]    # These are the years that have a parenting question, household_children_x
    treatments = {'firstborn', 'is_parent'}     # No new_child treatment because number of children (household_child_num_x) isn't available in the same waves
    dob_column = 'birthyr_2011'

    _demographics = [
        Demographic(name="gender", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=2, top_categories=[1,2]),  # 2011 only
        Demographic(name="race", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,3]),
        Demographic(name="marstat", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=[1, 5]),
        Demographic(name="educ", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),
        Demographic(name="employment", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,4]),

        # Chosen to be most similar to pew_churatd in CES
        Demographic(name="religservice_2011", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),

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
        # TODO: trim columns, as in CESPanel._trimmed_panel
        return self.panel.copy()

    def _recode_issues(self, df):
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

    def _consolidate_demographics(self, df):
        # TODO: implement
        return df

    def add_rural_urban(self, df):
        # TODO: implement (cdid_x is respondent's congressional district)
        return df

    def add_income_brackets(self, df):
        # TODO: implement (faminc_x questions)
        return df

    def add_parenthood_indicator(self, df):
        # parenthood is based on household_children (parent of minor children? yes/no)
        # This only assigns values 0, 1, and 3, because data about number of children is not available for all waves
        df = df.assign(
            parenthood=lambda x: np.select(
                [x.start_wave == w for w in self.start_waves],
                [np.where(
                    # household_children_20start is NA or household_children_20next is NA,
                    np.logical_or(
                        np.logical_and(x[f'household_children_20{w}'] != 1, x[f'household_children_20{w}'] != 2),
                        np.logical_and(x[f'household_children_20{self.waves[i + 1]}'] != 1, x[f'household_children_20{self.waves[i + 1]}'] != 2),
                    ),
                    np.nan,
                    np.where(
                        x[f'household_children_20{w}'] == 2,  # household_children_20start is no, so not a parent at the start
                        np.where(
                            x[f'household_children_20{self.waves[i + 1]}'] == 1,  # household_children_20next is yes, so had a child at the end
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
        return df

    def add_all_composite_issues(self, df):
        return df
