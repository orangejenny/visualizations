import numpy as np
import pandas as pd
import pdb

from parents_politics_panel import Demographic, DemographicType, ParentsPoliticsPanel

class YouGovPanel(ParentsPoliticsPanel):
    waves = [11, 16, 17]    # These are the years that have a parenting question, household_children_x
    treatments = {'firstborn', 'is_parent'}     # No new_child treatment because number of children (household_child_num_x) isn't available in the same waves

    # TODO: What demographics are in this survey?
    _demographics = [
        Demographic(name="gender", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=2, top_categories=[1,2]),
        Demographic(name="race", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,3]),
        Demographic(name="employ", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=8, top_categories=[1,2,4]),
        Demographic(name="educ", dtype=DemographicType.ORDERED_CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=None),
        Demographic(name="marstat", dtype=DemographicType.CATEGORICAL, lower_bound=1, upper_bound=6, top_categories=[1,5]),

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

    def add_age(self, df):
        # TODO: implement
        return df.copy()

    def _recode_issues(self, df):
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

    def _add_parenting(self, df):
        return df

    def _add_all_single_issues(self, df):
        return df

    def add_all_composite_issues(self, df):
        return df
