import pandas as pd

panel = pd.read_stata("~/Documents/visualizations/midterm/CCES_Panel_Full3waves_VV_V4.dta")  # n=9500

# Drop most columns
three_years = panel.loc[
    :,
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
]

# Add cycle
three_years = three_years.assign(cycle=lambda x: 101214)

