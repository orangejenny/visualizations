import numpy as np

from ces import CESPanel
from yougov import YouGovPanel

ces = CESPanel()
panel = ces.get_panel()
three_years = ces.get_all_waves()
two_years = ces.get_paired_waves()

yougov = YouGovPanel()

##########################
# Analysis: Demographics #
##########################

# In paired waves: 420 with new child, 18580 without
counts = ces.get_paired_waves().groupby('new_child', as_index=False).count()

# In all waves: 229 with new child in 2012, 9271 without
counts = ces.get_all_waves().groupby('new_child', as_index=False).count()

############################
# Analysis: Ideology/Party #
############################

### Exploratory: How often do people change ideology/party between two waves?
# For pid3, 0.8%, too coarse to be useful
ces.count_flippers("pid3_10", "pid3_12", 1, 2)

# For pid7, 20-25% each 2 years
ces.count_flippers("pid7_10", "pid7_12", 1, 7)
ces.count_flippers("pid7_12", "pid7_14", 1, 7)
ces.count_flippers("pid7_10", "pid7_14", 1, 7)
ces.count_flippers("ideo5_10", "ideo5_12", 1, 5)
ces.count_flippers("ideo5_12", "ideo5_14", 1, 5)
ces.count_flippers("ideo5_10", "ideo5_14", 1, 5)

### Exploratory: Ideology distribution across panel: roughly normal, skewing conservative
panel.groupby("ideo5_10").count().loc[:,'weight']

### Exploratory: Party distribution across panel
# Not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
panel.groupby("pid7_10").count().loc[:,'weight']
# Parents are still U-shaped, a little more liberal, also looks like more moderates
three_years.loc[np.equal(three_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'weight']

### Testing: ideological change: nothing significant
ces.t_test(two_years, 'ideo_delta')
ces.t_test(two_years, 'ideo_delta_abs')
ces.t_test(two_years, 'ideo_composite_delta')
ces.t_test(two_years, 'ideo_composite_delta_abs')

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
ces.t_test(young_adults, 'ideo_delta')
ces.t_test(young_adults, 'ideo_delta_abs')
ces.t_test(young_adults, 'ideo_composite_delta')
ces.t_test(young_adults, 'ideo_composite_delta_abs')

### Descriptive: ideological change
# Average ideological change over two years: trivially liberal, moreso for non-new-parents
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude
# New parents: 12% more liberal, 11% more conservative
# Non-new-parents: 12% more liberal, 10% more conservative
ces.count_percentages(ces.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo_direction')

# Using firstborn instead of new_child is still trivial, but new parents more conservative
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), 'firstborn', 'ideo')

# Younger adults look about the same as the whole cohort, trivially more liberal
ces.summarize_continuous(ces.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude, for younger adults
# New parents: 20% more liberal, 9% more conservative
# Non-new-parents: 14% more liberal, 10% more conservative
ces.count_percentages(ces.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo_direction')

### Testing: party change: nothing significant
young_adults = two_years.loc[np.less(two_years['age'], 30),:]
ces.t_test(young_adults, 'pid_delta')
ces.t_test(young_adults, 'pid_delta_abs')
ces.t_test(two_years, 'pid_delta')
ces.t_test(two_years, 'pid_delta_abs')
 
### Descriptive: party change
# Average party change over two years: bigger than ideology, but still small
# Vaguely interesting that it's a bigger change. Could just be that it's a bigger scale.
ces.summarize_continuous(ces.filter_na(two_years, 'pid_delta'), 'new_child', 'pid')
ces.summarize_continuous(ces.filter_na(two_years, 'pid_delta'), 'firstborn', 'pid')
ces.summarize_continuous(ces.filter_na(young_adults, 'pid_delta'), 'firstborn', 'pid')

####################
# Analysis: Issues #
####################

### Testing: continuous & composite issues
# "After" views: nothing
ces.t_tests(two_years, 'after')

# Change, incorporating direction: nothing
ces.t_tests(two_years, 'delta')

# Change, absolute value: climate change, gay, guns: climate change & climate composite persist, and oddly so does sales or inc
ces.t_tests(two_years, 'delta_abs')
ces.t_tests(two_years, 'delta_sq')

# Persistent change: nothing
ces.t_tests(three_years, 'persists')

# Persistent absolute change: climate change, tax/spend, climate composite, gay rights composite
ces.t_tests(three_years, 'persists_abs')

# Switching to firstborn and looking at change: for absolute change, climate change, climate composite and gay rights composite
ces.t_tests(two_years, 'delta', 'firstborn')
ces.t_tests(two_years, 'delta_abs', 'firstborn')
ces.t_tests(two_years, 'delta_sq', 'firstborn')

# Summary of continuous & composite issues
ces.summarize_continuous(two_years, "new_child", "climate_change")
ces.summarize_continuous(two_years, "new_child", "jobs_env")
ces.summarize_continuous(two_years, "new_child", "aff_action")
ces.summarize_continuous(two_years, "new_child", "tax_or_spend")
ces.summarize_continuous(two_years, "new_child", "sales_or_inc")
ces.summarize_continuous(two_years, "new_child", "climate_composite")
ces.summarize_continuous(two_years, "new_child", "gay_composite")
ces.summarize_continuous(two_years, "new_child", "immigration_composite")

# Non-response rates, continuous & composite issues
# Do non-response rates differ for parents and non-parents?
total = len(two_years)
for prefixes, suffix in ((ces.CONTINUOUS_PREFIXES, 'delta'), (ces.CATEGORICAL_PREFIXES, 'change')):
    for issue in prefixes:
        missing = len(two_years.loc[np.isnan(two_years[f'{issue}_{suffix}']),:])
        #print(f"Non-response for {issue}_{suffix}: {round(missing * 100 / total, 2)}%")

### Testing: categorical variables: both budget questions, but neither persists
ces.chisqs(two_years, 'after')
ces.chisqs(two_years, 'change')
ces.chisqs(three_years, 'persists')

# Descriptive statistics on categorical issues
ces.count_percentages(two_years, 'new_child', 'gay_marriage_before')

ces.count_percentages(two_years, 'new_child', 'schip_before')
ces.count_percentages(two_years, 'new_child', 'schip_after')

ces.count_percentages(two_years, 'new_child', 'budget_before')
ces.count_percentages(two_years, 'new_child', 'budget_after')

ces.count_percentages(two_years, 'new_child', 'budget_avoid_before')

# Persistence: how common is persistent change?
# Of the new parents who changed, how many keep that change?
# New parents often slightly more likely to experience persistent change than others
ces.continuous_persists("climate_change") # 25% vs 18%
ces.continuous_persists("jobs_env") # 23% vs 23%
ces.continuous_persists("aff_action") # 16% vs 16%
ces.continuous_persists("guns") # 16% vs 14%
ces.continuous_persists("tax_or_spend") # 29% vs 35%
ces.continuous_persists("sales_or_inc") # 39% vs 36%
ces.continuous_persists("climate_composite") # 30% vs 21%
ces.continuous_persists("gay_composite") # 17% vs 11%
ces.continuous_persists("military_composite") # 32% vs 30%
ces.continuous_persists("immigration_composite") # 50% vs 48%
 
ces.categorical_persists("gay_marriage") # 10% vs 7%
ces.categorical_persists("schip") # 15% vs 12%
ces.categorical_persists("budget") # 15% vs 12%
ces.categorical_persists("budget_avoid") # 16% vs 16%

####################
# Analysis: Gender #
####################
two_years_new_parents = two_years.loc[np.equal(two_years['new_child'], 1),:]
three_years_new_parents = three_years.loc[np.equal(three_years['new_child'], 1),:]
two_years_men = two_years.loc[np.equal(two_years['gender'], 1),:]
three_years_men = three_years.loc[np.equal(three_years['gender'], 1),:]
two_years_women = two_years.loc[np.equal(two_years['gender'], 2),:]
three_years_women = three_years.loc[np.equal(three_years['gender'], 2),:]

### Ideology/party description
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'ideo')
ces.summarize_continuous(ces.filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'pid')

### Ideology/party + continuous issues
# Compare new fathers to new mothers: tax vs spend; absolute aff action, jobs env,, climate composite, military composite
ces.t_tests(two_years_new_parents, 'delta', 'gender', a_value=1, b_value=2)
ces.t_tests(two_years_new_parents, 'delta_abs', 'gender', a_value=1, b_value=2)

# Compare new fathers to other men: tax or spend with direction...doesn't persist
ces.t_tests(two_years_men, 'delta')
ces.t_tests(two_years_men, 'delta_abs')
ces.t_test(three_years_men, 'tax_or_spend_persists')
ces.t_test(three_years_men, 'tax_or_spend_persists_abs')

# Compare new mothers to other women: absolute climate, gay, guns
ces.t_tests(two_years_women, 'delta')
ces.t_tests(two_years_women, 'delta_abs')
ces.t_test(three_years_women, 'climate_change_persists_abs')
ces.t_test(three_years_women, 'guns_persists')

### Describe continuous issues
for prefix in ces.CONTINUOUS_PREFIXES:
    ces.summarize_continuous(two_years_new_parents, "gender", prefix)
    ces.summarize_continuous(two_years, ["new_child", "gender"], prefix)

# New parents vs other: persistent abs: sales or inc
ces.t_tests(three_years_new_parents, 'persists', 'gender', a_value=1, b_value=2)
ces.t_tests(three_years_new_parents, 'persists_abs', 'gender', a_value=1, b_value=2)

ces.summarize_continuous(two_years_women, "new_child", "climate_change")
ces.summarize_continuous(two_years_women, "new_child", "guns")
ces.summarize_continuous(two_years_women, "new_child", "climate_composite")
ces.summarize_continuous(two_years_women, "new_child", "gay_composite")

# Mothers vs non-mothers: persists absolute value: gay composite
ces.t_tests(three_years_women, 'persists')
ces.t_tests(three_years_women, 'persists_abs')

### Categorical issues
# Compare new fathers to new mothers: both budget questions: budget persists
ces.chisqs(three_years_new_parents, "persists", "gender")

# Comparing new fathers to new mothers on budget_change
ces.count_percentages(two_years_new_parents, "gender", "budget_before")
ces.count_percentages(two_years_new_parents, "gender", "budget_after")
ces.count_percentages(two_years_new_parents, "gender", "budget_change")

# Compare new fathers to other men: nothing
ces.chisqs(two_years_men, 'change')

# Compare new mothers to other women: both budget questions: budget persists
ces.chisqs(two_years_women, 'change')
ces.chisqs(three_years_women, "persists")

# Comparing new mothers to other women on budget_change and budget_change_avoid
ces.count_percentages(two_years_women, "new_child", "budget_before")
ces.count_percentages(two_years_women, "new_child", "budget_after")
ces.count_percentages(two_years_women, "new_child", "budget_avoid_before")
ces.count_percentages(two_years_women, "new_child", "budget_avoid_after")


####################
# Analysis: Income #
####################
# Exploratory: what does the income distribution look like across the panel?
panel.loc[:, ['caseid', 'faminc_14']].groupby("faminc_14").count()

# Exploratory: what does the income distribution look like for new parents?
three_years.loc[np.equal(three_years['new_child'], 1),['income', 'new_child', 'caseid']].groupby('income').count()
three_years.loc[:,['new_child', 'income_quintile', 'caseid']].groupby(['new_child', 'income_quintile']).count()
three_years.loc[:,['high_income', 'caseid']].groupby('high_income').count()
three_years.loc[:,['low_income', 'caseid']].groupby('low_income').count()

two_years_high_income = two_years.loc[np.equal(two_years['high_income'], 1),:]
three_years_high_income = three_years.loc[np.equal(three_years['high_income'], 1),:]
two_years_low_income = two_years.loc[np.equal(two_years['high_income'], 0),:]
three_years_low_income = three_years.loc[np.equal(three_years['high_income'], 0),:]
ces.chisqs(two_years_women, 'change')

# Ideology & party: nothing
ces.summarize_continuous(ces.filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'ideo')
ces.summarize_continuous(ces.filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'pid')

# Comparing high-income new parents with other new parents: continuous: climate change
ces.t_tests(two_years_new_parents, 'delta', 'high_income')
ces.t_tests(two_years_new_parents, 'delta_abs', 'high_income')

ces.summarize_continuous(two_years_new_parents, "high_income", "climate_change")
ces.summarize_continuous(two_years_new_parents, "high_income", "climate_composite")

# Comparing high-income new parents with other high-income people: nothing
ces.t_tests(two_years_high_income, 'delta')
ces.t_tests(two_years_high_income, 'delta_abs')

# Comparing low-income new parents with other low-income people: climate change, jobs env, guns
ces.t_tests(two_years_low_income, 'delta')
ces.t_tests(two_years_low_income, 'delta_abs')

for prefix in ces.CONTINUOUS_PREFIXES:
    ces.summarize_continuous(two_years, ["new_child", "high_income"], prefix)

ces.t_test(three_years_new_parents, "climate_change_persists", "high_income")
ces.t_test(three_years_new_parents, "climate_change_persists_abs", "high_income")
# TODO: T tests for climate_composite return NaN
ces.t_test(three_years_new_parents, "climate_composite_persists", "high_income")
ces.t_test(three_years_new_parents, "climate_composite_persists_abs", "high_income")

# Chi square tests within new parents: high_income, low_income: nothing
ces.chisqs(two_years_new_parents, 'change', 'high_income')

# Chi square tests within high income: nothing
ces.chisqs(two_years_high_income, 'change')

# Chi square tests within low income: budget
ces.chisqs(two_years_low_income, 'change')
