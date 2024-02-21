import numpy as np

from ces import CESData

ces_data = CESData()
panel = ces_data.get_panel()
three_years = ces_data.get_all_waves()
two_years = ces_data.get_paired_waves()

##########################
# Analysis: Demographics #
##########################

# In two cycles: 420 with new child, 18580 without
counts = two_years.groupby('new_child', as_index=False).count()

# In three cycles: 229 with new child in 2012, 9271 without
counts = three_years.groupby('new_child', as_index=False).count()

############################
# Analysis: Ideology/Party #
############################

### Exploratory: How often do people change ideology/party between two waves?
# For pid3, 0.8%, too coarse to be useful
ces_data.count_flippers("pid3_10", "pid3_12", 1, 2)

# For pid7, 20-25% each 2 years
ces_data.count_flippers("pid7_10", "pid7_12", 1, 7)
ces_data.count_flippers("pid7_12", "pid7_14", 1, 7)
ces_data.count_flippers("pid7_10", "pid7_14", 1, 7)
ces_data.count_flippers("ideo5_10", "ideo5_12", 1, 5)
ces_data.count_flippers("ideo5_12", "ideo5_14", 1, 5)
ces_data.count_flippers("ideo5_10", "ideo5_14", 1, 5)

### Exploratory: Ideology distribution across panel: roughly normal, skewing conservative
panel.groupby("ideo5_10").count().loc[:,'weight']

### Exploratory: Party distribution across panel
# Not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
panel.groupby("pid7_10").count().loc[:,'weight']
# Parents are still U-shaped, a little more liberal, also looks like more moderates
three_years.loc[np.equal(three_years['new_child'], 1),:].groupby("pid7_10").count().loc[:,'weight']

### Testing: ideological change: nothing significant
ces_data.t_test(two_years, 'ideo_delta')
ces_data.t_test(two_years, 'ideo_delta_abs')
ces_data.t_test(two_years, 'ideo_composite_delta')
ces_data.t_test(two_years, 'ideo_composite_delta_abs')

young_adults = two_years.loc[np.less(two_years['age'], 30),:]
ces_data.t_test(young_adults, 'ideo_delta')
ces_data.t_test(young_adults, 'ideo_delta_abs')
ces_data.t_test(young_adults, 'ideo_composite_delta')
ces_data.t_test(young_adults, 'ideo_composite_delta_abs')

### Descriptive: ideological change
# Average ideological change over two years: trivially liberal, moreso for non-new-parents
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude
# New parents: 12% more liberal, 11% more conservative
# Non-new-parents: 12% more liberal, 10% more conservative
ces_data.count_percentages(ces_data.filter_na(two_years, 'ideo_delta'), 'new_child', 'ideo_direction')

# Using firstborn instead of new_child is still trivial, but new parents more conservative
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'ideo_delta'), 'firstborn', 'ideo')

# Younger adults look about the same as the whole cohort, trivially more liberal
ces_data.summarize_continuous(ces_data.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo')

# Counts of liberal/conservative movement, ignoring magnitude, for younger adults
# New parents: 20% more liberal, 9% more conservative
# Non-new-parents: 14% more liberal, 10% more conservative
ces_data.count_percentages(ces_data.filter_na(young_adults, 'ideo_delta'), 'new_child', 'ideo_direction')

### Testing: party change: nothing significant
young_adults = two_years.loc[np.less(two_years['age'], 30),:]
ces_data.t_test(young_adults, 'pid_delta')
ces_data.t_test(young_adults, 'pid_delta_abs')
ces_data.t_test(two_years, 'pid_delta')
ces_data.t_test(two_years, 'pid_delta_abs')
 
### Descriptive: party change
# Average party change over two years: bigger than ideology, but still small
# Vaguely interesting that it's a bigger change. Could just be that it's a bigger scale.
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'pid_delta'), 'new_child', 'pid')
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'pid_delta'), 'firstborn', 'pid')
ces_data.summarize_continuous(ces_data.filter_na(young_adults, 'pid_delta'), 'firstborn', 'pid')

####################
# Analysis: Issues #
####################

### Testing: continuous & composite issues
# "After" views: nothing
ces_data.t_tests(two_years, 'after')

# Change, incorporating direction: nothing
ces_data.t_tests(two_years, 'delta')

# Change, absolute value: climate change, gay, guns: climate change & climate composite persist, and oddly so does sales or inc
ces_data.t_tests(two_years, 'delta_abs')
ces_data.t_tests(two_years, 'delta_sq')

# Persistent change: nothing
ces_data.t_tests(three_years, 'persists')

# Persistent absolute change: climate change, tax/spend, climate composite, gay rights composite
ces_data.t_tests(three_years, 'persists_abs')

# Switching to firstborn and looking at change: for absolute change, climate change, climate composite and gay rights composite
ces_data.t_tests(two_years, 'delta', 'firstborn')
ces_data.t_tests(two_years, 'delta_abs', 'firstborn')
ces_data.t_tests(two_years, 'delta_sq', 'firstborn')

# Summary of continuous & composite issues
ces_data.summarize_continuous(two_years, "new_child", "climate_change")
ces_data.summarize_continuous(two_years, "new_child", "jobs_env")
ces_data.summarize_continuous(two_years, "new_child", "aff_action")
guns = ces_data.summarize_continuous(two_years, "new_child", "guns")
assert ([round(v, 2) for v in guns.iloc[1, 1:].values] == [1.78, 1.72, -0.06, 0.29])
ces_data.summarize_continuous(two_years, "new_child", "tax_or_spend")
ces_data.summarize_continuous(two_years, "new_child", "sales_or_inc")
ces_data.summarize_continuous(two_years, "new_child", "climate_composite")
ces_data.summarize_continuous(two_years, "new_child", "gay_composite")
military = ces_data.summarize_continuous(two_years, "new_child", "military_composite")
assert ([round(v, 2) for v in military.iloc[1, 1:].values] == [1.46, 1.49, 0.02, 0.16])
ces_data.summarize_continuous(two_years, "new_child", "immigration_composite")

# Non-response rates, continuous & composite issues
# Do non-response rates differ for parents and non-parents?
total = len(two_years)
for prefixes, suffix in ((ces_data.CONTINUOUS_PREFIXES, 'delta'), (ces_data.CATEGORICAL_PREFIXES, 'change')):
    for issue in prefixes:
        missing = len(two_years.loc[np.isnan(two_years[f'{issue}_{suffix}']),:])
        #print(f"Non-response for {issue}_{suffix}: {round(missing * 100 / total, 2)}%")

### Testing: categorical variables: both budget questions, but neither persists
assert 0.8664 == round(ces_data.chisq(two_years, 'ideo_direction').pvalue, 4)
assert 0.3215 == round(ces_data.chisq(two_years, 'pid_direction').pvalue, 4)
ces_data.chisqs(two_years, 'after')
ces_data.chisqs(two_years, 'change')
ces_data.chisqs(three_years, 'persists')

# Descriptive statistics on categorical issues
ces_data.count_percentages(two_years, 'new_child', 'gay_marriage_before')
after_counts = ces_data.count_percentages(two_years, 'new_child', 'gay_marriage_after')
assert (after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].values == [39.4, 60.6]).all()

ces_data.count_percentages(two_years, 'new_child', 'schip_before')
ces_data.count_percentages(two_years, 'new_child', 'schip_after')

ces_data.count_percentages(two_years, 'new_child', 'budget_before')
ces_data.count_percentages(two_years, 'new_child', 'budget_after')

ces_data.count_percentages(two_years, 'new_child', 'budget_avoid_before')
after_counts = ces_data.count_percentages(two_years, 'new_child', 'budget_avoid_after')
assert (after_counts.loc[np.equal(after_counts['new_child'], True), 'percent'].values == [20, 35.4, 44.6]).all()


# Persistence: how common is persistent change?
# Of the new parents who changed, how many keep that change?
# New parents often slightly more likely to experience persistent change than others
ces_data.continuous_persists("climate_change") # 25% vs 18%
ces_data.continuous_persists("jobs_env") # 23% vs 23%
ces_data.continuous_persists("aff_action") # 16% vs 16%
ces_data.continuous_persists("guns") # 16% vs 14%
ces_data.continuous_persists("tax_or_spend") # 29% vs 35%
ces_data.continuous_persists("sales_or_inc") # 39% vs 36%
ces_data.continuous_persists("climate_composite") # 30% vs 21%
ces_data.continuous_persists("gay_composite") # 17% vs 11%
ces_data.continuous_persists("military_composite") # 32% vs 30%
ces_data.continuous_persists("immigration_composite") # 50% vs 48%
 
ces_data.categorical_persists("gay_marriage") # 10% vs 7%
ces_data.categorical_persists("schip") # 15% vs 12%
ces_data.categorical_persists("budget") # 15% vs 12%
ces_data.categorical_persists("budget_avoid") # 16% vs 16%

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
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'ideo')
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'ideo_delta'), ['new_child', 'gender'], 'pid')

### Ideology/party + continuous issues
# Compare new fathers to new mothers: tax vs spend; absolute aff action, jobs env,, climate composite, military composite
ces_data.t_tests(two_years_new_parents, 'delta', 'gender', a_value=1, b_value=2)
ces_data.t_tests(two_years_new_parents, 'delta_abs', 'gender', a_value=1, b_value=2)

# Compare new fathers to other men: tax or spend with direction...doesn't persist
ces_data.t_tests(two_years_men, 'delta')
ces_data.t_tests(two_years_men, 'delta_abs')
ces_data.t_test(three_years_men, 'tax_or_spend_persists')
ces_data.t_test(three_years_men, 'tax_or_spend_persists_abs')

# Compare new mothers to other women: absolute climate, gay, guns
ces_data.t_tests(two_years_women, 'delta')
ces_data.t_tests(two_years_women, 'delta_abs')
ces_data.t_test(three_years_women, 'climate_change_persists_abs')
ces_data.t_test(three_years_women, 'guns_persists')

### Describe continuous issues
for prefix in ces_data.CONTINUOUS_PREFIXES:
    ces_data.summarize_continuous(two_years_new_parents, "gender", prefix)
    ces_data.summarize_continuous(two_years, ["new_child", "gender"], prefix)

# New parents vs other: persistent abs: sales or inc
ces_data.t_tests(three_years_new_parents, 'persists', 'gender', a_value=1, b_value=2)
ces_data.t_tests(three_years_new_parents, 'persists_abs', 'gender', a_value=1, b_value=2)

ces_data.summarize_continuous(two_years_women, "new_child", "climate_change")
ces_data.summarize_continuous(two_years_women, "new_child", "guns")
ces_data.summarize_continuous(two_years_women, "new_child", "climate_composite")
ces_data.summarize_continuous(two_years_women, "new_child", "gay_composite")

# Mothers vs non-mothers: persists absolute value: gay composite
ces_data.t_tests(three_years_women, 'persists')
ces_data.t_tests(three_years_women, 'persists_abs')

### Categorical issues
# Compare new fathers to new mothers: both budget questions: budget persists
assert 0.595 == round(ces_data.chisq(two_years_new_parents, 'ideo_direction', 'gender').pvalue, 4)
assert 0.1408 == round(ces_data.chisq(two_years_new_parents, 'pid_direction', 'gender').pvalue, 4)
ces_data.chisqs(three_years_new_parents, "persists", "gender")

# Comparing new fathers to new mothers on budget_change
ces_data.count_percentages(two_years_new_parents, "gender", "budget_before")
ces_data.count_percentages(two_years_new_parents, "gender", "budget_after")
counts = ces_data.count_percentages(two_years_new_parents, "gender", "budget_change")
assert (counts['caseid_x'].values == [160,  42, 122,  86]).all()
assert (counts['percent'].values == [79.2, 20.8, 58.7, 41.3]).all()

# Compare new fathers to other men: nothing
assert 0.8416 == round(ces_data.chisq(two_years_men, 'ideo_direction').pvalue, 4)
assert 0.2836 == round(ces_data.chisq(two_years_men, 'pid_direction').pvalue, 4)
ces_data.chisqs(two_years_men, 'change')

# Compare new mothers to other women: both budget questions: budget persists
assert 0.7833 == round(ces_data.chisq(two_years_women, 'ideo_direction').pvalue, 4)
assert 0.1103 == round(ces_data.chisq(two_years_women, 'pid_direction').pvalue, 4)
ces_data.chisqs(two_years_women, 'change')
ces_data.chisqs(three_years_women, "persists")

# Comparing new mothers to other women on budget_change and budget_change_avoid
ces_data.count_percentages(two_years_women, "new_child", "budget_before")
ces_data.count_percentages(two_years_women, "new_child", "budget_after")
ces_data.count_percentages(two_years_women, "new_child", "budget_avoid_before")
ces_data.count_percentages(two_years_women, "new_child", "budget_avoid_after")


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
assert 0.7833 == round(ces_data.chisq(two_years_women, 'ideo_direction').pvalue, 4)
assert 0.1103 == round(ces_data.chisq(two_years_women, 'pid_direction').pvalue, 4)
ces_data.chisqs(two_years_women, 'change')

# Ideology & party: nothing
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'ideo')
ces_data.summarize_continuous(ces_data.filter_na(two_years, 'high_income'), ['new_child', 'high_income'], 'pid')
assert 0.517 == round(ces_data.chisq(two_years_new_parents, 'ideo_direction', 'high_income').pvalue, 4)
assert 0.5566 == round(ces_data.chisq(two_years_new_parents, 'pid_direction', 'high_income').pvalue, 4)

# Comparing high-income new parents with other new parents: continuous: climate change
ces_data.t_tests(two_years_new_parents, 'delta', 'high_income')
ces_data.t_tests(two_years_new_parents, 'delta_abs', 'high_income')

ces_data.summarize_continuous(two_years_new_parents, "high_income", "climate_change")
ces_data.summarize_continuous(two_years_new_parents, "high_income", "climate_composite")

# Comparing high-income new parents with other high-income people: nothing
ces_data.t_tests(two_years_high_income, 'delta')
ces_data.t_tests(two_years_high_income, 'delta_abs')

# Comparing low-income new parents with other low-income people: climate change, jobs env, guns
ces_data.t_tests(two_years_low_income, 'delta')
ces_data.t_tests(two_years_low_income, 'delta_abs')

for prefix in ces_data.CONTINUOUS_PREFIXES:
    ces_data.summarize_continuous(two_years, ["new_child", "high_income"], prefix)

ces_data.t_test(three_years_new_parents, "climate_change_persists", "high_income")
ces_data.t_test(three_years_new_parents, "climate_change_persists_abs", "high_income")
# TODO: T tests for climate_composite return NaN
ces_data.t_test(three_years_new_parents, "climate_composite_persists", "high_income")
ces_data.t_test(three_years_new_parents, "climate_composite_persists_abs", "high_income")

# Chi square tests within new parents: high_income, low_income: nothing
ces_data.chisqs(two_years_new_parents, 'change', 'high_income')

# Chi square tests within high income: nothing
ces_data.chisqs(two_years_high_income, 'change')

# Chi square tests within low income: budget
ces_data.chisqs(two_years_low_income, 'change')
