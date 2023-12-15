# Overall TODO (there are specific TODOs for all of these):
#   - Filter/bucket people before analysis
#     - Are originally liberal/conservative people affected differently?
#     - Mothers vs fathers
#     - Low vs high income
#     - Limit to people interested in the news (newsint 1-4, with 1 high)
#   - Pull stats on consistency in three_years
#     - In ideology and pid
#     - In continuous policy changes
#     - In categorical policy changes
#   - For each filter_na call, note how many people were filtered out

library(haven)
library(tidyverse)

setwd("~/Documents/visualizations/midterm")
panel <- read_dta("CCES_Panel_Full3waves_VV_V4.dta") # n=9500

### Build three base data frames (10-12, 12-14, 10-14) ###
three_years <- panel %>% zap_labels() %>% 
  select(
    # Ideology and partisanship
    starts_with("ideo5_"),
    matches("pid3_1[024]"),
    starts_with("pid7_"),
    
    # Policy issues: categorical
    matches("CC1[024]_320"), # gun control (1-3 more strict, less strict, same)
    matches("CC1[024]_326"), # gay marriage (1/2 no/yes): note issue was very active during this time, with Obergefell in 2015
    matches("CC1[024]_328"), # budget (1 cut defense, 2 cut domestic, 3 raise taxes)
    matches("CC1[024]_329"), # budget move to avoid (1 cut defense, 2 cut domestic, 3 raise taxes)
    matches("CC1[024]_330B"), # SCHIP (1 renew, 2 expire)

    # Policy issues: continuous
    matches("CC1[024]_321"), # climate change (1-5 real to not real)
    matches("CC1[024]_325"), # job vs environment (1-5 favor environment to favor jobs)
    matches("CC1[024]_327"), # affirmative action (1-4 support to oppose)

    # Policy issues: excluding
    #matches("CC1[024]_322_[1-6]"), # immigration (multiple yes/no questions): ignore because of specificity of policies
    #matches("CC1[024]_324"), # abortion (1-4 conservative to liberal): ignore because of previous lit suggesting gender of child matters here
    #matches("CC1[024]_332"), # roll call votes (multiple, 1/2 support/oppose, discard other values): ignore because of specificity (and inconsistency of data between years)
    
    # Policy issues: more on budgets
    matches("CC1[024]_415r"), # taxes vs spending (examples given are of domestic spending) (0 to 100)
    matches("CC1[024]_416r"), # raise sales vs income tax (0 to 100)

    # Parenthood
    starts_with("gender_"),
    starts_with("child18_"),
    starts_with("child18num_"),
    
    # Demographics for controls/filters
    starts_with("birthyr_"),
    starts_with("faminc_"),
    starts_with("investor_"), # 1/2 yes/no
    starts_with("newsint_"), # Limit to 1-4, 1 is "high"
    starts_with("race_"), # Limit to 1-8, categorical
    starts_with("educ_"), # Limit to 1-6, categorical
    starts_with("marstat_"), # Limit to 1-6, categorical
    starts_with("pew_religimp_"), # Limit to 1-4, 1 is "very important"
  ) %>% mutate(
    cycle = 101214,
    
    # Recode guns to be continuous (swap 2 and 3 so 1, 2, 3 is more strict, same, less strict)
    CC10_320 = if_else(CC10_320 == 2, 3, if_else(CC10_320 == 3, 2, CC10_320)),
    CC12_320 = if_else(CC12_320 == 2, 3, if_else(CC12_320 == 3, 2, CC12_320)),
    CC14_320 = if_else(CC14_320 == 2, 3, if_else(CC14_320 == 3, 2, CC14_320)),
    
    # Replace NA with 0 for child18num columns, because NAs don't play nicely with comparators
    child18num_10 = coalesce(child18num_10, 0),
    child18num_12 = coalesce(child18num_12, 0),
    child18num_14 = coalesce(child18num_14, 0),
    
    # Consolidate demographics, arbitrarily using later data if there are differences
    gender = gender_10, # Verified gender doesn't change for anyone: all_data %>% filter(gender_10 != gender_12 | gender_12 != gender_14 | gender_10 != gender_14)
    age = 2010 - birthyr_10,
    race = if_else(race_10 < 10, race_10, if_else(race_12 < 10, race_12, race_14)), # Limit to 1-8, categorical
    income = faminc_14, # Use faminc_14 because the buckets vary by year, and the 2014 buckets are more granular
    investor = if_else(investor_14 < 3, investor_14, if_else(investor_12 < 3, investor_12, investor_10)), # yes/no has money in stocks
    newsint = if_else(newsint_14 < 5, newsint_14, if_else(newsint_12 < 5, newsint_12, newsint_10)), # interest in news/politics (1 high - 4 little)
    educ = if_else(educ_10 < 7, educ_10, if_else(educ_12 < 7, educ_12, educ_14)), # Limit to 1-6, categorical
    marstat = if_else(marstat_10 < 7, marstat_10, if_else(marstat_12 < 7, marstat_12, marstat_14)), # Limit to 1-6, categorical
    pew_religimp = if_else(pew_religimp_14 < 5, pew_religimp_14, if_else(pew_religimp_12 < 5, pew_religimp_12, pew_religimp_10)), # importance of religion (1 high - 4 little)
  ) %>% 
  # Now that demographics are consolidated, remove the year-specific columns
  select(-starts_with("gender_")) %>% 
  select(-starts_with("birthyr_")) %>% 
  select(-starts_with("race_")) %>% 
  select(-starts_with("faminc_")) %>% 
  select(-starts_with("investor_")) %>% 
  select(-starts_with("newsint_")) %>% 
  select(-starts_with("educ_")) %>% 
  select(-starts_with("marstat_")) %>% 
  select(-starts_with("pew_religimp_"))

# Add income quintiles: note that income options are different by cycle
# These are approximate, since incomes are given in ranges
ggplot(panel %>% filter(faminc_14 < 19), aes(x = faminc_14)) +
  geom_histogram(fill = "steelblue", binwidth = 1)
panel %>% group_by(faminc_14) %>% summarise(count = n())
three_years <- three_years %>% 
  mutate(
    income_quintile = case_when(
      income %in% c(1, 2) ~ 1,
      income %in% c(3, 4) ~ 2,
      income %in% c(5, 6, 7) ~ 3,
      income %in% c(8, 9, 10) ~ 4,  # note the 10 response could go into either 4th or 5th
      income %in% c(11, 12, 13, 14, 15, 16) ~ 5,
      .default = NA
    ),
    high_income = if_else(is.na(income_quintile), NA, if_else(income_quintile == 5, 1, 0)),
    low_income = if_else(is.na(income_quintile), NA, if_else(income_quintile %in% c(1, 2), 1, 0)),
  )
three_years %>% group_by(income_quintile) %>% summarise(count = n())
three_years %>% group_by(high_income) %>% summarise(count = n())
three_years %>% group_by(low_income) %>% summarise(count = n())
   
two_years <- merge(
  three_years %>% mutate(cycle = 1012),  # contains all data, but only look at 2010/2012
  three_years %>% mutate(cycle = 1214),  # contains all data, but only look at 2012/2014
  all = TRUE
)

# Prep column to eval
ecol <- function (col_name) {
  return(parse(text=as.name(col_name)))
}

# Add columns for new child, new mother, new father
# Note that there are separate columns for child18 and child18num, but
# `panel %>% filter(child18num_14 > 1 & child18_14 == 2) %>% summarise(count = n()) == 0`
# for all three cycles - perhaps child18num was skipped if child18 was asked?
add_parenting <- function(df) {
  return(
    df %>% mutate(
      new_child = if_else(cycle == 1214,
                          child18num_12 < child18num_14,
                          child18num_10 < child18num_12),
      firstborn = if_else(cycle == 1214,
                          child18num_12 == 0 & child18num_14 > 0,
                          child18num_10 == 0 & child18num_12 > 0),
    ) %>% select(-starts_with("child18num_"))
  )
}
three_years <- add_parenting(three_years)
two_years <- add_parenting(two_years)


add_ideo <- function(df) {
  valid = c(1:5)
  return(
    df %>% mutate(
      ideo_before = if_else(cycle == 1214, ideo5_12, ideo5_10),
      ideo_after = if_else(cycle == 1012, ideo5_12, ideo5_14),
      ideo_delta = if_else(ideo_before %in% valid & ideo_after %in% valid, ideo_after - ideo_before, NA),
      ideo_delta_abs = abs(ideo_delta),
      ideo_direction = if_else(is.na(ideo_delta),
                               NA,
                               if_else(ideo_delta > 0, 
                                       1, 
                                       if_else(ideo_delta < 0, -1, 0)))
    )
  )
}
three_years <- add_ideo(three_years)
two_years <- add_ideo(two_years)

add_pid <- function(df) {
  valid = c(1:7)
  return(
    df %>% mutate(
      pid_before = if_else(cycle == 1214, pid7_12, pid7_10),
      pid_after = if_else(cycle == 1012,  pid7_12, pid7_14),
      pid_delta = if_else(pid_before %in% valid & pid_after %in% valid, pid_after - pid_before, NA),
      pid_delta_abs = abs(pid_delta),
      pid_direction = if_else(is.na(pid_delta),
                              NA,
                              if_else(pid_delta > 0,
                                      1,
                                      if_else(pid_delta < 0, -1, 0))),
    )
  )
}
three_years <- add_pid(three_years)
two_years <- add_pid(two_years)

add_continuous_opinions <- function (df) {
  return(
    df %>% mutate(
      climate_change_before = if_else(cycle == 1214, CC12_321, CC10_321),
      climate_change_after = if_else(cycle == 1012,  CC12_321, CC14_321),
      climate_change_delta = if_else(climate_change_before %in% c(1:5) & climate_change_after %in% c(1:5), climate_change_after - climate_change_before, NA),
      jobs_env_before = if_else(cycle == 1214, CC12_325, CC10_325),
      jobs_env_after = if_else(cycle == 1012,  CC12_325, CC14_325),
      jobs_env_delta = if_else(jobs_env_before %in% c(1:5) & jobs_env_after %in% c(1:5), jobs_env_after - jobs_env_before, NA),
      aff_action_before = if_else(cycle == 1214, CC12_327, CC10_327),
      aff_action_after = if_else(cycle == 1012,  CC12_327, CC14_327),
      aff_action_delta = if_else(aff_action_before %in% c(1:4) & aff_action_after %in% c(1:4), aff_action_after - aff_action_before, NA),
      guns_before = if_else(cycle == 1214, CC12_320, CC10_320),
      guns_after = if_else(cycle == 1012,  CC12_320, CC14_320),
      guns_delta = if_else(guns_before %in% c(1:3) & guns_after %in% c(1:3), guns_after - guns_before, NA),
      tax_or_spend_before = if_else(cycle == 1214, CC12_415r, CC10_415r),
      tax_or_spend_after = if_else(cycle == 1012,  CC12_415r, CC14_415r),
      tax_or_spend_delta = if_else(tax_or_spend_before %in% c(0:100) & tax_or_spend_after %in% c(0:100), tax_or_spend_after - tax_or_spend_before, NA),
      sales_or_inc_before = if_else(cycle == 1214, CC12_416r, CC10_416r),
      sales_or_inc_after = if_else(cycle == 1012,  CC12_416r, CC14_416r),
      sales_or_inc_delta = if_else(sales_or_inc_before %in% c(0:100) & sales_or_inc_after %in% c(0:100), sales_or_inc_after - sales_or_inc_before, NA),
    ) %>% select(-ends_with("_320"), -ends_with("_321"), -ends_with("_325"), -ends_with("_327"),
                 -ends_with("_415r"), -ends_with("_416r"))
  )
}
three_years <- add_continuous_opinions(three_years)
two_years <- add_continuous_opinions(two_years)

add_categorical_opinions <- function (df) {
  return(
    df %>% mutate(
      gay_marriage_before = if_else(cycle == 1214, CC12_326, CC10_326),
      gay_marriage_after = if_else(cycle == 1012,  CC12_326, CC14_326),
      gay_marriage_change = if_else(
        gay_marriage_before %nin% c(1, 2) | gay_marriage_after %nin% c(1, 2), NA,
        if_else(gay_marriage_before == gay_marriage_after, 0, 1)),
      schip_before = if_else(cycle == 1214, CC12_330B, CC10_330B),
      schip_after = if_else(cycle == 1012,  CC12_330B, CC14_330B),
      schip_change = if_else(
        schip_before %nin% c(1, 2) | schip_after %nin% c(1, 2), NA,
        if_else(schip_before == schip_after, 0, 1)),
      budget_before = if_else(cycle == 1214, CC12_328, CC10_328),
      budget_after = if_else(cycle == 1012,  CC12_328, CC14_328),
      budget_change = if_else(
        budget_before %nin% c(1:3) | budget_after %nin% c(1:3), NA,
        if_else(budget_before == budget_after, 0, 1)),
      budget_combo = budget_before * 10 + budget_after,
      budget_avoid_before = if_else(cycle == 1214, CC12_329, CC10_329),
      budget_avoid_after = if_else(cycle == 1012,  CC12_329, CC14_329),
      budget_avoid_change = if_else(budget_avoid_before == budget_avoid_after, 0, 1),
      budget_avoid_change = if_else(
        budget_avoid_before %nin% c(1:3) | budget_avoid_after %nin% c(1:3), NA,
        if_else(budget_avoid_before == budget_avoid_after, 0, 1)),
      budget_avoid_combo = budget_avoid_before * 10 + budget_avoid_after,
    ) %>% select(-ends_with("_326"), -ends_with("_328"), -ends_with("_329"), -ends_with("_330B"))
  )
}
three_years <- add_categorical_opinions(three_years)
two_years <- add_categorical_opinions(two_years)

# pid3: Too coarse-grained to use as ideology, though note people do flip
# 1.1% people flipped between the major parties between 2010 and 2014
# (limited to people who identified with one of the two major parties, ignoring those who flipped twice)
count_flippers <- function (data_frame, before, after, valid_values) {
  valid_rows <- data_frame %>% filter(
    eval(ecol(before)) %in% valid_values &
    eval(ecol(after)) %in% valid_values
  )
  flippers <- valid_rows %>% filter(eval(ecol(before)) != eval(ecol(after)))
  flippers <- flippers %>% mutate(delta = eval(ecol(before)) - eval(ecol(after)))
  #ggplot(flippers, aes(x = delta)) +
  #  geom_histogram(fill = "steelblue", binwidth = 1)
  #ggplot(flippers, aes(x = eval(ecol(before)))) +
  #  geom_histogram(fill = "steelblue", binwidth = 1)
  return(
    round(nrow(flippers) * 100 / nrow(valid_rows), 1)
  )
}
count_flippers(three_years, "pid3_10", "pid3_12", c(1:2))
count_flippers(three_years, "pid7_10", "pid7_12", c(1:7)) # 20%
count_flippers(three_years, "pid7_12", "pid7_14", c(1:7)) # 20%
count_flippers(three_years, "pid7_10", "pid7_14", c(1:7)) # 25%
count_flippers(three_years, "ideo5_10", "ideo5_12", c(1:5)) # 25%
count_flippers(three_years, "ideo5_12", "ideo5_14", c(1:5)) # 20%
count_flippers(three_years, "ideo5_10", "ideo5_14", c(1:5)) # 28%

## Descriptive stats on ideology and party

# Iedology is roughly normal, skewing conservative
panel %>% group_by(ideo5_10) %>% summarise(count = n())
ggplot(three_years, aes(x = ideo5_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)
ggplot(three_years %>% filter(new_child == 1), aes(x = ideo5_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)

# Party affiliation is U-shaped
panel %>% group_by(pid7_10) %>% summarise(count = n())
ggplot(three_years, aes(x = pid7_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)
ggplot(three_years %>% filter(new_child == 1), aes(x = pid7_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)

# In two cycles: 420 with new child, 18580 without
two_years %>% group_by(new_child) %>% summarise(count = n())

# In three cycles: 229 with new child in 2012, 9271 without
three_years %>% group_by(new_child) %>% summarise(count = n())

# Look at direction, but not magnitude, of ideological change
# Non-new-parents: 2230 + 13853 + 1795 = 17878: 12.5% more liberal, 10.0% more conservative
# New parents: 49 + 304 + 43 = 396: 12.4% more liberal, 10.9% more conservative
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  group_by(new_child, ideo_direction) %>%
  summarise(count = n())
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  filter(age < 30) %>% 
  group_by(new_child, ideo_direction) %>%
  summarise(count = n())
# Limit to people who were consistent over three cycles
# Non-new-parents: 1403 + 5703 + 977 = 8083: 17.3% more liberal, 12.0% more conservative
# New parents: 34 + 127 + 25 = 186: 18.3% more liberal, 13.4% more conservative
valid <- c(1:5)
three_years %>%
  filter(ideo5_10 %in% valid & ideo5_12 %in% valid & ideo5_14 %in% valid) %>% 
  filter(ideo5_10 <= ideo5_12 & ideo5_12 <= ideo5_14 | ideo5_10 >= ideo5_12 & ideo5_12 >= ideo5_14) %>% 
  group_by(new_child, ideo_direction) %>%
  summarise(count = n())

filter_na <- function (data_frame, column) {
  return(
    data_frame %>% filter(!is.na(eval(ecol(column))))
  )
}

# Average ideological change over two years: barely liberal
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  group_by(new_child) %>%
  summarise(average_ideo = mean(ideo_delta), average_ideo_abs = mean(ideo_delta_abs))
two_years %>% # Using firstborn instead doesn't change anything remotely meaningful
  filter(!is.na(ideo_delta)) %>% 
  group_by(firstborn) %>%
  summarise(average_ideo = mean(ideo_delta), average_ideo_abs = mean(ideo_delta_abs))
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  filter(age < 30) %>% 
  group_by(new_child) %>%
  summarise(average_ideo = mean(ideo_delta), average_ideo_abs = mean(ideo_delta_abs))

# Average party change over two years: bigger, but still not shifting the group
two_years %>%
  filter(!is.na(pid_delta)) %>% 
  group_by(new_child) %>%
  summarise(average_pid = mean(pid_delta), average_pid_abs = mean(pid_delta_abs))
two_years %>%
  filter(!is.na(pid_delta)) %>% 
  group_by(firstborn) %>%
  summarise(average_pid = mean(pid_delta), average_pid_abs = mean(pid_delta_abs))
two_years %>%
  filter(!is.na(pid_delta)) %>% 
  filter(age < 30) %>% 
  group_by(firstborn) %>%
  summarise(average_pid = mean(pid_delta), average_pid_abs = mean(pid_delta_abs))
 
t.test(ideo_delta~new_child, data=filter_na(two_years, "ideo_delta")) # p = 0.4108
t.test(ideo_delta_abs~new_child, data=filter_na(two_years, "ideo_delta_abs")) # p = 0.6008
t.test(ideo_delta~new_child, data=filter_na(two_years %>% filter(age < 30), "ideo_delta")) # p = 0.6761
t.test(ideo_delta_abs~new_child, data=filter_na(two_years %>% filter(age < 30), "ideo_delta_abs")) # p = 0.6028

t.test(pid_delta~new_child, data=filter_na(two_years, "pid_delta")) # p = 0.4348
t.test(pid_delta_abs~new_child, data=filter_na(two_years, "pid_delta_abs")) # p = 0.07467
t.test(pid_delta~new_child, data=filter_na(two_years %>% filter(age < 30), "pid_delta")) # p = 0.4663
t.test(pid_delta_abs~new_child, data=filter_na(two_years %>% filter(age < 30), "pid_delta_abs")) # p = 0.6051

# TODO: Filter by age, noting sample skews towards 50s & 60s
ggplot(three_years, aes(x = age)) +
  geom_histogram(fill = "steelblue", binwidth = 5)
three_years %>% filter(age < 30) # n=282
ggplot(two_years %>% filter(new_child == 1), aes(x = age)) + # this is surprisingly old
  geom_histogram(fill = "steelblue", binwidth = 5)
ggplot(two_years %>% filter(child18_12 == 1 & child18num_12 > child18num_10), aes(x = age)) +
  geom_histogram(fill = "steelblue", binwidth = 5)

# T tests for parent subsets
# These groups are pretty small, once you filter to people who changed ideo/pid, only 30-60 people per group
two_years_new_parents <- two_years %>% filter(new_child == 1)
two_years_new_parents %>% filter(pid_delta != 0 & !is.na(pid_delta)) %>% group_by(gender) %>% summarise(count = n())
two_years_new_parents %>% filter(pid_delta != 0 & !is.na(pid_delta)) %>% group_by(income_bracket) %>% summarise(count = n())
t.test(ideo_delta~gender, data=filter_na(two_years_new_parents, "ideo_delta")) # p = 0.9288
t.test(pid_delta~gender, data=filter_na(two_years_new_parents, "pid_delta")) # p = 0.09352 # weird that this is so different from ideo_delta, but groups are small
t.test(ideo_delta~income_bracket, data=filter_na(two_years_new_parents, "ideo_delta")) # p = 0.6394
t.test(pid_delta~income_bracket, data=filter_na(two_years_new_parents, "pid_delta")) # p = 0.56

get_regression_table(lm(ideo_delta ~ as_factor(new_child) + age, data=filter_na(two_years, "ideo_delta")))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + as_factor(gender), data=filter_na(two_years, "ideo_delta")))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + as_factor(income_bracket), data=filter_na(two_years, "ideo_delta")))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + age + as_factor(gender), data=filter_na(two_years, "ideo_delta")))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + income, data=filter_na(two_years, "ideo_delta")))
# TODO: try out controls for religiosity, education, race, marital status, investor, newsint
# TODO: try filtering for newsint first

# dependent_var and independent var are strings, controls is a list of strings
run_lm <- function (data_frame, dependent_var, independent_var, controls=NULL) {
  if (length(controls) == 0) {
    return(lm(eval(ecol(dependent_var)) ~ eval(ecol(independent_var)),
              data=data_frame))
  } else {
    return(lm(eval(ecol(dependent_var)) ~ eval(ecol(independent_var))
              + eval(ecol(paste(controls, collapse=" + "))),
              data=data_frame))
  }
}
run_lm(filter_na(two_years, "ideo_delta"), "ideo_delta", "as_factor(new_child)", c("age"))

run_regression_table <- function (data_frame, dependent_var, independent_var, controls=NULL) {
  return(get_regression_table(run_lm(data_frame, dependent_var, independent_var, controls)))
}
run_regression_table(filter_na(two_years, "ideo_delta"), "ideo_delta", "as_factor(new_child)", c("age"))

# Policy issues: continuous: only tax or spend is significant
t.test(climate_change_delta~new_child, data=filter_na(two_years, "climate_change_delta")) # p = 0.56
t.test(jobs_env_delta~new_child, data=filter_na(two_years, "jobs_env_delta")) # p = 0.6602
t.test(aff_action_delta~new_child, data=filter_na(two_years, "aff_action_delta")) # p = 0.9901
t.test(guns_delta~new_child, data=filter_na(two_years, "guns_delta")) # p = 0.4005
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years, "tax_or_spend_delta")) # p = 0.3224
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years, "sales_or_inc_delta")) # p = 0.345
t.test(climate_change_delta~new_child, data=filter_na(three_years, "climate_change_delta")) # p = 0.2014
t.test(jobs_env_delta~new_child, data=filter_na(three_years, "jobs_env_delta")) # p = 0.5924
t.test(aff_action_delta~new_child, data=filter_na(three_years, "aff_action_delta")) # p = 0.2398
t.test(guns_delta~new_child, data=filter_na(three_years, "guns_delta")) # p = 0.0979
t.test(tax_or_spend_delta~new_child, data=filter_na(three_years, "tax_or_spend_delta")) # p = 0.0007678
t.test(sales_or_inc_delta~new_child, data=filter_na(three_years, "sales_or_inc_delta")) # p = 0.9061

# T tests for parent subsets: a couple approach significance on gender (p < 0.1, at least)
two_years_new_parents <- two_years %>% filter(new_child == 1)
t.test(climate_change_delta~gender, data=filter_na(two_years_new_parents, "climate_change_delta")) # p=0.8752
t.test(jobs_env_delta~gender, data=filter_na(two_years_new_parents, "jobs_env_delta")) # p=0.7913
t.test(aff_action_delta~gender, data=filter_na(two_years_new_parents, "aff_action_delta")) # p=0.0800
t.test(guns_delta~gender, data=filter_na(two_years_new_parents, "guns_delta")) # p=0.08586
t.test(tax_or_spend_delta~gender, data=filter_na(two_years_new_parents, "tax_or_spend_delta")) # p=0.04358
t.test(sales_or_inc_delta~gender, data=filter_na(two_years_new_parents, "sales_or_inc_delta")) # p=0.4426
t.test(climate_change_delta~income_bracket, data=filter_na(two_years_new_parents, "climate_change_delta")) # p=0.7924
t.test(jobs_env_delta~income_bracket, data=filter_na(two_years_new_parents, "jobs_env_delta")) # p=0.5589
t.test(aff_action_delta~income_bracket, data=filter_na(two_years_new_parents, "aff_action_delta")) # p=0.8019
t.test(guns_delta~income_bracket, data=filter_na(two_years_new_parents, "guns_delta")) # p=0.1472
t.test(tax_or_spend_delta~income_bracket, data=filter_na(two_years_new_parents, "tax_or_spend_delta")) # p=0.4926
t.test(sales_or_inc_delta~income_bracket, data=filter_na(two_years_new_parents, "sales_or_inc_delta")) # p=0.8732

# Parents are less willing to raise taxes, more interested in spending cuts...but only in three_years
ggplot(filter_na(three_years, "tax_or_spend_delta") %>% filter(new_child == 1), aes(x = tax_or_spend_delta)) +
  geom_histogram(fill = "steelblue", binwidth = 10) #+ facet_wrap(~ as_factor(new_child))
ggplot(three_years %>% filter(tax_or_spend_after < 101) %>% filter(new_child == 0), aes(x = tax_or_spend_after)) +
  geom_histogram(fill = "steelblue", binwidth = 10) #+ facet_wrap(~ as_factor(new_child))

run_chisq <- function(data, independent_var, dependent_var) {
  filtered <- filter_na(data, dependent_var)
  return(chisq.test(table(
    eval(parse(text=paste(c("filtered$", independent_var), collapse=""))),
    eval(parse(text=paste(c("filtered", "$", dependent_var), collapse="")))
  )))
}

# Chi square tests for categorical variables
run_chisq(two_years, "new_child", "ideo_direction") # p=0.8664
run_chisq(two_years, "new_child", "pid_direction") # p=0.3215, but p=0.07 when looking at firstborn
run_chisq(two_years, "new_child", "gay_marriage_change") # p=0.1347
run_chisq(two_years, "new_child", "schip_change") # p=0.3306
run_chisq(two_years, "new_child", "budget_change") # p=0.00280, but p=0.0647 when looking at firstborn
run_chisq(two_years, "new_child", "budget_avoid_change") # p=0.0154, but 0.002389 when looking at firstborn
run_chisq(two_years, "new_child", "gay_marriage_after") # p=0.2971
run_chisq(two_years, "new_child", "schip_after") # p=0.8188
run_chisq(two_years, "new_child", "budget_after") # p=0.224
run_chisq(two_years, "new_child", "budget_avoid_after") # p=0.0814

# Look closer at budget_change
agg_combo <- two_years %>% filter(budget_before != budget_after) %>% group_by(new_child, budget_combo) %>% summarise(count = n())
agg_after <- two_years %>% filter(budget_before != budget_after) %>% group_by(new_child, budget_after) %>% summarise(count = n())
# Looking at the combos, the most non-parents flip from cutting defense to raising taxes, and the 2nd-most want the opposite
# Among the parents, the most change from cutting domestic to cutting defense, and the lest want the opposite
ggplot(agg_combo, aes(x = as_factor(budget_combo), y = count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ as_factor(new_child))
# Looking at the "after" answers, non-parents want to raise taxes, while parents want to cut defense spending, and parents are more evenly split
ggplot(agg_after, aes(x = as_factor(budget_after), y = count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ as_factor(new_child))
# Look closer at budget_avoid_change
agg_combo <- temp %>% filter(budget_avoid_before != budget_avoid_after) %>% group_by(new_child, budget_avoid_combo) %>% summarise(count = n())
agg_after <- temp %>% filter(budget_avoid_before != budget_avoid_after) %>% group_by(new_child, budget_avoid_after) %>% summarise(count = n())
# Looking at the combos, the pattern of change is similar, with both parents and non-parents' getting more willing to raise taxes
ggplot(agg_combo, aes(x = as_factor(budget_avoid_combo), y = count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ as_factor(new_child))
# Looking at the "after" answers, parents more want to avoid cutting domestic spending,
# but both groups want to avoid cutting defense - which seems to contradict the previous question
ggplot(agg_after, aes(x = as_factor(budget_avoid_after), y = count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ as_factor(new_child))

# Chi square tests within new parents: gender
two_years_new_parents <- two_years %>% filter(new_child == 1)
run_chisq(two_years_new_parents, "gender", "ideo_direction") # p=0.595
run_chisq(two_years_new_parents, "gender", "pid_direction") # p=0.1408
run_chisq(two_years_new_parents, "gender", "gay_marriage_change") # p=0.485
run_chisq(two_years_new_parents, "gender", "schip_change") # p=0.6808
run_chisq(two_years_new_parents, "gender", "budget_change") # p=0.00001167
run_chisq(two_years_new_parents, "gender", "budget_avoid_change") # p=0.0005327, or p=0.01786 when looking at firstborn

# Look closer at budget_change and budget_avoid_change, by gender
# For both, women are a lot more likely to change
two_years_new_parents %>% group_by(gender, budget_change) %>% summarise(count = n())
two_years_new_parents %>% group_by(gender, budget_avoid_change) %>% summarise(count = n())
# Women changing are most likely flipping on whether to cut defense or domestic spending
# Women changing are more open to raising taxes
two_years_new_parents %>% filter(budget_change == 1) %>% group_by(gender, budget_combo) %>% summarise(count = n())
two_years_new_parents %>% filter(budget_avoid_change == 1) %>% group_by(gender, budget_avoid_combo) %>% summarise(count = n())

# Chi square tests within new parents: income_bracket
two_years_new_parents <- two_years %>% filter(new_child == 1)
run_chisq(two_years_new_parents, "income_bracket", "ideo_direction") # p=0.7384
run_chisq(two_years_new_parents, "income_bracket", "pid_direction") # p=0.6932
run_chisq(two_years_new_parents, "income_bracket", "gay_marriage_change") # p=0.8482
run_chisq(two_years_new_parents, "income_bracket", "schip_change") # p=0.7751
run_chisq(two_years_new_parents, "income_bracket", "budget_change") # p=0.1845
run_chisq(two_years_new_parents, "income_bracket", "budget_avoid_change") # p=0.5268

# TODO: Look at SCHIP by income:
#   1. Replace all income_bracket references with income_quintile, high_income, low_income
#     1.5 See if there's anything interesting there
#   2. Look at SCHIP distributions

# Non-parents 0.03 more liberal, new parents identical before and after
filter_na(two_years, "ideo_delta") %>%
  group_by(new_child) %>%
  summarise(mean_before = mean(ideo_before), mean_after = mean(ideo_after))
# Non-parents 0.02 more consevative, new parents 0.02 more liberal
filter_na(two_years, "pid_delta") %>%
  group_by(new_child) %>%
  summarise(mean_before = mean(pid_before), mean_after = mean(pid_after))

# Similar to previous, with both new mothers & fathers identical before and after
# non-parents getting 0.02 (women) or 0.03 (men) more liberal
filter_na(two_years, "ideo_delta") %>%
  group_by(new_child, gender) %>%
  summarise(mean_before = mean(ideo_before), mean_after = mean(ideo_after))
# With party identification, there's slightly larger change, but still < 0.1
filter_na(two_years, "pid_delta") %>%
  group_by(new_child, gender) %>%
  summarise(mean_before = mean(pid_before), mean_after = mean(pid_after))
