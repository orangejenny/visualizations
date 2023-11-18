# Overall TODO (there are specific TODOs for all of these):
#   0) Readability!
#   1) Look at  consistency in ideology change over 3 cycles (for all ideology variables) - use/extend count_flippers!
#   2) Use run_lm/run_regression_table function for all the policy variables...some are categorical, some are continuous
#   3) Parameterize ideology, use run_lm/run_regression_table
#   4) Use run_lm/run_regression_table with various controls
#   5) Note how many people get filtered out due to answers I can't do math with

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

    # Policy issues: continuous
    matches("CC1[024]_321"), # climate change (1-5 real to not real)
    matches("CC1[024]_325"), # job vs environment (1-5 favor environment to favor jobs)
    matches("CC1[024]_327"), # affirmative action (1-4 support to oppose)

    # Policy issues: excluding
    #matches("CC1[024]_322_[1-6]"), # immigration (multiple yes/no questions): ignore because of specificity of policies
    #matches("CC1[024]_324"), # abortion (1-4 conservative to liberal): ignore because of previous lit suggesting gender of child matters here
    #matches("CC1[024]_332"), # roll call votes (multiple, 1/2 support/oppose, discard other values): ignore because of specificity (and inconsistency of data between years)
    
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
    income = if_else(faminc_14 < 20, faminc_14, if_else(faminc_12 < 20, faminc_12, faminc_10)), # Bucket this (below)
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

# Add income bucket
three_years <- three_years %>% 
  mutate(
    income_bracket = if_else(income %in% seq(1,9),
                             "low",
                             if_else(income %in% seq(10, 18),
                                     "high",
                                     "unknown"))
  )
   
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
add_parenting <- function(df) {
  return(
    df %>% mutate(
      new_child = if_else(cycle == 1012, (
        eval(ecol("child18num_10")) < eval(ecol("child18num_12"))
      ), if_else(cycle == 1214, (
         eval(ecol("child18num_12")) < eval(ecol("child18num_14"))
      ), (
         # TODO: This tests if person had a child in either 2012 or 2014, maybe limit to 2012?
         # Since the point o the analysis will be to look at change over 4 years?
         eval(ecol("child18num_10")) < eval(ecol("child18num_12")) |
         eval(ecol("child18num_12")) < eval(ecol("child18num_14"))
      ))),
      new_father = if_else(!new_child, NA, gender == 1),
      new_mother = if_else(!new_child, NA, gender == 2),
    ) %>% select(-starts_with("child18num_"))
  )
}

three_years <- add_parenting(three_years)
two_years <- add_parenting(two_years)


add_ideo <- function(df) {
  valid = c(1:5)
  return(
    df %>% mutate(
      ideo_before = if_else(cycle == 1214, eval(ecol("ideo5_12")), eval(ecol("ideo5_10"))),
      ideo_after = if_else(cycle == 1012, eval(ecol("ideo5_12")), eval(ecol("ideo5_14"))),
      ideo_delta = if_else(ideo_before %in% valid & ideo_after %in% valid, ideo_after - ideo_before, NA),
      ideo_direction = if_else(is.na(ideo_delta),
                               NA,
                               if_else(ideo_delta > 0, 
                                       1, 
                                       if_else(ideo_delta < 0, -1, 0)))
    ) %>% select(-starts_with("ideo5_"))
  )
}
three_years <- add_ideo(three_years)
two_years <- add_ideo(two_years)

add_pid <- function(df) {
  valid = c(1:7)
  return(
    df %>% mutate(
      pid_before = if_else(cycle == 1214, eval(ecol("pid7_12")), eval(ecol("pid7_10"))),
      pid_after = if_else(cycle == 1012,  eval(ecol("pid7_12")), eval(ecol("pid7_14"))),
      pid_delta = if_else(pid_before %in% valid & pid_after %in% valid, pid_after - pid_before, NA),
      pid_direction = if_else(is.na(pid_delta),
                              NA,
                              if_else(pid_delta > 0,
                                      1,
                                      if_else(pid_delta < 0, -1, 0))),
    ) %>% select(-starts_with("pid7_"))
  )
}
three_years <- add_pid(three_years)
two_years <- add_pid(two_years)

add_continuous_opinions <- function (df) {
  return(
    df %>% mutate(
      climate_change_before = if_else(cycle == 1214, eval(ecol("CC12_321")), eval(ecol("CC10_321"))),
      climate_change_after = if_else(cycle == 1012,  eval(ecol("CC12_321")), eval(ecol("CC14_321"))),
      climate_change_delta = if_else(climate_change_before %in% c(1:5) & climate_change_after %in% c(1:5), climate_change_after - climate_change_before, NA),
      jobs_env_before = if_else(cycle == 1214, eval(ecol("CC12_325")), eval(ecol("CC10_325"))),
      jobs_env_after = if_else(cycle == 1012,  eval(ecol("CC12_325")), eval(ecol("CC14_325"))),
      jobs_env_delta = if_else(jobs_env_before %in% c(1:5) & jobs_env_after %in% c(1:5), jobs_env_after - jobs_env_before, NA),
      aff_action_before = if_else(cycle == 1214, eval(ecol("CC12_327")), eval(ecol("CC10_327"))),
      aff_action_after = if_else(cycle == 1012,  eval(ecol("CC12_327")), eval(ecol("CC14_327"))),
      aff_action_delta = if_else(aff_action_before %in% c(1:4) & aff_action_after %in% c(1:4), aff_action_after - aff_action_before, NA),
      guns_before = if_else(cycle == 1214, eval(ecol("CC12_320")), eval(ecol("CC10_320"))),
      guns_after = if_else(cycle == 1012,  eval(ecol("CC12_320")), eval(ecol("CC14_320"))),
      guns_delta = if_else(guns_before %in% c(1:3) & guns_after %in% c(1:3), guns_after - guns_before, NA),
    ) %>% select(-ends_with("_320"), -ends_with("_321"), -ends_with("_325"), -ends_with("_327"))
  )
}
three_years <- add_continuous_opinions(three_years)
two_years <- add_continuous_opinions(two_years)

add_categorical_opinions <- function (df) {
  return(
    df %>% mutate(
      gay_marriage_before = if_else(cycle == 1214, eval(ecol("CC12_326")), eval(ecol("CC10_326"))),
      gay_marriage_after = if_else(cycle == 1012,  eval(ecol("CC12_326")), eval(ecol("CC14_326"))),
      gay_marriage_change = if_else(
        gay_marriage_before %nin% c(1, 2) | gay_marriage_after %nin% c(1, 2), NA,
        if_else(gay_marriage_before == gay_marriage_after, 0, 1)),
      budget_before = if_else(cycle == 1214, eval(ecol("CC12_328")), eval(ecol("CC10_328"))),
      budget_after = if_else(cycle == 1012,  eval(ecol("CC12_328")), eval(ecol("CC14_328"))),
      budget_change = if_else(
        budget_before %nin% c(1:3) | budget_after %nin% c(1:3), NA,
        if_else(budget_before == budget_after, 0, 1)),
      budget_avoid_before = if_else(cycle == 1214, eval(ecol("CC12_329")), eval(ecol("CC10_329"))),
      budget_avoid_after = if_else(cycle == 1012,  eval(ecol("CC12_329")), eval(ecol("CC14_329"))),
      budget_avoid_change = if_else(budget_avoid_before == budget_avoid_after, 0, 1),
      budget_avoid_change = if_else(
        budget_avoid_before %nin% c(1:3) | budget_avoid_after %nin% c(1:3), NA,
        if_else(budget_avoid_before == budget_avoid_after, 0, 1)),
    ) %>% select(-ends_with("_326"), -ends_with("_328"), -ends_with("_329"))
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
  return(
    round(nrow(flippers) * 100 / nrow(valid_rows), 1)
  )
}
count_flippers(three_years, "pid3_10", "pid3_12", c(1:2))

# In two cycles: 420 with new child, 18580 without
two_years %>% group_by(new_child) %>% summarise(count = n())

# In three cycles: 392 with new child in either 2012 or 2014, 9108 with neither
# TODO: update after updating three_years' new_child column
three_years %>% group_by(new_child) %>% summarise(count = n())

# Look at direction, but not magnitude, of ideological change
# Non-new-parents: 2230 + 13853 + 1795 = 17878: 12.5% more liberal, 10.0% more conservative
# New parents: 49 + 304 + 43 = 396: 12.4% more liberal, 10.9% more conservative
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  group_by(new_child, ideo_direction) %>%
  summarise(count = n())
# TODO: add three_years (after changing three_years to be 2010-2014 only)
# TODO: in three_years, exclude flippers, look for consistent change

filter_na <- function (data_frame, column) {
  return(
    data_frame %>% filter(!is.na(eval(ecol(column))))
  )
}

 
# Which of these is correct to use? I *think* it's the lm,
# which, conveniently, is barely significant
t.test(ideo_delta~new_child, data=trends) # p = 0.0715
get_regression_table(lm(ideo_delta ~ as_factor(new_child), data=trends)) # p = 0.045

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
run_regression_table(trends, "ideo_delta", "as_factor(new_child)", c("age"))

run_chisq <- function(var1, var2) {
  return(chisq.test(table(var1, var2)))
}

# I think this is relevant, although it is not quite significant
run_chisq(trends$new_child, trends$direction)

# Non-parents 0.03 more liberal, new parents identical before and after
filter_na(two_years, "ideo_delta") %>%
  group_by(new_child) %>%
  summarise(mean_before = mean(ideo_before), mean_after = mean(ideo_after))

# Similar to previous, with both new mothers & fathers identical before and after
# non-parents getting 0.02 (women) or 0.03 (men) more liberal
filter_na(two_years, "ideo_delta") %>%
  group_by(new_child, gender) %>%
  summarise(mean_before = mean(ideo_before), mean_after = mean(ideo_after))