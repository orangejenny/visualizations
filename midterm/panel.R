# Overall TODO (there are specific TODOs for all of these):
#   0) Readability!
#   1) Look at  consistency in ideology change over 3 cycles (for all ideology variables) - use/extend count_flippers!
#   2) Use run_lm/run_regression_table function for all the policy variables...some are categorical, some are continuous
#   3) Parameterize ideology, use run_lm/run_regression_table
#   4) Use run_lm/run_regression_table with various controls

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
    starts_with("investor_"), # TODO: add to analysis (money in stocks)
    starts_with("newsint_"), # TODO: add to analysis (interest in news/politics (1 high - 4 little, discard other values))
    starts_with("race_"), # Limit to 1-8, categorical
    starts_with("educ_"), # Limit to 1-6, categorical
    starts_with("marstat_"), # Limit to 1-6, categorical
    starts_with("pew_religimp_"), # Limit to 1-4, 1 is "very important"
  ) %>% mutate(
    cycle = 101214,
    
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
  return(
    df %>% mutate(
      ideo_before = if_else(cycle == 1012,
                            eval(ecol("ideo5_10")),
                            if_else(cycle == 1214,
                                    eval(ecol("ideo5_12")),
                                    eval(ecol("ideo5_10")))),
      ideo_after = if_else(cycle == 1012, 
                           eval(ecol("ideo5_12")),
                           if_else(cycle == 1214,
                                   eval(ecol("ideo5_14")),
                                   eval(ecol("ideo5_14")))),
      ideo_delta = ideo_after - ideo_before,
    ) %>% select(-starts_with("ideo5_"))
  )
}
three_years <- add_ideo(three_years)
two_years <- add_ideo(two_years)

add_pid <- function(df) {
  return(
    df %>% mutate(
      pid_before = if_else(cycle == 1012,
                           eval(ecol("pid7_10")),
                           if_else(cycle == 1214,
                                   eval(ecol("pid7_12")),
                                   eval(ecol("pid7_10")))),
      pid_after = if_else(cycle == 1012, 
                           eval(ecol("pid7_12")),
                           if_else(cycle == 1214,
                                   eval(ecol("pid7_14")),
                                   eval(ecol("pid7_14")))),
      pid_delta = pid_after - pid_before,
    ) %>% select(-starts_with("pid7_"))
  )
}
three_years <- add_pid(three_years)
two_years <- add_pid(two_years)

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

trends <- all_data %>% 
  filter(ideo_before < 6, ideo_after < 6) %>% # TODO: note valid values are different for ideo vs pid
  mutate(leftward = ideo_before > ideo_after,
         rightward = ideo_before < ideo_after,
         no_change = ideo_before == ideo_after,
         direction = if_else(leftward, -1, if_else(rightward, 1, 0)))

# TODO: look at three years' data in data_101214, to check for consistent change
#   mutate data_101214 to add
#      no_change: 10 == 12 == 14
#      leftward: !no_change & 10 <= 12 & 12 <= 14
#      rightward: !no_change & 10 >= 12 & 12 >= 14
#      inconsistent: others
#   then repeat the lm and chisq.test below, filtering out inconsistent

# 17609, 602
trends %>% group_by(new_child) %>% summarise(count = n())

# New parents: 9.5% more conservative, 15.6% more liberal
# Non-new-parents: 10.1% more conservative, 12.3% more liberal
trends %>%
  group_by(new_child, income_bracket, direction) %>%
  summarise(count = n())
 
# Which of these is correct to use? I *think* it's the lm,
# which, conveniently, is barely significant
t.test(ideo_delta~new_child, data=trends) # p = 0.0715
get_regression_table(lm(ideo_delta ~ as_factor(new_child), data=trends)) # p = 0.045

get_regression_table(lm(ideo_delta ~ as_factor(new_child) + age, data=trends))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + gender, data=trends))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + as_factor(income_bracket), data=trends))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + age + gender, data=trends))
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + income, data=trends))
# TODO: try out controls for religiosity, education, race, marital status

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
run_lm(trends, "ideo_delta", "as_factor(new_child)", c("age"))

run_regression_table <- function (data_frame, dependent_var, independent_var, controls=NULL) {
  return(get_regression_table(run_lm(data_frame, dependent_var, independent_var, controls)))
}
run_regression_table(trends, "ideo_delta", "as_factor(new_child)", c("age"))

run_chisq <- function(var1, var2) {
  return(chisq.test(table(var1, var2)))
}

# I think this is relevant, although it is not quite significant
run_chisq(trends$new_child, trends$direction)

# Non-parents 0.02 more liberal, new parents 0.07 more liberal
# Similar when adding grouping by cycle
trends %>%
  group_by(new_child) %>%
  summarise(mean_before = mean(ideo_before), mean_after = mean(ideo_after))

# Similar to previous, with both new mothers & fathers getting 0.07 more liberal,
# non-parents getting 0.02 (women) or 0.03 (men) more liberal
# Similar when adding grouping by cycle
trends %>%
  group_by(new_child, gender) %>%
  summarise(mean_before = mean(ideo_before), mean_after = mean(ideo_after))

trends %>%
  group_by(new_child, direction, gender) %>%
  summarise(count = n())