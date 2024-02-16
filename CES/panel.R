library(haven)
library(tidyverse)

########################
# Functions: utilities #
########################

ecol <- function (col_name) {
  return(parse(text=as.name(col_name)))
}

filter_na <- function (data_frame, column) {
  return(
    data_frame %>% filter(!is.na(eval(ecol(column))))
  )
}

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

run_chisq <- function(data, independent_var, dependent_var) {
  filtered <- filter_na(data, dependent_var)
  return(chisq.test(table(
    eval(parse(text=paste(c("filtered$", independent_var), collapse=""))),
    eval(parse(text=paste(c("filtered", "$", dependent_var), collapse="")))
  )))
}

summarize_continuous <- function(data, group_by, issue) {
  return(
    summarize_continuous_helper(data %>% group_by(eval(ecol(group_by))), issue)
  )
}

summarize_continuous_helper <- function(grouped_data, issue) {
  return (  
     grouped_data %>% summarise(
      before = mean(eval(parse(text=paste(c(issue, "_before"), collapse=""))), na.rm = TRUE),
      after = mean(eval(parse(text=paste(c(issue, "_after"), collapse=""))), na.rm = TRUE),
      delta = mean(eval(parse(text=paste(c(issue, "_delta"), collapse=""))), na.rm = TRUE),
      delta_abs = mean(eval(parse(text=paste(c(issue, "_delta_abs"), collapse=""))), na.rm = TRUE),
     )
  )
}

continuous_persists <- function(data, issue) {
  numbers <- data %>% 
      filter(!is.na(eval(parse(text=paste(c(issue, "_persists"), collapse=""))))) %>% 
      mutate(has_issue = if_else(eval(parse(text=paste(c(issue, "_persists"), collapse=""))) == 0, 0, 1)) %>% 
      group_by(new_child, has_issue) %>% 
      summarise(count = n())
  counts <- as.list(numbers$count)
  return(
    paste("New parents: ", two_percents(counts[[3]], counts[[4]]),
          ";",
          "Others: ", two_percents(counts[[1]], counts[[2]]))
  )
}

categorical_persists <- function(data, issue) {
  numbers <- data %>% 
      filter(!is.na(eval(parse(text=paste(c(issue, "_persists"), collapse=""))))) %>% 
      group_by(new_child, eval(parse(text=paste(c(issue, "_persists"), collapse="")))) %>% 
      summarise(count = n())
  counts <- as.list(numbers$count)
  return(
    paste("New parents: ", two_percents(counts[[3]], counts[[4]]),
          ";",
          "Others: ", two_percents(counts[[1]], counts[[2]]))
  )
}

two_percents <- function(one, two) {
  total <- one + two
  return(
    paste(
      round(one * 100 / total),
      round(two * 100 / total)
    )
  )
}

three_percents <- function (one, two, three) {
  total <- one + two + three
  return(paste(
    round(one * 100 / total),
    round(two * 100 / total),
    round(three * 100 / total)
  ))
}

###################
# Functions: data #
###################

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

add_continuous_opinions <- function (df) {
  return(
    df %>% mutate(
      climate_change_before = if_else(cycle == 1214, CC12_321, CC10_321),
      climate_change_after = if_else(cycle == 1214,  CC14_321, CC12_321),
      jobs_env_before = if_else(cycle == 1214, CC12_325, CC10_325),
      jobs_env_after = if_else(cycle == 1214,  CC14_325, CC12_325),
      aff_action_before = if_else(cycle == 1214, CC12_327, CC10_327),
      aff_action_after = if_else(cycle == 1214,  CC14_327, CC12_327),
      guns_before = if_else(cycle == 1214, CC12_320, CC10_320),
      guns_after = if_else(cycle == 1214,  CC14_320, CC12_320),
      tax_or_spend_before = if_else(cycle == 1214, CC12_415r, CC10_415r),
      tax_or_spend_after = if_else(cycle == 1214,  CC14_415r, CC12_415r),
      sales_or_inc_before = if_else(cycle == 1214, CC12_416r, CC10_416r),
      sales_or_inc_after = if_else(cycle == 1214,  CC14_416r, CC12_416r),
    ) %>% mutate (
      climate_change_before = if_else(climate_change_before %in% c(1:5), climate_change_before, NA),
      climate_change_after = if_else(climate_change_after %in% c(1:5), climate_change_after, NA),
      climate_change_delta = climate_change_after - climate_change_before,
      climate_change_delta_abs = abs(climate_change_delta),
      climate_change_persists = if_else(
        climate_change_delta != 0 & !(climate_change_delta * (CC14_321 - CC12_321) < 0),
        CC14_321 - CC10_321, 0),
      climate_change_persists_abs = abs(climate_change_persists),
      jobs_env_before = if_else(jobs_env_before %in% c(1:5), jobs_env_before, NA),
      jobs_env_after = if_else(jobs_env_after %in% c(1:5), jobs_env_after, NA),
      jobs_env_delta = jobs_env_after - jobs_env_before,
      jobs_env_delta_abs = abs(jobs_env_delta),
      jobs_env_persists = if_else(
        jobs_env_delta != 0 & !(jobs_env_delta * (CC14_325 - CC12_325) < 0),
        CC14_325 - CC10_325, 0),
      jobs_env_persists_abs = abs(jobs_env_persists),
      aff_action_before = if_else(aff_action_before %in% c(1:4), aff_action_before, NA),
      aff_action_after = if_else(aff_action_after %in% c(1:4), aff_action_after, NA),
      aff_action_delta = aff_action_after - aff_action_before,
      aff_action_delta_abs = abs(aff_action_delta),
      aff_action_persists = if_else(
        aff_action_delta != 0 & !(aff_action_delta * (CC14_327 - CC12_327) < 0),
        CC14_327 - CC10_327, 0),
      aff_action_persists_abs = abs(aff_action_persists),
      guns_before = if_else(guns_before %in% c(1:3), guns_before, NA),
      guns_after = if_else(guns_after %in% c(1:3), guns_after, NA),
      guns_delta =  guns_after - guns_before,
      guns_delta_abs = abs(guns_delta),
      guns_persists = if_else(
        guns_delta != 0 & !(guns_delta * (CC14_320 - CC12_320) < 0),
        CC14_320 - CC10_320, 0),
      guns_persists_abs = abs(guns_persists),
      tax_or_spend_before = if_else(tax_or_spend_before %in% c(0:100), tax_or_spend_before, NA),
      tax_or_spend_after = if_else(tax_or_spend_after %in% c(0:100), tax_or_spend_after, NA),
      tax_or_spend_delta = tax_or_spend_after - tax_or_spend_before,
      tax_or_spend_delta_abs = abs(tax_or_spend_delta),
      tax_or_spend_persists = if_else(
        tax_or_spend_delta != 0 & !(tax_or_spend_delta * (CC14_415r - CC12_415r) < 0),
        CC14_415r - CC10_415r, 0),
      tax_or_spend_persists_abs = abs(tax_or_spend_persists),
      sales_or_inc_before = if_else(sales_or_inc_before %in% c(0:100), sales_or_inc_before, NA),
      sales_or_inc_after = if_else(sales_or_inc_after %in% c(0:100), sales_or_inc_after, NA),
      sales_or_inc_delta = sales_or_inc_after - sales_or_inc_before,
      sales_or_inc_delta_abs = abs(sales_or_inc_delta),
      sales_or_inc_persists = if_else(
        sales_or_inc_delta != 0 & !(sales_or_inc_delta * (CC14_416r - CC12_416r) < 0),
        CC14_416r - CC10_416r, 0),
      sales_or_inc_persists_abs = abs(sales_or_inc_persists),
    )
  )
}

add_categorical_opinions <- function (df) {
  return(
    df %>% mutate(
      gay_marriage_before = if_else(cycle == 1214, CC12_326, CC10_326),
      gay_marriage_after = if_else(cycle == 1214,  CC14_326, CC12_326),
      schip_before = if_else(cycle == 1214, CC12_330B, CC10_330B),
      schip_after = if_else(cycle == 1214,  CC14_330B, CC12_330B),
      budget_before = if_else(cycle == 1214, CC12_328, CC10_328),
      budget_after = if_else(cycle == 1214,  CC14_328, CC12_328),
      budget_avoid_before = if_else(cycle == 1214, CC12_329, CC10_329),
      budget_avoid_after = if_else(cycle == 1214,  CC14_329, CC12_329),
    ) %>% mutate (
      gay_marriage_before = if_else(gay_marriage_before %nin% c(1, 2), NA, gay_marriage_before),
      gay_marriage_after = if_else(gay_marriage_after %nin% c(1, 2), NA, gay_marriage_after),
      gay_marriage_change = if_else(
        is.na(gay_marriage_before) | is.na(gay_marriage_after), NA,
        if_else(gay_marriage_before == gay_marriage_after, 0, 1)),
      gay_marriage_persists = if_else(cycle != 101214, NA, gay_marriage_change & CC12_326 == CC14_326),
      schip_before = if_else(schip_before %nin% c(1, 2), NA, schip_before),
      schip_after = if_else(schip_after %nin% c(1, 2), NA, schip_after),
      schip_change = if_else(
         is.na(schip_before) | is.na(schip_after), NA,
         if_else(schip_before == schip_after, 0, 1)),
      schip_persists = if_else(cycle != 101214, NA, schip_change & CC12_330B == CC14_330B),
      budget_before = if_else(budget_before %nin% c(1:3), NA, budget_before),
      budget_after = if_else(budget_after %nin% c(1:3), NA, budget_after),
      budget_change = if_else(
        is.na(budget_before) | is.na(budget_after), NA,
        if_else(budget_before == budget_after, 0, 1)),
      budget_persists = if_else(cycle != 101214, NA, budget_change & CC12_328 == CC14_328),
      budget_combo = budget_before * 10 + budget_after,
      budget_avoid_before = if_else(budget_avoid_before %nin% c(1:3), NA, budget_avoid_before),
      budget_avoid_after = if_else(budget_avoid_after %nin% c(1:3), NA, budget_avoid_after),
      budget_avoid_change = if_else(
        is.na(budget_avoid_before) | is.na(budget_avoid_after), NA,
        if_else(budget_avoid_before == budget_avoid_after, 0, 1)),
      budget_avoid_persists = if_else(cycle != 101214, NA, budget_avoid_change & CC12_329 == CC14_329),
      budget_avoid_combo = budget_avoid_before * 10 + budget_avoid_after,
    )
  )
}

add_composite_opinions <- function (df) {
  return(
    df %>% mutate(
      # TODO: add in the jobs/environment question to this composite?
      # CC10_321 is climate change: 1-5 with 1 liberal
      # CC10_330C is clean energy act, with 1 support, 2, oppose, and other values invalid
      # Composite is 1-5, with lower values more liberal
      climate_composite_2010 = if_else(CC10_330C %nin% c(1:2), NA, (CC10_321 * 2.5 + CC10_330C) / 2),
      climate_composite_2012 = if_else(CC12_330C %nin% c(1:2), NA, (CC12_321 * 2.5 + CC12_330C) / 2),
      climate_composite_2014 = if_else(CC14_330C %nin% c(1:2), NA, (CC14_321 * 2.5 + CC14_330C) / 2),
      # CC10_326 is gay marriage ban: 1 support, 2 oppose
      # CC10_330G is ending don't ask don't tell: 1 support, 2 oppose, others invalid
      gay_composite_2010 = if_else(CC10_330G %nin% c(1:2), NA, (CC10_326 + CC10_330G) / 2),
      gay_composite_2012 = if_else(CC12_330G %nin% c(1:2), NA, (CC12_326 + CC12_330G) / 2),
      gay_composite_2014 = if_else(CC14_330G %nin% c(1:2), NA, (CC14_326 + CC14_330G) / 2),
      # CC10_414_1-CC10_414_6 are all usage of military for for different reasons: 1 yes, 2 no
      # CC10_414_7 is a "none of the above" for the previous six: 1 yes, 2 no
      military_composite_2010 = (CC10_414_1 + CC10_414_2 + CC10_414_3 + CC10_414_4 + CC10_414_5 + CC10_414_6 + if_else(CC10_414_7 == 1, 2, 1)) / 7,
      military_composite_2012 = (CC12_414_1 + CC12_414_2 + CC12_414_3 + CC12_414_4 + CC12_414_5 + CC12_414_6 + if_else(CC12_414_7 == 1, 2, 1)) / 7,
      military_composite_2014 = (CC14_414_1 + CC14_414_2 + CC14_414_3 + CC14_414_4 + CC14_414_5 + CC14_414_6 + if_else(CC14_414_7 == 1, 2, 1)) / 7,
      # CC10_322_1-CC10_322_7 are all yes/no immigration questions, 8 and 9 are "nothing"/"none of the above" which aren't clearly liberal or conservative
      # For 1,7, 1 is more liberal and 2 is more conservative
      # For 2,3,4,5,6, 1 is more conservative and 2 is more liberal
      # 2010: 1 2 3 4 7
      # 2012: 1 2 3 4 5 6 
      # 2014: 1 2 3 4 5 6
      immigration_composite_2010 = (if_else(CC10_322_1 == 1, 2, 1) + CC10_322_2 + CC10_322_3 + CC10_322_4 + if_else(CC10_322_7 == 1, 2, 1)) / 5,
      immigration_composite_2012 = (if_else(CC12_322_1 == 1, 2, 1) + CC12_322_2 + CC12_322_3 + CC12_322_4 + CC12_322_5 + CC12_322_6) / 6,
      immigration_composite_2014 = (if_else(CC14_322_1 == 1, 2, 1) + CC14_322_2 + CC14_322_3 + CC14_322_4 + CC14_322_5 + CC14_322_6) / 6,
    ) %>% mutate(
      climate_composite_before = if_else(cycle == 1214, climate_composite_2012, climate_composite_2010),
      gay_composite_before = if_else(cycle == 1214, gay_composite_2012, gay_composite_2010),
      military_composite_before = if_else(cycle == 1214, military_composite_2012, military_composite_2010),
      immigration_composite_before = if_else(cycle == 1214, immigration_composite_2012, immigration_composite_2010),
      climate_composite_after = if_else(cycle == 1214, climate_composite_2014, climate_composite_2012),
      gay_composite_after = if_else(cycle == 1214, gay_composite_2014, gay_composite_2012),
      military_composite_after = if_else(cycle == 1214, military_composite_2014, military_composite_2012),
      immigration_composite_after = if_else(cycle == 1214, immigration_composite_2014, immigration_composite_2012),
    ) %>% mutate (
      climate_composite_delta = climate_composite_after - climate_composite_before,
      gay_composite_delta = gay_composite_after - gay_composite_before,
      military_composite_delta = military_composite_after - military_composite_before,
      immigration_composite_delta = immigration_composite_after - immigration_composite_before,
      climate_composite_delta_abs = abs(climate_composite_delta),
      gay_composite_delta_abs = abs(gay_composite_delta),
      military_composite_delta_abs = abs(military_composite_delta),
      immigration_composite_delta_abs = abs(immigration_composite_delta),
      climate_composite_persists = if_else(
        climate_composite_delta != 0 & !(climate_composite_delta * (climate_composite_2014 - climate_composite_2012) < 0),
        climate_composite_2014 - climate_composite_2010, 0),
      gay_composite_persists = if_else(
        gay_composite_delta != 0 & !(gay_composite_delta * (gay_composite_2014 - gay_composite_2012) < 0),
        gay_composite_2014 - gay_composite_2010, 0),
      military_composite_persists = if_else(
        military_composite_delta != 0 & !(military_composite_delta * (military_composite_2014 - military_composite_2012) < 0),
        military_composite_2014 - military_composite_2010, 0),
      immigration_composite_persists = if_else(
        immigration_composite_delta != 0 & !(immigration_composite_delta * (immigration_composite_2014 - immigration_composite_2012) < 0),
        immigration_composite_2014 - immigration_composite_2010, 0),
      climate_composite_persists_abs = abs(climate_composite_persists),
      gay_composite_persists_abs = abs(gay_composite_persists),
      military_composite_persists_abs = abs(military_composite_persists),
      immigration_composite_persists_abs = abs(immigration_composite_persists),
    )
  )
}

drop_original_issues <- function (df) {
  return(
    df %>% select(
      -ends_with("_320"), -contains("_321"), -ends_with("_325"), -ends_with("_326"),
      -ends_with("_327"), -ends_with("_328"), -ends_with("_329"), -ends_with("_330B"),
      -ends_with("_330C"), -ends_with("_330G"), -contains("_414_"),
      -ends_with("_415r"), -ends_with("_416r")
    )
  )
}

#############
# Load data #
#############
setwd("~/Documents/visualizations/midterm")
panel <- read_dta("CCES_Panel_Full3waves_VV_V4.dta") # n=9500

################
# Extract data #
################
# Build three base data frames (10-12, 12-14, 10-14)
three_years <- panel %>% zap_labels() %>% 
  select(
    "caseid",
    "weight",
    
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
    #matches("CC1[024]_324"), # abortion (1-4 conservative to liberal): ignore because of previous lit suggesting gender of child matters here
    
    # Policy issues: more on budgets
    matches("CC1[024]_415r"), # taxes vs spending (examples given are of domestic spending) (0 to 100)
    matches("CC1[024]_416r"), # raise sales vs income tax (0 to 100)
    
    # Policy issues: additional issues for composites
    matches("CC1[024]_330C"), # clean energy act (1/2 support/oppose, discard other values)
    matches("CC1[024]_330G"), # end don't ask don't tell (1/2 support/oppose, discard other values)
    matches("CC1[024]_322_[1-7]"), # immigration policies (1/2 support/oppose)
    matches("CC1[024]_414_[1-7]"), # use of military force for various purposes (1/2 yes/no)

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
  mutate(
    # Add income brackets. These are approximate, since incomes are given in ranges.
    income_quintile = case_when(
      income %in% c(1, 2) ~ 1,
      income %in% c(3, 4) ~ 2,
      income %in% c(5, 6, 7) ~ 3,
      income %in% c(8, 9, 10) ~ 4,  # note the 10 response could go into either 4th or 5th quintile
      income %in% c(11, 12, 13, 14, 15, 16) ~ 5,
      .default = NA
    ),
    # "High income" is top 20% to match Reeves
    high_income = if_else(is.na(income_quintile), NA, if_else(income_quintile == 5, 1, 0)),
    # "Low income" is bottom 40%, to very roughly correspond with SCHIP eligibility
    low_income = if_else(is.na(income_quintile), NA, if_else(income_quintile %in% c(1, 2), 1, 0)),
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
  select(-starts_with("pew_religimp_")
)
two_years <- merge(
  three_years %>% mutate(cycle = 1012),  # contains all data, but only look at 2010/2012
  three_years %>% mutate(cycle = 1214),  # contains all data, but only look at 2012/2014
  all = TRUE
)

##################
# Transform data #
##################
three_years <- add_parenting(three_years)
two_years <- add_parenting(two_years)

three_years <- add_ideo(three_years)
two_years <- add_ideo(two_years)

three_years <- add_pid(three_years)
two_years <- add_pid(two_years)

three_years <- add_continuous_opinions(three_years)
two_years <- add_continuous_opinions(two_years)

three_years <- add_categorical_opinions(three_years)
two_years <- add_categorical_opinions(two_years)

three_years <- add_composite_opinions(three_years)
two_years <- add_composite_opinions(two_years)

three_years <- drop_original_issues(three_years)
two_years <- drop_original_issues(two_years)

##########################
# Analysis: Demographics #
##########################

# In two cycles: 420 with new child, 18580 without
two_years %>% group_by(new_child) %>% summarise(count = n())

# In three cycles: 229 with new child in 2012, 9271 without
three_years %>% group_by(new_child) %>% summarise(count = n())

# Exploratory: age distributions
ggplot(three_years, aes(x = age)) +
  geom_histogram(fill = "steelblue", binwidth = 5)
three_years %>% filter(age < 30) # n=282
ggplot(two_years %>% filter(new_child == 1), aes(x = age)) + # this is surprisingly old
  geom_histogram(fill = "steelblue", binwidth = 5)
ggplot(panel %>% filter(child18_12 == 1 & child18num_12 > child18num_10), aes(x = birthyr_12)) +
  geom_histogram(fill = "steelblue", binwidth = 5)

############################
# Analysis: Ideology/Party #
############################

### Exploratory: How often do people change ideology/party between two waves?
count_flippers(three_years, "pid3_10", "pid3_12", c(1:2)) # 0.8%: pid3 is too coarse to be useful
count_flippers(three_years, "pid7_10", "pid7_12", c(1:7)) # 20%
count_flippers(three_years, "pid7_12", "pid7_14", c(1:7)) # 20%
count_flippers(three_years, "pid7_10", "pid7_14", c(1:7)) # 25%
count_flippers(three_years, "ideo5_10", "ideo5_12", c(1:5)) # 25%
count_flippers(three_years, "ideo5_12", "ideo5_14", c(1:5)) # 20%
count_flippers(three_years, "ideo5_10", "ideo5_14", c(1:5)) # 28%

### Exploratory: Ideology distribution across panel: roughly normal, skewing conservative
panel %>% group_by(ideo5_10) %>% summarise(count = n())
ggplot(three_years, aes(x = ideo5_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)
ggplot(three_years %>% filter(new_child == 1), aes(x = ideo5_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)

### Exploratory: Party distribution across panel
# Not normal, but U-shaped, with more strong Democrats but similar total Dem/Rep
panel %>% group_by(pid7_10) %>% summarise(count = n())
ggplot(three_years, aes(x = pid7_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)
# Parents are still U-shaped, a little more liberal, also looks like more moderates
ggplot(three_years %>% filter(new_child == 1), aes(x = pid7_10)) +
  geom_histogram(fill = "steelblue", binwidth = 1)

### Testing: ideological change: nothing significant
t.test(ideo_delta~new_child, data=filter_na(two_years, "ideo_delta")) # p = 0.4108
t.test(ideo_delta_abs~new_child, data=filter_na(two_years, "ideo_delta_abs")) # p = 0.6008
t.test(ideo_delta~new_child, data=filter_na(two_years %>% filter(age < 30), "ideo_delta")) # p = 0.6761
t.test(ideo_delta_abs~new_child, data=filter_na(two_years %>% filter(age < 30), "ideo_delta_abs")) # p = 0.6028

### Descriptive: ideological change
# Average ideological change over two years: trivially liberal, moreso for non-new-parents
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  group_by(new_child) %>%
  summarise(
    ideo_before = mean(ideo_before),
    ideo_after = mean(ideo_after),
    ideo_delta = mean(ideo_delta),
    ideo_delta_abs = mean(ideo_delta_abs)
)
# Counts of liberal/conservative movement, ignoring magnitude
# New parents: three_percents(49, 304, 43) = 12% more liberal, 11% more conservative
# Non-new-parents: three_percents(2230, 13853, 1795) = 12% more liberal, 10% more conservative
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  group_by(new_child, ideo_direction) %>%
  summarise(count = n())
# Using firstborn instead of new_child is still trivial, but new parents more conservative
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  group_by(firstborn) %>%
  summarise(average_ideo = mean(ideo_delta), average_ideo_abs = mean(ideo_delta_abs))
# Younger adults look about the same as the whole cohort, trivially more liberal
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  filter(age < 30) %>% 
  group_by(new_child) %>%
  summarise(
    ideo_before = mean(ideo_before),
    ideo_after = mean(ideo_after),
    ideo_delta = mean(ideo_delta),
    ideo_delta_abs = mean(ideo_delta_abs)
  )
# Counts of liberal/conservative movement, ignoring magnitude, for younger adults
# New parents: three_percents(11, 40, 5) = 20% more liberal, % more conservative
# Non-new-parents: three_percents(70, 368, 49) = 14% more liberal, 10% more conservative
two_years %>%
  filter(!is.na(ideo_delta)) %>% 
  filter(age < 30) %>% 
  group_by(new_child, ideo_direction) %>%
  summarise(count = n())

### Testing: party change: nothing significant
t.test(pid_delta~new_child, data=filter_na(two_years %>% filter(age < 30), "pid_delta")) # p = 0.4663
t.test(pid_delta_abs~new_child, data=filter_na(two_years %>% filter(age < 30), "pid_delta_abs")) # p = 0.6051
t.test(pid_delta~new_child, data=filter_na(two_years, "pid_delta")) # p = 0.4348
t.test(pid_delta_abs~new_child, data=filter_na(two_years, "pid_delta_abs")) # p = 0.07467
 
### Descriptive: party change
# Average party change over two years: bigger than ideology, but still small
# Vaguely interesting that it's a bigger change. Could just be that it's a bigger scale.
# Are people less attached to party than ideological identity?
two_years %>%
  filter(!is.na(pid_delta)) %>% 
  group_by(new_child) %>%
  summarise(
    pid_after = mean(pid_after),
    pid_delta = mean(pid_delta),
    pid_delta_abs = mean(pid_delta_abs)
  )
two_years %>%
  filter(!is.na(pid_delta)) %>% 
  group_by(firstborn) %>%
  summarise(average_pid = mean(pid_delta), average_pid_abs = mean(pid_delta_abs))
two_years %>%
  filter(!is.na(pid_delta)) %>% 
  filter(age < 30) %>% 
  group_by(firstborn) %>%
  summarise(
    pid_before = mean(pid_before),
    pid_after = mean(pid_after),
    pid_delta = mean(pid_delta),
    pid_delta_abs = mean(pid_delta_abs),
  )

####################
# Analysis: Issues #
####################

### Testing: continuous & composite issues
# "After" views: nothing
t.test(climate_change_after~new_child, data=filter_na(two_years, "climate_change_after")) # p = 0.7907
t.test(jobs_env_after~new_child, data=filter_na(two_years, "jobs_env_after")) # p = 0.4994
t.test(aff_action_after~new_child, data=filter_na(two_years, "aff_action_after")) # p = 0.851
t.test(guns_after~new_child, data=filter_na(two_years, "guns_after")) # p = 0.505
t.test(tax_or_spend_after~new_child, data=filter_na(two_years, "tax_or_spend_after")) # p = 0.1531
t.test(sales_or_inc_after~new_child, data=filter_na(two_years, "sales_or_inc_after")) # p = 0.7913
t.test(climate_composite_after~new_child, data=filter_na(two_years, "climate_composite_after")) # p = 0.9979
t.test(gay_composite_after~new_child, data=filter_na(two_years, "gay_composite_after")) # p = 0.5043
t.test(military_composite_after~new_child, data=filter_na(two_years, "military_composite_after")) # p = 0.3742
t.test(immigration_composite_after~new_child, data=filter_na(two_years, "immigration_composite_after")) # p = 0.1499

# Change, incorporating direction: nothing
t.test(climate_change_delta~new_child, data=filter_na(two_years, "climate_change_delta")) # p = 0.56
t.test(jobs_env_delta~new_child, data=filter_na(two_years, "jobs_env_delta")) # p = 0.6602
t.test(aff_action_delta~new_child, data=filter_na(two_years, "aff_action_delta")) # p = 0.9901
t.test(guns_delta~new_child, data=filter_na(two_years, "guns_delta")) # p = 0.4005
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years, "tax_or_spend_delta")) # p = 0.3224
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years, "sales_or_inc_delta")) # p = 0.345
t.test(climate_composite_delta~new_child, data=filter_na(two_years, "climate_composite_delta")) # p = 0.7822
t.test(gay_composite_delta~new_child, data=filter_na(two_years, "gay_composite_delta")) # p = 0.8233
t.test(military_composite_delta~new_child, data=filter_na(two_years, "military_composite_delta")) # p = 0.5486
t.test(immigration_composite_delta~new_child, data=filter_na(two_years, "immigration_composite_delta")) # p = 0.787

# Change, absolute value: climate change, guns: climate change & climate composite persist, and oddly so does sales or inc
t.test(climate_change_delta_abs~new_child, data=filter_na(two_years, "climate_change_delta_abs")) # p = 0.0005519***
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years, "jobs_env_delta_abs")) # p = 0.1001
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years, "aff_action_delta_abs")) # p = 0.07633
t.test(guns_delta_abs~new_child, data=filter_na(two_years, "guns_delta_abs")) # p = 0.006267**
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years, "tax_or_spend_delta_abs")) # p = 0.6814
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years, "sales_or_inc_delta_abs")) # p = 0.3438
t.test(climate_composite_delta_abs~new_child, data=filter_na(two_years, "climate_composite_delta_abs")) # p = 0.001018**
t.test(gay_composite_delta_abs~new_child, data=filter_na(two_years, "gay_composite_delta_abs")) # p = 0.1422
t.test(military_composite_delta_abs~new_child, data=filter_na(two_years, "military_composite_delta_abs")) # p = 0.1629
t.test(immigration_composite_delta_abs~new_child, data=filter_na(two_years, "immigration_composite_delta_abs")) # p = 0.1416

# Persistent change: nothing
t.test(climate_change_persists~new_child, data=filter_na(three_years, "climate_change_persists")) # p=0.1511
t.test(jobs_env_persists~new_child, data=filter_na(three_years, "jobs_env_persists")) # p=0.6261
t.test(aff_action_persists~new_child, data=filter_na(three_years, "aff_action_persists")) # p=0.3376
t.test(guns_persists~new_child, data=filter_na(three_years, "guns_persists")) # p=0.5596
t.test(tax_or_spend_persists~new_child, data=filter_na(three_years, "tax_or_spend_persists")) # p=0.1892
t.test(sales_or_inc_persists~new_child, data=filter_na(three_years, "sales_or_inc_persists")) # p=0.5886
t.test(climate_composite_persists~new_child, data=filter_na(three_years, "climate_composite_persists")) # p=0.4749
t.test(gay_composite_persists~new_child, data=filter_na(three_years, "gay_composite_persists")) # p=0.8711
t.test(military_composite_persists~new_child, data=filter_na(three_years, "military_composite_persists")) # p=0.2531
t.test(immigration_composite_persists~new_child, data=filter_na(three_years, "immigration_composite_persists")) # p=0.3216

# Persistent absolute change: climate change, tax/spend, climate composite, gay rights composite
t.test(climate_change_persists_abs~new_child, data=filter_na(three_years, "climate_change_persists_abs")) # p=0.01737*
t.test(jobs_env_persists_abs~new_child, data=filter_na(three_years, "jobs_env_persists_abs")) # p=0.5739
t.test(aff_action_persists_abs~new_child, data=filter_na(three_years, "aff_action_persists_abs")) # p=0.8902
t.test(guns_persists_abs~new_child, data=filter_na(three_years, "guns_persists_abs")) # p=0.3267
t.test(tax_or_spend_persists_abs~new_child, data=filter_na(three_years, "tax_or_spend_persists_abs")) # p=0.03717*
t.test(sales_or_inc_persists_abs~new_child, data=filter_na(three_years, "sales_or_inc_persists_abs")) # p=0.5647
t.test(climate_composite_persists_abs~new_child, data=filter_na(three_years, "climate_composite_persists_abs")) # p=0.03602*
t.test(gay_composite_persists_abs~new_child, data=filter_na(three_years, "gay_composite_persists_abs")) # p=0.02934*
t.test(military_composite_persists_abs~new_child, data=filter_na(three_years, "military_composite_persists_abs")) # p=0.2671
t.test(immigration_composite_persists_abs~new_child, data=filter_na(three_years, "immigration_composite_persists_abs")) # p=0.4937

# Switching to firstborn and looking at change: for absolute change, climate composite and gay rights composite
t.test(climate_change_delta~firstborn, data=filter_na(two_years, "climate_change_delta")) # p = 0.5514
t.test(jobs_env_delta~firstborn, data=filter_na(two_years, "jobs_env_delta")) # p = 0.4092
t.test(aff_action_delta~firstborn, data=filter_na(two_years, "aff_action_delta")) # p = 0.8448
t.test(guns_delta~firstborn, data=filter_na(two_years, "guns_delta")) # p = 0.3768
t.test(tax_or_spend_delta~firstborn, data=filter_na(two_years, "tax_or_spend_delta")) # p = 0.6834
t.test(sales_or_inc_delta~firstborn, data=filter_na(two_years, "sales_or_inc_delta")) # p = 0.447
t.test(climate_composite_delta~firstborn, data=filter_na(two_years, "climate_composite_delta")) # p = 0.9456
t.test(gay_composite_delta~firstborn, data=filter_na(two_years, "gay_composite_delta")) # p = 0.9129
t.test(military_composite_delta~firstborn, data=filter_na(two_years, "military_composite_delta")) # p = 0.8931
t.test(immigration_composite_delta~firstborn, data=filter_na(two_years, "immigration_composite_delta")) # p = 0.8848

t.test(climate_change_delta_abs~firstborn, data=filter_na(two_years, "climate_change_delta_abs")) # p = 0.001621**
t.test(jobs_env_delta_abs~firstborn, data=filter_na(two_years, "jobs_env_delta_abs")) # p = 0.1119
t.test(aff_action_delta_abs~firstborn, data=filter_na(two_years, "aff_action_delta_abs")) # p = 0.1326
t.test(guns_delta_abs~firstborn, data=filter_na(two_years, "guns_delta_abs")) # p = 0.06643
t.test(tax_or_spend_delta_abs~firstborn, data=filter_na(two_years, "tax_or_spend_delta_abs")) # p = 0.5264
t.test(sales_or_inc_delta_abs~firstborn, data=filter_na(two_years, "sales_or_inc_delta_abs")) # p = 0.2689
t.test(climate_composite_delta_abs~firstborn, data=filter_na(two_years, "climate_composite_delta_abs")) # p = 0.00165**
t.test(gay_composite_delta_abs~firstborn, data=filter_na(two_years, "gay_composite_delta_abs")) # p = 0.03384*
t.test(military_composite_delta_abs~firstborn, data=filter_na(two_years, "military_composite_delta_abs")) # p = 0.3324
t.test(immigration_composite_delta_abs~firstborn, data=filter_na(two_years, "immigration_composite_delta_abs")) # p = 0.821

# Summary of continuous & composite issues
summarize_continuous(two_years, "new_child", "climate_change")
summarize_continuous(two_years, "new_child", "jobs_env")
summarize_continuous(two_years, "new_child", "aff_action")
summarize_continuous(two_years, "new_child", "guns")
summarize_continuous(two_years, "new_child", "tax_or_spend")
summarize_continuous(two_years, "new_child", "sales_or_inc")
summarize_continuous(two_years, "new_child", "climate_composite")
summarize_continuous(two_years, "new_child", "gay_composite")
summarize_continuous(two_years, "new_child", "military_composite")
summarize_continuous(two_years, "new_child", "immigration_composite")
 
# Non-response rates, continuous & composite issues
two_years %>% mutate(has_climate_change = if_else(is.na(climate_change_delta), 0, 1)) %>% group_by(new_child, has_climate_change) %>% summarise(count = n())
two_years %>% mutate(has_jobs_env = if_else(is.na(jobs_env_delta), 0, 1)) %>% group_by(new_child, has_jobs_env) %>% summarise(count = n())
two_years %>% mutate(has_aff_action = if_else(is.na(aff_action_delta), 0, 1)) %>% group_by(new_child, has_aff_action) %>% summarise(count = n())
two_years %>% mutate(has_guns = if_else(is.na(guns_delta), 0, 1)) %>% group_by(new_child, has_guns) %>% summarise(count = n())
two_years %>% mutate(has_tax_or_spend = if_else(is.na(tax_or_spend_delta), 0, 1)) %>% group_by(new_child, has_tax_or_spend) %>% summarise(count = n())
two_years %>% mutate(has_sales_or_inc = if_else(is.na(sales_or_inc_delta), 0, 1)) %>% group_by(new_child, has_sales_or_inc) %>% summarise(count = n())
two_years %>% mutate(has_climate_composite = if_else(is.na(climate_composite_delta), 0, 1)) %>% group_by(new_child, has_climate_composite) %>% summarise(count = n())
two_years %>% mutate(has_gay_composite = if_else(is.na(gay_composite_delta), 0, 1)) %>% group_by(new_child, has_gay_composite) %>% summarise(count = n())
two_years %>% mutate(has_military_composite = if_else(is.na(military_composite_delta), 0, 1)) %>% group_by(new_child, has_military_composite) %>% summarise(count = n())
two_years %>% mutate(has_immigration_composite = if_else(is.na(immigration_composite_delta), 0, 1)) %>% group_by(new_child, has_immigration_composite) %>% summarise(count = n())

# Non-response rates, categorical issues
two_years %>% group_by(new_child, gay_marriage_change) %>% summarise(count = n())
two_years %>% group_by(new_child, schip_change) %>% summarise(count = n())
two_years %>% group_by(new_child, budget_change) %>% summarise(count = n())
two_years %>% group_by(new_child, budget_avoid_change) %>% summarise(count = n()) # highest NA responses, at 3%

### Testing: categorical variables: both budget questions, but neither persists
run_chisq(two_years, "new_child", "ideo_direction") # p=0.8664
run_chisq(two_years, "new_child", "pid_direction") # p=0.3215, but p=0.07 when looking at firstborn
run_chisq(two_years, "new_child", "gay_marriage_change") # p=0.1347
run_chisq(two_years, "new_child", "schip_change") # p=0.3306
run_chisq(two_years, "new_child", "budget_change") # p=0.00280**
run_chisq(two_years, "new_child", "budget_avoid_change") # p=0.0154*
run_chisq(three_years, "new_child", "gay_marriage_persists") # p=0.06199
run_chisq(three_years, "new_child", "schip_persists") # p=0.6948
run_chisq(three_years, "new_child", "budget_persists") # p=0.32
run_chisq(three_years, "new_child", "budget_avoid_persists") # p=1
run_chisq(two_years, "new_child", "gay_marriage_after") # p=0.2971
run_chisq(two_years, "new_child", "schip_after") # p=0.8188
run_chisq(two_years, "new_child", "budget_after") # p=0.224
run_chisq(two_years, "new_child", "budget_avoid_after") # p=0.0814

# Descriptive statistics on categorical issues
two_years %>% group_by(new_child, gay_marriage_before) %>% summarise(count = n())
two_years %>% group_by(new_child, gay_marriage_after) %>% summarise(count = n())
two_percents(165, 254) # parents: 39 / 61
two_percents(6793, 11680) # others: 37 / 63
two_years %>% group_by(new_child, schip_after) %>% summarise(count = n())
two_percents(294, 123) # parents: 71 / 29
two_percents(12862, 5549) # others: 70 / 30
two_years %>% group_by(new_child, schip_after) %>% summarise(count = n())
# parents: three_percents(141, 207, 68) = 34 / 50 / 16
# others: three_percents(6886, 8139, 3357) = 37 / 44 / 18
two_years %>% group_by(new_child, budget_before) %>% summarise(count = n())
# parents: three_percents(141, 207, 68) = 34 / 50 / 16
# others: three_percents(6886, 8139, 3357) = 37 / 44 / 18
two_years %>% group_by(new_child, budget_after) %>% summarise(count = n())
# parents: three_percents(148, 187, 79) = 36 / 45 / 19
# others: three_percents(6230, 7948, 4157) = 34 / 43 / 23
two_years %>% group_by(new_child, budget_avoid_before) %>% summarise(count = n())
# parents: three_percents(58, 127, 230) = 14 / 31 / 35
# others: three_percents(3006, 6833, 8450) = 16 / 37 / 46
two_years %>% group_by(new_child, budget_avoid_after) %>% summarise(count = n())
# parents: three_percents(82, 145, 183) = 20 / 35 / 45
# others: three_percents(3967, 7098, 7125) = 22 / 39 / 39


two_years %>% group_by(new_child, gay_marriage_after) %>% summarise(count = n())
two_percents(165, 254) # parents: 39 / 61
two_percents(6793, 11680) # others: 37 / 63
two_years %>% group_by(new_child, gay_marriage_change) %>% summarise(count = n())

two_years_new_parents %>% group_by(gender, gay_marriage_after) %>% summarise(count = n())

# Persistence: how common is persistent change?
# Of the new parents who changed, how many keep that change?
# New parents often slightly more likely to experience persistent change than others

continuous_persists(three_years, "climate_change") # 25% vs 18%
continuous_persists(three_years, "jobs_env") # 23% vs 23%
continuous_persists(three_years, "aff_action") # 16% vs 16%
continuous_persists(three_years, "guns") # 16% vs 14%
continuous_persists(three_years, "tax_or_spend") # 29% vs 35%
continuous_persists(three_years, "sales_or_inc") # 39% vs 36%
continuous_persists(three_years, "climate_composite") # 30% vs 21%
continuous_persists(three_years, "gay_composite") # 17% vs 11%
continuous_persists(three_years, "military_composite") # 32% vs 30%
continuous_persists(three_years, "immigration_composite") # 50% vs 48%
 
categorical_persists(three_years, "gay_marriage") # 10% vs 7%
categorical_persists(three_years, "schip") # 15% vs 12%
categorical_persists(three_years, "budget") # 15% vs 12%
categorical_persists(three_years, "budget_avoid") # 16% vs 16%

####################
# Analysis: Gender #
####################
two_years_new_parents <- two_years %>% filter(new_child == 1)
three_years_new_parents <- three_years %>% filter(new_child == 1)
two_years_men <- two_years %>% filter(gender == 1)
three_years_men <- three_years %>% filter(gender == 1)
two_years_women <- two_years %>% filter(gender == 2)
three_years_women <- three_years %>% filter(gender == 2)

### Ideology/party
filter_na(two_years, "ideo_delta") %>% group_by(new_child, gender) %>% summarise(
  ideo_before = mean(ideo_before),
  ideo_after = mean(ideo_after),
  ideo_delta = mean(ideo_delta),
  ideo_delta_abs = mean(ideo_delta_abs),
)
filter_na(two_years, "pid_delta") %>% group_by(new_child, gender) %>% summarise(
  pid_before = mean(pid_before),
  pid_after = mean(pid_after),
  pid_delta = mean(pid_delta),
  pid_delta_abs = mean(pid_delta_abs),
)

# Compare new fathers to new mothers: nothing
t.test(ideo_delta~gender, data=filter_na(two_years_new_parents, "ideo_delta")) # p = 0.9288
t.test(pid_delta~gender, data=filter_na(two_years_new_parents, "pid_delta")) # p = 0.09352 # weird that this is so different from ideo_delta, but groups are small
t.test(ideo_delta_abs~gender, data=filter_na(two_years_new_parents, "ideo_delta_abs")) # p = 0.2446
t.test(pid_delta_abs~gender, data=filter_na(two_years_new_parents, "pid_delta_abs")) # p = 0.9272

# Compare new fathers to other men: nothing
t.test(ideo_delta~new_child, data=filter_na(two_years_men, "ideo_delta")) # p = 0.4799
t.test(pid_delta~new_child, data=filter_na(two_years_men, "pid_delta")) # p = 0.6003
t.test(ideo_delta_abs~new_child, data=filter_na(two_years_men, "ideo_delta_abs")) # p = 0.8063
t.test(pid_delta_abs~new_child, data=filter_na(two_years_men, "pid_delta_abs")) # p = 0.3356

# Compare new mothers to other women: nothing
t.test(ideo_delta~new_child, data=filter_na(two_years_women, "ideo_delta")) # p = 0.6568
t.test(pid_delta~new_child, data=filter_na(two_years_women, "pid_delta")) # p = 0.1065
t.test(ideo_delta_abs~new_child, data=filter_na(two_years_women, "ideo_delta_abs")) # p = 0.4037
t.test(pid_delta_abs~new_child, data=filter_na(two_years_women, "pid_delta_abs")) # p = 0.1042


### Continuous issues
# Compare new fathers to new mothers: tax vs spend, jobs vs env, aff action: nothing persists
t.test(climate_change_delta~gender, data=filter_na(two_years_new_parents, "climate_change_delta")) # p=0.8752
t.test(jobs_env_delta~gender, data=filter_na(two_years_new_parents, "jobs_env_delta")) # p=0.7913
t.test(aff_action_delta~gender, data=filter_na(two_years_new_parents, "aff_action_delta")) # p=0.0800
t.test(guns_delta~gender, data=filter_na(two_years_new_parents, "guns_delta")) # p=0.08586
t.test(tax_or_spend_delta~gender, data=filter_na(two_years_new_parents, "tax_or_spend_delta")) # p=0.04358*
t.test(sales_or_inc_delta~gender, data=filter_na(two_years_new_parents, "sales_or_inc_delta")) # p=0.4426
t.test(climate_composite_delta~gender, data=filter_na(two_years_new_parents, "climate_composite_delta")) # p=0.94667
t.test(gay_composite_delta~gender, data=filter_na(two_years_new_parents, "gay_composite_delta")) # p=0.7277
t.test(military_composite_delta~gender, data=filter_na(two_years_new_parents, "military_composite_delta")) # p=0.113
t.test(immigration_composite_delta~gender, data=filter_na(two_years_new_parents, "immigration_composite_delta")) # p=0.5765

# Absolute change: jobs/env, affirmative actions, and military & climate composites have some distinction
t.test(climate_change_delta_abs~gender, data=filter_na(two_years_new_parents, "climate_change_delta_abs")) # p=0.1647
t.test(jobs_env_delta_abs~gender, data=filter_na(two_years_new_parents, "jobs_env_delta_abs")) # p=0.02414*
t.test(aff_action_delta_abs~gender, data=filter_na(two_years_new_parents, "aff_action_delta_abs")) # p=0.005968**
t.test(guns_delta_abs~gender, data=filter_na(two_years_new_parents, "guns_delta_abs")) # p=0.6765
t.test(tax_or_spend_delta_abs~gender, data=filter_na(two_years_new_parents, "tax_or_spend_delta_abs")) # p=0.4833
t.test(sales_or_inc_delta_abs~gender, data=filter_na(two_years_new_parents, "sales_or_inc_delta_abs")) # p=0.7338
t.test(climate_composite_delta_abs~gender, data=filter_na(two_years_new_parents, "climate_composite_delta_abs")) # p=0.03053*
t.test(gay_composite_delta_abs~gender, data=filter_na(two_years_new_parents, "gay_composite_delta_abs")) # p=0.06045
t.test(military_composite_delta_abs~gender, data=filter_na(two_years_new_parents, "military_composite_delta_abs")) # p=0.008634**
t.test(immigration_composite_delta_abs~gender, data=filter_na(two_years_new_parents, "immigration_composite_delta_abs")) # p=0.2242

summarize_continuous(two_years_new_parents, "gender", "climate_change")
summarize_continuous(two_years_new_parents, "gender", "jobs_env")
summarize_continuous(two_years_new_parents, "gender", "aff_action")
summarize_continuous(two_years_new_parents, "gender", "guns")
summarize_continuous(two_years_new_parents, "gender", "tax_or_spend")
summarize_continuous(two_years_new_parents, "gender", "sales_or_inc")
summarize_continuous(two_years_new_parents, "gender", "climate_composite")
summarize_continuous(two_years_new_parents, "gender", "gay_composite")
summarize_continuous(two_years_new_parents, "gender", "military_composite")
summarize_continuous(two_years_new_parents, "gender", "immigration_composite")

summarize_continuous_helper(two_years %>% group_by(new_child, gender), "climate_change")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "jobs_env")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "aff_action")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "guns")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "tax_or_spend")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "sales_or_inc")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "climate_composite")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "gay_composite")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "military_composite")
summarize_continuous_helper(two_years %>% group_by(new_child, gender), "immigration_composite")

t.test(climate_change_persists~gender, data=filter_na(three_years_new_parents, "climate_change_persists")) # p=0.9324
t.test(tax_or_spend_persists~gender, data=filter_na(three_years_new_parents, "tax_or_spend_persists")) # p=0.3928
t.test(jobs_env_persists~gender, data=filter_na(three_years_new_parents, "jobs_env_persists")) # p=0.7585
t.test(aff_action_persists~gender, data=filter_na(three_years_new_parents, "aff_action_persists")) # p=0.6927
t.test(climate_composite_persists~gender, data=filter_na(three_years_new_parents, "climate_composite_persists")) # p=0.7354
t.test(military_composite_persists~gender, data=filter_na(three_years_new_parents, "military_composite_persists")) # p=0.7544

t.test(climate_change_persists_abs~gender, data=filter_na(three_years_new_parents, "climate_change_persists_abs")) # p=0.7848
t.test(tax_or_spend_persists_abs~gender, data=filter_na(three_years_new_parents, "tax_or_spend_persists_abs")) # p=0.426
t.test(jobs_env_persists_abs~gender, data=filter_na(three_years_new_parents, "jobs_env_persists_abs")) # p=0.5003
t.test(aff_action_persists_abs~gender, data=filter_na(three_years_new_parents, "aff_action_persists_abs")) # p=0.1995
t.test(climate_composite_persists_abs~gender, data=filter_na(three_years_new_parents, "climate_composite_persists_abs")) # p=0.7691
t.test(military_composite_persists_abs~gender, data=filter_na(three_years_new_parents, "military_composite_persists_abs")) # p=0.7647
 
# Compare new fathers to all men: tax or spend with direction: doesn't persist, but abs does
t.test(climate_change_delta~new_child, data=filter_na(two_years_men, "climate_change_delta")) # p=0.7033
t.test(jobs_env_delta~new_child, data=filter_na(two_years_men, "jobs_env_delta")) # p=0.6611
t.test(aff_action_delta~new_child, data=filter_na(two_years_men, "aff_action_delta")) # p=0.5205
t.test(guns_delta~new_child, data=filter_na(two_years_men, "guns_delta")) # p=0.102
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years_men, "tax_or_spend_delta")) # p=0.02781*
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years_men, "sales_or_inc_delta")) # p=0.6959
t.test(climate_composite_delta~new_child, data=filter_na(two_years_men, "climate_composite_delta")) # p=0.9079
t.test(gay_composite_delta~new_child, data=filter_na(two_years_men, "gay_composite_delta")) # p=0.9638
t.test(military_composite_delta~new_child, data=filter_na(two_years_men, "military_composite_delta")) # p=0.1029
t.test(immigration_composite_delta~new_child, data=filter_na(two_years_men, "immigration_composite_delta")) # p=0.4878

t.test(climate_change_delta_abs~new_child, data=filter_na(two_years_men, "climate_change_delta_abs")) # p=0.1032
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years_men, "jobs_env_delta_abs")) # p=0.884
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years_men, "aff_action_delta_abs")) # p=0.8886
t.test(guns_delta_abs~new_child, data=filter_na(two_years_men, "guns_delta_abs")) # p=0.1481
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years_men, "tax_or_spend_delta_abs")) # p=0.7743
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years_men, "sales_or_inc_delta_abs")) # p=0.8004
t.test(climate_composite_delta_abs~new_child, data=filter_na(two_years_men, "climate_composite_delta_abs")) # p=0.2943
t.test(gay_composite_delta_abs~new_child, data=filter_na(two_years_men, "gay_composite_delta_abs")) # p=0.5969
t.test(military_composite_delta_abs~new_child, data=filter_na(two_years_men, "military_composite_delta_abs")) # p=0.5996
t.test(immigration_composite_delta_abs~new_child, data=filter_na(two_years_men, "immigration_composite_delta_abs")) # p=0.9586
summarize_continuous(two_years_men, "new_child", "tax_or_spend")

t.test(tax_or_spend_persists~new_child, data=filter_na(three_years_men, "tax_or_spend_persists")) # p=0.07183
t.test(tax_or_spend_persists_abs~new_child, data=filter_na(three_years_men, "tax_or_spend_persists_abs")) # p=0.0003767

t.test(climate_change_persists~high_income, data=filter_na(three_years_new_parents, "climate_change_persists")) # p=0.5825

# Compare new mothers to all women: climate change, guns: none persist
t.test(climate_change_delta~new_child, data=filter_na(two_years_women, "climate_change_delta")) # p=0.6708
t.test(jobs_env_delta~new_child, data=filter_na(two_years_women, "jobs_env_delta")) # p=0.8081
t.test(aff_action_delta~new_child, data=filter_na(two_years_women, "aff_action_delta")) # p=0.6396
t.test(guns_delta~new_child, data=filter_na(two_years_women, "guns_delta")) # p=0.7687
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years_women, "tax_or_spend_delta")) # p=0.3475
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years_women, "sales_or_inc_delta")) # p=0.3451
t.test(climate_composite_delta~new_child, data=filter_na(two_years_women, "climate_composite_delta")) # p=0.7853
t.test(gay_composite_delta~new_child, data=filter_na(two_years_women, "gay_composite_delta")) # p=0.7501
t.test(military_composite_delta~new_child, data=filter_na(two_years_women, "military_composite_delta")) # p=0.6741
t.test(immigration_composite_delta~new_child, data=filter_na(two_years_women, "immigration_composite_delta")) # p=0.8084

t.test(climate_change_delta_abs~new_child, data=filter_na(two_years_women, "climate_change_delta_abs")) # p=0.001844**
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years_women, "jobs_env_delta_abs")) # p=0.06299
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years_women, "aff_action_delta_abs")) # p=0.05126
t.test(guns_delta_abs~new_child, data=filter_na(two_years_women, "guns_delta_abs")) # p=0.01459*
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years_women, "tax_or_spend_delta_abs")) # p=0.6958
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years_women, "sales_or_inc_delta_abs")) # p=0.2613
t.test(climate_composite_delta_abs~new_child, data=filter_na(two_years_women, "climate_composite_delta_abs")) # p=0.0008981***
t.test(gay_composite_delta_abs~new_child, data=filter_na(two_years_women, "gay_composite_delta_abs")) # p=0.001213**
t.test(military_composite_delta_abs~new_child, data=filter_na(two_years_women, "military_composite_delta_abs")) # p=0.538
t.test(immigration_composite_delta_abs~new_child, data=filter_na(two_years_women, "immigration_composite_delta_abs")) # p=0.5516

summarize_continuous(two_years_women, "new_child", "climate_change")
summarize_continuous(two_years_women, "new_child", "guns")
summarize_continuous(two_years_women, "new_child", "climate_composite")
summarize_continuous(two_years_women, "new_child", "gay_composite")

t.test(climate_change_persists~new_child, data=filter_na(three_years_women, "climate_change_persists")) # p=0.2937
t.test(climate_change_persists_abs~new_child, data=filter_na(three_years_women, "climate_change_persists_abs")) # p=0.1275
t.test(guns_persists~new_child, data=filter_na(three_years_women, "guns_persists")) # p=0.7097
t.test(guns_persists_abs~new_child, data=filter_na(three_years_women, "guns_persists_abs")) # p=0.4326
t.test(climate_composite_persists~new_child, data=filter_na(three_years_women, "climate_composite_persists")) # p=0.798
t.test(gay_composite_persists~new_child, data=filter_na(three_years_women, "gay_composite_persists")) # p=0.6172
t.test(military_composite_persists~new_child, data=filter_na(three_years_women, "military_composite_persists")) # p=0.4658
t.test(immigration_composite_persists~new_child, data=filter_na(three_years_women, "immigration_composite_persists")) # p=0.5404
t.test(climate_composite_persists_abs~new_child, data=filter_na(three_years_women, "climate_composite_persists_abs")) # p=0.2395
t.test(gay_composite_persists_abs~new_child, data=filter_na(three_years_women, "gay_composite_persists_abs")) # p=0.01103*
t.test(military_composite_persists_abs~new_child, data=filter_na(three_years_women, "military_composite_persists_abs")) # p=0.4897
t.test(immigration_composite_persists_abs~new_child, data=filter_na(three_years_women, "immigration_composite_persists_abs")) # p=0.6124

### Categorical issues

# Compare new fathers to new mothers: both budget questions: budget persists
run_chisq(two_years_new_parents, "gender", "ideo_direction") # p=0.595
run_chisq(two_years_new_parents, "gender", "pid_direction") # p=0.1408
run_chisq(two_years_new_parents, "gender", "gay_marriage_change") # p=0.485
run_chisq(two_years_new_parents, "gender", "schip_change") # p=0.6808
run_chisq(two_years_new_parents, "gender", "budget_change") # p=0.00001167**
run_chisq(two_years_new_parents, "gender", "budget_avoid_change") # p=0.0005327***

run_chisq(three_years_new_parents, "gender", "budget_persists") # p=0.002303**
run_chisq(three_years_new_parents, "gender", "budget_avoid_persists") # p=0.516

run_chisq(three_years_high_income, "new_child", "schip_persists") # p=0.? "Chi-squared approximation may be incorrect"
run_chisq(three_years_low_income, "new_child", "budget_persists") # p=0.09921

# Comparing new fathers to new mothers on budget_change
two_years_new_parents %>% group_by(gender, budget_before) %>% summarise(count = n())
# women: three_percents(77, 96, 39) = 36% / 45% / 18%
# men: three_percents(64, 111, 29) = 31% / 54% / 14%
two_years_new_parents %>% group_by(gender, budget_after) %>% summarise(count = n())
# women: 83 / 89 / 40 = 212 = 39% / 42% / 19%
# men: 65 / 98 / 39 = 202 = 32% / 49% / 19%
two_years_new_parents %>% group_by(gender, budget_change) %>% summarise(count = n())
# men %age who change: 4200 / (202)
# women %age who change: 8600 / (208)

# Compare new fathers to other men: nothing
run_chisq(two_years_men, "new_child", "ideo_direction") # p=0.8416
run_chisq(two_years_men, "new_child", "pid_direction") # p=0.2836
run_chisq(two_years_men, "new_child", "gay_marriage_change") # p=0.1541
run_chisq(two_years_men, "new_child", "schip_change") # p=1
run_chisq(two_years_men, "new_child", "budget_change") # p=1
run_chisq(two_years_men, "new_child", "budget_avoid_change") # p=0.8526

# Compare new mothers to other women: both budget questions: both persist
run_chisq(two_years_women, "new_child", "ideo_direction") # p=0.7833
run_chisq(two_years_women, "new_child", "pid_direction") # p=0.1103
run_chisq(two_years_women, "new_child", "gay_marriage_change") # p=0.5236
run_chisq(two_years_women, "new_child", "schip_change") # p=0.1476
run_chisq(two_years_women, "new_child", "budget_change") # p=0.0003211***
run_chisq(two_years_women, "new_child", "budget_avoid_change") # p=0.0006512***

run_chisq(three_years_women, "gender", "budget_persists") # p < 0.00000000000000022***
run_chisq(three_years_women, "gender", "budget_avoid_persists") # p < 0.00000000000000022***

# Comparing new mothers to other women on budget_change and budget_change_avoid
two_years_women %>% group_by(new_child, budget_before) %>% summarise(count = n())
# mothers: three_percents(77, 96, 39) = 36 / 45 / 18
# non-mothers: three_percents(3575, 2854, 1685)  = 44 / 35 / 21
two_years_women %>% group_by(new_child, budget_after) %>% summarise(count = n())
# mothers: three_percents(83, 89, 40) = 39 / 42 / 19
# non-mothers: three_percents(3157, 2804, 2127) = 39 / 35 / 26
two_years_women %>% group_by(new_child, budget_avoid_before) %>% summarise(count = n())
# mothers: three_percents(29, 69, 113) = 14 / 33 / 54
# non-mothers: three_percents(1239, 3386, 3452) = 15 / 42 / 43
two_years_women %>% group_by(new_child, budget_avoid_after) %>% summarise(count = n())
# mothers: three_percents(51, 76, 83) = 24 / 36 / 40
# non-mothers: three_percents(1616, 3526, 2877) = 20 / 44 / 36


####################
# Analysis: Income #
####################

# Exploratory: what does the income distribution look like across the panel?
ggplot(panel %>% filter(faminc_14 < 19), aes(x = faminc_14)) +
  geom_histogram(fill = "steelblue", binwidth = 1)
panel %>% group_by(faminc_14) %>% summarise(count = n())

# Exploratory: what does the income distribution look like for new parents?
three_years %>% filter(new_child == 1) %>% group_by(new_child, income) %>% summarise(count = n())
three_years %>% group_by(income_quintile, new_child) %>% summarise(count = n())
three_years %>% group_by(high_income) %>% summarise(count = n())
three_years %>% group_by(low_income) %>% summarise(count = n())
ggplot(three_years %>% filter(!is.na(income_quintile)) %>% filter(new_child == 1), aes(x = income)) +
  geom_histogram(fill = "steelblue", binwidth = 1)

two_years_high_income <- two_years %>% filter(high_income == 1)
three_years_high_income <- three_years %>% filter(high_income == 1)
two_years_low_income <- two_years %>% filter(high_income == 0)
three_years_low_income <- three_years %>% filter(high_income == 0)

# Ideology & party: nothing
two_years %>% filter(!is.na(high_income)) %>% group_by(new_child, high_income) %>% summarise(
  ideo_before = mean(ideo_before, na.rm = TRUE),
  ideo_after = mean(ideo_after, na.rm = TRUE),
  ideo_delta = mean(ideo_delta, na.rm = TRUE),
  ideo_delta_abs = mean(ideo_delta_abs, na.rm = TRUE),
  pid_before = mean(pid_before, na.rm = TRUE),
  pid_after = mean(pid_after, na.rm = TRUE),
  pid_delta = mean(pid_delta, na.rm = TRUE),
  pid_delta_abs = mean(pid_delta_abs, na.rm = TRUE),
)
run_chisq(two_years_new_parents, "high_income", "ideo_direction") # p=0.517
run_chisq(two_years_new_parents, "high_income", "pid_direction") # p=0.5566
t.test(ideo_delta~high_income, data=filter_na(two_years_new_parents, "ideo_delta")) # p=0.8842
t.test(pid_delta~high_income, data=filter_na(two_years_new_parents, "pid_delta")) # p=0.4337
t.test(ideo_delta_abs~high_income, data=filter_na(two_years_new_parents, "ideo_delta")) # p=0.1004
t.test(pid_delta_abs~high_income, data=filter_na(two_years_new_parents, "pid_delta")) # p=0.8435
t.test(ideo_delta~new_child, data=filter_na(two_years_high_income, "ideo_delta")) # p=0.9653
t.test(pid_delta~new_child, data=filter_na(two_years_high_income, "pid_delta")) # p=0.7799
t.test(ideo_delta_abs~new_child, data=filter_na(two_years_high_income, "ideo_delta")) # p=0.2312
t.test(pid_delta_abs~new_child, data=filter_na(two_years_high_income, "pid_delta")) # p=0.4288
t.test(ideo_delta~new_child, data=filter_na(two_years_low_income, "ideo_delta")) # p=0.6019
t.test(pid_delta~new_child, data=filter_na(two_years_low_income, "pid_delta")) # p=0.1405
t.test(ideo_delta_abs~new_child, data=filter_na(two_years_low_income, "ideo_delta")) # p=0.4017
t.test(pid_delta_abs~new_child, data=filter_na(two_years_low_income, "pid_delta")) # p=0.2996

# Comparing high-income new parents with other new parents: continuous: climate change
t.test(climate_change_delta~high_income, data=filter_na(two_years_new_parents, "climate_change_delta")) # p=0.4601
t.test(jobs_env_delta~high_income, data=filter_na(two_years_new_parents, "jobs_env_delta")) # p=0.1588
t.test(aff_action_delta~high_income, data=filter_na(two_years_new_parents, "aff_action_delta")) # p=0.2171
t.test(guns_delta~high_income, data=filter_na(two_years_new_parents, "guns_delta")) # p=0.2396
t.test(tax_or_spend_delta~high_income, data=filter_na(two_years_new_parents, "tax_or_spend_delta")) # p=0.8402
t.test(sales_or_inc_delta~high_income, data=filter_na(two_years_new_parents, "sales_or_inc_delta")) # p=0.6458
t.test(climate_composite_delta~high_income, data=filter_na(two_years_new_parents, "climate_composite_delta")) # p=0.249
t.test(gay_composite_delta~high_income, data=filter_na(two_years_new_parents, "gay_composite_delta")) # p=0.3821
t.test(military_composite_delta~high_income, data=filter_na(two_years_new_parents, "military_composite_delta")) # p=0.8474
t.test(immigration_composite_delta~high_income, data=filter_na(two_years_new_parents, "immigration_composite_delta")) # p=0.4787

t.test(climate_change_delta_abs~high_income, data=filter_na(two_years_new_parents, "climate_change_delta_abs")) # p=0.00521**
t.test(jobs_env_delta_abs~high_income, data=filter_na(two_years_new_parents, "jobs_env_delta_abs")) # p=0.2419
t.test(aff_action_delta_abs~high_income, data=filter_na(two_years_new_parents, "aff_action_delta_abs")) # p=0.4451
t.test(guns_delta_abs~high_income, data=filter_na(two_years_new_parents, "guns_delta_abs")) # p=0.09408
t.test(tax_or_spend_delta_abs~high_income, data=filter_na(two_years_new_parents, "tax_or_spend_delta_abs")) # p=0.9808
t.test(sales_or_inc_delta_abs~high_income, data=filter_na(two_years_new_parents, "sales_or_inc_delta_abs")) # p=0.5666
t.test(climate_composite_delta_abs~high_income, data=filter_na(two_years_new_parents, "climate_composite_delta_abs")) # p=0.01215*
t.test(gay_composite_delta_abs~high_income, data=filter_na(two_years_new_parents, "gay_composite_delta_abs")) # p=0.9746
t.test(military_composite_delta_abs~high_income, data=filter_na(two_years_new_parents, "military_composite_delta_abs")) # p=0.05247
t.test(immigration_composite_delta_abs~high_income, data=filter_na(two_years_new_parents, "immigration_composite_delta_abs")) # p=0.4996

summarize_continuous(two_years_new_parents, "high_income", "climate_change")
summarize_continuous(two_years_new_parents, "high_income", "climate_composite")

# Comparing high-income new parents with other high-income people: nothing
t.test(climate_change_delta~new_child, data=filter_na(two_years_high_income, "climate_change_delta")) # p=0.4548
t.test(jobs_env_delta~new_child, data=filter_na(two_years_high_income, "jobs_env_delta")) # p=0.1061
t.test(aff_action_delta~new_child, data=filter_na(two_years_high_income, "aff_action_delta")) # p=0.2238
t.test(guns_delta~new_child, data=filter_na(two_years_high_income, "guns_delta")) # p=0.1571
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years_high_income, "tax_or_spend_delta")) # p=0.5931
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years_high_income, "sales_or_inc_delta")) # p=0.3198
t.test(climate_composite_delta~new_child, data=filter_na(two_years_high_income, "climate_composite_delta")) # p=0.5003
t.test(gay_composite_delta~new_child, data=filter_na(two_years_high_income, "gay_composite_delta")) # p=0.6491
t.test(military_composite_delta~new_child, data=filter_na(two_years_high_income, "military_composite_delta")) # p=0.7735
t.test(immigration_composite_delta~new_child, data=filter_na(two_years_high_income, "immigration_composite_delta")) # p=0.4249

t.test(climate_change_delta_abs~new_child, data=filter_na(two_years_high_income, "climate_change_delta_abs")) # p=0.9549
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years_high_income, "jobs_env_delta_abs")) # p=0.9809
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years_high_income, "aff_action_delta_abs")) # p=0.5395
t.test(guns_delta_abs~new_child, data=filter_na(two_years_high_income, "guns_delta_abs")) # p=0.836
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years_high_income, "tax_or_spend_delta_abs")) # p=0.9156
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years_high_income, "sales_or_inc_delta_abs")) # p=0.8254
t.test(climate_composite_delta_abs~new_child, data=filter_na(two_years_high_income, "climate_composite_delta_abs")) # p=0.7129
t.test(gay_composite_delta_abs~new_child, data=filter_na(two_years_high_income, "gay_composite_delta_abs")) # p=0.112
t.test(military_composite_delta_abs~new_child, data=filter_na(two_years_high_income, "military_composite_delta_abs")) # p=0.5006
t.test(immigration_composite_delta_abs~new_child, data=filter_na(two_years_high_income, "immigration_composite_delta_abs")) # p=0.1593

# Comparing low-income new parents with other low-income people: climate change, jobs env, guns
t.test(climate_change_delta~new_child, data=filter_na(two_years_low_income, "climate_change_delta")) # p=0.925
t.test(jobs_env_delta~new_child, data=filter_na(two_years_low_income, "jobs_env_delta")) # p=0.9821
t.test(aff_action_delta~new_child, data=filter_na(two_years_low_income, "aff_action_delta")) # p=0.6958
t.test(guns_delta~new_child, data=filter_na(two_years_low_income, "guns_delta")) # p=0.9603
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years_low_income, "tax_or_spend_delta")) # p=0.296
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years_low_income, "sales_or_inc_delta")) # p=0.4142
t.test(climate_composite_delta~new_child, data=filter_na(two_years_low_income, "climate_composite_delta")) # p=0.3734
t.test(gay_composite_delta~new_child, data=filter_na(two_years_low_income, "gay_composite_delta")) # p=0.6488
t.test(military_composite_delta~new_child, data=filter_na(two_years_low_income, "military_composite_delta")) # p=0.6024
t.test(immigration_composite_delta~new_child, data=filter_na(two_years_low_income, "immigration_composite_delta")) # p=0.6518

t.test(climate_change_delta_abs~new_child, data=filter_na(two_years_low_income, "climate_change_delta_abs")) # p=0.0002297***
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years_low_income, "jobs_env_delta_abs")) # p=0.04443*
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years_low_income, "aff_action_delta_abs")) # p=0.08989
t.test(guns_delta_abs~new_child, data=filter_na(two_years_low_income, "guns_delta_abs")) # p=0.007776**
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years_low_income, "tax_or_spend_delta_abs")) # p=0.1105
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years_low_income, "sales_or_inc_delta_abs")) # p=0.3083
t.test(climate_composite_delta_abs~new_child, data=filter_na(two_years_low_income, "climate_composite_delta_abs")) # p=0.0006077***
t.test(gay_composite_delta_abs~new_child, data=filter_na(two_years_low_income, "gay_composite_delta_abs")) # p=0.007429**
t.test(military_composite_delta_abs~new_child, data=filter_na(two_years_low_income, "military_composite_delta_abs")) # p=0.2346
t.test(immigration_composite_delta_abs~new_child, data=filter_na(two_years_low_income, "immigration_composite_delta_abs")) # p=0.9871

summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "climate_change")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "jobs_env")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "aff_action")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "guns")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "tax_or_spend")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "sales_or_inc")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "climate_composite")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "gay_composite")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "military_composite")
summarize_continuous_helper(filter_na(two_years, "high_income") %>% group_by(new_child, high_income), "immigration_composite")

t.test(climate_change_persists~high_income, data=filter_na(three_years_new_parents, "climate_change_persists")) # p=0.5825
t.test(climate_change_persists_abs~high_income, data=filter_na(three_years_new_parents, "climate_change_persists_abs")) # p=0.5661
t.test(climate_composite_persists~high_income, data=filter_na(three_years_new_parents, "climate_composite_persists")) # p=0.6485
t.test(climate_composite_persists_abs~high_income, data=filter_na(three_years_new_parents, "climate_composite_persists_abs")) # p=0.7648

t.test(climate_change_persists_abs~new_child, data=filter_na(three_years_low_income, "climate_change_persists_abs")) # p=0.05492
t.test(climate_composite_persists_abs~new_child, data=filter_na(three_years_low_income, "climate_composite_persists_abs")) # p=0.1125
t.test(jobs_env_persists_abs~new_child, data=filter_na(three_years_low_income, "jobs_env_persists_abs")) # p=0.4577

# Chi square tests within new parents: high_income, low_income: nothing
run_chisq(two_years_new_parents, "high_income", "gay_marriage_change") # p=0.1
run_chisq(two_years_new_parents, "high_income", "schip_change") # p=0.9467
run_chisq(two_years_new_parents, "low_income", "schip_change") # p=0.4875
run_chisq(two_years_new_parents, "high_income", "budget_change") # p=0.2925
run_chisq(two_years_new_parents, "high_income", "budget_avoid_change") # p=0.8583

# Chi square tests within high income: SCHIP
run_chisq(two_years_high_income, "new_child", "gay_marriage_change") # p=0.5897
run_chisq(two_years_high_income, "new_child", "schip_change") # p=0.3227
run_chisq(two_years_high_income, "low_income", "schip_change") # p < 0.00000000000000022***
run_chisq(two_years_high_income, "new_child", "budget_change") # p=0.4254
run_chisq(two_years_high_income, "new_child", "budget_avoid_change") # p=0.0746

# Chi square tests within low income: budget
run_chisq(two_years_low_income, "new_child", "gay_marriage_change") # p=0.2626
run_chisq(two_years_low_income, "new_child", "schip_change") # p=0.2921
run_chisq(two_years_low_income, "low_income", "schip_change") # p=0.2311
run_chisq(two_years_low_income, "new_child", "budget_change") # p=0.00079***
run_chisq(two_years_low_income, "new_child", "budget_avoid_change") # p=0.05246

# SCHIP
two_years %>% filter(!is.na(high_income), !is.na(schip_before)) %>% group_by(new_child, high_income, schip_before) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income), !is.na(schip_after)) %>% group_by(new_child, high_income, schip_after) %>% summarise(count = n())