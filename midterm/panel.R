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
  return (  
    data %>% group_by(eval(ecol(group_by))) %>% summarise(
      before = mean(eval(parse(text=paste(c(issue, "_before"), collapse=""))), na.rm = TRUE),
      after = mean(eval(parse(text=paste(c(issue, "_after"), collapse=""))), na.rm = TRUE),
      delta = mean(eval(parse(text=paste(c(issue, "_delta"), collapse=""))), na.rm = TRUE),
      delta_abs = mean(eval(parse(text=paste(c(issue, "_delta_abs"), collapse=""))), na.rm = TRUE),
    )
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
      climate_change_after = if_else(cycle == 1012,  CC12_321, CC14_321),
      jobs_env_before = if_else(cycle == 1214, CC12_325, CC10_325),
      jobs_env_after = if_else(cycle == 1012,  CC12_325, CC14_325),
      aff_action_before = if_else(cycle == 1214, CC12_327, CC10_327),
      aff_action_after = if_else(cycle == 1012,  CC12_327, CC14_327),
      guns_before = if_else(cycle == 1214, CC12_320, CC10_320),
      guns_after = if_else(cycle == 1012,  CC12_320, CC14_320),
      tax_or_spend_before = if_else(cycle == 1214, CC12_415r, CC10_415r),
      tax_or_spend_after = if_else(cycle == 1012,  CC12_415r, CC14_415r),
      sales_or_inc_before = if_else(cycle == 1214, CC12_416r, CC10_416r),
      sales_or_inc_after = if_else(cycle == 1012,  CC12_416r, CC14_416r),
    ) %>% mutate (
      climate_change_before = if_else(climate_change_before %in% c(1:5), climate_change_before, NA),
      climate_change_after = if_else(climate_change_after %in% c(1:5), climate_change_after, NA),
      climate_change_delta = climate_change_after - climate_change_before,
      climate_change_delta_abs = abs(climate_change_delta),
      jobs_env_before = if_else(jobs_env_before %in% c(1:5), jobs_env_before, NA),
      jobs_env_after = if_else(jobs_env_after %in% c(1:5), jobs_env_after, NA),
      jobs_env_delta =  jobs_env_after - jobs_env_before,
      jobs_env_delta_abs = abs(jobs_env_delta),
      aff_action_before = if_else(aff_action_before %in% c(1:4), aff_action_before, NA),
      aff_action_after = if_else(aff_action_after %in% c(1:4), aff_action_after, NA),
      aff_action_delta = aff_action_after - aff_action_before,
      aff_action_delta_abs = abs(aff_action_delta),
      guns_before = if_else(guns_before %in% c(1:3), guns_before, NA),
      guns_after = if_else(guns_after %in% c(1:3), guns_after, NA),
      guns_delta =  guns_after - guns_before,
      guns_delta_abs = abs(guns_delta),
      tax_or_spend_before = if_else(tax_or_spend_before %in% c(0:100), tax_or_spend_before, NA),
      tax_or_spend_after = if_else(tax_or_spend_after %in% c(0:100), tax_or_spend_after, NA),
      tax_or_spend_delta = tax_or_spend_after - tax_or_spend_before,
      tax_or_spend_delta_abs = abs(tax_or_spend_delta),
      sales_or_inc_before = if_else(sales_or_inc_before %in% c(0:100), sales_or_inc_before, NA),
      sales_or_inc_after = if_else(sales_or_inc_after %in% c(0:100), sales_or_inc_after, NA),
      sales_or_inc_delta = sales_or_inc_after - sales_or_inc_before,
      sales_or_inc_delta_abs = abs(sales_or_inc_delta),
    ) %>% select(-ends_with("_320"), -ends_with("_321"), -ends_with("_325"), -ends_with("_327"),
                 -ends_with("_415r"), -ends_with("_416r"))
  )
}

add_categorical_opinions <- function (df) {
  return(
    df %>% mutate(
      gay_marriage_before = if_else(cycle == 1214, CC12_326, CC10_326),
      gay_marriage_after = if_else(cycle == 1012,  CC12_326, CC14_326),
      schip_before = if_else(cycle == 1214, CC12_330B, CC10_330B),
      schip_after = if_else(cycle == 1012,  CC12_330B, CC14_330B),
      budget_before = if_else(cycle == 1214, CC12_328, CC10_328),
      budget_after = if_else(cycle == 1012,  CC12_328, CC14_328),
      budget_avoid_before = if_else(cycle == 1214, CC12_329, CC10_329),
      budget_avoid_after = if_else(cycle == 1012,  CC12_329, CC14_329),
    ) %>% mutate (
      gay_marriage_before = if_else(gay_marriage_before %nin% c(1, 2), NA, gay_marriage_before),
      gay_marriage_after = if_else(gay_marriage_after %nin% c(1, 2), NA, gay_marriage_after),
      gay_marriage_change = if_else(
        is.na(gay_marriage_before) | is.na(gay_marriage_after), NA,
        if_else(gay_marriage_before == gay_marriage_after, 0, 1)),
      schip_before = if_else(schip_before %nin% c(1, 2), NA, schip_before),
      schip_after = if_else(schip_after %nin% c(1, 2), NA, schip_after),
      schip_change = if_else(
         is.na(schip_before) | is.na(schip_after), NA,
         if_else(schip_before == schip_after, 0, 1)),
      budget_before = if_else(budget_before %nin% c(1:3), NA, budget_before),
      budget_after = if_else(budget_after %nin% c(1:3), NA, budget_after),
      budget_change = if_else(
        is.na(budget_before) | is.na(budget_after), NA,
        if_else(budget_before == budget_after, 0, 1)),
      budget_combo = budget_before * 10 + budget_after,
      budget_avoid_before = if_else(budget_avoid_before %nin% c(1:3), NA, budget_avoid_before),
      budget_avoid_after = if_else(budget_avoid_after %nin% c(1:3), NA, budget_avoid_after),
      budget_avoid_change = if_else(
        is.na(budget_avoid_before) | is.na(budget_avoid_after), NA,
        if_else(budget_avoid_before == budget_avoid_after, 0, 1)),
      budget_avoid_combo = budget_avoid_before * 10 + budget_avoid_after,
    ) %>% select(-ends_with("_326"), -ends_with("_328"), -ends_with("_329"), -ends_with("_330B"))
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
  summarise(average_ideo = mean(ideo_delta), average_ideo_abs = mean(ideo_delta_abs))
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
  summarise(average_ideo = mean(ideo_delta), average_ideo_abs = mean(ideo_delta_abs))
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

####################
# Analysis: Issues #
####################

### Testing: continuous issues
# "After" views: nothing
t.test(climate_change_after~new_child, data=filter_na(two_years, "climate_change_after")) # p = 0.7907
t.test(jobs_env_after~new_child, data=filter_na(two_years, "jobs_env_after")) # p = 0.4994
t.test(aff_action_after~new_child, data=filter_na(two_years, "aff_action_after")) # p = 0.851
t.test(guns_after~new_child, data=filter_na(two_years, "guns_after")) # p = 0.505
t.test(tax_or_spend_after~new_child, data=filter_na(two_years, "tax_or_spend_after")) # p = 0.1531
t.test(sales_or_inc_after~new_child, data=filter_na(two_years, "sales_or_inc_after")) # p = 0.7913

# Change, incorporating direction: nothing
t.test(climate_change_delta~new_child, data=filter_na(two_years, "climate_change_delta")) # p = 0.56
t.test(jobs_env_delta~new_child, data=filter_na(two_years, "jobs_env_delta")) # p = 0.6602
t.test(aff_action_delta~new_child, data=filter_na(two_years, "aff_action_delta")) # p = 0.9901
t.test(guns_delta~new_child, data=filter_na(two_years, "guns_delta")) # p = 0.4005
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years, "tax_or_spend_delta")) # p = 0.3224
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years, "sales_or_inc_delta")) # p = 0.345

# Change, absolute value: climate change, guns
t.test(climate_change_delta_abs~new_child, data=filter_na(two_years, "climate_change_delta_abs")) # p = 0.0005519***
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years, "jobs_env_delta_abs")) # p = 0.1001
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years, "aff_action_delta_abs")) # p = 0.07633
t.test(guns_delta_abs~new_child, data=filter_na(two_years, "guns_delta_abs")) # p = 0.006267**
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years, "tax_or_spend_delta_abs")) # p = 0.6814
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years, "sales_or_inc_delta_abs")) # p = 0.3438

# Switching to firstborn and looking at change: nothing
t.test(climate_change_delta~firstborn, data=filter_na(two_years, "climate_change_delta")) # p = 0.5514
t.test(jobs_env_delta~firstborn, data=filter_na(two_years, "jobs_env_delta")) # p = 0.4092
t.test(aff_action_delta~firstborn, data=filter_na(two_years, "aff_action_delta")) # p = 0.8448
t.test(guns_delta~firstborn, data=filter_na(two_years, "guns_delta")) # p = 0.3768
t.test(tax_or_spend_delta~firstborn, data=filter_na(two_years, "tax_or_spend_delta")) # p = 0.6834
t.test(sales_or_inc_delta~firstborn, data=filter_na(two_years, "sales_or_inc_delta")) # p = 0.447

t.test(climate_change_delta_abs~firstborn, data=filter_na(two_years, "climate_change_delta_abs")) # p = 0.4946
t.test(jobs_env_delta_abs~firstborn, data=filter_na(two_years, "jobs_env_delta_abs")) # p = 0.1119
t.test(aff_action_delta_abs~firstborn, data=filter_na(two_years, "aff_action_delta_abs")) # p = 0.1326
t.test(guns_delta_abs~firstborn, data=filter_na(two_years, "guns_delta_abs")) # p = 0.06643
t.test(tax_or_spend_delta_abs~firstborn, data=filter_na(two_years, "tax_or_spend_delta_abs")) # p = 0.5264
t.test(sales_or_inc_delta_abs~firstborn, data=filter_na(two_years, "sales_or_inc_delta_abs")) # p = 0.2689

# Summary of continuous issues
continuous_stats <- two_years %>% group_by(new_child) %>% summarise(
  #climate_change_before = mean(climate_change_before, na.rm = TRUE),
  climate_change_after = mean(climate_change_after, na.rm = TRUE),
  #climate_change_delta = mean(climate_change_delta, na.rm = TRUE),
  climate_change_delta_abs = mean(climate_change_delta_abs, na.rm = TRUE),
  #jobs_env_before = mean(jobs_env_before, na.rm = TRUE),
  jobs_env_after = mean(jobs_env_after, na.rm = TRUE),
  #jobs_env_delta = mean(jobs_env_delta, na.rm = TRUE),
  jobs_env_delta_abs = mean(jobs_env_delta_abs, na.rm = TRUE),
  #aff_action_before = mean(aff_action_before, na.rm = TRUE),
  aff_action_after = mean(aff_action_after, na.rm = TRUE),
  #aff_action_delta = mean(aff_action_delta, na.rm = TRUE),
  aff_action_delta_abs = mean(aff_action_delta_abs, na.rm = TRUE),
  #guns_before = mean(guns_before, na.rm = TRUE),
  guns_after = mean(guns_after, na.rm = TRUE),
  #guns_delta = mean(guns_delta, na.rm = TRUE),
  guns_delta_abs = mean(guns_delta_abs, na.rm = TRUE),
  #tax_or_spend_before = mean(tax_or_spend_before, na.rm = TRUE),
  tax_or_spend_after = mean(tax_or_spend_after, na.rm = TRUE),
  #tax_or_spend_delta = mean(tax_or_spend_delta, na.rm = TRUE),
  tax_or_spend_delta_abs = mean(tax_or_spend_delta_abs, na.rm = TRUE),
  #sales_or_inc_before = mean(sales_or_inc_before, na.rm = TRUE),
  sales_or_inc_after = mean(sales_or_inc_after, na.rm = TRUE),
  #sales_or_inc_delta = mean(sales_or_inc_delta, na.rm = TRUE),
  sales_or_inc_delta_abs = mean(sales_or_inc_delta_abs, na.rm = TRUE),
)
 
# Non-response rates, continuous issues
two_years %>% mutate(has_climate_change = if_else(is.na(climate_change_delta), 0, 1)) %>% group_by(new_child, has_climate_change) %>% summarise(count = n())
two_years %>% mutate(has_jobs_env = if_else(is.na(jobs_env_delta), 0, 1)) %>% group_by(new_child, has_jobs_env) %>% summarise(count = n())
two_years %>% mutate(has_aff_action = if_else(is.na(aff_action_delta), 0, 1)) %>% group_by(new_child, has_aff_action) %>% summarise(count = n())
two_years %>% mutate(has_guns = if_else(is.na(guns_delta), 0, 1)) %>% group_by(new_child, has_guns) %>% summarise(count = n())
two_years %>% mutate(has_tax_or_spend = if_else(is.na(tax_or_spend_delta), 0, 1)) %>% group_by(new_child, has_tax_or_spend) %>% summarise(count = n())
two_years %>% mutate(has_sales_or_inc = if_else(is.na(sales_or_inc_delta), 0, 1)) %>% group_by(new_child, has_sales_or_inc) %>% summarise(count = n())

# Non-response rates, categorical issues
two_years %>% group_by(new_child, gay_marriage_change) %>% summarise(count = n())
two_years %>% group_by(new_child, schip_change) %>% summarise(count = n())
two_years %>% group_by(new_child, budget_change) %>% summarise(count = n())
two_years %>% group_by(new_child, budget_avoid_change) %>% summarise(count = n()) # highest NA responses, at 3%

### Testing: categorical variables: both budget questions
run_chisq(two_years, "new_child", "ideo_direction") # p=0.8664
run_chisq(two_years, "new_child", "pid_direction") # p=0.3215, but p=0.07 when looking at firstborn
run_chisq(two_years, "new_child", "gay_marriage_change") # p=0.1347
run_chisq(two_years, "new_child", "schip_change") # p=0.3306
run_chisq(two_years, "new_child", "budget_change") # p=0.00280**
run_chisq(two_years, "new_child", "budget_avoid_change") # p=0.0154*
run_chisq(two_years, "new_child", "gay_marriage_after") # p=0.2971
run_chisq(two_years, "new_child", "schip_after") # p=0.8188
run_chisq(two_years, "new_child", "budget_after") # p=0.224
run_chisq(two_years, "new_child", "budget_avoid_after") # p=0.0814

# Descriptive statistics on budget
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

####################
# Analysis: Gender #
####################
two_years_new_parents <- two_years %>% filter(new_child == 1)
two_years_men <- two_years %>% filter(gender == 1)
two_years_women <- two_years %>% filter(gender == 2)

### Ideology/party
filter_na(two_years, "ideo_delta") %>% group_by(new_child, gender) %>% summarise(
  ideo_before = mean(ideo_before),
  ideo_after = mean(ideo_after),
)
filter_na(two_years, "pid_delta") %>% group_by(new_child, gender) %>% summarise(
  pid_before = mean(pid_before),
  pid_after = mean(pid_after),
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
# Compare new fathers to new mothers: tax vs spend, jobs vs env, aff action
t.test(climate_change_delta~gender, data=filter_na(two_years_new_parents, "climate_change_delta")) # p=0.8752
t.test(jobs_env_delta~gender, data=filter_na(two_years_new_parents, "jobs_env_delta")) # p=0.7913
t.test(aff_action_delta~gender, data=filter_na(two_years_new_parents, "aff_action_delta")) # p=0.0800
t.test(guns_delta~gender, data=filter_na(two_years_new_parents, "guns_delta")) # p=0.08586
t.test(tax_or_spend_delta~gender, data=filter_na(two_years_new_parents, "tax_or_spend_delta")) # p=0.04358*
t.test(sales_or_inc_delta~gender, data=filter_na(two_years_new_parents, "sales_or_inc_delta")) # p=0.4426
t.test(climate_change_delta_abs~gender, data=filter_na(two_years_new_parents, "climate_change_delta_abs")) # p=0.1647
t.test(jobs_env_delta_abs~gender, data=filter_na(two_years_new_parents, "jobs_env_delta_abs")) # p=0.02414*
t.test(aff_action_delta_abs~gender, data=filter_na(two_years_new_parents, "aff_action_delta_abs")) # p=0.005968**
t.test(guns_delta_abs~gender, data=filter_na(two_years_new_parents, "guns_delta_abs")) # p=0.6765
t.test(tax_or_spend_delta_abs~gender, data=filter_na(two_years_new_parents, "tax_or_spend_delta_abs")) # p=0.4833
t.test(sales_or_inc_delta_abs~gender, data=filter_na(two_years_new_parents, "sales_or_inc_delta_abs")) # p=0.7338
summarize_continuous(two_years_new_parents, "gender", "tax_or_spend")
summarize_continuous(two_years_new_parents, "gender", "jobs_env")
summarize_continuous(two_years_new_parents, "gender", "aff_action")
 
# Compare new fathers to all men: tax or spend
t.test(climate_change_delta~new_child, data=filter_na(two_years_men, "climate_change_delta")) # p=0.7033
t.test(jobs_env_delta~new_child, data=filter_na(two_years_men, "jobs_env_delta")) # p=0.6611
t.test(aff_action_delta~new_child, data=filter_na(two_years_men, "aff_action_delta")) # p=0.5205
t.test(guns_delta~new_child, data=filter_na(two_years_men, "guns_delta")) # p=0.102
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years_men, "tax_or_spend_delta")) # p=0.02781*
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years_men, "sales_or_inc_delta")) # p=0.6959
t.test(climate_change_delta_abs~new_child, data=filter_na(two_years_men, "climate_change_delta_abs")) # p=0.1032
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years_men, "jobs_env_delta_abs")) # p=0.884
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years_men, "aff_action_delta_abs")) # p=0.8886
t.test(guns_delta_abs~new_child, data=filter_na(two_years_men, "guns_delta_abs")) # p=0.1481
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years_men, "tax_or_spend_delta_abs")) # p=0.7743
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years_men, "sales_or_inc_delta_abs")) # p=0.8004
summarize_continuous(two_years_men, "new_child", "tax_or_spend")

# Compare new mothers to all women: climate change, guns
t.test(climate_change_delta~new_child, data=filter_na(two_years_women, "climate_change_delta")) # p=0.6708
t.test(jobs_env_delta~new_child, data=filter_na(two_years_women, "jobs_env_delta")) # p=0.8081
t.test(aff_action_delta~new_child, data=filter_na(two_years_women, "aff_action_delta")) # p=0.6396
t.test(guns_delta~new_child, data=filter_na(two_years_women, "guns_delta")) # p=0.7687
t.test(tax_or_spend_delta~new_child, data=filter_na(two_years_women, "tax_or_spend_delta")) # p=0.3475
t.test(sales_or_inc_delta~new_child, data=filter_na(two_years_women, "sales_or_inc_delta")) # p=0.3451
t.test(climate_change_delta_abs~new_child, data=filter_na(two_years_women, "climate_change_delta_abs")) # p=0.001844**
t.test(jobs_env_delta_abs~new_child, data=filter_na(two_years_women, "jobs_env_delta_abs")) # p=0.06299
t.test(aff_action_delta_abs~new_child, data=filter_na(two_years_women, "aff_action_delta_abs")) # p=0.05126
t.test(guns_delta_abs~new_child, data=filter_na(two_years_women, "guns_delta_abs")) # p=0.01459*
t.test(tax_or_spend_delta_abs~new_child, data=filter_na(two_years_women, "tax_or_spend_delta_abs")) # p=0.6958
t.test(sales_or_inc_delta_abs~new_child, data=filter_na(two_years_women, "sales_or_inc_delta_abs")) # p=0.2613
summarize_continuous(two_years_women, "new_child", "climate_change")
summarize_continuous(two_years_women, "new_child", "guns")

### Categorical issues

# Compare new fathers to new mothers: both budget questions
run_chisq(two_years_new_parents, "gender", "ideo_direction") # p=0.595
run_chisq(two_years_new_parents, "gender", "pid_direction") # p=0.1408
run_chisq(two_years_new_parents, "gender", "gay_marriage_change") # p=0.485
run_chisq(two_years_new_parents, "gender", "schip_change") # p=0.6808
run_chisq(two_years_new_parents, "gender", "budget_change") # p=0.00001167**
run_chisq(two_years_new_parents, "gender", "budget_avoid_change") # p=0.0005327***

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

# Compare new mothers to other women: both budget questions
run_chisq(two_years_women, "new_child", "ideo_direction") # p=0.7833
run_chisq(two_years_women, "new_child", "pid_direction") # p=0.1103
run_chisq(two_years_women, "new_child", "gay_marriage_change") # p=0.5236
run_chisq(two_years_women, "new_child", "schip_change") # p=0.1476
run_chisq(two_years_women, "new_child", "budget_change") # p=0.0003211***
run_chisq(two_years_women, "new_child", "budget_avoid_change") # p=0.0006512***

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




# Comparing high-income parents to other parents, same for low-income
two_years_high_income <- two_years %>% filter(high_income == 1)
two_years_low_income <- two_years %>% filter(high_income == 0)
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
t.test(ideo_delta~high_income, data=filter_na(two_years_new_parents, "ideo_delta")) # p = 0.9682
t.test(pid_delta~high_income, data=filter_na(two_years_new_parents, "pid_delta")) # p = 0.977
t.test(ideo_delta_abs~high_income, data=filter_na(two_years_new_parents, "ideo_delta")) # p = 0.0287**
t.test(pid_delta_abs~high_income, data=filter_na(two_years_new_parents, "pid_delta")) # p = 0.4497
t.test(ideo_delta~new_child, data=filter_na(two_years_high_income, "ideo_delta")) # p = 0.5725
t.test(pid_delta~new_child, data=filter_na(two_years_high_income, "pid_delta")) # p = 0.7628
t.test(ideo_delta_abs~new_child, data=filter_na(two_years_high_income, "ideo_delta")) # p = 0.2613
t.test(pid_delta_abs~new_child, data=filter_na(two_years_high_income, "pid_delta")) # p = 0.1528
t.test(ideo_delta~new_child, data=filter_na(two_years_low_income, "ideo_delta")) # p = 0.4406
t.test(pid_delta~new_child, data=filter_na(two_years_low_income, "pid_delta")) # p = 0.2779
t.test(ideo_delta_abs~new_child, data=filter_na(two_years_low_income, "ideo_delta")) # p = 0.04348**
t.test(pid_delta_abs~new_child, data=filter_na(two_years_low_income, "pid_delta")) # p = 0.1083

two_years_new_parents %>% filter_na("high_income") %>% group_by(high_income) %>% summarise(
  ideo_before = mean(ideo_before, na.rm = TRUE),
  ideo_after = mean(ideo_after, na.rm = TRUE),
  ideo_delta = mean(ideo_delta, na.rm = TRUE),
  ideo_delta_abs = mean(ideo_delta_abs, na.rm = TRUE),
)
two_years_low_income %>% filter_na("high_income") %>% group_by(new_child) %>% summarise(
  ideo_before = mean(ideo_before, na.rm = TRUE),
  ideo_after = mean(ideo_after, na.rm = TRUE),
  ideo_delta = mean(ideo_delta, na.rm = TRUE),
  ideo_delta_abs = mean(ideo_delta_abs, na.rm = TRUE),
)



t.test(climate_change_delta~high_income, data=filter_na(two_years_new_parents, "climate_change_delta")) # p=0.3505
t.test(jobs_env_delta~high_income, data=filter_na(two_years_new_parents, "jobs_env_delta")) # p=0.3174
t.test(aff_action_delta~high_income, data=filter_na(two_years_new_parents, "aff_action_delta")) # p=0.1575
t.test(guns_delta~high_income, data=filter_na(two_years_new_parents, "guns_delta")) # p=0.3622
t.test(tax_or_spend_delta~high_income, data=filter_na(two_years_new_parents, "tax_or_spend_delta")) # p=0.2071
t.test(sales_or_inc_delta~high_income, data=filter_na(two_years_new_parents, "sales_or_inc_delta")) # p=0.4536
t.test(climate_change_delta_abs~high_income, data=filter_na(two_years_new_parents, "climate_change_delta_abs")) # p=0.4348
t.test(jobs_env_delta_abs~high_income, data=filter_na(two_years_new_parents, "jobs_env_delta_abs")) # p=0.5803
t.test(aff_action_delta_abs~high_income, data=filter_na(two_years_new_parents, "aff_action_delta_abs")) # p=0.01992*
t.test(guns_delta_abs~high_income, data=filter_na(two_years_new_parents, "guns_delta_abs")) # p=0.41
t.test(tax_or_spend_delta_abs~high_income, data=filter_na(two_years_new_parents, "tax_or_spend_delta_abs")) # p=0.562
t.test(sales_or_inc_delta_abs~high_income, data=filter_na(two_years_new_parents, "sales_or_inc_delta_abs")) # p=0.9427




# Chi square tests within new parents: high_income, low_income
run_chisq(two_years_new_parents, "high_income", "ideo_direction") # p=0.29
run_chisq(two_years_new_parents, "high_income", "pid_direction") # p=0.5819
run_chisq(two_years_new_parents, "high_income", "gay_marriage_change") # p=0.021*
run_chisq(two_years_new_parents, "high_income", "schip_change") # p=0.1
run_chisq(two_years_new_parents, "low_income", "schip_change") # p=0.8031
run_chisq(two_years_new_parents, "high_income", "budget_change") # p=0.2812
run_chisq(two_years_new_parents, "high_income", "budget_avoid_change") # p=0.08207

two_years_new_parents %>% group_by(high_income) %>% summarise(
  aff_action_before = mean(aff_action_before, na.rm = TRUE),
  aff_action_after = mean(aff_action_after, na.rm = TRUE),
  aff_action_delta_abs = mean(aff_action_delta_abs, na.rm = TRUE),
  aff_action_delta = mean(aff_action_delta, na.rm = TRUE),
)
two_years_new_parents %>% filter(!is.na(high_income)) %>% group_by(high_income, gay_marriage_before) %>% summarise(count = n())
two_years_new_parents %>% filter(!is.na(high_income)) %>% group_by(high_income, gay_marriage_after) %>% summarise(count = n())
# high income before: 22 / 42 = 34 / 66
# high income after: 24 / 40 = 38 / 62
# low income before: 150 / 192 = 62 / 38
# low income after: 160 / 184 = 47 / 53
two_years %>% filter(!is.na(high_income)) %>% group_by(high_income, gay_marriage_before) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income)) %>% group_by(high_income, gay_marriage_after) %>% summarise(count = n())
# high income before: two_percents(778, 1882) = 29 / 71
# high income after: two_percents(652, 2006) = 25 / 75
# low income before: two_percents(5490, 8250) = 40 / 60
# low income after: two_percents(4988, 8730) = 36 / 64
two_years %>% filter(!is.na(high_income), !is.na(gay_marriage_before)) %>% group_by(new_child, high_income, gay_marriage_before) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income), !is.na(gay_marriage_after)) %>% group_by(new_child, high_income, gay_marriage_after) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income), !is.na(schip_before)) %>% group_by(new_child, high_income, schip_before) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income), !is.na(schip_after)) %>% group_by(new_child, high_income, schip_after) %>% summarise(count = n())

two_years_new_parents %>% filter(!is.na(high_income)) %>% group_by(high_income, schip_before) %>% summarise(count = n())
two_years_new_parents %>% filter(!is.na(high_income)) %>% group_by(high_income, schip_after) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income)) %>% group_by(high_income, schip_before) %>% summarise(count = n())
two_years %>% filter(!is.na(high_income)) %>% group_by(high_income, schip_after) %>% summarise(count = n())