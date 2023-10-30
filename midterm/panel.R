# Overall TODO (there are specific TODOs for all of these):
#   0) See if any existing code could/should be extracted into functions
#      (just read top to bottom, considering I'll want to vary ideology,
#      policy, and a two-wave versus three-wave data set)
#   1) Make function to check lm and chisq for a given variable
#   2) Parameterize ideology, use that function
#   3) Add controls to that function
#   4) Use that function for all the policy variables
#   5) Add additional controls
#   6) Look at  consistency in ideology change over 3 cycles (for all ideology variables)

library(haven)
library(tidyverse)

setwd("~/Documents/visualizations/midterm")
panel <- read_dta("CCES_Panel_Full3waves_VV_V4.dta") # n=9500

# Drop most columns
slimmed <- panel %>% #zap_labels() %>% 
  select(
    # Ideology and partisanship
    starts_with("ideo5_"),
    matches("pid3_1[024]"), # TODO: add to analysis (related TODO below)
    starts_with("pid7_"), # TODO: add to analysis (related TODO below)
    
    # TODO: Check if any of these change
    #   Add before/after columns, then check lm and chisq.test for each
    #   (make a function that takes a data frame, adds the before/after values,
    #   filters to valid values, and does the lm and chisq.test, and maybe
    #   also does the lms with controls)
    matches("CC1[024]_320"), # gun control (1-3 more to less strict)
    matches("CC1[024]_321"), # climate change (1-5 real to not real)
    #matches("CC1[024]_322_[1-6]"), # immigration (multiple yes/no questions): ignore because of specificity of policies
    #matches("CC1[024]_324"), # abortion (1-4 conservative to liberal): ignore because of previous lit suggesting gender of child matters here
    matches("CC1[024]_325"), # job vs environment (1-5 favor environment to favor jobs)
    matches("CC1[024]_326"), # gay marriage (1/2 no/yes): note 
    matches("CC1[024]_327"), # affirmative action (1-4 support to oppose)
    matches("CC1[024]_328"), # budget (1 cut defense, 2 cut domestic, 3 raise taxes)
    matches("CC1[024]_329"), # budget move to avoid (1 cut defense, 2 cut domestic, 3 raise taxes)
    #matches("CC1[024]_332"), # roll call votes (multiple, 1/2 support/oppose, discard other values): ignore because of specificity (and inconsistency between years)
    
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
  )

data_1012 <- slimmed %>% mutate(cycle = 1012)  # This will be the 2010/2012 data
data_1214 <- slimmed %>% mutate(cycle = 1214)  # This will be the 2012/2014 data

all_data <- merge(data_1012, data_1214, all = TRUE) %>% 
  mutate(gender = gender_10, # Verified gender doesn't change for anyone: all_data %>% filter(gender_10 != gender_12 | gender_12 != gender_14 | gender_10 != gender_14)
         age = 2023 - birthyr_10, # Does this change?
         new_child = if_else(cycle == 1012, child18_10 < child18_12, child18_12 < child18_14),
         new_father = if_else(!new_child, NA, gender == 1),
         new_mother = if_else(!new_child, NA, gender == 2),
         
         # TODO: do this in a separate statement, so I can make the rest of this statement a function and try out different ideology variables
         ideo_before = if_else(cycle == 1012, ideo5_10, ideo5_12),
         ideo_after = if_else(cycle == 1012, ideo5_12, ideo5_14),
         ideo_delta = ideo_after - ideo_before,
         
         # Demographics: arbitrarily preferring the later responses
         income = coalesce(faminc_14, faminc_12, faminc_10),
         income_bracket = if_else(income %in% seq(1,9),
                                  "low",
                                  if_else(income %in% seq(10, 18),
                                          "high",
                                          "unknown"))) %>% 
         # TODO: add these columns for other controls
         # TODO: also remove the originals in the select(-c()) right below
         #starts_with("investor_"), # TODO: add to analysis (money in stocks)
         #starts_with("newsint_"), # TODO: add to analysis (interest in news/politics (1 high - 4 little, discard other values))
         #starts_with("race_"), # Limit to 1-8, categorical
         #starts_with("educ_"), # Limit to 1-6, categorical
         #starts_with("marstat_"), # Limit to 1-6, categorical
         #starts_with("pew_religimp_"), # Limit to 1-4, 1 is "very important"
  # TODO: pull column names into lists like `original_demographic_colums`
  # for the sake of DRYing this up?
  select(-c(gender_10, gender_12, gender_14,
            ideo5_10, ideo5_12, ideo5_14,
            child18_10, child18_12, child18_14,
            child18num_10, child18num_12, child18num_14,
            faminc_10, faminc_12, faminc_14)) %>% 
  filter(!is.na(new_child)) # only 2 rows, based on all_data %>% group_by(new_child) %>% summarise(count = n())

trends <- all_data %>% 
  filter(ideo_before < 6, ideo_after < 6) %>% # TODO: note this will be different by ideology
  mutate(leftward = ideo_before > ideo_after,
         rightward = ideo_before < ideo_after,
         no_change = ideo_before == ideo_after,
         direction = if_else(leftward, -1, if_else(rightward, 1, 0)))

# TODO: look at three years' data in slimmed, to check for consistent change
#   mutate slimmed to add
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
# TODO: try out controls for religiosity, education, race, marital status, which are now in slimmed

# I think this is relevant, although it is not quite significant
chisq.test(table(trends$new_child, trends$direction)) # p = 0.05793

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