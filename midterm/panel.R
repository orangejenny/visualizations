library(haven)

setwd("~/Documents/visualizations/midterm")
panel <- read_dta("CCES_Panel_Full3waves_VV_V4.dta") # n=9500

# Drop most columns
# TODO: add legislation
slimmed <- panel %>% zap_labels() %>% 
  select(
    starts_with("birthyr_"),
    starts_with("ideo5_"),
    starts_with("gender_"),
    starts_with("child18_"),
    starts_with("child18num_"),
    starts_with("faminc_"),
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
         ideo_before = if_else(cycle == 1012, ideo5_10, ideo5_12),
         ideo_after = if_else(cycle == 1012, ideo5_12, ideo5_14),
         ideo_delta = ideo_after - ideo_before,
         income_before = if_else(cycle == 1012, faminc_10, faminc_12),
         income_after = if_else(cycle == 1012, faminc_12, faminc_14),
         income_bracket = if_else(income_after %in% seq(1,9),
                                  "low",
                                  if_else(income_after %in% seq(10, 18),
                                          "high",
                                          "unknown"))) %>% 
  select(-c(gender_10, gender_12, gender_14,
            ideo5_10, ideo5_12, ideo5_14,
            child18_10, child18_12, child18_14,
            child18num_10, child18num_12, child18num_14,
            faminc_10, faminc_12, faminc_14)) %>% 
  filter(!is.na(new_child)) # only 2 rows, based on all_data %>% group_by(new_child) %>% summarise(count = n())

trends <- all_data %>% 
  filter(ideo_before < 6, ideo_after < 6) %>% 
  mutate(leftward = ideo_before > ideo_after,
         rightward = ideo_before < ideo_after,
         no_change = ideo_before == ideo_after,
         direction = if_else(leftward, -1, if_else(rightward, 1, 0)))

# 17609, 602
trends %>% group_by(new_child) %>% summarise(count = n())

# TODO: try out controls for religiosity, education, race, marital status
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
get_regression_table(lm(ideo_delta ~ as_factor(new_child) + coalesce(income_before, income_after), data=trends))

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