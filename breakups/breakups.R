### Functions ###

# Filter out N/A and unknown reasons
is_reason_value <- function (value) {
  return(value %in% seq(1, 13) | value == 99)
}

# Calculate reason's "score" for an individual.
# reasons: all reasons for the individual
# value: reason value (1-13) that we're getting the score for
# Returns decimal between 0 and 1
reason_score <- function (reasons, value) {
  index <- 0
  index_sum <- 0
  found_index <- 0
  for (reason in reasons) {
    if (is_reason_value(reason)) {
      index <- index + 1
      index_sum <- index_sum + index
      if (reason == value) {
        found_index <- index
      }
    }
  }
  if (found_index == 0) {
    return(0)
  }
  return((index - found_index + 1) / index_sum)
}

# Calculate per-reason statistics on a data frame that has had
# `as_survey_design` called on it. Surely there's a DRYer way to do this,
# but R doesn't seem to support dynamic creation of column names.
summarise_survey <- function (df) {
  return(
    df %>%
      summarise(
        # Counts of number of breakups citing each specific reason
        adultery_count = round(survey_total(adultery_present, vartype="ci"), 1),
        money_count = round(survey_total(money_present, vartype="ci"), 1),
        sex_count = round(survey_total(sex_present, vartype="ci"), 1),
        different_count = round(survey_total(different_present, vartype="ci"), 1),
        grewapart_count = round(survey_total(grewapart_present, vartype="ci"), 1),
        nokids_count = round(survey_total(nokids_present, vartype="ci"), 1),
        norespect_count = round(survey_total(norespect_present, vartype="ci"), 1),
        violence_count = round(survey_total(violence_present, vartype="ci"), 1),
        arguments_count = round(survey_total(arguments_present, vartype="ci"), 1),
        housework_count = round(survey_total(housework_present, vartype="ci"), 1),
        circumstance_count = round(survey_total(circumstance_present, vartype="ci"), 1),
        death_count = round(survey_total(death_present, vartype="ci"), 1),
        other_count = round(survey_total(other_present, vartype="ci"), 1),
        unanswered_count = round(survey_total(unanswered_present, vartype="ci"), 1),
        
        # Proportion of total breakups citing each specific reason
        adultery_prop = round(survey_total(adultery_present, vartype="ci") / survey_total(vartype="ci"), 4),
        money_prop = round(survey_total(money_present, vartype="ci") / survey_total(vartype="ci"), 4),
        sex_prop = round(survey_total(sex_present, vartype="ci") / survey_total(vartype="ci"), 4),
        different_prop = round(survey_total(different_present, vartype="ci") / survey_total(vartype="ci"), 4),
        grewapart_prop = round(survey_total(grewapart_present, vartype="ci") / survey_total(vartype="ci"), 4),
        nokids_prop = round(survey_total(nokids_present, vartype="ci") / survey_total(vartype="ci"), 4),
        norespect_prop = round(survey_total(norespect_present, vartype="ci") / survey_total(vartype="ci"), 4),
        violence_prop = round(survey_total(violence_present, vartype="ci") / survey_total(vartype="ci"), 4),
        arguments_prop = round(survey_total(arguments_present, vartype="ci") / survey_total(vartype="ci"), 4),
        housework_prop = round(survey_total(housework_present, vartype="ci") / survey_total(vartype="ci"), 4),
        circumstance_prop = round(survey_total(circumstance_present, vartype="ci") / survey_total(vartype="ci"), 4),
        death_prop = round(survey_total(death_present, vartype="ci") / survey_total(vartype="ci"), 4),
        other_prop = round(survey_total(other_present, vartype="ci") / survey_total(vartype="ci"), 4),
        unanswered_prop = round(survey_total(unanswered_present, vartype="ci") / survey_total(vartype="ci"), 4),
        
        adultery_mean = round(survey_mean(adultery_score, vartype="ci"), 4),
        money_mean = round(survey_mean(money_score, vartype="ci"), 4),
        sex_mean = round(survey_mean(sex_score, vartype="ci"), 4),
        different_mean = round(survey_mean(different_score, vartype="ci"), 4),
        grewapart_mean = round(survey_mean(grewapart_score, vartype="ci"), 4),
        nokids_mean = round(survey_mean(nokids_score, vartype="ci"), 4),
        norespect_mean = round(survey_mean(norespect_score, vartype="ci"), 4),
        violence_mean = round(survey_mean(violence_score, vartype="ci"), 4),
        arguments_mean = round(survey_mean(arguments_score, vartype="ci"), 4),
        housework_mean = round(survey_mean(housework_score, vartype="ci"), 4),
        circumstance_mean = round(survey_mean(circumstance_score, vartype="ci"), 4),
        death_mean = round(survey_mean(death_score, vartype="ci"), 4),
        other_mean = round(survey_mean(other_score, vartype="ci"), 4),
        unanswered_mean = round(survey_mean(unanswered_score, vartype="ci"), 4),
      )
  )
}

# Reformat data to put it in a dot plot with confidence intervals
# Expects data frame to contain _low and _upp columns as produced by survey_total with vartype = "ci"
dot_plot_data <- function (df, suffix) {
  low_suffix <- str_replace(suffix, "$", "_low")
  upp_suffix <- str_replace(suffix, "$", "_upp")
  return(cbind(
    # Pivot the three sets of columns (estimate, lower bound, upper bound),
    # smush them together, and remove the duplicate columns
    pivot_longer(df %>% select(is_mono, ends_with(suffix)),
                 ends_with(suffix)),
    pivot_longer(df %>% select(is_mono, ends_with(upp_suffix)),
                 ends_with(upp_suffix)),
    pivot_longer(df %>% select(is_mono, ends_with(low_suffix)),
                 ends_with(low_suffix))
    ) %>% 
    rename(is_mono = 1, reason = 2, stat = 3,
           drop_is_mono = 4, drop_reason = 5, upp = 6,
           drop_is_mono_again = 7, drop_reason_again = 8, low = 9) %>% 
    select(!starts_with('drop_')) %>% 
    mutate(reason = str_remove(reason, suffix)) %>% 
    mutate(is_mono_text = recode(is_mono, `0` = "Non-monogamous", `1` = "Monogamous")))
}
 
# Draw dot plot with confidence intervals, using data produced by dot_plot_data
dot_plot <- function(data) {
  ggplot(data,
         aes(x = reason, y = stat,
             ymin = low, ymax = upp,
             color = as_factor(is_mono_text))) +
    geom_pointrange(alpha = 0.8, position = position_dodge(width = 0.5)) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank())
}


### Script ###

library(haven)
library(tidyverse)
library(survey)
library(srvyr)
library(Hmisc)  # %nin%
library(moderndive)  # get_regression_table
library(modelsummary)

# Load data
setwd("~/Dropbox/SOCIOL 651/Final - NATSAL")
raw_2000 <- read_dta("UKDA-5223-stata11/stata11/natsal_2000_for_archive.dta")

options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")
options(scipen = 100)  # no scientific notation

# Reduce size of dataset
breakups <- raw_2000 %>% 
  select(
    sserial:dateyoi, # survey design variables
    dage, agrp, agrp2, # age, grouped age
    rsex, # respondent sex
    rsc, rsc_gp2, # social class, grouped social class
    ppms, ppmar, # previous relationship marital status
    totalpsu, total_wt,
    ppwhy01:ppwhy11,  # reasons why most recent live-in partnership ended
    idealnow, ideal5yr, # ideal relationship styles
  ) %>%
  mutate(pp_is_marriage = if_else(ppms == 1 | ppmar == 1, 1, 0)) %>% # is previous partnership a marriage?
  filter(is_reason_value(ppwhy01))  # filter to respondents who had a breakup

# Add frequencies and scores of breakup reasons
breakup_reasons = subset(breakups, select=ppwhy01:ppwhy11)
breakups <- breakups %>% 
  mutate(
    adultery_score = apply(breakup_reasons, 1, reason_score, 1),
    money_score = apply(breakup_reasons, 1, reason_score, 2),
    sex_score = apply(breakup_reasons, 1, reason_score, 3),
    different_score = apply(breakup_reasons, 1, reason_score, 4),
    grewapart_score = apply(breakup_reasons, 1, reason_score, 5),
    nokids_score = apply(breakup_reasons, 1, reason_score, 6),
    norespect_score = apply(breakup_reasons, 1, reason_score, 7),
    violence_score = apply(breakup_reasons, 1, reason_score, 8),
    arguments_score = apply(breakup_reasons, 1, reason_score, 9),
    housework_score = apply(breakup_reasons, 1, reason_score, 10),
    circumstance_score = apply(breakup_reasons, 1, reason_score, 11),
    death_score = apply(breakup_reasons, 1, reason_score, 12),
    other_score = apply(breakup_reasons, 1, reason_score, 13),
    unanswered_score = apply(breakup_reasons, 1, reason_score, 99),
  ) %>% 
  mutate(
    adultery_present = if_else(adultery_score > 0, 1, 0),
    money_present = if_else(money_score > 0, 1, 0),
    sex_present = if_else(sex_score > 0, 1, 0),
    different_present = if_else(different_score > 0, 1, 0),
    grewapart_present = if_else(grewapart_score > 0, 1, 0),
    nokids_present = if_else(nokids_score > 0, 1, 0),
    norespect_present = if_else(norespect_score > 0, 1, 0),
    violence_present = if_else(violence_score > 0, 1, 0),
    arguments_present = if_else(arguments_score > 0, 1, 0),
    housework_present = if_else(housework_score > 0, 1, 0),
    circumstance_present = if_else(circumstance_score > 0, 1, 0),
    death_present = if_else(death_score > 0, 1, 0),
    other_present = if_else(other_score > 0, 1, 0),
    unanswered_present = if_else(unanswered_score > 0, 1, 0),
  )

# Add mono/non-mono categorization and filter out "other" respondents
breakups <- breakups %>% 
  mutate(
    is_mono = recode(unclass(idealnow),  # unclass because this is a haven labelled column
                     `1` = -1, # no partners
                     `2` = -1, # casual partners
                     `3` = -1, # multiple regular partners
                     `4` = -1, # one regular partner
                     `5` = 0,  # cohabiting with outside sex
                     `6` = 1,  # cohabiting, no ouside sex
                     `7` = 0,  # married, with outside sex
                     `8` = 1,  # married, no outside sex
                     `9` = -1  # none/unknown
                     ),
  ) %>% 
  filter(is_mono %in% c(0, 1))

# Create survey data frame, using params specified in Natsal-2 technical report docs
breakups_as_survey <- breakups %>% 
  drop_na(psu) %>%
  as_survey_design(ids = totalpsu,  # because this is core + ethnic boost sample
                   strata = strata,
                   weights = total_wt,  # because this is core + ethnic boost sample
                   nest = TRUE)
 
# Overall counts: 1728 mono, 134 non-mono: 92% mono
weighted_counts <- breakups_as_survey %>% 
  group_by(is_mono) %>% 
  summarise(count = survey_total())

weighted_summary <- summarise_survey(breakups_as_survey %>% group_by(is_mono))

# Look at sample size for each reason
sample_sizes <- pivot_longer(weighted_summary %>% select(is_mono, ends_with("_count")),
                             ends_with("_count"))

# Drop uncommon reasons (count < 10 for non-mono respondents)
weighted_summary <- weighted_summary %>% 
  select(!starts_with('unanswered_')) %>% 
  select(!starts_with('other_')) %>% 
  select(!starts_with('nokids_')) %>% 
  select(!starts_with('death_')) %>% 
  select(!starts_with('circumstance_'))

# Visualize reason frequency & mean score, with confidence intervals
dot_plot(dot_plot_data(weighted_summary, "_prop")) +
  scale_y_continuous(breaks = seq(0, 0.5, by=0.1), labels = seq(0, 50, by=10)) +
  labs(title="Reason Frequency", x = "", y = "% of breakups citing reason")

dot_plot(dot_plot_data(weighted_summary, "_mean")) +
  scale_y_continuous(breaks = seq(0, 0.3, by=0.1), labels = seq(0, 0.3, by=0.1)) +
  labs(title="Reason Score", x = "", y = "Mean reason score")

# Chi square test for presence of arguments, which is the only reason
# that looks significant on the dot plot
svychisq(~is_mono+arguments_present, breakups_as_survey)  # p-value 0.001537

# Modeling for adultery score as a theoretically related to monogamy/non-monogamy difference
get_regression_table(lm(adultery_score ~ as_factor(is_mono), data=breakups_as_survey))

# Try controlling for various combinations of:
#   pp_is_marriage: Whether the relationship was a marriage or not
#   rsc_gp2: social class (large categories, like managerial/professional, unskilled labor, etc.)
#   dage: age in years (z score)
#   rsex: respondent sex
modelsummary(list(
  lm(adultery_score ~ as_factor(is_mono), data=breakups_as_survey),
  lm(adultery_score ~ as_factor(is_mono) + as_factor(pp_is_marriage), data=breakups_as_survey),
  lm(adultery_score ~ as_factor(is_mono) + as_factor(pp_is_marriage) + as_factor(rsc_gp2), data=breakups_as_survey),
  lm(adultery_score ~ as_factor(is_mono) + as_factor(pp_is_marriage) + as_factor(rsc_gp2) + as_factor(rsex) + scale(dage), data=breakups_as_survey),
  lm(adultery_score ~ as_factor(is_mono) + as_factor(rsc_gp2) + as_factor(rsex) + scale(dage), data=breakups_as_survey)
), stars = TRUE)

modelsummary(list(
  lm(adultery_score ~ as_factor(is_mono), data=breakups_as_survey),
  lm(adultery_score ~ as_factor(pp_is_marriage), data=breakups_as_survey)
), stars = TRUE)
