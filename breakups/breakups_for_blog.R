### Functions ###

# Filter out N/A and unknown reasons
is_reason_value <- function (value) {
  return(value %in% seq(1, 13) | value == 99)
}

# Calculate reason's "score" for an individual.
# reasons: all reasons for the individual
# value: reason value (1-13) that we're getting the score for
# Returns decimal between 0 and 1, or NA if reason is unused
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
    return(NA)
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
        # Proportion of total breakups mentioning each specific reason
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
        
        adultery_mean = round(survey_mean(adultery_score, vartype="ci", na.rm = TRUE), 4),
        money_mean = round(survey_mean(money_score, vartype="ci", na.rm = TRUE), 4),
        sex_mean = round(survey_mean(sex_score, vartype="ci", na.rm = TRUE), 4),
        different_mean = round(survey_mean(different_score, vartype="ci", na.rm = TRUE), 4),
        grewapart_mean = round(survey_mean(grewapart_score, vartype="ci", na.rm = TRUE), 4),
        nokids_mean = round(survey_mean(nokids_score, vartype="ci", na.rm = TRUE), 4),
        norespect_mean = round(survey_mean(norespect_score, vartype="ci", na.rm = TRUE), 4),
        violence_mean = round(survey_mean(violence_score, vartype="ci", na.rm = TRUE), 4),
        arguments_mean = round(survey_mean(arguments_score, vartype="ci", na.rm = TRUE), 4),
        housework_mean = round(survey_mean(housework_score, vartype="ci", na.rm = TRUE), 4),
        circumstance_mean = round(survey_mean(circumstance_score, vartype="ci", na.rm = TRUE), 4),
        death_mean = round(survey_mean(death_score, vartype="ci", na.rm = TRUE), 4),
        other_mean = round(survey_mean(other_score, vartype="ci", na.rm = TRUE), 4),
        unanswered_mean = round(survey_mean(unanswered_score, vartype="ci", na.rm = TRUE), 4),
      )
  )
}

# Reformat data to put it in a dot plot with confidence intervals
# Expects data frame to contain _low and _upp columns as produced by survey_total with vartype = "ci"
dot_plot_data <- function (df) {
  return(cbind(
    # Pivot the three sets of columns (estimate, lower bound, upper bound),
    # smush them together, and remove the duplicate columns
    pivot_longer(df %>% select(ends_with("_prop"), ends_with("_mean")),
                 c(ends_with("_prop"), ends_with("_mean"))),
    pivot_longer(df %>% select(ends_with("_upp")),
                 ends_with("_upp")),
    pivot_longer(df %>% select(ends_with("_low")),
                 ends_with("_low"))
  ) %>% 
    rename(reason = 1, stat = 2,
           drop_reason = 3, upp = 4,
           drop_reason_again = 5, low = 6) %>% 
    select(!starts_with('drop_')) %>% 
    mutate(stat_name = str_remove(reason, ".*_")) %>% 
    mutate(stat_name = if_else(stat_name == "prop", "% of breakups mentioning reason", "average % of blame given to reason")) %>% 
    mutate(reason = str_remove(reason, "_.*")))
}
 
# Draw dot plot with confidence intervals, using data produced by dot_plot_data
dot_plot <- function(data) {
  palette <- c("#0072B2", "#F0E442")
  ggplot(data,
         aes(x = reason, y = stat,
             ymin = low, ymax = upp,
             color = as_factor(stat_name))) +
    geom_pointrange(alpha = 0.8, position = position_dodge(width = 0.5)) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank()) +
    scale_y_continuous(breaks = seq(0, 0.6, by=0.1), labels=seq(0, 60, by=10)) +
    scale_colour_manual(values=palette) +
    labs(x = "", y = "")
}


### Script ###

library(haven)
library(tidyverse)
library(survey)
library(srvyr)
library(Hmisc)  # %nin%

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
    totalpsu, total_wt,
    ppwhy01:ppwhy11,  # reasons why most recent live-in partnership ended
  ) %>%
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
    adultery_present = if_else(coalesce(adultery_score, 0) > 0, 1, 0),
    money_present = if_else(coalesce(money_score, 0) > 0, 1, 0),
    sex_present = if_else(coalesce(sex_score, 0) > 0, 1, 0),
    different_present = if_else(coalesce(different_score, 0) > 0, 1, 0),
    grewapart_present = if_else(coalesce(grewapart_score, 0) > 0, 1, 0),
    nokids_present = if_else(coalesce(nokids_score, 0) > 0, 1, 0),
    norespect_present = if_else(coalesce(norespect_score, 0) > 0, 1, 0),
    violence_present = if_else(coalesce(violence_score, 0) > 0, 1, 0),
    arguments_present = if_else(coalesce(arguments_score, 0) > 0, 1, 0),
    housework_present = if_else(coalesce(housework_score, 0) > 0, 1, 0),
    circumstance_present = if_else(coalesce(circumstance_score, 0) > 0, 1, 0),
    death_present = if_else(coalesce(death_score, 0) > 0, 1, 0),
    other_present = if_else(coalesce(other_score, 0) > 0, 1, 0),
    unanswered_present = if_else(coalesce(unanswered_score, 0) > 0, 1, 0),
  )

# Create survey data frame, using params specified in Natsal-2 technical report docs
breakups_as_survey <- breakups %>% 
  drop_na(psu) %>%
  as_survey_design(ids = totalpsu,  # because this is core + ethnic boost sample
                   strata = strata,
                   weights = total_wt,  # because this is core + ethnic boost sample
                   nest = TRUE)
 
weighted_summary <- summarise_survey(breakups_as_survey)

# Look at sample size for each reason
sample_sizes <- pivot_longer(weighted_summary %>% select(ends_with("_count")),
                             ends_with("_count"))

# For visuals, drop death, other, unanswered
weighted_summary <- weighted_summary %>% 
  select(!starts_with('other_')) %>% 
  select(!starts_with('unanswered_')) %>% 
  select(!starts_with('death_'))

# Visualize reason frequency & mean score, with confidence intervals
plot_data <- dot_plot_data(weighted_summary)
dot_plot(dot_plot_data(weighted_summary))
