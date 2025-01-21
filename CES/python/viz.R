library(tidyverse)
library(ggplot2)

setwd('~/Documents/visualizations/CES/python')
palette <- c("#0072B2", "#B3257F", "#527200")

### Sample data for all plots
sample_covariate_data <- tibble(
  demographic = c(
    "age", "age", "age",
    "gender_1", "gender_1", "gender_1",
    "gender_2", "gender_2", "gender_2",
    "income", "income", "income",
    "pew_churatd", "pew_churatd", "pew_churatd"
  ),
  value = c(1, 3, 2, 1, 3, 4, 2, 6, 5, 6, 1, 2, 3, 4, 5),
  is_matched = c(
    "treatment", "pool", "matched",
    "treatment", "pool", "matched",
    "treatment", "pool", "matched",
    "treatment", "pool", "matched",
    "treatment", "pool", "matched"
  )
)

sample_panel_data <- tibble(
  issue = c("abortion", "aff_action", "gay_rights"),
  control_before = c(2.1, 3.2, 5.7),
  control_after = c(4.7, 6.4, 5.6),
  treatment_before = c(1.3, 5.1, 5.5),
  treatment_after = c(3.1, 1.0, 4.4)
)

sample_matching_data <- tibble(
  issue = c(
    "abortion", "abortion",
    "aff_action", "aff_action",
    "budget", "budget",
    "climate", "climate"
  ),
  value = c(
    1.2, 3.4,
    4.5, 4.7,
    2.4, 3.1,
    5.1, 6.5
  ),
  is_matched = c(
    "control", "treatment",
    "control", "treatment",
    "control", "treatment",
    "control", "treatment"
  )
)

### Covariate visualization
covariate_dot_plot <- function(treatment) {
  covariate_data <- read.csv(sprintf("output/viz/covariates_matching_%s.csv", treatment))
  titles = c(
    "firstborn" = "Matching covariates, first-time parents",
    "is_parent" = "Matching covariates, all parents"
  )
  ggplot(covariate_data,
         aes(x = as_factor(demographic), y = value,
             color = as_factor(is_matched))) +
    geom_point(alpha = 0.8, size=3) +
    scale_x_discrete(limits=levels(as_factor(covariate_data$demographic))) +
    scale_y_continuous(limits=c(0, 100)) +
    scale_colour_manual(values=palette, guide = guide_legend(title = "")) +
    labs(x = "", y = "", color="group", title=titles[treatment]) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust=1)) +
    theme(legend.position = "bottom")
}
covariate_dot_plot("firstborn")
covariate_dot_plot("is_parent")

### Panel visualization
panel_arrows = function(treatment) {
  panel_data <- read.csv(sprintf("output/viz/panel_%s.csv", treatment))

  # Add index to use for issue, so it can be adjusted slightly to separate the arrows visually
  panel_data <- panel_data %>% mutate(index = nrow(panel_data):1)
  dodge <- 0.2
  issue_labels <- rev(panel_data$issue)

  titles = c(
    "firstborn" = "Panel: Attitude changes, 2010-2012, first-time parents",
    "new_child" = "Panel: Attitude changes, 2010-2012, parents with a recent birth"
  )
  num_issues = nrow(panel_data)
  ggplot(panel_data) +
    geom_segment(aes(
      x=index + dodge,
      xend=index + dodge,
      y=treatment_before,
      yend=treatment_after,
      color=' parents',  # space to force first alphabetically, to get blue color to match parents in matching viz
    ), arrow = arrow(length = unit(0.1, "inches")), size=1, lineend = 'round', linejoin = 'round', alpha = 1) +
    geom_segment(aes(
      x=index - dodge,
      xend=index - dodge,
      y=control_before,
      yend=control_after,
      color='non-parents',
    ), arrow = arrow(length = unit(0.1, "inches")), size=1, lineend = 'round', linejoin = 'round', alpha = 1) +
    scale_colour_manual(values=palette, guide = guide_legend(title = "")) +
    scale_x_continuous(limits=c(0, num_issues + 1), breaks=1:num_issues, minor_breaks=c(), labels=issue_labels) +
    scale_y_continuous(limits=c(0, 10), breaks=c(0,10), labels=c('liberal', 'conservative')) +
    labs(x = "", y = "", color="group", title=titles[treatment]) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust=1)) +
    theme(legend.position = "bottom")
}
panel_arrows("firstborn")
panel_arrows("new_child")

### Matching visualization
matching_dot_plot <- function(treatment) {
  matching_data <- read.csv(sprintf("output/viz/matching_%s.csv", treatment))
  titles = c(
    "firstborn" = "Matching: 2012 attitudes, first-time parents",
    "is_parent" = "Matching: 2012 attitudes, all parents"
  )
  ggplot(matching_data,
         aes(x = as_factor(issue), y = value,
             color = as_factor(is_matched), shape = as_factor(is_matched))) +
    geom_point(alpha = 0.8, size=5) +
    scale_x_discrete(limits=rev(levels(as_factor(matching_data$issue)))) +
    scale_y_continuous(limits=c(0, 10), breaks=c(0,10), labels=c('liberal', 'conservative')) +
    scale_colour_manual(values=palette, guide = guide_legend(title = "")) +
    labs(x = "", y = "", color="", shape = "", title=titles[treatment]) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust=1)) +
    theme(legend.position = "bottom")
}
matching_dot_plot("firstborn")
matching_dot_plot("is_parent")
