library(ggplot2)

setwd('~/Documents/visualizations/CES/python')
palette <- c("#0072B2", "#B3257F", "#527200")

### Covariate visualization
covariate_data <- tibble(
  # Values are (pool, matched_set)
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

covariate_data <- read.csv('output/viz/covariates_matching_firstborn.csv')

# Dot plot
ggplot(covariate_data,
       aes(x = as_factor(demographic), y = value,
           color = as_factor(is_matched))) +
  geom_point(alpha = 0.5, size=3) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_colour_manual(values=palette, guide = guide_legend(title = "")) +
  labs(x = "", y = "", color="group", title="Matching: Covariate comparison") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

### Panel visualization
panel_data <- tibble(
  issue = c("abortion", "aff_action", "gay_rights"),
  control_before = c(2.1, 3.2, 5.7),
  control_after = c(4.7, 6.4, 5.6),
  treatment_before = c(1.3, 5.1, 5.5),
  treatment_after = c(3.1, 1.0, 4.4)
)

panel_data <- read.csv('output/viz/panel_firstborn.csv')

# Line range
ggplot(panel_data) +
  geom_segment(aes(
    x=issue,
    xend=issue,
    y=treatment_before,
    yend=treatment_after,
    color='parents',
  ), arrow = arrow(length = unit(0.1, "inches")), size=1, lineend = 'round', linejoin = 'round', alpha = 0.6) +
  geom_segment(aes(
    x=issue,
    xend=issue,
    y=control_before,
    yend=control_after,
    color='non-parents',
  ), arrow = arrow(length = unit(0.1, "inches")), size=1, lineend = 'round', linejoin = 'round', alpha = 0.6) +
  scale_colour_manual(values=palette, guide = guide_legend(title = "")) +
  scale_y_continuous(limits=c(0, 10), breaks=c(0,10), labels=c('liberal', 'conservative')) +
  labs(x = "", y = "", color="group", title="Panel: Comparing attitude changes, 2010-2012") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))


### Matching visualization
# TODO: Consider switching to linerange and adding matches for different treatments, gender, and income
matching_data <- tibble(
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

matching_data <- read.csv('output/viz/matching_is_parent.csv')

# Dot plot
ggplot(matching_data,
       aes(x = as_factor(issue), y = value,
           color = as_factor(is_matched))) +
  geom_point(alpha = 0.6, size=5) +
  scale_y_continuous(limits=c(0, 10), breaks=c(0,10), labels=c('liberal', 'conservative')) +
  scale_colour_manual(values=palette, guide = guide_legend(title = "")) +
  labs(x = "", y = "", color="group", title="Matching: Comparing 2012 attitudes of parents and non-parents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
