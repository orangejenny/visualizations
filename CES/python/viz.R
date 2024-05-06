library(ggplot2)

setwd('~/Documents/visualizations/CES/python')

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
palette <- c("#0072B2", "#F0E442", "#009E73")

# Dot plot
ggplot(covariate_data,
       aes(x = as_factor(demographic), y = value,
           color = as_factor(is_matched))) +
  geom_point(alpha = 0.8, size=5) +
  scale_y_continuous(limits=c(0, 10)) +
  scale_colour_manual(values=palette) +
  labs(x = "", y = "", color="group", title="Covariate comparison of entire pool and matched set") +
  theme_minimal()


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
    y=control_before,
    yend=control_after,
    color='control',
    alpha = 0.7,
  ), arrow = arrow(), size=2, lineend = 'round', linejoin = 'round') +
  geom_segment(aes(
    x=issue,
    xend=issue,
    y=treatment_before,
    yend=treatment_after,
    color='treatment',
    alpha = 0.7,
  ), arrow = arrow(), size=2, lineend = 'round', linejoin = 'round') +
  scale_colour_manual(values=palette) +
  scale_y_continuous(limits=c(0, 10)) +
  theme_minimal()
  