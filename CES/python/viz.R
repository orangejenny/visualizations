library(ggplot2)

# Set up sample data
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
palette <- c("#009E73", "#0072B2", "#F0E442")

# Dot plot
ggplot(covariate_data,
       aes(x = as_factor(demographic), y = value,
           color = as_factor(is_matched))) +
  geom_point(alpha = 0.8, size=5) +
  scale_y_continuous(limits=c(0, 10)) +
  scale_colour_manual(values=palette) +
  labs(x = "", y = "", color="group", title="Covariate comparison of entire pool and matched set") +
  theme_minimal()
