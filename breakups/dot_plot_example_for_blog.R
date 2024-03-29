# Set up sample data
static_data <- tibble(
  x = c("Alpha", "Alpha", "Alpha",
        "Beta", "Beta", "Beta",
        "Gamma", "Gamma", "Gamma",
        "Delta", "Delta", "Delta",
        "Epsilon", "Epsilon", "Epsilon"),
  y = c(1, 3, 2, 1, 3, 4, 2, 6, 5, 6, 4, 7, 8, 9, 5),
  z = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
  upp = c(1.2, 3.3, 2.1, 1.25, 3.3, 4.2, 2.2, 6.4, 5.3, 6.57, 4.15, 7.28, 8.6, 9.8, 5.2),
  low = c(0.65, 2.1, 1.35, 0.58, 2.47, 3.15, 1.5, 5.34, 4, 4.8, 1.9, 6.2, 7, 8.19, 4.55)
)
palette <- c("#009E73", "#0072B2", "#F0E442")

# Histogram
ggplot(static_data,
       aes(x = as_factor(x), y = y,
             fill = as_factor(z))) +
  geom_col(alpha = 0.8, position = position_dodge(), show.legend = FALSE) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.3, position = position_dodge(0.9)) +
  scale_fill_manual(values=palette) +
  labs(x = "", y = "", title="Too Much Happening") +
  theme_minimal()

# Dot plot
ggplot(static_data,
       aes(x = as_factor(x), y = y,
           ymin = low, ymax = upp,
           color = as_factor(z))) +
  geom_pointrange(alpha = 0.8, position = position_dodge(width = 0.5), show.legend = FALSE) +
  scale_colour_manual(values=palette) +
  labs(x = "", y = "", title="Light and Breezy") +
  theme_minimal()
