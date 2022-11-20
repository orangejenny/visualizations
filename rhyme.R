library(tidyverse)
library(skimr)
library(moderndive)  # get_correlation
library(ggrepel)     # geom_text_repel

# TODO: rhyme API
songs <- read_csv("songs.csv")
tags <- read_csv("tags.csv")

skim(songs)

# Histograms of rating, mood, energy
ggplot(songs, aes(x = rating)) + geom_bar()
ggplot(songs, aes(x = mood)) + geom_bar()
ggplot(songs, aes(x = energy)) + geom_bar()

# 2d bins, somewhat like the bubble matrix
ggplot(songs, aes(x = energy, y = mood)) + geom_bin2d()

# Yearly trends in rating, mood, energy
years <- tags %>%
  filter(tag_category == "years") %>% 
  group_by(tag) %>% 
  summarise(rating = mean(rating, na.rm = TRUE),
            mood = mean(mood, na.rm = TRUE),
            energy = mean(energy, na.rm = TRUE))
ggplot(years, aes(x = tag, y = rating)) +
  geom_line(group = 1, color = "blue") +
  geom_line(aes(y = mood), group = 1, color = "purple") +
  geom_line(aes(y = energy), group = 1, color = "green") +
  scale_y_continuous(limits = c(1, 5)) +
  theme(axis.text.x = element_text(angle=270, hjust=1))

# Correlations between rating, mood, and energy
songs %>% 
  filter(!is.na(rating), !is.na(mood)) %>% 
  get_correlation(formula = rating ~ mood)
songs %>% 
  filter(!is.na(rating), !is.na(energy)) %>% 
  get_correlation(formula = rating ~ energy)
songs %>% 
  filter(!is.na(mood), !is.na(energy)) %>% 
  get_correlation(formula = mood ~ energy)

# These end up like matrix dot plots
ggplot(songs, aes(x = mood, y = rating)) + geom_jitter()
ggplot(songs, aes(x = energy, y = rating)) + geom_jitter()
ggplot(songs, aes(x = mood, y = energy)) + geom_jitter()

# Rating, energy, mood for people
people_tags <- tags %>%
  filter(tag_category == "people") %>% 
  filter(!is.na(rating), !is.na(mood), !is.na(energy)) %>% 
  group_by(tag) %>% 
  summarise(count = n(), rating = mean(rating), mood = mean(mood), energy = mean(energy)) %>% 
  filter(count >= 20)

ggplot(people_tags, aes(x = tag, y = rating)) + 
  #geom_col(fill = "blue", position="dodge") +
  geom_col(aes(y = mood), fill = "purple", position="dodge") +
  #geom_col(aes(y = energy), fill = "green", position="dodge") +
  theme(axis.text.x = element_text(angle=270, hjust=1))


# Summaries for tag categories
tags %>% select(tag_category) %>% group_by(tag_category) %>% summarize(count = n())
cat_summary <- function(cat_name) {
  return(
    tags %>% 
      filter(tag_category == cat_name) %>% 
      group_by(tag) %>% 
      summarise(count = n(),
                rating_mean = mean(rating, na.rm = TRUE),
                mood_mean = mean(mood, na.rm = TRUE),
                energy_mean = mean(energy, na.rm = TRUE),
                rating_sd = sd(rating, na.rm = TRUE),
                mood_sd = sd(mood, na.rm = TRUE),
                energy_sd = sd(energy, na.rm = TRUE))
  )
}
emotions_summary <- cat_summary("emotions")
people_summary <- cat_summary("people")
years_summary <- cat_summary("years")
seasons_summary <- cat_summary("seasons")
colors_summary <- cat_summary("colors")
activities_summary <- cat_summary("activities")
 
# Dot plots, with and without ranges
ggplot(subset(people_summary, count > 9), aes(x = reorder(tag, mood_mean), y = mood_mean)) +
  geom_point() +
  coord_flip() +
  scale_y_continuous(limits = c(1, 5), breaks = c(1, 2, 3, 4, 5), minor_breaks = NULL) +
  labs(x = NULL, y = "mood") +
  theme_minimal()

ggplot(subset(years_summary, count > 9), aes(x = tag, y = mood_mean)) +
  geom_pointrange(aes(ymin = mood_mean - mood_sd, ymax = mood_mean + mood_sd)) +
  scale_x_discrete(breaks = seq(2000, 2020, by=4)) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = NULL, y = "mood")

# Scatterplot of people (mood x energy)
ggplot(subset(people_summary, count > 9), aes(x = energy_mean, y = mood_mean)) +
  geom_point() +
  geom_text_repel(aes(label = tag)) +
  labs(x = "energy", y = "mood") +
  theme_minimal()