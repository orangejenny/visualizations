library(ipumsr)
library(tidyverse) 

# Get data
setwd("~/Dropbox/SOCIOL 651/") 
ddi <- read_ipums_ddi("atus_00016.xml")
all_data <- read_ipums_micro(ddi)

# Flag people who do no housework and add columns to make weighting easier
all_data <- all_data %>% 
  mutate(CASEID = as.character(CASEID)) %>% 
  mutate(PERSON_COUNT = ifelse(YEAR == 2020, WT20, WT06) / 91) %>% # 91 days in a quarter
  mutate(NO_HOUSEWORK = ifelse(ACT_HHACT == 0, 1, 0)) %>% 
  mutate(NO_HOUSEWORK_PERSON_COUNT = NO_HOUSEWORK * PERSON_COUNT)

# Plot 1: Density histogram of time spent on housework
histogram_data <- all_data %>%
  filter(YEAR == 2021) %>% 
  mutate(SEX_LABEL = ifelse(SEX == 1, "Male", "Female")) %>% 
  filter(ACT_HHACT < 12 * 60)  # almost no one is >12 hours, and 12 hours makes a pretty graph

ggplot(histogram_data, aes(x = ACT_HHACT, y = ..density.., weight = PERSON_COUNT)) +
  geom_histogram(fill = "steelblue", binwidth = 10) +
  facet_wrap(~ SEX_LABEL) +
  scale_x_continuous(breaks = c(0, 60, 12 * 60),
                     labels = c(0, 1, 12),
                     minor_breaks = c()) +
  scale_y_continuous(breaks = seq()) +
  theme_minimal() +
  labs(x = "Time spent on housework, hours per day",
       y = "",
       title = "Opting out of housework, 2021")

# Percentage of opt-outers in population, by sex
histogram_data %>% 
  group_by(SEX) %>% 
  summarise(PERCENT = sum(NO_HOUSEWORK_PERSON_COUNT) * 100 / sum(PERSON_COUNT))

# Plot 2: Household size and gender
hh_data <- all_data %>% 
  filter(YEAR == 2019) %>% # 2019 is the most dramatic
  filter(HH_NUMADULTS > 0, HH_NUMADULTS < 6)  # exclude values with little data

hh_size <- hh_data %>% 
  group_by(HH_NUMADULTS) %>% 
  summarise(PERCENT = sum(NO_HOUSEWORK_PERSON_COUNT) * 100 / sum(PERSON_COUNT),
            SEX = "Total")
 
hh_size_with_sex <- hh_data %>% 
  group_by(HH_NUMADULTS, SEX) %>% 
  summarise(PERCENT = sum(NO_HOUSEWORK_PERSON_COUNT) * 100 / sum(PERSON_COUNT))

total_color <- "#1f78b4"
colors <- c("#a6cee3", "#b2df8a", total_color) # qualitative scale from colorbrewer2.org
ggplot(hh_size_with_sex,
       aes(x = as_factor(HH_NUMADULTS), y = PERCENT, fill = as_factor(SEX))) +
  geom_col(position="dodge") +
  geom_line(data = hh_size,
            mapping = aes(x = as_factor(HH_NUMADULTS), y = PERCENT, group = 1),
            color = total_color) +
  theme_minimal() +
  scale_fill_manual(values = colors, guide = guide_legend(title = "")) +
  labs(x = "Number of adults in household",
       y = "Percentage of people opting out",
       title = "Household size of opt-outers, 2019")

# Plot 3: Time trends, including gender and cohabitation
by_year <- all_data %>% 
  group_by(YEAR, SEX) %>% 
  summarise(PERCENT = sum(NO_HOUSEWORK_PERSON_COUNT) * 100 / sum(PERSON_COUNT))

by_year_and_partner <- all_data %>% 
  filter(SPOUSEPRES != 99) %>%  # "not in universe"
  mutate(PARTNER = ifelse(SPOUSEPRES %in% c(1, 2), 1, 0)) %>%  # 1  spouse, 2 = unmarried partner
  mutate(KEY = paste(ifelse(SEX == 1, "Male", "Female"), ifelse(PARTNER == 1, "living with partner", "single"), sep = ", ")) %>% 
  group_by(YEAR, SEX, PARTNER, KEY) %>% 
  summarise(PERCENT = sum(NO_HOUSEWORK_PERSON_COUNT) * 100 / sum(PERSON_COUNT))

series_label <- function(y, text) {
  return(annotate(geom = "text", x = 2021.2, y = y, label=text, hjust = 0, size = 3))
}
x_labels <- c(2005, 2010, 2015, 2020)
ggplot(by_year, aes(x = YEAR, y = PERCENT, linetype = as_factor(SEX))) +
  geom_line(show.legend = FALSE) + 
  geom_line(data = by_year_and_partner,
            mapping = aes(x = YEAR, y = PERCENT, color = KEY),
            show.legend = FALSE) +
  # Fake "legend" using annotations
  series_label(31, "Male, single") +
  series_label(28.5, "Male") +
  series_label(26, "Male, living with partner") +
  series_label(19, "Female, single") +
  series_label(13.5, "Female") +
  series_label(9, "Female, living with partner") +
  scale_x_continuous(limits = c(2003, 2027), # weird extension to fit annotations
                     labels = x_labels,
                     breaks = x_labels,
                     minor_breaks = seq(2003, 2021)) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_linetype_manual(values = c("twodash", "dashed")) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  labs(x = "Year",
       y = "Percentage of people opting out",
       title = "Trends in opting out, by gender and cohabitation status, 2013-2021")
