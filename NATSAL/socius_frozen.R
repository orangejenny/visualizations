# Macdowall, W., Nanchahal, K., Fenton, K., Copas, A., Carder, C., Senior, M.,
#     Wellings, K., Ridgway, G., Russell, M., National Centre for Social Research,
#     McCadden, A. 2005. National Survey of Sexual Attitudes and Lifestyles,
#     2000-2001. [data collection]. UK Data Service. SN: 5223,
#     DOI: 10.5255/UKDA-SN-5223-1

library(haven)
library(survey)
library(srvyr)
library(tidyverse)
library(networkD3)
library(ggalluvial)

## Setup
options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")
data_location <- "UKDA-5223-stata11/stata11/natsal_2000_for_archive.dta"

## Constants
plain_labels = c(
  "No partners",
  "Only casual partners",
  "Not cohabiting, non-monogamous",
  "Not cohabiting, monogamous",
  "Cohabiting, non-monogamous",
  "Cohabiting, monogamous",
  "Married, non-monogamous",
  "Married, monogamous",
  "Unknown"
)

alluvia_labels = c(
  " No partners ",
  "  Only casual partners ",
  "   Not cohabiting, non-monogamous   ",
  "    Not cohabiting,    \nmonogamous",
  "     Cohabiting, non-monogamous     ",
  "      Cohabiting,      \nmonogamous",
  "       Married, non-monogamous       ",
  "        Married,        \nmonogamous",
  "Unknown"
)

full_labels = c(
  "No partners (NP)",
  "Only casual\npartners (OCP)",
  "Not cohabiting,\nnon-monogamous (NCNM)",
  "Not cohabiting,\nmonogamous (NCM)",
  "Cohabiting,\nnon-monogamous (CNM)",
  "Cohabiting,\nmonogamous (CM)",
  "Married,\nnon-monogamous (MNM)",
  "Married,\nmonogamous (MM)",
  "Unknown (U)"
)

abbreviated_labels = c(
  "NP",
  "OCP",
  "NCNM",
  "NCM",
  "CNM",
  "CM",
  "MNM",
  "MM",
  "U"
)

# Meant for use with is_mono, but indices are off by 2 because is_mono return values start at -1
mono_labels = c(
  "Unknown",
  "No partners",
  "Monogamous",
  "Non-monogamous"
)
mono_full_labels = mono_labels
mono_abbreviations = mono_labels

## Calculation Functions
is_mono <- function(value) {
  return(if_else(
           value %in% c(2, 3, 5, 7), 2,
           if_else(value %in% c(4, 6, 8), 1,
           if_else(value == 1, 0,
           -1))))
}

reality_value <- function (columns) {
  marstat = columns[1]
  lypartn2 = columns[2]
  r1datel = columns[3]
  r1relat = columns[4]
  
  # Marital status
  if (marstat == 1) {  # married
    return(reality_type(lypartn2, 8, 7))
  }
  if (marstat %in% c(2, 3)) {  # unmarried and cohabiting
    return(reality_type(lypartn2, 6, 5))
  }
  
  # Most recent sex
  if (r1datel > 12 & r1datel != 9999) {  # Is 12 months a good threshold?
    return(1)  # "prefer no sex"
  }
  
  # Relationship with most recent partner => Might not still be with this person, if they aren't living together
  if (r1relat == 3) {  # "regular partners but never lived together"
    return(reality_type(lypartn2, 4, 3))
  }
  if (r1relat == 4) {  # "not regular partners"
    return(2)  # "only casual partners"
  }
  
  # This happens when there are unknown responses to things like marital status
  # or in the partnership/sexual history, so it does include respondents who
  # are, say, married, but monogamy/non-monogamy is unknown
  return(9)
}

# lypartn2: concurrent partnerships in past year
reality_type <- function (lypartn2, mono_value, poly_value) {
  if (lypartn2 == 0) {  # no
    return(mono_value)
  }
  if (lypartn2 == 1) {  # yes
    return(poly_value)
  }
  if (lypartn2 == 2) {  # 2+ partners, concurrency unknown
    # 1. Partnerships are considered concurrent if the month and year of 1st sex with the more recent partner is prior to the month and year of last sex with the former partner. These four variables are not available in the archived dataset for confidentiality reasons.
    # 2. Concurrency is assumed not to have taken place if the last sex with partner x occurred in the same month that the first sex with partner x+1 occurred.
    # "See notes in the Lancet paper by Johnson et al (2001) for details of the assumptions made" 
    # regarding serial monogamy vs non-monogamy
    # Lancet paper: "For the remaining respondents 
    # who had at least two partners in the year prior to interview, then 
    # these partners are assumed to be distributed between the serial monogamous
    # and concurrent categories with equal probabilities to those whose dates are known."
    # Validated that they were 50/50 odds with this:
    # `ideals_svy %>% filter(lypartn %in% c(2, 3)) %>% group_by(lypartn) %>% summarise(count = n())`
    # which yields 1019 serial monogamous and 1014 concurrent
    return(if_else(sample(0:1, 1) == 0, mono_value, poly_value))
  }
  if (lypartn2 == -1) {  # N/A
    return(9)
  }
  return(9)  # unknown
}

## Analysis Functions
read_and_filter <- function () {
  return(read_dta(data_location) %>% 
    select(
      totalpsu, strata, total_wt,
      rsex,                                # respondent gender
      dage, agrp, agrp2,                   # age variables
      rwpremar, rwadult, rwcohab, rwregp,  # opinions on premarital sex, adultery and infidelity
      idealnow, ideal5yr,                  # ideal columns
      lypartn,                             # determine proportion of serial mono vs concurrent
      marstat, lypartn2, r1datel, r1relat, # reality columns: must stay in this order for reality_value function to work
    ))
}

add_calculations <- function(filtered_data) {
  return(filtered_data %>% 
    mutate(reality = apply(subset(raw_2000, select=marstat:r1relat), 1, reality_value)) %>% 
    mutate(reality_is_mono = is_mono(reality)) %>% 
    mutate(future_is_mono = is_mono(ideal5yr)))
}

create_survey <- function(raw_data) {
  return(raw_data %>% 
    drop_na(totalpsu) %>%
    drop_na(strata) %>%
    as_survey_design(ids = totalpsu,
                     strata = strata,
                     weights = total_wt,
                     nest = TRUE)
  )
}


## Visualization Functions
create_alluvia <- function(survey) {
  return (survey %>% 
    group_by(reality, ideal5yr) %>% 
    summarise(count = survey_total()) %>%
    mutate(reality_label = alluvia_labels[reality],
           future_label = alluvia_labels[ideal5yr],
           future_full_label = full_labels[ideal5yr],
           reality_abbreviation = abbreviated_labels[reality])
  )
}

create_alluvia_mono <- function(survey) {
  return (survey %>% 
    group_by(reality_is_mono, future_is_mono) %>% 
    summarise(count = survey_total()) %>%
    mutate(reality = reality_is_mono,
           future = future_is_mono,
           reality_label = mono_labels[reality_is_mono + 2],
           future_label = mono_labels[future_is_mono + 2],
           future_full_label = mono_full_labels[future_is_mono + 2],
           reality_abbreviation = mono_abbreviations[reality_is_mono + 2])
  )
}

draw_alluvia <- function(data, title, annotations = c()) {
  ret = ggplot(as.data.frame(data),
       aes(y = count, axis1 = reality_label, axis2 = future_label)) +
  geom_alluvium(aes(fill = future_label), show.legend = FALSE) +
  geom_stratum(width = 1/4, show.legend = FALSE) +
  geom_text(stat = "stratum",
            min.y = 200,
            size = 3,
            mapping = aes(label = after_stat(stratum))) +
  theme_minimal() +
  theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
  ) +
  scale_x_discrete(limits = c("Current lifestyle", "Ideal lifestyle in 5 years"),
                   expand = c(0.15, 0.05)) +
  scale_y_continuous(breaks = c()) +
  labs(title = title,
       x = "",
       y = "")

  for (i in 1:length(annotations)) {
    ret <- ret + annotations[i]
  }

  return(ret)
}

draw_tiles <- function(alluvia_data, title) {
  totals_by_reality <- alluvia_data %>% 
    group_by(reality) %>% 
    summarise(total = sum(count))
  tiles <- merge(alluvia_data, totals_by_reality, by = "reality") %>% 
    mutate(percent = round(count * 100 / total)) %>% 
    select(-count) %>% 
    select(-count_se) %>%
    select(-total)

  ggplot(tiles, aes(x = reality_abbreviation, y = future_full_label, fill = percent)) + 
    scale_fill_gradient(low = "white", high = "steelblue",
                         trans = "log10",
                         na.value = "white") +
    geom_tile() +
    geom_text(aes(label = if_else(percent > 0, paste(as.character(percent), "%", sep=""), "")), size = 4) +
    theme(panel.background = element_rect(fill = "white")) +
    labs(title = title, x = "Current lifestyle", y = "Ideal lifestyle in 5 years")
}

# Analysis
raw_2000 <- read_and_filter()
ideals_raw <- add_calculations(raw_2000)
ideals_svy_with_unknown <- create_survey(ideals_raw)
ideals_svy <- ideals_svy_with_unknown %>% filter(reality != 9, ideal5yr != 9)

## Tables

# Proportions of current lifestyle, by monogamy
ideals_svy %>%
  group_by(reality_is_mono) %>%
  summarise(proportion = survey_prop()) %>%
  mutate(reality_label = mono_labels[reality_is_mono + 2])

# Proportions of future lifestyle, by monogamy
ideals_svy %>%
  group_by(future_is_mono) %>% 
  summarise(proportion = survey_prop()) %>% 
  mutate(future_label = mono_labels[future_is_mono + 2])

# Proportions of current lifestyle
ideals_svy %>%
  group_by(reality) %>% 
  summarise(proportion = survey_prop()) %>% 
  mutate(reality_label = plain_labels[reality])

# Proportions of future lifestyle
ideals_svy %>%
  group_by(ideal5yr) %>% 
  summarise(proportion = survey_prop()) %>% 
  mutate(future_label = plain_labels[ideal5yr])

# Number of future "Have no ideal / None of these / Don't know": 271 (out of 11851)
ideals_svy_with_unknown %>% filter(ideal5yr == 9) %>% summarise(total = survey_total())

# Proportions of current lifestyles, for future "Have no ideal / None of these / Don't know"
ideals_svy_with_unknown %>%
  group_by(reality) %>%
  filter(ideal5yr == 9) %>%
  summarise(proportion = survey_prop()) %>%
  mutate(reality_label = plain_labels[reality])

# Number of currently uncategorizable respondents: 1564
ideals_svy_with_unknown %>% filter(reality == 9) %>% summarise(total = survey_total())

# Proportions of future ideals, for currently uncategorizable
ideals_svy_with_unknown %>%
  group_by(ideal5yr) %>%
  filter(reality == 9) %>%
  summarise(proportion = survey_prop()) %>%
  mutate(future_label = plain_labels[ideal5yr])


## Visualizations

# Mono/non-mono status
alluvia_mono <- create_alluvia_mono(ideals_svy)
draw_alluvia(alluvia_mono, "Current monogamy/non-monogamy status and desired status in 5 years' time", c(
    annotate("text", x = 2, y = 1000, size = 3, label = "No partners"),
    annotate("segment", x = 1.98, y = 850, xend = 2, yend = 720)
))
draw_tiles(alluvia_mono, "Desired future monogamy, by percentage of participants\nin current lifestyle")

# All lifestyles
alluvia <- create_alluvia(ideals_svy)
is_alluvia_form(as.data.frame(alluvia), axes = 1:3, silent = TRUE)
draw_alluvia(alluvia, "Current lifestyle and desired lifestyle in 5 years' time")
draw_tiles(alluvia, "Desired future lifestyle, by percentage of participants\nin current lifestyle")

png(file="alluvia.png",width=11,height=8,units="in",res=300)
draw_alluvia(alluvia, "")
dev.off()
