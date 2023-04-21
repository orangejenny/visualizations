library(haven)
library(survey)
library(srvyr)
library(tidyverse)
library(networkD3)
library(ggalluvial)

## Setup
options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")
setwd("~/Dropbox/SOCIOL 651/Final - NATSAL")

## Constants
plain_labels = c(
  "prefer no sex",
  "only casual partners",
  "multiple regular partners",
  "monogamous, not living together",
  "living together, non-monogamous",
  "living together, monogamous",
  "married, non-monogamous",
  "married, monogamous",
  "unknown"
)

full_labels = c(
  "PNS - prefer no sex",
  "OCP - only casual partners",
  "MRP - multiple regular partners",
  "MNLT - monogamous, not living together",
  "LTNM - living together, non-monogamous",
  "LTM - living together, monogamous",
  "MNM - married, non-monogamous",
  "MM - married, monogamous",
  "U - unknown"
)

abbreviated_labels = c(
  "PNS",
  "OCP",
  "MRP",
  "MNLT",
  "LTNM",
  "LTM",
  "MNM",
  "MM",
  "U"
)
 
ideals_labels_types = c(
  "other",
  "poly",
  "poly",
  "mono",
  "poly",
  "mono",
  "poly",
  "mono",
  "other"
)

# Meant for use with is_mono, but indices are off by 2 because is_mono return -1, 0, or 1
mono_labels = c(
  "unknown",
  "poly",
  "mono"
)

sankey_labels <- data.frame(
  "name" = c(plain_labels, plain_labels),
  "node_type" = c(ideals_labels_types, ideals_labels_types),
  "time" = c("now", "now", "now", "now", "now", "now", "now", "now", "now",
             "in 5 years", "in 5 years", "in 5 years", "in 5 years", "in 5 years",
             "in 5 years", "in 5 years", "in 5 years", "in 5 years"),
  "node_value" = c(seq(0, 8, by = 1), seq(0, 8, by = 1)),
  "response_value" = c(seq(1, 9, by = 1), seq(1, 9, by = 1))
)

## Calculation Functions
is_mono <- function(value) {
  return(if_else(value %in% c(4, 6, 8), 1, if_else(value %in% c(2, 3, 5, 7), 0, -1)))
}

is_cohab <- function(value) {
  return(if_else(value %in% c(5, 6, 7, 8), 1, if_else(value %in% c(2, 3, 4), 0, -1)))
}

direction <- function(old, new) {
  if_else(old == -1 | new == -1, -2, if_else(
    old == new, 0, if_else(
      old < new, 1, -1
    )
  ))
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
    # regarding serial monogamy vs nonmonogamy
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
  return(read_dta("UKDA-5223-stata11/stata11/natsal_2000_for_archive.dta") %>% 
    #filter(dage <= 25) %>% 
    select(
      totalpsu, strata, total_wt,
      rsex, ethnic11,                      # gender and ethnic group
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
    mutate(future_is_mono = is_mono(ideal5yr)) %>% 
    mutate(is_mono_direction = direction(reality_is_mono, future_is_mono)) %>% 
    mutate(reality_is_cohab = is_cohab(reality)) %>% 
    mutate(future_is_cohab = is_cohab(ideal5yr)) %>% 
    mutate(cohab_direction = direction(reality_is_cohab, future_is_cohab)) %>%
    mutate(combined_direction = if_else(is_mono_direction == cohab_direction, is_mono_direction, -2)) %>%
    mutate(idealnow_node = idealnow - 1,  # zero index for sankeyNetwork
           idealnow_right_node = idealnow + 8,
           ideal5yr_node = ideal5yr + 8,  # differentiate current & future state values
           reality_node = reality - 1))
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
create_nodes <- function (grouped_survey) {
  return(
    grouped_survey %>% 
      summarise(
        count = survey_total()
      )    
  )
}

draw_sankey <- function(nodes, source, target) {
  sankeyNetwork(Links = as.data.frame(nodes),
              #LinkGroup = "reality_is_mono",
              Nodes = sankey_labels, Source = source,
              Target = target, Value = "count",
              NodeID = "name",
              NodeGroup = "node_type",
              sinksRight = FALSE,
              fontSize = 15, fontFamily = "Helvetica", nodeWidth = 20)
}

# TODO: DRY up this and create_alluvia_mono
create_alluvia <- function(survey) {
  return (survey %>% 
    group_by(reality, ideal5yr) %>% 
    #filter(reality != 8) %>%  # Filter out married people, who almost all want to stay that way
    #filter(reality %in% c(4, 5, 6, 7, 8)) %>%  # Filter down to partnered people
    #filter(reality %in% c(2, 3, 5, 7)) %>%  # Filter down to non-mono people
    #filter(reality %in% c(7)) %>%  # Filter down to married non-mono
    summarise(
      count = survey_total()
    ) %>% 
    mutate(reality_label = plain_labels[reality],
           future_label = plain_labels[ideal5yr],
           future_full_label = full_labels[ideal5yr],
           reality_abbreviation = abbreviated_labels[reality])
  )
}

create_alluvia_mono <- function(survey) {
  return (survey %>% 
    group_by(reality_is_mono, future_is_mono) %>% 
    summarise(
      count = survey_total()
    ) %>% 
    filter(reality_is_mono != -1 & future_is_mono != -1) %>% 
    mutate(reality_label = mono_labels[reality_is_mono + 2],
           future_label = mono_labels[future_is_mono + 2],
           future_full_label = future_label,
           reality_abbreviation = reality_label)
  )
}

draw_alluvia <- function(data) {
  return(ggplot(as.data.frame(data),
       aes(y = count, axis1 = reality_label, axis2 = future_label)) +
  geom_alluvium(aes(fill = future_label), show.legend = FALSE) +
  geom_stratum(width = 1/4, show.legend = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  #annotate("text", x = 2, y = 1200, size = 4, label = "monogamous, not living together") +
  #annotate("text", x = 1.4, y = 900, size = 4, label = "222") +
  #annotate("text", x = 1.6, y = 800, size = 4, label = "333") +
  #annotate("text", x = 1.8, y = 700, size = 4, label = "444") +
  #annotate("text", x = 2, y = 600, size = 4, label = "555") +
  scale_x_discrete(limits = c("Current Lifestyle", "Ideal Lifestyle In Five Years"),
                   expand = c(0.15, 0.05)) +
  theme_void())
}

# Analysis
raw_2000 <- read_and_filter()
ideals_raw <- add_calculations(raw_2000)
#ideals_raw <- ideals_raw %>% filter(reality != 8)
ideals_svy <- create_survey(ideals_raw)
ideal_nodes <- create_nodes(ideals_svy %>% group_by(idealnow_node, ideal5yr_node))
reality_nodes <- create_nodes(ideals_svy %>% group_by(reality_node, ideal5yr_node))
ideal_vs_reality_nodes <- create_nodes(ideals_svy %>% group_by(reality_node, idealnow_right_node))

## Visualizations

# Sankey diagrams
# What are people doing vs where do they want to be in 5 years?
draw_sankey(reality_nodes, "reality_node", "ideal5yr_node")
# What are people doing right now vs what they want to be doing right now?
draw_sankey(ideal_vs_reality_nodes, "reality_node", "idealnow_right_node")

# Alluvial diagram: reality vs ideal in 5 years
alluvia <- create_alluvia(ideals_svy)
is_alluvia_form(as.data.frame(alluvia), axes = 1:3, silent = TRUE)
draw_alluvia(alluvia)

# TODO: Look at partnered people only (there's a filter in create_alluvia for this)
# TODO: collapse non-partnered outcomes into a single "other" category

# Look at mono/poly categories only, not all lifestyles
alluvia_mono <- create_alluvia_mono(ideals_svy)
is_alluvia_form(as.data.frame(alluvia_mono), axes = 1:3, silent = TRUE)
draw_alluvia(alluvia_mono)

# Tiled plot showing percentage of each pair of current+future lifestyle
totals_by_reality <- alluvia %>% 
  group_by(reality) %>% 
  summarise(total = sum(count))
totals_by_future <- alluvia %>% 
  group_by(ideal5yr) %>% 
  summarise(total = sum(count))
tiles <- merge(alluvia, totals_by_reality, by = "reality") %>% 
  mutate(percent = round(count * 100 / total)) %>% 
  select(-count) %>% 
  select(-count_se) %>%
  select(-total)

ggplot(tiles, aes(x = reality_abbreviation, y = future_full_label, fill = percent)) + 
  scale_fill_gradient(low = "white", high = "steelblue",
                       #guide = "legend",
                       trans = "log10",
                       na.value = "white") +
  geom_tile() +
  geom_text(aes(label = if_else(percent > 0, as.character(percent), "")), size = 4) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(x = "Current Lifestyle", y = "Ideal Lifestyle In Five Years")

# Faceted age group plots for current lifestyle
ideal5yrage_groups <- ideals_svy %>% 
  mutate(reality_label = plain_labels[reality]) %>% 
  group_by(reality_label, agrp2) %>% 
  summarise(count = survey_total())
ggplot(age_groups, aes(x = agrp2, y = count)) + 
  geom_col() + 
  facet_wrap(~ reality_label) +
  scale_x_discrete(limits=c()) +
  labs(title = "Current Lifestyle by Age Group",
       x = "Age Group (16-19, 20-24, 25-29, 30-34, 35-39, 40-44)",
       y = "Participant Count")

# Faceted age group plots for ideal future lifestyle
age_groups_future <- ideals_svy %>% 
  mutate(future_label = plain_labels[ideal5yr]) %>% 
  group_by(future_label, agrp2) %>% 
  summarise(count = survey_total())
ggplot(age_groups_future, aes(x = agrp2, y = count)) + 
  geom_col() + 
  facet_wrap(~ future_label) +
  scale_x_discrete(limits=c()) +
  labs(title = "Ideal Lifestyle by Age Group",
       x = "Age Group (16-19, 20-24, 25-29, 30-34, 35-39, 40-44)",
       y = "Participant Count")

# Faceted gender plots for ideal future lifestyle, not very interesting
# More women want to be married vs cohabiting, more men want to be non-monogamous
rsex_future <- ideals_svy %>% 
  mutate(future_label = plain_labels[ideal5yr]) %>% 
  group_by(future_label, rsex) %>% 
  summarise(count = survey_total())
ggplot(rsex_future, aes(x = rsex, y = count)) + 
  geom_col() + 
  facet_wrap(~ future_label) +
  scale_x_discrete(limits=c()) +
  labs(title = "Ideal Lifestyle by Gender",
       x = "Sex (1=male, 2=female)",
       y = "Participant Count")

# Faceted ethnicity plots for ideal future lifestyle
# Useless because of the white dominance of sample, would need to look at rates, not counts
ethnic_future <- ideals_svy %>% 
  mutate(future_label = plain_labels[ideal5yr]) %>% 
  group_by(future_label, ethnic11) %>% 
  summarise(count = survey_total())
ggplot(ethnic_future, aes(x = ethnic11, y = count)) + 
  geom_col() + 
  facet_wrap(~ future_label) +
  scale_x_discrete(limits=c()) +
  labs(title = "Ideal Lifestyle by Ethnic Group",
       x = "Ethnic group (black, white, Indian, Pakistani, Bangladeshi, Chinese, other Asian, other, unanswered)",
       y = "Participant Count")
