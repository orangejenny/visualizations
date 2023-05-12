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

## Setup
options(survey.lonely.psu = "adjust")
options(na.action = "na.pass")
setwd("~/Dropbox/SOCIOL 651/Final - NATSAL")

## Constants
plain_labels = c(
  "No partners",
  "Only casual partners",
  "2+ regular partners",
  "Monogamous,\nliving apart",
  "Cohabiting, NM",
  "Cohabiting,\nmonogamous",
  "Married, NM",
  "Married,\nmonogamous",
  "Unknown"
)

full_labels = c(
  "NP - No partners",
  "OCP - Only casual partners",
  "2+RP - 2+ regular partners",
  "MLA - Monogamous, living apart",
  "CNM - Cohabiting, non-monogamous",
  "CM - Cohabiting, monogamous",
  "MNM - Married, non-monogamous",
  "MM - Married, monogamous",
  "U - Unknown"
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
  "none",
  "poly",
  "poly",
  "mono",
  "poly",
  "mono",
  "poly",
  "mono",
  "other"
)

# Meant for use with is_mono, but indices are off by 2 because is_mono return values start at -1
mono_labels = c(
  "Unknown",
  "No partners",
  "Monogamous",
  "Non-monogamous"
)

# Meant for use with commitment_level, indices off by 2 same as mono_labels
commitment_labels = c(
  "Unknown",
  "No partners",
  "Casual",
  "Not cohabiting",
  "Cohabiting",
  "Married"
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
  return(if_else(
           value %in% c(2, 3, 5, 7), 2,
           if_else(value %in% c(4, 6, 8), 1,
           if_else(value == 1, 0,
           -1))))
}

commitment_level <- function(value) {
  return(if_else(value %in% c(7, 8), 4, # married
           if_else(value %in% c(5, 6), 3, # unmarried & cohabiting
             if_else(value %in% c(3, 4), 2, # regularly partnered & not cohabiting
               if_else(value == 2, 1, # casual partners
                 if_else(value == 1, 0, # no partners
                   -1))))))  # unknown
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
    mutate(future_is_mono = is_mono(ideal5yr)) %>% 
    mutate(is_mono_direction = direction(reality_is_mono, future_is_mono)) %>% 
    mutate(reality_commitment_level = commitment_level(reality)) %>% 
    mutate(future_commitment_level = commitment_level(ideal5yr)) %>% 
    mutate(cohab_direction = direction(reality_commitment_level, future_commitment_level)) %>%
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

# Analysis
raw_2000 <- read_and_filter()
ideals_raw <- add_calculations(raw_2000)
ideals_svy <- create_survey(ideals_raw)
ideal_nodes <- create_nodes(ideals_svy %>% group_by(idealnow_node, ideal5yr_node))
reality_nodes <- create_nodes(ideals_svy %>% group_by(reality_node, ideal5yr_node))
ideal_vs_reality_nodes <- create_nodes(ideals_svy %>% group_by(reality_node, idealnow_right_node))

## Visualizations

# Sankey diagram: what are people doing vs where do they want to be in 5 years?
draw_sankey(reality_nodes, "reality_node", "ideal5yr_node")

# Sankey diagram: what are people doing right now vs what they want to be doing right now?
draw_sankey(ideal_vs_reality_nodes, "reality_node", "idealnow_right_node")
