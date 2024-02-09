setwd("~/Documents/visualizations")

data <- read_csv("~/Downloads/VOTER Panel Data Files/voter_panel.csv")

data %>% select(starts_with("household_children")) # household_children_2017, household_children_2016, household_children_2011

data %>% filter(household_children_2017 > household_children_2016) # 86 rows
data %>% filter(household_children_2016 > household_children_2011) # 601 rows
data %>% filter(household_children_2017 > household_children_2011) # 479 rows, which is comparable to CES

pivot_longer(data %>% slice_head(n = 1), starts_with("weight_"))

columns <- as.data.frame(colnames(data)) %>% 
  rename(name = 1) %>% 
  mutate(name_replaced = str_replace_all(name, "[0-9]", "."))

candidates <- columns %>%
  group_by(name_replaced) %>%
  summarise(total = n()) %>% 
  filter(total > 1)

# columns that start with issue_ are about importance of various issues

# TODO: incorporate weighting

# How big are sample sizes?
children <- data %>% 
  select(contains("household_children")) %>% 
  mutate(
    new_child_1_year = if_else(household_children_2017 > household_children_2016, 1, 0),
    new_child_5_years = if_else(household_children_2016 > household_children_2011, 1, 0),
    new_child_6_years = if_else(household_children_2017 > household_children_2011, 1, 0),
  ) %>% 
  summarise(
    everybody = n(), # n=~12K
    new_child_1_year = sum(new_child_1_year, na.rm = TRUE), # n=86
    new_child_5_years = sum(new_child_5_years, na.rm = TRUE), # n=601
    new_child_6_years = sum(new_child_6_years, na.rm = TRUE), # n=479
  )

one_year <- data %>% mutate(cycle = 1617)
five_years <- data %>% mutate(cycle = 1116)
six_years <- data %>% mutate(cycle = 1117)

#############
# Functions #
#############
add_parenting <- function(df) {
  return(
    df %>% mutate(
      new_child = if_else(cycle == 1617,
                          household_children_2016 < household_children_2017,
                          if_else(cycle == 1116,
                                  household_children_2011 < household_children_2016,
                                  household_children_2011 < household_children_2017)),
      firstborn = if_else(cycle == 1617,
                          household_children_2016 == 0 & household_children_2017 > 0,
                          if_else(cycle == 1116,
                                  household_children_2011 == 0 & household_children_2016 > 0,
                                  household_children_2011 == 0 & household_children_2017 > 0)),
    ) %>% select(-starts_with("household_children_"))
  )
}

add_ideo <- function(df) {
  valid = c(1:5)
  return(
    df %>% mutate(
      ideo_before = if_else(cycle == 1617, ideo5_2016, ideo5_2011),
      ideo_after = if_else(cycle == 1116, ideo5_2016, ideo5_2017),
      ideo_delta = if_else(ideo_before %in% valid & ideo_after %in% valid, ideo_after - ideo_before, NA),
      ideo_delta_abs = abs(ideo_delta),
      ideo_direction = if_else(is.na(ideo_delta),
                               NA,
                               if_else(ideo_delta > 0, 
                                       1, 
                                       if_else(ideo_delta < 0, -1, 0)))
    )
  )
}

add_pid <- function(df) {
  valid = c(1:7)
  return(
    df %>% mutate(
      pid_before = if_else(cycle == 1617, pid7_2016, pid7_2011),
      pid_after = if_else(cycle == 1116,  pid7_2016, pid7_2017),
      pid_delta = if_else(pid_before %in% valid & pid_after %in% valid, pid_after - pid_before, NA),
      pid_delta_abs = abs(pid_delta),
      pid_direction = if_else(is.na(pid_delta),
                              NA,
                              if_else(pid_delta > 0,
                                      1,
                                      if_else(pid_delta < 0, -1, 0))),
    )
  )
}

issues <- data %>% slice_head(n=1) %>% select(contains("issue_")) %>% select(
  contains("2011") | contains("2016") | contains("2017")
)
colnames(issues) # 12 issues shared between 2011, 2016, and 2017

add_issue_importance <- function (df) {
  return(
    df %>% mutate(
      abort_before = if_else(cycle == 1617, issue_abort_2016, issue_abort_2011),
      abort_after = if_else(cycle == 1116, issue_abort_2016, issue_abort_2017),
      budget_before = if_else(cycle == 1617, issue_budget_2016, issue_budget_2011),
      budget_after = if_else(cycle == 1116, issue_budget_2016, issue_budget_2017),
      econ_before = if_else(cycle == 1617, issue_econ_2016, issue_econ_2011),
      econ_after = if_else(cycle == 1116, issue_econ_2016, issue_econ_2017),
      educ_before = if_else(cycle == 1617, issue_educ_2016, issue_educ_2011),
      educ_after = if_else(cycle == 1116, issue_educ_2016, issue_educ_2017),
      envi_before = if_else(cycle == 1617, issue_envi_2016, issue_envi_2011),
      envi_after = if_else(cycle == 1116, issue_envi_2016, issue_envi_2017),
      gayrights_before = if_else(cycle == 1617, issue_gayrights_2016, issue_gayrights_2011),
      gayrights_after = if_else(cycle == 1116, issue_gayrights_2016, issue_gayrights_2017),
      healthcare_before = if_else(cycle == 1617, issue_healthcare_2016, issue_healthcare_2011),
      healthcare_after = if_else(cycle == 1116, issue_healthcare_2016, issue_healthcare_2017),
      immig_before = if_else(cycle == 1617, issue_immig_2016, issue_immig_2011),
      immig_after = if_else(cycle == 1116, issue_immig_2016, issue_immig_2017),
      medicare_before = if_else(cycle == 1617, issue_medicare_2016, issue_medicare_2011),
      medicare_after = if_else(cycle == 1116, issue_medicare_2016, issue_medicare_2017),
      socsec_before = if_else(cycle == 1617, issue_socsec_2016, issue_socsec_2011),
      socsec_after = if_else(cycle == 1116, issue_socsec_2016, issue_socsec_2017),
      taxes_before = if_else(cycle == 1617, issue_taxes_2016, issue_taxes_2011),
      taxes_after = if_else(cycle == 1116, issue_taxes_2016, issue_taxes_2017),
      terror_before = if_else(cycle == 1617, issue_terror_2016, issue_terror_2011),
      terror_after = if_else(cycle == 1116, issue_terror_2016, issue_terror_2017),
    ) %>% mutate(
      abort_before = if_else(abort_before %in% c(1:4), abort_before, NA),
      abort_after = if_else(abort_after %in% c(1:4), abort_after, NA),
      abort_delta = abort_after - abort_before,
      abort_delta_abs = abs(abort_delta),
      budget_before = if_else(budget_before %in% c(1:4), budget_before, NA),
      budget_after = if_else(budget_after %in% c(1:4), budget_after, NA),
      budget_delta = budget_after - budget_before,
      budget_delta_abs = abs(budget_delta),
      econ_before = if_else(econ_before %in% c(1:4), econ_before, NA),
      econ_after = if_else(econ_after %in% c(1:4), econ_after, NA),
      econ_delta = econ_after - econ_before,
      econ_delta_abs = abs(econ_delta),
      educ_before = if_else(educ_before %in% c(1:4), educ_before, NA),
      educ_after = if_else(educ_after %in% c(1:4), educ_after, NA),
      educ_delta = educ_after - educ_before,
      educ_delta_abs = abs(educ_delta),
      envi_before = if_else(envi_before %in% c(1:4), envi_before, NA),
      envi_after = if_else(envi_after %in% c(1:4), envi_after, NA),
      envi_delta = envi_after - envi_before,
      envi_delta_abs = abs(envi_delta),
      gayrights_before = if_else(gayrights_before %in% c(1:4), gayrights_before, NA),
      gayrights_after = if_else(gayrights_after %in% c(1:4), gayrights_after, NA),
      gayrights_delta = gayrights_after - gayrights_before,
      gayrights_delta_abs = abs(gayrights_delta),
      healthcare_before = if_else(healthcare_before %in% c(1:4), healthcare_before, NA),
      healthcare_after = if_else(healthcare_after %in% c(1:4), healthcare_after, NA),
      healthcare_delta = healthcare_after - healthcare_before,
      healthcare_delta_abs = abs(healthcare_delta),
      immig_before = if_else(immig_before %in% c(1:4), immig_before, NA),
      immig_after = if_else(immig_after %in% c(1:4), immig_after, NA),
      immig_delta = immig_after - immig_before,
      immig_delta_abs = abs(immig_delta),
      medicare_before = if_else(medicare_before %in% c(1:4), medicare_before, NA),
      medicare_after = if_else(medicare_after %in% c(1:4), medicare_after, NA),
      medicare_delta = medicare_after - medicare_before,
      medicare_delta_abs = abs(medicare_delta),
      socsec_before = if_else(socsec_before %in% c(1:4), socsec_before, NA),
      socsec_after = if_else(socsec_after %in% c(1:4), socsec_after, NA),
      socsec_delta = socsec_after - socsec_before,
      socsec_delta_abs = abs(socsec_delta),
      taxes_before = if_else(taxes_before %in% c(1:4), taxes_before, NA),
      taxes_after = if_else(taxes_after %in% c(1:4), taxes_after, NA),
      taxes_delta = taxes_after - taxes_before,
      taxes_delta_abs = abs(taxes_delta),
      terror_before = if_else(terror_before %in% c(1:4), terror_before, NA),
      terror_after = if_else(terror_after %in% c(1:4), terror_after, NA),
      terror_delta = terror_after - terror_before,
      terror_delta_abs = abs(terror_delta),
    )
  )
}

# TODO: add_composite_opinions

##################
# Transform data #
##################
one_year <- add_parenting(one_year)
five_years <- add_parenting(five_years)
six_years <- add_parenting(six_years)

one_year <- add_ideo(one_year)
five_years <- add_ideo(five_years)
six_years <- add_ideo(six_years)

one_year <- add_pid(one_year)
five_years <- add_pid(five_years)
six_years <- add_pid(six_years)

one_year <- add_issue_importance(one_year)
five_years <- add_issue_importance(five_years)
six_years <- add_issue_importance(six_years)

one_year <- add_composite_opinions(one_year)
five_years <- add_composite_opinions(five_years)
six_years <- add_parenting(six_years)

############
# Analysis # all of this is unweighted
############
t.test(ideo_delta~new_child, data=filter_na(one_year, "ideo_delta")) # p = 0.5205
t.test(ideo_delta_abs~new_child, data=filter_na(one_year, "ideo_delta_abs")) # p = 0.5036
t.test(ideo_delta~new_child, data=filter_na(five_years, "ideo_delta")) # p = 0.7588
t.test(ideo_delta_abs~new_child, data=filter_na(five_years, "ideo_delta_abs")) # p = 0.832
t.test(ideo_delta~new_child, data=filter_na(six_years, "ideo_delta")) # p = 0.4334
t.test(ideo_delta_abs~new_child, data=filter_na(six_years, "ideo_delta_abs")) # p = 0.919

t.test(pid_delta~new_child, data=filter_na(one_year, "pid_delta")) # p = 0.6554
t.test(pid_delta_abs~new_child, data=filter_na(one_year, "pid_delta_abs")) # p = 0.3472
t.test(pid_delta~new_child, data=filter_na(five_years, "pid_delta")) # p = 0.8048
t.test(pid_delta_abs~new_child, data=filter_na(five_years, "pid_delta_abs")) # p = 0.1255
t.test(pid_delta~new_child, data=filter_na(six_years, "pid_delta")) # p = 0.4166
t.test(pid_delta_abs~new_child, data=filter_na(six_years, "pid_delta_abs")) # p = 0.02172*

t.test(abort_after~new_child, data=filter_na(one_year, "abort_after")) # p = 0.9926
t.test(abort_delta~new_child, data=filter_na(one_year, "abort_delta")) # p = 0.7
t.test(abort_delta_abs~new_child, data=filter_na(one_year, "abort_delta_abs")) # p = 0.1582
t.test(budget_after~new_child, data=filter_na(one_year, "budget_after")) # p = 0.03732*
t.test(budget_delta~new_child, data=filter_na(one_year, "budget_delta")) # p = 0.8277
t.test(budget_delta_abs~new_child, data=filter_na(one_year, "budget_delta_abs")) # p = 0.278
t.test(econ_after~new_child, data=filter_na(one_year, "econ_after")) # p = 0.9337
t.test(econ_delta~new_child, data=filter_na(one_year, "econ_delta")) # p = 0.5585
t.test(econ_delta_abs~new_child, data=filter_na(one_year, "econ_delta_abs")) # p = 0.4471
t.test(educ_after~new_child, data=filter_na(one_year, "educ_after")) # p = 0.5293
t.test(educ_delta~new_child, data=filter_na(one_year, "educ_delta")) # p = 0.395
t.test(educ_delta_abs~new_child, data=filter_na(one_year, "educ_delta_abs")) # p = 0.1837
t.test(envi_after~new_child, data=filter_na(one_year, "envi_after")) # p = 0.2611
t.test(envi_delta~new_child, data=filter_na(one_year, "envi_delta")) # p = 0.3515
t.test(envi_delta_abs~new_child, data=filter_na(one_year, "envi_delta_abs")) # p = 0.5066
t.test(gayrights_after~new_child, data=filter_na(one_year, "gayrights_after")) # p = 0.3531
t.test(gayrights_delta~new_child, data=filter_na(one_year, "gayrights_delta")) # p = 0.857
t.test(gayrights_delta_abs~new_child, data=filter_na(one_year, "gayrights_delta_abs")) # p = 0.0139*
t.test(healthcare_after~new_child, data=filter_na(one_year, "healthcare_after")) # p = 0.1767
t.test(healthcare_delta~new_child, data=filter_na(one_year, "healthcare_delta")) # p = 0.816
t.test(healthcare_delta_abs~new_child, data=filter_na(one_year, "healthcare_delta_abs")) # p = 0.07095
t.test(immig_after~new_child, data=filter_na(one_year, "immig_after")) # p = 0.1749
t.test(immig_delta~new_child, data=filter_na(one_year, "immig_delta")) # p = 0.5284
t.test(immig_delta_abs~new_child, data=filter_na(one_year, "immig_delta_abs")) # p = 0.8988
t.test(medicare_after~new_child, data=filter_na(one_year, "medicare_after")) # p = 0.2348
t.test(medicare_delta~new_child, data=filter_na(one_year, "medicare_delta")) # p = 0.347
t.test(medicare_delta_abs~new_child, data=filter_na(one_year, "medicare_delta_abs")) # p = 0.7394
t.test(socsec_after~new_child, data=filter_na(one_year, "socsec_after")) # p = 0.7572
t.test(socsec_delta~new_child, data=filter_na(one_year, "socsec_delta")) # p = 0.5488
t.test(socsec_delta_abs~new_child, data=filter_na(one_year, "socsec_delta_abs")) # p = 0.3412
t.test(taxes_after~new_child, data=filter_na(one_year, "taxes_after")) # p = 0.29
t.test(taxes_delta~new_child, data=filter_na(one_year, "taxes_delta")) # p = 0.2987
t.test(taxes_delta_abs~new_child, data=filter_na(one_year, "taxes_delta_abs")) # p = 0.2323
t.test(terror_after~new_child, data=filter_na(one_year, "terror_after")) # p = 0.9023
t.test(terror_delta~new_child, data=filter_na(one_year, "terror_delta")) # p = 0.6169
t.test(terror_delta_abs~new_child, data=filter_na(one_year, "terror_delta_abs")) # p = 0.8365

t.test(abort_after~new_child, data=filter_na(five_years, "abort_after")) # p = 0.8964
t.test(abort_delta~new_child, data=filter_na(five_years, "abort_delta")) # p = 0.5706
t.test(abort_delta_abs~new_child, data=filter_na(five_years, "abort_delta_abs")) # p = 0.6335
t.test(budget_after~new_child, data=filter_na(five_years, "budget_after")) # p = 0.008964**
t.test(budget_delta~new_child, data=filter_na(five_years, "budget_delta")) # p = 0.1504
t.test(budget_delta_abs~new_child, data=filter_na(five_years, "budget_delta_abs")) # p = 0.3423
t.test(econ_after~new_child, data=filter_na(five_years, "econ_after")) # p = 0.04548*
t.test(econ_delta~new_child, data=filter_na(five_years, "econ_delta")) # p = 0.347
t.test(econ_delta_abs~new_child, data=filter_na(five_years, "econ_delta_abs")) # p = 0.09008
t.test(educ_after~new_child, data=filter_na(five_years, "educ_after")) # p = 0.6614
t.test(educ_delta~new_child, data=filter_na(five_years, "educ_delta")) # p = 0.00001316***
t.test(educ_delta_abs~new_child, data=filter_na(five_years, "educ_delta_abs")) # p = 02476
t.test(envi_after~new_child, data=filter_na(five_years, "envi_after")) # p = 0.001708**
t.test(envi_delta~new_child, data=filter_na(five_years, "envi_delta")) # p = 0.8145
t.test(envi_delta_abs~new_child, data=filter_na(five_years, "envi_delta_abs")) # p = 0.07895
t.test(gayrights_after~new_child, data=filter_na(five_years, "gayrights_after")) # p = 0.0000003187***
t.test(gayrights_delta~new_child, data=filter_na(five_years, "gayrights_delta")) # p = 0.6362
t.test(gayrights_delta_abs~new_child, data=filter_na(five_years, "gayrights_delta_abs")) # p = 0.05127
t.test(healthcare_after~new_child, data=filter_na(five_years, "healthcare_after")) # p = 0.1294
t.test(healthcare_delta~new_child, data=filter_na(five_years, "healthcare_delta")) # p = 0.0853
t.test(healthcare_delta_abs~new_child, data=filter_na(five_years, "healthcare_delta_abs")) # p = 0.006693**
t.test(immig_after~new_child, data=filter_na(five_years, "immig_after")) # p = 0.03887
t.test(immig_delta~new_child, data=filter_na(five_years, "immig_delta")) # p = 0.5217
t.test(immig_delta_abs~new_child, data=filter_na(five_years, "immig_delta_abs")) # p = 0.02958
t.test(medicare_after~new_child, data=filter_na(five_years, "medicare_after")) # p = 0.001701
t.test(medicare_delta~new_child, data=filter_na(five_years, "medicare_delta")) # p = 0.1663
t.test(medicare_delta_abs~new_child, data=filter_na(five_years, "medicare_delta_abs")) # p = 0.02714
t.test(socsec_after~new_child, data=filter_na(five_years, "socsec_after")) # p = 0.3126
t.test(socsec_delta~new_child, data=filter_na(five_years, "socsec_delta")) # p = 0.8832
t.test(socsec_delta_abs~new_child, data=filter_na(five_years, "socsec_delta_abs")) # p = 0.1808
t.test(taxes_after~new_child, data=filter_na(five_years, "taxes_after")) # p = 0.00005649***
t.test(taxes_delta~new_child, data=filter_na(five_years, "taxes_delta")) # p = 0.09663
t.test(taxes_delta_abs~new_child, data=filter_na(five_years, "taxes_delta_abs")) # p = 0.01268*
t.test(terror_after~new_child, data=filter_na(five_years, "terror_after")) # p = 0.0006683***
t.test(terror_delta~new_child, data=filter_na(five_years, "terror_delta")) # p = 0.4161
t.test(terror_delta_abs~new_child, data=filter_na(five_years, "terror_delta_abs")) # p = 0.0642

t.test(abort_after~new_child, data=filter_na(six_years, "abort_after")) # p = 0.9611
t.test(abort_delta~new_child, data=filter_na(six_years, "abort_delta")) # p = 0.8723
t.test(abort_delta_abs~new_child, data=filter_na(six_years, "abort_delta_abs")) # p = 0.2021
t.test(budget_after~new_child, data=filter_na(six_years, "budget_after")) # p = 0.01803
t.test(budget_delta~new_child, data=filter_na(six_years, "budget_delta")) # p = 0.1497
t.test(budget_delta_abs~new_child, data=filter_na(six_years, "budget_delta_abs")) # p = 0.2081
t.test(econ_after~new_child, data=filter_na(six_years, "econ_after")) # p = 0.04926*
t.test(econ_delta~new_child, data=filter_na(six_years, "econ_delta")) # p = 0.4554
t.test(econ_delta_abs~new_child, data=filter_na(six_years, "econ_delta_abs")) # p = 0.5249
t.test(educ_after~new_child, data=filter_na(six_years, "educ_after")) # p = 0.708
t.test(educ_delta~new_child, data=filter_na(six_years, "educ_delta")) # p = 0.00557**
t.test(educ_delta_abs~new_child, data=filter_na(six_years, "educ_delta_abs")) # p = 0.1133
t.test(envi_after~new_child, data=filter_na(six_years, "envi_after")) # p = 0.0008443***
t.test(envi_delta~new_child, data=filter_na(six_years, "envi_delta")) # p = 0.9764
t.test(envi_delta_abs~new_child, data=filter_na(six_years, "envi_delta_abs")) # p = 0.0009018***
t.test(gayrights_after~new_child, data=filter_na(six_years, "gayrights_after")) # p = 0.00006543***
t.test(gayrights_delta~new_child, data=filter_na(six_years, "gayrights_delta")) # p = 0.7333
t.test(gayrights_delta_abs~new_child, data=filter_na(six_years, "gayrights_delta_abs")) # p = 0.001627**
t.test(healthcare_after~new_child, data=filter_na(six_years, "healthcare_after")) # p = 0.00383
t.test(healthcare_delta~new_child, data=filter_na(six_years, "healthcare_delta")) # p = 0.3196
t.test(healthcare_delta_abs~new_child, data=filter_na(six_years, "healthcare_delta_abs")) # p = 0.006431**
t.test(immig_after~new_child, data=filter_na(six_years, "immig_after")) # p = 0.04641*
t.test(immig_delta~new_child, data=filter_na(six_years, "immig_delta")) # p = 0.6126
t.test(immig_delta_abs~new_child, data=filter_na(six_years, "immig_delta_abs")) # p = 0.004662**
t.test(medicare_after~new_child, data=filter_na(six_years, "medicare_after")) # p = 0.0001244***
t.test(medicare_delta~new_child, data=filter_na(six_years, "medicare_delta")) # p = 0.905
t.test(medicare_delta_abs~new_child, data=filter_na(six_years, "medicare_delta_abs")) # p = 0.00132**
t.test(socsec_after~new_child, data=filter_na(six_years, "socsec_after")) # p = 0.2239
t.test(socsec_delta~new_child, data=filter_na(six_years, "socsec_delta")) # p = 0.9191
t.test(socsec_delta_abs~new_child, data=filter_na(six_years, "socsec_delta_abs")) # p = 0.02474*
t.test(taxes_after~new_child, data=filter_na(six_years, "taxes_after")) # p = 0.0009713
t.test(taxes_delta~new_child, data=filter_na(six_years, "taxes_delta")) # p = 0.2795
t.test(taxes_delta_abs~new_child, data=filter_na(six_years, "taxes_delta_abs")) # p = 0.1777
t.test(terror_after~new_child, data=filter_na(six_years, "terror_after")) # p = 0.02428*
t.test(terror_delta~new_child, data=filter_na(six_years, "terror_delta")) # p = 0.21
t.test(terror_delta_abs~new_child, data=filter_na(six_years, "terror_delta_abs")) # p = 0.2935
