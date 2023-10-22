library(haven)

panel <- read_dta("CCES_Panel_Full3waves_VV_V4.dta") # n=9500

panel %>%
  select(CC12_387_10, CC14_387_10) %>% 
  group_by(CC12_387_10, CC14_387_10) %>% 
  summarise(count = n())


# Filter to people whose ideology changed over time, n=3200
trends <- panel %>% zap_labels() %>% 
  select(
    CC12_387_10, CC14_387_10,
    starts_with("ideo5_"),
    starts_with("gender_"),
    starts_with("child18num_"),
    starts_with("faminc_")
  ) %>% 
  mutate(baby_in_2012 = CC12_387_10 == 1, baby_in_2014 = CC14_387_10 == 1) %>%
  mutate(new_parent = CC12_387_10 == 1 | CC14_387_10 == 1) %>% 
  # TODO: why is new_parent NA for a bunch of people? Lack of response?
  #mutate(new_parent = CC12_387_10 == 1 & child18num_12 == 1 | CC14_387_10 == 1 & child18num_14 == 1) %>% # filter to first-time parents, recognizing this misses multiple births and includes families with large age gaps
  mutate(new_father = new_parent & gender_10 == 1) %>% # Verified that the 3200 rows post-filtering are all 1 or 2, and the counts are the same across years
  mutate(new_mother = new_parent & gender_10 == 2) %>% 
  filter(ideo5_10 < 8, ideo5_12 < 8, ideo5_14 < 8) %>% 
  filter(ideo5_10 < 6, ideo5_12 < 6, ideo5_14 < 6) %>% # filter out "not sure" (?)
  mutate(left_bump = if_else(ideo5_10 > ideo5_12 & ideo5_12 < ideo5_14, 1, 0)) %>% 
  mutate(right_bump = if_else(ideo5_10 < ideo5_12 & ideo5_12 > ideo5_14, 1, 0)) %>% 
  mutate(leftward = if_else(ideo5_10 >= ideo5_12 & ideo5_12 >= ideo5_14, 1, 0)) %>% 
  mutate(rightward = if_else(ideo5_10 <= ideo5_12 & ideo5_12 <= ideo5_14, 1, 0)) %>% 
  # TODO: update to use 2010/2012 for people who reported births in both 2012 and 2014, since the earlier birth might be a firstborn
  mutate(ideo_before = if_else(CC12_387_10 == 1, ideo5_10, ideo5_12)) %>% # for non-parents, this looks at their 2012 & 2014 responses - is anyone a new parent in both 2012 and 2014?
  mutate(ideo_after = if_else(CC12_387_10 == 1, ideo5_12, ideo5_14)) %>% 
  filter(ideo_before != ideo_after) %>% # remove and instead add a column for staying the same?
  mutate(ideo_way_after = if_else(CC12_387_10 == 1, ideo5_14, NA)) %>% 
  mutate(moved_left = ideo_before > ideo_after) %>% 
  mutate(moved_right = ideo_before < ideo_after)

agg <- trends %>% group_by(baby_in_2014, moved_left, moved_right) %>% summarise(count = n())

trends %>%
  group_by(new_parent, gender_10) %>%
  summarise(before = sum(ideo5_10) / n(), after = sum(ideo5_12) / n())

trends %>%
  group_by(new_parent) %>%
  summarise(before = sum(ideo5_10) / n(), after = sum(ideo5_12) / n())

trends %>% filter(new_parent == TRUE) %>% group_by(faminc_10) %>% summarise(count=n())

trends %>%
  group_by(CC14_387_10) %>%
  summarise(before = sum(ideo5_12) / n(), after = sum(ideo5_14) / n())


# TODO: 
#   - see why new parent is often NA (TODO above)
#   - add column to represent firstborn
#   - reformat data so there's one set from 2010/2012 and one from 2012/2014
#   - add columns for legislation