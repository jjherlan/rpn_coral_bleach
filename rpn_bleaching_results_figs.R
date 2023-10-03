require(plyr)
require(tidyverse)
require(ggmisc)

# Mean Daily Temp

rpn_sst_2014_15_3months.stats <- 
  rpn_sst_2014_15_3months %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  ) %>%
  pivot_longer(!date_time,
               names_to = 'site', 
               values_to = 'temp'
  ) %>%
  mutate_at(vars(site), factor) %>%
  mutate(year = format(date_time, '%Y'),
         month = format(date_time, '%m'), 
         day = format(date_time, '%d')
  ) %>%
  group_by(site, month, year, day) %>%
  summarise(temp_mean = mean(temp)) %>%
  mutate_at(vars(month, year, day), factor) %>%
  group_by(site) %>%
  filter(date_time >= ymd_hms('2014-12-01 00:00:00'),
         date_time <= ymd_hms('2015-03-01 23:45:00'))