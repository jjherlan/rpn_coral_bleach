require(tidyverse)
require(lubridate)

#make_datetime_100 <- function(year, month, day, time) {
#  make_datetime(year, month, day, time %/% 100, time %% 100)
#}

rpn_temp.main <-
tibble(date_time = 
  seq(ymd_hm('2014-01-01 00:00'),
    ymd_hm('2016-12-31 23:45'), 
    by = '15 mins')
  )

rpn_temp.main

rpn_temp_10530651 <- read_csv("rpn_IslaPascua_10530651.csv")

rpn_temp_10530651.main <-
  rpn_temp_10530651 %>% 
  #as.Date(date,"%m/%d/%Y")
  #select(date, time) %>% 
  #mutate(date_main = make_datetime(date, time))
  #mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  #mutate(time = hm(time)) %>% 
  mutate_at(vars(time), funs(hour, minute)) %>%
  select(year, month, day, hour, minute, temp) %>% 
  mutate(date_time = make_datetime(year, month, day, hour, minute)) %>%
  rename(temp_10530651 = temp)

# %>% separate(time, into = c('hour', 'minute'))

mergeCols <- c("date_time")

#inner_dplyr <- inner_join(rpn_temp.main, rpn_temp_10530651.main, by = mergeCols)
#left_dplyr  <- left_join(rpn_temp.main, rpn_temp_10530651.main, by = mergeCols)
#right_dplyr <- right_join(rpn_temp.main, rpn_temp_10530651.main, by = mergeCols)

#rpn_temp_main  <- 
#  full_join(rpn_temp.main, rpn_temp_10530651.main, by = mergeCols)

# Do this last step

rpn_temp_main1  <- 
  full_join(rpn_temp.main, 
            rpn_temp_10530651.main, 
            #rpn_temp_10530653_Norte.main, 
            by = mergeCols)

rpn_temp_main2  <- 
  full_join(rpn_temp_main1, 
            rpn_temp_10530653_Norte.main, 
            by = mergeCols)

rpn_temp_main3  <- 
  full_join(rpn_temp_main2, 
            rpn_temp_10530652_sur.main, 
            by = mergeCols)

rpn_temp_main4  <- 
  full_join(rpn_temp_main3, 
            rpn_temp_10530652_Sur1.main, 
            by = mergeCols)

rpn_temp_main5  <- 
  full_join(rpn_temp_main4, 
            rpn_temp_10530652_Sur2.main, 
            by = mergeCols) 

rpn_sea_water_temp.main <-
  rpn_temp_main5 %>%
    select(date_time, 
           temp_10530651, 
           temp_10530653_Norte, 
           temp_10530652_sur, 
           temp_10530652_Sur1,
           temp_10530652_Sur2
           )

rpn_sea_water_temp.gg <- ggplot(data = rpn_sea_water_temp.main, aes(x = date_time)
                                ) +
  geom_line(aes(y = temp_10530651, colour = "temp_10530651 (°C)")) +
  geom_line(aes(y = temp_10530653_Norte, colour = "temp_10530653_Norte (°C)")) +
  geom_line(aes(y = temp_10530652_sur, colour = "temp_10530652_sur (°C)")) +
  geom_line(aes(y = temp_10530652_Sur1, colour = "temp_10530652_Sur1 (°C)")) +
  geom_line(aes(y = temp_10530652_Sur2, colour = "temp_10530652_Sur2 (°C)")) +
#  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  #scale_color_manual(values = c("red2")) +
  scale_fill_brewer(palette = "Set1") +
  # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    legend.title = element_blank()
    )

rpn_sea_water_temp.gg

rpn_sea_water_temp.gg <- ggplot(data = rpn_sea_water_temp.main, aes(x = date_time)
) +
  geom_line(aes(y = temp_10530651, colour = "temp_10530651 (°C)")) +
  geom_line(aes(y = temp_10530653_Norte, colour = "temp_10530653_Norte (°C)")) +
  geom_line(aes(y = temp_10530652_sur, colour = "temp_10530652_sur (°C)")) +
  geom_line(aes(y = temp_10530652_Sur1, colour = "temp_10530652_Sur1 (°C)")) +
  geom_line(aes(y = temp_10530652_Sur2, colour = "temp_10530652_Sur2 (°C)")) +
  #  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  #scale_color_manual(values = c("red2")) +
  scale_fill_brewer(palette = "Set1") +
  # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  )

rpn_sea_water_temp.gg

rpn_sst_2015_increasing <- 
rpn_sea_water_temp.main %>% 
  filter(date_time >= ymd_hms('2015-01-01 00:00:00'),
         date_time <= ymd_hms('2015-03-20 23:45:00') 
         ) %>%
  select(date_time,
         temp_10530651, 
         temp_10530652_Sur1,
         temp_10530653_Norte
         )

rpn_sea_water_temp.gg <- ggplot(data = rpn_sst_2015_increasing, aes(x = date_time)
) +
  geom_line(aes(y = temp_10530651, colour = "temp_10530651_SE_25_m (°C)")) +
  geom_line(aes(y = temp_10530653_Norte, colour = "temp_10530653_Norte_15_m (°C)")) +
#  geom_line(aes(y = temp_10530652_sur, colour = "temp_10530652_sur (°C)")) +
  geom_line(aes(y = temp_10530652_Sur1, colour = "temp_10530652_Sur1_SE_15_m (°C)")) +
#  geom_line(aes(y = temp_10530652_Sur2, colour = "temp_10530652_Sur2_SE_15_m (°C)")) +
  #  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) 
  scale_y_continuous(limits = c(20, 30), breaks = c(20, 22.5, 25, 27.5, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  #scale_color_manual(values = c("red2")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  )

rpn_sea_water_temp.gg
  
###################################################
rpn_temp <- read_csv("rpn_IslaPascua_10530651.csv")

rpn_temp

rpn_temp.one <- 
  rpn_temp %>%
  filter(date >= '2014-10-09',
         date <= '2015-03-25') 
#  %>% 
#  dplyr::select() %>%
#  pivot_longer(!date, names_to = "metric", values_to = "value")

rpn_temp.one.gg <- ggplot(data = rpn_temp.one, aes(x = item, y = temp)) +
  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("red2")) +
                                # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_temp.one.gg

rpn_temp2 <- read_csv("rpn_IPblock_10530653_Norte.csv")

rpn_temp2

rpn_temp.two <- 
  rpn_temp2 %>%
  filter(date >= '2014-10-12',
         date <= '2016-01-06')

rpn_temp_10530653_Norte.main <-
  rpn_temp.two %>% 
  #as.Date(date,"%m/%d/%Y")
  #select(date, time) %>% 
  #mutate(date_main = make_datetime(date, time))
  #mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  #mutate(time = hm(time)) %>% 
  mutate_at(vars(time), funs(hour, minute)) %>%
  select(year, month, day, hour, minute, temp) %>% 
  mutate(date_time = make_datetime(year, month, day, hour, minute)) %>%
  rename(temp_10530653_Norte = temp)

rpn_temp.two.gg <- ggplot(data = rpn_temp.two, aes(x = item, y = temp)) +
  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("red2")) +
  # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_temp.two.gg

#####################################################

rpn_temp3 <- read_csv("rpn_IPblock_10530652_sur.csv")

rpn_temp3

rpn_temp.three <- 
  rpn_temp3 %>%
  filter(date >= '2014-10-12',
         date <= '2015-04-09')

rpn_temp_10530652_sur.main <-
  rpn_temp.three %>% 
  #as.Date(date,"%m/%d/%Y")
  #select(date, time) %>% 
  #mutate(date_main = make_datetime(date, time))
  #mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  #mutate(time = hm(time)) %>% 
  mutate_at(vars(time), funs(hour, minute)) %>%
  select(year, month, day, hour, minute, temp) %>% 
  mutate(date_time = make_datetime(year, month, day, hour, minute)) %>%
  rename(temp_10530652_sur = temp)

rpn_temp.three.gg <- ggplot(data = rpn_temp.three, aes(x = item, y = temp)) +
  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("red2")) +
  # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_temp.three.gg

######################################################

rpn_temp4 <- read_csv("rpn_IPblock_10530652_Sur2.csv")

rpn_temp4

rpn_temp.four <- 
  rpn_temp4 %>%
  filter(date >= '2015-04-12',
         date <= '2016-05-02')

rpn_temp_10530652_Sur2.main <-
  rpn_temp.four %>% 
  #as.Date(date,"%m/%d/%Y")
  #select(date, time) %>% 
  #mutate(date_main = make_datetime(date, time))
  #mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  #mutate(time = hm(time)) %>% 
  mutate_at(vars(time), funs(hour, minute)) %>%
  select(year, month, day, hour, minute, temp) %>% 
  mutate(date_time = make_datetime(year, month, day, hour, minute)) %>%
  rename(temp_10530652_Sur2 = temp)

rpn_temp.four.gg <- ggplot(data = rpn_temp.four, aes(x = item, y = temp)) +
  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("red2")) +
  # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_temp.four.gg

##

# rpn_temp4 <- read_csv("rpn_IPblock_10530652_Sur2.csv")
# 
# rpn_temp4
# 
# rpn_temp.four <- 
#   rpn_temp4 %>%
#   filter(date >= '2015-04-12',
#          date <= '2016-05-02')
# 
# rpn_temp.four.gg <- ggplot(data = rpn_temp.four, aes(x = item, y = temp)) +
#   geom_line(aes(color = "Sea Temp (°C)")) +
#   #  ylim(0, 30) +
#   #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
#   #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
#   scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
#   #scale_x_continuous(breaks = seq(0, 336, 24)) +
#   #scale_x_date(date_labels = "%b %y") +
#   scale_color_manual(values = c("red2")) +
#   # , "blue"))+
#   labs(x = NULL) +
#   theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
#     #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
#     panel.background = element_rect(fill = "#FFFFFF", color = NA),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.y = element_line(color = "#b2b2b2"),
#     #panel.spacing.x = unit(1, "cm"),
#     #panel.spacing.y = unit(0.5, "cm"),
#     #panel.spacing = unit(1, "lines"),
#     axis.ticks.x = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = 'right',
#     #plot.title = element_text(size = 11),
#     axis.text.y = element_text(size = 10),
#     #axis.text.x = element_text(size = 12),
#     axis.text.x = element_blank(),
#     axis.title.y = element_blank(),
#     legend.title = element_blank())
# 
# rpn_temp.four.gg

rpn_temp5 <- read_csv("rpn_IPblock_10530652_Sur1.csv")

rpn_temp5

rpn_temp.five <- 
  rpn_temp5 %>%
  filter(date >= '2014-10-17',
         date <= '2015-05-09')

rpn_temp_10530652_Sur1.main <-
  rpn_temp.five %>% 
  #as.Date(date,"%m/%d/%Y")
  #select(date, time) %>% 
  #mutate(date_main = make_datetime(date, time))
  #mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  #mutate(time = hm(time)) %>% 
  mutate_at(vars(time), funs(hour, minute)) %>%
  select(year, month, day, hour, minute, temp) %>% 
  mutate(date_time = make_datetime(year, month, day, hour, minute)) %>%
  rename(temp_10530652_Sur1 = temp)

rpn_temp.five.gg <- ggplot(data = rpn_temp.five, aes(x = item, y = temp)) +
  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  #scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("red2")) +
  # , "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_temp.five.gg

# date <- c(2014-01-01:2016-12-31) %>%
# mutate_at(vars(date), date)



























