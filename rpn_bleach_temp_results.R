require(plyr)
require(tidyverse)
require(ggmisc)

#require(betareg)
require(lmtest)
require(glmmTMB)
#require(boot)
#require(emmeans)
#require(brms)
#require(mvtnorm)
#require(rstan)
#require(VGAM)
require(Rmisc)
require(car)
require(multcomp)
require(lme4)
#require(DHARMa)
require(glmmTMB)
require(sjPlot)
require(lmerTest)
require(nlme)

rpn_sst_2015_increasing <- 
  rpn_sea_water_temp.main %>% 
  filter(date_time >= ymd_hms('2015-01-01 00:00:00'),
         date_time <= ymd_hms('2015-03-20 23:45:00') 
  ) %>%
  select(date_time,
         temp_10530651, 
         temp_10530652_Sur1,
         temp_10530653_Norte
  ) %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  )

rpn_sst_2015_increasing.stats <- 
  rpn_sst_2015_increasing %>%
  pivot_longer(!date_time,
    names_to = 'site', 
    values_to = 'temp'
    ) %>%
  mutate_at(vars(site), factor)

rpn_sst_2015_increasing.stats2 <- 
  rpn_sst_2015_increasing.stats %>% 
  mutate_at(vars(date_time), as.character)

rpn_sst_2015_increasing.summary <-
  rpn_sst_2015_increasing.stats %>%
  group_by(site) %>%
  dplyr::summarize(mean = mean(temp), 
                 sd = sd(temp), 
                 n = n(),
                 se = sd/sqrt(n)
) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

# Linear model

#model_1.lm <- lm(temp ~ site + date_time, 
#               data = rpn_sst_2015_increasing.stats2)

# summary(model_1.lm)
# par(mfrow = c(2,2))
# plot(model_1.lm)

model_1.lme <- lme(temp ~ site, 
                   random = ~ 1|date_time, 
                   data = rpn_sst_2015_increasing.stats2)

# Sample data

# data(Arabidopsis)
# Arabidopsis

# GLM

# model_1.gls <- gls(temp ~ site + date_time, 
#                    data = rpn_sst_2015_increasing.stats2,
#            method = "ML")

# summary(model_1.gls)

# Overview of the variables

# par(mfrow = c(1,1))

ggplot(rpn_sst_2015_increasing.stats, 
       aes(x = temp, fill = site, color = site)
        ) +
  geom_histogram(alpha = 0.5, position = "identity")

VarCorr(model_1.lme)

summary(model_1.lme)
anova(model_1.lme)

par(mfrow = c(2, 2))

plot(model_1.lme)
qqnorm(model_1.lme)
qqplot(model_1.lme)

# model_1.lmer <- lmer(temp ~ site + (1|date_time), data = rpn_sst_2015_increasing.stats)
# summary(model_1.lmer)

# par(mfrow = c(2, 2))

# plot(model_1.lmer)

#qqPlot(model_1.lmer)

#temp$model_1.lmer

#resid 

d <- density(residuals)
plot(d, main = 'Residual KDE Plot',
     xlab = 'Residual value')

temp <- model_1.lmer@frame[["temp"]]
residuals <- model_1.lmer@u

rpn_temp_fitted <- 
  tibble(
    x = residuals,
    y = temp
  )
  
rpn_fitted.gg <- ggplot(data = model_1.lmer, aes(x = temp, y = u, color = site)
) +
  geom_point() +
#  geom_point(aes(y = temp_10530651, colour = "temp_10530651 (°C)")) +
#  geom_line(aes(y = temp_10530653_Norte, colour = "temp_10530653_Norte (°C)")) +
#  geom_line(aes(y = temp_10530652_sur, colour = "temp_10530652_sur (°C)")) +
#  geom_line(aes(y = temp_10530652_Sur1, colour = "temp_10530652_Sur1 (°C)")) +
#  geom_line(aes(y = temp_10530652_Sur2, colour = "temp_10530652_Sur2 (°C)")) +
  #  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  #scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
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

rpn_sea_water_temp.gg <- ggplot(data = rpn_sst_2015_increasing.stats, aes(x = date_time)
) +
  geom_line(aes(y = temp, colour = site)
            ) +
  # geom_line(aes(y = temp_10530651, colour = "temp_10530651 (°C)")) +
  # geom_line(aes(y = temp_10530653_Norte, colour = "temp_10530653_Norte (°C)")) +
  # geom_line(aes(y = temp_10530652_sur, colour = "temp_10530652_sur (°C)")) +
  # geom_line(aes(y = temp_10530652_Sur1, colour = "temp_10530652_Sur1 (°C)")) +
  # geom_line(aes(y = temp_10530652_Sur2, colour = "temp_10530652_Sur2 (°C)")) +
  #  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(20, 30), breaks = c(20, 22.5, 25, 27.5, 30)) +
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

rpn_sea_water_temp.main.gg <- ggplot(data = rpn_sea_water_temp.main, aes(x = date_time)
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

rpn_sea_water_temp.main.gg

rpn_sst_2015_increasing.stats

rpn_sst_se_25_2015.main <-
  rpn_sst_2015_increasing.stats %>%
  filter(site == 'se_25') 

rpn_sst_se_15_2015.main <-
  rpn_sst_2015_increasing.stats %>%
  filter(site == 'se_15') 

rpn_sst_n_15_2015.main <-
  rpn_sst_2015_increasing.stats %>%
  filter(site == 'n_15')

#  %>%
#  group_by(date_time) %>%
#  dplyr::summarize(mean = mean(temp), 
#                   sd = sd(temp), 
#                   n = n(),
#                   se = sd/sqrt(n)
#  ) %>%
#  mutate(se = sd / sqrt(n),
#         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
#         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

rpn_sst_2015_increasing.stats

rpn_sst_se_25_2015.summary <-
  rpn_sst_2015_increasing.stats %>%
  filter(site == 'se_25') %>%
  group_by(date_time) %>%
  dplyr::summarize(mean = mean(temp), 
                   sd = sd(temp), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

rpn_sst_se_25_2015.day <-
  rpn_sst_2015_increasing.stats %>%
  mutate(year = format(date_time, '%Y'),
       month = format(date_time, '%m'), 
       day = format(date_time, '%d')
       ) %>%
  group_by(month, year, day) %>%
  summarise(total = mean(temp))

rpn_sst_se_25_2015.month <-
  rpn_sst_2015_increasing.stats %>%
  mutate(year = format(date_time, '%Y'),
         month = format(date_time, '%m'), 
         day = format(date_time, '%d')
  ) %>%
  group_by(year, month) %>%
  summarise(total = mean(temp))

mean(25.5, 25.8)

mmm_max <- 25.5

# Fit the data

se_25.lm <- lm(temp ~ poly(date_time, 2, raw = TRUE), data = rpn_sst_se_25_2015.main)

summary(se_25.lm)

se_25.lm_fitted <- se_25.lm$fitted.values
se_25.date_time <- rpn_sst_se_25_2015.main$date_time

# se_25.lm2 <- lm(temp ~ date_time + I(date_time^2), data = rpn_sst_se_25_2015.main)

rpn_sst_se_25_2015.gg <- ggplot(data = rpn_sst_se_25_2015.main, aes(x = date_time, y = temp)
      ) +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

rpn_sst_se_25_2015.gg

# Polynomial 

se_25.lm_fitted.main <-
  tibble(
    x = se_25.date_time,
    y = se_25.lm_fitted
  )

rpn_sst_se_25_2015.gg <- ggplot(data = se_25.lm_fitted.main, aes(x = x, y = y)
) +
  geom_line() 

# Raw data

rpn_sst_se_15_2015.gg <- ggplot(data = rpn_sst_se_15_2015.main, aes(x = date_time, y = temp)
) +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
  
rpn_sst_se_15_2015.gg

# Polynomial

se_15.lm <- lm(temp ~ poly(date_time, 2, raw = TRUE), data = rpn_sst_se_15_2015.main)

summary(se_15.lm)

se_15.lm_fitted <- se_15.lm$fitted.values
se_15.date_time <- rpn_sst_se_15_2015.main$date_time

se_15.lm_fitted.main <-
  tibble(
    x = se_15.date_time,
    y = se_15.lm_fitted
  )

rpn_sst_se_15_2015.gg <- ggplot(data = se_15.lm_fitted.main, aes(x = x, y = y)
              ) +
  geom_line() 

rpn_sst_se_15_2015.gg

# 
# North 15 m
# 

rpn_sst_n_15_2015.gg <- ggplot(data = rpn_sst_n_15_2015.main, aes(x = date_time, y = temp)
) +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

rpn_sst_n_15_2015.gg
  
n_15.lm <- lm(temp ~ poly(date_time, 2, raw = TRUE), data = rpn_sst_n_15_2015.main)

summary(n_15.lm)

n_15.lm_fitted <- n_15.lm$fitted.values
n_15.date_time <- rpn_sst_n_15_2015.main$date_time

n_15.lm_fitted.main <-
  tibble(
    x = n_15.date_time,
    y = n_15.lm_fitted
  )

rpn_sst_n_15_2015.gg <- ggplot(data = n_15.lm_fitted.main, aes(x = x, y = y)
) +
  geom_line() 

rpn_sst_n_15_2015.gg

rpn_temp.lm_fitted.main <-
  tibble(
    x = n_15.date_time,
    y1 = se_25.lm_fitted,
    y2 = se_15.lm_fitted,
    y3 = n_15.lm_fitted,
  )

rpn_temp_fitted.main.gg <- ggplot(data = rpn_temp.lm_fitted.main, aes(x = x)
) +
  geom_line(aes(y = y1, colour = "se_25")) +
  geom_line(aes(y = y2, colour = "se_15")) +
  geom_line(aes(y = y3, colour = "n_15")) +
  #geom_line(aes(y = temp_10530652_Sur1, colour = "temp_10530652_Sur1 (°C)")) +
  #geom_line(aes(y = temp_10530652_Sur2, colour = "temp_10530652_Sur2 (°C)")) +
  #  geom_line(aes(color = "Sea Temp (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(22, 27), breaks = seq(22, 27, by = 1)) +
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

rpn_temp_fitted.main.gg

anova(se_25.lm)

# March 2015, 20 days

rpn_sst_2015_march <- 
  rpn_sea_water_temp.main %>% 
  filter(date_time >= ymd_hms('2015-03-01 00:00:00'),
         date_time <= ymd_hms('2015-03-20 23:45:00') 
  ) %>%
select(date_time,
       temp_10530651, 
       temp_10530652_Sur1,
       temp_10530653_Norte
) %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  )

rpn_sst_2015_march.stats <- 
  rpn_sst_2015_march %>%
  pivot_longer(!date_time,
               names_to = 'site', 
               values_to = 'temp'
  ) %>%
  mutate_at(vars(site), factor)

#rpn_sst_2015_increasing.stats2 <- 
#  rpn_sst_2015_increasing.stats %>% 
#  mutate_at(vars(date_time), as.character)

rpn_sst_2015_march.summary <-
  rpn_sst_2015_march.stats %>%
  group_by(site) %>%
  dplyr::summarize(mean = mean(temp), 
                   sd = sd(temp), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 

rpn_sst_2014_15_3months <- 
  rpn_sea_water_temp.main %>% 
  filter(date_time >= ymd_hms('2014-12-01 00:00:00'),
         date_time <= ymd_hms('2015-03-01 23:45:00') 
  ) %>%
  select(date_time,
         temp_10530651, 
         temp_10530652_Sur1,
         temp_10530653_Norte
  )

date1 <- as.POSIXct('2015-03-01')
date2 <- as.POSIXct('2014-12-01')

date1 - date2

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
  
rpn_sst_2014_15_3months.stats <- 
  rpn_sst_2014_15_3months %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  )

rpn_sst_2014_15_3months.gg <- 
  rpn_sst_2014_15_3months %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  ) %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = se_25, colour = "se_25")) +
  geom_line(aes(y = se_15, colour = "se_15")) +
  geom_line(aes(y = n_15, colour = "n_15")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(20, 27), breaks = seq(20, 27, by = 1)) +
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

rpn_sst_2014_15_3months.gg

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
  mutate_at(vars(site), factor)

rpn_sst_2014_15_3months.stats2 <- 
  rpn_sst_2014_15_3months.stats %>%
  mutate(year = format(date_time, '%Y'),
         month = format(date_time, '%m'), 
         day = format(date_time, '%d')
  ) %>%
  group_by(site, year, month, day) %>%
  summarise(temp_daily_mean = mean(temp)) %>%
  mutate_at(vars(year, month, day), as.character) %>%
# select(year, month, day) %>% 
  mutate(date = make_date(year, month, day))

rpn_sst_2014_15_3months.stats3 <-
  rpn_sst_2014_15_3months.stats2 %>%
  select(site, temp_daily_mean) %>%
  mutate(temp_category = case_when(
                           temp_daily_mean <= 20.9 ~ '20 - 20.9',
                           temp_daily_mean <= 21.9 ~ '21 - 21.9',
                           temp_daily_mean <= 22.9 ~ '22 - 22.9',
                           temp_daily_mean <= 23.9 ~ '23 - 23.9',
                           temp_daily_mean <= 24.9 ~ '24 - 24.9',
                           temp_daily_mean <= 25.9 ~ '25 - 25.9',
                           temp_daily_mean <= 26.9 ~ '26 - 26.9',
                           temp_daily_mean <= 27.9 ~ '27 - 27.9',
                           )
         ) %>% 
  group_by(site, temp_category) %>%
  dplyr::summarize(n = n()) %>%
  mutate_at(vars(temp_category), factor)


model_1.lme <- lme(temp ~ site, 
                   random = ~ 1|date_time, 
                   data = rpn_sst_2014_15_3months.stats)

summary(model_1.lme)

rpn_sst_2014_15_daily.gg <- 
  rpn_sst_2014_15_3months %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  ) %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = se_25, colour = "se_25")) +
  geom_line(aes(y = se_15, colour = "se_15")) +
  geom_line(aes(y = n_15, colour = "n_15")) +
  #geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(20, 27), breaks = seq(20, 27, by = 1)) +
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

rpn_sst_2014_15_daily.gg

mu <- ddply(rpn_sst_2014_15_3months.stats3, 
            "site", 
            summarise, 
            grp.mean = mean(temp_daily_mean))

head(mu)

rpn_sst_daily.gg <- ggplot(data = rpn_sst_2014_15_3months.stats3, 
                                aes(x = temp_category,
                                    y = n,
                                    fill = temp_category
                                  #  , 
                                  #  color = temp_category
                                    )
                                ) +
     geom_bar(stat = "identity", alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
# Add mean lines
# geom_vline(data = mu, aes(xintercept = grp.mean, color = site),
#             linetype = "dashed") +
facet_grid( ~ site) +
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
rpn_sst_daily.gg

rpn_sst_2014_15_daily2.gg <- 
  rpn_sst_2014_15_3months.stats2 %>%
#  rename(
#    se_25 = temp_10530651, 
#    se_15 = temp_10530652_Sur1,
#    n_15  = temp_10530653_Norte
#  ) %>%
  ggplot() +
#  geom_line(aes(y = se_25, colour = "se_25")) +
#  geom_line(aes(y = se_15, colour = "se_15")) +
#  geom_line(aes(y = n_15, colour = "n_15")) +
   geom_line(aes(y = temp_daily_mean, x = date, color = site)) +
  scale_y_continuous(limits = c(20, 27), breaks = seq(20, 27, by = 1)) +
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

rpn_sst_2014_15_daily2.gg

model_2.lme <- lme(temp_daily_mean ~ site, 
                   random = ~ 1|date, 
                   data = rpn_sst_2014_15_3months.stats2)

summary(model_2.lme)

rpn_sst_n_15.lm <- 
  rpn_sst_2014_15_3months.stats2 %>%
  filter(site == 'n_15') 

rpn_sst_se_15.lm <- 
  rpn_sst_2014_15_3months.stats2 %>%
  filter(site == 'se_15') 

rpn_sst_se_25.lm <- 
  rpn_sst_2014_15_3months.stats2 %>%
  filter(site == 'se_25') 

#  %>%
#  mutate(obs = 1:n() %>%
#  mutate_at(vars(obs), character))
           
# %>%
#  add_column(day = c())

# North 15 m
rpn_n_15_model1.lm <- lm(temp_daily_mean ~ date,
                         data = rpn_sst_n_15.lm)
summary(rpn_n_15_model1.lm)

# SE 15 m

rpn_se_15_model1.lm <- lm(temp_daily_mean ~ date,
                         data = rpn_sst_se_15.lm)
summary(rpn_se_15_model1.lm)

# SE 25 m

rpn_se_25_model1.lm <- lm(temp_daily_mean ~ date,
                         data = rpn_sst_se_25.lm)
summary(rpn_se_25_model1.lm)

# rpn_n_15_model1.lm <- lm(temp_daily_mean ~ obs,
#                   data = rpn_sst_n_15.lm)

# summary(rpn_n_15_model1.lm)

slope <- function(x, y) cov(x, y) / var(x)
intercept <- function(x, y) mean(y) - slope(x, y) * mean(x)

rpn_sst_dec14_mar15 <- 
  rpn_sea_water_temp.main %>% 
  filter(date_time >= ymd_hms('2014-12-01 00:00:00'),
         date_time <= ymd_hms('2015-03-20 23:45:00') 
  ) %>%
  select(date_time,
         temp_10530651, 
         temp_10530652_Sur1,
         temp_10530653_Norte
  ) %>%
  rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  )

rpn_sst_dec14_mar15_increasing.stats <- 
  rpn_sst_dec14_mar15 %>%
  pivot_longer(!date_time,
               names_to = 'site', 
               values_to = 'temp'
  ) %>%
  mutate_at(vars(site), factor)

rpn_sst_dec14_mar15.stats <- 
  rpn_sst_dec14_mar15_increasing.stats %>%
  mutate(year = format(date_time, '%Y'),
         month = format(date_time, '%m'), 
         day = format(date_time, '%d')
  ) %>%
  group_by(site, year, month, day) %>%
  summarise(temp_daily_mean = mean(temp)) %>%
  # mutate_at(vars(year, month, day), as.character) %>%
  # select(year, month, day) %>% 
  mutate(date = make_date(year, month, day))

rpn_sst_dec14_mar15.stats_hist <-
  rpn_sst_dec14_mar15.stats %>%
  select(site, temp_daily_mean) %>%
  mutate(temp_category = case_when(
    temp_daily_mean <= 20.9 ~ '20 - 20.9',
    temp_daily_mean <= 21.9 ~ '21 - 21.9',
    temp_daily_mean <= 22.9 ~ '22 - 22.9',
    temp_daily_mean <= 23.9 ~ '23 - 23.9',
    temp_daily_mean <= 24.9 ~ '24 - 24.9',
    temp_daily_mean <= 25.9 ~ '25 - 25.9',
    temp_daily_mean <= 26.9 ~ '26 - 26.9',
    temp_daily_mean <= 27.9 ~ '27 - 27.9',
  )
  ) %>% 
  group_by(site, temp_category) %>%
  dplyr::summarize(n = n()) %>%
  mutate_at(vars(temp_category), factor)

rpn_sst_dec14_mar15.gg <- ggplot(data = rpn_sst_dec14_mar15.stats_hist, 
                           aes(x = temp_category,
                               y = n,
                               fill = temp_category
                               #  , 
                               #  color = temp_category
                           )
) +
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_fill_brewer(palette = "YlOrRd") +
  # Add mean lines
  # geom_vline(data = mu, aes(xintercept = grp.mean, color = site),
  #             linetype = "dashed") +
  facet_grid( ~ site) +
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
rpn_sst_dec14_mar15.gg





























