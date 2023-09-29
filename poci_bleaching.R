require(tidyverse)
require(cowplot)
require(ggpubr)
theme_set(theme_cowplot())


#rpn_poci_bl.main <- data.frame(
#  location = rep(c('north', 'west', 'se'), each = 4, times = 1)) %>%
#  add_column(depth = rep(c('sh', 'dp'), each = 2, times = 3)) %>%
#  add_column(transect = rep(c('one', 'two'), each = 1, times = 6)) %>%
#  add_column(total_count = rep(c(0), times = 12)) %>% 
#  add_column(cover = rep(c(0), times = 12)) %>%
#  add_column(failures = rep(c(126), times = 12)) %>%
#  add_column(groups = rep(c('north.sh', 'north.dp',
#                            'west.sh',  'west.dp',
#                            'southeast.sh', 'southeast.dp'), 
#                          each = 2, times = 1) )

rpn_poci_bl.main 
rpn_poci_pb.main
rpn_poci_pale.main
rpn_poci_healthy.main

rpn_poci_bleach.main
rpn_poci_pb.main
rpn_poci_pale.main
rpn_poci_healthy.main

rpn_poci_bleach.summ  <-
  rpn_poci_bleach.main %>%
  group_by(location, depth) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  add_column(groups = rep(c('bl'), times = 6)) %>%
  mutate_at(vars(location, depth, groups), factor)

rpn_poci_pb.summ  <-
  rpn_poci_pb.main %>%
  group_by(location, depth) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  add_column(groups = rep(c('pb'), times = 6)) %>%
  mutate_at(vars(location, depth, groups), factor)

rpn_poci_pale.summ  <-
  rpn_poci_pale.main %>%
  group_by(location, depth) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  add_column(groups = rep(c('p'), times = 6)) %>%
  mutate_at(vars(location, depth, groups), factor)

rpn_poci_healthy.summ  <-
  rpn_poci_healthy.main %>%
  group_by(location, depth) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  add_column(groups = rep(c('h'), times = 6)) %>%
  mutate_at(vars(location, depth, groups), factor)

rpn_bleach_poci <-
  bind_rows(
    rpn_poci_bleach.summ,
    rpn_poci_pb.summ,
    rpn_poci_pale.summ,
    rpn_poci_healthy.summ)

rpn_bleach_poci

rpn_poci_bleach_summ.gg <- rpn_poci_bleach.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_poci_bleach_2015.ggbarplot <- ggplot(rpn_poci_bleach_summ.gg, aes(x = factor(coast, x_labels), 
                                                              y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1)) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + 
  #facet_grid(groups ~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  #facet_grid(~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Pocillopora"), " spp. - Bleached"))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_poci_bleach_2015.ggbarplot

#rpn_poci_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_poci_2015.ggbarplot)
leg1 <- as_ggplot(rpn_poci_2015.ggbarplot)

# Partially bleached

rpn_poci_pb_summ.gg <- rpn_poci_pb.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_poci_pb_2015.ggbarplot <- ggplot(rpn_poci_pb_summ.gg, aes(x = factor(coast, x_labels), 
                                                              y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1)) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + 
  #facet_grid(groups ~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  #facet_grid(~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Porites lobata"), " - Partially Bleached"))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_poci_pb_2015.ggbarplot

#rpn_poci_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_poci_2015.ggbarplot)
leg1 <- as_ggplot(rpn_poci_2015.ggbarplot)

################################################################################

# Pale

rpn_poci_pale_summ.gg <- rpn_poci_pale.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_poci_pale_2015.ggbarplot <- ggplot(rpn_poci_pale_summ.gg, aes(x = factor(coast, x_labels), 
                                                                  y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1)) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + 
  #facet_grid(groups ~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  #facet_grid(~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Porites lobata"), " - Pale"))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_poci_pale_2015.ggbarplot

#rpn_poci_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_poci_2015.ggbarplot)
leg1 <- as_ggplot(rpn_poci_2015.ggbarplot)

################################################################################

# Not bleached

rpn_poci_healthy_summ.gg <- rpn_poci_healthy.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_poci_healthy_2015.ggbarplot <- ggplot(rpn_poci_healthy_summ.gg, aes(x = factor(coast, x_labels), 
                                                                        y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1)) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + 
  #facet_grid(groups ~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  #facet_grid(~ depth2, margin = FALSE, labeller = labeller(depth2 = label_names)) +
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) +
  ggtitle(expression(paste(italic(" Porites lobata"), " - No bleach"))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_poci_healthy_2015.ggbarplot

#rpn_poci_summ.gg$coast = c('North', 'West', 'Southeast')
#leg1 <- get_legend(rpn_poci_2015.ggbarplot)
#leg1 <- as_ggplot(rpn_poci_2015.ggbarplot)

rpn_bleach_poci_summ.gg <-
  bind_rows(
    rpn_poci_bleach_summ.gg,
    rpn_poci_pb_summ.gg,
    rpn_poci_pale_summ.gg,
    rpn_poci_healthy_summ.gg)

rpn_bleach_poci_main.gg <- rpn_bleach_poci_summ.gg %>%
  add_column(bleach = rep(c("Bleached", "Partially Bleached", "Pale", "Not Bleached"), each = 6, times = 1)) %>%
  #add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(bleach = factor(bleach, levels = c("Bleached", "Partially Bleached", "Pale", "Not Bleached")))

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

bleach_labels = c("Bleached" = "Bleached", "Partially Bleached" = "Partially Bleached", 
                  "Pale" = "Pale", "Not Bleached" = "Not Bleached")

rpn_bleach_poci_main.ggbarplot <- ggplot(rpn_bleach_poci_main.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_grid(bleach ~ depth2, margin = FALSE) + 
  #facet_grid(group ~ depth2, margin = FALSE, labeller = labeller(group = group_labels)) +
  #ggtitle(expression(paste(italic(" Porites "), "spp."))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_bleach_poci_main.ggbarplot

################################################################################

## Pocillopora

rpn_bleach <- read.csv('rpn_bleach_2015.csv') %>%
  as_tibble() %>%
  mutate_at(vars(location, depth, transect, group, status), factor)

rpn_bleach

rpn_summary <- rpn_bleach %>%  
  group_by(location, depth, transect, group) %>%
  dplyr::summarise(total_count = n())

rpn_POCI <- rpn_bleach %>%  
  filter(group == "POCI") %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_POCI

rpn_poci <- read.csv('rpn_cover.csv') %>%
  as_tibble() %>%
  filter(group == "POCI") %>%
  mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/total_points,
    failures = total_points - total_count
  )

rpn_plob

## Pocillopora

### Generalized linear model

poci.glm <- glm(cbind(total_count, failures) ~ location * depth, 
                family = binomial(link = "logit"), 
                data = rpn_poci)

par(mfrow = c(2, 2))
plot(poci.glm)

summary(poci.glm)

#### https://www.rpubs.com/daharo_calpoly/502695

rpn_poci$groups <- interaction(rpn_poci$location, rpn_poci$depth)

## Create a post-hoc model

model_poci <- with(rpn_poci, glm(cbind(total_count, failures) ~ groups, family = binomial))

## Determine the post-hoc comparisons of interest

summary(glht(model_poci, 
             linfct = mcp(groups =
                            #Is the difference between these groups different from zero?
                            c("(north.sh) - (west.sh) = 0",
                              "(north.sh) - (west.dp) = 0",
                              "(north.sh) - (se.sh) = 0",
                              "(north.sh) - (se.dp) = 0",
                              "(north.dp) - (west.sh) = 0",
                              "(north.dp) - (west.dp) = 0",
                              "(north.dp) - (se.sh) = 0",
                              "(north.dp) - (se.dp) = 0",
                              "(se.sh) - (west.sh) = 0",
                              "(se.sh) - (west.dp) = 0",
                              "(se.dp) - (west.sh) = 0",
                              "(se.dp) - (west.dp) = 0"))),
        test = adjusted("holm"))

rpn_poci2 <- read.csv('rpn_cover.csv') %>%
  as_tibble() %>%
  filter(group == "POCI") %>%
  mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/total_points,
    failures = total_points - total_count
  ) %>%
  group_by(location, depth) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

rpn_poci2

rpn_poci3 <-
  as_tibble(rpn_poci2)

rpn_poci_summ <-
  rpn_poci3 %>%
  add_column(coast = rep(c('North', 'Southeast', 'West'), each = 2)) %>%
  add_column(depth2 = rep(c('15 m', '8 m'), times = 3)) %>%
  mutate_at(vars(coast, depth2), factor)

rpn_poci_summ

rpn_poci_summ.gg <- rpn_poci_summ %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

rpn_poci_summ.gg <- rpn_poci_summ %>%
  mutate(coast = factor(coast, levels = c('North', 'West', 'Southeast')),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

rpn_poci_summ.gg

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_poci_2015.ggbarplot <- ggplot(rpn_poci_summ.gg, aes(x = coast, y = mean, fill = coast)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1)

rpn_poci_2015.ggbarplot <- ggplot(rpn_poci_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  #ggtitle(expression(paste(italic(" Pocillopora "), "spp."))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
  #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
  labs(x = NULL) +
  theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
        strip.background = element_rect(fill = "#FFFFFF", color = NA),    
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2"),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing = unit(1, "lines"),
        axis.ticks = element_blank(),
        legend.position = 'none',
        #plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_poci_2015.ggbarplot

# Pocillopora Bleaching

rpn_poci_bleach <- rpn_bleach %>%  
  filter(group == 'POCI' & status == 'BL') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_poci_bleach

rpn_poci_bleach2 <-
  as.data.frame(rpn_poci_bleach) %>%
  add_row(location = 'north', depth = 'sh', transect = 'one', total_count = 0) %>%
  add_row(location = 'north', depth = 'sh', transect = 'two', total_count = 0) %>%
  add_row(location = 'north', depth = 'dp', transect = 'two', total_count = 0) %>%
  add_row(location = 'west', depth = 'sh', transect = 'one', total_count = 0) %>%
  add_row(location = 'west', depth = 'dp', transect = 'two', total_count = 0) %>%
  add_row(location = 'se', depth = 'sh', transect = 'two', total_count = 0)

rpn_poci_bleach2

rpn_poci_bleach.main <- rpn_poci_bleach2 %>%
  #as_tibble() %>%
  #filter(group == "PLOB") %>%
  #mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  )

rpn_poci_bleach.main 

rpn_poci_pb <- rpn_bleach %>%  
  filter(group == 'POCI' & status == 'PB') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_poci_pb

rpn_poci_pb2 <-
  as.data.frame(rpn_poci_pb) %>%
  add_row(location = 'north', depth = 'sh', transect = 'one', total_count = 0) %>%
  add_row(location = 'north', depth = 'sh', transect = 'two', total_count = 0) %>%
  add_row(location = 'west', depth = 'sh', transect = 'two', total_count = 0) %>%
  add_row(location = 'west', depth = 'dp', transect = 'one', total_count = 0)

rpn_poci_pb2

rpn_poci_pb.main <- rpn_poci_pb2 %>%
  #as_tibble() %>%
  #filter(group == "PLOB") %>%
  #mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  )

rpn_poci_pb.main 

rpn_poci_pale <- rpn_bleach %>%  
  filter(group == 'POCI' & status == 'P') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_poci_pale

rpn_poci_pale2 <-
  as.data.frame(rpn_poci_pale) %>%
  add_row(location = 'north', depth = 'sh', transect = 'one', total_count = 0) %>%
  add_row(location = 'north', depth = 'sh', transect = 'two', total_count = 0)

rpn_poci_pale2

rpn_poci_pale.main <- rpn_poci_pale2 %>%
  #as_tibble() %>%
  #filter(group == "PLOB") %>%
  #mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  )

rpn_poci_pale.main 

rpn_poci_healthy <- rpn_bleach %>%  
  filter(group == 'POCI' & status == 'H') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_poci_healthy

rpn_poci_healthy2 <-
  as.data.frame(rpn_poci_healthy) %>%
  add_row(location = 'north', depth = 'sh', transect = 'one', total_count = 0) %>%
  add_row(location = 'north', depth = 'dp', transect = 'two', total_count = 0) %>%
  add_row(location = 'se', depth = 'dp', transect = 'two', total_count = 0)

rpn_poci_healthy.main <- rpn_poci_healthy2 %>%
  #as_tibble() %>%
  #filter(group == "PLOB") %>%
  #mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  )

rpn_poci_healthy.main


































