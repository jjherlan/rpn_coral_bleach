require(tidyverse)
require(cowplot)
require(ggpubr)
theme_set(theme_cowplot())

# Porites lobata

rpn_plob_2015.ggbarplot <- ggplot(rpn_plob_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Porites "), "spp."))) +
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
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_plob_2015.ggbarplot

#rpn_plob_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_plob_2015.ggbarplot)
leg1 <- as_ggplot(rpn_plob_2015.ggbarplot)

rpn_poci_2015.ggbarplot <- ggplot(rpn_poci_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Pocillopora "), "spp."))) +
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
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_poci_2015.ggbarplot


# Macrolagae

rpn_ma_2015.ggbarplot <- ggplot(rpn_ma_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Macrolagae ")))) +
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
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_ma_2015.ggbarplot

# Turf

rpn_turf_2015.ggbarplot <- ggplot(rpn_turf_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Turf ")))) +
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
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_turf_2015.ggbarplot

# Sand

rpn_sand_2015.ggbarplot <- ggplot(rpn_sand_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Sand ")))) +
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
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_sand_2015.ggbarplot

# Bare substrate = calcium carbonate (dead coral) + basalt (lava)

rpn_bare_2015.ggbarplot <- ggplot(rpn_bare_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" Bare ")))) +
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
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_bare_2015.ggbarplot

# CCA

rpn_cca_2015.ggbarplot <- ggplot(rpn_cca_summ.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_wrap( ~ depth2, labeller = as_labeller(label_names), dir = "v", ncol = 1) + 
  ggtitle(expression(paste(italic(" CCA ")))) +
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
        plot.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_cca_2015.ggbarplot

rpn_plob_2015 <-
rpn_plob_summ.gg %>%
  add_column(group = rep(c('plob'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_poci_2015 <-
  rpn_poci_summ.gg %>%
  add_column(group = rep(c('poci'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_ma_2015 <-
  rpn_ma_summ.gg %>%
  add_column(group = rep(c('ma'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_turf_2015 <-
  rpn_turf_summ.gg %>%
  add_column(group = rep(c('turf'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_cca_2015 <-
  rpn_cca_summ.gg %>%
  add_column(group = rep(c('cca'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_bare_2015 <-
  rpn_bare_summ.gg %>%
  add_column(group = rep(c('bare'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_sand_2015 <-
  rpn_sand_summ.gg %>%
  add_column(group = rep(c('sand'), times = 6)) %>%
  mutate_at(vars(group), factor)

rpn_cover_groups <-
  bind_rows(
  rpn_plob_2015,
  rpn_poci_2015,
  rpn_ma_2015,
  rpn_turf_2015,
  rpn_cca_2015,
  rpn_bare_2015,
  rpn_sand_2015)

# par(mfrow = c(3, 4))

# plot_grid(
# 
# rpn_plob_2015.ggbarplot,
# rpn_poci_2015.ggbarplot,
# rpn_ma_2015.ggbarplot,
# rpn_turf_2015.ggbarplot,
# rpn_cca_2015.ggbarplot,
# rpn_bare_2015.ggbarplot,
# rpn_sand_2015.ggbarplot
# 
# #labels = c('A', 'B')
# 
# )

rpn_cover_groups.gg <-
  rpn_cover_groups %>%
  mutate(group_labels = case_when(
          group == "plob" ~ "PLOB", 
          group == "poci" ~ "POCI",
          group == "ma" ~ "MA",
          group == "turf" ~ "Turf",
          group == "cca" ~ "CCA",
          group == "bare" ~ "Bare",
          group == "sand" ~ "Sand")
    ) %>%
  mutate_at(vars(group_labels), factor) %>%
  add_column(
    cld = rep(c('a'), times = 42)) %>%
  mutate_at(vars(cld), factor)

label_names = c("PLOB" = "PLOB",
                "POCI" = "POCI",
                "MA" = "MA",
                "Turf" = "Turf",
                "CCA" = "CCA",
                "Bare" = "Bare",
                "Sand" = "Sand")

x_labels = c("North", "West", "Southeast")

rpn_cover_groups.gg$group_labels <- factor(rpn_cover_groups.gg$group_labels, 
                                           levels = c("PLOB", "POCI", "MA", "Turf", "CCA", "Bare", "Sand"))

rpn_2015.ggbarplot <- ggplot(rpn_cover_groups.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  facet_grid(group_labels ~ depth2, margin = FALSE) + #, labeller = labeller(group_labels = label_names)) + 
  #ggtitle(expression(paste(italic(" Porites "), "spp."))) +
  #geom_text(aes(label = cld, y = upper.ci), vjust = -0.25, size = 2) +
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

rpn_2015.ggbarplot

# Bleaching Plots

rpn_plob_bl.main <- data.frame(
  location = rep(c('north', 'west', 'se'), each = 4, times = 1)) %>%
  add_column(depth = rep(c('sh', 'dp'), each = 2, times = 3)) %>%
  add_column(transect = rep(c('one', 'two'), each = 1, times = 6)) %>%
  add_column(total_count = rep(c(0), times = 12)) %>% 
  add_column(cover = rep(c(0), times = 12)) %>%
  add_column(failures = rep(c(126), times = 12)) %>%
  add_column(groups = rep(c('north.sh', 'north.dp',
                        'west.sh',  'west.dp',
                        'southeast.sh', 'southeast.dp'), 
             each = 2, times = 1)
             
  ) %>%
  as_tibble()

rpn_plob_bl.main 
rpn_plob_pb.main
rpn_plob_pale.main
rpn_plob_healthy.main

rpn_plob_bl.summ  <-
  rpn_plob_bl.main %>%
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

rpn_plob_pb.summ  <-
  rpn_plob_pb.main %>%
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

rpn_plob_pale.summ  <-
  rpn_plob_pale.main %>%
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

rpn_plob_healthy.summ  <-
  rpn_plob_healthy.main %>%
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

rpn_bleach_plob <-
  bind_rows(
    rpn_plob_bl.summ,
    rpn_plob_pb.summ,
    rpn_plob_pale.summ,
    rpn_plob_healthy.summ)

rpn_bleach_plob

rpn_plob_bl_summ.gg <- rpn_plob_bl.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_plob_bl_2015.ggbarplot <- ggplot(rpn_plob_bl_summ.gg, aes(x = factor(coast, x_labels), 
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
  ggtitle(expression(paste(italic(" Porites lobata"), " - Bleached"))) +
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

rpn_plob_bl_2015.ggbarplot

#rpn_plob_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_plob_2015.ggbarplot)
leg1 <- as_ggplot(rpn_plob_2015.ggbarplot)

# Partially bleached

rpn_plob_pb_summ.gg <- rpn_plob_pb.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_plob_pb_2015.ggbarplot <- ggplot(rpn_plob_pb_summ.gg, aes(x = factor(coast, x_labels), 
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
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_plob_pb_2015.ggbarplot

#rpn_plob_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_plob_2015.ggbarplot)
leg1 <- as_ggplot(rpn_plob_2015.ggbarplot)

################################################################################

# Pale

rpn_plob_pale_summ.gg <- rpn_plob_pale.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_plob_pale_2015.ggbarplot <- ggplot(rpn_plob_pale_summ.gg, aes(x = factor(coast, x_labels), 
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
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_plob_pale_2015.ggbarplot

#rpn_plob_summ.gg$coast = c('North', 'West', 'Southeast')
leg1 <- get_legend(rpn_plob_2015.ggbarplot)
leg1 <- as_ggplot(rpn_plob_2015.ggbarplot)

################################################################################

# Not bleached

rpn_plob_healthy_summ.gg <- rpn_plob_healthy.summ %>%
  add_column(coast = rep(c("North", "Southeast", "West"), each = 2, times = 1)) %>%
  add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(coast = factor(coast, levels = c("North", "West", "Southeast")),
         depth2 = factor(depth2, levels = c('8 m', '15 m')
         )
  )

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

rpn_plob_healthy_2015.ggbarplot <- ggplot(rpn_plob_healthy_summ.gg, aes(x = factor(coast, x_labels), 
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
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_plob_healthy_2015.ggbarplot

#rpn_plob_summ.gg$coast = c('North', 'West', 'Southeast')
#leg1 <- get_legend(rpn_plob_2015.ggbarplot)
#leg1 <- as_ggplot(rpn_plob_2015.ggbarplot)

rpn_bleach_plob_summ.gg <-
  bind_rows(
    rpn_plob_bl_summ.gg,
    rpn_plob_pb_summ.gg,
    rpn_plob_pale_summ.gg,
    rpn_plob_healthy_summ.gg)

rpn_bleach_plob_main.gg <- rpn_bleach_plob_summ.gg %>%
  add_column(bleach = rep(c("Bleached", "Partially Bleached", "Pale", "Not Bleached"), each = 6, times = 1)) %>%
  #add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(bleach = factor(bleach, levels = c("Bleached", "Partially Bleached", "Pale", "Not Bleached")))

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

bleach_labels = c("Bleached" = "Bleached", "Partially Bleached" = "Partially Bleached", 
                  "Pale" = "Pale", "Not Bleached" = "Not Bleached")

rpn_bleach_plob_main.ggbarplot <- ggplot(rpn_bleach_plob_main.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Proportion of Counts (%)")), limits = c(0, 1.0), 
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

rpn_bleach_plob_main.ggbarplot
#labeller = labeller(bleach = bleach_labels)

rpn_bleach_plob_summ.gg <-
  bind_rows(
    #rpn_plob_bl_summ.gg,
    rpn_plob_pb_summ.gg,
    rpn_plob_pale_summ.gg,
    rpn_plob_healthy_summ.gg)

rpn_bleach_plob_main.gg <- rpn_bleach_plob_summ.gg %>%
  add_column(bleach = rep(c(#"Bleached", 
                            "Partially Bleached", "Pale", "Not Bleached"), each = 6, times = 1)) %>%
  #add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(bleach = factor(bleach, levels = c(#"Bleached", 
                                            "Partially Bleached", "Pale", "Not Bleached")))

rpn_bleach_plob_main.ggbarplot <- ggplot(rpn_bleach_plob_main.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Proportion of Counts (%)")), limits = c(0, 1.0), 
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

rpn_bleach_plob_main.ggbarplot
#labeller = labeller(bleach = bleach_labels)
################################################################################

# plot_grid(
#   
#   rpn_plob_2015.ggbarplot,
#   rpn_poci_2015.ggbarplot,
#   rpn_ma_2015.ggbarplot,
#   rpn_turf_2015.ggbarplot,
#   rpn_cca_2015.ggbarplot,
#   rpn_bare_2015.ggbarplot,
#   rpn_sand_2015.ggbarplot
#   
#   #labels = c('A', 'B')
#   
# )
# 
# group_labels = c("plob" = "PLOB", 
#                  "poci" = "POCI",
#                  "ma" = "MA",
#                  "turf" = "Turf",
#                  "cca" = "CCA",
#                  "bare" = "Bare",
#                  "sand" = "Sand")
# 
# rpn_2015.ggbarplot <- ggplot(rpn_cover_groups, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
#   geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
#   geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
#   scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
#                      labels = function(x) paste0(x*100)) + 
#   scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
#   scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
#   facet_grid(group ~ depth2, margin = FALSE, labeller = labeller(group = group_labels)) + 
#   #ggtitle(expression(paste(italic(" Porites "), "spp."))) +
#   #geom_text(aes(label = cld, y = upper.ci), vjust = -0.5, size = 10) +
#   #scale_y_log10(expression(paste("Colony Size (", cm^2, ")"), limits = c(0, 100000))) +
#   labs(x = NULL) +
#   theme(strip.text = element_text(size = 10, color = "black", hjust = 0.50),
#         strip.background = element_rect(fill = "#FFFFFF", color = NA),    
#         panel.background = element_rect(fill = "#FFFFFF", color = NA),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_line(color = "#b2b2b2"),
#         panel.spacing.x = unit(1, "cm"),
#         panel.spacing.y = unit(0.5, "cm"),
#         panel.spacing = unit(1, "lines"),
#         axis.ticks = element_blank(),
#         legend.position = 'right',
#         plot.title = element_text(size = 11),
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_blank(),
#         axis.title.y = element_text(size = 14),
#         legend.title = element_blank())
# 
# rpn_2015.ggbarplot

# tmp6 <- data.frame(Month = c(1,1,2,2,3,3,4,4),
#                    Teams = c("A", "B", "A", "B", "A", "B", "A", "B"),
#                    Total = c(1000, 1200, 1150, 1220, 1300, 1030, 1060, 1380),
#                    Failed = c(6,4,7,8,2,6,9,4))
# tmp6


rpn_bleach_plob_main.gg <- rpn_bleach_plob_summ.gg %>%
  add_column(bleach = rep(c("Bleached", "Partially Bleached", "Pale", "Not Bleached"), each = 6, times = 1)) %>%
  #add_column(depth2 = rep(c("sh" = "8 m", "dp" = "15 m"), each = 1, times = 3)) %>%
  mutate(bleach = factor(bleach, levels = c("Bleached", "Partially Bleached", "Pale", "Not Bleached")))

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

bleach_labels = c("Bleached" = "Bleached", "Partially Bleached" = "Partially Bleached", 
                  "Pale" = "Pale", "Not Bleached" = "Not Bleached")

rpn_bleach_plob_main.ggbarplot <- ggplot(rpn_bleach_plob_main.gg, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Proportion of Counts (%)")), limits = c(0, 1.0), 
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

rpn_bleach_plob_main.ggbarplot

rpn_crw.main.gg <- ggplot(data = rpn_crw.main4, aes(x = date, y = sst_max)) +
  geom_line(aes(color = "SST Max (°C)")) +
  #  ylim(0, 30) +
  #  scale_fill_discrete(name = "Dose", labels = c("A", "B")) +
  geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  scale_color_manual(values = c("red2", "blue"))+
  labs(x = NULL) +
  theme(#strip.text = element_text(size = 10, color = "black", hjust = 0.50),
    #strip.background = element_rect(fill = "#FFFFFF", color = NA),    
    #panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#b2b2b2"),
    #panel.spacing.x = unit(1, "cm"),
    #panel.spacing.y = unit(0.5, "cm"),
    #panel.spacing = unit(1, "lines"),
    #axis.ticks.x = element_line(linewidth = 0),
    axis.ticks.y = element_blank(),
    legend.position = 'right',
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    #axis.text.x = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_crw.main.gg

















