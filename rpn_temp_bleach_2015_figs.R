require(plyr)
require(tidyverse)
library(ggpmisc)
require(ggtext)
require(glue)
require(ggthemes)
require(patchwork)

rpn_sst_2014_15_daily2.gg <- 
  rpn_sst_2014_15_3months.stats2 %>%
  ggplot(aes(y = temp_daily_mean, x = date, color = site)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(20, 27), breaks = seq(20, 27, by = 1)) +
  scale_x_date(limits = as.Date(c("2014-12-01", "2015-03-01")), 
                            date_breaks = "1 month", 
                            date_labels = "%b %Y") +
  scale_color_manual(
    breaks = c("n_15", "se_15", "se_25"),
    labels = c("North 15 m", "Southeast 15 m", "Southeast 25 m"),
    values = c("red", "blue", "purple")
                                        ) +
    labs(
#      title = "Temperatures\n", 
#         x = "TY [°C]", 
         y = "Temperature °C" 
#         color = "Legend Title\n"
                                        ) +
  #  scale_fill_brewer(palette = "Set1") +
  stat_poly_line(linewidth = 0.5,
                 se = FALSE) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
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
    legend.text = element_text(size = 12),
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 15, vjust = 1.25, 
                               hjust = 0.5, size = 14),
    axis.title.y = element_text(size = 16 
                                #hjust = 1.0 
                                #vjust = 1.0
                                ),
    legend.title = element_blank()
  )

rpn_sst_2014_15_daily2.gg

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


# Histogram

rpn_sst_dec14_mar15.stats_recode_hist <-
  rpn_sst_dec14_mar15.stats_hist %>%
  mutate(site2 = recode(site,
                        n_15 = 'North 15 m',
                        se_15 = 'Southeast 15 m',
                        se_25 = 'Southeast 25 m'
                        )
         )
  

rpn_sst_dec14_mar15.gg <- ggplot(data = rpn_sst_dec14_mar15.stats_recode_hist, 
                                 aes(x = temp_category,
                                     y = n,
                                     fill = temp_category
                                     #  , 
                                     #  color = temp_category
                                 )
) +
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    #      title = "Temperatures\n", 
    y = "Number of Days", 
#    x = "Temperature °C",
    fill = "Temperature °C\n"
  ) +
  # Add mean lines
  # geom_vline(data = mu, aes(xintercept = grp.mean, color = site),
  #             linetype = "dashed") +
  facet_grid( ~ site2) +
  theme(strip.text = element_text(size = 14, color = "black", hjust = 0.50),
    strip.background = element_rect(fill = "#FFFFFF", color = NA),    
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
    legend.text = element_text(size = 14),
    #plot.title = element_text(size = 11),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_blank(), 
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 14)
  )

rpn_sst_dec14_mar15.gg

# Seawater Temperature December 1, 2014 to March 20, 2015

rpn_sst_2014_2015_increasing <- 
  rpn_sea_water_temp.main %>% 
  filter(date_time >= ymd_hms('2014-12-01 00:00:00'),
         date_time <= ymd_hms('2015-03-20 23:45:00') 
  ) %>%
  dplyr::select(date_time,
         temp_10530651, 
         temp_10530652_Sur1,
         temp_10530653_Norte
  ) %>%
  dplyr::rename(
    se_25 = temp_10530651, 
    se_15 = temp_10530652_Sur1,
    n_15  = temp_10530653_Norte
  )

rpn_sst_2014_2015_increasing.stats <- 
  rpn_sst_2014_2015_increasing %>%
  pivot_longer(!date_time,
               names_to = 'site', 
               values_to = 'temp'
  ) %>%
  mutate_at(vars(site), factor) %>%
  mutate(site2 = recode(site,
                        n_15 = 'North 15 m',
                        se_15 = 'Southeast 15 m',
                        se_25 = 'Southeast 25 m')
  ) 

#%>%
#mutate_at(vars(date_time), as.character)
  

rpn_sea_water_temp_main.gg <- ggplot(data = rpn_sst_2014_2015_increasing.stats, 
                                     aes(x = date_time)
) +
  geom_line(aes(y = temp, color = site2)
  ) +
  scale_y_continuous(limits = c(20, 30), breaks = c(20, 22.5, 25, 27.5, 30)) +
#  scale_x_date(#limits = as.Date(c("2014-12-01 00:00:00", "2015-03-01 23:45:00"))) + 
               #date_breaks = "1 month", 
#               date_labels = "%b %Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    #      title = "Temperatures\n", 
    y = "Temperature °C", 
    #    x = "Temperature °C",
    # color = "Study Site"
  ) +
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
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(angle = 0.0, vjust = 1.75, 
                               hjust = 0.5, size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    #plot.title = element_text(size = 11),
  )

rpn_sea_water_temp_main.gg


coral_labels <- c('PLOB' = "Porites", 
                  'POCI' = "Pocillopora"
)

coral_labels

x_labels = c('ana' = "North", 
             'man' = "West", 
             'se' = "Southeast")

x_labels

depth_labels = c('sh' = "8 m", 'dp' = "15 m")

depth_labels

bleach_labels = c("Bleached", 
                  "Partially bleached", 
                  "Pale", 
                  "Not bleached")

bleach_labels = c('BL' = "Bleached", 
                  'PB' = "Partially bleached", 
                  'P' = "Pale", 
                  'H' = "Not bleached")

bleach_labels

hcl_palettes(plot = TRUE)

# New facet label names for dose variable
depth.labs <- c('sh', 'dp')
names(depth.labs) <- c('Shallow', 'Deep')

# New facet label names for supp variable
species.labs <- c('PLOB', 'POCI')
names(species.labs) <- c('Porites', 'Pocillopora')

rpn_bleach_prev_main.barplot$species2 <- factor(rpn_bleach_prev_main.barplot$species, labels = c('Porites', 'Pocillopora'))

rpn_bleach_prev_main.barplot$species2 <- factor(rpn_bleach_prev_main.barplot$species, 
                                                labels = expression(paste(c(italic('Porites'), ('Pocillopora')))))

rpn_bleach_prev_main.barplot$depth2 <- factor(rpn_bleach_prev_main.barplot$depth, labels = c('Shallow', 'Deep'))

plot_labeller <- function(variable,value){
  if (variable=='facet1') {
    return(facet1_names[value])
  } else if (variable=='facet2') {
    return(facet2_names[value])
  } else {
    return(as.character(value))
  }
}

levels(names) <- c("italic('Porites')")

hospital_names <- list(
  'Hospital#1'="Some Hospital",
  'Hospital#2'="Another Hospital",
  'Hospital#3'="Hospital Number 3",
  'Hospital#4'="The Other Hospital"
)

hospital_labeller <- function(variable,value){
  return(hospital_names[value])
}

ggplot(survey,aes(x=age)) + stat_bin(aes(n=nrow(h3),y=..count../n), binwidth=10)
+ facet_grid(hospital ~ ., labeller = hospital_labeller)

rpn_bleach.ggbarplot <-
  rpn_bleach_prev_main.barplot %>%
  #  mutate(across(species, factor, levels = c('PLOB', 'POCI')),
  #       across(depth, factor, levels = c('sh', 'dp'))) %>%
  ggplot(aes(x = location, y = prev, fill = bleach)) +   
  geom_bar(position = "stack", stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_y_continuous(expression(paste("Bleaching Prevalence (%)")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) +
  scale_fill_manual(values=c("#FFFFFF", "#6699FF", "#0000CC", "#000066"), breaks = c('BL', 'PB', 'P', 'H'), 
                    labels=c('Bleached', 
                             'Partially Bleached', #\n
                             'Pale', 
                             'Not bleached')) +
#  facet_grid(factor(depth2, levels = c('Shallow', 'Deep')) ~ factor(species2, 
#                                                                    levels = c(expression(italic("Porites"),
#                                                                               expression(italic("Pocillopora")))))) +
  facet_grid(factor(depth2, levels = c('Shallow', 'Deep')) ~ factor(species2, 
                                                                    levels = c("Porites", "Pocillopora"))) +                                                                                       
  labs(x = NULL) +
  #  ylab('Prevalence (%)') +
  theme(strip.text = element_text(size = 14, color = "black", hjust = 0.50),
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
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 0.0, vjust = 1.75, 
                                   hjust = 0.5, size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank()
  )
        #plot.title = element_text(size = 11),

rpn_bleach.ggbarplot

# rpn_plob_2015 <-
#   rpn_plob_summ.gg %>%
#   add_column(group = rep(c('plob'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_poci_2015 <-
#   rpn_poci_summ.gg %>%
#   add_column(group = rep(c('poci'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_ma_2015 <-
#   rpn_ma_summ.gg %>%
#   add_column(group = rep(c('ma'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_turf_2015 <-
#   rpn_turf_summ.gg %>%
#   add_column(group = rep(c('turf'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_cca_2015 <-
#   rpn_cca_summ.gg %>%
#   add_column(group = rep(c('cca'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_bare_2015 <-
#   rpn_bare_summ.gg %>%
#   add_column(group = rep(c('bare'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_sand_2015 <-
#   rpn_sand_summ.gg %>%
#   add_column(group = rep(c('sand'), times = 6)) %>%
#   mutate_at(vars(group), factor)
# 
# rpn_cover_groups <-
#   bind_rows(
#     rpn_plob_2015,
#     rpn_poci_2015,
#     rpn_ma_2015,
#     rpn_turf_2015,
#     rpn_cca_2015,
#     rpn_bare_2015,
#     rpn_sand_2015)
# 
# label_names <- c("8 m" = "8 m", "15 m" = "15 m")
# 
# group_labels <- c(plob = "PLOB", 
#                   poci = "POCI",
#                   ma = "MA",
#                   turf = "Turf",
#                   cca = "CCA",
#                   bare = "Bare",
#                   sand = "Sand")
# 
# conservation_status <- c('cd'='Conservation Dependent',
#                          'en'='Endangered',
#                          'lc'='Least concern',
#                          'nt'='Near Threatened',
#                          'vu'='Vulnerable',
#                          'domesticated'='Domesticated')
# 
# labeller = labeller(group = group_labels)
# 
# global_labeller <- labeller(group = group_labels)
# 
# rpn_cover_groups.main <-
#   rpn_cover_groups %>%
#   add_column(func.group = rep(c(plob = "PLOB", 
#                                 poci = "POCI",
#                                 ma = "MA",
#                                 turf = "Turf",
#                                 cca = "CCA",
#                                 bare = "Bare",
#                                 sand = "Sand"), each = 6)) %>%
#   mutate_at(vars(func.group), factor)
# 
# rpn_2015.ggbarplot <- ggplot(rpn_cover_groups.main, aes(x = factor(coast, x_labels), y = mean, fill = coast)) +   
#   geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
#   geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
#   scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
#                      labels = function(x) paste0(x*100)) + 
#   scale_x_discrete(expand = c(0, 1)) + 
#   scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
#   facet_grid(factor(func.group, group_labels) ~ depth2) + 
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


rpn_crw.main.gg <- ggplot(data = rpn_crw.main4, aes(x = date, y = sst_max)) +
  geom_line(aes(color = "SST Max (°C)")) +
  geom_line(aes(y = dhw, color = "Degree Heating Week")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(breaks = seq(0, 336, 24)) +
  scale_x_date(date_labels = "%b %y") +
  scale_color_manual(values = c("red2", "blue")) +
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
    axis.text.x = element_text(angle = 60, hjust = 0.25, size = 8),
    axis.title.y = element_blank(),
    legend.title = element_blank())

rpn_crw.main.gg

rpn_cover_groups.main <-
  as.data.frame(rpn_cover_groups.main)

rpn_cover_2015_coral.main <-
  tibble(rpn_cover_groups.main %>%
  filter(group == 'plob' | group == 'poci'))

coral_genus <- expression(paste(italic(" Porites ")))

rpn_cover_2015_coral_species.main <-
  rpn_cover_2015_coral.main %>%
  mutate(coral =
    case_when(func.group == "PLOB" ~ "Porites",
              func.group == "POCI" ~ "Pocillopora") 
    ) %>%
  mutate(species = glue("*{coral}*"))

rpn_cover_2015_coral_species.main$coral
rpn_cover_2015_coral_species.main$species

rpn_cover_2015_coral_species.ggplot <-  
  rpn_cover_2015_coral_species.main %>%
  ggplot(aes(x = coast, ymin = mean, ymax = mean + sd, 
             fill = interaction(coast, depth2))) +   
  geom_bar(aes(y = mean),
           position = position_dodge(0.9), 
           stat = "identity", 
           width = 0.75, 
           color = "black", 
           linewidth = 0.50, 
           alpha = 0.6
           ) +
#  geom_linerange(aes(ymin = mean, ymax = mean + sd), 
#                 position = position_dodge2(width = - 0.50), 
#                # stat = "identity", 
#                 linewidth = 0.75) +
  geom_errorbar(#aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.25,
                #stat = 'Identity',
                position = position_dodge(width = 0.9)) +   
  scale_x_discrete(expand = c(0, 1)) + 
  scale_y_continuous(expression(paste("Coral Cover (%)")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) +
  scale_fill_manual("depth2", 
                    values = c("#3366FF", "#FFCC33", "#33FF00", # 8 m
                               "#0000CC", "#CC9900", "#66CC00"   # 15 m
                               )) + #
  facet_grid(~ factor(species, levels = c('*Porites*', '*Pocillopora*'))) +
  labs(x = NULL) +
#  theme_clean() +
  theme(strip.text = element_markdown(size = 10, color = "black", hjust = 0.50),
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
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        legend.title = element_blank())

rpn_cover_2015_coral_species.ggplot


# Coral cover
rpn_cover_2015_coral_species.ggplot

# Temperature plots

rpn_crw.main.gg # plot 1
rpn_sea_water_temp_main.gg # plot 2
rpn_sst_2014_15_daily2.gg # plot 3
rpn_sst_dec14_mar15.gg # plot 4

# Bleaching prevalence
rpn_bleach.ggbarplot

nested <- ((rpn_crw.main.gg|rpn_sea_water_temp_main.gg)/(rpn_sst_2014_15_daily2.gg|rpn_sst_dec14_mar15.gg))
plot_annotation(tag_levels = 'A') #add figure labels
nested #view multi-panel figure

multi <- (rpn_crw.main.gg + 
            rpn_sea_water_temp_main.gg + 
            rpn_sst_2014_15_daily2.gg + 
            rpn_sst_dec14_mar15.gg) + 
  plot_layout(ncol = 2) +
#  plot_spacer() +
  plot_annotation(tag_levels = 'A') + #add figure labels
theme(
  plot.margin = unit(c(50, 50, 50, 50), "pt")
  )
  
multi #view multi-panel figure

multi <-
  plot_annotation(tag_levels = 'A') + #add figure labels

  (rpn_crw.main.gg +
  theme(plot.margin = unit(c(10, 10, 10, 10), "pt"))) +
  (rpn_sea_water_temp_main.gg +
  theme(plot.margin = unit(c(0, 0, 0, 50), "pt"))) +
  (rpn_sst_2014_15_daily2.gg + 
  theme(plot.margin = unit(c(50, 0, 0, 0), "pt"))) +
  (rpn_sst_dec14_mar15.gg +
  theme(plot.margin = unit(c(0, 0, 50, 0), "pt")))
   
   
   
   
   
   
# G3 <- G1 + plot_spacer() + 
#   G2 + 
#   plot_layout(widths = c(4, -1.1 ,4.5),
#               guides = "collect") & 
#   theme(legend.position = "top")















































