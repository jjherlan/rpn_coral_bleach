# ----------------------------------
# Bleached Porites lobata
# ----------------------------------

rpn_plob_bleach <- rpn_bleach %>%  
  filter(group == 'PLOB' & status == 'BL') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

# ----------------------------------
# Partially Bleached Porites lobata
# ----------------------------------

rpn_plob_pb <- rpn_bleach %>% 
  filter(group == 'PLOB' & status == 'PB') %>% 
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_plob_pb

rpn_plob_pb2 <-
  as.data.frame(rpn_plob_pb) %>%
  add_row(location = 'se', depth = 'dp', transect = 'two', total_count = 0) %>%
  add_row(location = 'se', depth = 'sh', transect = 'one', total_count = 0) %>%
  add_row(location = 'se', depth = 'sh', transect = 'two', total_count = 0)

rpn_plob_pb.main <- rpn_plob_pb2 %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  ) %>%
  mutate_at(vars(location, depth, transect), factor) %>%
  arrange(location, depth, transect) %>%
  add_column(bleach = rep(c('pb'), times = 12)) %>%
  add_column(coast = rep(c('North', 'Southeast', 'West'), each = 4)) %>%
  add_column(depth2 = rep(c('15 m', '8 m'), each = 2, times = 3)) %>%
  mutate_at(vars(coast, depth2), factor)

rpn_plob_pb.main

# ----------------------------------
# Pale Porites lobata
# ----------------------------------

rpn_plob_pale <- rpn_bleach %>%  
  filter(group == 'PLOB' & status == 'P') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_plob_pale

rpn_plob_pale.main <- rpn_plob_pale %>%
  #as_tibble() %>%
  #filter(group == "PLOB") %>%
  #mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  ) %>%
  mutate_at(vars(location, depth, transect), factor) %>%
  arrange(location, depth, transect) %>%
  add_column(bleach = rep(c('p'), times = 12)) %>%
  add_column(coast = rep(c('North', 'Southeast', 'West'), each = 4)) %>%
  add_column(depth2 = rep(c('15 m', '8 m'), each = 2, times = 3)) %>%
  mutate_at(vars(coast, depth2), factor)

rpn_plob_pale.main

# ----------------------------------
# No bleach Porites lobata
# ----------------------------------

rpn_plob_healthy <- rpn_bleach %>%  
  filter(group == 'PLOB' & status == 'H') %>%
  group_by(location, depth, transect) %>%
  dplyr::summarise(total_count = n())

rpn_plob_healthy

rpn_plob_healthy2 <-
  as.data.frame(rpn_plob_healthy) %>%
  add_row(location = 'se', depth = 'dp', transect = 'two', total_count = 0) %>%
  add_row(location = 'west', depth = 'sh', transect = 'two', total_count = 0)

rpn_plob_healthy2

rpn_plob_healthy.main <- rpn_plob_healthy2 %>%
  #as_tibble() %>%
  #filter(group == "PLOB") %>%
  #mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/126,
    failures = 126 - total_count
  ) %>%
  mutate_at(vars(location, depth, transect), factor) %>%
  arrange(location, depth, transect) %>%
  add_column(bleach = rep(c('h'), times = 12)) %>%
  add_column(coast = rep(c('North', 'Southeast', 'West'), each = 4)) %>%
  add_column(depth2 = rep(c('15 m', '8 m'), each = 2, times = 3)) %>%
  mutate_at(vars(coast, depth2), factor)

rpn_plob_healthy.main 

rpn_plob_bleach <-
  bind_rows(
    rpn_plob_pb.main,
    rpn_plob_pale.main,
    rpn_plob_healthy.main) %>%
mutate(bleached2 = case_when(
  bleach == "pb" ~ "bleached", 
  bleach == "p" ~ "bleached", 
  bleach == "h" ~ "non_bleached")
)

# rpn_plob_bleach2 <- rpn_plob_bleach %>%
#   as_tibble() %>%
#   #filter(group == "PLOB") %>%
#   add_column(total_points = rep(c(126), times = 36)) %>%
#   mutate_at(vars(coast, depth2, transect, bleach), factor) %>%
#   mutate(
#     cover = total_count/total_points
#   ) %>%  
#   group_by(coast, depth2, bleach) %>%
#   dplyr::summarize(mean = mean(cover), 
#                    sd = sd(cover), 
#                    n = n(),
#                    se = sd/sqrt(n)
#   ) %>%
#   mutate(se = sd / sqrt(n),
#          lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
#          upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
#   add_column(bleach2 = rep(c("Not bleached", "Pale", "Partially bleached"), times = 6))

rpn_plob_bleach2 <- rpn_plob_bleach %>%
  as_tibble() %>%
  #filter(group == "PLOB") %>%
  add_column(total_points = rep(c(126), times = 36)) %>%
  mutate_at(vars(coast, depth2, transect, bleached2), factor) %>%
  mutate(
    cover = total_count/total_points
  ) %>%  
  group_by(coast, depth2, bleached2) %>%
  dplyr::summarize(mean = mean(cover), 
                   sd = sd(cover), 
                   n = n(),
                   se = sd/sqrt(n)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 
#%>%
#  add_column(bleach2 = rep(c("Not bleached", "Pale", "Partially bleached"), times = 6))

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

bleach_labels = c("Partially bleached", "Pale", "Not bleached")


###GRAPH###
rpn_plob_bleach.ggbarplot <- ggplot(rpn_plob_bleach2, aes(x = factor(coast, x_labels), 
                                                          y = mean, fill = 
                                                            factor(coast, x_labels
                                                            )
)
) +   
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  
  facet_grid(factor(bleach2, bleach_labels) ~ factor(depth2, label_names), margin = FALSE) + 
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

rpn_plob_bleach.ggbarplot

##GRAPH##

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

bleach_labels = c("Partially bleached", "Pale", "Not bleached")

rpn_plob_bleach.ggbarplot <- ggplot(rpn_plob_bleach2, aes(x = factor(coast, x_labels), 
                                                          y = mean, fill = factor(
                                                            depth2, label_names
                                                            #factor(coast, x_labels
                                                          )
) 
) +   
  #geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  #geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  #scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
  #                   labels = function(x) paste0(x*100)) + 
  #scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  #scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  
  geom_bar(stat = "identity", 
           position = position_dodge(), width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), 
                 position = position_dodge2(width = 0.75)
  ) +
  scale_y_continuous(expression(paste("Proportion of Counts (%)")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1)) + 
  scale_fill_manual(values = c("#FFFFCC","#FFC74E"
                               #, "#82A5C0", 
                               #"#ABC178"
  )
  ) +
  facet_wrap(vars(
    bleach2
    #bleach_labels
    #coral_labels
  )
  ) +
  #facet_grid(factor(bleach2, bleach_labels) ~ factor(depth2, label_names), margin = FALSE) + 
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
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_plob_bleach.ggbarplot

##GRAPH##

x_labels = c("North", "West", "Southeast")

label_names = c("8 m" = "8 m", "15 m" = "15 m")

bleach_labels = c("Partially bleached", "Pale", "Not bleached")

rpn_plob_bleach.ggbarplot <- ggplot(rpn_plob_bleach2, aes(x = factor(coast, x_labels), 
                                                          y = mean, fill = factor(
                                                            depth2, label_names
                                                            #factor(coast, x_labels
                                                          )
) 
) +   
  #geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  #geom_linerange(aes(ymin = mean, ymax = mean + sd), linewidth = 0.75) +
  #scale_y_continuous(expression(paste("Percent Cover")), limits = c(0, 1.0), 
  #                   labels = function(x) paste0(x*100)) + 
  #scale_x_discrete(expand = c(0, 1), labels = x_labels) + 
  #scale_fill_manual(values = c("#FFC74E", "#82A5C0", "#ABC178")) + #
  
  geom_bar(stat = "identity", 
           position = position_dodge(), width = 0.75, color = "black", linewidth = 0.50, alpha = 0.6) +
  geom_linerange(aes(ymin = mean, ymax = mean + sd), 
                 position = position_dodge2(width = 0.75)
  ) +
  scale_y_continuous(expression(paste("Proportion of Counts (%)")), limits = c(0, 1.0), 
                     labels = function(x) paste0(x*100)) + 
  scale_x_discrete(expand = c(0, 1)) + 
  scale_fill_manual(values = c("#FFFFCC","#FFC74E"
                               #, "#82A5C0", 
                               #"#ABC178"
  )
  ) +
  facet_wrap(vars(
    bleach2
    #bleach_labels
    #coral_labels
  )
  ) +
  #facet_grid(factor(bleach2, bleach_labels) ~ factor(depth2, label_names), margin = FALSE) + 
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
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank())

rpn_plob_bleach.ggbarplot
