# code/figures.R
# This script contains the code used to generate the final 
# figures and tables seen in the manuscript.


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(ggsci) # Scientific colour palettes

# The correlations
ALL_cor_fig <- ALL_cor %>% 
  filter(Parameter1 == "sst", ts != "full") %>%
  dplyr::rename(var = Parameter2) %>% 
  dplyr::select(-Parameter1) %>% 
  mutate(region = toupper(region)) %>% 
  mutate(region = factor(region, levels = c("MAB", "GM", "SS", "CBS", "GSL", "NFS")),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         var = as.character(var)) %>% 
  mutate(var = case_when(var == "sst" ~ "SST",
                         var == "bottomT" ~ "Bottom",
                         var == "sss" ~ "SSS",
                         var == "mld_cum" ~ "MLD",
                         var == "mld_1_cum" ~ "MLD_1",
                         var == "t2m" ~ "Air",
                         var == "tcc_cum" ~ "Cloud",
                         var == "p_e_cum" ~ "P-E",
                         var == "mslp_cum" ~ "MSLP",
                         var == "lwr_budget" ~ "Qlw",
                         var == "swr_budget" ~ "Qsw",
                         var == "lhf_budget" ~ "Qlh",
                         var == "shf_budget" ~ "Qsh",
                         var == "qnet_budget" ~ "Qnet",
                         TRUE ~ var),
         var = factor(var, levels = c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw",
                                      "Air", "Cloud", "P-E", "MSLP",
                                      "SST", "SSS", "MLD", "MLD_1", "Bottom")))

# Get total counts
total_count <- ALL_cor_fig %>% 
  select(region, season, event_no) %>% 
  distinct() %>% 
  summarise(count = n())
region_count <- ALL_cor_fig %>% 
  dplyr::select(region:ts, n_Obs) %>% 
  unique() %>% 
  group_by(region, ts) %>% 
  summarise(count = n(), .groups = "drop")
season_count <- ALL_cor_fig %>% 
  dplyr::select(region:ts, n_Obs) %>% 
  unique() %>% 
  group_by(season, ts) %>% 
  summarise(count = n(), .groups = "drop")

# Labeller to convert node numbers to characters
node_labeller <- c(
  "1" = "A)", "2" = "B)", "3" = "C)", "4" = "D)", "5" = "E)", "6" = "F)", 
  "7" = "G)", "8" = "H)", "9" = "I)", "10" = "J)", "11" = "K)", "12" = "L)"
)


# Figure 1 ----------------------------------------------------------------

# The study area polygons and MHWs detected therein

# Reorder site labels
NWA_coords$region <- factor(NWA_coords$region, levels = c("MAB", "GM", "SS", "GSL", "CBS", "NFS"))

# Event count by region 
season_count_full <- OISST_MHW_event %>% 
  group_by(season) %>% 
  summarise(count = n(), .groups = "drop")

# Event count by region 
region_count_full <- OISST_MHW_event %>% 
  group_by(region) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  mutate(region = toupper(region))

# Create labels for number of MHWs per region
region_prop_label <- NWA_coords %>%
  left_join(region_count_full, by = "region") %>%
  group_by(region) %>%
  mutate(lon_center = mean(lon), lat_center = mean(lat)) %>%
  na.omit() %>% 
  mutate(lon_center = case_when(region == "GSL" ~ lon_center+2,
                                region == "SS" ~ lon_center+1,
                                region == "GM" ~ lon_center-1,
                                region == "MAB" ~ lon_center+1.3,
                                TRUE ~ lon_center),
         lat_center = case_when(region == "GM" ~ lat_center-1.5,
                                region == "MAB" ~ lat_center+0.5,
                                TRUE ~ lat_center)) %>%
  ungroup()

# Study area
NWA_study_area <- ggplot(data = NWA_coords, aes(x = lon, y = lat)) +
  geom_polygon(aes(colour = region, fill = region), size = 2, alpha = 0.2) +
  geom_label(data = region_prop_label, label.size = 3, show.legend = F, size = 3,
             aes(x = lon_center, y = lat_center, label = count, colour = region)) +
  geom_label(data = region_prop_label, label.size = 0, show.legend = F, size = 3,
             aes(x = lon_center, y = lat_center, label = count), colour = "black") +
  geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  # geom_label(data = total_count, aes(x = -75, y = 50, label = paste0("n: ",count))) +
  geom_label(data = filter(season_count_full, season == "Spring"), 
             aes(x = -55, y = 41, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count_full, season == "Summer"), 
             aes(x = -55, y = 39.65, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count_full, season == "Autumn"), 
             aes(x = -55, y = 38.3, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count_full, season == "Winter"), 
             aes(x = -55, y = 36.95, label = paste0(season,": ",count))) +
  coord_cartesian(xlim = NWA_corners[1:2], ylim = NWA_corners[3:4], expand = F) +
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "top") +
  scale_y_continuous(breaks = c(40, 50),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,5,4,3,6)], 
                      aesthetics = c("colour", "fill")) +
  labs(x = NULL, y = NULL, colour = "Region", fill = "Region") +
  # theme_bw() +
  theme(legend.position = c(0.6, 0.1),
        legend.background = element_rect(colour = "black"),
        legend.direction = "horizontal",
        panel.border = element_rect(colour = "black", fill = NA))
# NWA_study_area

# Lollis
MHW_lolli_plot <- ggplot(data = OISST_MHW_event , aes(x = date_peak, y = intensity_cumulative)) +
  geom_lolli(aes(colour = region), colour_n = "red", n = 0, size = 1.0, show.legend = F) +
  labs(x = "Peak Date", y = "Cumulative Intensity (°C days)") +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,5,4,3,6)]) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(50, 200, 50), expand = c(0,0)) +
  facet_wrap(~region, ncol = 2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 30),
        panel.border = element_rect(colour = "black", fill = NA))
# MHW_lolli_plot

# Combine
fig_1 <- cowplot::plot_grid(NWA_study_area, MHW_lolli_plot, labels = c('A)', 'B)'), label_size = 10,
                            align = 'hv', rel_widths = c(1.2, 1), nrow = 1, axis = "l")
ggsave("figures/fig_1.jpg", fig_1, height = 90, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_1.png", fig_1, height = 90, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_1.pdf", fig_1, height = 90, width = 180, units = "mm")


# Table 1 -----------------------------------------------------------------

# MHW metric summaries by region/season
MHW_event_prep <- OISST_MHW_event %>% 
  dplyr::select(region, season, duration, intensity_mean, intensity_max, 
                intensity_cumulative, rate_onset, rate_decline) %>% 
  mutate(region = toupper(region))

# Differences between regions
summary_region <- MHW_event_prep %>% 
  dplyr::rename(group = region) %>% 
  pivot_longer(duration:rate_decline) %>% 
  group_by(group, name) %>% 
  summarise(value_mean = round(mean(value), 1),
            value_sd = round(sd(value), 1), .groups = "drop") %>% 
  unite("value_summary", value_mean:value_sd, sep = " ± ") %>% 
  pivot_wider(names_from = name, values_from = value_summary) %>% 
  dplyr::rename(i_cum = intensity_cumulative, i_max = intensity_max, 
                i_mean = intensity_mean, r_decline = rate_decline, r_onset = rate_onset) %>% 
  dplyr::select(group, duration, i_mean, i_max, i_cum)#, r_onset, r_decline) # Decided to remove rate of onset and decline

# Differences between seasons
summary_season <- MHW_event_prep %>% 
  dplyr::rename(group = season) %>% 
  pivot_longer(duration:rate_decline) %>% 
  group_by(group, name) %>% 
  summarise(value_mean = round(mean(value), 1),
            value_sd = round(sd(value), 1), .groups = "drop") %>% 
  unite("value_summary", value_mean:value_sd, sep = " ± ") %>% 
  pivot_wider(names_from = name, values_from = value_summary) %>% 
  dplyr::rename(i_cum = intensity_cumulative, i_max = intensity_max, 
                i_mean = intensity_mean, r_decline = rate_decline, r_onset = rate_onset) %>% 
  dplyr::select(group, duration, i_mean, i_max, i_cum)#, r_onset, r_decline)

# Table showing the mean +- SD per region and season
summary_region_season <- rbind(summary_region, summary_season)
write_csv(summary_region_season, "figures/tab_1.csv")
tab_1 <- knitr::kable(summary_region_season)#, format = "latex")
tab_1


# Figure 2 ----------------------------------------------------------------

# Prep magnitude data
ALL_mag <- ALL_cor_fig %>% 
  dplyr::select(region:ts, var, n_Obs, mag) %>% 
  na.omit()

# Calculate the proportions of change in magnitudes
ALL_mag_prop <- ALL_mag %>% 
  filter(var != "SST") %>% 
  left_join(filter(ALL_mag, var == "SST"), 
            by = c("region", "season", "event_no", "ts", "n_Obs")) %>% 
  dplyr::select(-var.y) %>% 
  dplyr::rename(var = var.x, mag_Qx = mag.x, mag_SSTa = mag.y) %>% 
  mutate(prop = mag_Qx/mag_SSTa) %>% 
  mutate(prop_cap = case_when(prop >= quantile(prop, 0.95) ~ quantile(prop, 0.95),
                              prop <= quantile(prop, 0.05) ~ quantile(prop, 0.05),
                              TRUE ~ prop),
         prop_cat = case_when(prop > 0.5 ~ "> 0.5",
                              prop <= 0.5 & prop >= 0 ~ "0 - 0.5",
                              prop < 0.5 ~ "< 0"))

# Median proportion of changes
ALL_mag_prop %>% 
  filter(ts == "onset", var == "Qnet") %>% 
  summarise(median(prop))
ALL_mag_prop %>% 
  filter(ts == "decline", var == "Qnet") %>% 
  summarise(median(prop))

# Number of events contributing negatively to onset or decline
ALL_mag_prop %>% 
  filter(ts == "onset", var == "Qnet", mag_Qx > 0) %>% 
  summarise(n())
ALL_mag_prop %>% 
  filter(ts == "decline", var == "Qnet", mag_Qx < 0) %>% 
  summarise(n())

# Filter out only events that were driven or decayed by Qnet
ALL_mag_prop_onset <- ALL_mag_prop %>% 
  filter(ts == "onset", prop > 0.5)
ALL_mag_prop_decline <- ALL_mag_prop %>% 
  filter(ts == "decline", prop > 0.5)

# Scatterplot of mag T_Qx vs mag SSTa
mag_scat <- ALL_mag_prop %>%
  filter(var == "Qnet") %>% 
  ggplot(aes(x = mag_SSTa, y = mag_Qx)) +
  geom_segment(aes(x = -3.5, xend = 3.5, y = -3.5, yend = 3.5), linetype = "dashed", colour = "grey50") +
  geom_hline(aes(yintercept = 0), colour = "red") +
  # geom_point(aes(colour = prop_cap), size = 1) +
  geom_point(aes(colour = prop_cat), size = 1) +
  geom_rect(aes(xmin = -3.5, xmax = -0.01, ymin = -3.5, ymax = 3.5), colour = "darkorchid1", fill = NA) +
  geom_rect(aes(xmin = 0.01, xmax = 3.5, ymin = -3.5, ymax = 3.5), colour = "deeppink1", fill = NA) +
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.4, yend = 3.3), arrow = arrow(angle = 30, length = unit(3, "mm")), colour = "deeppink1") +
  geom_label(aes(x = 0.8, y = 2.5, label = "onset"), colour = "deeppink1") +
  geom_segment(aes(x = -0.4, xend = -0.4, y = -1.4, yend = -3.3), arrow = arrow(angle = 30, length = unit(3, "mm")), colour = "darkorchid1") +
  geom_label(aes(x = -0.8, y = -2.5, label = "decline"), colour = "darkorchid1") +
  # geom_point(data = ALL_mag_prop_onset, aes(fill = prop_cap), colour = "deeppink1", shape = 21, stroke = 1) +
  # geom_point(data = ALL_mag_prop_decline, aes(fill = prop_cap), colour = "darkorchid1", shape = 21, stroke = 1) +
  # geom_smooth(aes(colour = ts), method = "lm", show.legend = F) +
  # geom_smooth(data = filter(ALL_mag_prop, ts == "onset"), colour = "deeppink1", method = "lm", size = 0.5) +
  # geom_smooth(data = filter(ALL_mag_prop, ts == "decline"), colour = "darkorchid1", method = "lm", size = 0.5) +
  # scale_fill_viridis_c(option = "D") +
  # scale_fill_distiller(palette = "PuOr") +
  # scale_colour_gradient2(low = pal_jco()(3)[1], mid = pal_jco()(3)[3], high = pal_jco()(3)[2]) +
  scale_colour_manual(values = c(pal_jco()(3)[c(1,3,2)])) +
  # scale_colour_manual(values = c("deeppink1", "darkorchid1")) +
  # scale_y_continuous(limits = c(-3, 3)) +
  # scale_x_continuous(limits = c(-3, 3)) +
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), expand = F) +
  labs(x = "𝚫SSTa", 
       # y = "𝚫*T*<sub>*Q* net</sub>",
       y = "𝚫T<sub>Qnet</sub>",
       colour = "Prop.") +
  theme(axis.title.y = ggtext::element_markdown(),
        legend.position = c(0.11, 0.76),
        legend.background = element_rect(colour = "black", fill = "white"),
        # legend.direction = "horizontal",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.background = element_rect(colour = "black"))
  # theme(legend.position = "bottom")
# mag_scat

# Boxplots showing the range of magnitudes
mag_box <- ALL_mag_prop %>%
  filter(var == "Qnet") %>% 
  ggplot(aes(x = ts, y = prop)) +
  geom_hline(aes(yintercept = 0), colour = "red") +
  geom_boxplot(aes(fill = ts), show.legend = F) +
  # geom_point(aes(colour = mag_SSTa), position = position_jitterdodge(dodge.width = 0.9)) +
  scale_fill_manual(values = c("deeppink1", "darkorchid1")) +
  # scale_colour_viridis_c() +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = "Phase", 
       y = "𝚫T<sub>Qnet</sub> / 𝚫SSTa") +
  theme(axis.title.y = ggtext::element_markdown(),
        panel.border = element_rect(colour = "black", fill = NA))
# mag_box

# Combine and save
fig_2 <- ggpubr::ggarrange(mag_scat, mag_box, labels = c("A)", "B)"), align = "hv", widths = c(1.5, 1))
ggsave("figures/fig_2.jpg", fig_2, height = 72, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_2.png", fig_2, height = 72, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_2.pdf", fig_2, height = 72, width = 180, units = "mm")


# Table 2 -----------------------------------------------------------------

# Count of events with greatest increase/decrease in a SST_Qx term

# Counts by time series phase
ALL_mag_count_ts <- ALL_mag_prop %>% 
  group_by(ts, var) %>% 
  mutate(ts_count = n(),
         group = "Total") %>% 
  filter(prop > 0.5) %>% 
  mutate(filter_count = n(),
         filter_prop = round(filter_count/ts_count, 2)) %>% 
  dplyr::select(group, ts, var, filter_prop) %>% 
  distinct() %>% 
  mutate(var = factor(var, levels = c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"))) %>% 
  pivot_wider(names_from = var, values_from = filter_prop) %>% 
  dplyr::select(group, ts, Qnet, Qlh, Qsh, Qlw, Qsw)

# Counts by time region phase
ALL_mag_count_region <- ALL_mag_prop %>% 
  group_by(region, ts, var) %>% 
  mutate(ts_count = n()) %>% 
  filter(prop > 0.5) %>% 
  mutate(filter_count = n(),
         filter_prop = round(filter_count/ts_count, 2)) %>% 
  dplyr::select(region, ts, var, filter_prop) %>% 
  distinct() %>% 
  pivot_wider(names_from = var, values_from = filter_prop) %>% 
  dplyr::select(region, ts, Qnet, Qlh, Qsh, Qlw, Qsw) %>% 
  arrange(region, ts) %>% 
  dplyr::rename(group = region)

# Counts by time series phase
ALL_mag_count_season <- ALL_mag_prop %>% 
  group_by(season, ts, var) %>% 
  mutate(ts_count = n()) %>% 
  filter(prop > 0.5) %>% 
  mutate(filter_count = n(),
         filter_prop = round(filter_count/ts_count, 2)) %>% 
  dplyr::select(season, ts, var, filter_prop) %>% 
  distinct() %>% 
  pivot_wider(names_from = var, values_from = filter_prop) %>% 
  dplyr::select(season, ts, Qnet, Qlh, Qsh, Qlw, Qsw) %>% 
  arrange(season, ts) %>% 
  dplyr::rename(group = season)

# Combine and prep
ALL_mag_count_ts_region_season <- rbind(ALL_mag_count_ts, ALL_mag_count_region, ALL_mag_count_season) %>% 
  dplyr::select(group:Qnet) %>% 
  pivot_wider(names_from = ts, values_from = Qnet) %>% 
  dplyr::select(group, onset, decline) %>% 
  mutate(onset = paste0(onset*100,"%"),
         decline = paste0(decline*100,"%"))

# Print table
write_csv(ALL_mag_count_ts_region_season, "figures/tab_2.csv")
tab_2 <- knitr::kable(ALL_mag_count_ts_region_season)#, format = "latex")
tab_2


# Table 3 -----------------------------------------------------------------

# The top count of Qx terms by RMSE per region

# Create TRUE/FALSE table when prop is over 0.5 for any variable
ALL_mag_TF <- ALL_mag_prop %>% 
  mutate(prop_TF = ifelse(prop > 0.5, TRUE, FALSE)) %>% 
  dplyr::select(region:n_Obs, prop_TF) %>% 
  pivot_wider(names_from = var, values_from = prop_TF)

# The base RMSE results
ALL_RMSE <- ALL_cor_fig %>% 
  filter(rmse > 0) %>% 
  left_join(ALL_mag_TF, by = c("region", "season", "event_no", "ts", "n_Obs")) %>% 
  filter(Qnet == TRUE) %>% 
  dplyr::select(region:ts, var, n_Obs, rmse)

# Get the top results other than Qnet
ALL_RMSE_top <- ALL_RMSE %>% 
  filter(var != "Qnet") %>% # Remove Qnet
  group_by(region, season, event_no, ts) %>% 
  filter(rmse == min(rmse))

# The top count by phase
ALL_RMSE_ts <- ALL_RMSE_top %>%
  group_by(ts) %>%
  mutate(total_count = n(),
         group = "Total") %>% 
  group_by(group, ts, total_count, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  replace_na(list(Qlh = 0, Qsh = 0, Qlw = 0, Qsw = 0)) %>% 
  dplyr::select(group, ts, total_count, Qlh:Qsw) %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/total_count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/total_count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/total_count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/total_count)*100),"%)"))

# The top count by region
ALL_RMSE_region <- ALL_RMSE_top %>%
  group_by(region, ts) %>%
  mutate(total_count = n()) %>% 
  group_by(region, ts, total_count, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  replace_na(list(Qlh = 0, Qsh = 0, Qlw = 0, Qsw = 0)) %>% 
  dplyr::select(region, ts, total_count, Qlh:Qlw) %>% 
  filter(ts != "full") %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/total_count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/total_count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/total_count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/total_count)*100),"%)")) %>% 
  dplyr::rename(group = region)

# The top count by season
ALL_RMSE_season <- ALL_RMSE_top %>% 
  group_by(season, ts) %>%
  mutate(total_count = n()) %>% 
  group_by(season, ts, total_count, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  replace_na(list(Qlh = 0, Qsh = 0, Qlw = 0, Qsw = 0)) %>% 
  dplyr::select(season, ts, total_count, Qlh:Qlw) %>% 
  filter(ts != "full") %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/total_count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/total_count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/total_count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/total_count)*100),"%)")) %>% 
  dplyr::rename(group = season)

# Print table
ALL_RMSE_ts_region_season <- rbind(ALL_RMSE_ts, ALL_RMSE_region, ALL_RMSE_season)
write_csv(ALL_RMSE_ts_region_season, "figures/tab_3.csv")
tab_3 <- knitr::kable(ALL_RMSE_ts_region_season)#, format = "latex")
tab_3


# Figure 3 ----------------------------------------------------------------

# Boxplots showing the range of RMSE values for T_Qx by season
fig_3 <- ALL_RMSE %>%
  filter(var != "Qnet") %>% 
  # mutate(var = as.character(var)) %>% 
  # mutate(var = paste0("T<sub>",var,"</sub>")) +
  # mutate(var = paste0("T[",var,"]")) +
  ggplot(aes(x = var, y = rmse)) +
  geom_boxplot(aes(fill = ts), outlier.size = 0.5) +
  geom_hline(aes(yintercept = 0), colour = "red") +
  scale_fill_manual(values = c("deeppink1", "darkorchid1")) +
  facet_wrap(~season, nrow = 2) + 
  # scale_x_discrete() +
  labs(x = "Heat flux variable (T<sub>Qx</sub>)", y = "RMSE", fill = "Phase") +
  theme(legend.position = "top",
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.x = ggtext::element_markdown())
# fig_3
ggsave("figures/fig_3.jpg", fig_3, height = 120, width = 85, units = "mm", dpi = 300)
ggsave("figures/fig_3.png", fig_3, height = 120, width = 85, units = "mm", dpi = 300)
ggsave("figures/fig_3.pdf", fig_3, height = 120, width = 85, units = "mm")


# Figure 4 ----------------------------------------------------------------

# Histogram of r values

# Function for plotting histograms of chosen variables
hist_var <- function(var_choices, y_label){
  hist_plot <- ALL_cor_fig %>% 
    filter(var %in% var_choices) %>% 
    group_by(var, ts) %>% 
    mutate(mean_r = mean(r)) %>% 
    ungroup() %>% 
    mutate(var = as.character(var),
           var = case_when(var == "Air" ~ "T[air]",
                           var == "P-E" ~ "P-E[cum]",
                           var == "MSLP" ~ "MSLP[cum]",
                           var == "MLD" ~ "MLD[cum]",
                           var == "Bottom" ~ "T[b]",
                           TRUE ~ var)) %>% 
    ggplot(aes(x = r)) +
    geom_vline(aes(xintercept = 0), colour = "red", size = 0.5) +
    geom_vline(aes(xintercept = mean_r), colour = "red", size = 0.5, linetype = "dashed") +
    geom_histogram(bins = 20) +
    # scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0),
                       limits = c(-1, 1),
                       breaks = c(-0.5, 0, 0.5),
                       labels = c("-0.5", "0", "0.5")) +
    facet_grid(ts ~ var, labeller = label_parsed) +
    # coord_fixed(ratio = 0.01) +
    labs(x = "Correlation value (*r*)", y = "Count", title = y_label) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title.x = ggtext::element_markdown())
  if(y_label == "Atmosphere"){
    hist_plot <- hist_plot +
      scale_y_continuous(expand = c(0, 0), breaks = c(15, 45)) +
      labs(x = NULL)
      # theme(axis.text.x = element_blank(),
      #       axis.title.x = element_blank(),
      #       axis.ticks.x = element_blank())
  } else{
    hist_plot <- hist_plot +
      scale_y_continuous(expand = c(0, 0), breaks = c(30, 90)) #+
  }
  return(hist_plot)
}

# The three figures
# hist_Q <- hist_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
hist_air <- hist_var(c("Air", "P-E", "MSLP"), "Atmosphere")
hist_ocean <- hist_var(c("SSS", "MLD", "Bottom"), "Ocean")

# Combine air and sea
fig_4 <- ggpubr::ggarrange(hist_air, hist_ocean, ncol = 1, nrow = 2, labels = c("A)", "B)"), heights = c(1, 1.1))
# fig_4
ggsave("figures/fig_4.jpg", fig_4, height = 100, width = 85, units = "mm", dpi = 300)
ggsave("figures/fig_4.png", fig_4, height = 100, width = 85, units = "mm", dpi = 300)
ggsave("figures/fig_4.pdf", fig_4, height = 100, width = 85, units = "mm")


# Figure 5 ----------------------------------------------------------------

# Most important variables by region/season
boxplot_var <-  function(var_choices, y_label){
  box_plot <- ALL_cor_fig %>% 
    filter(var %in% var_choices) %>% 
    mutate(var = as.character(var),
           var = case_when(var == "Air" ~ "T[air]",
                           var == "P-E" ~ "P-E[cum]",
                           var == "MSLP" ~ "MSLP[cum]",
                           var == "MLD" ~ "MLD[cum]",
                           var == "Bottom" ~ "T[b]",
                           TRUE ~ var)) %>% 
    ggplot(aes(x = ts, y = r)) +
    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
    geom_boxplot(aes(fill = season), notch = F, outlier.size = 0.5) +
    # geom_violin(aes(fill = season)) + # Looks bad
    facet_wrap(~var, nrow = 1, labeller = label_parsed) +
    # scale_fill_manual(values = c("#a99a35", "#8baa43", "#e89c3c", "#9a9997")) + # muted
    # scale_fill_manual(values = c("#d6cf36", "#a5bfe4", "#efbe83", "#b6b6b4")) + # larger spread
    scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Accent")[c(1,4,3,2)]) +
    scale_y_continuous(expand = c(0, 0)) +
    # scale_x_continuous(expand = c(0, 0)) +
    # facet_grid(ts ~ Parameter2) +
    # coord_equal(ratio = 1.5) +
    labs(x = "Phase", y = "Corr. value (*r*)", fill = "Season") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title.y = ggtext::element_markdown(),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))
  if(y_label == "Atmosphere"){
    box_plot <- box_plot +
      # scale_y_continuous(expand = c(0, 0), breaks = c(15, 45)) +
      labs(x = NULL)
    # theme(axis.text.x = element_blank(),
    #       axis.title.x = element_blank(),
    #       axis.ticks.x = element_blank())
  }
  return(box_plot)
}

# box_Q <- boxplot_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
box_air <- boxplot_var(c("Air", "P-E", "MSLP"), "Atmosphere")
box_ocean <- boxplot_var(c("SSS", "MLD", "Bottom"), "Ocean")

# Combine bottom two
fig_5 <- ggpubr::ggarrange(box_air, box_ocean, ncol = 1, nrow = 2, labels = c("A)", "B)"), common.legend = T, heights = c(1, 1.1))
# fig_5
ggsave("figures/fig_5.jpg", fig_5, height = 100, width = 85, units = "mm", dpi = 300)
ggsave("figures/fig_5.png", fig_5, height = 100, width = 85, units = "mm", dpi = 300)
ggsave("figures/fig_5.pdf", fig_5, height = 100, width = 85, units = "mm")


# Figure 6 ----------------------------------------------------------------

# The SOM region/season figure

# Load the SOM data
SOM <- readRDS("data/som.Rda")

# Base data for SOM figures
base_data <- fig_data_prep(SOM)

# SOM region + season panels
fig_6 <- fig_map_func("region_season", base_data, 1, 9, 13) +
  facet_wrap(~node, labeller = labeller(node = node_labeller)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
# fig_6
ggsave("figures/fig_6.jpg", fig_6, height = 180, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_6.png", fig_6, height = 180, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_6.pdf", fig_6, height = 180, width = 180, units = "mm")


# Figure 7 ----------------------------------------------------------------

# SOM atmosphere panels
fig_7 <- fig_map_func("air_u_v_mslp_anom", base_data, 1, 9, 13) +
  facet_wrap(~node, labeller = labeller(node = node_labeller)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
# fig_7
ggsave("figures/fig_7.jpg", fig_7, height = 180, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_7.png", fig_7, height = 180, width = 180, units = "mm", dpi = 300)
ggsave("figures/fig_7.pdf", fig_7, height = 180, width = 180, units = "mm")


# Table 4 -----------------------------------------------------------------

# The summary of the SOM nodes

# Get node info
SOM_info <- SOM$info

# Assign nodes to each MHW and get mean+-sd metrics
MHW_SOM <- left_join(OISST_MHW_event, SOM_info, by = c("region", "event_no")) %>% 
  group_by(node) %>%
  mutate(count = n()) %>% 
  dplyr::select(node, count, duration, intensity_mean, intensity_max, intensity_cumulative) %>% #, rate_onset, rate_decline) %>% 
  summarise_all(c("mean", "sd")) %>% 
  mutate_all(round, 1) %>% 
  mutate(node = LETTERS[node]) %>% 
  dplyr::rename(count = count_mean) %>% 
  dplyr::select(-count_sd) %>% 
  unite("D", duration_mean, duration_sd, sep = " ± ") %>% 
  unite("imean", intensity_mean_mean, intensity_mean_sd, sep = " ± ") %>% 
  unite("imax", intensity_max_mean, intensity_max_sd, sep = " ± ") %>% 
  unite("icum", intensity_cumulative_mean, intensity_cumulative_sd, sep = " ± ") #%>% 
  # unite("ronset", rate_onset_mean, rate_onset_sd, sep = " ± ") %>% 
  # unite("rdecline", rate_decline_mean, rate_decline_sd, sep = " ± ")
write_csv(MHW_SOM, "figures/tab_4.csv")
tab_4 <- knitr::kable(MHW_SOM)
tab_4


# Table 5 -----------------------------------------------------------------

# Expert summary of the SOM nodes
