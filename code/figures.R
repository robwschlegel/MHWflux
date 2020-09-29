# code/figures.R
# This script contains the code used to generate the final 
# figures and tables seen in the manuscript.


# Setup -------------------------------------------------------------------

source("code/functions.R")

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
                                region == "MAB" ~ lon_center+1.8,
                                TRUE ~ lon_center),
         lat_center = case_when(region == "GM" ~ lat_center-1.5,
                                region == "MAB" ~ lat_center+0.8,
                                TRUE ~ lat_center)) %>%
  ungroup()

# Study area
NWA_study_area <- ggplot(data = NWA_coords, aes(x = lon, y = lat)) +
  geom_polygon(aes(colour = region, fill = region), size = 2, alpha = 0.2) +
  geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  geom_label(data = region_prop_label, label.size = 3, show.legend = F,
             aes(x = lon_center, y = lat_center, label = count, colour = region)) +
  geom_label(data = region_prop_label, label.size = 0, show.legend = F,
             aes(x = lon_center, y = lat_center, label = count), colour = "black") +
  geom_label(data = filter(season_count_full, season == "Spring"), 
             aes(x = -55, y = 41, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count_full, season == "Summer"), 
             aes(x = -55, y = 40, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count_full, season == "Autumn"), 
             aes(x = -55, y = 39, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count_full, season == "Winter"), 
             aes(x = -55, y = 38, label = paste0(season,": ",count))) +
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
  theme(legend.position = c(0.7, 0.1),
        legend.background = element_rect(colour = "black"),
        legend.direction = "horizontal",
        panel.background = element_rect(colour = "black"))
# NWA_study_area

# Lollis
MHW_lolli_plot <- ggplot(data = OISST_MHW_event , aes(x = date_peak, y = intensity_cumulative)) +
  geom_lolli(aes(colour = region), colour_n = "red", n = 0, size = 1.0, show.legend = F) +
  labs(x = "Peak Date", y = "Cumulative Intensity (°C x days)") +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,5,4,3,6)]) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(50, 200, 50), expand = c(0,0)) +
  facet_wrap(~region, ncol = 2) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(colour = "black"))
# MHW_lolli_plot

# Combine
fig_1 <- cowplot::plot_grid(NWA_study_area, MHW_lolli_plot, labels = c('A)', 'B)'), label_size = 10,
                            align = 'hv', rel_widths = c(1.2, 1), nrow = 1, axis = "l")
ggsave("figures/fig_1.png", fig_1, height = 5, width = 10)
ggsave("figures/fig_1.pdf", fig_1, height = 5, width = 10)


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
  dplyr::select(group, duration, i_mean, i_max, i_cum, r_onset, r_decline)

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
  dplyr::select(group, duration, i_mean, i_max, i_cum, r_onset, r_decline)

# Table showing the mean +- SD per region and season
summary_region_season <- rbind(summary_region, summary_season)
tab_1 <- knitr::kable(summary_region_season)#, format = "latex")
tab_1


# Table 2 -----------------------------------------------------------------

# Count of events with greatest increase/decrease in a SST_Qx term

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
  droplevels()

# Get the top results other than Qnet
ALL_mag_top <- ALL_mag_prop %>% 
  filter(var != "Qnet") %>% # Remove Qnet
  droplevels() %>% 
  group_by(region, season, event_no, ts) %>% 
  filter(prop == max(prop))

# The top count by region
ALL_mag_region <- ALL_mag_top %>% 
  group_by(region, ts, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  mutate(Qlw = replace_na(Qlw, 0)) %>% 
  left_join(region_count, by = c("region", "ts")) %>% 
  dplyr::select(region, ts, count, Qlh:Qlw) %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/count)*100),"%)")) %>% 
  dplyr::rename(group = region)

# The top count by season
ALL_mag_season <- ALL_mag_top %>% 
  group_by(season, ts, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  mutate(Qsh = replace_na(Qsh, 0)) %>%
  left_join(season_count, by = c("season", "ts")) %>% 
  dplyr::select(season, ts, count, Qlh:Qsw) %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/count)*100),"%)")) %>% 
  dplyr::rename(group = season)

# Print table
ALL_mag_region_season <- rbind(ALL_mag_region, ALL_mag_season)
tab_2 <- knitr::kable(ALL_mag_region_season)#, format = "latex")
tab_2


# Figure 2 ----------------------------------------------------------------

# Central tendencies for proportions of change and the magnitudes thereof 

fig_2 <- ALL_mag_prop %>% 
  filter(var != "Qnet") %>% 
  ggplot(aes(x = ts, y = prop, fill = season)) +
  geom_hline(aes(yintercept = 0), colour = "red") +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +
  geom_point(aes(colour = mag_Qx), position = position_jitterdodge(dodge.width = 0.9)) +
  scale_y_continuous(limits = c(-2, 2), expand = c(0,0), breaks = c(-1, 0, 1)) +
  scale_fill_manual(values = c("#a99a35", "#8baa43", "#e89c3c", "#9a9997")) +
  scale_colour_gradient2(low = "blue", high = "red", mid = "grey") +
  facet_wrap(~var, nrow = 2) +
  labs(x = NULL, y = "ΔSST_Qx / ΔSSTa", colour = "ΔSST_Qx", fill = "Season") +
  theme(legend.position = "top")
# fig_2
ggsave("figures/fig_2.png", fig_2, height = 9, width = 10)
ggsave("figures/fig_2.pdf", fig_2, height = 9, width = 10)


# Table 3 -----------------------------------------------------------------

# The top count of Qx terms by RMSE per region

# The base RMSE results
ALL_RMSE <- ALL_cor %>% 
  filter(rmse > 0) %>% 
  dplyr::rename(var = Parameter2) %>% 
  dplyr::select(region:ts, var, n_Obs, rmse) %>% 
  mutate(var = as.character(var),
         var = case_when(var == "lwr_budget" ~ "Qlw",
                         var == "swr_budget" ~ "Qsw",
                         var == "lhf_budget" ~ "Qlh",
                         var == "shf_budget" ~ "Qsh",
                         var == "qnet_budget" ~ "Qnet",
                         TRUE ~ var),
         var = factor(var, levels = c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw")),
         region = toupper(region))

# Get the top results other than Qnet
ALL_RMSE_top <- ALL_RMSE %>% 
  filter(var != "Qnet") %>% # Remove Qnet
  group_by(region, season, event_no, ts) %>% 
  filter(rmse == min(rmse))

# The top count by region
ALL_RMSE_region <- ALL_RMSE_top %>% 
  group_by(region, ts, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  mutate(Qlw = replace_na(Qlw, 0)) %>% 
  left_join(region_count, by = c("region", "ts")) %>% 
  dplyr::select(region, ts, count, Qlh:Qsw) %>% 
  filter(ts != "full") %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/count)*100),"%)")) %>% 
  dplyr::rename(group = region)
# knitr::kable(ALL_RMSE_region)

# The top count by season
ALL_RMSE_season <- ALL_RMSE_top %>% 
  group_by(season, ts, var) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = var, values_from = count) %>% 
  # mutate(Qlw = replace_na(Qlw, 0)) %>% 
  left_join(season_count, by = c("season", "ts")) %>% 
  dplyr::select(season, ts, count, Qlh:Qsw) %>% 
  filter(ts != "full") %>% 
  mutate(Qlh = paste0(Qlh, " (",round((Qlh/count)*100),"%)"),
         Qsh = paste0(Qsh, " (",round((Qsh/count)*100),"%)"),
         Qlw = paste0(Qlw, " (",round((Qlw/count)*100),"%)"),
         Qsw = paste0(Qsw, " (",round((Qsw/count)*100),"%)")) %>% 
  dplyr::rename(group = season)
# knitr::kable(ALL_RMSE_season)

# Print table
ALL_RMSE_region_season <- rbind(ALL_RMSE_region, ALL_RMSE_season)
tab_3 <- knitr::kable(ALL_RMSE_region_season)#, format = "latex")
tab_3


# Table 4 -----------------------------------------------------------------

# Central tendencies of RMSE values per variable per region
ALL_RMSE_central_region <- ALL_RMSE %>% 
  filter(ts != "full", var != "Qnet") %>% 
  dplyr::rename(group = region) %>% 
  group_by(group, ts, var) %>% 
  summarise(rmse_mean = round(mean(rmse, na.rm = T), 1),
            rmse_sd = round(sd(rmse, na.rm = T), 1), .groups = "drop") %>% 
  unite("rmse_summary", rmse_mean:rmse_sd, sep = " ± ") %>% 
  pivot_wider(names_from = var, values_from = rmse_summary)

# Central tendencies of RMSE values per variable per season
ALL_RMSE_central_season <- ALL_RMSE %>% 
  filter(ts != "full", var != "Qnet") %>% 
  dplyr::rename(group = season) %>% 
  group_by(group, ts, var) %>% 
  summarise(rmse_mean = round(mean(rmse, na.rm = T), 1),
            rmse_sd = round(sd(rmse, na.rm = T), 1), .groups = "drop") %>% 
  unite("rmse_summary", rmse_mean:rmse_sd, sep = " ± ") %>% 
  pivot_wider(names_from = var, values_from = rmse_summary)

# Print table
ALL_RMSE_central_region_season <- rbind(ALL_RMSE_central_region, ALL_RMSE_central_season)
tab_4 <- knitr::kable(ALL_RMSE_central_region_season)#, format = "latex")
tab_4


# Figure 3 ----------------------------------------------------------------

# Histogram of r values

# Function for plotting histograms of chosen variables
hist_var <- function(var_choices, y_label){
  ALL_cor_fig %>% 
    filter(var %in% var_choices) %>% 
    ggplot(aes(x = r)) +
    geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
    geom_histogram(bins = 10) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-0.9, -0.5, 0, 0.5, 0.9)) +
    facet_grid(ts ~ var) +
    coord_fixed(ratio = 0.01) +
    labs(x = NULL, y = y_label) +
    theme(axis.title.y = element_text(size = 16),
          panel.background = element_rect(colour = "black"))
}

# The three figures
# hist_Q <- hist_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
hist_air <- hist_var(c("Air", "P-E", "MSLP"), "Atmosphere")
hist_ocean <- hist_var(c("SSS", "MLD", "Bottom"), "Ocean")

# Combine air and sea
fig_3 <- ggpubr::ggarrange(hist_air, hist_ocean, ncol = 2, nrow = 1, align = "h",  labels = c("A)", "B)"))
# fig_3
ggsave("figures/fig_3.png", fig_3, height = 2.7, width = 10)
ggsave("figures/fig_3.pdf", fig_3, height = 2.7, width = 10)


# Figure 4 ----------------------------------------------------------------

# Most important variables by region/season
boxplot_var <-  function(var_choices, y_label){
  ALL_cor_fig %>% 
    filter(var %in% var_choices) %>% 
    ggplot(aes(x = ts, y = r)) +
    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
    geom_boxplot(aes(fill = season), notch = F) +
    # geom_violin(aes(fill = season)) + # Looks bad
    facet_wrap(~var, nrow = 1) +
    scale_fill_manual(values = c("#a99a35", "#8baa43", "#e89c3c", "#9a9997")) + # muted
    # scale_fill_manual(values = c("#d6cf36", "#a5bfe4", "#efbe83", "#b6b6b4")) + # larger spread
    # scale_y_continuous(expand = c(0, 0)) +
    # scale_x_continuous(expand = c(0, 0)) +
    # facet_grid(ts ~ Parameter2) +
    coord_equal(ratio = 1.5) +
    labs(x = NULL, y = y_label, fill = "Region") +
    theme(axis.title.y = element_text(size = 16),
          panel.background = element_rect(colour = "black"))
}

# box_Q <- boxplot_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
box_air <- boxplot_var(c("Air", "P-E", "MSLP"), "Atmosphere")
box_ocean <- boxplot_var(c("SSS", "MLD", "Bottom"), "Ocean")

# Combine bottom two
fig_4 <- ggpubr::ggarrange(box_air, box_ocean, ncol = 2, nrow = 1, align = "h",  labels = c("A)", "B)"), common.legend = T)
# fig_4
ggsave("figures/fig_4.png", fig_4, height = 2.7, width = 10)
ggsave("figures/fig_4.png", fig_4, height = 2.7, width = 10)


# Figure 5 ----------------------------------------------------------------

# The SOM region/season figure

# Load the SOM data
SOM <- readRDS("data/som.Rda")

# Base data for SOM figures
base_data <- fig_data_prep(SOM)

# SOM region + season panels
fig_5 <- fig_map_func("region_season", base_data, 1, 9, 13) +
  facet_wrap(~node, labeller = labeller(node = node_labeller)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
# fig_5
ggsave("figures/fig_5.png", fig_4, height = 9, width = 13)
ggsave("figures/fig_5.pdf", fig_4, height = 9, width = 13)


# Figure 6 ----------------------------------------------------------------

# SOM atmosphere panels
fig_6 <- fig_map_func("air_u_v_mslp_anom", base_data, 1, 9, 13) +
  facet_wrap(~node, labeller = labeller(node = node_labeller)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
# fig_6
ggsave("figures/fig_6.png", fig_6, height = 9, width = 13)
ggsave("figures/fig_6.pdf", fig_6, height = 9, width = 13)


# Table 5 -----------------------------------------------------------------

# The summary of the SOM nodes


# Figure 7 ----------------------------------------------------------------

# The correlation results clustered by SOM node

# Load correlations
ALL_cor_wide <- ALL_cor %>% 
  ungroup() %>% 
  filter(Parameter1 == "sst") %>% 
  dplyr::select(region:ts, Parameter2, r, n_Obs) %>% 
  pivot_wider(values_from = r, names_from = Parameter2)

# Combine MHW metrics and correlation results
events_cor_prep <- OISST_MHW_event %>% 
  dplyr::select(region, season, event_no, duration, intensity_mean, intensity_max, 
                intensity_cumulative, rate_onset, rate_decline) %>% 
  left_join(ALL_cor_wide, by = c("region", "season", "event_no")) %>% 
  # ungroup() %>% 
  dplyr::select(region:n_Obs, sst, bottomT, sss, mld_cum, t2m, p_e_cum, mslp_cum)

# Grab only the node info
SOM_info <- SOM$info

# Join to the GLORYS MHW correlation results
events_cor_SOM <- left_join(events_cor_prep, SOM_info, by = c("region", "event_no"))

fig_7 <- events_cor_SOM %>% 
  dplyr::select(node, ts, bottomT:mslp_cum) %>% 
  group_by(node, ts) %>% 
  summarise_if(is.numeric, mean) %>% 
  pivot_longer(cols = bottomT:mslp_cum) %>%
  filter(name != "temp",
         ts != "full") %>%
  ungroup() %>% 
  mutate(node = as.factor(node),
         ts = factor(ts, levels = c("decline", "full", "onset")),
         name = case_when(name == "sst" ~ "SST",
                          name == "bottomT" ~ "Bottom",
                          name == "sss" ~ "SSS",
                          name == "mld_cum" ~ "MLD",
                          name == "t2m" ~ "Air",
                          name == "p_e_cum" ~ "P-E",
                          name == "mslp_cum" ~ "MSLP",
                          TRUE ~ name),
         name = factor(name, levels = c("P-E", "Air", "MSLP", "MLD", "SSS", "Bottom"))) %>% 
  ggplot(aes(x = name, y = ts)) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~node, labeller = labeller(node = node_labeller)) +
  scale_fill_gradient2(low = "blue", high = "red") +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL, fill = "r (mean)") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1.0),
        panel.background = element_rect(colour = "black"))
# fig_7
ggsave("figures/fig_7.png", fig_7, height = 7, width = 14)
ggsave("figures/fig_7.pdf", fig_7, height = 7, width = 14)

