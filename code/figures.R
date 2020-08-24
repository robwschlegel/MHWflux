# code/figures.R
# This script contains the code used to generate the final 
# figures and tables seen in the manuscript.


# Setup -------------------------------------------------------------------

source("code/functions.R")

# The correlations
ALL_cor_fig <- readRDS("data/ALL_cor.Rda") %>% 
  mutate(region = toupper(region)) %>% 
  mutate(region = factor(region, levels = c("MAB", "GM", "SS", "CBS", "GSL", "NFS")),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         Parameter2 = as.character(Parameter2)) %>% 
  filter(Parameter1 == "sst") %>% #,
  # ts != "full") %>% 
  mutate(Parameter2 = case_when(Parameter2 == "sst" ~ "SST",
                                Parameter2 == "bottomT" ~ "Bottom",
                                Parameter2 == "sss" ~ "SSS",
                                Parameter2 == "mld_cum" ~ "MLD",
                                Parameter2 == "mld_1_cum" ~ "MLD_1",
                                Parameter2 == "t2m" ~ "Air",
                                Parameter2 == "tcc_cum" ~ "Cloud",
                                Parameter2 == "p_e_cum" ~ "P-E",
                                Parameter2 == "mslp_cum" ~ "MSLP",
                                Parameter2 == "lwr_budget" ~ "Qlw",
                                Parameter2 == "swr_budget" ~ "Qsw",
                                Parameter2 == "lhf_budget" ~ "Qlh",
                                Parameter2 == "shf_budget" ~ "Qsh",
                                Parameter2 == "qnet_budget" ~ "Qnet",
                                TRUE ~ Parameter2),
         Parameter2 = factor(Parameter2, levels = c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw",
                                                    "Air", "Cloud", "P-E", "MSLP",
                                                    "SST", "SSS", "MLD", "MLD_1", "Bottom")))


# Figure 1 ----------------------------------------------------------------

# The study area polygons and MHWs detected thereien

# Convert study area labels to uppercase
NWA_coords$region <- toupper(NWA_coords$region)

# Reorder site labels
NWA_coords$region <- factor(NWA_coords$region, levels = c("MAB", "GM", "SS", "GSL", "CBS", "NFS"))

# Event count by region 
season_count <- OISST_MHW_event %>% 
  group_by(season) %>% 
  summarise(count = n(), .groups = "drop")

# Event count by region 
region_count <- OISST_MHW_event %>% 
  group_by(region) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  mutate(region = toupper(region))

# Create labels for number of MHWs per region
region_prop_label <- NWA_coords %>%
  left_join(region_count, by = "region") %>%
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
  geom_polygon(aes(colour = region, fill = region), size = 1.5, alpha = 0.2) +
  geom_polygon(data = map_base, aes(group = group), show.legend = F) +
  geom_label(data = region_prop_label, label.size = 1.5, show.legend = F,
             aes(x = lon_center, y = lat_center, label = count, colour = region)) +
  geom_label(data = filter(season_count, season == "Spring"), 
             aes(x = -55, y = 41, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count, season == "Summer"), 
             aes(x = -55, y = 40, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count, season == "Autumn"), 
             aes(x = -55, y = 39, label = paste0(season,": ",count))) +
  geom_label(data = filter(season_count, season == "Winter"), 
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


# Figure 2 ----------------------------------------------------------------

# Boxplots showing range of RMSE results per region and season

# Prep data
ALL_RMSE_fig <- ALL_cor_fig %>% 
  filter(rmse > 0 ) %>% 
  dplyr::select(region, season, ts, Parameter2, rmse)

# Region boxplot
fig_2a <- ALL_RMSE_fig %>% 
  ggplot(aes(x = Parameter2, y = rmse)) +
  geom_boxplot(aes(fill = ts)) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~region) +
  labs(x = NULL, y = "RMSE", fill = "Time series") +
  theme(panel.background = element_rect(colour = "black"))
# fig_2a

# Season boxplot
fig_2b <- ALL_RMSE_fig %>% 
  ggplot(aes(x = Parameter2, y = rmse)) +
  geom_boxplot(aes(fill = ts)) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~season) +
  labs(x = NULL, y = "RMSE", fill = "Time series") +
  theme(panel.background = element_rect(colour = "black"))
# fig_2b

# Combine and save
fig_2 <- ggpubr::ggarrange(fig_2a, fig_2b, ncol = 2, nrow = 1, labels = c("A)", "B)"), 
                          widths = c(3, 2), align = "h", common.legend = T)
# fig_2
ggsave("figures/fig_2.png", fig_2, height = 5, width = 10)


# Figure 3 ----------------------------------------------------------------

# Histogram of r values

# Function for plotting histograms of chosen variables
hist_var <- function(var_choices, y_label){
  ALL_cor_fig %>% 
    filter(Parameter2 %in% var_choices) %>% 
    ggplot(aes(x = r)) +
    geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
    geom_histogram(bins = 10) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-0.9, -0.5, 0, 0.5, 0.9)) +
    facet_grid(ts ~ Parameter2) +
    coord_fixed(ratio = 0.01) +
    labs(x = NULL, y = y_label) +
    theme(axis.title.y = element_text(size = 16),
          panel.background = element_rect(colour = "black"))
}

# The three figures
hist_Q <- hist_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
hist_air <- hist_var(c("Air", "P-E", "MSLP"), "Atmosphere")
hist_ocean <- hist_var(c("SSS", "MLD", "Bottom"), "Ocean")

# Combine bottom two
hist_bottom <- ggpubr::ggarrange(hist_air, hist_ocean, ncol = 2, nrow = 1, align = "h",  labels = c("B)", "C)"))

# The final figure
fig_3 <- ggpubr::ggarrange(hist_Q, hist_bottom, ncol = 1, nrow = 2, align = "h",  labels = c("A)"))
# fig_3
ggsave("figures/fig_3.png", fig_3, height = 7, width = 10)


# Figure 4 ----------------------------------------------------------------

# Most important variables by region/season
boxplot_var <-  function(var_choices, y_label){
  ALL_cor_fig %>% 
    filter(Parameter2 %in% var_choices) %>% 
    ggplot(aes(x = ts, y = r)) +
    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
    geom_boxplot(aes(fill = season), notch = F) +
    # geom_violin(aes(fill = season)) + # Looks bad
    facet_wrap(~Parameter2, nrow = 1) +
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

box_Q <- boxplot_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
box_air <- boxplot_var(c("Air", "P-E", "MSLP"), "Atmosphere")
box_ocean <- boxplot_var(c("SSS", "MLD", "Bottom"), "Ocean")

# Combine bottom two
box_bottom <- ggpubr::ggarrange(box_air, box_ocean, ncol = 2, nrow = 1, align = "h",  labels = c("B)", "C)"), legend = "none")

# The final figure
fig_4 <- ggpubr::ggarrange(box_Q, box_bottom, ncol = 1, nrow = 2, align = "h",  labels = c("A)"), common.legend = T)
# fig_4
ggsave("figures/fig_4.png", fig_4, height = 5, width = 10)


# Figure 5 ----------------------------------------------------------------

# SOM region + season panels.
# Created in the MHWNWA project.

# I'm thinking of combining these two figures: 5 + 6

# Figure 6 ----------------------------------------------------------------

# SOM atmosphere panels.
# Created in the MHWNWA project.


# Figure 7 ----------------------------------------------------------------

# The correlation results clustered by SOM node

# Load correlations
ALL_cor_wide <- readRDS("data/ALL_cor.Rda") %>% 
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
  dplyr::select(region:n_Obs, sst, bottomT, sss, mld_cum, mld_1_cum, t2m, tcc_cum, p_e_cum, mslp_cum,
                lwr_budget, swr_budget, lhf_budget, shf_budget, qnet_budget)


# Load the SOM from the MHWNWA
SOM <- readRDS("data/som.Rda")

# Grab only the node info
SOM_info <- SOM$info

# Join to the GLORYS MHW correlation results
events_cor_SOM <- left_join(events_cor_prep, SOM_info, by = c("region", "event_no"))

fig_7 <- events_cor_SOM %>% 
  dplyr::select(node, ts, bottomT:qnet_budget, -mld_1_cum) %>% 
  group_by(node, ts) %>% 
  summarise_if(is.numeric, mean) %>% 
  pivot_longer(cols = bottomT:qnet_budget) %>%
  filter(name != "temp",
         ts != "full") %>%
  ungroup() %>% 
  mutate(node = as.factor(node),
         ts = factor(ts, levels = c("decline", "full", "onset")),
         name = case_when(name == "sst" ~ "SST",
                          name == "bottomT" ~ "Bottom",
                          name == "sss" ~ "SSS",
                          name == "mld_cum" ~ "MLD",
                          name == "mld_1_cum" ~ "MLD_1_c",
                          name == "t2m" ~ "Air",
                          name == "tcc_cum" ~ "Cloud",
                          name == "p_e_cum" ~ "P_E",
                          name == "mslp_cum" ~ "MSLP",
                          name == "lwr_budget" ~ "Qlw",
                          name == "swr_budget" ~ "Qsw",
                          name == "lhf_budget" ~ "Qlh",
                          name == "shf_budget" ~ "Qsh",
                          name == "qnet_budget" ~ "Qnet",
                          TRUE ~ name),
         name = factor(name, levels = c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh", "Cloud", 
                                        "P_E", "Air", "MSLP", "MLD", "SSS", "Bottom"))) %>% 
  ggplot(aes(x = name, y = ts)) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~node, scales = "free") +
  scale_fill_gradient2(low = "blue", high = "red") +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL, fill = "r (mean)") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1.0),
        panel.background = element_rect(colour = "black"))
# fig_7
ggsave("figures/fig_7.png", fig_7, height = 7, width = 14)

