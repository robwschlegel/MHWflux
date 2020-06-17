# code/figures.R
# This script contains the code used to generate the final 
# figures and tables seen in the manuscript.


# Setup -------------------------------------------------------------------

source("code/functions.R")


# Figure 1 ----------------------------------------------------------------

# The study area polygons and MHWs detected thereien

# This figure is currently created in the MHWNWA project


# Figure 2 ----------------------------------------------------------------

# Histogram of r values

# The correlations
ALL_cor <- readRDS("data/ALL_cor.Rda") %>% 
  mutate(region = factor(region, levels = c("mab", "gm", "ss", "cbs", "gsl", "nfs")),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         Parameter2 = as.character(Parameter2)) %>% 
  filter(Parameter1 == "sst",
         ts != "full") %>% 
  mutate(Parameter2 = case_when(Parameter2 == "sst" ~ "SST",
                                Parameter2 == "bottomT" ~ "Bottom",
                                Parameter2 == "sss" ~ "SSS",
                                Parameter2 == "mld_cum" ~ "MLD",
                                Parameter2 == "mld_1_cum" ~ "MLD_1_c",
                                Parameter2 == "t2m" ~ "Air",
                                Parameter2 == "tcc_cum" ~ "Cloud",
                                Parameter2 == "p_e_cum" ~ "P_E",
                                Parameter2 == "mslp_cum" ~ "MSLP",
                                Parameter2 == "lwr_mld_cum" ~ "Qlw",
                                Parameter2 == "swr_mld_cum" ~ "Qsw",
                                Parameter2 == "lhf_mld_cum" ~ "Qlh",
                                Parameter2 == "shf_mld_cum" ~ "Qsh",
                                Parameter2 == "qnet_mld_cum" ~ "Qnet",
                                TRUE ~ Parameter2))
# Function for plotting histograms of chosen variables
hist_var <- function(var_choices, y_label){
  ALL_cor %>% 
    filter(Parameter2 %in% var_choices) %>% 
    ggplot(aes(x = r)) +
    geom_vline(aes(xintercept = 0), colour = "red", size = 1) +
    geom_histogram(bins = 20) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    facet_grid(ts ~ Parameter2) +
    labs(x = NULL, y = y_label) +
    theme(axis.title.y = element_text(size = 16)) +
    coord_fixed(ratio = 0.01)
}

hist_Q <- hist_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
hist_air <- hist_var(c("Air", "Cloud", "P_E", "MSLP"), "Atmosphere")
hist_ocean <- hist_var(c("SSS", "MLD", "Bottom"), "Ocean")

fig_2 <- ggpubr::ggarrange(hist_Q, hist_air, hist_ocean, ncol = 1, nrow = 3, align = "h",  labels = c("A)", "B)", "C)"))
fig_2
ggsave("figures/fig_2.png", fig_2, height = 10, width = 14)


# Figure 3 ----------------------------------------------------------------

# Most important variables by region/season
boxplot_var <-  function(var_choices, y_label){
  ALL_cor %>% 
    filter(Parameter2 %in% var_choices) %>% 
    ggplot(aes(x = ts, y = r)) +
    geom_hline(aes(yintercept = 0), colour = "red", size = 1) +
    geom_boxplot(aes(fill = season), notch = F) +
    facet_wrap(~Parameter2, nrow = 1) +
    # scale_y_continuous(expand = c(0, 0)) +
    # scale_x_continuous(expand = c(0, 0)) +
    # facet_grid(ts ~ Parameter2) +
    labs(x = NULL, y = y_label, fill = "Region") +
    theme(axis.title.y = element_text(size = 16)) +
    coord_equal()
}

box_Q <- boxplot_var(c("Qnet", "Qlh", "Qsh", "Qlw", "Qsw"), "Heat flux")
box_air <- boxplot_var(c("Air", "Cloud", "P_E", "MSLP"), "Atmosphere")
box_ocean <- boxplot_var(c("SSS", "MLD", "Bottom"), "Ocean")

fig_3 <- ggpubr::ggarrange(box_Q, box_air, box_ocean, ncol = 1, nrow = 3, align = "h",  
                           labels = c("A)", "B)", "C)"), common.legend = T)
fig_3
ggsave("figures/fig_3.png", fig_3, height = 10, width = 14)


# Figure 4 ----------------------------------------------------------------

# Importance of variables as MHW metrics increse

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
                lwr_mld_cum, swr_mld_cum, lhf_mld_cum, shf_mld_cum, qnet_mld_cum)

fig_4 <- events_cor_prep %>% 
  mutate(duration = plyr::round_any(duration, 10)) %>% 
  group_by(ts, duration) %>% 
  mutate(count = n()) %>% 
  summarise_if(is.numeric, mean) %>% 
  pivot_longer(cols = sst:qnet_mld_cum) %>% 
  ungroup() %>% 
  filter(name != "sst",
         ts != "full",
         name != "mld_1_cum") %>% # Only 1 event is longer than this 
  mutate(name = case_when(name == "sst" ~ "SST",
                          name == "bottomT" ~ "Bottom",
                          name == "sss" ~ "SSS",
                          name == "mld_cum" ~ "MLD",
                          name == "mld_1_cum" ~ "MLD_1_c",
                          name == "t2m" ~ "Air",
                          name == "tcc_cum" ~ "Cloud",
                          name == "p_e_cum" ~ "P_E",
                          name == "mslp_cum" ~ "MSLP",
                          name == "lwr_mld_cum" ~ "Qlw",
                          name == "swr_mld_cum" ~ "Qsw",
                          name == "lhf_mld_cum" ~ "Qlh",
                          name == "shf_mld_cum" ~ "Qsh",
                          name == "qnet_mld_cum" ~ "Qnet",
                          TRUE ~ name),
         name = factor(name, levels = rev(c("Qnet", "Qlw", "Qsw", "Qlh", "Qsh", "Cloud", 
                                        "P_E", "Air", "MSLP", "MLD", "SSS", "Bottom")))) %>% 
  ggplot(aes(x = duration, y = name)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = count)) +
  facet_wrap(~ts) +
  scale_fill_gradient2(low = "blue", high = "red") +
  coord_cartesian(expand = F) +
  labs(y = NULL, x = "Duration (10 day bins)", fill = "r (mean)") +
  theme(legend.position = "bottom")
# fig_4
ggsave("figures/fig_4.png", height = 8, width = 10)


# Figure 5 ----------------------------------------------------------------

# SOM region + season panels.
# Created in the MHWNWA project.


# Figure 6 ----------------------------------------------------------------

# SOM atmosphere panels.
# Created in the MHWNWA project.


# Figure 7 ----------------------------------------------------------------

# The correlation results clustered by SOM node

# Load the SOM from the MHWNWA
SOM <- readRDS("../MHWNWA/data/SOM/som.Rda")

# Grab only the node info
SOM_info <- SOM$info

# Join to the GLORYS MHW correlation results
events_cor_SOM <- left_join(events_cor_prep, SOM_info, by = c("region", "event_no"))

fig_7 <- events_cor_SOM %>% 
  dplyr::select(node, ts, bottomT:qnet_mld_cum, -mld_1_cum) %>% 
  group_by(node, ts) %>% 
  summarise_if(is.numeric, mean) %>% 
  pivot_longer(cols = bottomT:qnet_mld_cum) %>%
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
                          name == "lwr_mld_cum" ~ "Qlw",
                          name == "swr_mld_cum" ~ "Qsw",
                          name == "lhf_mld_cum" ~ "Qlh",
                          name == "shf_mld_cum" ~ "Qsh",
                          name == "qnet_mld_cum" ~ "Qnet",
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
        axis.text.x = element_text(angle = 30, hjust = 1.0))
# fig_7
ggsave("figures/fig_7.png", fig_7, height = 6, width = 12)
