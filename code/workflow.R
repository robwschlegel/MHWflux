# code/workflow.R
# This script may be run with source() in order to calculate the
# climatologies for all of the variables used in the study


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
# system.time(
# workflowr::wflow_publish(files = c("analysis/index.Rmd", 
#                                    "analysis/polygon-prep.Rmd",
#                                    "analysis/data-prep.Rmd",
#                                    "analysis/mhw-flux.Rmd"#, 
#                                    # "analysis/k-means-flux.Rmd"
#                                    ),
#                          message = "Re-built site.")
# ) # 85 seconds


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------

# See the "Preparing the data" vignette


# OISST prep --------------------------------------------------------------

# See the "Preparing the data" vignette for OISST prep


# GLORYS prep -------------------------------------------------------------

# See the "Preparing the data" vignette for GLORYS prep


# ERA5 prep ---------------------------------------------------------------

### ERA5 time series data are saved in individual variables
## For some reason these files will not allow themselves to be processed in parallel
## I suspect it is due to the size of the NetCDF files
## So we are taking a more roundabout approach

# The full list of file names
ERA5_files <- data.frame(files = c(ERA5_lhf_files, ERA5_shf_files, ERA5_lwr_files, ERA5_swr_files,
                                   ERA5_u_files, ERA5_v_files, ERA5_mslp_files, ERA5_t2m_files,
                                   ERA5_tcc_files, ERA5_pcp_files, ERA5_evp_files),
                         var_name = rep(c("lhf", "shf", "lwr", "swr", "u", "v", "mslp", 
                                          "t2m", "tcc", "pcp", "evp"), each = 26),
                         var_group = rep(c("lhf", "shf", "lwr", "swr", "u", "v", "mslp",
                                           "t2m", "tcc", "pcp", "evp"), each = 26))

# Run everything in one go
plyr::ddply(ERA5_files, c("var_group"), process_ERA5, .parallel = F) # Can't run in parallel

# Load the heat flux layers
system.time(ERA5_lhf_anom <- readRDS("data/ERA5_lhf_anom.Rda")) # 40 seconds
colnames(ERA5_lhf_anom)[6] <- "lhf"
ERA5_shf_anom <- readRDS("data/ERA5_shf_anom.Rda")
colnames(ERA5_shf_anom)[6] <- "shf"
ERA5_lwr_anom <- readRDS("data/ERA5_lwr_anom.Rda")
colnames(ERA5_lwr_anom)[6] <- "lwr"
ERA5_swr_anom <- readRDS("data/ERA5_swr_anom.Rda")
colnames(ERA5_swr_anom)[6] <- "swr"

# Combine to make Qnet
ccols <- c(1, 2, 5, 6); jcols <- c("lon", "lat", "t")
system.time(
  ERA5_qnet <- left_join(ERA5_lhf_anom[,ccols], ERA5_shf_anom[,ccols], by = jcols) %>% 
    left_join(ERA5_lwr_anom[,ccols], by = jcols) %>%
    left_join(ERA5_swr_anom[,ccols], by = jcols) %>% 
    mutate(val = lhf+shf+lwr+swr) %>% 
    dplyr::select(lon, lat, t, val)
) # 189 seconds

# Cleanup
rm(ERA5_lhf_anom, ERA5_shf_anom, ERA5_lwr_anom, ERA5_swr_anom); gc()

# Set number of cores
  # NB: This is very RAM heavy, be careful with core use
registerDoParallel(cores = 25)

# Calculate anomalies and save
system.time(
  ERA5_qnet_anom <- plyr::ddply(ERA5_qnet, c("lon", "lat"), calc_clim_anom, .parallel = T, point_accuracy = 8) %>% 
    mutate(var = "qnet") %>% 
    dplyr::select(lon, lat, var, doy, t, val, seas, thresh, anom)
) # 222 seconds
system.time(saveRDS(ERA5_qnet_anom, "data/ERA5_qnet_anom.Rda")) # 151 seconds
rm(ERA5_qnet, ERA5_qnet_anom); gc()

# Load the ts data
ERA5_lwr_ts <- readRDS("data/ERA5_lwr_ts.Rda")
ERA5_swr_ts <- readRDS("data/ERA5_swr_ts.Rda")
ERA5_lhf_ts <- readRDS("data/ERA5_lhf_ts.Rda")
ERA5_shf_ts <- readRDS("data/ERA5_shf_ts.Rda")
ERA5_tcc_ts <- readRDS("data/ERA5_tcc_ts.Rda")
ERA5_u_ts <- readRDS("data/ERA5_u_ts.Rda")
ERA5_v_ts <- readRDS("data/ERA5_v_ts.Rda")
ERA5_mslp_ts <- readRDS("data/ERA5_mslp_ts.Rda")
ERA5_t2m_ts <- readRDS("data/ERA5_t2m_ts.Rda")
ERA5_pcp_ts <- readRDS("data/ERA5_pcp_ts.Rda")
ERA5_evp_ts <- readRDS("data/ERA5_evp_ts.Rda")

# Stitch them together
join_cols <- c("region", "t")
ERA5_all_ts <- left_join(ERA5_lwr_ts, ERA5_swr_ts, by = join_cols) %>%
  left_join(ERA5_lhf_ts, by = join_cols) %>%
  left_join(ERA5_shf_ts, by = join_cols) %>%
  group_by(region, t) %>% 
  summarise_all("mean") %>% # Merge half days from time shift in loading step
  ungroup() %>% 
  left_join(ERA5_tcc_ts, by = join_cols) %>%
  left_join(ERA5_u_ts, by = join_cols) %>%
  left_join(ERA5_v_ts, by = join_cols) %>%
  left_join(ERA5_mslp_ts, by = join_cols) %>%
  left_join(ERA5_t2m_ts, by = join_cols) %>% 
  left_join(ERA5_pcp_ts, by = join_cols) %>% 
  left_join(ERA5_evp_ts, by = join_cols) %>% 
  mutate(p_e = tp+e, # All ERA5 values are positive downward, so negative evaporation shows water leaving the surface
         qnet = msnlwrf+msnswrf+mslhf+msshf,
         wind_spd = round(sqrt(u10^2 + v10^2), 2),
         wind_dir = round((270-(atan2(v10, u10)*(180/pi)))%%360))
saveRDS(ERA5_all_ts, "data/ERA5_all_ts.Rda")

# See the "Preparing the data" vignette for the code used to create the clims and anomalies


# MHW detection -----------------------------------------------------------

# See the "Preparing the data" vignette


# Heat flux vs. MHW -------------------------------------------------------

# See the "Heat flux vs. MHW" vignette


# Anomaly dataframe -------------------------------------------------------

# Loading all of the anomaly dataframes at once doesn't use up too much RAM,
# but the combining of them hangs really badly
# For that reason we are going to load and combine them one at a time,
# purging the memory as we go

# print(paste0("Began combining all anoms at ", Sys.time()))

## ERA 5
  # NB: We start with ERA 5 as it has the most pixels due to it being atmospheric
system.time(ERA5_u_anom <- load_anom("data/ERA5_u_anom.Rda")) # xxx seconds
system.time(ERA5_v_anom <- load_anom("data/ERA5_v_anom.Rda")) # xxx seconds
system.time(ALL_anom <- merge(ERA5_u_anom, ERA5_v_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # xxx seconds
rm(ERA5_u_anom, ERA5_v_anom); gc()
system.time(ERA5_t2m_anom <- load_anom("data/ERA5_t2m_anom.Rda")) # xxx seconds
system.time(ALL_anom <- merge(ALL_anom, ERA5_t2m_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # xxx seconds
rm(ERA5_t2m_anom); gc()
system.time(ERA5_qnet_anom <- load_anom("data/ERA5_qnet_anom.Rda")) # xxx seconds
system.time(ALL_anom <- merge(ALL_anom, ERA5_qnet_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # xxx seconds
rm(ERA5_qnet_anom); gc()

## GLORYS
system.time(GLORYS_all_anom <- load_anom("data/GLORYS_all_anom.Rda", GLORYS = T)) # 247 seconds
system.time(ALL_anom <- merge(ALL_anom, GLORYS_all_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # xxx seconds
rm(GLORYS_all_anom); gc()

## OISST
system.time(OISST_all_anom <- load_anom("data/OISST_all_anom.Rda")) # xxx seconds
colnames(OISST_all_anom)[4] <- "anom_sst"
system.time(ALL_anom <- merge(ALL_anom, OISST_sst_anom,
                              by = c("lon", "lat", "t"), all.x = T)) # xxx seconds
rm(OISST_all_anom); gc()

## Save
# NB: This causes RStudio server to hang, but it still works
print(paste0("Began saving all anoms at ", Sys.time()))
system.time(saveRDS(ALL_anom, "data/ALL_anom.Rda")) # xxx seconds

## Test visuals
# Load
# system.time(ALL_anom <- readRDS("data/ALL_anom.Rda")) # xxx seconds
# Plot
# ALL_anom %>%
#   filter(t == "2000-01-01") %>%
#   ggplot(aes(x = lon, y = lat)) +
#   geom_raster(aes(fill = sst_anom))


# Other dataframe ---------------------------------------------------------

# ## These data are not used in the SOM calculation,
# ## But are used to create summary figures
# 
# ## Load and merge GLORYS U and V data
# system.time(GLORYS_u_sub <- load_anom("data/base/GLORYS_u.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 42 seconds
# system.time(GLORYS_v_sub <- load_anom("data/base/GLORYS_v.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 42 seconds
# system.time(GLORYS_uv_sub <- left_join(GLORYS_u_sub, GLORYS_v_sub,
#                                        by = c("lon", "lat", "t"))) # 5 seconds
# 
# ## Load ERA 5 U and V data
# system.time(ERA5_u_sub <- load_anom("data/base/ERA5_u.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 60 seconds
# system.time(ERA5_v_sub <- load_anom("data/base/ERA5_v.Rda") %>%
#               filter(lon %in% lon_sub,
#                      lat %in% lat_sub)) # 60 seconds
# system.time(ERA5_uv_sub <- left_join(ERA5_u_sub, ERA5_v_sub,
#                                      by = c("lon", "lat", "t"))) # 7 seconds
# 
# ## Combine all
# system.time(ALL_uv_sub <- left_join(ERA5_uv_sub, GLORYS_uv_sub,
#                                     by = c("lon", "lat", "t"))) # 5 seconds
# 
# ## Load air temp data
# system.time(ERA5_t2m <- load_anom("data/base/ERA5_t2m.Rda")) # 55 seconds
# 
# # Load MSLP real and anomaly data and combine
# system.time(ERA5_mslp <- load_anom("data/base/ERA5_mslp.Rda")) # 52 seconds
# system.time(ERA5_mslp_anom <- load_anom("data/anom/ERA5_mslp_anom.Rda")) # 52 seconds
# system.time(ERA5_mslp_all <- left_join(ERA5_mslp, ERA5_mslp_anom,
#                                        by = c("lon", "lat", "t"))) # 5 seconds
# 
# ## Load SST data
# system.time(OISST_sst <- load_anom("data/base/OISST_sst.Rda", OISST = T)) # 33 seconds
# 
# ## Merge everything
# system.time(ALL_other <- left_join(ERA5_t2m, ERA5_mslp_all,
#                                    by = c("lon", "lat", "t")) %>%
#               left_join(OISST_sst, by = c("lon", "lat", "t")) %>%
#               left_join(ALL_uv_sub, by = c("lon", "lat", "t"))) # 260 seconds
# # Save
# # NB: This causes RStudio server to hang, but it still works
# # print(paste0("Began saving all other data at ", Sys.time()))
# system.time(saveRDS(ALL_other, "data/anom/ALL_other.Rda")) # 318 seconds
# 
# ## Test visuals
# # Load
# # system.time(ALL_other <- readRDS("data/anom/ALL_other.Rda")) # 29 seconds
# # Plot
# # ALL_other %>%
# #   filter(t == "2000-01-01") %>%
# #   ggplot(aes(x = lon, y = lat)) +
# #   geom_raster(aes(fill = sst)) +
# #   geom_segment(aes(xend = lon + u10 * wind_uv_scalar,
# #                    yend = lat + v10 * wind_uv_scalar),
# #                arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
# #                linejoin = "mitre", size = 0.4, alpha = 0.4) +
# #   geom_contour(aes(z = msl, colour = stat(level)), size = 1)


# Data packets ------------------------------------------------------------

# # Set number of cores
# # NB: 50 cores requires too much RAM
# doParallel::registerDoParallel(cores = 25)
# 
# ## Create one big anomaly packet from OISST data
# # print(paste0("Began creating data packets at ", Sys.time()))
# system.time(synoptic_states <- plyr::ddply(OISST_MHW_event, c("region", "event_no"),
#                                            data_packet_func, .parallel = T)) # 204 seconds
# # Save
# saveRDS(synoptic_states, "data/SOM/synoptic_states.Rda")
# 
# ## Create other synoptic states per MHW per variable
# doParallel::registerDoParallel(cores = 10) # NB: Be careful here...
# system.time(synoptic_states_other <- plyr::ddply(OISST_MHW_event, c("region", "event_no"),
#                                                  data_packet_func, .parallel = T, df = ALL_other)) # 122 seconds
# # Save
# saveRDS(synoptic_states_other, "data/SOM/synoptic_states_other.Rda")
# 
# ## Create wide data packet that is fed to SOM
# system.time(packet <- readRDS("data/SOM/synoptic_states.Rda") %>%
#               select(region, event_no, synoptic) %>%
#               unnest() %>%
#               wide_packet_func()) # 122 seconds
# # Save
# saveRDS(packet, "data/SOM/packet.Rda")

# Visualise data packets --------------------------------------------------

# Look at event sizes etc.
# OISST_MHW_event

# NB: Just pick a region and event number to save a synoptic summary of it

# # The 2012 lobster MHW
# fig_data_packet("gm",	14)
# fig_data_packet("gsl",	39)
# 
# # Smol MHWs
# fig_data_packet("cbs",	14)
# fig_data_packet("mab", 20)
# 
# # Hecking big MHWs
# fig_data_packet("mab",	23)


# SOM analysis ------------------------------------------------------------

# # OISST SOM analysis
# packet <- readRDS("data/SOM/packet.Rda")
# synoptic_states_other <- readRDS("data/SOM/synoptic_states_other.Rda")
# system.time(som <- som_model_PCI(packet, synoptic_states_other)) # 83 seconds
# saveRDS(som, file = "data/SOM/som.Rda")


# Visuals -----------------------------------------------------------------

# # OISST visuals
# som <- readRDS("data/SOM/som.Rda")
# fig_all_som(som)

