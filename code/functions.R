# code/functions.R
# This script holds all of the functions called from other scripts in the code/ folder


## Ideas
# MHW intensity globally is heterogenous
# We also know that there are different primary drivers in different parts of the world
# So can this knowledge be used to identify the drivers of the most extreme events?
# Can the risk of certain drivers be quantified/ordered?
# Can the increase in these with climate change be measured?
# Following on, can we then say with any confidence what aspects of anthropogenic
# climate change pose the greatest risk(s) to the oceans w.r.t. MHWs?

# The K-means for heat budget fingerprints is also a solid plan

# Could one use a random forest to predict a MHW given environmental anomalies?
# Could the random forest model be used to determine the most useful variables
# Can one predict duration/intensity?


# Required bits -----------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

# Packages used in this script
library(tidyverse) # Base suite of functions
library(lubridate) # For convenient date manipulation
library(heatwaveR)
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(tidync)
library(correlation)
library(ggraph)
library(Metrics) # For RMSE calculations

# Set number of cores
library(doParallel); registerDoParallel(cores = 50)

# Disable scientific notation for numeric values
# I just find it annoying
options(scipen = 999)

# File locations
OISST_files <- dir("../data/OISST", full.names = T, pattern = "avhrr")
GLORYS_files <- dir("../data/GLORYS", full.names = T, pattern = "MHWflux")
ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")[1:26]
ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")[1:26]
ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")[1:26]
ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")[1:26]
ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")[15:40]
ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")[15:40]
ERA5_mslp_files <- dir("../../oliver/data/ERA/ERA5/MSLP", full.names = T, pattern = "ERA5")[15:40]
ERA5_t2m_files <- dir("../../oliver/data/ERA/ERA5/T2M", full.names = T, pattern = "ERA5")[15:40]
ERA5_tcc_files <- dir("../../oliver/data/ERA/ERA5/CLOUD/", full.names = T, pattern = "ERA5")[1:26]
ERA5_pcp_files <- dir("../../oliver/data/ERA/ERA5/PRCP", full.names = T, pattern = "ERA5")[15:40]
ERA5_evp_files <- dir("../../oliver/data/ERA/ERA5/EVAP", full.names = T, pattern = "ERA5")[15:40]

# Corners of the study area
  # Created in 'analysis/polygon-prep.Rmd'
NWA_corners <- readRDS("metadata/NWA_corners.Rda")

# Individual regions
  # Created in 'analysis/polygon-prep.Rmd'
NWA_coords <- readRDS("metadata/NWA_coords.Rda")

# The different product grids
OISST_grid <- readRDS("metadata/OISST_grid.Rda")
GLORYS_grid <- readRDS("metadata/GLORYS_grid.Rda")

# The pixels in each region
  # Created in 'analysis/data-prep.Rmd'
OISST_regions <- readRDS("metadata/OISST_regions.Rda")
GLORYS_regions <- readRDS("metadata/GLORYS_regions.Rda")

# OISST MHW results
OISST_region_MHW <- readRDS("data/OISST_region_MHW.Rda")

# OISST MHW Clims
OISST_MHW_clim <- OISST_region_MHW %>%
  select(-cats) %>%
  unnest(events) %>%
  filter(row_number() %% 2 == 1) %>%
  unnest(events)

# OISST events
OISST_MHW_event <- OISST_region_MHW %>%
  select(-cats) %>%
  unnest(events) %>%
  filter(row_number() %% 2 == 0) %>%
  unnest(events) %>% 
  mutate(month_peak = lubridate::month(date_peak, label = T),
         season = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                            month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                            month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                            month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn"),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))) %>%
  select(-month_peak)

# OISST MHW Categories
OISST_MHW_cats <- OISST_region_MHW %>%
  select(-events) %>%
  unnest(cats)

# Physical variable anomalies
ALL_anom <- readRDS("data/ALL_anom.Rda")
ALL_anom_cum <- readRDS("data/ALL_anom_cum.Rda")

# Combine the anomaly dataframes into one
ALL_anom_full <- rbind(ALL_anom[,c("region", "var", "t", "anom")], 
                       ALL_anom_cum[,c("region", "var", "t", "anom")])
ALL_anom_full_wide <- ALL_anom_full %>% 
  pivot_wider(values_from = anom, names_from = var)

# The base land polygon
  # Created in 'analysis/polygon-prep.Rmd'
map_base <- readRDS("metadata/map_base.Rda")

# The base map frame used for all figures
frame_base <- ggplot(map_base, aes(x = lon, y = lat)) +
  scale_x_continuous(breaks = seq(-70, -50, 10),
                     labels = c("70°W", "60°W", "50°W"),
                     position = "top") +
  scale_y_continuous(breaks = c(40, 50),
                     labels = scales::unit_format(suffix = "°N", sep = "")) +
  coord_cartesian(xlim = c(NWA_corners[1:2]), ylim = c(NWA_corners[3:4]), expand = F) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))

# Easy join for DOY values
doy_index <- data.frame(doy_int = 1:366,
                        doy = format(seq(as.Date("2016-01-01"),
                                         as.Date("2016-12-31"),
                                         by = "day"), "%m-%d"),
                        stringsAsFactors = F)

# Establish the vector scalar for the currents
current_uv_scalar <- 4

# Establish the vector scalar for the wind
wind_uv_scalar <- 0.5

# Reduced wind/ current vector grid
lon_sub <- seq(NWA_corners[1], NWA_corners[2], by = 1)
lat_sub <- seq(NWA_corners[3], NWA_corners[4], by = 1)

# Bathymetry data
  # Created in MHWNWA/analysis/polygon-prep.Rmd
bathy <- readRDS("metadata/NWA_bathy_lowres.Rda")

# The OISST land mask
# land_mask_OISST <- readRDS("data/land_mask_OISST.Rda")

# Filter it to the smaller domain only
# land_mask_OISST_sub <- land_mask_OISST%>%
#   filter(lon >= NWA_corners[1], lon <= NWA_corners[2],
#          lat >= NWA_corners[3], lat <= NWA_corners[4])


# Extract data from OISST NetCDF ------------------------------------------

load_OISST <- function(file_name){
  res <- tidync(file_name) %>%
    hyper_filter(lat = dplyr::between(lat, NWA_corners[3], NWA_corners[4]),
                 time = dplyr::between(time, as.integer(as.Date("1993-01-01")),
                                       as.integer(as.Date("2018-12-31")))) %>%
    hyper_tibble() %>%
    mutate(time = as.Date(time, origin = "1970-01-01")) %>%
    dplyr::rename(temp = sst, t = time) %>%
    select(lon, lat, t, temp)
  return(res)
}


# Extract data from GLORYS NetCDF -----------------------------------------

# testers...
# file_name <- "../data/GLORYS/MHWflux_GLORYS_1993-1.nc"
# tidync(file_name)
# ncdump::NetCDF(file_name)$variable[,1:6]
load_GLORYS <- function(file_name, region = F){
  # SST, U, V, SSS
  res1 <- tidync(file_name) %>% 
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time * 3600, origin = '1950-01-01', tz = "GMT"))) %>%
    dplyr::select(-depth)
  # MLD, bottomT, SSH
  res2 <- tidync(file_name) %>%
    activate("D2,D1,D0") %>% # Need to explicitly grab the other data as they don't use the depth dimension
    hyper_tibble() %>%
    mutate(time = as.Date(as.POSIXct(time*3600, origin = '1950-01-01', tz = "GMT"))) %>% 
    dplyr::select(-siconc, -usi, -sithick, -vsi)
  # Combine, filter by region, and create means
  res <- left_join(res1, res2, by = c("longitude", "latitude", "time")) %>% 
    dplyr::rename(lon = longitude, lat = latitude, t = time,
                  temp = thetao, sss = so, u = uo, v = vo,
                  mld = mlotst, ssh = zos) %>% 
    mutate(temp = round(temp, 4), bottomT = round(bottomT, 4),
           mld = round(mld, 4), ssh = round(ssh, 4),
           u = round(u, 6), v = round(v, 6)) %>% 
    dplyr::select(lon, lat, t, temp, bottomT, mld, sss, ssh, u, v)
  rm(res1, res2); gc()
  if(region){
    res_mean <- res %>% 
      right_join(GLORYS_regions, by = c("lon", "lat")) %>%
      na.omit() %>% 
      group_by(region, t) %>%
      dplyr::select(-lon, -lat) %>% 
      summarise_all("mean") %>%
      ungroup()
  } else{
    res_mean <- res %>% 
      right_join(GLORYS_grid, by = c("lon", "lat")) %>%
      na.omit() %>% 
      dplyr::select(lon_OISST, lat_OISST, t, mld, u, v) %>% 
      group_by(lon_OISST, lat_OISST, t) %>%
      summarise_all("mean") %>%
      ungroup() %>% 
      rename(lon = lon_OISST, lat = lat_OISST)
  }
  rm(res); gc()
  return(res_mean)
}

# Test visuals
# res_mean %>%
#   filter(t == "1993-01-31") %>%
#   ggplot(aes(x = lon, y = lat, colour = mld)) +
#   # geom_raster()
#   geom_point()


# Extract data from ERA 5 NetCDF ------------------------------------------

# The ERA5 data need to be loaded from midday to midday
# The final midday is the date for that averaged datum

# Function for loading a single ERA 5 NetCDF file
# The ERA5 data are saved as annual single variables
# testers...
# file_name <- "../../oliver/data/ERA/ERA5/LWR/ERA5_LWR_1993.nc"
# ncdump::NetCDF(file_name)$variable[1:6]
load_ERA5 <- function(file_name, time_shift = 0, region = F){
  # The base data
  res <- tidync(file_name) %>%
    hyper_filter(longitude = dplyr::between(longitude, NWA_corners[1]-0.5+360, NWA_corners[2]+0.5+360),
                 latitude = dplyr::between(latitude, NWA_corners[3]-0.5, NWA_corners[4]+0.5)) %>%
    hyper_tibble() %>%
    dplyr::rename(lon = longitude, lat = latitude, t = time) %>%
    mutate(lon = if_else(lon > 180, lon-360, lon)) %>% # Shift to +- 180 scale
    mutate(lon = lon+0.125, lat = lat-0.125) # Regrid to match OISST coords
  # Means based on region pixels
  if(region){
    res_mean <- res %>% 
      right_join(OISST_regions, by = c("lon", "lat")) %>%
      na.omit() %>% 
      mutate(t = as.POSIXct(t * 3600, origin = '1900-01-01', tz = "GMT")) %>%
      mutate(t = t+time_shift) %>% # Time shift
      mutate(t = as.Date(t)) %>%
      dplyr::select(-lon, -lat) %>% 
      group_by(region, t) %>% 
      summarise_all("mean") %>% 
      ungroup()
  # Means based on study area pixels
  } else{
    res_mean <- res %>% 
      right_join(OISST_grid, by = c("lon", "lat")) %>%
      na.omit() %>% 
      mutate(t = as.POSIXct(t * 3600, origin = '1900-01-01', tz = "GMT")) %>%
      mutate(t = t+time_shift) %>% # Time shift
      mutate(t = as.Date(t)) %>%
      group_by(lon, lat, t) %>% 
      summarise_all("mean") %>% 
      ungroup()
  }
  return(res_mean)
}

# Test visuals
# res_mean %>%
#   filter(t == "1993-01-31") %>%
#   ggplot(aes(x = lon, y = lat, fill = msnlwrf )) +
#   geom_raster()


# Calculate clims and anoms in gridded data -------------------------------

# This function expects to be given only one pixel/ts at a time
calc_clim_anom <- function(df, point_accuracy){
  res <- ts2clm(df, y = val, roundClm = point_accuracy,
                climatologyPeriod = c("1993-01-01", "2018-12-25"))
  res$anom <- round(res$val-res$seas, point_accuracy)
  return(res)
}


# Correlation functions ---------------------------------------------------

# Convenience wrapper for RMSE calculations
rmse_wrap <- function(df_sub){
  df_res <- df_sub %>% 
    summarise(qnet_budget = rmse(sst, qnet_budget),
              lhf_budget = rmse(sst, lhf_budget),
              shf_budget = rmse(sst, shf_budget),
              lwr_budget = rmse(sst, lwr_budget),
              swr_budget = rmse(sst, swr_budget)) %>% 
    pivot_longer(cols = everything(), names_to = "Parameter2", values_to = "rmse") %>% 
    mutate(Parameter1 = "sst")
}

# Subset data based on events and regions and run all correlations
# This also runs RMSE for the Qx terms
cor_all <- function(df, df_event){
  
  # Get the info for the focus event
  event_sub <- df_event %>% 
    filter(event_no == df$event_no[1],
           region == df$region[1])
  
  # Subset the time series for the onset and decline portions
  ts_full <- ALL_anom_full_wide %>% 
    filter(t >= event_sub$date_start,
           t <= event_sub$date_end,
           region == event_sub$region) %>% 
    mutate(qnet_budget = c(0, qnet_mld_cum[1:event_sub$duration-1]) + sst[1],
           lhf_budget = c(0, lhf_mld_cum[1:event_sub$duration-1]) + sst[1],
           shf_budget = c(0, shf_mld_cum[1:event_sub$duration-1]) + sst[1],
           lwr_budget = c(0, lwr_mld_cum[1:event_sub$duration-1]) + sst[1],
           swr_budget = c(0, swr_mld_cum[1:event_sub$duration-1]) + sst[1])
  ts_onset <- ts_full %>% 
    filter(t >= event_sub$date_start,
           t <= event_sub$date_peak)
  ts_decline <- ts_full %>% 
    filter(t >= event_sub$date_peak,
           t <= event_sub$date_end)
  # Run the correlations
  R2_full <- broom::glance(lm(sst ~ t, ts_full))
  ts_full_cor <- correlation(ts_full, redundant = T) %>% 
    mutate(ts = "full",
           sst_R2 = round(R2_full$r.squared, 4),
           sst_p = round(R2_full$p.value, 4)) %>% 
    left_join(rmse_wrap(ts_full), by = c("Parameter1", "Parameter2"))
  if(nrow(ts_onset) > 2){
    R2_onset <- broom::glance(lm(sst ~ t, ts_onset))
    ts_onset_cor <- correlation(ts_onset, redundant = T) %>% 
      mutate(ts = "onset",
             sst_R2 = round(R2_onset$r.squared, 4),
             sst_p = round(R2_onset$p.value, 4)) %>% 
      left_join(rmse_wrap(ts_onset), by = c("Parameter1", "Parameter2"))
  } else {
    ts_onset_cor <- ts_full_cor[0,]
  }
  if(nrow(ts_decline) > 2){
    R2_decline <- broom::glance(lm(sst ~ t, ts_decline))
    ts_decline_cor <- correlation(ts_decline, redundant = T) %>% 
      mutate(ts = "decline",
             sst_R2 = round(R2_decline$r.squared, 4),
             sst_p = round(R2_decline$p.value, 4)) %>% 
      left_join(rmse_wrap(ts_decline), by = c("Parameter1", "Parameter2"))
  } else {
    ts_decline_cor <- ts_full_cor[0,]
  }
  
  # Combine and finish
  ts_cor <- rbind(ts_full_cor, ts_onset_cor, ts_decline_cor) %>% 
    mutate(p = round(p, 4),
           r = round(r, 2),
           CI_low = round(CI_low, 2),
           CI_high = round(CI_high, 2),
           t = round(t, 4),
           rmse = round(rmse, 4),
           ts = factor(ts, levels = c("onset", "full", "decline")))
}


# K-means functions -------------------------------------------------------

# Nobody here but us chickens...

