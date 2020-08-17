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
library(data.table)
library(lubridate) # For convenient date manipulation
library(heatwaveR)
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(tidync)
library(correlation)
library(ggraph)
library(yasomi)
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
ERA5_grid <- readRDS("metadata/ERA5_grid.Rda")

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
ALL_ts_anom <- readRDS("data/ALL_ts_anom.Rda")
ALL_ts_anom_cum <- readRDS("data/ALL_ts_anom_cum.Rda")

# Combine the anomaly dataframes into one
ALL_ts_anom_full <- rbind(ALL_ts_anom[,c("region", "var", "t", "anom")], 
                          ALL_ts_anom_cum[,c("region", "var", "t", "anom")])
ALL_ts_anom_full_wide <- ALL_ts_anom_full %>% 
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

# Reduced wind/current vector grid
lon_sub <- seq(NWA_corners[1], NWA_corners[2], by = 1)+0.125
lat_sub <- seq(NWA_corners[3], NWA_corners[4], by = 1)-0.125

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
# file_name <- "../../oliver/data/ERA/ERA5/EVAP/ERA5_EVAP_1993.nc"
# ncdump::NetCDF(file_name)$variable[1:6]
load_ERA5 <- function(file_name, time_shift = 0){
  res <- tidync(file_name) %>%
    hyper_filter(longitude = dplyr::between(longitude, NWA_corners[1]-0.5+360, NWA_corners[2]+0.5+360),
                 latitude = dplyr::between(latitude, NWA_corners[3]-0.5, NWA_corners[4]+0.5)) %>%
    hyper_tibble() %>%
    dplyr::rename(lon = longitude, lat = latitude, t = time) %>%
    mutate(lon = if_else(lon > 180, lon-360, lon)) %>% # Shift to +- 180 scale
    mutate(lon = lon+0.125, lat = lat-0.125) %>% # Regrid to match OISST coords
    right_join(ERA5_grid, by = c("lon", "lat")) %>%
    na.omit() %>% 
    mutate(t = as.POSIXct(t * 3600, origin = '1900-01-01', tz = "GMT")) %>%
    mutate(t = t+time_shift) %>% # Time shift
    mutate(t = as.Date(t))
  # Switch to data.table for faster means
  res_dt <- data.table(res)
  setkey(res_dt, lon, lat, t)
  res_mean <- res_dt[, lapply(.SD, mean), by = list(lon, lat, t)]
  return(res_mean)
}

# Test visuals
# res_mean %>%
#   filter(t == "1993-02-01") %>%
#   ggplot(aes(x = lon, y = lat, fill = msnlwrf )) +
#   geom_raster()

# Function for processing ERA5 data
# file_df <- filter(ERA5_files, var_group == "evp")
process_ERA5 <- function(file_df){
  
  # Find the necessary time shift
  if(file_df$var_name[1] %in% c("lhf", "shf", "lwr", "swr")){
    var_shift = 43200
  } else{
    var_shift = 0
  }
  
  # The base data
  print(paste0("Began loading ",file_df$var_name[1]," at ", Sys.time()," with a ",var_shift," time shift"))
  # system.time(
  res_base <- plyr::ldply(file_df$files, load_ERA5, .parallel = F, .progress = "text", time_shift = var_shift)
  # ) # 66 seconds for one, 378 seconds for 4
  
  # Combine the little half days
  print(paste0("Began meaning ",file_df$var_name[1]," at ", Sys.time()))
  res_dt <- data.table(res_base)
  setkey(res_dt, lon, lat, t)
  res_mean <- res_dt[, lapply(.SD, mean), by = list(lon, lat, t)]
  
  # The clims+anoms
  print(paste0("Began anoms on ",file_df$var_name[1]," at ", Sys.time()))
  # system.time(
  res_anom <- res_mean %>% 
    pivot_longer(cols = c(-lon, -lat, -t), names_to = "var", values_to = "val") %>% 
    plyr::ddply(., c("lon", "lat", "var"), calc_clim_anom, .parallel = T, point_accuracy = 8)
  # ) # 53 seconds for four
  saveRDS(res_anom, paste0("data/ERA5_",file_df$var_name[1],"_anom.Rda"))
  
  # The time series
  print(paste0("Began ts for ",file_df$var_name[1]," at ", Sys.time()))
  # system.time(
  res_ts <- res_mean %>% 
    right_join(OISST_regions, by = c("lon", "lat")) %>%
    na.omit() %>% 
    dplyr::select(-lon, -lat) %>% 
    group_by(region, t) %>% 
    summarise_all("mean") %>% 
    ungroup()
  # ) # 1 second for one, 2 seconds for four
  saveRDS(res_ts, paste0("data/ERA5_",file_df$var_name[1],"_ts.Rda"))
  rm(res_base, res_anom, res_ts); gc()
  return()
}

# test visuals
# ggplot(data = filter(res_anom, t == "1993-01-01"), aes(x = lon, y = lat)) + 
  # geom_tile(aes(fill = anom))
# ggplot(data = res_ts, aes(x = t, y = mslhf)) +
  # geom_line()


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

# Load anomaly data -------------------------------------------------------

# This function loads anomaly data and converts it to a data.table for combining with others
load_anom <- function(file_name, GLORYS = F){
  res <- readRDS(file_name)
  if(GLORYS){
    res_sub <- res %>% 
      dplyr::select(lon, lat, t, var, anom) %>% 
      filter(var %in% c("u", "v", "mld")) %>% 
      pivot_wider(id_cols = c(lon, lat, t), values_from = anom, 
                  names_from = var, names_prefix = "anom_")
  } else {
    val_col <- res$var[1]
    res_sub <- res %>% 
      dplyr::select(lon, lat, t, anom) %>% 
      `colnames<-`(c("lon", "lat", "t", paste0("anom_", val_col)))
  }
  res_dt <- setkey(data.table(res_sub, key = c("lon", "lat", "t")))
  rm(res, res_sub); gc()
  return(res_dt)
}


# Build data packets ------------------------------------------------------

# testers...
# event_sub <- OISST_MHW_event[1,]
data_packet_func <- function(event_sub, df = ALL_anom){
  
  # Filter base anomaly range
  packet_base <- df %>%
    filter(t >= event_sub$date_start, t <= event_sub$date_end,
           lon >= NWA_corners[1], lon <= NWA_corners[2],
           lat >= NWA_corners[3], lat <= NWA_corners[4])
  rm(df); gc()
  
  # Create mean synoptic values
  packet_mean <- packet_base %>%
    # select(-doy) %>%
    group_by(lon, lat) %>%
    summarise_all(mean, na.rm = T) %>%
    arrange(lon, lat) %>%
    ungroup()
  packet_mean[is.na(packet_mean)] <- NA
  packet_mean <- nest(packet_mean, .key = "synoptic")
  
  # Combine results with MHW dataframe
  packet_res <- cbind(event_sub, packet_mean)
  
  # Test visuals
  ggplot(packet_mean, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = anom_sst)) +
    scale_fill_gradient2(low = "blue", high = "red") +
    coord_cartesian(xlim = NWA_corners[1:2],
                    ylim = NWA_corners[3:4], expand = F)
  
  # Exit
  return(packet_res)
}

# Function for casting wide the data packets
wide_packet_func <- function(df){
  
  # Cast the data to a single row
  res <- select(df, -t) %>%
    data.table::data.table() %>%
    reshape2::melt(id = c("region", "event_no", "lon", "lat"),
                   measure = c(colnames(.)[-c(1:4)]),
                   variable.name = "var", value.name = "val") %>%
    dplyr::arrange(var, lon, lat) %>%
    unite(coords, c(lon, lat, var), sep = "BBB") %>%
    unite(event_ID, c(region, event_no), sep = "BBB") %>%
    reshape2::dcast(event_ID ~ coords, value.var = "val")
  
  # Remove columns (pixels) with missing data
  res_fix <- res[,colSums(is.na(res))<1]
  
  # Remove columns (pixels) with no variance
  # This may occur in pixels where there is no variance in MLD anomaly
  no_var <- data.frame(min = sapply(res_fix[,-1], min),
                       max = sapply(res_fix[,-1], max)) %>%
    mutate(col_name = row.names(.)) %>%
    filter(min == max)
  res_filter <- res_fix[,!(colnames(res_fix) %in% no_var$col_name)]
  
  # Exit
  return(res_filter)
}

# Visualise a single data packet ------------------------------------------

# This function will create a summary figure for a single data packet
# It is based on 'data/synoptic_states.Rda' and 'data/SOM/synoptic_states_other.Rda'
#testers...
# event_region <- "gm"
# event_num <- 14
fig_data_packet <- function(event_region, event_num){
  
  # Load and prep mean anom states
  if(!exists("synoptic_states")){
    synoptic_states <- readRDS("data/synoptic_states.Rda")
  }
  if(!exists("synoptic_states_other")){
    synoptic_states_other <- readRDS("data/synoptic_states_other.Rda")
  }
  single_packet <- synoptic_states %>%
    filter(region == event_region,
           event_no == event_num) %>%
    unnest(cols = "synoptic")
  single_packet_other <- synoptic_states_other %>%
    filter(region == event_region,
           event_no == event_num) %>%
    unnest(cols = "synoptic")
  single_packet_full <- left_join(single_packet, single_packet_other)
  
  # Reduce wind/ current vectors
  lon_sub <- seq(min(single_packet_full$lon), max(single_packet_full$lon), by = 1)
  lat_sub <- seq(min(single_packet_full$lat), max(single_packet_full$lat), by = 1)
  vec_sub <- single_packet_full %>%
    filter(lon %in% lon_sub, lat %in% lat_sub) #%>%
    # na.omit()
  
  # Establish the vector scalar for the currents
  current_uv_scalar <- 4
  
  # SUbset region polygon
  NWA_coords_sub <- NWA_coords %>%
    filter(region == event_region)
  
  # Load and prep SST time series
  OISST_region <- readRDS("data/OISST_all_ts.Rda")
  OISST_region_sub <- OISST_region %>%
    filter(region == event_region) %>%
    ts2clm(climatologyPeriod = c("1993-01-01", "2018-12-25")) %>%
    filter(t >= single_packet_full$date_start[1]-5,
           t <= single_packet_full$date_end[1]+5)
  
  # MHW segment data.frame
  OISST_segments <- data.frame(date_start = single_packet_full$date_start[1],
                               date_end = single_packet_full$date_end[1],
                               date_peak = single_packet_full$date_peak[1],
                               temp_start = OISST_region_sub$temp[OISST_region_sub$t == single_packet_full$date_start[1]],
                               temp_end = OISST_region_sub$temp[OISST_region_sub$t == single_packet_full$date_end[1]],
                               temp_peak = OISST_region_sub$temp[OISST_region_sub$t == single_packet_full$date_peak[1]])
  
  # Determine date breaks
  if(nrow(OISST_region_sub) < 60){
    date_breaks_op <- "1 week"
    date_labels_op <- "%d %b %Y"
  } else if(nrow(OISST_region_sub) < 120){
    date_breaks_op <- "1 month"
    date_labels_op <- "%b %Y"
  } else if(nrow(OISST_region_sub) >= 120){
    date_breaks_op <- "2 months"
    date_labels_op <- "%b %Y"
  }
  
  # SST ts
  ts_panel <- ggplot(data = OISST_region_sub, aes(x = t, y = temp)) +
    geom_flame(aes(y2 = thresh), fill = "salmon") +
    geom_line() +
    geom_line(aes(y = seas), col = "skyblue") +
    geom_line(aes(y = thresh), col = "navy") +
    geom_segment(data = OISST_segments, colour = "limegreen",
                 size = 2, lineend = "round",
                 aes(x = date_start-1, xend = date_start-1,
                     y = temp_start-0.5, yend = temp_start+0.5)) +
    geom_segment(data = OISST_segments, colour = "limegreen",
                 size = 2, lineend = "round",
                 aes(x = date_end+1, xend = date_end+1,
                     y = temp_end-0.5, yend = temp_end+0.5)) +
    # geom_segment(data = OISST_segments, colour = "deeppink3",
    #              size = 2, lineend = "round",
    #              aes(x = date_peak, xend = date_peak,
    #                  y = temp_peak-0.5, yend = temp_peak+0.5)) +
    scale_x_date(expand = c(0, 0), date_breaks = date_breaks_op, date_labels = date_labels_op, ) +
    labs(x = NULL, y = "Temp. (°C)  ") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
  # ts_panel
  
  # SST + U + V (anom)
  sst_u_v_anom <- frame_base +
    geom_raster(data = single_packet_full, aes(fill = anom_sst)) +
    # The bathymetry
    stat_contour(data = bathy[bathy$depth > -2000,],
                 aes(x = lon, y = lat, z = depth), alpha = 0.5,
                 colour = "black", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = vec_sub,
                 aes(xend = lon + anom_u * current_uv_scalar,
                     yend = lat + anom_v * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # The region polygon
    geom_polygon(data = NWA_coords_sub, aes(group = region),
                 fill = NA, colour = "darkmagenta", size = 1.5, alpha = 1) +
    # Diverging gradient
    scale_fill_gradient2(name = "SST\nanom. (°C)", low = "blue", high = "red") +
    theme(legend.position = "bottom")
  # sst_u_v_anom
  
  # Air temp + U10 + V10 + MSLP
  air_u_v_mslp_anom <- frame_base +
    # The air temperature
    geom_raster(data = single_packet_full, aes(fill = anom_t2m)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The mean sea level pressure contours
    geom_contour(data = single_packet_full,
                 aes(z = anom_mslp, colour = stat(level)), size = 1.5) +
    # The wind vectors
    geom_segment(data = vec_sub,
                 aes(xend = lon + anom_u10 * wind_uv_scalar,
                     yend = lat + anom_v10 * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # The region polygon
    geom_polygon(data = NWA_coords_sub, aes(group = region),
                 fill = NA, colour = "darkmagenta", size = 1.5, alpha = 1) +
    # Colour scale
    scale_fill_gradient2(name = "Air temp.\nanom. (°C)", low = "blue", high = "red") +
    scale_colour_gradient2("MSLP anom.\n(hPa)", #guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    theme(legend.position = "bottom")
  # air_u_v_mslp_anom
  
  # Qnet + MLD
  qnet_mld_anom <- frame_base +
    # The MLD
    geom_raster(data = single_packet_full, aes(fill = anom_mld)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # The net downward heat flux contours
    geom_contour(data = single_packet_full, binwidth = 50,
                 aes(z = anom_qnet, colour = stat(level)), size = 1.5) +
    # The region polygon
    geom_polygon(data = NWA_coords_sub, aes(group = region),
                 fill = NA, colour = "darkmagenta", size = 1.5, alpha = 1) +
    # Colour scale
    scale_fill_gradient2("MLD\nanom. (m)",low = "blue", high = "red") +
    scale_colour_gradient2("Qnet anom.\n(W/m2)", #guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    theme(legend.position = "bottom")
  # qnet_mld_anom
  
  # Merge the panels together
  bottom_row <- cowplot::plot_grid(sst_u_v_anom, air_u_v_mslp_anom, qnet_mld_anom, labels = c('B)', 'C)', 'D)'),
                                   align = 'h', rel_widths = c(1, 1, 1), nrow = 1)
  top_row <- cowplot::plot_grid(NULL, ts_panel, NULL, labels = c('', 'A)', ''), nrow = 1, rel_widths = c(1,2,1))
  fig_all <- cowplot::plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1.4, 3))
  # fig_all
  ggsave(fig_all, filename = paste0("output/synoptic_",event_region,"_",event_num,".png"), height = 7, width = 16)
  # ggsave(fig_all, filename = paste0("talk/graph/synoptic_",event_region,"_",event_num,".pdf"), height = 6, width = 15)
}


# Run SOM and create summary output ---------------------------------------

som_model_PCI <- function(data_packet, other_states, xdim = 4, ydim = 3){
  # Create a scaled matrix for the SOM
  # Cancel out first column as this is the reference ID of the event per row
  data_packet_matrix <- as.matrix(scale(data_packet[,-1]))
  
  # Create the grid that the SOM will use to determine the number of nodes
  som_grid <- somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal")
  
  # Run the SOM with PCI
  som_model <- batchsom(data_packet_matrix,
                        somgrid = som_grid,
                        init = "pca",
                        max.iter = 100)
  
  # Create a data.frame of info
  node_info <- data.frame(event_ID = data_packet[,"event_ID"],
                          node = som_model$classif) %>%
    separate(event_ID, into = c("region", "event_no"), sep = "BBB") %>%
    group_by(node) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    mutate(event_no = as.numeric(as.character(event_no))) %>%
    left_join(select(OISST_MHW_cats, region, event_no, category, peak_date),
              by = c("region", "event_no")) %>%
    mutate(month_peak = lubridate::month(peak_date, label = T),
           season_peak = case_when(month_peak %in% c("Jan", "Feb", "Mar") ~ "Winter",
                                   month_peak %in% c("Apr", "May", "Jun") ~ "Spring",
                                   month_peak %in% c("Jul", "Aug", "Sep") ~ "Summer",
                                   month_peak %in% c("Oct", "Nov", "Dec") ~ "Autumn")) %>%
    select(-peak_date, -month_peak)
  
  # Determine which event goes in which node and melt
  data_packet_long <- cbind(node = som_model$classif, data_packet) %>%
    separate(event_ID, into = c("region", "event_no"), sep = "BBB") %>%
    data.table() %>%
    reshape2::melt(id = c("node", "region", "event_no"),
                   measure = c(colnames(.)[-c(1:3)]),
                   variable.name = "variable", value.name = "value")
  
  # Create the mean values that serve as the unscaled results from the SOM
  node_data <- data_packet_long %>%
    group_by(node, variable) %>%
    summarise(val = mean(value, na.rm = T),
              sd = sd(value, na.rm = T)) %>%
    ungroup() %>%
    separate(variable, into = c("lon", "lat", "var"), sep = "BBB") %>%
    dplyr::arrange(node, var, lon, lat) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat),
           val = round(val, 4),
           sd = round(sd, 4))
  
  # Load the other synoptic state data as necessary
  # system.time(
  synoptic_states_other_unnest <- other_states %>%
    select(region, event_no, synoptic) %>%
    unnest(cols = "synoptic")
  # ) # 5 seconds
  
  # Create mean node states for data not used in the calculation
  other_data <- data_packet_long %>%
    select(node, region, event_no) %>%
    unique() %>%
    mutate(event_no = as.numeric(event_no)) %>%
    left_join(synoptic_states_other_unnest, by = c("region", "event_no")) %>%
    select(-t, -region, -event_no) %>%
    gather(key = "var", value = "value", -c(node:lat)) %>%
    group_by(node, var, lon, lat) %>%
    summarise(val = mean(value, na.rm = T),
              sd = sd(value, na.rm = T)) %>%
    ungroup() %>%
    dplyr::arrange(node, var, lon, lat) %>%
    mutate(val = round(val, 4),
           sd = round(sd, 4))
  
  # ANOSIM for goodness of fit for node count
  # node_data_wide <- node_data %>%
  #   unite(coords, c(lon, lat, var), sep = "BBB") %>%
  #   dplyr::select(-sd) %>%
  #   data.table() %>%
  #   dcast(node~coords, value.var = "val")
  
  # Calculate similarity
  # NB: As of Nov 2019 Vegan changed the anosim function
  # it will no longer run if all values are unique, which they are here
  # som_anosim <- vegan::anosim(as.matrix(node_data_wide[,-"node"]),
  #                             node_data_wide$node, distance = "euclidean")$signif
  
  # Combine and exit
  res <- list(data = node_data,
              other_data = other_data,
              info = node_info)#,
  #ANOSIM = paste0("p = ",som_anosim))
  return(res)
}


# Figure data processing --------------------------------------------------

fig_data_prep <- function(data_packet, region_MHW){
  
  # Prep MHW event data
  region_MHW_event <- region_MHW %>%
    select(-cats) %>%
    unnest(events) %>%
    filter(row_number() %% 2 == 0) %>%
    unnest(events)
  
  ## Cast the data wide
  # Base data
  som_data_wide <- data_packet$data %>%
    dplyr::select(-sd) %>%
    spread(var, val) #%>%
  # mutate(mld_anom_cut = cut(mld_anom, breaks = seq(-0.5, 0.5, 0.1)))
  other_data_wide <- data_packet$other_data %>%
    dplyr::select(-sd) %>%
    spread(var, val)
  # Variance data
  som_sd_wide <- data_packet$data %>%
    dplyr::select(-val) %>%
    spread(var, sd) #%>%
  # mutate(mld_anom_cut = cut(mld_anom, breaks = seq(-0.5, 0.5, 0.1)))
  other_sd_wide <- data_packet$other_data %>%
    dplyr::select(-val) %>%
    spread(var, sd)
  
  # currents <- currents[(currents$lon %in% lon_sub & currents$lat %in% lat_sub),]
  som_data_sub <- som_data_wide %>%
    select(node, lon, lat, anom_u, anom_v, anom_u10, anom_v10) %>%
    filter(lon %in% lon_sub, lat %in% lat_sub) #%>%
  # mutate(arrow_size = 0.1)
  # Creating dynamic arrow sizes does not work as ggplot cannot match up the vectors correctly
  
  # MHW season of (peak) occurrence and other meta-data
  region_MHW_meta <- region_MHW_event %>%
    left_join(data_packet$info, by = c("region", "event_no"))
  
  # Grid of complete node x season matrix
  node_season_grid <- expand.grid(seq(1:max(region_MHW_meta$node, na.rm = T)),
                                  c("Summer", "Autumn", "Winter", "Spring"),
                                  stringsAsFactors = F) %>%
    dplyr::rename(node = Var1, season_peak = Var2)
  
  # Proportion of MHWs in each season in each node
  node_season_info <- region_MHW_meta %>%
    dplyr::select(region:event_no, node:season_peak) %>%
    group_by(node, season_peak) %>%
    mutate(node_season_count = n(),
           node_season_prop = round(n()/count, 2)) %>%
    select(node, count, season_peak:node_season_prop) %>%
    unique() %>%
    na.omit() %>%
    right_join(node_season_grid, by = c("node", "season_peak")) %>%
    mutate(node_season_count = ifelse(is.na(node_season_count), 0, node_season_count),
           node_season_prop = ifelse(is.na(node_season_prop), 0, node_season_prop)) %>%
    # Fill holes in count column created by right_join
    group_by(node) %>%
    mutate(count = max(count, na.rm = T)) %>%
    ungroup()
  
  # Grid of complete node x region matrix
  node_region_grid <- expand.grid(seq(1:max(region_MHW_meta$node, na.rm = T)),
                                  unique(region_MHW_meta$region),
                                  stringsAsFactors = F) %>%
    dplyr::rename(node = Var1, region = Var2)
  
  # Proportion of MHWs in each region in each node
  node_region_info <- region_MHW_meta %>%
    dplyr::select(region:event_no, node:count) %>%
    group_by(node, region) %>%
    mutate(node_region_count = n(),
           node_region_prop = round(n()/count, 2)) %>%
    select(node, count, region, node_region_count, node_region_prop) %>%
    unique() %>%
    na.omit() %>%
    right_join(node_region_grid, by = c("node", "region")) %>%
    mutate(node_region_count = ifelse(is.na(node_region_count), 0, node_region_count),
           node_region_prop = ifelse(is.na(node_region_prop), 0, node_region_prop)) %>%
    # Fill holes in count column created by right_join
    group_by(node) %>%
    mutate(count = max(count, na.rm = T)) %>%
    ungroup()
  
  # Create labels for number of states per region per node
  region_prop_label <- NWA_coords %>%
    group_by(region) %>%
    mutate(lon_center = mean(lon), lat_center = mean(lat)) %>%
    left_join(node_region_info, by = "region") %>%
    na.omit()
  
  # Calculate mean and median intensities/durations per node for plotting
  node_h_lines <- region_MHW_meta %>%
    group_by(node) %>%
    summarise(mean_int_cum = mean(intensity_cumulative, na.rm = T),
              median_int_cum = median(intensity_cumulative, na.rm = T),
              mean_int_max = mean(intensity_max, na.rm = T),
              median_int_max = median(intensity_max, na.rm = T),
              mean_dur = mean(duration, na.rm = T),
              median_dur = median(duration, na.rm = T),
              .groups = "drop") %>%
    mutate_all(round, digits = 2)
  
  # Find the eddy tracks present during each MHW
  # eddy_data <- data_packet$info %>%
  #   left_join(region_MHW_event, by = c("region", "event_no")) %>%
  #   group_by(node, region, event_no) %>%
  #   nest() %>%
  #   mutate(res = map(data, eddy_track_extract)) %>%
  #   select(-data) %>%
  #   unnest(cols = "res") %>%
  #   mutate(cyclonic_type = factor(cyclonic_type, labels = c("Cyclonic", "Anticyclonic")))
  
  # Combine and exit
  res <- list(som_data_wide = som_data_wide,
              som_data_sub = som_data_sub,
              other_data_wide = other_data_wide,
              som_sd_wide = som_sd_wide,
              other_sd_wide = other_sd_wide,
              region_MHW_meta = region_MHW_meta,
              node_season_info = node_season_info,
              node_region_info = node_region_info,
              region_prop_label = region_prop_label,
              node_h_lines = node_h_lines)#,
              # eddy_data = eddy_data)
  return(res)
}


# Create summary figures of all nodes together ----------------------------

# testers...
# som_packet <- readRDS("data/som.Rda")
# region_MHW <- readRDS("data/OISST_region_MHW.Rda")
# col_num = 4
# fig_height = 9
# fig_width = 13
fig_all_som <- function(som_packet,
                        col_num = 4,
                        fig_height = 9,
                        fig_width = 13){
  
  # Base data for all figures
  base_data <- fig_data_prep(data_packet = som_packet, region_MHW = OISST_region_MHW)
  
  # Events per region and season per node
  region_season <- fig_region_season(base_data, col_num, fig_height, fig_width)
  
  # SST + U + V (anom)
  sst_u_v_anom <- fig_sst_u_v_anom(base_data, col_num, fig_height, fig_width)
  
  # Air Temp + U + V (anom)
  air_u_v_mslp_anom <- fig_air_u_v_mslp_anom(base_data, col_num, fig_height, fig_width)
  
  # Net downward heat flux and MLD (anom)
  qnet_mld_anom <- fig_qnet_mld_anom(base_data, col_num, fig_height, fig_width)
  
  # SST + U + V (real)
  sst_u_v_real <- fig_sst_u_v_real(base_data, col_num, fig_height, fig_width)
  
  # Air Temp + U + V (real)
  air_u_v_mslp_real <- fig_air_u_v_mslp_real(base_data, col_num, fig_height, fig_width)
  
  # Lollis showing cum. int. + season
  cum_int_season <- fig_cum_int_season(base_data, col_num, fig_height, fig_width)
  
  # Lollis showing max. int. + region
  max_int_region <- fig_max_int_region(base_data, col_num, fig_height, fig_width)
  
  # Lollis showing duration + rate onset
  duration_rate_onset <- fig_duration_rate_onset(base_data, col_num, fig_height, fig_width)
  
  # Create SD per pixel plots for env. variables
  sd_col_names <- c(colnames(base_data$som_data_wide),
                    colnames(base_data$other_data_wide))
  sd_col_names <- sd_col_names[-which(sd_col_names %in% c("node", "lon", "lat"))]
  plyr::l_ply(sd_col_names, .fun = fig_sd, .parallel = T,
              fig_data = base_data, col_num = col_num, fig_height = fig_height, fig_width = fig_width)
  
  # Create individual node summaries
  # doMC::registerDoMC(cores = 4)
  plyr::l_ply(1:max(base_data$region_MHW_meta$node, na.rm = T), .fun = fig_single_node, .parallel = T,
              fig_packet = base_data, fig_height = fig_height, fig_width = fig_width)
}


# Create a summary figure for a chosen node -------------------------------
## NB: This function contains objects created in "IMBeR_2019_figures.R"

# testers...
# fig_packet <- fig_data_prep(readRDS("data/SOM/som.Rda"))
# fig_packet <- base_data
# node_number = 8
# fig_height = 9
# fig_width = 13
fig_single_node <- function(node_number, fig_packet, fig_height, fig_width){
  
  # Filter out non-target nodes
  fig_packet$som_data_wide <- filter(fig_packet$som_data_wide, node == node_number)
  fig_packet$som_data_sub <- filter(fig_packet$som_data_sub, node == node_number)
  fig_packet$other_data_wide <- filter(fig_packet$other_data_wide, node == node_number)
  fig_packet$region_MHW_meta <- filter(fig_packet$region_MHW_meta, node == node_number)
  fig_packet$node_season_info <- filter(fig_packet$node_season_info, node == node_number)
  fig_packet$node_region_info <- filter(fig_packet$node_region_info, node == node_number)
  fig_packet$region_prop_label <- filter(fig_packet$region_prop_label, node == node_number)
  fig_packet$node_h_lines <- filter(fig_packet$node_h_lines, node == node_number)
  fig_packet$eddy_data <- filter(fig_packet$eddy_data, node == node_number)
  
  # Events per region and season per node
  region_season_sub <- fig_region_season(fig_packet, 1, fig_height, fig_width)
  
  # SST + U + V (anom)
  sst_u_v_anom_sub <- fig_sst_u_v_anom(fig_packet, 1, fig_height, fig_width)
  
  # Air Temp + U + V (anom)
  air_u_v_mslp_anom_sub <- fig_air_u_v_mslp_anom(fig_packet, 1, fig_height, fig_width)
  
  # Net downward heat flux and MLD (anom)
  qnet_mld_anom_sub <- fig_qnet_mld_anom(fig_packet, 1, fig_height, fig_width)
  
  # SST + U + V (real)
  sst_u_v_real_sub <- fig_sst_u_v_real(fig_packet, 1, fig_height, fig_width)
  
  # Air Temp + U + V (real)
  air_u_v_mslp_real_sub <- fig_air_u_v_mslp_real(fig_packet, 1, fig_height, fig_width)
  
  # Lollis showing cum.int. + season
  cum_int_season_sub <- fig_cum_int_season(fig_packet, 1, fig_height, fig_width)
  
  # Lollis showing max.int + region
  max_int_region_sub <- fig_max_int_region(fig_packet, 1, fig_height, fig_width)
  
  # Lollis showing duration + rate onset
  duration_rate_onset_sub <- fig_duration_rate_onset(fig_packet, 1, fig_height, fig_width)
  
  # Create title
  title <- cowplot::ggdraw() + cowplot::draw_label(paste0("Node: ",node_number), fontface = 'bold')
  
  # Stick them together
  fig_all_sub <- cowplot::plot_grid(region_season_sub,
                                    sst_u_v_anom_sub, air_u_v_mslp_anom_sub,
                                    qnet_mld_anom_sub,
                                    sst_u_v_real_sub, air_u_v_mslp_real_sub,
                                    cum_int_season_sub, max_int_region_sub,
                                    duration_rate_onset_sub,
                                    labels = c('A)', 'B)', 'C)', 'D)', 'E)', 'F)', 'G)', 'H)', 'I)'),
                                    nrow = 3, rel_heights = c(1, 1, 0.8), align = "h")
  fig_all_title <- cowplot::plot_grid(title, fig_all_sub, ncol = 1, rel_heights = c(0.05, 1))
  # fig_all_title
  # ggsave(fig_all_title, filename = paste0("output/node_",node_number,"_panels.png"), height = 12, width = 16)
  ggsave(fig_all_title, filename = paste0("output/SOM/node_",node_number,"_panels_",product,".pdf"),
         height = fig_height+5, width = fig_width+3)
  ggsave(fig_all_title, filename = paste0("output/SOM/node_",node_number,"_panels_",product,".png"),
         height = fig_height+5, width = fig_width+3)
}


# Counts per region and season figure -------------------------------------

# testers...
# fig_data <- fig_data_prep(readRDS("data/SOM/som.Rda"))
# col_num = 4
fig_region_season <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  region_season <- frame_base +
    # The regions
    geom_polygon(data = fig_data$region_prop_label,
                 aes(group = region, fill = node_region_prop), colour = "black") +
    # Eddy tracks
    # NB: This looks aweful. I turned it off for some figures for a talk
    # geom_point(data = fig_data$eddy_data, aes(x = lon, y = lat),
    # colour = "darkorange", alpha = 0.3, size = 0.1) +
    # The base map
    geom_polygon(data = map_base, aes(group = group), show.legend = F) +
    # Count per region
    geom_label(data = fig_data$region_prop_label,
               aes(x = lon_center, y = lat_center, label = node_region_count)) +
    # Overall node count
    geom_label(data = fig_data$region_prop_label,
               aes(x = -68, y = 36, label = paste0("n = ",count))) +
    # Winter count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Winter"),
               aes(x = -58, y = 38, fill = node_season_prop,
                   label = paste0("Winter\n n = ",node_season_count))) +
    # Spring count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Spring"),
               aes(x = -48, y = 38, fill = node_season_prop,
                   label = paste0("Spring\n n = ",node_season_count))) +
    # Summer count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Summer"),
               aes(x = -58, y = 34, fill = node_season_prop,
                   label = paste0("Summer\n n = ",node_season_count))) +
    # Autumn count
    geom_label(data = filter(fig_data$node_season_info, season_peak == "Autumn"),
               aes(x = -48, y = 34, fill = node_season_prop,
                   label = paste0("Autumn\n n = ",node_season_count))) +
    # Colour scale
    scale_fill_distiller("Proportion\nof events",
                         palette = "BuPu", direction = 1) +
    theme(legend.position = "bottom")
  if(col_num != 1){
    region_season <- region_season +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/region_season.pdf"), region_season, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/region_season.png"), region_season, height = fig_height, width = fig_width)
  }
  return(region_season)
}


# SST + U + V (anom) figure -----------------------------------------------

fig_sst_u_v_anom <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  sst_u_v_anom <- frame_base +
    # The ocean temperature
    geom_raster(data = fig_data$som_data_wide, aes(fill = anom_sst)) +
    # The region polygons
    geom_polygon(data = NWA_coords, aes(group = region),
                 fill = NA, colour = "black", size = 1, alpha = 0.2) +
    # The bathymetry
    stat_contour(data = bathy[bathy$depth > -2000,],
                 aes(x = lon, y = lat, z = depth), alpha = 0.5,
                 colour = "black", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = fig_data$som_data_sub,
                 aes(xend = lon + anom_u * current_uv_scalar,
                     yend = lat + anom_v * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.8) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # Diverging gradient
    scale_fill_gradient2(name = "SST\nanom. (°C)", low = "blue", high = "red") +
    theme(legend.position = "bottom")
  if(col_num != 1){
    sst_u_v_anom <- sst_u_v_anom +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/sst_u_v_anom_.pdf"), sst_u_v_anom, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/sst_u_v_anom_.png"), sst_u_v_anom, height = fig_height, width = fig_width)
  }
  return(sst_u_v_anom)
}


# Air temp + U + V + MSLP (anom) figure -----------------------------------

fig_air_u_v_mslp_anom <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  air_u_v_mslp_anom <- frame_base +
    # The air temperature
    geom_raster(data = fig_data$som_data_wide, aes(fill = anom_t2m)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The mean sea level pressure contours
    geom_contour(data = fig_data$other_data_wide,
                 aes(z = anom_mslp, colour = stat(level)), size = 1) +
    # The wind vectors
    geom_segment(data = fig_data$som_data_sub,
                 aes(xend = lon + anom_u10 * wind_uv_scalar,
                     yend = lat + anom_v10 * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # Colour scale
    scale_fill_gradient2(name = "Air temp.\nanom. (°C)", low = "blue", high = "red") +
    scale_colour_gradient2("MSLP anom.\n(hPa)",# guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    theme(legend.position = "bottom")#, legend.box = "vertical")
  if(col_num != 1){
    air_u_v_mslp_anom <- air_u_v_mslp_anom +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/air_u_v_mslp_anom_.pdf"), air_u_v_mslp_anom, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/air_u_v_mslp_anom_.png"), air_u_v_mslp_anom, height = fig_height, width = fig_width)
  }
  return(air_u_v_mslp_anom)
}


# QNET + MLD (anom) figure ------------------------------------------------

fig_qnet_mld_anom <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  qnet_mld_anom <- frame_base +
    # The MLD
    geom_raster(data = fig_data$som_data_wide, aes(fill = anom_mld)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # The net downward heat flux contours
    geom_contour(data = fig_data$som_data_wide, binwidth = 50,
                 aes(z = anom_qnet, colour = stat(level)), size = 1) +
    # Colour scale
    scale_fill_gradient2("MLD\nanom. (m)",low = "blue", high = "red") +
    scale_colour_gradient2("Qnet anom.\n(W/m2)", #guide = "legend",
                           low = "green", mid = "grey", high = "yellow") +
    theme(legend.position = "bottom")
  if(col_num != 1){
    qnet_mld_anom <- qnet_mld_anom +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/qnet_mld_anom_.pdf"), qnet_mld_anom, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/qnet_mld_anom_.png"), qnet_mld_anom, height = fig_height, width = fig_width)
  }
  return(qnet_mld_anom)
}


# SST + U + V (real) figure -----------------------------------------------

fig_sst_u_v_real <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  sst_u_v_real <- frame_base +
    # The ocean temperature
    geom_raster(data = fig_data$other_data_wide, aes(fill = sst)) +
    # The bathymetry
    # stat_contour(data = bathy[bathy$depth < -100 & bathy$depth > -300,],
    # aes(x = lon, y = lat, z = depth), alpha = 0.5,
    # colour = "ivory", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
    # The current vectors
    geom_segment(data = fig_data$other_data_wide,
                 aes(xend = lon + u * current_uv_scalar,
                     yend = lat + v * current_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.8) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
    # Diverging gradient
    scale_fill_viridis_c(name = "SST (°C)", option = "D") +
    theme(legend.position = "bottom")
  if(col_num != 1){
    sst_u_v_real <- sst_u_v_real +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/sst_u_v_real_.pdf"), sst_u_v_real, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/sst_u_v_real_.png"), sst_u_v_real, height = fig_height, width = fig_width)
  }
  return(sst_u_v_real)
}


# Air temp + U + V + MSLP (real) figure -----------------------------------

fig_air_u_v_mslp_real <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  air_u_v_mslp_real <- frame_base +
    # The air temperature
    geom_raster(data = fig_data$other_data_wide, aes(fill = t2m)) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    # The mean sea level pressure contours
    geom_contour(data = fig_data$other_data_wide,
                 aes(z = msl, colour = stat(level)), size = 1) +
    # The wind vectors
    geom_segment(data = fig_data$other_data_wide,
                 aes(xend = lon + u10 * wind_uv_scalar,
                     yend = lat + v10 * wind_uv_scalar),
                 arrow = arrow(angle = 40, length = unit(0.1, "cm"), type = "open"),
                 linejoin = "mitre", size = 0.4, alpha = 0.4) +
    # Colour scale
    scale_fill_viridis_c(name = "Air temp. (°C)", option = "A") +
    scale_colour_gradient("MSLP (hPa)", #guide = "legend",
                          low = "white", high =  "black") +
    theme(legend.position = "bottom")
  if(col_num != 1){
    air_u_v_mslp_real <- air_u_v_mslp_real +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/air_u_v_mslp_real_.pdf"), air_u_v_mslp_real, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/air_u_v_mslp_real_.png"), air_u_v_mslp_real, height = fig_height, width = fig_width)
  }
  return(air_u_v_mslp_real)
}


# SD figures --------------------------------------------------------------

fig_sd <- function(col_name, fig_data, col_num, fig_height, fig_width){
  
  # Pick correct data.frame from list
  if({{col_name}} %in% colnames(fig_data$som_sd_wide)){
    sub_data <- fig_data$som_sd_wide
  } else{
    sub_data <- fig_data$other_sd_wide
  }
  
  # The figure
  sd_heat_map <- frame_base +
    # The MLD
    geom_raster(data = sub_data, aes_string(fill = {{col_name}})) +
    # The land mass
    geom_polygon(data = map_base, aes(group = group), alpha = 1,
                 fill = "grey80", colour = "black", size = 0.5, show.legend = FALSE) +
    # Colour scale
    scale_fill_gradient2(paste0("SD: ",{{col_name}}),low = "white", high = "red") +
    # The facets
    facet_wrap(~node, ncol = col_num) +
    # Etc.
    theme(legend.position = "bottom")
  if(col_num != 1){
    ggsave(paste0("output/SOM/",col_name,"_sd.pdf"), sd_heat_map, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/",col_name,"_sd.png"), sd_heat_map, height = fig_height, width = fig_width)
  }
  # return(sd_heat_map)
}


# Cum. int. + season lolli ------------------------------------------------

fig_cum_int_season <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  cum_int_seas <- ggplot(data = fig_data$region_MHW_meta,
                         aes(x = date_peak, y = intensity_cumulative)) +
    # Count label
    # geom_label(aes(x = mean(range(date_peak)),
    #                y = max(intensity_cumulative),
    #                label = paste0("n = ", count))) +
    # Mean stat label
    # geom_label(data = fig_data$node_h_lines,
    #            aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
    #                y = max(fig_data$OISST_MHW_meta$intensity_cumulative)*0.9,
    #                label = paste0("mean = ", mean_int_cum))) +
    # Median stat label
    # geom_label(data = fig_data$node_h_lines,
  #            aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
  #                y = max(fig_data$OISST_MHW_meta$intensity_cumulative)*0.8,
  #                label = paste0("median = ", median_int_cum))) +
  geom_lolli(aes(colour = season_peak), size = 2) +
    # geom_point(aes(colour = season_peak), size = 2) +
    geom_smooth(method = "lm", se = F, aes(colour = season_peak), size = 2) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_int_cum), linetype = "dashed") +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = median_int_cum), linetype = "dotted") +
    scale_colour_brewer(palette = "Set1") +
    scale_x_date(labels = scales::date_format("%Y"),
                 date_breaks = "2 years", date_minor_breaks = "1 year") +
    labs(x = "", y = "Cum. intensity (°C x days)", colour = "Season") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30))
  if(col_num != 1){
    cum_int_seas <- cum_int_seas +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/cum_int_season.pdf"), cum_int_seas, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/cum_int_season.png"), cum_int_seas, height = fig_height, width = fig_width)
  }
  return(cum_int_seas)
}


# Max. int. + region lolli ------------------------------------------------

fig_max_int_region <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  max_int_region <- ggplot(data = fig_data$region_MHW_meta,
                           aes(x = date_peak, y = intensity_max)) +
    # Count label
    # geom_label(aes(x = mean(range(date_peak)),
    #                y = max(intensity_max),
    #                label = paste0("n = ", count))) +
    # Mean stat label
    # geom_label(data = fig_data$node_h_lines,
    #            aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
    #                y = max(fig_data$OISST_MHW_meta$intensity_max)*0.9,
    #                label = paste0("mean = ", mean_int_max))) +
    # Median stat label
    # geom_label(data = fig_data$node_h_lines,
  #            aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
  #                y = max(fig_data$OISST_MHW_meta$intensity_max)*0.8,
  #                label = paste0("median = ", median_int_max))) +
  geom_lolli(aes(colour = region), size = 2) +
    # geom_point(aes(colour = region), size = 2) +
    geom_smooth(method = "lm", se = F, aes(colour = region), size = 2) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_int_max), linetype = "dashed") +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = median_int_max), linetype = "dotted") +
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Dark2')[c(1,2,4,5,3,6)]) +
    scale_x_date(labels = scales::date_format("%Y"),
                 date_breaks = "2 years", date_minor_breaks = "1 year") +
    labs(x = "", y = "Max. intensity (°C)", colour = "Region") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30))
  if(col_num != 1){
    max_int_region <- max_int_region +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/max_int_region_.pdf"), max_int_region, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/max_int_region_.png"), max_int_region, height = fig_height, width = fig_width)
  }
  return(max_int_region)
}


# Rate onset + duration lolli ---------------------------------------------

fig_duration_rate_onset <- function(fig_data, col_num, fig_height, fig_width){
  # The figure
  duration_rate_onset <- ggplot(data = fig_data$region_MHW_meta,
                                aes(x = date_peak, y = duration)) +
    # geom_smooth(method = "lm", se = F, aes(colour = region)) +
    # Count label
    # geom_label(aes(x = mean(range(date_peak)),
    #                y = max(duration),
    #                label = paste0("n = ", count))) +
    # Mean stat label
    # geom_label(data = fig_data$node_h_lines,
    #            aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
    #                y = max(fig_data$OISST_MHW_meta$duration)*0.9,
    #                label = paste0("mean = ", mean_dur))) +
    # Median stat label
  # geom_label(data = fig_data$node_h_lines,
  #            aes(x = mean(range(fig_data$OISST_MHW_meta$date_peak)),
  #                y = max(fig_data$OISST_MHW_meta$duration)*0.8,
  #                label = paste0("median = ", median_dur))) +
  geom_lolli() +
    geom_point(aes(colour = rate_onset)) +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = mean_dur), linetype = "dashed") +
    geom_hline(data = fig_data$node_h_lines, aes(yintercept = median_dur), linetype = "dotted") +
    scale_x_date(labels = scales::date_format("%Y"),
                 date_breaks = "2 years", date_minor_breaks = "1 year") +
    scale_colour_gradient(low = "white", high = "darkorange") +
    labs(x = "", y = "Duration (days)", colour = "Rate onset (°C x days)") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 30))
  if(col_num != 1){
    duration_rate_onset <- duration_rate_onset +
      facet_wrap(~node, ncol = col_num)
    ggsave(paste0("output/SOM/duration_rate_onset.pdf"), duration_rate_onset, height = fig_height, width = fig_width)
    ggsave(paste0("output/SOM/duration_rate_onset.png"), duration_rate_onset, height = fig_height, width = fig_width)
  }
  return(duration_rate_onset)
}


# Summary stat heat map ---------------------------------------------------

# Show with this figure the different mean and median values for the main
# summary stats as a proportion (0 -- 1) so that nodes may be quickly
# compared visually
fig_heat_stat <- function(){
  heat_data <- fig_data_prep(readRDS("data/som.Rda"))$node_h_lines
  heat_data_long <- heat_data %>%
    gather(key = "stat", value = "val", -node) %>%
    group_by(stat) %>%
    mutate(prop = val/max(val))
  heat_stat <- ggplot(heat_data_long, aes(x = 1, y = stat)) +
    geom_tile(aes(fill = prop), colour = "black") +
    geom_hline(aes(yintercept = 3.5)) +
    scale_fill_gradient(low = "white", high = "red") +
    facet_wrap(~node, ncol = 4) +
    labs(y = NULL, x = NULL, fill = "prop.") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  heat_stat
}
# fig_heat_stat()
# ggsave("output/SOM/heat_stat.pdf", height = 4, width = 6)
# ggsave("output/SOM/heat_stat.png", height = 4, width = 6)
# ggsave("docs/assets/SOM/heat_stat.png", height = 4, width = 6)

