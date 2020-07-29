# code/workflow.R
# This script may be run with source() in order to calculate the
# climatologies for all of the variables used in the study


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
system.time(
workflowr::wflow_publish(files = c("analysis/index.Rmd", 
                                   "analysis/data-prep.Rmd",
                                   "analysis/mhw-flux.Rmd"#, 
                                   # "analysis/k-means-flux.Rmd"
                                   ),
                         message = "Re-built site.")
) # 85 seconds


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------

# See the "Preparing the data" vignette


# Data prep ---------------------------------------------------------------

# Set number of cores
# NB: This is very RAM heavy, be carfeul with core use
# doParallel::registerDoParallel(cores = 26)
# 
# # Check the exact time frame of the daily GLORYS data
# GLORYS_info <- ncdump::NetCDF("../data/GLORYS/MHWflux_GLORYS_1993-1.nc")
# 
# # Prep all GLORYS data in one go
# GLORYS_files <- dir("../data/GLORYS", full.names = T, pattern = "MHWflux")
# system.time(
#   GLORYS_all_ts <- load_all_GLORYS_region(GLORYS_files) %>%
#     dplyr::arrange(region, t)
# ) # 187 seconds on 25 cores
# saveRDS(GLORYS_all_ts, "data/GLORYS_all_ts.Rda")

### ERA5 data are saved in individual variables
## Nb: Be cautious with how massive these processes are
doParallel::registerDoParallel(cores = 10)

## First need to make vertain that the correct direction of time lag is introduced for the Qx integrals

## Long wave radiation
# "msnlwrf"
print(paste0("Began loading msnlwrf at ", Sys.time()))
ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")
ERA5_lwr_ts <- plyr::ldply(ERA5_lwr_files, load_ERA5_region, 
                           .parallel = F, .progress = "text", time_shift = -43200) # 12 hour backward shift
ERA5_lwr_ts$msnlwrf <- round(ERA5_lwr_ts$msnlwrf, 6)
saveRDS(ERA5_lwr_ts, "data/ERA5_lwr_ts.Rda")

## Short wave radiation
# "msnswrf"
print(paste0("Began loading msnswrf at ", Sys.time()))
ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
ERA5_swr_ts <- plyr::ldply(ERA5_swr_files, load_ERA5_region, 
                           .parallel = F, .progress = "text", time_shift = -43200) # 12 hour backward shift
ERA5_swr_ts$msnswrf <- round(ERA5_swr_ts$msnswrf, 6)
saveRDS(ERA5_swr_ts, "data/ERA5_swr_ts.Rda")

## Latent heat flux
# "mslhf"
print(paste0("Began loading mslhf at ", Sys.time()))
ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
ERA5_lhf_ts <- plyr::ldply(ERA5_lhf_files, load_ERA5_region, 
                           .parallel = F, .progress = "text", time_shift = -43200) # 12 hour backward shift
ERA5_lhf_ts$mslhf <- round(ERA5_lhf_ts$mslhf, 6)
saveRDS(ERA5_lhf_ts, "data/ERA5_lhf_ts.Rda")

## Sensible heat flux
# "msshf"
print(paste0("Began loading msshf at ", Sys.time()))
ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
ERA5_shf_ts <- plyr::ldply(ERA5_shf_files, load_ERA5_region, 
                           .parallel = F, .progress = "text", time_shift = -43200) # 12 hour backward shift
ERA5_shf_ts$msshf <- round(ERA5_shf_ts$msshf, 6)
saveRDS(ERA5_shf_ts, "data/ERA5_shf_ts.Rda")

## Cloud cover
# "tcc"
print(paste0("Began loading e at ", Sys.time()))
ERA5_tcc_files <- dir("../../oliver/data/ERA/ERA5/CLOUD/", full.names = T, pattern = "ERA5")
ERA5_tcc_ts <- plyr::ldply(ERA5_tcc_files, load_ERA5_region, .parallel = F, .progress = "text")
ERA5_tcc_ts$tcc <- round(ERA5_tcc_ts$tcc, 4)
saveRDS(ERA5_tcc_ts, "data/ERA5_tcc_ts.Rda")

## Surface winds U component
# "u10"
print(paste0("Began loading u10 at ", Sys.time()))
ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")[15:40]
ERA5_u_ts <- plyr::ldply(ERA5_u_files, load_ERA5_region, .parallel = F, .progress = "text")
ERA5_u_ts$u10 <- round(ERA5_u_ts$u10, 6)
saveRDS(ERA5_u_ts, "data/ERA5_u_ts.Rda")

## Surface winds V component
# "v10"
print(paste0("Began loading v10 at ", Sys.time()))
ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")[15:40]
ERA5_v_ts <- plyr::ldply(ERA5_v_files, load_ERA5_region, .parallel = F, .progress = "text")
ERA5_v_ts$v10 <- round(ERA5_v_ts$v10, 6)
saveRDS(ERA5_v_ts, "data/ERA5_v_ts.Rda")

## Mean sea level pressure
# "mslp"
print(paste0("Began loading MSLP at ", Sys.time()))
ERA5_mslp_files <- dir("../../oliver/data/ERA/ERA5/MSLP", full.names = T, pattern = "ERA5")[15:40]
ERA5_mslp_ts <- plyr::ldply(ERA5_mslp_files, load_ERA5_region, .parallel = F, .progress = "text")
saveRDS(ERA5_mslp_ts, "data/ERA5_mslp_ts.Rda")

## Air temperature at 2 metres
# "t2m
print(paste0("Began loading t2m at ", Sys.time()))
ERA5_t2m_files <- dir("../../oliver/data/ERA/ERA5/T2M", full.names = T, pattern = "ERA5")[15:40]
ERA5_t2m_ts <- plyr::ldply(ERA5_t2m_files, load_ERA5_region, .parallel = F, .progress = "text")
ERA5_t2m_ts$t2m <- round(ERA5_t2m_ts$t2m-273.15, 4)
saveRDS(ERA5_t2m_ts, "data/ERA5_t2m_ts.Rda")

## Precipitation
# "tp"
print(paste0("Began loading tp at ", Sys.time()))
ERA5_pcp_files <- dir("../../oliver/data/ERA/ERA5/PRCP", full.names = T, pattern = "ERA5")[15:40]
ERA5_pcp_ts <- plyr::ldply(ERA5_pcp_files, load_ERA5_region, .parallel = F, .progress = "text")
ERA5_pcp_ts$tp <- round(ERA5_pcp_ts$tp, 8)
saveRDS(ERA5_pcp_ts, "data/ERA5_pcp_ts.Rda")

## Evaporation
# "e"
print(paste0("Began loading e at ", Sys.time()))
ERA5_evp_files <- dir("../../oliver/data/ERA/ERA5/EVAP", full.names = T, pattern = "ERA5")[15:40]
ERA5_evp_ts <- plyr::ldply(ERA5_evp_files, load_ERA5_region, .parallel = F, .progress = "text")
ERA5_evp_ts$e <- round(ERA5_evp_ts$e, 8)
saveRDS(ERA5_evp_ts, "data/ERA5_evp_ts.Rda")

# Reload the data
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
  filter(t > "1992-12-31") %>% 
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
         qnet = msnlwrf + msnswrf + mslhf + msshf,
         wind_spd = round(sqrt(u10^2 + v10^2), 2),
         wind_dir = round((270-(atan2(v10, u10)*(180/pi)))%%360))
saveRDS(ERA5_all_ts, "data/ERA5_all_ts.Rda")

# See the "Preparing the data" vignette for the code used to create the clims and anomalies


# MHW detection -----------------------------------------------------------

# See the "Preparing the data" vignette


# Heat flux vs. MHW -------------------------------------------------------

# See the "Heat flux vs. MHW" vignette


# K-means MHW -------------------------------------------------------------

# See the "K-means MHW" vignette


# Visuals -----------------------------------------------------------------

# Nothing here yet

