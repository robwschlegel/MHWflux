# code/workflow.R
# This script may be run with source() in order to calculate the
# climatologies for all of the variables used in the study


# Workflowr code ----------------------------------------------------------

# All analysis files
# dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
# system.time(
# workflowr::wflow_publish(files = c("analysis/index.Rmd", "analysis/data-prep.Rmd"),
#                          message = "Re-publish entire site.")
# ) # 70 seconds


# Startup -----------------------------------------------------------------

# Base libraries etc.
source("code/functions.R")


# Study area --------------------------------------------------------------

# See the "Preparing the data" vignette


# Data prep ---------------------------------------------------------------

# Set number of cores
# NB: This is very RAM heavy, be carfeul with core use
doParallel::registerDoParallel(cores = 26)

# Prep all GLORYS data in one go
# GLORYS_files <- dir("../data/GLORYS", full.names = T, pattern = "MHWflux")
# system.time(
#   GLORYS_all_ts <- load_all_GLORYS_region(GLORYS_files) %>% 
#     dplyr::arrange(region, t)
# ) # 187 seconds on 25 cores
# saveRDS(GLORYS_all_ts, "data/GLORYS_all_ts.Rda")

### ERA5 data are saved in individual variables
## NB: The machine refuses to run more than one chunk at a time
## One must restart R after each go... dumb

## Long wave radiation
# "msnlwrf"
# print(paste0("Began loading msnlwrf at ", Sys.time()))
# ERA5_lwr_files <- dir("../../oliver/data/ERA/ERA5/LWR", full.names = T, pattern = "ERA5")
# ERA5_lwr_ts <- plyr::ldply(ERA5_lwr_files, load_ERA5_region, .parallel = T)
# ERA5_lwr_ts$msnlwrf <- round(ERA5_lwr_ts$msnlwrf, 6)
# saveRDS(ERA5_lwr_ts, "data/ERA5_lwr_ts.Rda")

## Short wave radiation
# "msnswrf"
# print(paste0("Began loading msnswrf at ", Sys.time()))
# ERA5_swr_files <- dir("../../oliver/data/ERA/ERA5/SWR", full.names = T, pattern = "ERA5")
# ERA5_swr_ts <- plyr::ldply(ERA5_swr_files, load_ERA5_region, .parallel = T)
# ERA5_swr_ts$msnswrf <- round(ERA5_swr_ts$msnswrf, 6)
# saveRDS(ERA5_swr_ts, "data/ERA5_swr_ts.Rda")

## Latent heat flux
# "mslhf"
# print(paste0("Began loading mslhf at ", Sys.time()))
# ERA5_lhf_files <- dir("../../oliver/data/ERA/ERA5/SLHF", full.names = T, pattern = "ERA5")
# ERA5_lhf_ts <- plyr::ldply(ERA5_lhf_files, load_ERA5_region, .parallel = T)
# ERA5_lhf_ts$mslhf <- round(ERA5_lhf_ts$mslhf, 6)
# saveRDS(ERA5_lhf_ts, "data/ERA5_lhf_ts.Rda")

## Sensible heat flux
# "msshf"
print(paste0("Began loading msshf at ", Sys.time()))
ERA5_shf_files <- dir("../../oliver/data/ERA/ERA5/SSHF", full.names = T, pattern = "ERA5")
ERA5_shf_ts <- plyr::ldply(ERA5_shf_files, load_ERA5_region, .parallel = T)
ERA5_shf_ts$msshf <- round(ERA5_shf_ts$msshf, 6)
saveRDS(ERA5_shf_ts, "data/ERA5_shf_ts.Rda")

## Surface winds U component
# "u10"
# print(paste0("Began loading u10 at ", Sys.time()))
# ERA5_u_files <- dir("../../oliver/data/ERA/ERA5/U10", full.names = T, pattern = "ERA5")[15:40]
# ERA5_u_ts <- plyr::ldply(ERA5_u_files, load_ERA5_region, .parallel = T)
# ERA5_u_ts$u10 <- round(ERA5_u_ts$u10, 6)
# saveRDS(ERA5_u_ts, "data/ERA5_u_ts.Rda")

## Surface winds V component
# "v10"
# print(paste0("Began loading v10 at ", Sys.time()))
# ERA5_v_files <- dir("../../oliver/data/ERA/ERA5/V10", full.names = T, pattern = "ERA5")[15:40]
# ERA5_v_ts <- plyr::ldply(ERA5_v_files, load_ERA5_region, .parallel = T)
# ERA5_v_ts$v10 <- round(ERA5_v_ts$v10, 6)
# saveRDS(ERA5_v_ts, "data/ERA5_v_ts.Rda")

## Mean sea level pressure
# "mslp"
# print(paste0("Began loading MSLP at ", Sys.time()))
# ERA5_mslp_files <- dir("../../oliver/data/ERA/ERA5/MSLP", full.names = T, pattern = "ERA5")[15:40]
# ERA5_mslp_ts <- plyr::ldply(ERA5_mslp_files, load_ERA5_region, .parallel = T)
# saveRDS(ERA5_mslp_ts, "data/ERA5_mslp_ts.Rda")

## Air temperature at 2 metres
# print(paste0("Began loading t2m at ", Sys.time()))
# ERA5_t2m_files <- dir("../../oliver/data/ERA/ERA5/T2M", full.names = T, pattern = "ERA5")[15:40]
# ERA5_t2m_ts <- plyr::ldply(ERA5_t2m_files, load_ERA5_region, .parallel = T)
# ERA5_t2m_ts$t2m <- round(ERA5_t2m_tst2m, 4)-272.15
# saveRDS(ERA5_t2m_ts, "data/ERA5_t2m_ts.Rda")

# Reload the data
# ERA5_lwr_ts <- readRDS("data/ERA5_lwr_ts.Rda")
# ERA5_swr_ts <- readRDS("data/ERA5_swr_ts.Rda")
# ERA5_lhf_ts <- readRDS("data/ERA5_lhf_ts.Rda")
# ERA5_shf_ts <- readRDS("data/ERA5_shf_ts.Rda")
# ERA5_u_ts <- readRDS("data/ERA5_u_ts.Rda")
# ERA5_v_ts <- readRDS("data/ERA5_v_ts.Rda")
# ERA5_mslp_ts <- readRDS("data/ERA5_mslp_ts.Rda")
# ERA5_t2m_ts <- readRDS("data/ERA5_t2m_ts.Rda")

# Stitch them together
# join_cols <- c("lon", "lat", "t")
# ERA5_all_ts <- left_join(ERA5_lwr_ts, ERA5_swr_ts, by = join_cols) %>% 
#   left_join(ERA5_lhf_ts, by = join_cols) %>% 
#   left_join(ERA5_shf_ts, by = join_cols) %>% 
#   left_join(ERA5_u_ts, by = join_cols) %>% 
#   left_join(ERA5_v_ts, by = join_cols) %>% 
#   left_join(ERA5_mslp_ts, by = join_cols) %>% 
#   left_join(ERA5_t2m_ts, by = join_cols)
# saveRDS(ERA5_all, "data/ERA5_all_ts.Rda")

# See the "Preparing the data" vignette for the code used to create the clims and anomalies


# MHW detection -----------------------------------------------------------

# See the "Preparing the data" vignette


# Heat flux vs. MHW -------------------------------------------------------

# See the "Heat flux vs. MHW" vignette


# K-means MHW -------------------------------------------------------------

# See the "K-means MHW" vignette


# Visuals -----------------------------------------------------------------

# Nothing here yet
