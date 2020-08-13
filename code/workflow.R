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

# Set number of cores
# NB: This is very RAM heavy, be careful with core use
registerDoParallel(cores = 50)

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
plyr::ddply(ERA5_files, c("var_group"), process_ERA5, .parallel = F)

# Load the heat flux layers


# Combine to make Qnet


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


# Visuals -----------------------------------------------------------------

# Nothing here yet

