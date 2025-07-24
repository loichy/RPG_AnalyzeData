#===============================================================================
# Description: Script to pair panel datasets that will be used to 
# reproduce the models in Cui et al. (2020)
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap, units, dplyr, InterpolateR)

# List directories 
dir <- list()
dir$root <- here()
dir$data <- here(dir$root, "data")
dir$raw <- here(dir$data, "raw")
dir$derived <- here(dir$data, "derived")
dir$final <- here(dir$data, "final")
dir$script <- here(dir$root, "script")
dir$output <- here(dir$root, "output")
dir$shapefiles <- here(dir$data, "shapefiles")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#===============================================================================
# 2). Load datasets ------
#===============================================================================

# Loading the datasets over our time period

RPG_panel <- readRDS(here(dir$final, "RPG_COMPLETE_Aggreg_ALL.rds")) 

GAEZ_panel <- readRDS(here(dir$final, "GAEZ_Yieldchange_ReAggregated.rds"))

Temp_panel <- readRDS(here(dir$raw, "era5_weather_communes_yearly_2006_2023_reference_1971_2000.rds"))

Precip_panel <- readRDS(here(dir$raw, "era5_total_precipitation_daily_sum_communes_yearly_2006_2023_reference_1971_2000.rds"))

#===============================================================================
# 3). Join datasets ------
#===============================================================================

# Join climate variables with RPG panel data
RPG_panel <- RPG_panel |>
  mutate(year = as.numeric(year)) |>
  left_join(Temp_panel, by = c("insee", "year")) |>
  select(insee, year, region_code, LIBELLE_GROUPE_CULTURE_AGG, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, surf_code_group_perc, surf_code_group_m2, mean, reference_mean, starts_with("days_bin")) |>
  arrange(insee, year, LIBELLE_GROUPE_CULTURE_AGG)

RPG_panel <- RPG_panel |>
  mutate(year = as.numeric(year)) |>
  left_join(Precip_panel, by = c("insee", "year")) |>
  select(insee, year, region_code, LIBELLE_GROUPE_CULTURE_AGG, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, surf_code_group_perc, surf_code_group_m2, mean, reference_mean, starts_with("days_bin"), total_precip_wet_days, reference_avg_total_precip_wet_days) |>
  arrange(insee, year, LIBELLE_GROUPE_CULTURE_AGG)

# Join GAEZ potential yield variables
GAEZ_panel <- GAEZ_panel |>
  rename(LIBELLE_GROUPE_CULTURE_AGG = groupe_rpg)

df_panel <- RPG_panel |>
  left_join(GAEZ_panel, by = c("insee", "LIBELLE_GROUPE_CULTURE_AGG"))

#===============================================================================
# 4). Add additional variables for econometric analysis ------
#===============================================================================

df_panel <- df_panel |>
  mutate(surf_code_group_m2 = drop_units(surf_code_group_m2),
         surf_agri_geo_unit_m2 = drop_units(surf_agri_geo_unit_m2)) |>
  mutate(log_surf_agri = log(surf_code_group_m2),
         change = (value_futur_rpg - value_hist_rpg) / value_hist_rpg,
         value_hist_rpg_t_ha = value_hist_rpg / 1000)

#===============================================================================
# Save data
saveRDS(df_panel, here(dir$final, "Joined_GAEZ_RPG_Panel.rds"))



