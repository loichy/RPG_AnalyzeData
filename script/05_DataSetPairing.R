#===============================================================================
# Description: Script to pair cross-section datasets that will be used to 
# implement a multinomial logit
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

GAEZ_yield <- readRDS(here(dir$final, "GAEZ_Yieldchange_ReAggregated.rds"))%>% 
  mutate(
    LIBELLE_GROUPE_CULTURE_AGG = groupe_rpg
  ) %>% 
  select(-groupe_rpg)

RPG_Variations_final <- readRDS(here(dir$final, "LongPeriod_AcreageVariations.rds"))

#===============================================================================
# 3). Load and prepare climate variables datasets ------
#===============================================================================

# Yearly mean daily mean temperature dataset
Temp_yearly <- readRDS(here(dir$raw, "era5_weather_communes_yearly_2006_2023_reference_1971_2000.rds"))

Temp_yearly <- Temp_yearly |>
  group_by(insee) |>
  summarise(moyenne_temp_period = mean(mean, na.rm = TRUE),
            moyenne_temp_ref = mean(reference_mean, na.rm = TRUE))

# Quarterly mean daily mean temperature dataset
Temp_quarterly <- readRDS(here(dir$raw, "era5_weather_communes_quarterly_2006_2023_reference_1971_2000.rds"))

Temp_quarterly <- Temp_quarterly |>
  group_by(insee, quarter) |>
  summarise(moyenne_temp_period = mean(mean, na.rm = TRUE),
         moyenne_temp_ref = mean(reference_mean, na.rm = TRUE))

## Pivoting the dataset to get quarterly mean values in columns
Temp_quarterly_wide <- Temp_quarterly |>
  pivot_wider(id_cols = insee, names_from = quarter, values_from = c("moyenne_temp_period", "moyenne_temp_ref"))

# Monthly sum precipitation dataset
Precip_yearly <- readRDS(here(dir$raw, "era5_total_precipitation_daily_sum_communes_yearly_2006_2023_reference_1971_2000.rds"))

Precip_yearly <- Precip_yearly |>
  group_by(insee) |>
  summarise(moyenne_precip_period = mean(total_precip_wet_days, na.rm = TRUE),
            moyenne_precip_ref = mean(reference_avg_total_precip_wet_days, na.rm = TRUE))

#===============================================================================
# 4). Join/pair them ------
#===============================================================================
RPG_yearly_GAEZ <- RPG_Variations_final %>%
  left_join(GAEZ_yield, by = c("insee", "LIBELLE_GROUPE_CULTURE_AGG")) %>% 
  select(
    insee, name, region_code, LIBELLE_GROUPE_CULTURE_AGG, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2_2007, surf_code_group_perc_2007, surf_code_group_perc_2023, surf_code_group_m2_2007, surf_code_group_m2_2023, diff_2023_2007_share_abs, diff_2023_2007_share_perc, mean_share_debut, mean_share_fin, mean_m2_debut, mean_m2_fin, diff_fin_debut_share_abs, diff_fin_debut_share_perc, diff_fin_debut_m2_abs, diff_fin_debut_m2_perc, etat, etat_libelle, 
    value_hist_rpg, value_futur_rpg
  ) %>% 
  arrange(insee, LIBELLE_GROUPE_CULTURE_AGG)

RPG_yearly_GAEZ <- RPG_yearly_GAEZ |>
  left_join(Temp_quarterly_wide, by = "insee")

RPG_yearly_GAEZ <- RPG_yearly_GAEZ |>
  left_join(Temp_yearly, by = "insee")

RPG_yearly_GAEZ <- RPG_yearly_GAEZ |>
  left_join(Precip_yearly, by = "insee")

#===============================================================================
# Save data
saveRDS(RPG_yearly_GAEZ, here(dir$final, "Joined_GAEZ_RPG.rds"))
