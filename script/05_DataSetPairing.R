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
# 2). Load and prepare datasets ------
#===============================================================================

# GAEZ dataset
GAEZ_yield <- readRDS(here(dir$final, "GAEZ_Yieldchange_ReAggregated.rds"))%>% 
  mutate(
    LIBELLE_GROUPE_CULTURE_AGG = groupe_rpg
  ) %>% 
  select(-groupe_rpg)

# RPG dataset
RPG_Variations_final <- readRDS(here(dir$final, "LongPeriod_AcreageVariations.rds"))

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
# 3). Join/pair datasets ------
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
# 4). Finalize dataset and add variables for econometric analysis ------
#===============================================================================

# Verify that there are no duplicated rows in the final dataset

RPG_yearly_GAEZ_final <- RPG_yearly_GAEZ |>
  filter(!is.na(etat)) 

duplicated_rows <- RPG_yearly_GAEZ_final |>
  count(insee, LIBELLE_GROUPE_CULTURE_AGG) |>
  filter(n > 1) ## if 0 => no duplicatas


# Compute Potential Yield Change variable

RPG_yearly_GAEZ_final <- RPG_yearly_GAEZ_final |>
  mutate(change = (value_futur_rpg - value_hist_rpg)/value_hist_rpg)


# Compute Comparative Advantage Index for historical potentiel yield 

# 1). Numerator = communal potential yield per crop / sum of all communal potential yield
numerator_hist <- RPG_yearly_GAEZ_final |>
  group_by(insee) |>
  mutate(part_commune = value_hist_rpg / sum(value_hist_rpg, na.rm = TRUE)) |>
  ungroup()

# 2). Denominator = average potential yield at a national level per crop / sum of APY
denom_hist <- RPG_yearly_GAEZ_final |>
  group_by(LIBELLE_GROUPE_CULTURE_AGG) |>
  summarise(mean_crop = mean(value_hist_rpg, na.rm = TRUE), .groups = "drop")

total_mean_crop <- sum(denom_hist$mean_crop, na.rm = TRUE)

denom_hist <- denom_hist |>
  mutate(part_nationale = mean_crop / total_mean_crop)

# 3). Comparative Advantage Index
RPG_yearly_GAEZ_final <- numerator_hist |>
  left_join(denom_hist |>
              select(LIBELLE_GROUPE_CULTURE_AGG, part_nationale), by = "LIBELLE_GROUPE_CULTURE_AGG") |>
  mutate(AC_hist = part_commune / part_nationale) |>
  select(!part_commune & !part_nationale)


# Compute Comparative Advantage Index for future potentiel yield 

# 1). Numerator = communal potential yield per crop / sum of all communal potential yield
numerator_futur <- RPG_yearly_GAEZ_final |>
  group_by(insee) |>
  mutate(part_commune = value_futur_rpg / sum(value_futur_rpg, na.rm = TRUE)) |>
  ungroup()

# 2). Denominator = average potential yield at a national level per crop / sum of APY
denom_futur <- RPG_yearly_GAEZ_final |>
  group_by(LIBELLE_GROUPE_CULTURE_AGG) |>
  summarise(mean_crop = mean(value_hist_rpg, na.rm = TRUE), .groups = "drop")

total_mean_crop <- sum(denom_futur$mean_crop, na.rm = TRUE)

denom_futur <- denom_futur |>
  mutate(part_nationale = mean_crop / total_mean_crop)

# 3). Comparative Advantage Index
RPG_yearly_GAEZ_final <- numerator_futur |>
  left_join(denom_futur |>
              select(LIBELLE_GROUPE_CULTURE_AGG, part_nationale), by = "LIBELLE_GROUPE_CULTURE_AGG") |>
  mutate(AC_futur = part_commune / part_nationale) |>
  select(!part_commune & !part_nationale)

# Difference between AC futur and AC historical
RPG_yearly_GAEZ_final <- RPG_yearly_GAEZ_final |>
  mutate(AC_diff = (AC_futur - AC_hist)/AC_hist)


#===============================================================================
# Save data
saveRDS(RPG_yearly_GAEZ_final, here(dir$final, "Joined_GAEZ_RPG.rds"))
