#===============================================================================
# Description: Script to create a dataframe containing acreage change over the 
# period and, then pair it with the potential yield change from GAEZ
# This dataset will then be used in econometric estimations
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap, units, dplyr)

# List directories 
dir <- list()
dir$root <- here()
dir$data <- here(dir$root, "data")
dir$raw <- here(dir$data, "raw")
dir$derived <- here(dir$data, "derived")
dir$final <- here(dir$data, "final")
dir$script <- here(dir$root, "script")
dir$output <- here(dir$root, "output")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#===============================================================================
# 2). Load and prepare the datasets ------
#===============================================================================

RPG_Variations <- readRDS(here(dir$final, "RPG_ReAggregated_ALL.rds")) %>% 
  arrange(year, insee, LIBELLE_GROUPE_CULTURE_AGG)

GAEZ_yield <- readRDS(here(dir$final, "GAEZ_Yieldchange_ReAggregated.rds"))%>% 
  mutate(
    LIBELLE_GROUPE_CULTURE_AGG = groupe_rpg
  ) %>% 
  select(-groupe_rpg)

# check_2018 <- RPG_Variations %>% 
#   filter(year == 2018) %>% 
#   group_by(insee) %>% 
#   summarize(
#     parcels=sum(parcel_cult_code_group_perc),
#     surf =sum(surf_code_group_perc))
# check_2008 <- RPG_Variations %>% 
#   filter(year == 2008) %>% 
#   group_by(insee) %>% 
#   summarize(
#     parcels=sum(parcel_cult_code_group_perc),
#     surf =sum(surf_code_group_perc))

#===============================================================================
# 3). Create panel data ------
#===============================================================================

# For tomorrow: fill with 0s, and then create the long acreage vaeriables + dynamic

# Get the list of all crops
all_crops <- unique(RPG_Variations$LIBELLE_GROUPE_CULTURE_AGG)

# Fill using complete()
df_complete <- RPG_Variations %>%
  tidyr::complete(
    insee, name, year, LIBELLE_GROUPE_CULTURE_AGG = all_crops,
    fill = list(
      parcel_cult_code_group_n = 0,
      parcel_cult_code_group_perc = 0,
      surf_code_group_m2 = 0,
      surf_code_group_perc = 0
    )
  )

#===============================================================================
# 4). Calculating acreage variations over the period ------
#===============================================================================

# Make sure year is numeric
df <- RPG_All %>%
  mutate(year = as.numeric(year),
         surf_code_group_perc = as.numeric(surf_code_group_perc))

# Difference between year 2007 and 2023
diff_years <- df %>%
  filter(year %in% c(2007, 2023)) %>%
  select(insee, region_code, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, CODE_GROUP, LIBELLE_GROUPE_CULTURE, year, surf_code_group_perc) %>%
  pivot_wider(names_from = year, values_from = surf_code_group_perc, names_prefix = "year_", values_fill = 0) %>%
  mutate(diff_2007_2023_abs = year_2023 - year_2007,
         diff_2007_2023_perc = ((year_2023 - year_2007) / year_2007) * 100)

# 2. Difference between mean of 2007–2010 and 2020–2023
diff_means <- df %>%
  filter(year %in% c(2007:2010, 2020:2023)) %>%
  select(insee, region_code, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, CODE_GROUP, LIBELLE_GROUPE_CULTURE, year, surf_code_group_perc) %>%
  mutate(period = case_when(
    year %in% 2007:2010 ~ "debut",
    year %in% 2020:2023 ~ "fin"
  )) %>%
  group_by(insee, CODE_GROUP, LIBELLE_GROUPE_CULTURE, period) %>%
  summarise(mean_perc = mean(surf_code_group_perc, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = mean_perc, names_prefix = "mean_", values_fill = 0) %>%
  mutate(diff_final_debut_abs = mean_fin - mean_debut,
         diff_final_debut_perc = ((mean_fin - mean_debut) / mean_debut) * 100)

# 3. Combine both differences into one table
final_result <- full_join(diff_years, diff_means, 
                          by = c("insee", "CODE_GROUP", "LIBELLE_GROUPE_CULTURE")) %>% 
  arrange(insee, as.numeric(CODE_GROUP))

#===============================================================================
# 5). Creating categorical variable accounting for the presence of crops ------
#===============================================================================

final_result <- final_result |>
  mutate(
    etat = case_when(
      mean_debut < 0.001 & mean_fin < 0.001                 ~ 4,  # jamais cultivé
      mean_debut >= 0.001  & mean_fin >= 0.001              ~ 1,  # maintenu
      year_2007 == 0 & year_2023 > 0                        ~ 2,  # apparition
      year_2007 > 0  & year_2023 == 0                       ~ 3   # disparition
    ),
    etat_libelle = case_when(
      etat == 1 ~ "maintenu",
      etat == 2 ~ "apparition",
      etat == 3 ~ "disparition",
      etat == 4 ~ "jamais cultivé"
    )
  )

#===============================================================================
# 6). Join/pair them ------
#===============================================================================
RPG_yearly_GAEZ <- RPG_Variations %>%
  left_join(GAEZ_yield, by = c("insee", "LIBELLE_GROUPE_CULTURE_AGG")) %>% 
  select(
    insee, name, region_code, year, LIBELLE_GROUPE_CULTURE_AGG, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, N_Parcels,
    surf_code_group_m2, surf_code_group_perc, parcel_cult_code_group_n, parcel_cult_code_group_perc,
    value_hist_rpg, value_futur_rpg
  ) %>% 
  arrange(year, insee, LIBELLE_GROUPE_CULTURE_AGG)



#===============================================================================
# Save data
saveRDS(final_result, here(dir$final, "LongPeriod_AcreageVariations.rds"))
