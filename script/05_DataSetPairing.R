#===============================================================================
# Description: Script to pair both datasets
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
# 2). Load datasets ------
#===============================================================================

GAEZ_yield <- readRDS(here(dir$final, "GAEZ_Yieldchange_ReAggregated.rds"))%>% 
  mutate(
    LIBELLE_GROUPE_CULTURE_AGG = groupe_rpg
  ) %>% 
  select(-groupe_rpg)

RPG_Variations_final <- readRDS(here(dir$final, "LongPeriod_AcreageVariations.rds"))

#===============================================================================
# 3). Join/pair them ------
#===============================================================================
RPG_yearly_GAEZ <- RPG_Variations_final %>%
  left_join(GAEZ_yield, by = c("insee", "LIBELLE_GROUPE_CULTURE_AGG")) %>% 
  select(
    insee, name, region_code, LIBELLE_GROUPE_CULTURE_AGG, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2_2007, surf_code_group_perc_2007, surf_code_group_perc_2023, surf_code_group_m2_2007, surf_code_group_m2_2023, diff_2023_2007_share_abs, diff_2023_2007_share_perc, mean_share_debut, mean_share_fin, mean_m2_debut, mean_m2_fin, diff_fin_debut_share_abs, diff_fin_debut_share_perc, diff_fin_debut_m2_abs, diff_fin_debut_m2_perc, etat, etat_libelle, 
    value_hist_rpg, value_futur_rpg
  ) %>% 
  arrange(insee, LIBELLE_GROUPE_CULTURE_AGG)

#===============================================================================
# Save data
saveRDS(RPG_yearly_GAEZ, here(dir$final, "Joined_GAEZ_RPG.rds"))
