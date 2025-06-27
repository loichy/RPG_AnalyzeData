#===============================================================================
# Description: Script to create a dataframe containing acreage change over the 
# period for a subset of cultures
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
# 2). Load and prepare dataset ------
#===============================================================================

# test avec Bretagne car fichier trop lourd
RPG_53 <- readRDS(here(dir$raw, "RPG_Aggregated_Brittany.rds"))

RPG_All <- readRDS(here(dir$raw, "RPG_Aggregated_All.rds"))

cultures_select <- c("Blé tendre", "Maïs grain et ensilage", "Vignes", 
                     "Protéagineux", "Légumineuses à grains", "Plantes à fibres")
  

part_culture_commune <- RPG_53 |>
  filter(LIBELLE_GROUPE_CULTURE %in% cultures_select) |>
  group_by(year, insee, LIBELLE_GROUPE_CULTURE) |>
  summarise(surface_culture = sum(surf_agri_geo_unit_m2, na.rm = TRUE), .groups = "drop") |>
  group_by(year, insee) |>
  mutate(surface_commune = sum(surface_culture, na.rm = TRUE),
         proportion = surface_culture / surface_commune) |>
  ungroup()  

#===============================================================================
# 3). Calculating acreage variations over the period ------
#===============================================================================

RPG_final <- part_culture_commune |>
  filter(year %in% c(2007:2010, 2020:2023)) |>
  mutate(period = case_when(
    year == 2007 ~ "y2007",
    year == 2023 ~ "y2023",
    year %in% 2007:2010 ~ "debut",
    year %in% 2020:2023 ~ "fin",
    TRUE ~ NA_character_
  )) |>
  group_by(insee, LIBELLE_GROUPE_CULTURE, period) |>
  summarise(moy_proportion = mean(proportion, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = moy_proportion) |>
  mutate(
    var_2007_2023 = y2023 - y2007,
    var_debut_fin = fin - debut
  ) |>
  select(insee, LIBELLE_GROUPE_CULTURE, var_2007_2023, var_debut_fin)



