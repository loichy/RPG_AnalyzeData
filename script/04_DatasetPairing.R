#===============================================================================
# Description: Script to pair the GAEZ dataset and the RPG dataset for the relevant
# cultures in France 
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

RPG_Variations <- readRDS(here(dir$raw, "LongPeriod_AcreageVariations.rds"))

cultures_select <- c("Blé tendre", "Maïs grain et ensilage", 
                     "Protéagineux", "Légumineuses à grains", "Plantes à fibres")

RPG_Variations_filt <- RPG_Variations |>
  filter(LIBELLE_GROUPE_CULTURE %in% cultures_select)

trad_cultures <- tibble::tibble(
  culture_en = c("Chickpea", "Dry pea", "Flax", "Maize", "Wheat"),
  culture_fr = c("Légumineuses à grains", "Plantes à fibres", "Maïs grain et ensilage", "Blé tendre")
)


GAEZ_yield <- readRDS(here(dir$raw, "GAEZ_yieldchange_communes_filt.rds"))

GAEZ_yield_filt <- GAEZ_yield |>
  filter(variable == "ylHr") |>
  filter(crop != "Gram") 
  



