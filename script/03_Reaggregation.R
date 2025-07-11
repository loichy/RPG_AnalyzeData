#===============================================================================
# Description: Script to prepare GAEZ and RPG datasets to be paired together :
# reaggregating both datasets so that the level of crop aggregation is the same
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tibble, data.table, here, sf, tmap, units, dplyr)

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
# 2). Load and prepare GAEZ dataset ------
#===============================================================================

GAEZ_France <- readRDS(here(dir$raw, "GAEZ_yieldchange_communes_filt.rds"))

# Extracting Grass culture which is only present in theme 3
GAEZ_France_Grass <- GAEZ_France |>
  filter(theme_id == 3) %>% 
  filter(crop == "Grass")

# Using theme 4 for the rest of the cultures
GAEZ_France_th4 <- GAEZ_France |>
  filter(!crop %in% c(
    "Biomass sorghum",
    "Cassava",
    "Coconut",
    "Jatropha",
    "Oil palm",
    "Reed canary grass",
    "Tea"
  )) %>% 
  filter(theme_id == 4, variable == "ylHr") 
GAEZ_France_select <- GAEZ_France_th4 |>
  bind_rows(GAEZ_France_Grass)

# Adding the RPG cultures to the dataframe
correspondance_gaez_rpg <- tribble(
  ~culture_gaez,        ~groupe_rpg,
  "Buckwheat",          "Autres céréales",
  "Foxtail millet",     "Autres céréales",
  "Oat",                "Autres céréales",
  "Pearl millet",       "Autres céréales",
  "Rye",                "Autres céréales",
  "Sorghum",            "Autres céréales",
  "Switchgrass",        "Autres céréales",
  "Tobacco",            "Autres cultures industrielles",
  "Groundnut",          "Autres oléagineux",
  "Soybean",            "Autres oléagineux",
  "Wheat",              "Blé tendre",
  "Sugar beet",         "Canne à sucre",
  "Sugar cane",         "Canne à sucre",
  "Rapeseed",           "Colza",
  "Miscanthus",         "Divers",
  "Alfalfa",            "Fourrage",
  "Napier grass",       "Fourrage",
  "Cabbage",            "Légumes ou fleurs",
  "Carrot",             "Légumes ou fleurs",
  "Onion",              "Légumes ou fleurs",
  "Phaseolus bean",     "Légumes ou fleurs",
  "Sweet potato",       "Légumes ou fleurs",
  "Tomato",             "Légumes ou fleurs",
  "White potato",       "Légumes ou fleurs",
  "Yam",                "Légumes ou fleurs",
  "Chickpea",           "Légumineuses à grains",
  "Cowpea",             "Légumineuses à grains",
  "Dry pea",            "Légumineuses à grains",
  "Gram",               "Légumineuses à grains",
  "Pigeonpea",          "Légumineuses à grains",
  "Maize",              "Maïs grain et ensilage",
  "Olive",              "Oliviers",
  "Barley",             "Orge",
  "Grass",              "Pâturages",
  "Cotton",             "Plantes à fibres",
  "Flax",               "Plantes à fibres",
  "Dryland rice",       "Riz",
  "Wetland rice",       "Riz",
  "Sunflower",          "Tournesol",
  "Banana",             "Vergers",
  "Citrus",             "Vergers",
  "Cocoa",              "Vergers",
  "Coffee",             "Vergers"
)
GAEZ_France_match <- GAEZ_France_select |>
  left_join(correspondance_gaez_rpg, by = c("crop" = "culture_gaez"))

# Create weights associated to each GAEZ crop
GAEZ_France_weight <- GAEZ_France_match |>
  group_by(insee, groupe_rpg) |>
  mutate(
    sum_value_hist_group = sum(value_hist, na.rm = TRUE), # Compute for each RPG category to the total cumulated yields from each communes
    weight = if_else(sum_value_hist_group > 0,
                     value_hist / sum_value_hist_group, # Weight corresponds to the proportion of each GAEZ crop yield in the total yield of the RPG category
                     0)
  ) |>
  ungroup()

# Aggregate at RPG level while taking into account the weights associated to each GAEZ crops
rpg_level <- GAEZ_France_weight |>
  group_by(insee,groupe_rpg) |>
  summarise(
    value_hist_rpg = sum(value_hist * weight, na.rm = TRUE),
    value_futur_rpg = sum(value * weight, na.rm = TRUE),
    .groups = "drop"
  )

#===============================================================================
# 2). Load and prepare RPG dataset (which will be used in script 04) ------
#===============================================================================

RPG_cultures <- readRDS("data/raw/RPG_Aggregated_ALL.rds")

RPG_cultures_transf <- RPG_cultures |>
  mutate(LIBELLE_GROUPE_CULTURE_AGG = case_when(
    LIBELLE_GROUPE_CULTURE %in% c("Prairies temporaires", "Prairies permanentes", "Estives et landes") ~ "Pâturages",
    LIBELLE_GROUPE_CULTURE %in% c("Protéagineux", "Légumineuses à grains") ~ "Légumineuses à grains/Protéagineux",
    LIBELLE_GROUPE_CULTURE %in% c("Vignes", "Fruits à coque", "Gel (surfaces gelées sans production)") ~ "Divers",
    TRUE ~ LIBELLE_GROUPE_CULTURE
  )) 

# Méthode plus rapide que summarize : convert to data.table
dt <- as.data.table(RPG_cultures_transf)

# Extract crops to aggregate
fusion_cultures <- c("Pâturages", "Légumineuses à grains/Protéagineux", "Divers")
dt_fusion <- dt[LIBELLE_GROUPE_CULTURE %in% fusion_cultures]

# Separate numerical columns where observations will be added up
cols_num <- names(dt_fusion)[sapply(dt_fusion, is.numeric)]
cols_id  <- setdiff(names(dt_fusion), c("name", "year", "LIBELLE_GROUPE_CULTURE", cols_num))

# Aggregate numeric columns
fusion_sum <- dt_fusion[, lapply(.SD, sum, na.rm = TRUE),
                        by = .(name, year, LIBELLE_GROUPE_CULTURE),
                        .SDcols = cols_num]

# Aggregate non numeric columns
fusion_ids <- dt_fusion[, lapply(.SD, \(x) first(na.omit(x))),
                        by = .(name, year, LIBELLE_GROUPE_CULTURE),
                        .SDcols = cols_id]

# Rebind original dataset
dt_fusion_agg <- merge(fusion_sum, fusion_ids, 
                       by = c("name", "year", "LIBELLE_GROUPE_CULTURE"))
dt_autres <- dt[!(LIBELLE_GROUPE_CULTURE %in% fusion_cultures)]
RPG_final <- rbind(dt_fusion_agg, dt_autres, fill = TRUE)

# Updating crop group codes to match the new group names
RPG_final <- RPG_final |>
  mutate(CODE_GROUP = case_when(
    CODE_GROUP %in% c("17", "19") ~ "18",
    CODE_GROUP %in% c("8") ~ "15",
    TRUE ~ CODE_GROUP))

check <- RPG_final %>% 
  group_by(name, year) %>% 
  summarise(surf_agri = first(surf_agri_geo_unit_m2),
            totals = sum(surf_code_group_m2, na.rm = TRUE),
            totals_perc = sum(surf_code_group_perc, na.rm = TRUE))

saveRDS(RPG_final, "data/raw/RPG_NEW_Aggreg_ALL.rds")



