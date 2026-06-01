# ==============================================================================
# On aggrège les données du RPG au niveau des Petites Régions Agricoles
# ==============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap, units, knitr, kableExtra, plotly, viridis, shiny, vegan, paletteer, readxl, biscale, cowplot, rstatix)

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


# Load the data

RPG_GROUP <- readRDS("D:/Data/RPG_Data/final/RPG_Aggregated_GROUP.rds")

RPG_CULT <- readRDS("D:/Data/RPG_Data/final/RPG_Aggregated_CULT.rds")


pra_shp <- read_sf(here(dir$shapefiles, "pra_shp/CommunePra_SFWGS84_WithCell.shp")) %>%
  select(PRA_Code, geometry) %>%
  filter(!is.na(PRA_Code)) %>%
  unique()

# Lors de cette agrégation, deux villages ne sont pas associés à une PRA : 
# Leurs codes sont 14666 (Sannerville) et 27058 (Les Trois Lacs).
# Sannerville est fusionnée en 2017 pour créer Saline : annulé par arrêté en 2019
# Géographiquement, même PRA que "Troarn" : PRA_Code 14353.
# Les Trois Lacs est une nouvelle commune créée en 2017.
# Géographiquement, même PRA que "Villers-sur-le-Roule" : PRA_Code 27332.
# On ajoute donc les informations manquantes :

CODGEO <- c("14666", "27058")
PRA_Code <- c("14353", "27332")
PRA_Lib <- c("PAYS D AUGE - 14", "VALLEE DE LA SEINE - 27")

bonus_pra_com <- data.frame(CODGEO, PRA_Code, PRA_Lib)


pra_com <- read_xls(here(dir$raw, "Referentiel_CommuneRA_PRA_2017.xls"), skip = 5) %>%
  select(CODGEO, PRA_Code, PRA_Lib) %>%
  bind_rows(bonus_pra_com)


# Chaque commune étant associée à une unique PRA, on peut les regrouper :

RPG_CULT_pra <- pra_com %>% 
  rename(insee = "CODGEO") %>%
  full_join(RPG_CULT, by = "insee", relationship = "many-to-many") %>%
  mutate(year = as.numeric(year)) %>%
  select(!c(data_type, name, region_code, dept_code))

RPG_GROUP_pra <- pra_com %>% 
  rename(insee = "CODGEO") %>%
  full_join(RPG_GROUP, by = "insee", relationship = "many-to-many") %>%
  mutate(year = as.numeric(year)) %>%
  select(!c(data_type, name, region_code))


# On aggrège les surfaces agricoles/nb de parcelles totales par PRA

 pra_insee_filtered <- RPG_CULT_pra %>%
  select(insee, PRA_Code, year, N_Parcels, 
         surf_tot_geo_unit_m2, surf_agri_geo_unit_m2) %>%
  unique() 
 
 pra_insee_filtered2 <- RPG_GROUP_pra %>%
  select(insee, PRA_Code, year, N_Parcels, 
          surf_tot_geo_unit_m2, surf_agri_geo_unit_m2) %>%
  unique()


 agrreg_pra <- pra_insee_filtered %>%
  filter(!is.na(year)) %>%
  group_by(PRA_Code, year) %>%
  summarise(
     n_parcels = sum(N_Parcels, na.rm = TRUE),
     surf_tot_m2 = sum(surf_tot_geo_unit_m2, na.rm = TRUE),
     surf_agri_m2 = sum(surf_agri_geo_unit_m2, na.rm = TRUE),
     .groups = "drop")
 
 agrreg_pra2 <- pra_insee_filtered2 %>%
   filter(!is.na(year)) %>%
   group_by(PRA_Code, year) %>%
   summarise(
     n_parcels = sum(N_Parcels, na.rm = TRUE),
     surf_tot_m2 = sum(surf_tot_geo_unit_m2, na.rm = TRUE),
     surf_agri_m2 = sum(surf_agri_geo_unit_m2, na.rm = TRUE),
     .groups = "drop")
 
 # On teste l'aggrégation en sommant pour chaque année n_parcels et N_parcels
 # On obtient bien les mêmes nombres : OK
 
 # test <- agrreg_pra2 %>%
 #   group_by(year) %>%
 #   summarise(area = sum(n_parcels))
 # 
 # test2 <- pra_insee_filtered2 %>%
 #   group_by(year) %>%
 #   summarise(area = sum(N_Parcels))

 
 # On aggrège maintenant les données au niveau PRA et groupe/culture
 
 RPG_Aggregated_CULT_Pra <- RPG_CULT_pra %>%
   select(PRA_Code, PRA_Lib, year, CODE_CULTU,
          parcel_cult_n, surf_cult_m2, LIBELLE_CULTURE) %>%
   group_by(year, PRA_Code, PRA_Lib, CODE_CULTU, LIBELLE_CULTURE) %>% 
   summarise(n_parcels_cult = sum(parcel_cult_n),
             surf_cult_m2 = sum(surf_cult_m2),
             .groups = "drop") %>%
   full_join(agrreg_pra, by = c("PRA_Code", "year")) %>%
   mutate(perc_cult_parcel = as.numeric(n_parcels_cult / n_parcels),
          perc_cult_m2 = as.numeric(surf_cult_m2 / surf_agri_m2)
          ) 

 RPG_Aggregated_GROUP_Pra <- RPG_GROUP_pra %>%
   select(PRA_Code, PRA_Lib, year, CODE_GROUP,
          parcel_code_group_n, surf_code_group_m2, LIBELLE_GROUPE_CULTURE) %>%
   group_by(year, PRA_Code, PRA_Lib, CODE_GROUP, LIBELLE_GROUPE_CULTURE) %>% 
   summarise(n_parcels_group = sum(parcel_code_group_n),
             surf_code_group_m2 = sum(surf_code_group_m2),
             .groups = "drop") %>%
   full_join(agrreg_pra2, by = c("PRA_Code", "year")) %>%
   mutate(perc_group_parcel = as.numeric(n_parcels_group / n_parcels),
          perc_group_m2 = as.numeric(surf_code_group_m2 / surf_agri_m2)
          ) 
 
 # On teste en s'assurant que la somme des % fait bien 1 : OK
 
 # test3 <- RPG_Aggregated_GROUP_Pra %>%
 #   group_by(year, PRA_Code) %>%
 #   summarise(taux = sum(perc_group_m2), .groups = "drop")

 
 # On sauvegarde :
 
 saveRDS(
   RPG_Aggregated_CULT_Pra,
   file = here(dir$raw, "RPG_Aggregated_CULT_Pra.rds")
   )
 
 saveRDS(
   RPG_Aggregated_GROUP_Pra,
   file = here(dir$raw, "RPG_Aggregated_GROUP_Pra.rds")
   )