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


# 1.) Convert the dataset into data.table
dt <- as.data.table(RPG_Variations)  

# 2.) Extract unique values of commune, year and crop
communes <- unique(dt$insee)
annees   <- unique(dt$year)
cultures <- unique(dt$LIBELLE_GROUPE_CULTURE_AGG)

# 3.) Create a reference table with data.table::CJ()
ref <- CJ(insee = communes, year = annees, LIBELLE_GROUPE_CULTURE_AGG = cultures)

# 4.) Merge with desired columns from dt
dt_sub <- dt[, .(insee, year, LIBELLE_GROUPE_CULTURE_AGG, 
                 parcel_cult_code_group_n, parcel_cult_code_group_perc, 
                 surf_code_group_m2, surf_code_group_perc)]

# Merge ref with the reduced dt
dt_complet <- merge(
  ref,
  dt_sub,
  by = c("insee", "year", "LIBELLE_GROUPE_CULTURE_AGG"),
  all.x = TRUE
)

# complete with columns depending on communes and year
# first: creating table with information at communes-level, and which can vary across years
commune_info <- dt[
  , .(
    name = name[1],
    region_code = region_code[1],
    surf_tot_geo_unit_m2 = surf_tot_geo_unit_m2[1],
    surf_agri_geo_unit_m2 = surf_agri_geo_unit_m2[1],
    N_Parcels = N_Parcels[1]
  ),
  by = .(insee, year)
]

## add this to the table comple
dt_complet <- merge(dt_complet, 
                    commune_info, 
                    by = c("insee","year"), 
                    all.x = TRUE
                    )

# 5.) Replace NAs of numeric columns with 0
num_cols <- names(dt)[sapply(dt, is.numeric) & !(names(dt) %in% c("surf_tot_geo_unit_m2", "surf_agri_geo_unit_m2", "N_Parcels"))]

for (col in num_cols) {
  set(dt_complet, which(is.na(dt_complet[[col]])), col, 0)
}

RPG_All_final <- as.data.frame(dt_complet) %>% 
  select(
    insee, name, region_code, year, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, N_Parcels,
    LIBELLE_GROUPE_CULTURE_AGG, surf_code_group_m2, surf_code_group_perc, parcel_cult_code_group_n,   
    parcel_cult_code_group_perc
  ) %>% 
  arrange(insee, year, LIBELLE_GROUPE_CULTURE_AGG)

saveRDS(RPG_All_final, here(dir$final,"RPG_COMPLETE_Aggreg_ALL.rds"))

#===============================================================================
# 4). Calculating acreage variations over the period ------
#===============================================================================

# Make sure year is numeric
df <- RPG_All_final %>%
  mutate(year = as.numeric(year),
         surf_code_group_perc = as.numeric(surf_code_group_perc),
         surf_code_group_m2 = as.numeric(surf_code_group_m2)
  )

# Difference between year 2007 and 2023
wide_perc <- df %>%
  filter(year %in% c(2007, 2023)) %>%
  # Pivot the % variable
  select(insee, name, region_code, surf_tot_geo_unit_m2, LIBELLE_GROUPE_CULTURE_AGG, year, surf_code_group_perc) %>%
  pivot_wider(
    names_from = year,
    values_from = surf_code_group_perc,
    names_prefix = "surf_code_group_perc_",
    values_fill = 0
  )

# Pivot the m2 variable
wide_m2 <- df %>%
  filter(year %in% c(2007, 2023)) %>%
  select(insee, name, region_code, LIBELLE_GROUPE_CULTURE_AGG, year, surf_code_group_m2) %>%
  pivot_wider(
    names_from = year,
    values_from = surf_code_group_m2,
    names_prefix = "surf_code_group_m2_",
    values_fill = 0
  )

# Join them back together
result <- wide_perc %>%
  left_join(
    wide_m2,
    by = c("insee", "name", "region_code", "LIBELLE_GROUPE_CULTURE_AGG")
  )

# Compute differences in area between 2023 and 2007
diff_years <- result %>% 
  mutate(
    diff_2023_2007_share_abs = surf_code_group_perc_2023 - surf_code_group_perc_2007,
    diff_2023_2007_share_perc = ((surf_code_group_perc_2023 - surf_code_group_perc_2007) / surf_code_group_perc_2007) * 100,
    diff_2023_2007_m2_abs = surf_code_group_m2_2023 - surf_code_group_m2_2007,
    diff_2023_2007_m2_perc = ((surf_code_group_m2_2023 - surf_code_group_m2_2007) / surf_code_group_m2_2007) * 100
  )

# 2. Difference between mean of 2007–2010 and 2020–2023
wide_mean_share <- df %>%
  filter(year %in% c(2007:2010, 2020:2023)) %>%
  select(insee, region_code, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, LIBELLE_GROUPE_CULTURE_AGG, year, surf_code_group_perc) %>%
  mutate(period = case_when(
    year %in% 2007:2010 ~ "debut",
    year %in% 2020:2023 ~ "fin"
  )) %>%
  group_by(insee, LIBELLE_GROUPE_CULTURE_AGG, period) %>%
  summarise(
    mean_share = mean(surf_code_group_perc, na.rm = TRUE), 
    .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = mean_share, names_prefix = "mean_share_", values_fill = 0) 

# Add some static commune information to the table:
commune_info <- df %>%
  select(insee, region_code, surf_tot_geo_unit_m2) %>%
  group_by(insee) %>% 
  slice(1)
agri_area_2007 <- df %>%
  filter(year == 2007) %>%
  select(insee, surf_agri_geo_unit_m2) %>%
  group_by(insee) %>% 
  slice(1)
wide_mean_share_withinfo <- wide_mean_share %>%
  left_join(commune_info, by = "insee") %>%
  left_join(agri_area_2007, by = "insee")

# Now compute the mean area in m2 for the same periods
wide_mean_m2 <- df %>%
  filter(year %in% c(2007:2010, 2020:2023)) %>%
  select(insee, region_code, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, LIBELLE_GROUPE_CULTURE_AGG, year, surf_code_group_m2) %>%
  mutate(period = case_when(
    year %in% 2007:2010 ~ "debut",
    year %in% 2020:2023 ~ "fin"
  )) %>%
  group_by(insee, LIBELLE_GROUPE_CULTURE_AGG, period) %>%
  summarise(mean_m2 = mean(surf_code_group_m2, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = mean_m2, names_prefix = "mean_m2_", values_fill = 0) 

# Join them back together
result_mean <- wide_mean_share_withinfo %>%
  left_join(
    wide_mean_m2,
    by = c("insee", "LIBELLE_GROUPE_CULTURE_AGG")
  )

# Compute differences in area between mean 2020-2023 and 2007-2010
diff_mean_years <- result_mean %>%
  mutate(
    diff_fin_debut_share_abs = mean_share_fin - mean_share_debut,
    diff_fin_debut_share_perc = ((mean_share_fin - mean_share_debut) / mean_share_debut) * 100,
    diff_fin_debut_m2_abs = mean_m2_fin - mean_m2_debut,
    diff_fin_debut_m2_perc = ((mean_m2_fin - mean_m2_debut) / mean_m2_debut) * 100
  )


# 3. Combine both differences into one table
final_result <- full_join(diff_years, diff_mean_years, 
                          by = c("insee", "region_code", "LIBELLE_GROUPE_CULTURE_AGG", "surf_tot_geo_unit_m2")) %>% 
  mutate(
    surf_agri_geo_unit_m2_2007 = surf_agri_geo_unit_m2
  ) %>% 
  select(-surf_agri_geo_unit_m2) %>% 
  select(
    insee, name, region_code, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2_2007,
    LIBELLE_GROUPE_CULTURE_AGG, everything()
  ) %>% 
  arrange(insee, LIBELLE_GROUPE_CULTURE_AGG)

#===============================================================================
# 5). Creating categorical variable accounting for the presence of crops ------
#===============================================================================

final_result <- final_result |>
  mutate(
    etat = case_when(
      mean_share_debut < 0.001 & mean_share_fin < 0.001                 ~ 4,  # jamais cultivé
      mean_share_debut >= 0.001  & mean_share_fin >= 0.001              ~ 1,  # maintenu
      mean_share_debut < 0.001 & mean_share_fin >= 0.001                ~ 2,  # apparition
      mean_share_debut >= 0.001  & mean_share_fin < 0.001               ~ 3   # disparition
    ),
    etat_libelle = case_when(
      etat == 1 ~ "maintenu",
      etat == 2 ~ "apparition",
      etat == 3 ~ "disparition",
      etat == 4 ~ "jamais cultivé"
    )
  )

#===============================================================================
# Save data
saveRDS(final_result, here(dir$final, "LongPeriod_AcreageVariations.rds"))
