# =============================================================================.
# Econometric Analysis: we want to measure the impact of climate change (through 
# long term changes in temperature, precipitations, GSL) on crop choice.
# =============================================================================.


# ==== Load Data ====

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap, units, knitr, kableExtra, 
               plotly, viridis, shiny, vegan, paletteer, readxl, biscale, cowplot, 
               rstatix, fixest, dplyr, stringr, modelsummary)

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


# Load the data: yearly and PRA located data

temp_pra_yearly <- readRDS("D:/Data/ERA5_Data/final/era5_2m_temperature_pra_yearly_1980_2024_reference_1971_2000.rds") %>%
  select(-quarter, -month, -freq)

precip_pra_yearly <- readRDS("D:/Data/ERA5_Data/final/era5_total_precipitation_pra_yearly_1980_2024_reference_1971_2000.rds") %>%
  select(-quarter, -month, -freq)

gaez_data <- readRDS("D:/Data/GAEZ_Data/Final/GAEZ_yieldchange_pra.rds")


# Adding the RPG cultures to the dataframe
# 7 crops from the gaez data don't have an equivalent in the rpg groups

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
) %>%
  rename("crop" = "culture_gaez")


gaez_filtered <- gaez_data %>%
  full_join(correspondance_gaez_rpg, by = "crop") %>%
  # We choose to filter by one model, one projection and one variable, if not : 
  # group_by(model, rcp, variable) %>%
  filter(model == "IPSL-CM5A-LR", rcp == "rcp6p0", variable == "ylHr0") %>%
  arrange(pra_code, groupe_rpg) %>%
  group_by(pra_code, groupe_rpg) %>%
  # Create weights associated to each GAEZ crop
  mutate(
    sum_value_hist_group = sum(value_hist[value_hist > 0], na.rm = T), # Compute for each RPG category to the total cumulated yields from each PRA
    weight = if_else(sum_value_hist_group > 0,
                     value_hist / sum_value_hist_group, # Weight corresponds to the proportion of each GAEZ crop yield in the total yield of the RPG category
                     0)) %>%
  ungroup()


# Aggregate at RPG level while taking into account the weights associated to each GAEZ crops
gaez_pra <- gaez_filtered %>% 
  group_by(pra_code, groupe_rpg) %>% 
  summarise(
    value_hist_rpg = sum(value_hist * weight, na.rm = TRUE),
    value_futur_rpg = sum(value * weight, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(change_group = (value_futur_rpg - value_hist_rpg)/value_hist_rpg) %>% 
  unique() %>%
  rename("LIBELLE_GROUPE_CULTURE" = "groupe_rpg",
         "code_insee" = "pra_code") %>%
  mutate(code_insee = str_remove(code_insee, "^0"))


# To join the data, the PRA codes cannot start with a 0

RPG_GROUP_pra <- readRDS("D:/Data/RPG_Data/final/RPG_Aggregated_GROUP_Pra.rds") %>%
  rename(code_insee = "PRA_Code") %>%
  mutate(code_insee = str_remove(code_insee, "^0")) %>%
  filter(code_insee != "75000") # We take Paris out - 75000 (only one observation)

climate_pra_yearly <- temp_pra_yearly %>%
  left_join(precip_pra_yearly, 
            by = c("code_insee", "name_insee", "year", "reference_period"))

df_pra_yearly <- RPG_GROUP_pra %>%
  select(-PRA_Lib) %>%
  left_join(climate_pra_yearly, 
            by = c("code_insee", "year"), 
            relationship = "many-to-many") %>%
  left_join(gaez_pra, by = c("LIBELLE_GROUPE_CULTURE", "code_insee")) %>% 
  mutate(
    change_group = replace(change_group, is.na(change_group), 0),
    value_hist_rpg = replace(value_hist_rpg, is.na(value_hist_rpg), 0),
    value_futur_rpg = replace(value_futur_rpg, is.na(value_futur_rpg), 0)
         ) 

# We see that the number of observations isn't multiplied by 3 (eventhough there
# are 3 differents statistics) : daily_minimum doesn't have as many observations
# table(temp_pra_yearly$statistic)

# temp_min <- temp_pra_yearly %>%
#   filter(statistic == "daily_minimum") %>%
#   select(year, code_insee, statistic)
# 
# temp_max <- temp_pra_yearly %>%
#   filter(statistic == "daily_maximum") %>%
#   select(year, code_insee, statistic)
# 
# check <- temp_min %>%
#   full_join(temp_max, by = c("year", "code_insee")) %>%
#   filter(is.na(statistic.x))


# ==== Testing Data on Basque Country ====

# Let's plot a few graphs to test the logic of the data
# We focus on one PRA to simplify: MONTAGNE BASQUE - 64, code 64140

df_basque <- df_pra_yearly %>%
  filter(code_insee == "64140") 

ggplot(df_basque %>%
         filter(statistic.x == "daily_mean"), aes(x = year, y = gsdd_above25)) +
  geom_line() + theme_bw() +
  labs(y = "Growing season degre-days > 25°C",
       x = "Year")

# Exceptionnal temperatures in 2003, 2019 and 2022 
# Which matches well the 2003, 2019 and 2022 heatwaves

ggplot(df_basque, aes(x = year, y = R95p)) +
  geom_line() + theme_bw() +
  labs(y = "Total rainfall above the 95th percentile",
       x = "Year")

ggplot(df_basque, aes(x = year, y = R99p)) +
  geom_line() + theme_bw() +
  labs(y = "Total rainfall above the 99th percentile",
       x = "Year")

# Pic in 2013: region experienced large floods: consistent
# The time series makes perfect sense!


# ==== Building the indicators for future models ====

# Bins created in the function directly !

# We want to control for total annual precipitation
# We multiply by 1000 to have the evolution in mm instead of m

df_pra_yearly$total_precip_wet_days_mm <- 1000*df_pra_yearly$total_precip_wet_days

df_pra <- df_pra_yearly

# We define the lower bounds: to go until gsdd_bin_24_27

bornes_inf <- seq(0, 24, by = 3)

# Loop to create the bins 

for (i in bornes_inf) {
  inf <- i
  sup <- i+3
  
  # Formatage avec deux chiffres (ex: 00, 03, 06...)
  col_inf <- paste0("gsdd_above", sprintf("%02d", inf))
  col_sup <- paste0("gsdd_above", sprintf("%02d", sup))
  nom_bin <- paste0("gsdd_bin_", sprintf("%02d", inf), "_", sprintf("%02d", sup))
  
  # Calcul de la différence 
  df_pra[[nom_bin]] <- df_pra[[col_inf]] - df_pra[[col_sup]]
}

# Special case for the last bin: 27_more

df_pra$gsdd_bin_27_more <- df_pra$gsdd_above27


mon_dico <- c(
  "gsdd_bin_00_03"  = "Tranche GSDD [0°C - 3°C]",
  "gsdd_bin_03_06" = "Tranche GSDD [3°C - 6°C]",
  "gsdd_bin_06_09" = "Tranche GSDD [6°C - 9°C]",
  "gsdd_bin_09_12" = "Tranche GSDD [9°C - 12°C]",
  "gsdd_bin_12_15" = "Tranche GSDD [12°C - 15°C]",
  "gsdd_bin_15_18" = "Tranche GSDD [15°C - 18°C]",
  "gsdd_bin_18_21" = "Tranche GSDD [18°C - 21°C]",
  "gsdd_bin_21_24" = "Tranche GSDD [21°C - 24°C]",
  "gsdd_bin_24_27" = "Tranche GSDD [24°C - 27°C]",
  "gsdd_bin_27_more" = "Tranche GSDD [+27°C]",
  "gsdd_bin_00_10" = "Tranche GSDD [0°C - 10°C]",
  "gsdd_bin_10_26" = "Tranche GSDD [10°C - 26°C]",
  "gsdd_bin_26_more" = "Tranche GSDD [+26°C]",
  "total_precip_wet_days_mm" = "PRCP totales annuelles",
  "total_precip_wet_days_mm2" = "(PRCP totales annuelles)$^2$",
  "gsl_days" = "GSL",
  "gsl_start_doy" = "1st GSL doy",
  "year" = "Année",
  "code_insee" = "PRA"
)

# ==== Descriptive statistics ====

stat_desc <- df_pra %>%
  filter(statistic.x == "daily_mean") %>%
  get_summary_stats(gsdd_bin_00_03, gsdd_bin_03_06, gsdd_bin_06_09, 
                    gsdd_bin_09_12, gsdd_bin_12_15,  gsdd_bin_15_18, 
                    gsdd_bin_18_21, gsdd_bin_21_24, gsdd_bin_24_27, gsdd_bin_27_more, 
                    gsl_days, gsl_start_doy, 
                    total_precip_wet_days_mm,
                    change_group) %>%
  select(variable, min, q1, median, mean, q3, max, sd)

df_stat <- df_pra %>%
  filter(statistic.x == "daily_mean") 

datasummary(
  ('Tranche GSDD [0°C - 3°C]' = gsdd_bin_00_03) + 
    ('Tranche GSDD [3°C - 6°C]' = gsdd_bin_03_06) + 
    ('Tranche GSDD [6°C - 9°C]' = gsdd_bin_06_09) +
    ('Tranche GSDD [9°C - 12°C]' = gsdd_bin_09_12) + 
    ('Tranche GSDD [12°C - 15°C]' = gsdd_bin_12_15) +  
    ('Tranche GSDD [15°C - 18°C]' = gsdd_bin_15_18) + 
    ('Tranche GSDD [18°C - 21°C]' = gsdd_bin_18_21) + 
    ('Tranche GSDD [21°C - 24°C]' = gsdd_bin_21_24) + 
    ('Tranche GSDD [24°C - 27°C]' = gsdd_bin_24_27) + 
    ('Tranche GSDD [+27°C]' = gsdd_bin_27_more) + 
    ('GSL' = gsl_days) + 
    ('1st GSL doy' = gsl_start_doy) + 
    ('PRCP totales annuelles en mm' = total_precip_wet_days_mm) +
    ('Taux de croissance des rendements potentiels' = change_group) ~   
    N + Mean + SD + Median + Min + Max,                            
  data = df_stat,
  output = "latex"   
)

stat_desc %>%
  mutate(variable = recode(
    variable,
    "gsdd_bin_00_03"  = "Tranche GSDD [0°C - 3°C]",
    "gsdd_bin_03_06" = "Tranche GSDD [3°C - 6°C]",
    "gsdd_bin_06_09" = "Tranche GSDD [6°C - 9°C]",
    "gsdd_bin_09_12" = "Tranche GSDD [9°C - 12°C]",
    "gsdd_bin_12_15" = "Tranche GSDD [12°C - 15°C]",
    "gsdd_bin_15_18" = "Tranche GSDD [15°C - 18°C]",
    "gsdd_bin_18_21" = "Tranche GSDD [18°C - 21°C]",
    "gsdd_bin_21_24" = "Tranche GSDD [21°C - 24°C]",
    "gsdd_bin_24_27" = "Tranche GSDD [24°C - 27°C]",
    "gsdd_bin_27_more" = "Tranche GSDD [+27°C]",
    "total_precip_wet_days_mm" = "PRCP totales annuelles",
    "total_precip_wet_days_mm2" = "(PRCP totales annuelles)$^2$",
    "gsl_days" = "GSL",
    "gsl_start_doy" = "1st GSL doy",
    "change_group" = "Taux de croissance des rendements potentiels"
  )) %>%
  kable(booktabs = T,"latex") %>% # Add "latex" to get the Latex table
  kable_styling()


# ==== Model n°1 : Planted_surface = B*GSDD (3°C bins) + ... + epsilon ====

# For the moment, we will only use the daily_mean statistic 

# 1. Surface in ha

model1 <- function (code_group, nom, stat) {

df <- df_pra %>%
  filter(CODE_GROUP == code_group, statistic.x == stat)

# 1 ha = 10 000m2

# We list all the columns that start with gsdd_bin_
les_bins <- grep("^gsdd_bin_", names(df), value = TRUE)

# We will want to control for precipitations 
df$total_precip_wet_days_mm2 <- df$total_precip_wet_days_mm^2

controls <- c("total_precip_wet_days_mm", "total_precip_wet_days_mm2")

# We create the formula: "surf_code_group_m2 ~ gsdd_bin_0_3 + gsdd_bin_03_06 + ..."

reg_surf_mean <- feols(
  as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "))), 
  df)

# We add year fixed effects

fe_surf_mean1 <- feols(
  as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), "| year")), 
  df)

# And PRA fixed effects

fe_surf_mean2 <- feols(
  as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), "| year + code_insee")),
  df)


# We add precipitation controls

fe_surf_mean3 <- feols(
  as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), 
                   "+", paste(controls, collapse = " + "), "| year + code_insee")),
  df)

# We add potential yield controls

fe_surf_mean4 <- feols(
  as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), 
                   "+", paste(controls, collapse = " + "), "+ i(year, change_group)", "| year + code_insee")),
  df)


latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2, fe_surf_mean3,
                fe_surf_mean4,
       dict = mon_dico,
       tex = F)

return(latex)
}

model1("8", "Protéagineux", "daily_mean")

model1("1", "Blé", "daily_mean")

model1("2", "Maïs", "daily_mean")

model1("21", "Vignes", "daily_mean")



# 2. Surface in %

model1_bis <- function (code_group, nom, stat) {
  
  df <- df_pra %>%
    filter(CODE_GROUP == code_group, statistic.x == stat)
  
  # We list all the columns that start with gsdd_bin_
  les_bins <- grep("^gsdd_bin_", names(df), value = TRUE)
  
  # We will want to control for precipitations 
  df$total_precip_wet_days_mm2 <- df$total_precip_wet_days_mm^2
  
  controls <- c("total_precip_wet_days_mm", "total_precip_wet_days_mm2")
  
  # We create the formula: "surf_code_group_m2 ~ gsdd_bin_0_3 + gsdd_bin_03_06 + ..."
  reg_surf_mean <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "))), 
    df)
  
  # We add year fixed effects
  fe_surf_mean1 <- feols(
      as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), "| year")), 
      df)
  
  # And PRA fixed effects
  fe_surf_mean2 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), "| year + code_insee")),
    df)
  
  # We add precipitation controls
  fe_surf_mean3 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), 
                     "+", paste(controls, collapse = " + "), "| year + code_insee")),
    df)
  
  # We add potential yield controls
  
  fe_surf_mean4 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), 
                     "+", paste(controls, collapse = " + "), "+ i(year, change_group)", "| year + code_insee")),
    df)
  
  latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2, fe_surf_mean3,
                  fe_surf_mean4,
                  dict = mon_dico,
                  tex = F)
  
  return(latex)
}


model1_bis("8", "Protéagineux", "daily_mean")

model1_bis("1", "Blé", "daily_mean")

model1_bis("2", "Maïs", "daily_mean")

model1_bis("21", "Vignes", "daily_mean")


# ==== Model n°2 : Planted_surface = B*GSL + ... + epsilon ====

# 1. Surface in ha

model2 <- function (code_group, nom, stat) {
  
  df <- df_pra %>%
    filter(CODE_GROUP == code_group, statistic.x == stat)
  
  # We want to capture non-linear effects
  df$gsl_days2 <- df$gsl_days^2
  
  df$gsl_start_doy2 <- df$gsl_start_doy^2

  reg_surf_mean <- feols(
    log(surf_code_group_m2/10000) ~ gsl_days + gsl_days2, df)
  
  # We add year fixed effects
  
  fe_surf_mean1 <- feols(
    log(surf_code_group_m2/10000) ~ gsl_days + gsl_days2 | year, df)
  
  # And PRA fixed effects
  
  fe_surf_mean2 <- feols(
    log(surf_code_group_m2/10000) ~ gsl_days + gsl_days2 | year + code_insee, df)
  
  fe_surf_mean3 <- feols(
    log(surf_code_group_m2/10000) ~ gsl_days + gsl_days2
    + gsl_start_doy + gsl_start_doy2 | year + code_insee, df)
  

  mon_dico <- c(
    "gsl_days" = "GSL",
    "gsl_days2" = "(GSL)$^2$",
    "(Intercept)"    = "Constante",
    "log(surf_code_group_m2/10000)" = paste0("Log(Surface de ", nom, " en ha)"),
    "year" = "Année",
    "code_insee" = "PRA",
    "gsl_start_doy" = "1st GSL doy",
    "gsl_start_doy2" = "(1st GSL doy)$^2$"
  )
  
  latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2, fe_surf_mean3,
                  dict = mon_dico,
                  tex = F)
  
  return(latex)
}

model2("8", "Protéagineux", "daily_mean")

model2("1", "Blé", "daily_mean")

model2("2", "Maïs", "daily_mean")

model2("21", "Vignes", "daily_mean")



# 2. Surface in %

model2_bis <- function (code_group, nom, stat) {
  
  df <- df_pra %>%
    filter(CODE_GROUP == code_group, statistic.x == stat)
  
  # We want to capture non-linear effects
  df$gsl_days2 <- df$gsl_days^2

  df$gsl_start_doy2 <- df$gsl_start_doy^2
  

  reg_surf_mean <- feols(
    log(perc_group_m2) ~ gsl_days + gsl_days2, df)
  
  # We add year fixed effects
  
  fe_surf_mean1 <- feols(
    log(perc_group_m2) ~ gsl_days + gsl_days2 | year, df)
  
  # And PRA fixed effects
  
  fe_surf_mean2 <- feols(
    log(perc_group_m2) ~ gsl_days + gsl_days2 | year + code_insee, df)
  
  fe_surf_mean3 <- feols(
    log(perc_group_m2) ~ gsl_days + gsl_days2
    + gsl_start_doy + gsl_start_doy2 | year + code_insee, df)
  

  mon_dico <- c(
    "gsl_days" = "GSL",
    "gsl_days2" = "(GSL)$^2$",
    "(Intercept)"    = "Constante",
    "log(perc_group_m2)" = paste0("Log(Surface de ", nom, " en %)"),
    "year" = "Année",
    "code_insee" = "PRA",
    "gsl_start_doy" = "1st GSL doy",
    "gsl_start_doy2" = "(1st GSL doy)$^2$"
  )
  
  latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2, fe_surf_mean3,
                  dict = mon_dico,
                  tex = F)
  
  return(latex)
}

model2_bis("8", "Protéagineux", "daily_mean")

model2_bis("1", "Blé", "daily_mean")

model2_bis("2", "Maïs", "daily_mean")

model2_bis("21", "Vignes", "daily_mean")


# ==== Model n°3 : Planted_surface = B*GSDD(non linear function) + ... + epsilon ====

# We will tend to use the daily_maximum here

# 1. Surface in ha

model3 <- function (code_group, nom, stat, breaks) {
  
  df <- df_pra_yearly %>%
    filter(CODE_GROUP == code_group, statistic.x == stat)
  
  # We define the lower bounds: to go until gsdd_bin_26_more
  
  # Loop to create the bins 
  
  # Loop to create the bins between breakpoints
  for (i in seq_along(breaks)) {
    inf <- breaks[i]
    sup <- if (i < length(breaks)) breaks[i + 1] else NULL
    
    col_inf <- paste0("gsdd_above", sprintf("%02d", inf))
    
    if (!is.null(sup)) {
      # Standard bin between two breakpoints
      col_sup <- paste0("gsdd_above", sprintf("%02d", sup))
      nom_bin <- paste0("gsdd_bin_", sprintf("%02d", inf), "_", sprintf("%02d", sup))
      df[[nom_bin]] <- df[[col_inf]] - df[[col_sup]]
    } else {
      # Last bin: above the last breakpoint
      nom_bin <- paste0("gsdd_bin_", sprintf("%02d", inf), "_more")
      df[[nom_bin]] <- df[[col_inf]]
    }
  }
  
  # 1 ha = 10 000m2
  
  # We list all the columns that start with gsdd_bin_
  les_bins <- grep("^gsdd_bin_", names(df), value = TRUE)
  
  # We will want to control for precipitations
  df$total_precip_wet_days_mm2 <- df$total_precip_wet_days_mm^2
  
  # We define the controls we want to set
  
  controls <- c("total_precip_wet_days_mm", "total_precip_wet_days_mm2")
  
  # We create the formula: "surf_code_group_m2 ~ gsdd_bin_0_3 + gsdd_bin_03_06 + ..."
  
  reg_surf_mean <- feols(
    as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "))), 
    df)
  
  # We add year fixed effects
  
  fe_surf_mean1 <- feols(
    as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), "| year")), 
    df)
  
  # And PRA fixed effects
  
  fe_surf_mean2 <- feols(
    as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), "| year + code_insee")),
    df)
  
  
  # We add precipitation and potentiel yields controls
  
  fe_surf_mean3 <- feols(
    as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), 
                     "+", paste(controls, collapse = " + "), "| year + code_insee")),
    df)
  
  # We add potential yield controls
  
  fe_surf_mean4 <- feols(
    as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "), 
                     "+", paste(controls, collapse = " + "), "+ i(year, change_group)", "| year + code_insee")),
    df)
  
  latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2, fe_surf_mean3,
                  fe_surf_mean4,
                  dict = mon_dico,
                  tex = F)
  
  return(latex)
}

model3("8", "Protéagineux", "daily_maximum", c(0, 10, 26))

model3("1", "Blé", "daily_maximum", c(0, 10, 26))

model3("2", "Maïs", "daily_maximum", c(0, 10, 26))

model3("21", "Vignes", "daily_maximum", c(0, 10, 26))

# 1. Surface in ha

model3_bis <- function (code_group, nom, stat, breaks) {
  
  df <- df_pra_yearly %>%
    filter(CODE_GROUP == code_group, statistic.x == stat)
  
  # We define the lower bounds: to go until gsdd_bin_26_more
  
  # Loop to create the bins 
  
  # Loop to create the bins between breakpoints
  for (i in seq_along(breaks)) {
    inf <- breaks[i]
    sup <- if (i < length(breaks)) breaks[i + 1] else NULL
    
    col_inf <- paste0("gsdd_above", sprintf("%02d", inf))
    
    if (!is.null(sup)) {
      # Standard bin between two breakpoints
      col_sup <- paste0("gsdd_above", sprintf("%02d", sup))
      nom_bin <- paste0("gsdd_bin_", sprintf("%02d", inf), "_", sprintf("%02d", sup))
      df[[nom_bin]] <- df[[col_inf]] - df[[col_sup]]
    } else {
      # Last bin: above the last breakpoint
      nom_bin <- paste0("gsdd_bin_", sprintf("%02d", inf), "_more")
      df[[nom_bin]] <- df[[col_inf]]
    }
  }
  
  # 1 ha = 10 000m2
  
  # We list all the columns that start with gsdd_bin_
  les_bins <- grep("^gsdd_bin_", names(df), value = TRUE)
  
  # We will want to control for precipitations
  df$total_precip_wet_days_mm2 <- df$total_precip_wet_days_mm^2
  
  # We define the controls we want to set
  
  controls <- c("total_precip_wet_days_mm", "total_precip_wet_days_mm2")
  
  # We create the formula: "perc_group_m2 ~ gsdd_bin_... + ..."
  
  reg_surf_mean <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "))), 
    df)
  
  # We add year fixed effects
  
  fe_surf_mean1 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), "| year")), 
    df)
  
  # And PRA fixed effects
  
  fe_surf_mean2 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), "| year + code_insee")),
    df)
  
  
  # We add precipitation controls
  
  fe_surf_mean3 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), 
                     "+", paste(controls, collapse = " + "), "| year + code_insee")),
    df)
  
  # We add potential yield controls
  
  fe_surf_mean4 <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "), 
                     "+", paste(controls, collapse = " + "), "+ i(year, change_group)", "| year + code_insee")),
    df)

  
  
  latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2, fe_surf_mean3,
                  fe_surf_mean4,
                  dict = mon_dico,
                  tex = F)
  
  return(latex)
}

model3_bis("8", "Protéagineux", "daily_maximum", c(0, 10, 26))

model3_bis("1", "Blé", "daily_maximum", c(0, 10, 26))

model3_bis("2", "Maïs", "daily_maximum", c(0, 10, 26))

model3_bis("21", "Vignes", "daily_maximum", c(0, 10, 26))
