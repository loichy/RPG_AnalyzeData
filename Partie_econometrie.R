# =============================================================================.
# Partie économétrique : on va tenter d'expliquer l'évolution de certaines 
# surfaces par le changement climatique : variable de température/précipitation
# =============================================================================.


# ==== Load Data ====

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap, units, knitr, kableExtra, 
               plotly, viridis, shiny, vegan, paletteer, readxl, biscale, cowplot, 
               rstatix, fixest)

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


# Load the data : au niveau des PRA et par année

temp_pra_yearly <- readRDS("D:/Data/ERA5_Data/final/era5_2m_temperature_pra_yearly_1980_2024_reference_1971_2000.rds") %>%
  select(-quarter, -month, -freq)

precip_pra_yearly <- readRDS("D:/Data/ERA5_Data/final/era5_total_precipitation_pra_yearly_1980_2024_reference_1971_2000.rds") %>%
  select(-quarter, -month, -freq)

# Par soucis de concordance, le code de la PRA ne peut pas commencer par zéro

RPG_GROUP_pra <- readRDS("D:/Data/RPG_Data/final/RPG_Aggregated_GROUP_Pra.rds") %>%
  rename(code_insee = "PRA_Code") %>%
  mutate(code_insee = str_remove(code_insee, "^0")) %>%
  filter(code_insee != "75000") # On enlève PARIS - 75000 (une seule obs en 2024)

climate_pra_yearly <- temp_pra_yearly %>%
  left_join(precip_pra_yearly, 
            by = c("code_insee", "name_insee", "year", "reference_period"))

df_pra_yearly <- RPG_GROUP_pra %>%
  select(-PRA_Lib) %>%
  left_join(climate_pra_yearly, 
            by = c("code_insee", "year"), 
            relationship = "many-to-many") 


# On remarque que le nb d'observations n'est pas multiplié par 3 (malgré 3 stat 
# différentes pour températures) car daily_minimum n'a pas autant d'observations
# table(temp_pra_yearly$statistic)

temp_min <- temp_pra_yearly %>%
  filter(statistic == "daily_minimum") %>%
  select(year, code_insee, statistic)

temp_max <- temp_pra_yearly %>%
  filter(statistic == "daily_maximum") %>%
  select(year, code_insee, statistic)

check <- temp_min %>%
  full_join(temp_max, by = c("year", "code_insee")) %>%
  filter(is.na(statistic.x))


# ==== Testing Data on Basque Country ====

# On va d'abord tracer quelques graphiques pour contrôler la logique des données
# On se focalise sur une PRA pour simplifier : MONTAGNE BASQUE - 64, code 64140

df_basque <- df_pra_yearly %>%
  filter(code_insee == "64140") 

ggplot(df_basque %>%
         filter(statistic.x == "daily_mean"), aes(x = year, y = gsdd_above25)) +
  geom_line() + theme_bw() +
  labs(y = "Growing season degre-days > 25°C",
       x = "Year")

# Pics exceptionnels en 2003, 2019 et 2022 
# Correspond bien aux canicules de 2003, 2019 (records battus) et 2022

ggplot(df_basque, aes(x = year, y = R95p)) +
  geom_line() + theme_bw() +
  labs(y = "Total rainfall above the 95th percentile",
       x = "Year")

ggplot(df_basque, aes(x = year, y = R99p)) +
  geom_line() + theme_bw() +
  labs(y = "Total rainfall above the 99th percentile",
       x = "Year")

# Pic en 2013 alors que la région a connu des inondations monstrueuses : cohérent
# La série temporelle semble tout à fait logique !


# ==== Building the indicators for future models ====


# 1. Formons des bins de 3°C des GSDD :

df_pra <- df_pra_yearly 

# On définit les bornes inférieures : pour aller jusqu'à gsdd_bin_24_27

bornes_inf <- seq(0, 24, by = 3)

# Boucle pour créer les bins

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

# Cas particulier pour le dernier bin : 27_more

df_pra$gsdd_bin_27_more <- df_pra$gsdd_above27 



# ==== Modèle n°1 : Surface_plantée = B*GSDD (bins de 3°C) + epsilon ====

# On peut par exemple expliquée la surface des protéagineux : groupe 8
# On pourra différencier daily_min, max et mean 

# 1. Surface en ha

model1 <- function (code_group, nom, stat) {

df <- df_pra %>%
  filter(CODE_GROUP == code_group, statistic.x == stat)

# On peut exprimer la surface plantée en ha : 1 ha = 10 000m2

# On liste toutes les colonnes qui commencent par gsdd_bin_
les_bins <- grep("^gsdd_bin_", names(df), value = TRUE)

# On crée la formule : "surf_code_group_m2 ~ gsdd_bin_0_3 + gsdd_bin_03_06 + ..."

reg_surf_mean <- feols(
  as.formula(paste("log(surf_code_group_m2/10000) ~", paste(les_bins, collapse = " + "))), 
  df)

# On peut rajouter des effets fixes années :

fe_surf_mean1 <- feols(
  log(surf_code_group_m2/10000) ~ gsdd_bin_00_03 + gsdd_bin_03_06 + gsdd_bin_06_09 
  + gsdd_bin_09_12 + gsdd_bin_12_15 + gsdd_bin_15_18 + gsdd_bin_18_21 + 
    gsdd_bin_21_24 + gsdd_bin_24_27 + gsdd_bin_27_more | year, df)

# Et aussi des effets fixes PRA :

fe_surf_mean2 <- feols(
  log(surf_code_group_m2/10000) ~ gsdd_bin_00_03 + gsdd_bin_03_06 + gsdd_bin_06_09 
  + gsdd_bin_09_12 + gsdd_bin_12_15 + gsdd_bin_15_18 + gsdd_bin_18_21 + 
    gsdd_bin_21_24 + gsdd_bin_24_27 + gsdd_bin_27_more | year + code_insee, df)

# On arrange les titres pour que ce soit plus lisible :

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
    "(Intercept)"    = "Constante",
  "log(surf_code_group_m2/10000)" = paste0("Log(Surface de ", nom, " en ha)")
)

latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2,
       dict = mon_dico,
       tex = T)

return(latex)
}

model1("8", "Protéagineux", "daily_mean")

model1("1", "Blé", "daily_mean")

model1("2", "Maïs", "daily_mean")

model1("21", "Vignes", "daily_mean")

model1("20", "Vergers", "daily_mean")


# 2. Surface en %

model2 <- function (code_group, nom, stat) {
  
  df <- df_pra %>%
    filter(CODE_GROUP == code_group, statistic.x == stat)
  
  # On peut exprimer la surface plantée en ha : 1 ha = 10 000m2
  
  # On liste toutes les colonnes qui commencent par gsdd_bin_
  les_bins <- grep("^gsdd_bin_", names(df), value = TRUE)
  
  # On crée la formule : "surf_code_group_m2 ~ gsdd_bin_0_3 + gsdd_bin_03_06 + ..."
  
  reg_surf_mean <- feols(
    as.formula(paste("log(perc_group_m2) ~", paste(les_bins, collapse = " + "))), 
    df)
  
  # On peut rajouter des effets fixes années :
  
  fe_surf_mean1 <- feols(
    log(perc_group_m2) ~ gsdd_bin_00_03 + gsdd_bin_03_06 + gsdd_bin_06_09 
    + gsdd_bin_09_12 + gsdd_bin_12_15 + gsdd_bin_15_18 + gsdd_bin_18_21 + 
      gsdd_bin_21_24 + gsdd_bin_24_27 + gsdd_bin_27_more | year, df)
  
  # Et aussi des effets fixes PRA :
  
  fe_surf_mean2 <- feols(
    log(perc_group_m2) ~ gsdd_bin_00_03 + gsdd_bin_03_06 + gsdd_bin_06_09 
    + gsdd_bin_09_12 + gsdd_bin_12_15 + gsdd_bin_15_18 + gsdd_bin_18_21 + 
      gsdd_bin_21_24 + gsdd_bin_24_27 + gsdd_bin_27_more | year + code_insee, df)
  
  # On arrange les titres pour que ce soit plus lisible :
  
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
    "(Intercept)"    = "Constante",
    "log(perc_group_m2)" = paste0("Log(Surface de ", nom, " en %)")
  )
  
  latex <- etable(reg_surf_mean, fe_surf_mean1, fe_surf_mean2,
                  dict = mon_dico,
                  tex = T)
  
  return(latex)
}


model2("8", "Protéagineux", "daily_mean")

model2("1", "Blé", "daily_mean")

model2("2", "Maïs", "daily_mean")

model2("21", "Vignes", "daily_mean")

model2("20", "Vergers", "daily_mean")
