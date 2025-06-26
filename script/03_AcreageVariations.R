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
# 1). Load and prepare dataset ------
#===============================================================================

# test avec Bretagne car fichier trop lourd
# RPG_53 <- readRDS(here(dir$raw, "RPG_Aggregated_Brittany.rds"))

RPG_All <- readRDS(here(dir$raw, "RPG_Aggregated_All.rds")) %>% 
  arrange(insee, LIBELLE_GROUPE_CULTURE, year)


#===============================================================================
# 1). Calculating acreage variations over the period ------
#===============================================================================

# Make sure year is numeric
df <- RPG_All %>%
  mutate(year = as.numeric(year),
         surf_code_group_perc = as.numeric(surf_code_group_perc))

# Difference between year 2007 and 2023
diff_years <- df %>%
  filter(year %in% c(2007, 2023)) %>%
  select(insee, CODE_GROUP, LIBELLE_GROUPE_CULTURE, year, surf_code_group_perc) %>%
  pivot_wider(names_from = year, values_from = surf_code_group_perc, names_prefix = "year_", values_fill = 0) %>%
  mutate(diff_2007_2023_abs = year_2023 - year_2007,
         diff_2007_2023_perc = ((year_2023 - year_2007) / year_2007) * 100)

# 2. Difference between mean of 2007–2010 and 2020–2023
diff_means <- df %>%
  filter(year %in% c(2007:2010, 2020:2023)) %>%
  select(insee, CODE_GROUP, LIBELLE_GROUPE_CULTURE, year, surf_code_group_perc) %>%
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


# Example of linear model estimated with OLS:

RPG_cult1 <- final_result %>% 
  filter(LIBELLE_GROUPE_CULTURE == "Blé tendre") %>% 
  filter(!is.na(year_2007) & !is.na(year_2023))

reg1 <- lm(as.numeric(year_2023) ~ as.numeric(year_2007), data = final_result)
summary(reg1)
str(reg1)
res_reg1 <- reg1$residuals

#===============================================================================
# Save data
saveRDS(final_result, here(dir$final, "LongPeriod_AcreageVariations.rds"))
