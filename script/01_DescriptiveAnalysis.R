#===============================================================================
# Description: Scrip to run some descriptive statistics and graph of the RPG data
# for years 2007-2023 at the commune level
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
dir$faostat_data <- here(dir$root, "faostat_data")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#===============================================================================
# 2) Complete table of RPG France data - cleaned data
#===============================================================================

#Step 1: aggregation of data for regions ‘R11’ ‘R24’ ‘R27’ ‘R28’ ‘R32’ ‘R44’ ‘R52’ ‘R53’ ‘R75’ ‘R76’ ‘R84’ ‘R93’ ‘R94’ into a single table 
#Step 2: data cleansing - elimination of null data

RPG_aggregated_all <- readRDS(
  here(dir$final, paste0("RPG_Aggregated_ALL.rds"))
)

#===============================================================================
# 3) Generate contrasted colours palette
#===============================================================================
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

#===============================================================================
# 4) Generate descriptive statistics ------
#===============================================================================

# First overview of the data (mean, median, quartiles ...)
summary(RPG_aggregated_all)

# Distribution of crop types by number of plots cultivated in France
year_simplified <- c(2007, 2012, 2017, 2023)
RPG_aggregated_simplified <- RPG_aggregated_all %>%
  filter(year %in% year_simplified)

# Aggregate per year and crop 
df_parcelles <- RPG_aggregated_simplified %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  summarise(N = sum(N_Parcels, na.rm = TRUE), .groups = "drop")

ggplot(df_parcelles, aes(x = LIBELLE_GROUPE_CULTURE, y = N)) +
  geom_col(fill = "darkgrey") +
  facet_wrap(~ year, ncol = 2) +
  labs(
    title = "Nombre de parcelles par type de culture",
    x = "Type de culture",
    y = "Nombre de parcelles"
  ) +
  coord_flip() +
  theme_minimal()

ggsave(
  filename = here(dir$output, paste0("nombre_parcelles_culture_France.png")),
  width = 10,
  height = 6
)

# Change in agricultural area over time - violin plot
RPG_aggregated_all %>%
  ggplot(aes(x = factor(year), y = surf_agri_geo_unit_m2)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, color = "black") + # Ajout d'une boîte 
  labs(title = "Distribution de la surface agricole totale par année en Bretagne (2007-2023)",
       x = "Année", y = "Surface agricole totale (m2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = here(dir$output, paste0("violin_plot_France.png"))
)

#===============================================================================
# 5) Inferential statistics - analysis of changes in crop groups between 2007 
# and 2023 -----
#===============================================================================

# Changes in the various crop groups between 2007 and 2023 (top crops per year)
cultures_groupes <- RPG_aggregated_all %>%
  group_by(CODE_GROUP, LIBELLE_GROUPE_CULTURE, year) %>%
  summarise(surface_groupe = sum(surf_code_group_m2),
            parcels_groupe = sum(parcel_cult_code_group_n),
            .groups = "drop")

print(cultures_groupes)

top_cultures_par_an <- cultures_groupes %>% 
  group_by(year) %>% 
  slice_max(surface_groupe, n = 3)

print(top_cultures_par_an)

## Scatter plot with size proportional to agricultural area
png(here(dir$output, "graphique_dispersion_cultures.png"), width = 800, height = 600)
plot(
  cultures_groupes$year, 
  cultures_groupes$CODE_GROUP, 
  cex = cultures_groupes$surface_groupe / max(cultures_groupes$surface_groupe, na.rm = TRUE) * 5,  
  col = factor(cultures_groupes$CODE_GROUP),
  xlab = "Année",
  ylab = "Groupe de culture",
  main = "Nuage de points avec taille proportionnelle à la surface agricole",
)

dev.off()

# Share of different crop groups in total agricultural area 
# Relative growth in areas and plots from one year to the next
cultures_groupes_growth_rates <- cultures_groupes %>%
  group_by(year, CODE_GROUP, LIBELLE_GROUPE_CULTURE) %>%
  arrange(CODE_GROUP, year) %>% 
  group_by(CODE_GROUP, LIBELLE_GROUPE_CULTURE) %>% 
  mutate(
    taux_croissance = (surface_groupe - lag(surface_groupe)) / lag(surface_groupe) * 100,
    taux_croissance_parcels = (parcels_groupe - lag(parcels_groupe)) / lag(parcels_groupe) * 100
  ) %>%
  mutate(
    mean_taux_croissance = mean(taux_croissance, na.rm = TRUE), 
    mean_taux_croissance_parcels = mean(taux_croissance_parcels, na.rm = TRUE))

print(cultures_groupes_growth_rates)

## Related graphs based on growth rates (area cultivated and number of plots cultivated)
### Area growth rates
cultures_groupes_growth_rates %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(x = year, y = taux_croissance, color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Taux de croissance des surfaces cultivées") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("taux_croissance_surface_France.png"))
)

### Growth rates by plot of land cultivated 
cultures_groupes_growth_rates %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(x = year, y = taux_croissance_parcels, color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Taux de croissance des parcelles cultivées") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("taux_croissance_parcelles_France.png"))
)

### Graph showing mean growth rates in cultivated area for crop groups over the period 2007-2023
cultures_groupes_growth_rates %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(x = year, y = as.numeric(mean_taux_croissance), color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Taux de croissance moyen des surfaces cultivées") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("mean_growth_rate_surf_France.png"))
)

### Graph showing mean growth rates in the number of cultivated plots for crop groups over the period 2007-2023
cultures_groupes_growth_rates %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(x = year, y = as.numeric(mean_taux_croissance_parcels), color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Taux de croissance moyen des surfaces cultivées") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("mean_growth_rate_parcels_France.png"))
)

### Combined plot of mean (= general trend for crop groups over the period) and annual growth rates 
# By cultivated agricultural area
cultures_groupes_growth_rates %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  ggplot(aes(x = year, y = as.numeric(taux_croissance), color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_line(alpha = 0.3) +  # annual fluctuation lines
  geom_point(alpha = 0.5) + 
  geom_hline(aes(yintercept = as.numeric(mean_taux_croissance), color = LIBELLE_GROUPE_CULTURE), linetype = "dashed", size = 0.8) + # mean line
  scale_color_manual(values = c25) +
  labs(
    title = "Fluctuations annuelles et moyennes des groupes de culture",
    y = "Taux de croissance des surfaces cultivées"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("combined_plot_surf_France.png"))
)

# By number of plots cultivated 
cultures_groupes_growth_rates %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  ggplot(aes(x = year, y = as.numeric(taux_croissance_parcels), color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_line(alpha = 0.3) + # annual fluctuation line
  geom_point(alpha = 0.5) + 
  geom_hline(aes(yintercept = as.numeric(mean_taux_croissance_parcels), color = LIBELLE_GROUPE_CULTURE), linetype = "dashed", size = 0.8) + # mean line
  scale_color_manual(values = c25) +
  labs(
    title = "Fluctuations annuelles et moyennes des groupes de culture",
    y = "Taux de croissance du nombre de parcelles cultivées"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("combined_plot_parcels_France.png"))
)
## Graph according to the location of crops in the communes
RPG_aggregated_all %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  summarise(nb_communes = n_distinct(name)) %>%
  ggplot(aes(x = year, y = nb_communes, color = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Nombre de communes")

ggsave(
  filename = here(dir$output, paste0("implantation_cultures_communes_France.png"))
)

# Emergence / disappearance of culture groups over time
## Detailed growth rates of crop groups over the years of the study period
# Measurement of growth in relation to a fixed reference year (year of implementation) = overall view of the trend over the period studied
year_by_culture <- RPG_aggregated_all %>%
  group_by(CODE_GROUP, LIBELLE_GROUPE_CULTURE, year) %>%
  summarise(
    annee_min = min(year),
    surface_group = sum(surf_code_group_m2, na.rm = TRUE),
    parcels_cult = sum(parcel_cult_code_group_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CODE_GROUP, LIBELLE_GROUPE_CULTURE) %>%
  mutate(
    annee_min = min(year),
    surface_base = sum(surface_group[year == annee_min], na.rm = TRUE),
    parcels_base = sum(parcels_cult[year == annee_min], na.rm = TRUE),
    taux_croissance_surf = (surface_group - surface_base) / surface_base * 100,
    taux_croissance_parcels = (parcels_cult - parcels_base) / parcels_base * 100,
    nb_annees = n_distinct(year)
  ) %>%
  ungroup()

print(year_by_culture)

## Study of crop groups according to sub-periods
#Land parcel data is collected using two different methods: 
#- between 2007 and 2015: based on the categorisation of anonymous parcels (grouping together of crops and parcels into larger groups)
#- from 2015 onwards: categorisation of parcel crops (more detailed analysis).

#The aim is to study the 2007-2015 period on the one hand and the 2015-2023 period on the other, in order to avoid the bias in the graphical results created by the data calculation method.

year_by_culture_binary <- RPG_aggregated_all %>%
  group_by(CODE_GROUP, LIBELLE_GROUPE_CULTURE, year) %>%
  summarise(
    surface_group = sum(surf_code_group_m2, na.rm = TRUE),
    parcels_cult = sum(parcel_cult_code_group_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    binary_period = ifelse(year > 2015, 1, 0) # 1 = 2007-2015, 0 = 2015-2023
  ) %>%
  group_by(CODE_GROUP, LIBELLE_GROUPE_CULTURE, binary_period) %>%
  mutate(
    # Calcul de l'année de base pour chaque période
    min_year = min(year), 
    surface_base = sum(surface_group[year == min_year], na.rm = TRUE),
    parcels_base = sum(parcels_cult[year == min_year], na.rm = TRUE),
    taux_croissance_surf_binary = (surface_group - surface_base) / surface_base * 100,
    taux_croissance_parcels_binary = (parcels_cult - parcels_base) / parcels_base * 100
  ) %>%
  ungroup()

print(year_by_culture_binary)

## Graph showing cultures over time (age)
ggplot(year_by_culture, aes(x = nb_annees, fill = LIBELLE_GROUPE_CULTURE)) +
  geom_bar()

## Related graphs based on growth rates (area cultivated and number of plots cultivated)
### Area growth rates
year_by_culture %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(annee_min, year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(x = year, y = taux_croissance_surf, color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Taux de croissance des surfaces cultivées") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("croissance_surf_group_France.png"))
)

### Growth rates by plot of land cultivated 
year_by_culture %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(annee_min, year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(x = year, y = taux_croissance_parcels, color = LIBELLE_GROUPE_CULTURE, group = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Taux de croissance des parcelles cultivées") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

ggsave(
  filename = here(dir$output, paste0("croissance_parcelles_group_France.png"))
)

### Growth rate binary by area cultivated (two facets : 2007-2015, 2016-2023)
year_by_culture_binary %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(binary_period, year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(
    x = year,
    y = as.numeric(taux_croissance_surf_binary),
    color = LIBELLE_GROUPE_CULTURE,
    group = LIBELLE_GROUPE_CULTURE
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) +
  facet_wrap(~ binary_period, ncol = 1, scales = "free_x") +
  labs(
    title = "Évolution des groupes de culture dans le temps",
    y = "Taux de croissance des surfaces cultivées",
    x = "Année"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm")
  )

ggsave(
  filename = here(dir$output, paste0("growth_rate_binary_surf_France.png"))
)
### Growth rate binary by plot of land cultivated (two facets : 2007-2015 VS 2016-2023)
year_by_culture_binary %>%
  filter(!is.na(LIBELLE_GROUPE_CULTURE)) %>%
  arrange(year) %>%
  group_by(binary_period, year, LIBELLE_GROUPE_CULTURE) %>%
  ggplot(aes(
    x = year,
    y = taux_croissance_parcels_binary,
    color = LIBELLE_GROUPE_CULTURE,
    group = LIBELLE_GROUPE_CULTURE
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) + 
  facet_wrap(~ binary_period, ncol = 1, scales = "free_x") +
  labs(
    title = "Évolution des groupes de culture dans le temps",
    y = "Taux de croissance des surfaces cultivées",
    x = "Année"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm")
  )

ggsave(
  filename = here(dir$output, paste0("growth_rate_binary_parcels_France.png"))
)

## Study of the top 10 and bottom 10 crop groups
### Top 10 by area cultivated 
# The 10 first
# Calculation of the mean growth rate on the binary period 
top_10_cultures <- year_by_culture_binary %>%
  group_by(LIBELLE_GROUPE_CULTURE) %>%
  summarise(
    mean_taux_croissance = mean(taux_croissance_surf_binary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_taux_croissance)) %>%
  slice(1 : 10) # Selection of 10

# Filter to keep only selected crops
filtered_data <- year_by_culture_binary %>%
  filter(LIBELLE_GROUPE_CULTURE %in% top_10_cultures$LIBELLE_GROUPE_CULTURE)

filtered_data %>%
  ggplot(aes(
    x = year,
    y = as.numeric(taux_croissance_surf_binary),
    color = LIBELLE_GROUPE_CULTURE,
    group = LIBELLE_GROUPE_CULTURE
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) + 
  facet_wrap(~ binary_period, ncol = 1, scales = "free_x") +
  labs(
    title = "Évolution des 10 cultures avec le taux de croissance moyen le plus élevé",
    y = "Taux de croissance des surfaces cultivées",
    x = "Année"
  ) +
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm")
  ) 

ggsave(
  filename = here(dir$output, paste0("first_ten_crops_growth_rate_surf_Fr.png"))
)

# The last 10
# Calculation of the mean growth rate on the binary period
top_10_cultures_last <- year_by_culture_binary %>%
  group_by(LIBELLE_GROUPE_CULTURE) %>%
  summarise(
    mean_taux_croissance = mean(taux_croissance_surf_binary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_taux_croissance) %>%
  slice(1 : 10) # Sélection des 10 premiers

# Filter to keep only selected crops
filtered_data <- year_by_culture_binary %>%
  filter(LIBELLE_GROUPE_CULTURE %in% top_10_cultures_last$LIBELLE_GROUPE_CULTURE)

filtered_data %>%
  ggplot(aes(
    x = year,
    y = as.numeric(taux_croissance_surf_binary),
    color = LIBELLE_GROUPE_CULTURE,
    group = LIBELLE_GROUPE_CULTURE
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) + 
  facet_wrap(~ binary_period, ncol = 1, scales = "free_x") +
  labs(
    title = "Évolution des 10 cultures avec le taux de croissance moyen le plus faible",
    y = "Taux de croissance des surfaces cultivées",
    x = "Année"
  ) +
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm")
  ) 

ggsave(
  filename = here(dir$output, paste0("last_ten_crops_growth_rate_surf_Fr.png"))
)

### Top 10 by plot of land cultivated 
# The first 10
# Calculation of the mean growth rate on the binary variable
top_10_cultures_parcels <- year_by_culture_binary %>%
  group_by(LIBELLE_GROUPE_CULTURE) %>%
  summarise(
    mean_taux_croissance = mean(taux_croissance_parcels_binary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_taux_croissance)) %>%
  slice(1 : 10) # selection of 10

# Filter to keep only selected crops
filtered_data <- year_by_culture_binary %>%
  filter(LIBELLE_GROUPE_CULTURE %in% top_10_cultures_parcels$LIBELLE_GROUPE_CULTURE)

filtered_data %>%
  ggplot(aes(
    x = year,
    y = taux_croissance_parcels_binary,
    color = LIBELLE_GROUPE_CULTURE,
    group = LIBELLE_GROUPE_CULTURE
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) + 
  facet_wrap(~ binary_period, ncol = 1, scales = "free_x") +
  labs(
    title = "Évolution des 10 cultures avec le taux de croissance moyen le plus élevé",
    y = "Taux de croissance du nombre de parcelles cultivées",
    x = "Année"
  ) +
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm")
  ) 

ggsave(
  filename = here(dir$output, paste0("first_ten_crops_growth_rate_parcels_Fr.png"))
)

# The last 10
# Calculation of the mean growth rate on the binary variable
top_10_cultures_parcels_last <- year_by_culture_binary %>%
  group_by(LIBELLE_GROUPE_CULTURE) %>%
  summarise(
    mean_taux_croissance = mean(taux_croissance_parcels_binary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_taux_croissance) %>%
  slice(1 : 10) # Selection of 10

# Filter to keep only selected crops
filtered_data <- year_by_culture_binary %>%
  filter(LIBELLE_GROUPE_CULTURE %in% top_10_cultures_parcels_last$LIBELLE_GROUPE_CULTURE)

filtered_data %>%
  ggplot(aes(
    x = year,
    y = taux_croissance_parcels_binary,
    color = LIBELLE_GROUPE_CULTURE,
    group = LIBELLE_GROUPE_CULTURE
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c25) + 
  facet_wrap(~ binary_period, ncol = 1, scales = "free_x") +
  labs(
    title = "Évolution des 10 cultures avec le taux de croissance moyen le plus faible",
    y = "Taux de croissance du nombre de parcelles cultivées",
    x = "Année"
  ) +
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.3, "cm")
  ) 

ggsave(
  filename = here(dir$output, paste0("last_ten_crops_growth_rate_parcels_Fr.png"))
)
#===============================================================================
# 6) Inferential statistics - Analysis of crop diversity ------
# ==============================================================================

# Average change in crop diversity (average number of crops per municipality)
RPG_aggregated_all %>%
  group_by(year, name) %>%
  summarise(N_Parcels = n_distinct(LIBELLE_GROUPE_CULTURE), .groups = "drop") %>%
  group_by(year) %>%
  summarise(moyenne_cultures = mean(N_Parcels)) %>%
  ggplot(aes(x = year, y = moyenne_cultures)) +
  geom_point(color = "black") +
  labs(title = "Évolution moyenne de la diversité culturale", y = "Nombre moyen de cultures distinctes")

ggsave(
  filename = here(dir$output, paste0("diversite_culturale_France.png"))
)

# Evolution of crop groups over time 
RPG_aggregated_all %>%
  group_by(year, LIBELLE_GROUPE_CULTURE) %>%
  summarise(nb_communes = n_distinct(name)) %>%
  ggplot(aes(x = year, y = nb_communes, color = LIBELLE_GROUPE_CULTURE)) +
  geom_point() +
  labs(title = "Evolution des groupes de culture dans le temps",
       y = "Nombre de communes")

ggsave(
  filename = here(dir$output, paste0("crop_groups_change_France.png"))
)

# Crop diversity indicator: Shannon index
## Shannon index calculation
shannon_index <- RPG_aggregated_all %>%
  group_by(year, name, LIBELLE_GROUPE_CULTURE) %>%
  summarise(surface_culture = sum(surf_code_group_m2), .groups = "drop") %>%
  group_by(year, name) %>%
  mutate(total_surface = sum(surface_culture),
         proportion = surface_culture / total_surface) %>%
  summarise(shannon = -sum(proportion * log(proportion), na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  summarise(moyenne_shannon = mean(shannon, na.rm = TRUE))

## Graph of crop diversity using the average Shannon index
ggplot(shannon_index, aes(x = year, y = moyenne_shannon)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Évolution de la diversité culturale (Indice de Shannon)",
       x = "Année",
       y = "Indice de Shannon moyen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave(
  filename = here(dir$output, paste0("indice_Shannon_France.png"))
)