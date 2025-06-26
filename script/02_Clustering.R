#===============================================================================
# Description: Script to run clustering at a communal level in a given year
# by regions of France
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(cluster, factoextra, dplyr, here, tidyr, tibble, FactoMineR, sf, ggplot2, viridis)

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
# 2) Load Data ------
#===============================================================================

RPG_R53 <- readRDS(here(dir$raw, "RPG_Aggregated_Brittany_wide.rds"))

#===============================================================================
# 3) Application of K-Medoids method ------
#===============================================================================
RPG_R53_wide <- readRDS(here(dir$rqz, "RPG_Aggregated_Brittany_wide.rds"))
columns_to_select <- paste0("parcel_cult_code_group_perc_G", 1:25)
RPG_R53_clean <- na.omit(RPG_R53_wide)

commune_scaled <- RPG_R53_clean %>%
  select(all_of(columns_to_select)) %>%
  scale()

# Détermination du nombre de cluster optimal
sil_data <- fviz_nbclust(commune_scaled, clara, method = "silhouette", correct.d=TRUE)
optimal_k <- which.max(sil_data$data$y)

# Clustering final
set.seed(123)
clustering <- clara(commune_scaled, k = optimal_k, samples = 50, pamLike = TRUE)

# Graphique
fviz_cluster(clustering, 
             data = commune_scaled,
             ellipse.type = "t", 
             repel = TRUE,
             geom = "point", pointsize = 1,
             ggtheme = theme_minimal())



# Analyse des clusters 

RPG_R53_clean <- RPG_R53_clean %>%
  mutate(cluster = list(clustering$clustering))

cluster_summary <- RPG_R53_clean %>%
  group_by(cluster) %>%
  summarise(across(starts_with("parcel_cult_code_group_perc"), mean, na.rm = TRUE))

cluster_summary <- cluster_summary %>%
  rowwise() %>%
  mutate(dominant_production = names(.)[which.max(c_across(starts_with("parcel_cult_code_group_perc")))])

# Création d'un vecteur de labels pour les clusters
cluster_labels <- cluster_summary %>%
  select(cluster, dominant_production) %>%
  deframe() # Transforme en vecteur existant qui a déjà un nom : cluster -> dominant_production

# Graphique avec les labels de cluster
colors <- c("red", "blue", "green")

fviz_cluster(clustering, 
             data = commune_scaled,
             ellipse.type = "t", 
             geom = "point", pointsize = 1,
             ggtheme = theme_minimal()) +
  scale_color_manual(values = colors,
                     labels = cluster_labels) +
  labs(color = "Production dominante")

## Cluster by regions

# Load dataframe for France
RPG_Aggregated_ALL_wide <- readRDS("data/raw/RPG_Aggregated_ALL_wide.rds")

columns_to_select <- paste0("parcel_cult_code_group_perc_G", 1:25)
RegionCode_vector <- unique(RPG_Aggregated_ALL_wide$region_code)

TopCultures_by_Region <- lapply(RegionCode_vector, function(region_code) {
  # Filter data for region of interest
  RPG_region <- RPG_Aggregated_ALL_wide %>%
    filter(region_code == !!region_code, year == 2023) %>%
    select(insee, all_of(columns_to_select)) %>%
    na.omit()
  
  # Delete columns with constant variance
  constant_cols <- which(apply(RPG_region[, columns_to_select], 2, function(x) var(x, na.rm = TRUE) == 0))
  variable_columns <- setdiff(columns_to_select, names(constant_cols))
  
  # Standardisation
  commune_scaled <- RPG_region %>%
    select(all_of(variable_columns)) %>%
    scale()
  
  # Optimal number of clusters
  sil_data <- fviz_nbclust(commune_scaled, clara, method = "silhouette", correct.d = TRUE)
  optimal_k <- which.max(sil_data$data$y)
  
  # Clustering CLARA
  set.seed(123)
  clustering <- clara(commune_scaled, k = optimal_k, samples = 50, pamLike = TRUE, correct.d = TRUE)
  
  # Add clusters to the dataframe
  RPG_region_clustered <- RPG_region %>%
    mutate(cluster = clustering$clustering)
  
  # Computing mean values across clusters
  cluster_profiles <- RPG_region_clustered %>%
    select(cluster, all_of(columns_to_select)) %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")
  
  cluster_profiles_long <- cluster_profiles %>%
    pivot_longer(-cluster, names_to = "groupe_culture", values_to = "moyenne")
  
  # Computing top 3 dominating cultures in each cluster
  top_cultures <- cluster_profiles_long %>%
    group_by(cluster) %>%
    arrange(desc(moyenne), .by_group = TRUE) %>%
    slice_head(n = 3) %>%
    mutate(region = region_code)
  
  # Join with shapefile
  communes_sf <- st_read("data/shapefiles/communes-20220101.shp", quiet = TRUE) %>%
    mutate(insee = as.character(insee))
  
  map_data <- communes_sf %>%
    left_join(RPG_region_clustered, by = "insee") %>%
    filter(!is.na(cluster))
  
  # Generate map of clusters
  p <- ggplot(map_data) +
    geom_sf(aes(fill = as.factor(cluster)), color = "white", size = 0.1) +
    scale_fill_viridis_d(name = "Cluster") +
    theme_minimal() +
    labs(title = paste0("Clustering des communes – Région ", region_code, " (2023)"),
         caption = "Source : RPG 2023 – Traitement personnel")
  
  ggsave(
    filename = here(dir$output, paste0("cluster_region_", region_code, ".pdf")),
    plot = p,
    width = 12, height = 8
  )
  
  # Return list of top cultures
  return(top_cultures)
})


