#===============================================================================
# Description: Mise en oeuvre de la méthode de clusterisation "k-means" - Typologie des spécialisations
# author:Aziliz 
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(cluster, factoextra, dplyr, here, tidyr, tibble, FactoMineR)

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




