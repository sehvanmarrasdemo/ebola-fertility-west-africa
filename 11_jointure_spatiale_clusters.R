# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 11_jointure_spatiale_clusters.R
#
# Objective:
# Complete remaining spatial assignments:
#
# - Guinea:
#   401 DHS clusters -> 2015 administrative polygons
#
# - Sierra Leone:
#   29 modern Karene clusters -> historical districts
#   administrative areas consistent with the Ebola period
#
# Important:
# No nearest-neighbour assignment is performed
# automatically.
#
# Inputs:
# - partial DHS–Backer crosswalk
# - prepared Guinea polygons
# - prepared Sierra Leone polygons
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(sf)
library(dplyr)


# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

path_crosswalk <-
  "data_intermediate/crosswalk_dhs_backer_partiel.rds"

path_gn_polygones <-
  "data_intermediate/polygones_guinee_2015_prepares.rds"

path_sl_polygones <-
  "data_intermediate/polygones_sierra_leone_2015_prepares.rds"


stopifnot(
  file.exists(path_crosswalk),
  file.exists(path_gn_polygones),
  file.exists(path_sl_polygones)
)

# ------------------------------------------------------------
# 3. Import data
# ------------------------------------------------------------

crosswalk_clusters <- readRDS(
  path_crosswalk
)

gn_admin2 <- readRDS(
  path_gn_polygones
)

sl_admin2 <- readRDS(
  path_sl_polygones
)
cat("\n========================================\n")
cat("CRS DES POLYGONES\n")
cat("========================================\n")

cat(
  "Guinée EPSG :",
  st_crs(gn_admin2)$epsg,
  "\n"
)

cat(
  "Sierra Leone EPSG :",
  st_crs(sl_admin2)$epsg,
  "\n"
)
# ------------------------------------------------------------
# 5. Prepare Guinea GPS clusters
# ------------------------------------------------------------

gn_clusters <- crosswalk_clusters %>%
  
  filter(
    pays == "Guinée"
  )


cat(
  "\nClusters Guinée :",
  nrow(gn_clusters),
  "\n"
)

cat(
  "Coordonnées valides :",
  sum(gn_clusters$coordonnees_valides),
  "\n"
)
# ------------------------------------------------------------
# 6. Convert Guinea clusters to sf points
# ------------------------------------------------------------

gn_points <- st_as_sf(
  
  gn_clusters,
  
  coords = c(
    "longitude",
    "latitude"
  ),
  
  crs = 4326,
  
  remove = FALSE
)
# ------------------------------------------------------------
# 7. Spatial join for Guinea
# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. Spatial join for Guinea
# ------------------------------------------------------------

gn_jointure <- st_join(
  
  gn_points,
  
  gn_admin2 %>%
    select(
      nom_polygone_original,
      nom_corrige,
      unite_backer_spatial = unite_backer
    ),
  
  join = st_within,
  
  left = TRUE
)

# ------------------------------------------------------------
# 8. Check duplicates after Guinea join
# ------------------------------------------------------------

cat(
  "\nLignes avant jointure :",
  nrow(gn_points),
  "\n"
)

cat(
  "Lignes après jointure :",
  nrow(gn_jointure),
  "\n"
)


doublons_gn <- gn_jointure %>%
  
  st_drop_geometry() %>%
  
  count(
    cluster
  ) %>%
  
  filter(
    n > 1
  )


cat(
  "Clusters dupliqués après jointure :",
  nrow(doublons_gn),
  "\n"
)


# ------------------------------------------------------------
# 9. Summary of Guinea spatial assignments
# ------------------------------------------------------------

controle_gn_spatial <- gn_jointure %>%
  
  st_drop_geometry() %>%
  
  summarise(
    
    clusters_total =
      n(),
    
    clusters_dans_polygone =
      sum(!is.na(nom_polygone_original)),
    
    clusters_hors_polygone =
      sum(is.na(nom_polygone_original)),
    
    clusters_avec_unite_backer =
      sum(!is.na(unite_backer_spatial)),
    
    clusters_mandiana =
      sum(
        nom_polygone_original == "Mandiana",
        na.rm = TRUE
      )
  )


cat("\n========================================\n")
cat("CONTROLE SPATIAL - GUINEE\n")
cat("========================================\n")

print(controle_gn_spatial)
clusters_par_prefecture_gn <- gn_jointure %>%
  
  st_drop_geometry() %>%
  
  count(
    nom_polygone_original,
    unite_backer_spatial,
    name = "n_clusters"
  ) %>%
  
  arrange(
    nom_polygone_original
  )


print(
  tibble::as_tibble(clusters_par_prefecture_gn),
  n = Inf
)
# ------------------------------------------------------------
# 10. Number of clusters per prefecture
# ------------------------------------------------------------

clusters_par_prefecture_gn <- gn_jointure %>%
  
  st_drop_geometry() %>%
  
  count(
    nom_polygone_original,
    unite_backer_spatial,
    name = "n_clusters"
  ) %>%
  
  arrange(
    nom_polygone_original
  )


print(
  tibble::as_tibble(clusters_par_prefecture_gn),
  n = Inf
)

# ------------------------------------------------------------
# 11. Extract Karene clusters
# ------------------------------------------------------------

karene_clusters <- crosswalk_clusters %>%
  
  filter(
    pays == "Sierra Leone",
    dhs_region == "Karene"
  )


cat(
  "\nClusters Karene :",
  nrow(karene_clusters),
  "\n"
)

cat(
  "Coordonnées valides :",
  sum(karene_clusters$coordonnees_valides),
  "\n"
)

# ------------------------------------------------------------
# 12. Convert Karene to sf points
# ------------------------------------------------------------

karene_points <- st_as_sf(
  
  karene_clusters,
  
  coords = c(
    "longitude",
    "latitude"
  ),
  
  crs = 4326,
  
  remove = FALSE
)

cat(
  "\nNombre de points Karene :",
  nrow(karene_points),
  "\n"
)

cat(
  "Type de géométrie :",
  paste(
    unique(st_geometry_type(karene_points)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "EPSG :",
  st_crs(karene_points)$epsg,
  "\n"
)

# ------------------------------------------------------------
# 13. Spatial join of Karene to historical districts
# ------------------------------------------------------------

karene_jointure <- st_join(
  
  karene_points,
  
  sl_admin2 %>%
    select(
      nom_polygone_original,
      unite_backer_spatial = unite_backer
    ),
  
  join = st_within,
  
  left = TRUE
)
cat(
  "\nLignes avant jointure :",
  nrow(karene_points),
  "\n"
)

cat(
  "Lignes après jointure :",
  nrow(karene_jointure),
  "\n"
)
# ------------------------------------------------------------
# 14. Check duplicate Karene assignments
# ------------------------------------------------------------

doublons_karene <- karene_jointure %>%
  
  st_drop_geometry() %>%
  
  count(
    cluster
  ) %>%
  
  filter(
    n > 1
  )


cat(
  "\nClusters Karene dupliqués :",
  nrow(doublons_karene),
  "\n"
)
# ------------------------------------------------------------
# 15. Spatial summary for Karene
# ------------------------------------------------------------

controle_karene_spatial <- karene_jointure %>%
  
  st_drop_geometry() %>%
  
  summarise(
    
    clusters_total =
      n(),
    
    clusters_attribues =
      sum(!is.na(unite_backer_spatial)),
    
    clusters_hors_polygone =
      sum(is.na(nom_polygone_original))
  )


cat("\n========================================\n")
cat("CONTROLE SPATIAL - KARENE\n")
cat("========================================\n")

print(controle_karene_spatial)

# ------------------------------------------------------------
# 16. Historical distribution of Karene clusters
# ------------------------------------------------------------

repartition_karene <- karene_jointure %>%
  
  st_drop_geometry() %>%
  
  count(
    nom_polygone_original,
    unite_backer_spatial,
    name = "n_clusters"
  ) %>%
  
  arrange(
    desc(n_clusters)
  )


cat("\n========================================\n")
cat("REPARTITION HISTORIQUE DE KARENE\n")
cat("========================================\n")

print(repartition_karene)
# ------------------------------------------------------------
# 17. Prepare final spatial assignments
# ------------------------------------------------------------

# Guinea
attribution_gn <- gn_jointure %>%
  
  st_drop_geometry() %>%
  
  transmute(
    pays,
    cluster,
    unite_backer_spatial,
    prefecture_spatiale = nom_polygone_original
  )


# Karene
attribution_karene <- karene_jointure %>%
  
  st_drop_geometry() %>%
  
  transmute(
    pays,
    cluster,
    unite_backer_spatial
  )
# ------------------------------------------------------------
# 18. Integrate Guinea's spatial assignments
# ------------------------------------------------------------

crosswalk_final <- crosswalk_clusters %>%
  
  left_join(
    attribution_gn,
    by = c(
      "pays",
      "cluster"
    )
  ) %>%
  
  mutate(
    
    unite_backer = case_when(
      
      pays == "Guinée" ~
        unite_backer_spatial,
      
      TRUE ~
        unite_backer
    ),
    
    methode_attribution = case_when(
      
      pays == "Guinée" &
        !is.na(unite_backer_spatial) ~
        "spatiale_polygone_historique",
      
      pays == "Guinée" &
        is.na(unite_backer_spatial) &
        prefecture_spatiale == "Mandiana" ~
        "mandiana_sans_donnees_backer",
      
      TRUE ~
        methode_attribution
    )
  )
# ------------------------------------------------------------
# 19. Integrate historical assignments for Karene
# ------------------------------------------------------------

crosswalk_final <- crosswalk_final %>%
  
  left_join(
    
    attribution_karene %>%
      rename(
        unite_backer_karene =
          unite_backer_spatial
      ),
    
    by = c(
      "pays",
      "cluster"
    )
  ) %>%
  
  mutate(
    
    unite_backer = case_when(
      
      pays == "Sierra Leone" &
        dhs_region == "Karene" ~
        unite_backer_karene,
      
      TRUE ~
        unite_backer
    ),
    
    methode_attribution = case_when(
      
      pays == "Sierra Leone" &
        dhs_region == "Karene" ~
        "spatiale_geographie_historique",
      
      TRUE ~
        methode_attribution
    ),
    
    attribution_resolue =
      !is.na(unite_backer)
  )
# ------------------------------------------------------------
# 20. Check the final crosswalk
# ------------------------------------------------------------

controle_crosswalk_final <- crosswalk_final %>%
  
  group_by(pays) %>%
  
  summarise(
    
    clusters_total =
      n(),
    
    clusters_attribues =
      sum(attribution_resolue),
    
    clusters_non_attribues =
      sum(!attribution_resolue),
    
    .groups = "drop"
  )


cat("\n========================================\n")
cat("CROSSWALK FINAL DHS / BACKER\n")
cat("========================================\n")

print(controle_crosswalk_final)
# ------------------------------------------------------------
# 21. Identify remaining unassigned clusters
# ------------------------------------------------------------

clusters_non_attribues_final <- crosswalk_final %>%
  
  filter(
    !attribution_resolue
  ) %>%
  
  select(
    pays,
    cluster,
    dhs_region,
    prefecture_spatiale,
    methode_attribution
  )


print(clusters_non_attribues_final)

# ------------------------------------------------------------
# 22. Check that all assigned areas exist in Backer
# ------------------------------------------------------------

controle_noms_final <- crosswalk_final %>%
  
  filter(
    !is.na(unite_backer)
  ) %>%
  
  distinct(
    pays,
    unite_backer
  ) %>%
  
  anti_join(
    
    backer_unites %>%
      distinct(
        pays,
        unite_backer
      ),
    
    by = c(
      "pays",
      "unite_backer"
    )
  )


cat(
  "\nNombre d'unités attribuées absentes de Backer :",
  nrow(controle_noms_final),
  "\n"
)

# ------------------------------------------------------------
# 23. Assess Mandiana's impact on woman-month data
# ------------------------------------------------------------

base_femme_mois <- readRDS(
  "data_intermediate/base_femme_mois_15_39.rds"
)


clusters_mandiana <- crosswalk_final %>%
  
  filter(
    pays == "Guinée",
    prefecture_spatiale == "Mandiana"
  ) %>%
  
  distinct(
    pays,
    cluster
  )


impact_mandiana <- base_femme_mois %>%
  
  semi_join(
    clusters_mandiana,
    by = c(
      "pays",
      "cluster"
    )
  ) %>%
  
  summarise(
    
    clusters =
      n_distinct(cluster),
    
    femmes =
      n_distinct(id_femme),
    
    femmes_mois =
      n(),
    
    naissances =
      sum(nb_naissances)
  )


cat("\n========================================\n")
cat("IMPACT DE MANDIANA\n")
cat("========================================\n")

print(impact_mandiana)
totaux_guinee <- base_femme_mois %>%
  
  filter(
    pays == "Guinée"
  ) %>%
  
  summarise(
    
    femmes_total =
      n_distinct(id_femme),
    
    femmes_mois_total =
      n(),
    
    naissances_total =
      sum(nb_naissances)
  )


impact_mandiana <- impact_mandiana %>%
  
  mutate(
    
    pct_femmes =
      100 * femmes /
      totaux_guinee$femmes_total,
    
    pct_femmes_mois =
      100 * femmes_mois /
      totaux_guinee$femmes_mois_total,
    
    pct_naissances =
      100 * naissances /
      totaux_guinee$naissances_total
  )


print(impact_mandiana)
# ------------------------------------------------------------
# 24. Clean and save the final crosswalk
# ------------------------------------------------------------

crosswalk_final <- crosswalk_final %>%
  
  select(
    pays,
    code_pays,
    cluster,
    latitude,
    longitude,
    coordonnees_valides,
    
    adm1_name,
    dhs_region,
    
    prefecture_spatiale,
    
    unite_backer,
    
    methode_attribution,
    attribution_resolue
  )
doublons_final <- crosswalk_final %>%
  
  count(
    pays,
    cluster
  ) %>%
  
  filter(
    n > 1
  )


cat(
  "\nDoublons pays × cluster dans le crosswalk final :",
  nrow(doublons_final),
  "\n"
)
saveRDS(
  crosswalk_final,
  "data_intermediate/crosswalk_dhs_backer_final.rds"
)


write.csv(
  crosswalk_final,
  "outputs/tables/crosswalk_dhs_backer_final.csv",
  row.names = FALSE
)
write.csv(
  impact_mandiana,
  "outputs/tables/impact_mandiana.csv",
  row.names = FALSE
)
# ------------------------------------------------------------
# 25. Final check
# ------------------------------------------------------------

cat("\n========================================\n")
cat("SCRIPT 11 TERMINE\n")
cat("========================================\n")

cat(
  "Nombre total de clusters :",
  nrow(crosswalk_final),
  "\n"
)

cat(
  "Clusters avec unité Backer :",
  sum(!is.na(crosswalk_final$unite_backer)),
  "\n"
)

cat(
  "Clusters sans unité Backer :",
  sum(is.na(crosswalk_final$unite_backer)),
  "\n"
)

cat(
  "Fichier final sauvegardé :",
  file.exists(
    "data_intermediate/crosswalk_dhs_backer_final.rds"
  ),
  "\n"
)
