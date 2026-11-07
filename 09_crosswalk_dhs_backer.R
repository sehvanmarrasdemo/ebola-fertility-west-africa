# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 09_crosswalk_dhs_backer.R
#
# Objective:
# Construct an initial match between DHS clusters
# and administrative areas used by Backer.
#
# Assignment:
# - Liberia: direct administrative matching
# - Sierra Leone: direct matching plus historical recoding
# - Guinea: spatial assignment to be performed later
#
# Cases still unresolved spatially:
# - Guinea: prefectures
# - Sierra Leone: Karene, created after the Ebola period from
#   parts of several historical districts
#
# Inputs:
# - data_intermediate/gps_clusters_dhs_harmonises.rds
# - data_intermediate/backer_unites_3pays.rds
#
# Outputs:
# - data_intermediate/crosswalk_dhs_backer_partiel.rds
# - outputs/tables/crosswalk_dhs_backer_partiel.csv
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Import data
# ------------------------------------------------------------

path_gps <-
  "data_intermediate/gps_clusters_dhs_harmonises.rds"

path_backer <-
  "data_intermediate/backer_unites_3pays.rds"


stopifnot(
  file.exists(path_gps),
  file.exists(path_backer)
)


gps_clusters <- readRDS(path_gps)

backer_unites <- readRDS(path_backer)

# ------------------------------------------------------------
# 3. Check dimensions
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DONNEES D'ENTREE\n")
cat("========================================\n")

cat(
  "Clusters DHS :",
  nrow(gps_clusters),
  "\n"
)

cat(
  "Unités Backer :",
  nrow(backer_unites),
  "\n"
)


gps_clusters %>%
  count(pays) %>%
  print()


backer_unites %>%
  count(pays) %>%
  print()

# ------------------------------------------------------------
# 5. Add assignment method
# ------------------------------------------------------------

crosswalk_clusters <- crosswalk_clusters %>%
  
  mutate(
    
    methode_attribution = case_when(
      
      pays == "Liberia" ~
        "administrative_directe",
      
      pays == "Sierra Leone" &
        dhs_region %in% c(
          "Western Area Urban",
          "Western Area Rural",
          "Falaba"
        ) ~
        "recodage_historique",
      
      pays == "Sierra Leone" &
        dhs_region == "Karene" ~
        "spatiale_a_faire",
      
      pays == "Sierra Leone" ~
        "administrative_directe",
      
      pays == "Guinée" ~
        "spatiale_a_faire",
      
      TRUE ~
        NA_character_
    ),
    
    attribution_resolue =
      !is.na(unite_backer)
  )

# ------------------------------------------------------------
# 6. Check assignments
# ------------------------------------------------------------

controle_attribution <- crosswalk_clusters %>%
  
  group_by(pays) %>%
  
  summarise(
    
    clusters_total =
      n(),
    
    clusters_attribues =
      sum(attribution_resolue),
    
    clusters_a_resoudre =
      sum(!attribution_resolue),
    
    .groups = "drop"
  )


cat("\n========================================\n")
cat("CONTROLE DU CROSSWALK\n")
cat("========================================\n")

print(controle_attribution)

# ------------------------------------------------------------
# 7. Identify unresolved clusters
# ------------------------------------------------------------

clusters_non_resolus <- crosswalk_clusters %>%
  
  filter(
    !attribution_resolue
  ) %>%
  
  count(
    pays,
    dhs_region,
    coordonnees_valides,
    name = "n_clusters"
  ) %>%
  
  arrange(
    pays,
    dhs_region
  )


cat("\n========================================\n")
cat("CLUSTERS ENCORE NON RESOLUS\n")
cat("========================================\n")

print(clusters_non_resolus)
# ------------------------------------------------------------
# 8. Karene-specific checks
# ------------------------------------------------------------

controle_karene <- crosswalk_clusters %>%
  
  filter(
    pays == "Sierra Leone",
    dhs_region == "Karene"
  ) %>%
  
  count(
    coordonnees_valides,
    name = "n_clusters"
  )


cat("\n========================================\n")
cat("COORDONNEES DES CLUSTERS DE KARENE\n")
cat("========================================\n")

print(controle_karene)

# ------------------------------------------------------------
# 9. Check names against Backer
# ------------------------------------------------------------

attributions_invalides <- crosswalk_clusters %>%
  
  filter(
    attribution_resolue
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


cat("\n========================================\n")
cat("ATTRIBUTIONS ABSENTES DE BACKER\n")
cat("========================================\n")

print(attributions_invalides)

cat(
  "\nNombre d'attributions invalides :",
  nrow(attributions_invalides),
  "\n"
)

# ------------------------------------------------------------
# 10. Check uniqueness of country × cluster
# ------------------------------------------------------------

doublons_crosswalk <- crosswalk_clusters %>%
  
  count(
    pays,
    cluster
  ) %>%
  
  filter(
    n > 1
  )


cat(
  "\nDoublons pays × cluster :",
  nrow(doublons_crosswalk),
  "\n"
)

# ------------------------------------------------------------
# 11. Save the partial crosswalk
# ------------------------------------------------------------

saveRDS(
  crosswalk_clusters,
  "data_intermediate/crosswalk_dhs_backer_partiel.rds"
)


write.csv(
  crosswalk_clusters,
  "outputs/tables/crosswalk_dhs_backer_partiel.csv",
  row.names = FALSE
)


cat("\n========================================\n")
cat("SCRIPT 09 TERMINE\n")
cat("========================================\n")

cat(
  "Le crosswalk déterministe a été construit.\n"
)

cat(
  "Les attributions spatiales restantes seront réalisées ensuite.\n"
)