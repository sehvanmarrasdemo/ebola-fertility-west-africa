# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 09_crosswalk_dhs_backer.R
#
# Objectif :
# Construire une première correspondance entre les clusters DHS
# et les unités administratives utilisées par Backer.
#
# Attribution :
# - Liberia : correspondance administrative directe
# - Sierra Leone : correspondance directe + recodages historiques
# - Guinée : attribution spatiale à réaliser ultérieurement
#
# Cas encore non résolu spatialement :
# - Guinée : préfectures
# - Sierra Leone : Karene, créé après la période Ebola à partir
#   de portions de plusieurs anciens districts
#
# Entrées :
# - data_intermediate/gps_clusters_dhs_harmonises.rds
# - data_intermediate/backer_unites_3pays.rds
#
# Sorties :
# - data_intermediate/crosswalk_dhs_backer_partiel.rds
# - outputs/tables/crosswalk_dhs_backer_partiel.csv
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Importer les données
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
# 3. Vérifier les dimensions
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
# 5. Ajouter la méthode d'attribution
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
# 6. Contrôle des attributions
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
# 7. Identifier les clusters non résolus
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
# 8. Contrôle spécifique de Karene
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
# 9. Vérifier les noms avec Backer
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
# 10. Vérifier l'unicité pays × cluster
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
# 11. Sauvegarder le crosswalk partiel
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