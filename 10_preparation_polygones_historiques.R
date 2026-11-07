# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 10_preparation_polygones_historiques.R
#
# Objectif :
# Importer et contrôler les limites administratives utilisées
# pour les attributions spatiales restantes.
#
# Pays concernés :
# - Guinée : préfectures
# - Sierra Leone : districts pré-redécoupage de 2017
#
# IMPORTANT :
# Aucune jointure DHS -> Backer n'est encore réalisée ici.
# On valide d'abord les polygones et leurs noms.
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(sf)
library(dplyr)


# ------------------------------------------------------------
# 2. Dossiers contenant les shapefiles
# ------------------------------------------------------------

dossier_gn <-
  "data_raw/geographie/GN_2015_ADM2"

dossier_sl <-
  "data_raw/geographie/SL_2015_ADM2"


stopifnot(
  dir.exists(dossier_gn),
  dir.exists(dossier_sl)
)

# ------------------------------------------------------------
# 3. Identifier automatiquement les fichiers .shp
# ------------------------------------------------------------

shp_gn <- list.files(
  dossier_gn,
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

shp_sl <- list.files(
  dossier_sl,
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)


cat("\nFichiers SHP Guinée :\n")
print(shp_gn)

cat("\nFichiers SHP Sierra Leone :\n")
print(shp_sl)

# On veut exactement un shapefile dans chaque dossier

stopifnot(
  length(shp_gn) == 1,
  length(shp_sl) == 1
)# ------------------------------------------------------------
# 4. Importer les polygones
# ------------------------------------------------------------

gn_admin2 <- st_read(
  shp_gn,
  quiet = TRUE
)

sl_admin2 <- st_read(
  shp_sl,
  quiet = TRUE
)# ------------------------------------------------------------
# 5. Dimensions
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DIMENSIONS DES POLYGONES\n")
cat("========================================\n")

cat(
  "Guinée :",
  nrow(gn_admin2),
  "polygones -",
  ncol(gn_admin2),
  "colonnes\n"
)

cat(
  "Sierra Leone :",
  nrow(sl_admin2),
  "polygones -",
  ncol(sl_admin2),
  "colonnes\n"
)

# ------------------------------------------------------------
# 6. Noms des variables
# ------------------------------------------------------------

cat("\n========================================\n")
cat("VARIABLES - GUINEE\n")
cat("========================================\n")

print(names(gn_admin2))


cat("\n========================================\n")
cat("VARIABLES - SIERRA LEONE\n")
cat("========================================\n")

print(names(sl_admin2))
# ------------------------------------------------------------
# 7. Géométrie
# ------------------------------------------------------------

cat("\n========================================\n")
cat("TYPE DE GEOMETRIE\n")
cat("========================================\n")

cat(
  "Guinée :",
  paste(
    unique(st_geometry_type(gn_admin2)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "Sierra Leone :",
  paste(
    unique(st_geometry_type(sl_admin2)),
    collapse = ", "
  ),
  "\n"
)


# ------------------------------------------------------------
# 8. CRS
# ------------------------------------------------------------

cat("\n========================================\n")
cat("SYSTEME DE COORDONNEES\n")
cat("========================================\n")

print(st_crs(gn_admin2))
print(st_crs(sl_admin2))

# ------------------------------------------------------------
# 9. Validité des géométries
# ------------------------------------------------------------

controle_geometrie <- data.frame(
  
  pays = c(
    "Guinée",
    "Sierra Leone"
  ),
  
  polygones = c(
    nrow(gn_admin2),
    nrow(sl_admin2)
  ),
  
  geometries_invalides = c(
    sum(!st_is_valid(gn_admin2)),
    sum(!st_is_valid(sl_admin2))
  )
)


cat("\n========================================\n")
cat("VALIDITE DES GEOMETRIES\n")
cat("========================================\n")

print(controle_geometrie)

# ------------------------------------------------------------
# 10. Unités administratives
# ------------------------------------------------------------

if ("NAME_2" %in% names(gn_admin2)) {
  
  cat("\n========================================\n")
  cat("UNITES ADMIN2 - GUINEE\n")
  cat("========================================\n")
  
  gn_admin2 %>%
    st_drop_geometry() %>%
    count(
      NAME_1,
      NAME_2,
      name = "n"
    ) %>%
    arrange(
      NAME_1,
      NAME_2
    ) %>%
    tibble::as_tibble() %>%
    print(n = Inf)
}


if ("NAME_2" %in% names(sl_admin2)) {
  
  cat("\n========================================\n")
  cat("UNITES ADMIN2 - SIERRA LEONE\n")
  cat("========================================\n")
  
  sl_admin2 %>%
    st_drop_geometry() %>%
    count(
      NAME_1,
      NAME_2,
      name = "n"
    ) %>%
    arrange(
      NAME_1,
      NAME_2
    ) %>%
    tibble::as_tibble() %>%
    print(n = Inf)
}
cat(
  "\nNombre de NAME_2 Guinée :",
  n_distinct(gn_admin2$NAME_2),
  "\n"
)

cat(
  "Nombre de NAME_2 Sierra Leone :",
  n_distinct(sl_admin2$NAME_2),
  "\n"
)

# ------------------------------------------------------------
# 11. Comparer les noms des polygones avec Backer
# ------------------------------------------------------------

normaliser_nom <- function(x) {
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x <- tolower(x)
  
  x <- gsub(
    "[^a-z0-9]",
    "",
    x
  )
  
  x
}


# Importer les unités Backer
backer_unites <- readRDS(
  "data_intermediate/backer_unites_3pays.rds"
)
# ------------------------------------------------------------
# 12. Comparaison Guinée : polygones / Backer
# ------------------------------------------------------------

gn_polygones_noms <- gn_admin2 %>%
  st_drop_geometry() %>%
  distinct(NAME_2) %>%
  mutate(
    nom_normalise = normaliser_nom(NAME_2)
  )


gn_backer_noms <- backer_unites %>%
  filter(pays == "Guinée") %>%
  distinct(unite_backer) %>%
  mutate(
    nom_normalise = normaliser_nom(unite_backer)
  )


comparaison_gn <- full_join(
  gn_polygones_noms,
  gn_backer_noms,
  by = "nom_normalise"
)


cat("\n========================================\n")
cat("COMPARAISON GUINEE POLYGONES / BACKER\n")
cat("========================================\n")

print(
  tibble::as_tibble(comparaison_gn),
  n = Inf
)

# ------------------------------------------------------------
# 13. Comparaison Sierra Leone : polygones / Backer
# ------------------------------------------------------------

sl_polygones_noms <- sl_admin2 %>%
  st_drop_geometry() %>%
  distinct(NAME_2) %>%
  mutate(
    nom_normalise = normaliser_nom(NAME_2)
  )


sl_backer_noms <- backer_unites %>%
  filter(pays == "Sierra Leone") %>%
  distinct(unite_backer) %>%
  mutate(
    nom_normalise = normaliser_nom(unite_backer)
  )


comparaison_sl_polygones <- full_join(
  sl_polygones_noms,
  sl_backer_noms,
  by = "nom_normalise"
)


print(
  tibble::as_tibble(comparaison_sl_polygones),
  n = Inf
)

# ------------------------------------------------------------
# 14. Harmoniser les noms des polygones Guinée avec Backer
# ------------------------------------------------------------

gn_admin2_prepare <- gn_admin2 %>%
  
  mutate(
    
    nom_polygone_original = NAME_2,
    
    nom_corrige = case_when(
      
      # Correction de nomenclature
      NAME_2 == "Yamou" ~ "Yomou",
      
      TRUE ~ NAME_2
    ),
    
    nom_normalise =
      normaliser_nom(nom_corrige)
  ) %>%
  
  left_join(
    
    backer_unites %>%
      filter(pays == "Guinée") %>%
      distinct(unite_backer) %>%
      mutate(
        nom_normalise =
          normaliser_nom(unite_backer)
      ),
    
    by = "nom_normalise"
  )

cat("\n========================================\n")
cat("POLYGONES GUINEE APRES HARMONISATION\n")
cat("========================================\n")

gn_admin2_prepare %>%
  st_drop_geometry() %>%
  select(
    nom_polygone_original,
    nom_corrige,
    unite_backer
  ) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

gn_admin2_prepare %>%
  st_drop_geometry() %>%
  filter(is.na(unite_backer)) %>%
  select(
    nom_polygone_original,
    nom_corrige,
    unite_backer
  )

# ------------------------------------------------------------
# 15. Harmoniser les noms Sierra Leone avec Backer
# ------------------------------------------------------------

sl_admin2_prepare <- sl_admin2 %>%
  
  mutate(
    
    nom_polygone_original = NAME_2,
    
    unite_backer = case_when(
      
      NAME_2 == "Western Urban" ~
        "Freetown",
      
      NAME_2 == "Western Rural" ~
        "West Rural",
      
      TRUE ~ NAME_2
    )
  )

controle_sl_backer <- sl_admin2_prepare %>%
  
  st_drop_geometry() %>%
  
  distinct(unite_backer) %>%
  
  anti_join(
    
    backer_unites %>%
      filter(pays == "Sierra Leone") %>%
      distinct(unite_backer),
    
    by = "unite_backer"
  )


cat("\n========================================\n")
cat("NOMS SIERRA LEONE ABSENTS DE BACKER\n")
cat("========================================\n")

print(controle_sl_backer)

cat(
  "Nombre de problèmes :",
  nrow(controle_sl_backer),
  "\n"
)

# ------------------------------------------------------------
# 16. Sauvegarder les polygones préparés
# ------------------------------------------------------------

saveRDS(
  gn_admin2_prepare,
  "data_intermediate/polygones_guinee_2015_prepares.rds"
)

saveRDS(
  sl_admin2_prepare,
  "data_intermediate/polygones_sierra_leone_2015_prepares.rds"
)


cat("\n========================================\n")
cat("SCRIPT 10 TERMINE\n")
cat("========================================\n")