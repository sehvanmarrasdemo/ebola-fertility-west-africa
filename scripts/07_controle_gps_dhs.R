# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 07_controle_gps_dhs.R
#
# Objective:
# Import and validate DHS GPS files for the three countries.
#
# This step checks:
# - number of geographic clusters;
# - available variables;
# - coordinate reference system;
# - geometry type;
# - the match with clusters present
#   in the woman-month dataset.
#
# Countries:
# - Guinea DHS 2018
# - Liberia DHS 2019–2020
# - Sierra Leone DHS 2019
#
# Inputs:
# data_raw/dhs/.../GPS/
#
# Comparison dataset:
# data_intermediate/base_femme_mois_15_39.rds
#
# Output:
# No final geographic join at this stage.
#
# Important:
# DHS GPS coordinates are displaced to protect
# respondents' confidentiality. Spatial precision
# must therefore be considered in the interpretation.
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(sf)
library(dplyr)


# ------------------------------------------------------------
# 2. Define GPS file paths
# ------------------------------------------------------------

path_gn_gps <-
  "data_raw/dhs/GN_2018/GPS/GNGE71FL/GNGE71FL.shp"

path_lb_gps <-
  "data_raw/dhs/LIB_2019_2020/GPS/LBGE7AFL/LBGE7AFL.shp"

path_sl_gps <-
  "data_raw/dhs/SL_2019/GPS/SLGE7AFL/SLGE7AFL.shp"


# ------------------------------------------------------------
# 3. Check that files exist
# ------------------------------------------------------------

stopifnot(
  file.exists(path_gn_gps),
  file.exists(path_lb_gps),
  file.exists(path_sl_gps)
)


# ------------------------------------------------------------
# 4. Import GPS files
# ------------------------------------------------------------

gn_gps <- st_read(
  path_gn_gps,
  quiet = TRUE
)

lb_gps <- st_read(
  path_lb_gps,
  quiet = TRUE
)

sl_gps <- st_read(
  path_sl_gps,
  quiet = TRUE
)


# ------------------------------------------------------------
# 5. Check number of rows and variables
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DIMENSIONS DES FICHIERS GPS\n")
cat("========================================\n")

cat(
  "Guinée :",
  nrow(gn_gps),
  "clusters -",
  ncol(gn_gps),
  "colonnes\n"
)

cat(
  "Liberia :",
  nrow(lb_gps),
  "clusters -",
  ncol(lb_gps),
  "colonnes\n"
)

cat(
  "Sierra Leone :",
  nrow(sl_gps),
  "clusters -",
  ncol(sl_gps),
  "colonnes\n"
)


# ------------------------------------------------------------
# 6. Display available variable names
# ------------------------------------------------------------

cat("\n========================================\n")
cat("VARIABLES GPS - GUINEE\n")
cat("========================================\n")

print(names(gn_gps))


cat("\n========================================\n")
cat("VARIABLES GPS - LIBERIA\n")
cat("========================================\n")

print(names(lb_gps))


cat("\n========================================\n")
cat("VARIABLES GPS - SIERRA LEONE\n")
cat("========================================\n")

print(names(sl_gps))


# ------------------------------------------------------------
# 7. Automatically search for potentially
# useful for geographic matching
# ------------------------------------------------------------

trouver_variables_geo <- function(data) {
  
  noms <- names(data)
  
  noms[
    grepl(
      "DHS|CLUST|LAT|LON|LONG|REG|ADM|URBAN",
      noms,
      ignore.case = TRUE
    )
  ]
}


cat("\n========================================\n")
cat("VARIABLES GEO POTENTIELLES\n")
cat("========================================\n")

cat("\nGuinée :\n")
print(trouver_variables_geo(gn_gps))

cat("\nLiberia :\n")
print(trouver_variables_geo(lb_gps))

cat("\nSierra Leone :\n")
print(trouver_variables_geo(sl_gps))


# ------------------------------------------------------------
# 8. Check geometry type
# ------------------------------------------------------------

cat("\n========================================\n")
cat("TYPE DE GEOMETRIE\n")
cat("========================================\n")

cat(
  "Guinée :",
  paste(unique(st_geometry_type(gn_gps)), collapse = ", "),
  "\n"
)

cat(
  "Liberia :",
  paste(unique(st_geometry_type(lb_gps)), collapse = ", "),
  "\n"
)

cat(
  "Sierra Leone :",
  paste(unique(st_geometry_type(sl_gps)), collapse = ", "),
  "\n"
)


# ------------------------------------------------------------
# 9. Check coordinate reference system
# ------------------------------------------------------------

cat("\n========================================\n")
cat("SYSTEME DE COORDONNEES\n")
cat("========================================\n")

cat(
  "Guinée EPSG :",
  st_crs(gn_gps)$epsg,
  "\n"
)

cat(
  "Liberia EPSG :",
  st_crs(lb_gps)$epsg,
  "\n"
)

cat(
  "Sierra Leone EPSG :",
  st_crs(sl_gps)$epsg,
  "\n"
)


# ------------------------------------------------------------
# 10. Check geographic extent
# ------------------------------------------------------------

cat("\n========================================\n")
cat("BOUNDING BOXES\n")
cat("========================================\n")

cat("\nGuinée :\n")
print(st_bbox(gn_gps))

cat("\nLiberia :\n")
print(st_bbox(lb_gps))

cat("\nSierra Leone :\n")
print(st_bbox(sl_gps))


# ------------------------------------------------------------
# 11. Check whether DHSCLUST is present
# ------------------------------------------------------------

cat("\n========================================\n")
cat("VARIABLE DHSCLUST\n")
cat("========================================\n")

cat(
  "Guinée :",
  "DHSCLUST" %in% names(gn_gps),
  "\n"
)

cat(
  "Liberia :",
  "DHSCLUST" %in% names(lb_gps),
  "\n"
)

cat(
  "Sierra Leone :",
  "DHSCLUST" %in% names(sl_gps),
  "\n"
)


# ------------------------------------------------------------
# 12. If DHSCLUST exists, count unique clusters
# ------------------------------------------------------------

if ("DHSCLUST" %in% names(gn_gps)) {
  
  cat(
    "\nClusters GPS uniques - Guinée :",
    n_distinct(gn_gps$DHSCLUST),
    "\n"
  )
}

if ("DHSCLUST" %in% names(lb_gps)) {
  
  cat(
    "Clusters GPS uniques - Liberia :",
    n_distinct(lb_gps$DHSCLUST),
    "\n"
  )
}

if ("DHSCLUST" %in% names(sl_gps)) {
  
  cat(
    "Clusters GPS uniques - Sierra Leone :",
    n_distinct(sl_gps$DHSCLUST),
    "\n"
  )
}

# ------------------------------------------------------------
# 12b. Check zero or missing GPS coordinates
# ------------------------------------------------------------

coordonnees_gps <- bind_rows(
  
  gn_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Guinée",
      cluster = as.numeric(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM)
    ),
  
  lb_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Liberia",
      cluster = as.numeric(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM)
    ),
  
  sl_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Sierra Leone",
      cluster = as.numeric(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM)
    )
) %>%
  
  mutate(
    coordonnees_invalides =
      is.na(latitude) |
      is.na(longitude) |
      latitude == 0 |
      longitude == 0
  )


controle_coordonnees <- coordonnees_gps %>%
  group_by(pays) %>%
  summarise(
    clusters_total = n(),
    coordonnees_valides = sum(!coordonnees_invalides),
    coordonnees_invalides = sum(coordonnees_invalides),
    .groups = "drop"
  )


cat("\n========================================\n")
cat("CONTROLE DES COORDONNEES GPS\n")
cat("========================================\n")

print(controle_coordonnees)

# ------------------------------------------------------------
# 12c. Assess the extent of clusters without coordinates
# in the woman-month dataset
# ------------------------------------------------------------

clusters_invalides <- coordonnees_gps %>%
  filter(coordonnees_invalides) %>%
  select(
    pays,
    cluster
  )


impact_clusters_invalides <- base_femme_mois %>%
  semi_join(
    clusters_invalides,
    by = c(
      "pays",
      "cluster"
    )
  ) %>%
  group_by(pays) %>%
  summarise(
    
    clusters_concernes =
      n_distinct(cluster),
    
    femmes_concernees =
      n_distinct(id_femme),
    
    femmes_mois_concernees =
      n(),
    
    naissances_concernees =
      sum(nb_naissances),
    
    .groups = "drop"
  )


cat("\n========================================\n")
cat("IMPACT DES CLUSTERS SANS COORDONNEES\n")
cat("========================================\n")

print(impact_clusters_invalides)

# ------------------------------------------------------------
# 12d. Share of the sample affected
# ------------------------------------------------------------

totaux_pays <- base_femme_mois %>%
  group_by(pays) %>%
  summarise(
    
    femmes_total =
      n_distinct(id_femme),
    
    femmes_mois_total =
      n(),
    
    naissances_total =
      sum(nb_naissances),
    
    .groups = "drop"
  )


impact_clusters_invalides <- impact_clusters_invalides %>%
  left_join(
    totaux_pays,
    by = "pays"
  ) %>%
  mutate(
    
    pct_femmes =
      100 *
      femmes_concernees /
      femmes_total,
    
    pct_femmes_mois =
      100 *
      femmes_mois_concernees /
      femmes_mois_total,
    
    pct_naissances =
      100 *
      naissances_concernees /
      naissances_total
  )


print(impact_clusters_invalides)

# ------------------------------------------------------------
# 12e. Available administrative information
# for clusters with missing coordinates
# ------------------------------------------------------------

gps_admin <- bind_rows(
  
  gn_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Guinée",
      cluster = as.numeric(DHSCLUST),
      ADM1NAME,
      DHSREGNA,
      ADM1DHS
    ),
  
  lb_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Liberia",
      cluster = as.numeric(DHSCLUST),
      ADM1NAME,
      DHSREGNA,
      ADM1DHS
    ),
  
  sl_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Sierra Leone",
      cluster = as.numeric(DHSCLUST),
      ADM1NAME,
      DHSREGNA,
      ADM1DHS
    )
)


clusters_invalides_admin <- clusters_invalides %>%
  left_join(
    gps_admin,
    by = c("pays", "cluster")
  )


print(clusters_invalides_admin)
# ------------------------------------------------------------
# 13. Import woman-month dataset
# ------------------------------------------------------------

base_femme_mois <- readRDS(
  "data_intermediate/base_femme_mois_15_39.rds"
)


# ------------------------------------------------------------
# 14. Number of clusters in woman-month data
# ------------------------------------------------------------

clusters_femme_mois <- base_femme_mois %>%
  distinct(
    pays,
    cluster
  ) %>%
  count(
    pays,
    name = "clusters_femme_mois"
  )


cat("\n========================================\n")
cat("CLUSTERS DANS LA BASE FEMME-MOIS\n")
cat("========================================\n")

print(clusters_femme_mois)


# ------------------------------------------------------------
# 15. Compare IR and GPS if DHSCLUST exists
# ------------------------------------------------------------

if (
  "DHSCLUST" %in% names(gn_gps) &&
  "DHSCLUST" %in% names(lb_gps) &&
  "DHSCLUST" %in% names(sl_gps)
) {
  
  gps_clusters <- bind_rows(
    
    gn_gps %>%
      st_drop_geometry() %>%
      transmute(
        pays = "Guinée",
        cluster = as.numeric(DHSCLUST)
      ),
    
    lb_gps %>%
      st_drop_geometry() %>%
      transmute(
        pays = "Liberia",
        cluster = as.numeric(DHSCLUST)
      ),
    
    sl_gps %>%
      st_drop_geometry() %>%
      transmute(
        pays = "Sierra Leone",
        cluster = as.numeric(DHSCLUST)
      )
  ) %>%
    distinct(
      pays,
      cluster
    )
  
  
  clusters_calendrier <- base_femme_mois %>%
    distinct(
      pays,
      cluster
    )
  
  
  # Clusters present in the dataset but absent from GPS
  clusters_sans_gps <- clusters_calendrier %>%
    anti_join(
      gps_clusters,
      by = c(
        "pays",
        "cluster"
      )
    )
  
  
  # GPS clusters not contributing to the age-15–39 dataset
  gps_sans_calendrier <- gps_clusters %>%
    anti_join(
      clusters_calendrier,
      by = c(
        "pays",
        "cluster"
      )
    )
  
  
  cat("\n========================================\n")
  cat("CORRESPONDANCE BASE FEMME-MOIS / GPS\n")
  cat("========================================\n")
  
  cat(
    "Clusters de la base sans GPS :",
    nrow(clusters_sans_gps),
    "\n"
  )
  
  cat(
    "Clusters GPS sans femme-mois éligible :",
    nrow(gps_sans_calendrier),
    "\n"
  )
}
# ------------------------------------------------------------
# 16. Inventory DHS administrative units
# ------------------------------------------------------------

admin_gn <- gn_gps %>%
  st_drop_geometry() %>%
  count(
    ADM1NAME,
    DHSREGNA,
    name = "n_clusters"
  ) %>%
  arrange(
    ADM1NAME,
    DHSREGNA
  )


admin_lb <- lb_gps %>%
  st_drop_geometry() %>%
  count(
    ADM1NAME,
    DHSREGNA,
    name = "n_clusters"
  ) %>%
  arrange(
    ADM1NAME,
    DHSREGNA
  )


admin_sl <- sl_gps %>%
  st_drop_geometry() %>%
  count(
    ADM1NAME,
    DHSREGNA,
    name = "n_clusters"
  ) %>%
  arrange(
    ADM1NAME,
    DHSREGNA
  )

cat("\n========================================\n")
cat("UNITES ADMINISTRATIVES DHS - GUINEE\n")
cat("========================================\n")

print(admin_gn, row.names = FALSE)


cat("\n========================================\n")
cat("UNITES ADMINISTRATIVES DHS - LIBERIA\n")
cat("========================================\n")

print(admin_lb, row.names = FALSE)


cat("\n========================================\n")
cat("UNITES ADMINISTRATIVES DHS - SIERRA LEONE\n")
cat("========================================\n")

print(admin_sl, row.names = FALSE)

cat("\nNombre de valeurs ADM1NAME :\n")

cat(
  "Guinée :",
  n_distinct(gn_gps$ADM1NAME),
  "\n"
)

cat(
  "Liberia :",
  n_distinct(lb_gps$ADM1NAME),
  "\n"
)

cat(
  "Sierra Leone :",
  n_distinct(sl_gps$ADM1NAME),
  "\n"
)


cat("\nNombre de valeurs DHSREGNA :\n")

cat(
  "Guinée :",
  n_distinct(gn_gps$DHSREGNA),
  "\n"
)

cat(
  "Liberia :",
  n_distinct(lb_gps$DHSREGNA),
  "\n"
)

cat(
  "Sierra Leone :",
  n_distinct(sl_gps$DHSREGNA),
  "\n"
)
# ------------------------------------------------------------
# 16. End
# ------------------------------------------------------------

cat("\n========================================\n")
cat("CONTROLE GPS TERMINE\n")
cat("========================================\n")

cat(
  "Aucune attribution administrative n'a encore été réalisée.\n"
)
# ------------------------------------------------------------
# 17. Create a harmonised DHS GPS cluster dataset
# ------------------------------------------------------------

gps_clusters_harmonises <- bind_rows(
  
  gn_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Guinée",
      code_pays = "GN",
      cluster = as.numeric(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM),
      adm1_name = as.character(ADM1NAME),
      dhs_region = as.character(DHSREGNA)
    ),
  
  lb_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Liberia",
      code_pays = "LB",
      cluster = as.numeric(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM),
      adm1_name = as.character(ADM1NAME),
      dhs_region = as.character(DHSREGNA)
    ),
  
  sl_gps %>%
    st_drop_geometry() %>%
    transmute(
      pays = "Sierra Leone",
      code_pays = "SL",
      cluster = as.numeric(DHSCLUST),
      latitude = as.numeric(LATNUM),
      longitude = as.numeric(LONGNUM),
      adm1_name = as.character(ADM1NAME),
      dhs_region = as.character(DHSREGNA)
    )
) %>%
  
  mutate(
    coordonnees_valides =
      !is.na(latitude) &
      !is.na(longitude) &
      latitude != 0 &
      longitude != 0
  )


# Verification
gps_clusters_harmonises %>%
  count(
    pays,
    coordonnees_valides
  ) %>%
  print()


# Saving outputs
saveRDS(
  gps_clusters_harmonises,
  "data_intermediate/gps_clusters_dhs_harmonises.rds"
)
