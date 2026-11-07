# ============================================================
# 13_appariement_femme_mois_ebola.R
# Linking the DHS woman-month dataset
# with local monthly Ebola exposure
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Import required datasets
# ------------------------------------------------------------

femme_mois <- readRDS(
  "data_intermediate/base_femme_mois_15_39.rds"
)

crosswalk <- readRDS(
  "data_intermediate/crosswalk_dhs_backer_final.rds"
)

backer_mensuel <- readRDS(
  "data_intermediate/backer_mensuel_3pays.rds"
)

# ------------------------------------------------------------
# 2. Check dimensions
# ------------------------------------------------------------

cat("Dimensions femme_mois :", dim(femme_mois), "\n")
cat("Dimensions crosswalk :", dim(crosswalk), "\n")
cat("Dimensions Backer mensuel :", dim(backer_mensuel), "\n")

# ------------------------------------------------------------
# 3. Check variable names
# ------------------------------------------------------------

cat("\nVariables femme_mois :\n")
print(names(femme_mois))

cat("\nVariables crosswalk :\n")
print(names(crosswalk))

cat("\nVariables Backer mensuel :\n")
print(names(backer_mensuel))

# ------------------------------------------------------------
# 4. Check join keys
# ------------------------------------------------------------

# Crosswalk: one row per country × cluster
controle_crosswalk <- crosswalk %>%
  count(pays, cluster) %>%
  filter(n > 1)

cat(
  "Doublons pays × cluster dans crosswalk :",
  nrow(controle_crosswalk),
  "\n"
)

# Monthly Backer data: one row per country × area × month
controle_backer <- backer_mensuel %>%
  count(pays, unite_backer, annee_mois) %>%
  filter(n > 1)

cat(
  "Doublons pays × unite_backer × annee_mois dans Backer :",
  nrow(controle_backer),
  "\n"
)

# ------------------------------------------------------------
# 5. Join Backer geography to woman-month data
# ------------------------------------------------------------

femme_mois_geo <- femme_mois %>%
  left_join(
    crosswalk %>%
      select(
        pays,
        cluster,
        unite_backer,
        methode_attribution,
        attribution_resolue
      ),
    by = c("pays", "cluster")
  )

# ------------------------------------------------------------
# 6. Checks after geographic join
# ------------------------------------------------------------

cat(
  "Nombre de lignes avant jointure :",
  nrow(femme_mois),
  "\n"
)

cat(
  "Nombre de lignes après jointure :",
  nrow(femme_mois_geo),
  "\n"
)

cat(
  "Femmes-mois sans unité Backer :",
  sum(is.na(femme_mois_geo$unite_backer)),
  "\n"
)

controle_geo_pays <- femme_mois_geo %>%
  group_by(pays) %>%
  summarise(
    femmes_mois = n(),
    sans_unite_backer = sum(is.na(unite_backer)),
    .groups = "drop"
  )

print(controle_geo_pays)

# ------------------------------------------------------------
# 7. Join monthly Ebola exposure
# ------------------------------------------------------------

femme_mois_ebola <- femme_mois_geo %>%
  left_join(
    backer_mensuel %>%
      select(
        pays,
        unite_backer,
        annee_mois,
        cas_mois,
        population,
        cas_mois_pour_10000
      ),
    by = c(
      "pays",
      "unite_backer",
      "annee_mois"
    )
  )

# ------------------------------------------------------------
# 8. Checks after Ebola join
# ------------------------------------------------------------

cat(
  "Nombre de lignes avant jointure Ebola :",
  nrow(femme_mois_geo),
  "\n"
)

cat(
  "Nombre de lignes après jointure Ebola :",
  nrow(femme_mois_ebola),
  "\n"
)

cat(
  "Femmes-mois avec exposition Ebola observée :",
  sum(!is.na(femme_mois_ebola$cas_mois)),
  "\n"
)

cat(
  "Femmes-mois avec exposition Ebola manquante :",
  sum(is.na(femme_mois_ebola$cas_mois)),
  "\n"
)

controle_exposition <- femme_mois_ebola %>%
  group_by(pays) %>%
  summarise(
    femmes_mois = n(),
    exposition_observee = sum(!is.na(cas_mois)),
    exposition_manquante = sum(is.na(cas_mois)),
    .groups = "drop"
  )

print(controle_exposition)
controle_periode <- femme_mois_ebola %>%
  filter(!is.na(cas_mois)) %>%
  group_by(pays) %>%
  summarise(
    premier_mois = min(annee_mois),
    dernier_mois = max(annee_mois),
    .groups = "drop"
  )

print(controle_periode)

# ------------------------------------------------------------
# 9. Create Ebola exposure coverage status
# ------------------------------------------------------------

femme_mois_ebola <- femme_mois_ebola %>%
  mutate(
    statut_exposition = case_when(
      
      # No Backer area available: mainly Mandiana
      is.na(unite_backer) ~ "unite_non_couverte_backer",
      
      # Month outside the period covered by Backer
      annee_mois < "2013-12" | annee_mois > "2015-05" ~
        "hors_periode_backer",
      
      # Covered month and area with no recorded cases
      !is.na(cas_mois) & cas_mois == 0 ~
        "zero_cas_observe",
      
      # Covered month with at least one case
      !is.na(cas_mois) & cas_mois > 0 ~
        "cas_observes",
      
      # Safeguard: any other unexpected case
      TRUE ~ "manquant_inattendu"
    )
  )

# Validation
controle_statut <- femme_mois_ebola %>%
  count(pays, statut_exposition)

print(controle_statut)

# ------------------------------------------------------------
# 10. Distinguish periods before and after Backer coverage
# ------------------------------------------------------------

femme_mois_ebola <- femme_mois_ebola %>%
  mutate(
    statut_exposition = case_when(
      
      is.na(unite_backer) ~
        "unite_non_couverte_backer",
      
      annee_mois < "2013-12" ~
        "avant_backer",
      
      annee_mois > "2015-05" ~
        "apres_backer",
      
      !is.na(cas_mois) & cas_mois == 0 ~
        "zero_cas_observe",
      
      !is.na(cas_mois) & cas_mois > 0 ~
        "cas_observes",
      
      TRUE ~
        "manquant_inattendu"
    )
  )

controle_statut2 <- femme_mois_ebola %>%
  count(pays, statut_exposition)

print(controle_statut2)

# ------------------------------------------------------------
# 11. Construct final monthly exposure
# ------------------------------------------------------------

femme_mois_ebola <- femme_mois_ebola %>%
  mutate(
    
    cas_mois_exposition = case_when(
      statut_exposition == "avant_backer" ~ 0,
      statut_exposition == "zero_cas_observe" ~ 0,
      statut_exposition == "cas_observes" ~ cas_mois,
      TRUE ~ NA_real_
    ),
    
    cas_mois_pour_10000_exposition = case_when(
      statut_exposition == "avant_backer" ~ 0,
      statut_exposition == "zero_cas_observe" ~ 0,
      statut_exposition == "cas_observes" ~ cas_mois_pour_10000,
      TRUE ~ NA_real_
    )
  )
controle_exposition_finale <- femme_mois_ebola %>%
  group_by(pays, statut_exposition) %>%
  summarise(
    n = n(),
    exposition_finale_na =
      sum(is.na(cas_mois_pour_10000_exposition)),
    exposition_finale_zero =
      sum(cas_mois_pour_10000_exposition == 0, na.rm = TRUE),
    .groups = "drop"
  )

print(controle_exposition_finale)

# ------------------------------------------------------------
# 12. Save woman-month data with Ebola exposure
# ------------------------------------------------------------

saveRDS(
  femme_mois_ebola,
  "data_intermediate/base_femme_mois_15_39_ebola.rds"
)

write.csv(
  controle_exposition_finale,
  "outputs/tables/controle_exposition_femme_mois.csv",
  row.names = FALSE
)

cat("Script 13 terminé.\n")
