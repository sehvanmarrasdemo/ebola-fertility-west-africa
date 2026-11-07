# ============================================================
# 14_construction_taux_fecundite_mensuels.R
# Construction of monthly fertility rates
# Backer area × age group × month
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Import woman-month data with Ebola exposure
# ------------------------------------------------------------

femme_mois_ebola <- readRDS(
  "data_intermediate/base_femme_mois_15_39_ebola.rds"
)

# ------------------------------------------------------------
# 2. Checks of DHS sampling weights
# ------------------------------------------------------------

cat("Nombre de lignes :", nrow(femme_mois_ebola), "\n")

cat(
  "Poids manquants :",
  sum(is.na(femme_mois_ebola$poids)),
  "\n"
)

cat(
  "Poids <= 0 :",
  sum(femme_mois_ebola$poids <= 0, na.rm = TRUE),
  "\n"
)

cat(
  "Naissances manquantes :",
  sum(is.na(femme_mois_ebola$nb_naissances)),
  "\n"
)

cat(
  "Groupes d'âge :\n"
)

print(
  sort(unique(femme_mois_ebola$groupe_age))
)

# ------------------------------------------------------------
# 3. Construct area × age × month cells
# ------------------------------------------------------------

taux_fecundite_mensuels <- femme_mois_ebola %>%
  group_by(
    pays,
    unite_backer,
    groupe_age,
    annee,
    mois,
    annee_mois
  ) %>%
  summarise(
    
    # Unweighted number of woman-months
    femmes_mois_n = n(),
    
    # DHS-weighted woman-months
    femmes_mois_ponderees = sum(poids),
    
    # Unweighted number of observed births
    naissances_n = sum(nb_naissances),
    
    # Weighted number of births
    naissances_ponderees = sum(nb_naissances * poids),
    
    # Ebola exposure in the area during this month
    cas_mois_exposition = first(cas_mois_exposition),
    
    cas_mois_pour_10000_exposition =
      first(cas_mois_pour_10000_exposition),
    
    statut_exposition = first(statut_exposition),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    # Monthly realised fertility rate
    # per 1,000 woman-months
    taux_fecundite_1000 =
      (naissances_ponderees / femmes_mois_ponderees) * 1000
  )
cat(
  "Nombre de cellules créées :",
  nrow(taux_fecundite_mensuels),
  "\n"
)

cat(
  "Taux manquants :",
  sum(is.na(taux_fecundite_mensuels$taux_fecundite_1000)),
  "\n"
)

cat(
  "Femmes-mois pondérées <= 0 :",
  sum(
    taux_fecundite_mensuels$femmes_mois_ponderees <= 0,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Naissances totales avant agrégation :",
  sum(femme_mois_ebola$nb_naissances),
  "\n"
)

cat(
  "Naissances totales après agrégation :",
  sum(taux_fecundite_mensuels$naissances_n),
  "\n"
)
# ------------------------------------------------------------
# 4. Check cell sizes
# ------------------------------------------------------------

summary(taux_fecundite_mensuels$femmes_mois_n)

quantile(
  taux_fecundite_mensuels$femmes_mois_n,
  probs = c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1)
)

cat(
  "Cellules avec moins de 5 femmes-mois :",
  sum(taux_fecundite_mensuels$femmes_mois_n < 5),
  "\n"
)

cat(
  "Cellules avec moins de 10 femmes-mois :",
  sum(taux_fecundite_mensuels$femmes_mois_n < 10),
  "\n"
)

cat(
  "Cellules avec moins de 20 femmes-mois :",
  sum(taux_fecundite_mensuels$femmes_mois_n < 20),
  "\n"
)
cat(
  "Cellules avec 0 naissance :",
  sum(taux_fecundite_mensuels$naissances_n == 0),
  "\n"
)

cat(
  "Part des cellules avec 0 naissance (%) :",
  round(
    mean(taux_fecundite_mensuels$naissances_n == 0) * 100,
    2
  ),
  "\n"
)
# ------------------------------------------------------------
# 5. Check exposure consistency within cells
# ------------------------------------------------------------

controle_exposition_cellules <- femme_mois_ebola %>%
  group_by(
    pays,
    unite_backer,
    groupe_age,
    annee,
    mois,
    annee_mois
  ) %>%
  summarise(
    n_valeurs_cas = n_distinct(
      cas_mois_exposition,
      na.rm = TRUE
    ),
    
    n_valeurs_intensite = n_distinct(
      cas_mois_pour_10000_exposition,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

cat(
  "Cellules avec plusieurs valeurs de cas Ebola :",
  sum(controle_exposition_cellules$n_valeurs_cas > 1),
  "\n"
)

cat(
  "Cellules avec plusieurs valeurs d'intensité Ebola :",
  sum(controle_exposition_cellules$n_valeurs_intensite > 1),
  "\n"
)

# ------------------------------------------------------------
# 6. Save monthly fertility rates
# ------------------------------------------------------------

saveRDS(
  taux_fecundite_mensuels,
  "data_intermediate/taux_fecundite_mensuels_unite_age.rds"
)

write.csv(
  taux_fecundite_mensuels,
  "data_clean/taux_fecundite_mensuels_unite_age.csv",
  row.names = FALSE
)

cat("Script 14 terminé.\n")
file.exists(
  "data_intermediate/taux_fecundite_mensuels_unite_age.rds",
  "data_clean/taux_fecundite_mensuels_unite_age.csv"
)
