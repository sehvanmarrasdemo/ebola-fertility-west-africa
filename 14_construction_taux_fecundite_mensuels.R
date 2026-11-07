# ============================================================
# 14_construction_taux_fecundite_mensuels.R
# Construction des taux mensuels de fécondité
# unité Backer × groupe d'âge × mois
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Importation de la base femme-mois avec exposition Ebola
# ------------------------------------------------------------

femme_mois_ebola <- readRDS(
  "data_intermediate/base_femme_mois_15_39_ebola.rds"
)

# ------------------------------------------------------------
# 2. Contrôles des poids DHS
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
# 3. Construction des cellules unité × âge × mois
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
    
    # Nombre de femmes-mois non pondéré
    femmes_mois_n = n(),
    
    # Femmes-mois pondérées DHS
    femmes_mois_ponderees = sum(poids),
    
    # Nombre de naissances observées non pondéré
    naissances_n = sum(nb_naissances),
    
    # Nombre de naissances pondéré
    naissances_ponderees = sum(nb_naissances * poids),
    
    # Exposition Ebola de l'unité pendant ce mois
    cas_mois_exposition = first(cas_mois_exposition),
    
    cas_mois_pour_10000_exposition =
      first(cas_mois_pour_10000_exposition),
    
    statut_exposition = first(statut_exposition),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    # Taux mensuel de fécondité réalisée
    # pour 1 000 femmes-mois
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
# 4. Contrôle de la taille des cellules
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
# 5. Vérification de la constance de l'exposition dans les cellules
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
# 6. Sauvegarde des taux mensuels de fécondité
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
