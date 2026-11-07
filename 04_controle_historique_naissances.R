# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 04_controle_historique_naissances.R
#
# Objectif :
# Vérifier la qualité et la cohérence des dates de naissance
# déclarées dans les historiques DHS avant de construire
# la base femme-mois.
#
# Pays :
# - Guinée, DHS 2018
# - Liberia, DHS 2019-2020
# - Sierra Leone, DHS 2019
#
# Entrée :
# data_intermediate/dhs_naissances_harmonisees.rds
#
# Sortie :
# outputs/tables/controle_historique_naissances.csv
#
# Important :
# Ce script ne corrige ni ne supprime aucune observation.
# Il sert uniquement au contrôle de qualité.
# ============================================================


# ------------------------------------------------------------
# 1. Charger les packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Importer la base harmonisée des naissances
# ------------------------------------------------------------

path_naissances <- "data_intermediate/dhs_naissances_harmonisees.rds"

stopifnot(file.exists(path_naissances))

dhs_naissances <- readRDS(path_naissances)


# ------------------------------------------------------------
# 3. Vérification générale
# ------------------------------------------------------------

cat("\n========================================\n")
cat("CONTROLE DES HISTORIQUES DE NAISSANCES\n")
cat("========================================\n")

cat(
  "Nombre total de naissances :",
  nrow(dhs_naissances),
  "\n"
)

dhs_naissances %>%
  count(pays) %>%
  print()


# ------------------------------------------------------------
# 4. Vérifier les valeurs manquantes
# ------------------------------------------------------------

controle_manquants <- dhs_naissances %>%
  group_by(pays) %>%
  summarise(
    n_naissances = n(),
    
    mois_manquant =
      sum(is.na(naissance_mois)),
    
    annee_manquante =
      sum(is.na(naissance_annee)),
    
    cmc_manquant =
      sum(is.na(naissance_cmc)),
    
    interview_cmc_manquant =
      sum(is.na(interview_cmc)),
    
    naissance_mere_cmc_manquante =
      sum(is.na(naissance_femme_cmc)),
    
    .groups = "drop"
  )

cat("\n==============================\n")
cat("VALEURS MANQUANTES\n")
cat("==============================\n")

print(controle_manquants)


# ------------------------------------------------------------
# 5. Vérifier que le mois de naissance est compris entre 1 et 12
# ------------------------------------------------------------

controle_mois <- dhs_naissances %>%
  group_by(pays) %>%
  summarise(
    mois_invalides = sum(
      !is.na(naissance_mois) &
        !(naissance_mois %in% 1:12)
    ),
    .groups = "drop"
  )

cat("\n==============================\n")
cat("MOIS DE NAISSANCE INVALIDES\n")
cat("==============================\n")

print(controle_mois)


# ------------------------------------------------------------
# 6. Vérifier la cohérence entre mois, année et CMC
#
# Formule DHS :
# CMC = (année - 1900) * 12 + mois
# ------------------------------------------------------------

controle_cmc <- dhs_naissances %>%
  mutate(
    naissance_cmc_recalculee =
      (naissance_annee - 1900) * 12 +
      naissance_mois,
    
    incoherence_cmc =
      !is.na(naissance_cmc) &
      !is.na(naissance_cmc_recalculee) &
      naissance_cmc != naissance_cmc_recalculee
  ) %>%
  group_by(pays) %>%
  summarise(
    incoherences_cmc =
      sum(incoherence_cmc),
    .groups = "drop"
  )

cat("\n==============================\n")
cat("COHERENCE MOIS / ANNEE / CMC\n")
cat("==============================\n")

print(controle_cmc)


# ------------------------------------------------------------
# 7. Vérifier qu'aucune naissance n'est datée après l'interview
# ------------------------------------------------------------

controle_apres_interview <- dhs_naissances %>%
  mutate(
    naissance_apres_interview =
      !is.na(naissance_cmc) &
      !is.na(interview_cmc) &
      naissance_cmc > interview_cmc
  ) %>%
  group_by(pays) %>%
  summarise(
    naissances_apres_interview =
      sum(naissance_apres_interview),
    .groups = "drop"
  )

cat("\n==============================\n")
cat("NAISSANCES APRES L'INTERVIEW\n")
cat("==============================\n")

print(controle_apres_interview)


# ------------------------------------------------------------
# 8. Vérifier les années de naissance observées
# ------------------------------------------------------------

controle_periode <- dhs_naissances %>%
  group_by(pays) %>%
  summarise(
    annee_min =
      min(naissance_annee, na.rm = TRUE),
    
    annee_max =
      max(naissance_annee, na.rm = TRUE),
    
    .groups = "drop"
  )

cat("\n==============================\n")
cat("PERIODE COUVERTE PAR LES HISTORIQUES\n")
cat("==============================\n")

print(controle_periode)


# ------------------------------------------------------------
# 9. Calculer l'âge approximatif de la mère à chaque naissance
#
# Cette variable sert ici uniquement au contrôle.
# Elle est exprimée en mois puis en années.
# ------------------------------------------------------------

controle_age_mere <- dhs_naissances %>%
  mutate(
    age_mere_naissance_mois =
      naissance_cmc - naissance_femme_cmc,
    
    age_mere_naissance =
      age_mere_naissance_mois / 12
  ) %>%
  group_by(pays) %>%
  summarise(
    age_min =
      min(age_mere_naissance, na.rm = TRUE),
    
    age_max =
      max(age_mere_naissance, na.rm = TRUE),
    
    naissance_avant_10_ans =
      sum(
        age_mere_naissance < 10,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

cat("\n==============================\n")
cat("AGE DE LA MERE A LA NAISSANCE\n")
cat("==============================\n")

print(controle_age_mere)


# ------------------------------------------------------------
# 10. Construire un tableau récapitulatif
# ------------------------------------------------------------

controle_historique <- controle_manquants %>%
  left_join(
    controle_mois,
    by = "pays"
  ) %>%
  left_join(
    controle_cmc,
    by = "pays"
  ) %>%
  left_join(
    controle_apres_interview,
    by = "pays"
  ) %>%
  left_join(
    controle_periode,
    by = "pays"
  ) %>%
  left_join(
    controle_age_mere,
    by = "pays"
  )


cat("\n========================================\n")
cat("TABLEAU RECAPITULATIF\n")
cat("========================================\n")

print(controle_historique)


# ------------------------------------------------------------
# 11. Sauvegarder le tableau de contrôle
# ------------------------------------------------------------

write.csv(
  controle_historique,
  "outputs/tables/controle_historique_naissances.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 12. Fin
# ------------------------------------------------------------

cat("\n========================================\n")
cat("CONTROLE TERMINE\n")
cat("========================================\n")
cat("Aucune naissance n'a été supprimée ou modifiée.\n")