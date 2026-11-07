# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 05_preparation_evenements_naissance.R
#
# Objectif :
# Préparer les événements de naissance utilisés dans l'analyse.
#
# Pour chaque naissance :
# - calculer le mois approximatif de conception (-9 mois)
# - calculer l'âge de la mère au mois de conception
# - appliquer la fenêtre temporelle propre à chaque pays
# - retenir les conceptions lorsque la femme a 15-39 ans
#
# Entrées :
# data_intermediate/dhs_naissances_harmonisees.rds
# data_intermediate/fenetres_analyse.rds
#
# Sortie :
# data_intermediate/dhs_evenements_naissance_analyse.rds
#
# Important :
# Les naissances multiples sont conservées.
# Une naissance gémellaire correspond donc à plusieurs
# naissances observées dans un même femme-mois.
# ============================================================


# ------------------------------------------------------------
# 1. Charger le package
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Importer les données intermédiaires
# ------------------------------------------------------------

dhs_naissances <- readRDS(
  "data_intermediate/dhs_naissances_harmonisees.rds"
)

fenetres_analyse <- readRDS(
  "data_intermediate/fenetres_analyse.rds"
)


# ------------------------------------------------------------
# 3. Vérifier les données importées
# ------------------------------------------------------------

cat("\nNombre total de naissances importées :",
    nrow(dhs_naissances), "\n")

print(fenetres_analyse)


# ------------------------------------------------------------
# 4. Ajouter les bornes temporelles propres à chaque pays
# ------------------------------------------------------------

naissances_temp <- dhs_naissances %>%
  left_join(
    fenetres_analyse %>%
      select(
        pays,
        debut_analyse_cmc,
        fin_analyse_cmc
      ),
    by = "pays"
  )


# Vérifier que toutes les observations ont reçu une fenêtre
sum(is.na(naissances_temp$debut_analyse_cmc))
sum(is.na(naissances_temp$fin_analyse_cmc))


# ------------------------------------------------------------
# 5. Construire le mois approximatif de conception
#
# Hypothèse principale :
# conception ≈ naissance - 9 mois
#
# Des analyses de robustesse à -8 et -10 mois seront
# réalisées plus tard.
# ------------------------------------------------------------

naissances_temp <- naissances_temp %>%
  mutate(
    conception_cmc = naissance_cmc - 9
  )


# ------------------------------------------------------------
# 6. Calculer l'âge de la mère au mois de conception
# ------------------------------------------------------------

naissances_temp <- naissances_temp %>%
  mutate(
    age_conception_mois =
      conception_cmc - naissance_femme_cmc,
    
    age_conception =
      age_conception_mois / 12
  )


# ------------------------------------------------------------
# 7. Transformer le CMC de conception en année et mois
# ------------------------------------------------------------

naissances_temp <- naissances_temp %>%
  mutate(
    
    conception_annee =
      1900 + (conception_cmc - 1) %/% 12,
    
    conception_mois =
      ((conception_cmc - 1) %% 12) + 1,
    
    conception_ym =
      sprintf(
        "%04d-%02d",
        conception_annee,
        conception_mois
      )
  )


# ------------------------------------------------------------
# 8. Identifier les observations éligibles
# ------------------------------------------------------------

naissances_temp <- naissances_temp %>%
  mutate(
    
    dans_fenetre =
      conception_cmc >= debut_analyse_cmc &
      conception_cmc <= fin_analyse_cmc,
    
    age_eligible =
      age_conception >= 15 &
      age_conception < 40
  )


# ------------------------------------------------------------
# 9. Contrôle avant filtrage
# ------------------------------------------------------------

controle_selection <- naissances_temp %>%
  group_by(pays) %>%
  summarise(
    
    naissances_br_total = n(),
    
    n_dans_fenetre =
      sum(dans_fenetre, na.rm = TRUE),
    
    n_age_15_39 =
      sum(age_eligible, na.rm = TRUE),
    
    n_dans_fenetre_et_age =
      sum(
        dans_fenetre & age_eligible,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

cat("\n========================================\n")
cat("CONTROLE DE LA SELECTION\n")
cat("========================================\n")

print(controle_selection)


# ------------------------------------------------------------
# 10. Créer la base finale des événements de naissance
# ------------------------------------------------------------

evenements_naissance <- naissances_temp %>%
  filter(
    dans_fenetre,
    age_eligible
  )


# ------------------------------------------------------------
# 11. Vérifier la période réellement obtenue
# ------------------------------------------------------------

controle_periode <- evenements_naissance %>%
  group_by(pays) %>%
  summarise(
    
    premiere_conception =
      min(conception_ym),
    
    derniere_conception =
      max(conception_ym),
    
    nombre_naissances =
      n(),
    
    .groups = "drop"
  )

cat("\n========================================\n")
cat("PERIODE DES EVENEMENTS RETENUS\n")
cat("========================================\n")

print(controle_periode)


# ------------------------------------------------------------
# 12. Vérifier les naissances multiples
#
# Plusieurs lignes pour la même femme et le même mois
# ne constituent pas nécessairement un doublon :
# il peut s'agir de jumeaux ou d'autres naissances multiples.
# ------------------------------------------------------------

controle_multiples <- evenements_naissance %>%
  count(
    pays,
    id_femme,
    conception_cmc,
    name = "nb_naissances_mois"
  ) %>%
  count(
    pays,
    nb_naissances_mois,
    name = "nombre_femme_mois"
  ) %>%
  arrange(
    pays,
    nb_naissances_mois
  )

cat("\n========================================\n")
cat("NOMBRE DE NAISSANCES PAR FEMME-MOIS\n")
cat("========================================\n")

print(controle_multiples)


# ------------------------------------------------------------
# 13. Sauvegarder la base
# ------------------------------------------------------------

saveRDS(
  evenements_naissance,
  "data_intermediate/dhs_evenements_naissance_analyse.rds"
)


# ------------------------------------------------------------
# 14. Sauvegarder le contrôle de sélection
# ------------------------------------------------------------

write.csv(
  controle_selection,
  "outputs/tables/controle_selection_evenements.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 15. Fin
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PREPARATION DES EVENEMENTS TERMINEE\n")
cat("========================================\n")

cat(
  "Nombre final d'événements de naissance :",
  nrow(evenements_naissance),
  "\n"
)