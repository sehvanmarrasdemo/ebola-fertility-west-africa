# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 06_construction_base_femme_mois.R
#
# Objectif :
# Construire la base femme-mois centrale de l'analyse.
#
# Une observation = une femme pendant un mois donné.
#
# Le mois correspond à l'axe temporel des conceptions
# approximatives :
# naissance observée - 9 mois.
#
# Pour chaque femme, on conserve uniquement les mois :
# - appartenant à la fenêtre temporelle de son pays ;
# - pendant lesquels elle est âgée de 15 à 39 ans.
#
# Les événements de naissance préparés au script 05
# sont ensuite rattachés à leur mois approximatif
# de conception.
#
# Entrées :
# - data_intermediate/dhs_femmes_harmonisees.rds
# - data_intermediate/dhs_evenements_naissance_analyse.rds
# - data_intermediate/fenetres_analyse.rds
#
# Sortie :
# - data_intermediate/base_femme_mois_15_39.rds
#
# Important :
# - Les femmes sans naissance sont conservées.
# - Les naissances multiples sont conservées.
# - Les données brutes DHS ne sont jamais modifiées.
# ============================================================


# ------------------------------------------------------------
# 1. Charger les packages
# ------------------------------------------------------------

library(dplyr)
library(tidyr)


# ------------------------------------------------------------
# 2. Définir les chemins d'entrée
# ------------------------------------------------------------

path_femmes <-
  "data_intermediate/dhs_femmes_harmonisees.rds"

path_evenements <-
  "data_intermediate/dhs_evenements_naissance_analyse.rds"

path_fenetres <-
  "data_intermediate/fenetres_analyse.rds"


# ------------------------------------------------------------
# 3. Vérifier que les fichiers existent
# ------------------------------------------------------------

stopifnot(
  file.exists(path_femmes),
  file.exists(path_evenements),
  file.exists(path_fenetres)
)


# ------------------------------------------------------------
# 4. Importer les bases intermédiaires
# ------------------------------------------------------------

femmes <- readRDS(path_femmes)

evenements <- readRDS(path_evenements)

fenetres <- readRDS(path_fenetres)


# ------------------------------------------------------------
# 5. Vérifier les données importées
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DONNEES IMPORTEES\n")
cat("========================================\n")

cat(
  "Nombre total de femmes :",
  nrow(femmes),
  "\n"
)

cat(
  "Nombre total d'événements de naissance :",
  nrow(evenements),
  "\n"
)


cat("\nFemmes par pays :\n")

femmes %>%
  count(pays) %>%
  print()


cat("\nEvénements de naissance par pays :\n")

evenements %>%
  count(pays) %>%
  print()


# ------------------------------------------------------------
# 6. Ajouter à chaque femme la fenêtre temporelle de son pays
# ------------------------------------------------------------

femmes_temp <- femmes %>%
  left_join(
    fenetres %>%
      select(
        pays,
        debut_analyse_cmc,
        fin_analyse_cmc
      ),
    by = "pays"
  )


# ------------------------------------------------------------
# 7. Vérifier que toutes les femmes ont reçu une fenêtre
# ------------------------------------------------------------

cat(
  "\nFemmes sans borne de début :",
  sum(is.na(femmes_temp$debut_analyse_cmc)),
  "\n"
)

cat(
  "Femmes sans borne de fin :",
  sum(is.na(femmes_temp$fin_analyse_cmc)),
  "\n"
)


# ------------------------------------------------------------
# 8. Vérifier les dates de naissance des femmes
# ------------------------------------------------------------

cat(
  "Femmes sans CMC de naissance :",
  sum(is.na(femmes_temp$naissance_femme_cmc)),
  "\n"
)


# ------------------------------------------------------------
# 9. Déterminer les mois d'éligibilité de chaque femme
#
# 15 ans = 15 * 12 = 180 mois
#
# Une femme est retenue tant que son âge est strictement
# inférieur à 40 ans.
#
# Le dernier mois admissible correspond donc au mois 479
# après son mois de naissance.
# ------------------------------------------------------------

femmes_temp <- femmes_temp %>%
  mutate(
    
    # Premier mois approximatif à partir duquel
    # la femme a 15 ans
    debut_age_cmc =
      naissance_femme_cmc + 180,
    
    # Dernier mois avant 40 ans
    fin_age_cmc =
      naissance_femme_cmc + 479,
    
    # Premier mois réellement retenu
    debut_femme_cmc =
      pmax(
        debut_analyse_cmc,
        debut_age_cmc
      ),
    
    # Dernier mois réellement retenu
    fin_femme_cmc =
      pmin(
        fin_analyse_cmc,
        fin_age_cmc
      ),
    
    # Nombre total de mois de contribution
    n_mois =
      fin_femme_cmc -
      debut_femme_cmc +
      1
  )


# ------------------------------------------------------------
# 10. Contrôler les femmes ayant au moins un mois éligible
# ------------------------------------------------------------

controle_femmes_eligibles <- femmes_temp %>%
  group_by(pays) %>%
  summarise(
    
    femmes_ir = n(),
    
    femmes_avec_au_moins_un_mois =
      sum(
        !is.na(n_mois) &
          n_mois > 0
      ),
    
    femmes_sans_mois_eligible =
      sum(
        is.na(n_mois) |
          n_mois <= 0
      ),
    
    mois_min_par_femme =
      min(
        n_mois[n_mois > 0],
        na.rm = TRUE
      ),
    
    mois_moyens_par_femme =
      mean(
        n_mois[n_mois > 0],
        na.rm = TRUE
      ),
    
    mois_max_par_femme =
      max(
        n_mois[n_mois > 0],
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


cat("\n========================================\n")
cat("FEMMES ELIGIBLES\n")
cat("========================================\n")

print(controle_femmes_eligibles)


# ------------------------------------------------------------
# 11. Conserver uniquement les femmes contribuant
# à au moins un mois
# ------------------------------------------------------------

femmes_eligibles <- femmes_temp %>%
  filter(
    !is.na(n_mois),
    n_mois > 0
  )


# ------------------------------------------------------------
# 12. Développer chaque femme en observations femme-mois
#
# Exemple :
#
# Une femme ayant 100 mois éligibles
# devient 100 lignes.
# ------------------------------------------------------------

base_femme_mois <- femmes_eligibles %>%
  
  select(
    
    # Identification
    pays,
    code_pays,
    caseid,
    id_femme,
    
    # Sondage et géographie
    cluster,
    menage,
    numero_femme,
    psu,
    strate_v022,
    strate_v023,
    region,
    residence,
    
    # Poids DHS
    poids_brut,
    poids,
    
    # Informations temporelles sur la femme
    interview_cmc,
    naissance_femme_cmc,
    age_enquete,
    
    # Bornes individuelles
    debut_femme_cmc,
    fin_femme_cmc,
    
    # Nombre de mois à créer
    n_mois
  ) %>%
  
  uncount(
    weights = n_mois,
    .id = "index_mois"
  ) %>%
  
  mutate(
    
    mois_cmc =
      debut_femme_cmc +
      index_mois -
      1
  )


# ------------------------------------------------------------
# 13. Créer des identifiants géographiques uniques
# entre les trois pays
# ------------------------------------------------------------

base_femme_mois <- base_femme_mois %>%
  mutate(
    
    id_cluster =
      paste0(
        code_pays,
        "_",
        cluster
      ),
    
    id_psu =
      paste0(
        code_pays,
        "_",
        psu
      )
  )


# ------------------------------------------------------------
# 14. Recalculer l'âge de la femme pour chaque mois
# ------------------------------------------------------------

base_femme_mois <- base_femme_mois %>%
  mutate(
    
    age_mois =
      mois_cmc -
      naissance_femme_cmc,
    
    # Age révolu approximatif en années
    age =
      floor(
        age_mois / 12
      ),
    
    # Groupes d'âge quinquennaux
    groupe_age =
      cut(
        age,
        
        breaks = c(
          15,
          20,
          25,
          30,
          35,
          40
        ),
        
        right = FALSE,
        
        labels = c(
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39"
        )
      )
  )


# ------------------------------------------------------------
# 15. Transformer le CMC en année et mois
# ------------------------------------------------------------

base_femme_mois <- base_femme_mois %>%
  mutate(
    
    annee =
      1900 +
      (mois_cmc - 1) %/% 12,
    
    mois =
      ((mois_cmc - 1) %% 12) + 1,
    
    annee_mois =
      sprintf(
        "%04d-%02d",
        annee,
        mois
      )
  )


# ------------------------------------------------------------
# 16. Contrôle de la base AVANT ajout des naissances
# ------------------------------------------------------------

controle_base <- base_femme_mois %>%
  group_by(pays) %>%
  summarise(
    
    femmes =
      n_distinct(id_femme),
    
    femmes_mois =
      n(),
    
    premiere_periode =
      min(annee_mois),
    
    derniere_periode =
      max(annee_mois),
    
    age_min =
      min(age),
    
    age_max =
      max(age),
    
    .groups = "drop"
  )


cat("\n========================================\n")
cat("CONTROLE BASE FEMME-MOIS\n")
cat("========================================\n")

print(controle_base)


# ------------------------------------------------------------
# 17. Vérifier explicitement la restriction 15-39 ans
# ------------------------------------------------------------

controle_age_hors_champ <- base_femme_mois %>%
  filter(
    age < 15 |
      age >= 40 |
      is.na(age)
  )


cat(
  "\nNombre de femme-mois hors 15-39 ans :",
  nrow(controle_age_hors_champ),
  "\n"
)


# ------------------------------------------------------------
# 18. Vérifier l'absence de doublons femme-mois
# ------------------------------------------------------------

doublons_femme_mois <- base_femme_mois %>%
  count(
    pays,
    id_femme,
    mois_cmc
  ) %>%
  filter(
    n > 1
  )


cat(
  "Nombre de doublons femme-mois :",
  nrow(doublons_femme_mois),
  "\n"
)


# ------------------------------------------------------------
# 19. Agréger les événements de naissance par femme et mois
#
# Plusieurs naissances peuvent être observées dans
# un même femme-mois en cas de naissance multiple.
# ------------------------------------------------------------

naissances_femme_mois <- evenements %>%
  count(
    pays,
    id_femme,
    conception_cmc,
    name = "nb_naissances"
  )


# ------------------------------------------------------------
# 20. Vérifier que tous les événements correspondent
# à un femme-mois existant
# ------------------------------------------------------------

evenements_non_matches <- naissances_femme_mois %>%
  anti_join(
    
    base_femme_mois %>%
      distinct(
        pays,
        id_femme,
        mois_cmc
      ),
    
    by = c(
      "pays",
      "id_femme",
      "conception_cmc" = "mois_cmc"
    )
  )


cat(
  "\nEvénements sans femme-mois correspondant :",
  nrow(evenements_non_matches),
  "\n"
)


# ------------------------------------------------------------
# 21. Ajouter les naissances à la base femme-mois
# ------------------------------------------------------------

base_femme_mois <- base_femme_mois %>%
  left_join(
    
    naissances_femme_mois,
    
    by = c(
      "pays",
      "id_femme",
      "mois_cmc" = "conception_cmc"
    )
    
  ) %>%
  
  mutate(
    
    nb_naissances =
      coalesce(
        nb_naissances,
        0L
      )
  )


# ------------------------------------------------------------
# 22. Vérifier le nombre de naissances retrouvé
# ------------------------------------------------------------

controle_naissances <- base_femme_mois %>%
  group_by(pays) %>%
  summarise(
    
    total_naissances =
      sum(nb_naissances),
    
    femme_mois_avec_naissance =
      sum(nb_naissances > 0),
    
    femme_mois_sans_naissance =
      sum(nb_naissances == 0),
    
    .groups = "drop"
  )


# Nombre attendu directement depuis le script 05
naissances_attendues <- evenements %>%
  count(
    pays,
    name = "naissances_attendues"
  )


controle_naissances <- controle_naissances %>%
  left_join(
    naissances_attendues,
    by = "pays"
  ) %>%
  mutate(
    
    difference =
      total_naissances -
      naissances_attendues
  )


cat("\n========================================\n")
cat("NAISSANCES DANS LA BASE FEMME-MOIS\n")
cat("========================================\n")

print(controle_naissances)


cat(
  "\nNombre total de naissances attendu :",
  nrow(evenements),
  "\n"
)

cat(
  "Nombre total de naissances retrouvé :",
  sum(base_femme_mois$nb_naissances),
  "\n"
)


# ------------------------------------------------------------
# 23. Distribution du nombre de naissances
# par femme-mois
# ------------------------------------------------------------

distribution_naissances <- base_femme_mois %>%
  count(
    pays,
    nb_naissances,
    name = "nombre_femme_mois"
  ) %>%
  arrange(
    pays,
    nb_naissances
  )


cat("\n========================================\n")
cat("DISTRIBUTION DU NOMBRE DE NAISSANCES\n")
cat("========================================\n")

print(distribution_naissances)


# ------------------------------------------------------------
# 24. Vérifier la répartition par groupe d'âge
# ------------------------------------------------------------

controle_groupes_age <- base_femme_mois %>%
  count(
    pays,
    groupe_age,
    name = "femmes_mois"
  ) %>%
  arrange(
    pays,
    groupe_age
  )


cat("\n========================================\n")
cat("FEMMES-MOIS PAR GROUPE D'AGE\n")
cat("========================================\n")

print(controle_groupes_age)


# ------------------------------------------------------------
# 25. Vérification finale des périodes
# ------------------------------------------------------------

controle_periodes_final <- base_femme_mois %>%
  group_by(pays) %>%
  summarise(
    
    premier_mois =
      min(annee_mois),
    
    dernier_mois =
      max(annee_mois),
    
    nombre_mois_distincts =
      n_distinct(mois_cmc),
    
    .groups = "drop"
  )


cat("\n========================================\n")
cat("PERIODES FINALES\n")
cat("========================================\n")

print(controle_periodes_final)


# ------------------------------------------------------------
# 26. Sauvegarder les tableaux de contrôle
# ------------------------------------------------------------

write.csv(
  controle_femmes_eligibles,
  "outputs/tables/controle_femmes_eligibles.csv",
  row.names = FALSE
)

write.csv(
  controle_base,
  "outputs/tables/controle_base_femme_mois.csv",
  row.names = FALSE
)

write.csv(
  controle_naissances,
  "outputs/tables/controle_naissances_femme_mois.csv",
  row.names = FALSE
)

write.csv(
  controle_groupes_age,
  "outputs/tables/controle_groupes_age.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 27. Sauvegarder la base femme-mois
# ------------------------------------------------------------

saveRDS(
  base_femme_mois,
  "data_intermediate/base_femme_mois_15_39.rds"
)


# ------------------------------------------------------------
# 28. Résumé final
# ------------------------------------------------------------

cat("\n========================================\n")
cat("BASE FEMME-MOIS CONSTRUITE\n")
cat("========================================\n")

cat(
  "Nombre total de femmes-mois :",
  nrow(base_femme_mois),
  "\n"
)

cat(
  "Nombre total de femmes :",
  n_distinct(base_femme_mois$id_femme),
  "\n"
)

cat(
  "Nombre total de naissances :",
  sum(base_femme_mois$nb_naissances),
  "\n"
)

cat("\nFichier créé :\n")
cat(
  "data_intermediate/base_femme_mois_15_39.rds\n"
)