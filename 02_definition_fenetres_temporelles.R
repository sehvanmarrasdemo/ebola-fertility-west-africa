# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 02_definition_fenetres_temporelles.R
#
# Objectif :
# Déterminer la fenêtre temporelle maximale théoriquement
# exploitable pour chaque pays.
#
# Population principale :
# Femmes âgées de 15 à 39 ans au mois historique considéré.
#
# Règles :
# - Début = dernière interview DHS - 120 mois
#   afin de préserver la couverture des femmes de 15-39 ans.
#
# - Fin = première interview DHS - 9 mois
#   afin de laisser le temps aux conceptions de se traduire
#   par une naissance observable avant l'entretien.
#
# Entrées :
# Objets IR importés dans le script 01 :
# gn_ir, lb_ir, sl_ir
#
# Sorties :
# data_intermediate/fenetres_analyse.rds
# outputs/tables/fenetres_analyse.csv
#
# Important :
# Il s'agit d'une fenêtre théorique.
# Elle sera ensuite vérifiée empiriquement dans la base
# femme-mois.
# ============================================================


# ------------------------------------------------------------
# 1. Charger les packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Fonction de conversion du CMC DHS en année-mois
# ------------------------------------------------------------

cmc_vers_annee_mois <- function(cmc) {
  
  annee <- 1900 + (cmc - 1) %/% 12
  mois  <- ((cmc - 1) %% 12) + 1
  
  sprintf("%04d-%02d", annee, mois)
}


# ------------------------------------------------------------
# 3. Fonction de calcul de la fenêtre d'un pays
# ------------------------------------------------------------

calculer_fenetre <- function(data, pays) {
  
  # Première interview observée dans le pays
  premiere_interview <- min(
    as.numeric(data$v008),
    na.rm = TRUE
  )
  
  # Dernière interview observée dans le pays
  derniere_interview <- max(
    as.numeric(data$v008),
    na.rm = TRUE
  )
  
  # Début :
  # 10 ans avant la dernière interview
  debut_analyse <- derniere_interview - 120
  

  # Fin :
  # le mois d'interview est incomplet,
  # donc on retire 10 mois au total
  fin_analyse <- premiere_interview - 10
  
  tibble(
    pays = pays,
    
    premiere_interview_cmc = premiere_interview,
    derniere_interview_cmc = derniere_interview,
    
    premiere_interview = cmc_vers_annee_mois(premiere_interview),
    derniere_interview = cmc_vers_annee_mois(derniere_interview),
    
    debut_analyse_cmc = debut_analyse,
    fin_analyse_cmc = fin_analyse,
    
    debut_analyse = cmc_vers_annee_mois(debut_analyse),
    fin_analyse = cmc_vers_annee_mois(fin_analyse)
  )
}

# ------------------------------------------------------------
# 4. Calculer la fenêtre pour les trois pays
# ------------------------------------------------------------

fenetre_gn <- calculer_fenetre(
  gn_ir,
  "Guinée"
)

fenetre_lb <- calculer_fenetre(
  lb_ir,
  "Liberia"
)

fenetre_sl <- calculer_fenetre(
  sl_ir,
  "Sierra Leone"
)


# ------------------------------------------------------------
# 5. Regrouper les trois pays
# ------------------------------------------------------------

fenetres_analyse <- bind_rows(
  fenetre_gn,
  fenetre_lb,
  fenetre_sl
)


# ------------------------------------------------------------
# 6. Afficher les résultats
# ------------------------------------------------------------

print(fenetres_analyse)


# ------------------------------------------------------------
# 7. Affichage simplifié
# ------------------------------------------------------------

fenetres_analyse %>%
  select(
    pays,
    premiere_interview,
    derniere_interview,
    debut_analyse,
    fin_analyse
  ) %>%
  print()


# ------------------------------------------------------------
# 8. Sauvegarder les résultats
# ------------------------------------------------------------

saveRDS(
  fenetres_analyse,
  "data_intermediate/fenetres_analyse.rds"
)

write.csv(
  fenetres_analyse,
  "outputs/tables/fenetres_analyse.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. Fin du script
# ------------------------------------------------------------

cat("\n========================================\n")
cat("FENETRES TEMPORELLES CALCULEES\n")
cat("========================================\n")

fenetres_analyse %>%
  select(
    pays,
    debut_analyse,
    fin_analyse
  ) %>%
  print()