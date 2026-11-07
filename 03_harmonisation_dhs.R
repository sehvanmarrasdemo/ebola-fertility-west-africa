# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 03_harmonisation_dhs.R
#
# Objectif :
# Harmoniser les fichiers DHS IR et BR de la Guinée,
# du Liberia et de la Sierra Leone.
#
# Entrées :
# - gn_ir, lb_ir, sl_ir
# - gn_br, lb_br, sl_br
#
# Sorties :
# - data_intermediate/dhs_femmes_harmonisees.rds
# - data_intermediate/dhs_naissances_harmonisees.rds
#
# Important :
# Les fichiers bruts DHS ne sont jamais modifiés.
# ============================================================


# ------------------------------------------------------------
# 1. Charger les packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Fonction d'harmonisation du fichier IR
# 1 ligne = 1 femme
# ------------------------------------------------------------

harmoniser_ir <- function(data, pays, code_pays) {
  
  data %>%
    transmute(
      
      # Identification
      pays = pays,
      code_pays = code_pays,
      
      caseid = as.character(caseid),
      
      # Identifiant unique entre les trois pays
      id_femme = paste0(code_pays, "_", as.character(caseid)),
      
      # Plan de sondage DHS
      cluster = as.numeric(v001),
      menage = as.numeric(v002),
      numero_femme = as.numeric(v003),
      
      psu = as.numeric(v021),
      
      strate_v022 = as.numeric(v022),
      strate_v023 = as.numeric(v023),
      
      region = as.numeric(v024),
      residence = as.numeric(v025),
      
      # Poids DHS
      poids_brut = as.numeric(v005),
      poids = as.numeric(v005) / 1000000,
      
      # Date de l'entretien
      interview_mois = as.numeric(v006),
      interview_annee = as.numeric(v007),
      interview_cmc = as.numeric(v008),
      
      # Date de naissance et âge de la femme
      naissance_femme_cmc = as.numeric(v011),
      age_enquete = as.numeric(v012)
    )
}


# ------------------------------------------------------------
# 3. Harmoniser les fichiers IR
# ------------------------------------------------------------

femmes_gn <- harmoniser_ir(
  gn_ir,
  pays = "Guinée",
  code_pays = "GN"
)

femmes_lb <- harmoniser_ir(
  lb_ir,
  pays = "Liberia",
  code_pays = "LB"
)

femmes_sl <- harmoniser_ir(
  sl_ir,
  pays = "Sierra Leone",
  code_pays = "SL"
)


# ------------------------------------------------------------
# 4. Regrouper les trois pays
# ------------------------------------------------------------

dhs_femmes <- bind_rows(
  femmes_gn,
  femmes_lb,
  femmes_sl
)


# ------------------------------------------------------------
# 5. Vérifications de la base femmes
# ------------------------------------------------------------

cat("\n==============================\n")
cat("BASE FEMMES HARMONISEE\n")
cat("==============================\n")

dhs_femmes %>%
  count(pays) %>%
  print()


cat("\nDoublons id_femme :",
    sum(duplicated(dhs_femmes$id_femme)),
    "\n")


# ------------------------------------------------------------
# 6. Fonction d'harmonisation du fichier BR
# 1 ligne = 1 naissance vivante déclarée
# ------------------------------------------------------------

harmoniser_br <- function(data, pays, code_pays) {
  
  data %>%
    transmute(
      
      # Identification
      pays = pays,
      code_pays = code_pays,
      
      caseid = as.character(caseid),
      
      id_femme = paste0(code_pays, "_", as.character(caseid)),
      
      # Identification de la naissance
      index_naissance = as.numeric(bidx),
      rang_naissance = as.numeric(bord),
      naissance_multiple = as.numeric(b0),
      
      # Date de naissance de l'enfant
      naissance_mois = as.numeric(b1),
      naissance_annee = as.numeric(b2),
      naissance_cmc = as.numeric(b3),
      
      # Géographie / sondage de la mère
      cluster = as.numeric(v001),
      menage = as.numeric(v002),
      numero_femme = as.numeric(v003),
      
      # Poids DHS de la mère
      poids_brut = as.numeric(v005),
      poids = as.numeric(v005) / 1000000,
      
      # Date de l'enquête
      interview_cmc = as.numeric(v008),
      
      # Date de naissance de la mère
      naissance_femme_cmc = as.numeric(v011)
    ) %>%
    
    # Identifiant unique de chaque événement de naissance
    mutate(
      id_naissance = paste0(
        id_femme,
        "_",
        index_naissance
      )
    )
}


# ------------------------------------------------------------
# 7. Harmoniser les fichiers BR
# ------------------------------------------------------------

naissances_gn <- harmoniser_br(
  gn_br,
  pays = "Guinée",
  code_pays = "GN"
)

naissances_lb <- harmoniser_br(
  lb_br,
  pays = "Liberia",
  code_pays = "LB"
)

naissances_sl <- harmoniser_br(
  sl_br,
  pays = "Sierra Leone",
  code_pays = "SL"
)


# ------------------------------------------------------------
# 8. Regrouper les trois pays
# ------------------------------------------------------------

dhs_naissances <- bind_rows(
  naissances_gn,
  naissances_lb,
  naissances_sl
)


# ------------------------------------------------------------
# 9. Vérifications de la base naissances
# ------------------------------------------------------------

cat("\n==============================\n")
cat("BASE NAISSANCES HARMONISEE\n")
cat("==============================\n")

dhs_naissances %>%
  count(pays) %>%
  print()


cat("\nDoublons id_naissance :",
    sum(duplicated(dhs_naissances$id_naissance)),
    "\n")


# ------------------------------------------------------------
# 10. Vérifier que toutes les mères du BR existent dans l'IR
# ------------------------------------------------------------

controle_meres <- dhs_naissances %>%
  distinct(id_femme) %>%
  anti_join(
    dhs_femmes %>% distinct(id_femme),
    by = "id_femme"
  )

cat(
  "\nNombre de mères présentes dans BR mais absentes de IR :",
  nrow(controle_meres),
  "\n"
)


# ------------------------------------------------------------
# 11. Sauvegarde des bases intermédiaires
# ------------------------------------------------------------

saveRDS(
  dhs_femmes,
  "data_intermediate/dhs_femmes_harmonisees.rds"
)

saveRDS(
  dhs_naissances,
  "data_intermediate/dhs_naissances_harmonisees.rds"
)


# ------------------------------------------------------------
# 12. Fin
# ------------------------------------------------------------

cat("\n========================================\n")
cat("HARMONISATION DHS TERMINEE\n")
cat("========================================\n")
cat("Aucune donnée brute n'a été modifiée.\n")