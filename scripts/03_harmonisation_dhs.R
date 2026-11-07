# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 03_harmonisation_dhs.R
#
# Objective:
# Harmonise DHS IR and BR files for Guinea,
# of Liberia and Sierra Leone.
#
# Inputs:
# - gn_ir, lb_ir, sl_ir
# - gn_br, lb_br, sl_br
#
# Outputs:
# - data_intermediate/dhs_femmes_harmonisees.rds
# - data_intermediate/dhs_naissances_harmonisees.rds
#
# Important:
# Raw DHS files are never modified.
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Function to harmonise IR data
# One row = one woman
# ------------------------------------------------------------

harmoniser_ir <- function(data, pays, code_pays) {
  
  data %>%
    transmute(
      
      # Identification
      pays = pays,
      code_pays = code_pays,
      
      caseid = as.character(caseid),
      
      # Unique identifier across the three countries
      id_femme = paste0(code_pays, "_", as.character(caseid)),
      
      # DHS survey design
      cluster = as.numeric(v001),
      menage = as.numeric(v002),
      numero_femme = as.numeric(v003),
      
      psu = as.numeric(v021),
      
      strate_v022 = as.numeric(v022),
      strate_v023 = as.numeric(v023),
      
      region = as.numeric(v024),
      residence = as.numeric(v025),
      
      # DHS sampling weights
      poids_brut = as.numeric(v005),
      poids = as.numeric(v005) / 1000000,
      
      # Interview date
      interview_mois = as.numeric(v006),
      interview_annee = as.numeric(v007),
      interview_cmc = as.numeric(v008),
      
      # Woman's date of birth and age
      naissance_femme_cmc = as.numeric(v011),
      age_enquete = as.numeric(v012)
    )
}


# ------------------------------------------------------------
# 3. Harmonise IR files
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
# 4. Combine all three countries
# ------------------------------------------------------------

dhs_femmes <- bind_rows(
  femmes_gn,
  femmes_lb,
  femmes_sl
)


# ------------------------------------------------------------
# 5. Checks of women's dataset
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
# 6. Function to harmonise BR data
# One row = one reported live birth
# ------------------------------------------------------------

harmoniser_br <- function(data, pays, code_pays) {
  
  data %>%
    transmute(
      
      # Identification
      pays = pays,
      code_pays = code_pays,
      
      caseid = as.character(caseid),
      
      id_femme = paste0(code_pays, "_", as.character(caseid)),
      
      # Birth identification
      index_naissance = as.numeric(bidx),
      rang_naissance = as.numeric(bord),
      naissance_multiple = as.numeric(b0),
      
      # Child's date of birth
      naissance_mois = as.numeric(b1),
      naissance_annee = as.numeric(b2),
      naissance_cmc = as.numeric(b3),
      
      # Mother's geography / survey design
      cluster = as.numeric(v001),
      menage = as.numeric(v002),
      numero_femme = as.numeric(v003),
      
      # Mother's DHS sampling weight
      poids_brut = as.numeric(v005),
      poids = as.numeric(v005) / 1000000,
      
      # Survey date
      interview_cmc = as.numeric(v008),
      
      # Mother's date of birth
      naissance_femme_cmc = as.numeric(v011)
    ) %>%
    
    # Unique identifier for each birth event
    mutate(
      id_naissance = paste0(
        id_femme,
        "_",
        index_naissance
      )
    )
}


# ------------------------------------------------------------
# 7. Harmonise BR files
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
# 8. Combine all three countries
# ------------------------------------------------------------

dhs_naissances <- bind_rows(
  naissances_gn,
  naissances_lb,
  naissances_sl
)


# ------------------------------------------------------------
# 9. Checks of birth dataset
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
# 10. Check that all BR mothers appear in IR
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
# 11. Save intermediate datasets
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
# 12. End
# ------------------------------------------------------------

cat("\n========================================\n")
cat("HARMONISATION DHS TERMINEE\n")
cat("========================================\n")
cat("Aucune donnée brute n'a été modifiée.\n")
