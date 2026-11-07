# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 02_definition_fenetres_temporelles.R
#
# Objective:
# Determine the maximum theoretically
# usable observation window for each country.
#
# Main population:
# Women aged 15–39 in the historical month considered.
#
# Rules:
# - Start = latest DHS interview minus 120 months
#   to preserve coverage of women aged 15–39.
#
# - End = earliest DHS interview minus 9 months
#   to allow time for conceptions to result
#   in an observable birth before the interview.
#
# Inputs:
# IR objects imported in script 01:
# gn_ir, lb_ir, sl_ir
#
# Outputs:
# data_intermediate/fenetres_analyse.rds
# outputs/tables/fenetres_analyse.csv
#
# Important:
# This is a theoretical observation window.
# It will subsequently be checked empirically in the dataset
# woman-months.
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Function to convert DHS CMC to year-month
# ------------------------------------------------------------

cmc_vers_annee_mois <- function(cmc) {
  
  annee <- 1900 + (cmc - 1) %/% 12
  mois  <- ((cmc - 1) %% 12) + 1
  
  sprintf("%04d-%02d", annee, mois)
}


# ------------------------------------------------------------
# 3. Function to calculate a country's observation window
# ------------------------------------------------------------

calculer_fenetre <- function(data, pays) {
  
  # Earliest interview observed in the country
  premiere_interview <- min(
    as.numeric(data$v008),
    na.rm = TRUE
  )
  
  # Latest interview observed in the country
  derniere_interview <- max(
    as.numeric(data$v008),
    na.rm = TRUE
  )
  
  # Start:
  # 10 years before the latest interview
  debut_analyse <- derniere_interview - 120
  

  # End:
  # the interview month is incomplete,
  # therefore we subtract 10 months in total
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
# 4. Calculate observation windows for all three countries
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
# 5. Combine all three countries
# ------------------------------------------------------------

fenetres_analyse <- bind_rows(
  fenetre_gn,
  fenetre_lb,
  fenetre_sl
)


# ------------------------------------------------------------
# 6. Display results
# ------------------------------------------------------------

print(fenetres_analyse)


# ------------------------------------------------------------
# 7. Simplified display
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
# 8. Save results
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
# 9. End of script
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