# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 05_preparation_evenements_naissance.R
#
# Objective:
# Prepare birth events used in the analysis.
#
# For each birth:
# - calculate approximate conception month (-9 months)
# - calculate the mother's age at conception month
# - apply each country's observation window
# - retain conceptions when the woman is aged 15–39
#
# Inputs:
# data_intermediate/dhs_naissances_harmonisees.rds
# data_intermediate/fenetres_analyse.rds
#
# Output:
# data_intermediate/dhs_evenements_naissance_analyse.rds
#
# Important:
# Multiple births are retained.
# A twin birth therefore corresponds to multiple
# births observed in the same woman-month.
# ============================================================


# ------------------------------------------------------------
# 1. Load the package
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Import intermediate data
# ------------------------------------------------------------

dhs_naissances <- readRDS(
  "data_intermediate/dhs_naissances_harmonisees.rds"
)

fenetres_analyse <- readRDS(
  "data_intermediate/fenetres_analyse.rds"
)


# ------------------------------------------------------------
# 3. Check imported data
# ------------------------------------------------------------

cat("\nNombre total de naissances importées :",
    nrow(dhs_naissances), "\n")

print(fenetres_analyse)


# ------------------------------------------------------------
# 4. Add country-specific observation bounds
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


# Check that all observations were assigned an observation window
sum(is.na(naissances_temp$debut_analyse_cmc))
sum(is.na(naissances_temp$fin_analyse_cmc))


# ------------------------------------------------------------
# 5. Construct approximate conception month
#
# Main assumption:
# conception ≈ birth minus 9 months
#
# Robustness checks using -8 and -10 months will be
# performed later.
# ------------------------------------------------------------

naissances_temp <- naissances_temp %>%
  mutate(
    conception_cmc = naissance_cmc - 9
  )


# ------------------------------------------------------------
# 6. Calculate mother's age at conception month
# ------------------------------------------------------------

naissances_temp <- naissances_temp %>%
  mutate(
    age_conception_mois =
      conception_cmc - naissance_femme_cmc,
    
    age_conception =
      age_conception_mois / 12
  )


# ------------------------------------------------------------
# 7. Convert conception CMC to year and month
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
# 8. Identify eligible observations
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
# 9. Check before filtering
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
# 10. Create the final birth-event dataset
# ------------------------------------------------------------

evenements_naissance <- naissances_temp %>%
  filter(
    dans_fenetre,
    age_eligible
  )


# ------------------------------------------------------------
# 11. Check the actual observation period
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
# 12. Check multiple births
#
# Multiple rows for the same woman and month
# do not necessarily indicate a duplicate:
# they may represent twins or other multiple births.
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
# 13. Save the dataset
# ------------------------------------------------------------

saveRDS(
  evenements_naissance,
  "data_intermediate/dhs_evenements_naissance_analyse.rds"
)


# ------------------------------------------------------------
# 14. Save the selection validation
# ------------------------------------------------------------

write.csv(
  controle_selection,
  "outputs/tables/controle_selection_evenements.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 15. End
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PREPARATION DES EVENEMENTS TERMINEE\n")
cat("========================================\n")

cat(
  "Nombre final d'événements de naissance :",
  nrow(evenements_naissance),
  "\n"
)