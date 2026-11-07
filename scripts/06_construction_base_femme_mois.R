# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 06_construction_base_femme_mois.R
#
# Objective:
# Construct the central woman-month analysis dataset.
#
# One observation = one woman in a given month.
#
# The month represents the approximate conception timeline
# :
# observed birth minus 9 months.
#
# For each woman, retain only months:
# - within her country's observation window;
# - during which she is aged 15–39.
#
# Birth events prepared in script 05
# are subsequently linked to their approximate
# conception month.
#
# Inputs:
# - data_intermediate/dhs_femmes_harmonisees.rds
# - data_intermediate/dhs_evenements_naissance_analyse.rds
# - data_intermediate/fenetres_analyse.rds
#
# Output:
# - data_intermediate/base_femme_mois_15_39.rds
#
# Important:
# - Women without births are retained.
# - Multiple births are retained.
# - Raw DHS data are never modified.
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(dplyr)
library(tidyr)


# ------------------------------------------------------------
# 2. Define input paths
# ------------------------------------------------------------

path_femmes <-
  "data_intermediate/dhs_femmes_harmonisees.rds"

path_evenements <-
  "data_intermediate/dhs_evenements_naissance_analyse.rds"

path_fenetres <-
  "data_intermediate/fenetres_analyse.rds"


# ------------------------------------------------------------
# 3. Check that files exist
# ------------------------------------------------------------

stopifnot(
  file.exists(path_femmes),
  file.exists(path_evenements),
  file.exists(path_fenetres)
)


# ------------------------------------------------------------
# 4. Import intermediate datasets
# ------------------------------------------------------------

femmes <- readRDS(path_femmes)

evenements <- readRDS(path_evenements)

fenetres <- readRDS(path_fenetres)


# ------------------------------------------------------------
# 5. Check imported data
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
# 6. Add each woman's country-specific observation window
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
# 7. Check that all women have an observation window
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
# 8. Check women's dates of birth
# ------------------------------------------------------------

cat(
  "Femmes sans CMC de naissance :",
  sum(is.na(femmes_temp$naissance_femme_cmc)),
  "\n"
)


# ------------------------------------------------------------
# 9. Determine each woman's eligible months
#
# 15 years = 15 * 12 = 180 months
#
# A woman is retained as long as her age is strictly
# under 40 years.
#
# The last eligible month is therefore month 479
# after her birth month.
# ------------------------------------------------------------

femmes_temp <- femmes_temp %>%
  mutate(
    
    # First approximate month from which
    # the woman turns 15
    debut_age_cmc =
      naissance_femme_cmc + 180,
    
    # Last month before turning 40
    fin_age_cmc =
      naissance_femme_cmc + 479,
    
    # First month actually included
    debut_femme_cmc =
      pmax(
        debut_analyse_cmc,
        debut_age_cmc
      ),
    
    # Last month actually included
    fin_femme_cmc =
      pmin(
        fin_analyse_cmc,
        fin_age_cmc
      ),
    
    # Total months contributed
    n_mois =
      fin_femme_cmc -
      debut_femme_cmc +
      1
  )


# ------------------------------------------------------------
# 10. Check women with at least one eligible month
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
# 11. Retain only contributing women
# to at least one month
# ------------------------------------------------------------

femmes_eligibles <- femmes_temp %>%
  filter(
    !is.na(n_mois),
    n_mois > 0
  )


# ------------------------------------------------------------
# 12. Expand each woman into woman-month records
#
# Example:
#
# A woman with 100 eligible months
# becomes 100 rows.
# ------------------------------------------------------------

base_femme_mois <- femmes_eligibles %>%
  
  select(
    
    # Identification
    pays,
    code_pays,
    caseid,
    id_femme,
    
    # Survey design and geography
    cluster,
    menage,
    numero_femme,
    psu,
    strate_v022,
    strate_v023,
    region,
    residence,
    
    # DHS sampling weights
    poids_brut,
    poids,
    
    # Woman's temporal information
    interview_cmc,
    naissance_femme_cmc,
    age_enquete,
    
    # Individual bounds
    debut_femme_cmc,
    fin_femme_cmc,
    
    # Number of months to create
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
# 13. Create unique geographic identifiers
# across the three countries
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
# 14. Recalculate women's age for each month
# ------------------------------------------------------------

base_femme_mois <- base_femme_mois %>%
  mutate(
    
    age_mois =
      mois_cmc -
      naissance_femme_cmc,
    
    # Approximate age in completed years
    age =
      floor(
        age_mois / 12
      ),
    
    # Five-year age groups
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
# 15. Convert CMC to year and month
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
# 16. Check dataset BEFORE adding births
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
# 17. Explicitly check the 15–39 age restriction
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
# 18. Check for duplicate woman-month records
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
# 19. Aggregate birth events by woman and month
#
# Multiple births may be observed within
# one woman-month in the case of a multiple birth.
# ------------------------------------------------------------

naissances_femme_mois <- evenements %>%
  count(
    pays,
    id_femme,
    conception_cmc,
    name = "nb_naissances"
  )


# ------------------------------------------------------------
# 20. Check that all events match
# to an existing woman-month record
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
# 21. Add births to woman-month data
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
# 22. Check number of matched births
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


# Expected count taken directly from script 05
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
# 23. Distribution of birth counts
# per woman-month
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
# 24. Check distribution by age group
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
# 25. Final check of time periods
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
# 26. Save validation tables
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
# 27. Save woman-month data
# ------------------------------------------------------------

saveRDS(
  base_femme_mois,
  "data_intermediate/base_femme_mois_15_39.rds"
)


# ------------------------------------------------------------
# 28. Final summary
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
