# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 01_import_verification_dhs.R
#
# Objective:
# Import and check IR and BR files for all three countries
# their structure before any transformation.
#
# Countries:
# - Guinea, DHS 2018
# - Liberia, DHS 2019–2020
# - Sierra Leone, DHS 2019
#
# Inputs:
# data_raw/dhs/
#
# Outputs:
# No transformed individual-level data.
# This script is used solely to validate the raw data.
# ============================================================


# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(haven)
library(dplyr)


# ------------------------------------------------------------
# 2. Define raw data file paths
# ------------------------------------------------------------

# Guinea 2018
path_gn_ir <- "data_raw/dhs/GN_2018/IR/GNIR71DT/GNIR71FL.DTA"
path_gn_br <- "data_raw/dhs/GN_2018/BR/GNBR71DT/GNBR71FL.DTA"

# Liberia 2019-2020
path_lb_ir <- "data_raw/dhs/LIB_2019_2020/IR/LBIR7ADT/LBIR7AFL.DTA"
path_lb_br <- "data_raw/dhs/LIB_2019_2020/BR/LBBR7ADT/LBBR7AFL.DTA"

# Sierra Leone 2019
path_sl_ir <- "data_raw/dhs/SL_2019/IR/SLIR7ADT/SLIR7AFL.DTA"
path_sl_br <- "data_raw/dhs/SL_2019/BR/SLBR7ADT/SLBR7AFL.DTA"


# ------------------------------------------------------------
# 3. Check that all files exist
# ------------------------------------------------------------

stopifnot(
  file.exists(path_gn_ir),
  file.exists(path_gn_br),
  file.exists(path_lb_ir),
  file.exists(path_lb_br),
  file.exists(path_sl_ir),
  file.exists(path_sl_br)
)

cat("Les 6 fichiers DHS sont présents.\n")


# ------------------------------------------------------------
# 4. Import IR files
# One row = one interviewed woman
# ------------------------------------------------------------

gn_ir <- read_dta(path_gn_ir)
lb_ir <- read_dta(path_lb_ir)
sl_ir <- read_dta(path_sl_ir)


# ------------------------------------------------------------
# 5. Import BR files
# One row = one reported birth
# ------------------------------------------------------------

gn_br <- read_dta(path_gn_br)
lb_br <- read_dta(path_lb_br)
sl_br <- read_dta(path_sl_br)


# ------------------------------------------------------------
# 6. Check file dimensions
# ------------------------------------------------------------

cat("\n==============================\n")
cat("DIMENSIONS DES FICHIERS IR\n")
cat("==============================\n")

cat("Guinée :", nrow(gn_ir), "femmes -", ncol(gn_ir), "variables\n")
cat("Liberia :", nrow(lb_ir), "femmes -", ncol(lb_ir), "variables\n")
cat("Sierra Leone :", nrow(sl_ir), "femmes -", ncol(sl_ir), "variables\n")


cat("\n==============================\n")
cat("DIMENSIONS DES FICHIERS BR\n")
cat("==============================\n")

cat("Guinée :", nrow(gn_br), "naissances -", ncol(gn_br), "variables\n")
cat("Liberia :", nrow(lb_br), "naissances -", ncol(lb_br), "variables\n")
cat("Sierra Leone :", nrow(sl_br), "naissances -", ncol(sl_br), "variables\n")


# ------------------------------------------------------------
# 7. Expected key variables in IR files
# ------------------------------------------------------------

variables_ir <- c(
  "caseid",  # identifiant de la femme
  "v001",    # cluster DHS
  "v002",    # ménage
  "v003",    # numéro de la femme dans le ménage
  "v005",    # poids d'enquête
  "v008",    # date de l'enquête en CMC
  "v011",    # date de naissance de la femme en CMC
  "v012",    # âge au moment de l'enquête
  "v021",    # unité primaire d'échantillonnage
  "v022",    # variable de stratification
  "v023",    # autre variable de stratification DHS
  "v024",    # région
  "v025"     # milieu urbain/rural
)


# ------------------------------------------------------------
# 8. Check presence of IR variables
# ------------------------------------------------------------

verifier_variables <- function(data, variables, nom_base) {
  
  presentes <- variables[variables %in% names(data)]
  absentes  <- variables[!variables %in% names(data)]
  
  cat("\n------------------------------\n")
  cat(nom_base, "\n")
  cat("------------------------------\n")
  
  cat("Variables présentes :\n")
  print(presentes)
  
  cat("\nVariables absentes :\n")
  print(absentes)
}


verifier_variables(gn_ir, variables_ir, "Guinée IR")
verifier_variables(lb_ir, variables_ir, "Liberia IR")
verifier_variables(sl_ir, variables_ir, "Sierra Leone IR")


# ------------------------------------------------------------
# 9. Expected key variables in BR files
# ------------------------------------------------------------

variables_br <- c(
  "caseid",  # identifiant de la mère
  "v001",    # cluster
  "v002",    # ménage
  "v003",    # numéro de la femme
  "v005",    # poids DHS
  "v008",    # date de l'enquête en CMC
  "v011",    # date de naissance de la mère en CMC
  "v012",    # âge de la mère à l'enquête
  "v021",    # PSU
  "v022",    # stratification
  "v023",    # stratification alternative
  "v024",    # région
  "v025",    # urbain/rural
  "bidx",    # index de la naissance dans l'historique
  "bord",    # rang de naissance
  "b0",      # naissance multiple
  "b1",      # mois de naissance de l'enfant
  "b2",      # année de naissance de l'enfant
  "b3"       # date de naissance de l'enfant en CMC
)


# ------------------------------------------------------------
# 10. Check presence of BR variables
# ------------------------------------------------------------

verifier_variables(gn_br, variables_br, "Guinée BR")
verifier_variables(lb_br, variables_br, "Liberia BR")
verifier_variables(sl_br, variables_br, "Sierra Leone BR")


# ------------------------------------------------------------
# 11. Check women's age in IR files
# ------------------------------------------------------------

cat("\n==============================\n")
cat("AGE DES FEMMES A L'ENQUETE\n")
cat("==============================\n")

cat(
  "Guinée :",
  min(gn_ir$v012, na.rm = TRUE),
  "-",
  max(gn_ir$v012, na.rm = TRUE),
  "ans\n"
)

cat(
  "Liberia :",
  min(lb_ir$v012, na.rm = TRUE),
  "-",
  max(lb_ir$v012, na.rm = TRUE),
  "ans\n"
)

cat(
  "Sierra Leone :",
  min(sl_ir$v012, na.rm = TRUE),
  "-",
  max(sl_ir$v012, na.rm = TRUE),
  "ans\n"
)


# ------------------------------------------------------------
# 12. Check DHS sampling weights
# ------------------------------------------------------------

summary(gn_ir$v005)
summary(lb_ir$v005)
summary(sl_ir$v005)


# ------------------------------------------------------------
# 13. Check number of DHS clusters
# ------------------------------------------------------------

cat("\n==============================\n")
cat("NOMBRE DE CLUSTERS DHS\n")
cat("==============================\n")

cat("Guinée :", n_distinct(gn_ir$v001), "\n")
cat("Liberia :", n_distinct(lb_ir$v001), "\n")
cat("Sierra Leone :", n_distinct(sl_ir$v001), "\n")


# ------------------------------------------------------------
# 14. Check individual identifiers
# ------------------------------------------------------------

cat("\n==============================\n")
cat("UNICITE DES FEMMES DANS IR\n")
cat("==============================\n")

cat(
  "Guinée - doublons caseid :",
  sum(duplicated(gn_ir$caseid)),
  "\n"
)

cat(
  "Liberia - doublons caseid :",
  sum(duplicated(lb_ir$caseid)),
  "\n"
)

cat(
  "Sierra Leone - doublons caseid :",
  sum(duplicated(sl_ir$caseid)),
  "\n"
)


# ------------------------------------------------------------
# 15. Final summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("FIN DE LA VERIFICATION DES DONNEES DHS\n")
cat("========================================\n")
cat("Aucune donnée brute n'a été modifiée.\n")
# ------------------------------------------------------------
# 15. Check survey fieldwork dates
# ------------------------------------------------------------

# Check that the interview month and year are present
variables_date <- c("v006", "v007", "v008")

verifier_variables(gn_ir, variables_date, "Guinée - dates enquête")
verifier_variables(lb_ir, variables_date, "Liberia - dates enquête")
verifier_variables(sl_ir, variables_date, "Sierra Leone - dates enquête")


# Function to summarise interview dates
resume_dates_enquete <- function(data, pays) {
  
  dates <- data %>%
    transmute(
      annee = as.numeric(v007),
      mois = as.numeric(v006)
    ) %>%
    filter(
      !is.na(annee),
      !is.na(mois)
    )
  
  cat("\n------------------------------\n")
  cat(pays, "\n")
  cat("------------------------------\n")
  
  cat(
    "Première date observée :",
    sprintf("%02d", min(dates$mois[dates$annee == min(dates$annee)])),
    "/",
    min(dates$annee),
    "\n"
  )
  
  cat(
    "Dernière date observée :",
    sprintf("%02d", max(dates$mois[dates$annee == max(dates$annee)])),
    "/",
    max(dates$annee),
    "\n"
  )
}


resume_dates_enquete(gn_ir, "Guinée")
resume_dates_enquete(lb_ir, "Liberia")
resume_dates_enquete(sl_ir, "Sierra Leone")

#----------------------------
#INTERVIEW TABLE

table(gn_ir$v007, gn_ir$v006)
table(lb_ir$v007, lb_ir$v006)
table(sl_ir$v007, sl_ir$v006)
