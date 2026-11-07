# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 00_inventaire_donnees_brutes.R
#
# Objective:
# Check that the required raw DHS files are available
# for Guinea, Liberia, and Sierra Leone.
#
# Inputs:
# data_raw/dhs/
#
# Outputs:
# No transformed dataset.
#
# Important:
# This script does not modify any raw data.
# ============================================================


# ------------------------------------------------------------
# 1. Check the working directory
# ------------------------------------------------------------

getwd()


# ------------------------------------------------------------
# 2. Check that the DHS directory exists
# ------------------------------------------------------------

dir.exists("data_raw/dhs")


# ------------------------------------------------------------
# 3. List all files under data_raw/dhs
# ------------------------------------------------------------

tous_fichiers_dhs <- list.files(
  path = "data_raw/dhs",
  recursive = TRUE,
  full.names = TRUE
)

tous_fichiers_dhs


# ------------------------------------------------------------
# 4. Find Stata files (.DTA)
# ------------------------------------------------------------

fichiers_dta <- list.files(
  path = "data_raw/dhs",
  pattern = "\\.DTA$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

fichiers_dta


# ------------------------------------------------------------
# 5. Count the Stata files found
# ------------------------------------------------------------

length(fichiers_dta)


# ------------------------------------------------------------
# 6. Find DHS geographic files (.shp)
# ------------------------------------------------------------

fichiers_gps <- list.files(
  path = "data_raw/dhs",
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

fichiers_gps


# ------------------------------------------------------------
# 7. Count the GPS files found
# ------------------------------------------------------------

length(fichiers_gps)


# ------------------------------------------------------------
# 8. Summarise the file inventory
# ------------------------------------------------------------

cat("\n----------------------------------------\n")
cat("INVENTAIRE DES DONNEES DHS\n")
cat("----------------------------------------\n")
cat("Nombre de fichiers .DTA trouvés :", length(fichiers_dta), "\n")
cat("Nombre de fichiers .shp trouvés :", length(fichiers_gps), "\n")
cat("----------------------------------------\n")
