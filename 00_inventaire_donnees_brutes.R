# ============================================================
# MEMOIRE - Ebola et fécondité en Afrique de l'Ouest
# Script : 00_inventaire_donnees_brutes.R
#
# Objectif :
# Vérifier la présence des fichiers DHS bruts nécessaires
# pour la Guinée, le Liberia et la Sierra Leone.
#
# Entrées :
# data_raw/dhs/
#
# Sorties :
# Aucune base transformée.
#
# Important :
# Ce script ne modifie aucune donnée brute.
# ============================================================


# ------------------------------------------------------------
# 1. Vérifier le répertoire de travail
# ------------------------------------------------------------

getwd()


# ------------------------------------------------------------
# 2. Vérifier que le dossier DHS existe
# ------------------------------------------------------------

dir.exists("data_raw/dhs")


# ------------------------------------------------------------
# 3. Lister tous les fichiers présents dans data_raw/dhs
# ------------------------------------------------------------

tous_fichiers_dhs <- list.files(
  path = "data_raw/dhs",
  recursive = TRUE,
  full.names = TRUE
)

tous_fichiers_dhs


# ------------------------------------------------------------
# 4. Rechercher les fichiers Stata (.DTA)
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
# 5. Compter les fichiers Stata trouvés
# ------------------------------------------------------------

length(fichiers_dta)


# ------------------------------------------------------------
# 6. Rechercher les fichiers géographiques DHS (.shp)
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
# 7. Compter les fichiers GPS trouvés
# ------------------------------------------------------------

length(fichiers_gps)


# ------------------------------------------------------------
# 8. Résumé simple de l'inventaire
# ------------------------------------------------------------

cat("\n----------------------------------------\n")
cat("INVENTAIRE DES DONNEES DHS\n")
cat("----------------------------------------\n")
cat("Nombre de fichiers .DTA trouvés :", length(fichiers_dta), "\n")
cat("Nombre de fichiers .shp trouvés :", length(fichiers_gps), "\n")
cat("----------------------------------------\n")