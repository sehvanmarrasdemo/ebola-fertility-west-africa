# ============================================================
# 12_preparation_temporelle_backer.R
# Préparation temporelle des données Ebola Backer
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# ------------------------------------------------------------
# 1. Importation des données brutes Backer
# ------------------------------------------------------------

chemin_backer <- "data_raw/backer/countryeboladata.txt"

backer_raw <- read.delim(
  chemin_backer,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# ------------------------------------------------------------
# 2. Vérification des colonnes hebdomadaires W1-W74
# ------------------------------------------------------------

vars_week <- paste0("W", 1:74)

cat("Dimensions du fichier :", dim(backer_raw), "\n")

cat(
  "Nombre de variables W trouvées :",
  sum(vars_week %in% names(backer_raw)),
  "\n"
)

cat(
  "Nombre de valeurs manquantes dans W1-W74 :",
  sum(is.na(backer_raw[, vars_week])),
  "\n"
)

# ------------------------------------------------------------
# 3. Passage du format large au format long
# ------------------------------------------------------------

backer_hebdo <- backer_raw %>%
  pivot_longer(
    cols = all_of(vars_week),
    names_to = "semaine_backer",
    values_to = "cas_semaine"
  ) %>%
  mutate(
    numero_semaine = as.integer(sub("W", "", semaine_backer)),
    date_debut_semaine =
      as.Date("2013-12-30") + 7 * (numero_semaine - 1)
  )

# ------------------------------------------------------------
# 4. Contrôles
# ------------------------------------------------------------

cat("Nombre de lignes :", nrow(backer_hebdo), "\n")
cat("Nombre de semaines :", n_distinct(backer_hebdo$numero_semaine), "\n")

cat(
  "Première date :",
  as.character(min(backer_hebdo$date_debut_semaine)),
  "\n"
)

cat(
  "Dernière date :",
  as.character(max(backer_hebdo$date_debut_semaine)),
  "\n"
)

cat(
  "Valeurs manquantes dans cas_semaine :",
  sum(is.na(backer_hebdo$cas_semaine)),
  "\n"
)

cat(
  "Valeurs négatives :",
  sum(backer_hebdo$cas_semaine < 0, na.rm = TRUE),
  "\n"
)

# ------------------------------------------------------------
# 5. Contrôle des totaux de cas par pays
# ------------------------------------------------------------

controle_totaux_pays <- backer_hebdo %>%
  group_by(country) %>%
  summarise(
    total_cas = sum(cas_semaine),
    .groups = "drop"
  )

print(controle_totaux_pays)

# ------------------------------------------------------------
# 6. Agrégation des cas Ebola au niveau mensuel
# ------------------------------------------------------------

backer_mensuel <- backer_hebdo %>%
  mutate(
    unite_backer = prefecture,
    population = pop.size * 1000,
    mois_date = floor_date(date_debut_semaine, unit = "month")
  ) %>%
  group_by(
    country,
    unite_backer,
    mois_date
  ) %>%
  summarise(
    cas_mois = sum(cas_semaine),
    population = first(population),
    .groups = "drop"
  ) %>%
  mutate(
    cas_mois_pour_10000 = (cas_mois / population) * 10000,
    annee = year(mois_date),
    mois = month(mois_date),
    annee_mois = format(mois_date, "%Y-%m")
  )

# ------------------------------------------------------------
# 7. Contrôles
# ------------------------------------------------------------

cat("Nombre de lignes :", nrow(backer_mensuel), "\n")
cat("Nombre de mois :", n_distinct(backer_mensuel$mois_date), "\n")

cat(
  "Première date mensuelle :",
  as.character(min(backer_mensuel$mois_date)),
  "\n"
)

cat(
  "Dernière date mensuelle :",
  as.character(max(backer_mensuel$mois_date)),
  "\n"
)

cat(
  "Populations manquantes :",
  sum(is.na(backer_mensuel$population)),
  "\n"
)

cat(
  "Populations <= 0 :",
  sum(backer_mensuel$population <= 0, na.rm = TRUE),
  "\n"
)
# ------------------------------------------------------------
# 8. Contrôle final des totaux après agrégation mensuelle
# ------------------------------------------------------------

controle_totaux_mensuels <- backer_mensuel %>%
  group_by(country) %>%
  summarise(
    total_cas = sum(cas_mois),
    .groups = "drop"
  )

print(controle_totaux_mensuels)

# ------------------------------------------------------------
# 9. Harmonisation des noms de pays
# ------------------------------------------------------------

# ------------------------------------------------------------
# 9. Harmonisation des noms de pays
# ------------------------------------------------------------

backer_hebdo <- backer_hebdo %>%
  mutate(
    pays = case_when(
      toupper(country) == "GUINEA" ~ "Guinée",
      toupper(country) == "LIBERIA" ~ "Liberia",
      toupper(country) == "SIERRA LEONE" ~ "Sierra Leone",
      TRUE ~ country
    )
  )

backer_mensuel <- backer_mensuel %>%
  mutate(
    pays = case_when(
      toupper(country) == "GUINEA" ~ "Guinée",
      toupper(country) == "LIBERIA" ~ "Liberia",
      toupper(country) == "SIERRA LEONE" ~ "Sierra Leone",
      TRUE ~ country
    )
  )
# ------------------------------------------------------------
# 10. Sauvegarde des fichiers
# ------------------------------------------------------------

saveRDS(
  backer_hebdo,
  "data_intermediate/backer_hebdomadaire_3pays.rds"
)

saveRDS(
  backer_mensuel,
  "data_intermediate/backer_mensuel_3pays.rds"
)

write.csv(
  backer_mensuel,
  "outputs/tables/backer_mensuel_3pays.csv",
  row.names = FALSE
)

cat("Script 12 terminé.\n")
file.exists(
  "data_intermediate/backer_hebdomadaire_3pays.rds",
  "data_intermediate/backer_mensuel_3pays.rds",
  "outputs/tables/backer_mensuel_3pays.csv"
)