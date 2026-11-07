# ============================================================
# THESIS - Ebola and fertility in West Africa
# Script : 08_preparation_geographie_backer.R
#
# Objective:
# Prepare geographic areas used in the
# Backer data and compare them with information
# administrative fields available in DHS GPS files.
#
# Countries:
# - Guinea
# - Liberia
# - Sierra Leone
#
# Inputs:
# - data_raw/backer/countryeboladata.txt
# - data_intermediate/gps_clusters_dhs_harmonises.rds
#
# Expected outputs:
# - data_intermediate/backer_unites_3pays.rds
# - outputs/tables/backer_unites_3pays.csv
#
# Important:
# No final spatial join is performed here.
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Define paths
# ------------------------------------------------------------

path_backer <-
  "data_raw/backer/countryeboladata.txt"

path_gps <-
  "data_intermediate/gps_clusters_dhs_harmonises.rds"


# ------------------------------------------------------------
# 3. Check file presence
# ------------------------------------------------------------

stopifnot(
  file.exists(path_backer),
  file.exists(path_gps)
)


# ------------------------------------------------------------
# 4. Import Backer file
# ------------------------------------------------------------

backer_raw <- read.delim(
  path_backer,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# If the file was not recognised as tab-separated,
# try reading it as space-separated data
if (ncol(backer_raw) == 1) {
  
  backer_raw <- read.table(
    path_backer,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


# ------------------------------------------------------------
# 5. Check dimensions
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DIMENSIONS DU FICHIER BACKER\n")
cat("========================================\n")

cat(
  "Nombre de lignes :",
  nrow(backer_raw),
  "\n"
)

cat(
  "Nombre de variables :",
  ncol(backer_raw),
  "\n"
)

print(names(backer_raw))
# ------------------------------------------------------------
# 6. Check required variables
# ------------------------------------------------------------

variables_attendues <- c(
  "country",
  "prefecture",
  "city",
  "pop.size",
  "lat",
  "lon"
)

variables_absentes <-
  variables_attendues[
    !variables_attendues %in% names(backer_raw)
  ]

cat(
  "\nVariables indispensables absentes :\n"
)

print(variables_absentes)
# ------------------------------------------------------------
# 7. Identify weekly variables W1 through W74
# ------------------------------------------------------------

variables_semaines <- names(backer_raw)[
  grepl("^W[0-9]+$", names(backer_raw))
]


# Put them back into numerical order
numero_semaine <- as.numeric(
  sub("W", "", variables_semaines)
)

variables_semaines <-
  variables_semaines[
    order(numero_semaine)
  ]


cat(
  "\nNombre de variables hebdomadaires :",
  length(variables_semaines),
  "\n"
)

cat(
  "Première variable :",
  first(variables_semaines),
  "\n"
)

cat(
  "Dernière variable :",
  last(variables_semaines),
  "\n"
)
# ------------------------------------------------------------
# 8. Harmonise country names
# ------------------------------------------------------------

backer_raw <- backer_raw %>%
  mutate(
    
    pays = case_when(
      
      grepl(
        "GUINEA",
        country,
        ignore.case = TRUE
      ) ~ "Guinée",
      
      grepl(
        "LIBERIA",
        country,
        ignore.case = TRUE
      ) ~ "Liberia",
      
      grepl(
        "SIERRA",
        country,
        ignore.case = TRUE
      ) ~ "Sierra Leone",
      
      TRUE ~ NA_character_
    )
  )


# Verify
table(
  backer_raw$country,
  backer_raw$pays,
  useNA = "ifany"
)
# ------------------------------------------------------------
# 9. Create a dataset with one row per Backer area
# ------------------------------------------------------------

backer_unites <- backer_raw %>%
  mutate(
    
    # Population in Backer is expressed in thousands
    population =
      as.numeric(pop.size) * 1000,
    
    # Raw total cases observed across W1–W74
    total_cases_raw =
      rowSums(
        across(
          all_of(variables_semaines)
        ),
        na.rm = TRUE
      )
    
  ) %>%
  
  transmute(
    
    pays,
    
    # The generic name "prefecture" is used in
    # the Backer file for areas in all three countries
    unite_backer =
      as.character(prefecture),
    
    ville_backer =
      as.character(city),
    
    population,
    
    latitude_backer =
      as.numeric(lat),
    
    longitude_backer =
      as.numeric(lon),
    
    total_cases_raw
  )
# ------------------------------------------------------------
# 10. Number of Backer areas by country
# ------------------------------------------------------------

controle_unites_backer <- backer_unites %>%
  count(
    pays,
    name = "n_unites"
  )


cat("\n========================================\n")
cat("UNITES BACKER PAR PAYS\n")
cat("========================================\n")

print(controle_unites_backer)
# ------------------------------------------------------------
# 11. Check uniqueness of areas
# ------------------------------------------------------------

doublons_unites <- backer_unites %>%
  count(
    pays,
    unite_backer
  ) %>%
  filter(
    n > 1
  )


cat(
  "\nDoublons pays × unité Backer :",
  nrow(doublons_unites),
  "\n"
)
# ------------------------------------------------------------
# 12. Display Backer areas
# ------------------------------------------------------------

cat("\n========================================\n")
cat("UNITES BACKER - GUINEE\n")
cat("========================================\n")

backer_unites %>%
  filter(pays == "Guinée") %>%
  select(
    unite_backer,
    total_cases_raw
  ) %>%
  arrange(unite_backer) %>%
  print(n = Inf)


cat("\n========================================\n")
cat("UNITES BACKER - LIBERIA\n")
cat("========================================\n")

backer_unites %>%
  filter(pays == "Liberia") %>%
  select(
    unite_backer,
    total_cases_raw
  ) %>%
  arrange(unite_backer) %>%
  print(n = Inf)


cat("\n========================================\n")
cat("UNITES BACKER - SIERRA LEONE\n")
cat("========================================\n")

backer_unites %>%
  filter(pays == "Sierra Leone") %>%
  select(
    unite_backer,
    total_cases_raw
  ) %>%
  arrange(unite_backer) %>%
  print(n = Inf)

print(
  tibble::as_tibble(
    backer_unites %>%
      filter(pays == "Liberia") %>%
      select(unite_backer, total_cases_raw) %>%
      arrange(unite_backer)
  ),
  n = Inf
)
# ------------------------------------------------------------
# 13. Function to normalise geographic names
# ------------------------------------------------------------

normaliser_nom <- function(x) {
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x <- tolower(x)
  
  x <- gsub(
    "[^a-z0-9]",
    "",
    x
  )
  
  x
}
# ------------------------------------------------------------
# 15. Compare Liberia DHS and Backer
# ------------------------------------------------------------

liberia_dhs_admin <- gps_clusters %>%
  filter(
    pays == "Liberia"
  ) %>%
  distinct(
    adm1_name
  ) %>%
  mutate(
    nom_normalise =
      normaliser_nom(adm1_name)
  )


liberia_backer <- backer_unites %>%
  filter(
    pays == "Liberia"
  ) %>%
  distinct(
    unite_backer
  ) %>%
  mutate(
    nom_normalise =
      normaliser_nom(unite_backer)
  )


comparaison_liberia <- full_join(
  
  liberia_dhs_admin,
  liberia_backer,
  
  by = "nom_normalise"
)


cat("\n========================================\n")
cat("COMPARAISON LIBERIA DHS / BACKER\n")
cat("========================================\n")

print(comparaison_liberia)

# ------------------------------------------------------------
# 16. Compare Sierra Leone DHS and Backer
# ------------------------------------------------------------

sl_dhs_admin <- gps_clusters %>%
  filter(
    pays == "Sierra Leone"
  ) %>%
  distinct(
    dhs_region
  ) %>%
  mutate(
    nom_normalise =
      normaliser_nom(dhs_region)
  )


sl_backer <- backer_unites %>%
  filter(
    pays == "Sierra Leone"
  ) %>%
  distinct(
    unite_backer
  ) %>%
  mutate(
    nom_normalise =
      normaliser_nom(unite_backer)
  )


comparaison_sl <- full_join(
  
  sl_dhs_admin,
  sl_backer,
  
  by = "nom_normalise"
)


cat("\n========================================\n")
cat("COMPARAISON SIERRA LEONE DHS / BACKER\n")
cat("========================================\n")

print(comparaison_sl)

# ------------------------------------------------------------
# 17. Save Backer areas
# ------------------------------------------------------------

saveRDS(
  backer_unites,
  "data_intermediate/backer_unites_3pays.rds"
)

write.csv(
  backer_unites,
  "outputs/tables/backer_unites_3pays.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 18. End
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PREPARATION GEOGRAPHIQUE BACKER TERMINEE\n")
cat("========================================\n")

cat(
  "Nombre total d'unités Backer :",
  nrow(backer_unites),
  "\n"
)