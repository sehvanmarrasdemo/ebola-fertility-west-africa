# Script-by-script workflow

This index describes the existing scripts, preserving their original research logic and order. The scripts themselves currently contain mostly French comments.

| Script | Purpose |
| --- | --- |
| `00_inventaire_donnees_brutes.R` | Inventory available DHS source files without modifying them |
| `01_import_verification_dhs.R` | Import and validate DHS individual and birth histories |
| `02_definition_fenetres_temporelles.R` | Define country-specific observation windows |
| `03_harmonisation_dhs.R` | Harmonise women and birth records |
| `04_controle_historique_naissances.R` | Validate retrospective birth histories |
| `05_preparation_evenements_naissance.R` | Prepare birth events using approximate conception months |
| `06_construction_base_femme_mois.R` | Construct the woman-month panel |
| `07_controle_gps_dhs.R` | Inspect DHS GPS clusters and invalid coordinates |
| `08_preparation_geographie_backer.R` | Prepare subnational Ebola reporting units |
| `09_crosswalk_dhs_backer.R` | Build an initial DHS–Backer geographic crosswalk |
| `10_preparation_polygones_historiques.R` | Harmonise historical administrative polygons |
| `11_jointure_spatiale_clusters.R` | Complete spatial cluster assignment |
| `12_preparation_temporelle_backer.R` | Convert weekly Ebola case observations to monthly exposure |
| `13_appariement_femme_mois_ebola.R` | Merge geographic and temporal Ebola exposure with woman-month records |
| `14_construction_taux_fecundite_mensuels.R` | Construct weighted monthly fertility rates |

## Documented validation results (from the thesis workflow)

The earlier research documentation records 34,513 women, 2,560,074 woman-month observations, 1,302 DHS clusters, and 33,875 aggregated age-area-month analytical cells. These are previously documented processing checks; they have **not** been independently reproduced from the code-only package.

Important safeguards include preserving multiple births, separating unobserved epidemic exposure from observed zero cases, and not inventing missing Backer observations for Mandiana or the post-May-2015 period.

## Execution note

Run from the repository root in numeric order, with input data present and the local output folders created. Generated files are deliberately excluded from Git. Use the original French documentation and the script comments for detailed methodological decisions.
