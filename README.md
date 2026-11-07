# Ebola and Fertility Dynamics in West Africa

**Sehvan Marras** · Master's research in Demography · UCLouvain / Université de Montréal

This repository contains the R scripts developed for my Master's thesis on fertility dynamics surrounding the 2014–2015 Ebola epidemic in Guinea, Liberia, and Sierra Leone.

The project combines Demographic and Health Surveys (DHS), local Ebola incidence data, and historical administrative boundaries to study fertility patterns before, during, and after the epidemic.

## Research question

How did fertility patterns evolve around the Ebola epidemic, and to what extent did these patterns differ across areas experiencing different levels of epidemic intensity?

## Data and workflow

The pipeline uses post-Ebola DHS surveys from Guinea (2018), Liberia (2019–2020), and Sierra Leone (2019), alongside weekly subnational Ebola incidence data from Backer and collaborators (December 2013–May 2015).

The scripts prepare the survey records, reconstruct woman-month histories, match DHS clusters to historical Ebola reporting areas, and calculate weighted monthly fertility rates.

| Scripts | Stage |
| --- | --- |
| `00`–`04` | Input inventory and DHS harmonisation |
| `05`–`06` | Birth events and woman-month histories for ages 15–39 |
| `07`–`11` | Geographic harmonisation between DHS clusters and Ebola reporting areas |
| `12`–`13` | Construction and linkage of monthly Ebola exposure |
| `14` | Weighted monthly fertility rates by country, area, and age group |

## Repository structure

```text
.
├── README.md
├── .gitignore
├── scripts/
│   ├── 00_inventaire_donnees_brutes.R
│   ├── 01_import_verification_dhs.R
│   ├── ...
│   └── 14_construction_taux_fecundite_mensuels.R
└── documentation/
    ├── WORKFLOW.md
    └── DATA_ACCESS.md
```

The 15 R scripts follow the processing sequence. Comments are in English; original variable names and dataset references are retained.

See [WORKFLOW.md](documentation/WORKFLOW.md) for details on each script.

## Methodological notes

- Women contribute observations for months in which they are aged 15–39, within each survey's observation window.
- Approximate conception month is defined as birth month minus nine months.
- Monthly fertility rates count live births, including multiple births, and use DHS sampling weights in both the numerator and denominator.
- Geographic matching accounts for historical administrative changes. DHS cluster coordinates are displaced and refer to residence at the survey date, which may differ from residence during the epidemic.
- Zero reported Ebola cases are distinguished from missing exposure data; months beyond the source's coverage are not automatically treated as zero.

## Running the scripts

Run the scripts from the repository root in numerical order. Main R packages: `dplyr`, `tidyr`, `haven`, `sf`, `readr`, and `lubridate`.

Input data must be obtained separately, subject to the relevant access conditions. See [DATA_ACCESS.md](documentation/DATA_ACCESS.md) for expected files and locations. DHS microdata, cluster coordinates, and individual-level outputs are not included in this repository.

Local data and output folders (`data_raw/`, `data_intermediate/`, `data_clean/`, and `outputs/`) are excluded from version control.

## Project status

The pipeline currently extends through the construction of weighted monthly fertility rates (Script 14). Descriptive analyses, statistical modelling, and robustness checks are the next stages of the thesis.
