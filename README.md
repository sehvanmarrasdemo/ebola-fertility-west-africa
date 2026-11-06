# Ebola epidemic and fertility dynamics in West Africa

**R code portfolio — work in progress (Master's thesis in Demography)**

This repository documents the data-processing workflow for a research project on fertility dynamics before, during, and after the 2014–2015 Ebola epidemic in Guinea, Liberia, and Sierra Leone. It links retrospective birth histories from post-Ebola Demographic and Health Surveys (DHS) with historical subnational Ebola incidence data, using geographic harmonisation and monthly fertility-rate construction.

**This is a methodological code portfolio, not a published dataset or a completed causal analysis.** The scripts have been organised from the researcher's working copies. They have not been rerun in this distribution because the restricted-access DHS input data are not provided.

## Research workflow

| Stage | Scripts | Main task |
| --- | --- | --- |
| Input checks and DHS preparation | `00`–`07` | Inventory, validate, harmonise birth histories, and build woman-month records (ages 15–39) |
| Geographic harmonisation | `08`–`11` | Link DHS survey clusters to historical Ebola reporting areas |
| Ebola exposure | `12`–`13` | Reshape weekly incidence to monthly records and join it to woman-month histories |
| Fertility indicators | `14` | Calculate weighted monthly birth rates by country, geographic area, and five-year age group |

The script filenames are numbered in execution order in [`scripts/`](scripts/). A script-by-script guide is provided in [`documentation/WORKFLOW.md`](documentation/WORKFLOW.md).

## Sources and analytical choices

- DHS post-Ebola surveys: Guinea 2018; Liberia 2019–2020; Sierra Leone 2019.
- Historical epidemic series: Backer et al. subnational weekly data (`W1`–`W74`, December 2013–May 2015).
- Historical administrative boundaries are used to align survey locations with epidemic reporting areas.
- The analysis reconstructs approximate conception timing as birth month minus nine months. This is an approximation, not an observed conception date.
- Women enter the woman-month data when aged 15–39 in the historical month; DHS sampling weights are incorporated into the aggregated rates.
- Missing Ebola exposure is **not** automatically interpreted as zero incidence. In particular, months after May 2015 are outside the available weekly incidence series.

## How to use the code

1. Install R and the required packages: `dplyr`, `tidyr`, `haven`, `sf`, `readr`, and `lubridate`.
2. Open R/RStudio with the repository root as the working directory. The scripts use project-relative paths and should **not** be run from inside `scripts/`.
3. Obtain the source datasets independently and lawfully, respecting each provider's access conditions. Recreate the expected input paths described in [`documentation/DATA_ACCESS.md`](documentation/DATA_ACCESS.md).
4. Create local folders `data_intermediate/`, `data_clean/`, and `outputs/tables/` before execution.
5. Run scripts `00` through `14` in numeric order. The scripts were designed as separate research stages; no automatic end-to-end run is claimed here.

No DHS survey records, cluster coordinates, participant-level derived datasets, or analytical exports are included. Some public boundary and Ebola data may also require separate retrieval. The repository's `.gitignore` excludes the entire data and outputs directories by default.

## Interpretation and limitations

The constructed outcomes represent retrospective, realised fertility rates rather than directly observed conceptions. Survey cluster GPS coordinates are displaced for confidentiality and capture residence at interview rather than necessarily at the epidemic date. Geographic exposure misclassification is therefore possible. The code describes a data-processing pipeline; it does not by itself establish a causal effect of Ebola on fertility.

## Status

Pipeline documented through monthly weighted fertility-rate construction (`14`). Descriptive analysis, model estimation, and sensitivity checks are separate ongoing thesis work and are not presented as completed here.

## Attribution

Research code prepared by Sehvan Marras as part of a Master's thesis in Demography. DHS data remain governed by the DHS Program's terms of use; third-party incidence and geographic datasets remain subject to their respective licences and citation requirements.
