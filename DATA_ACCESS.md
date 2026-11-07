# Data access and privacy

**Do not upload DHS microdata, GPS cluster files, or respondent-level derived data to a public GitHub repository.** The repository is deliberately code-only.

The scripts expect the following local data inputs. Paths are relative to the project root:

| Source | Expected path or folder |
| --- | --- |
| Guinea DHS IR and BR | `data_raw/dhs/GN_2018/IR/GNIR71DT/GNIR71FL.DTA` and `data_raw/dhs/GN_2018/BR/GNBR71DT/GNBR71FL.DTA` |
| Liberia DHS IR and BR | `data_raw/dhs/LIB_2019_2020/IR/LBIR7ADT/LBIR7AFL.DTA` and `data_raw/dhs/LIB_2019_2020/BR/LBBR7ADT/LBBR7AFL.DTA` |
| Sierra Leone DHS IR and BR | `data_raw/dhs/SL_2019/IR/SLIR7ADT/SLIR7AFL.DTA` and `data_raw/dhs/SL_2019/BR/SLBR7ADT/SLBR7AFL.DTA` |
| Guinea DHS GPS | `data_raw/dhs/GN_2018/GPS/GNGE71FL/GNGE71FL.shp` |
| Liberia DHS GPS | `data_raw/dhs/LIB_2019_2020/GPS/LBGE7AFL/LBGE7AFL.shp` |
| Sierra Leone DHS GPS | `data_raw/dhs/SL_2019/GPS/SLGE7AFL/SLGE7AFL.shp` |
| Backer Ebola weekly series | `data_raw/backer/countryeboladata.txt` |
| Historical boundary shapefiles | Within `data_raw/geographie/GN_2015_ADM2/` and `data_raw/geographie/SL_2015_ADM2/` |

The geographic input folders must contain the shapefile components (`.shp`, `.shx`, `.dbf`, `.prj`, etc.) used by `sf`.

## Publication checklist

- [ ] Confirm permission to publish any third-party code and documentation.
- [ ] Keep all DHS IR, BR and GPS/GE files outside Git version control.
- [ ] Keep woman-month records, crosswalks, and individual/cluster-linked exports private.
- [ ] Review tables and figures for disclosure risks before selectively publishing them.
- [ ] Verify no secrets, local personal details, or dataset contents appear in any commit history.
- [ ] Check dataset citation requirements and institutional/supervisor expectations.

`.gitignore` is a preventive measure, not a substitute for reviewing files and Git history before publishing.
