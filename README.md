# climate-impacts-legumes

Reproducible analysis for Chapter 1 of my PhD thesis: a systematic meta-analysis of crop model projections for legumes in Africa, with cereals and other crops as benchmarks.

## Data

Studies were identified through a PRISMA-compliant search of Web of Science and Scopus, cross-checked with the OpenAlex API (`Yield_Predictions/OpenAlex/`). Each record in the dataset is one crop model run: crop, site, climate scenario, time horizon, adaptation practice and projected yield change.

## Scripts

| Script | What it does |
|---|---|
| `r_scripts/adding_climate.R` | Extracts temperature and precipitation change (ΔT, ΔP) at each study site from WorldClim CMIP6 and NEX-GDDP-CMIP6 |
| `r_scripts/adding_spam.R` | Fills missing baseline yields from the nearest MapSPAM 2020 grid cell |
| `r_scripts/model.R` | Fits the GAMM of yield change on ΔT, ΔP, adaptation and CO₂ fertilisation, with random intercepts for study and country; exports `excel_data/gamm_supplementary_table.xlsx` |
| `r_scripts/bootstrapping.R` | Study-stratified bootstrap of quadratic yield–ΔT and yield–ΔP curves (after Challinor et al., 2014), adaptation effects, maps and protein-yield figures |
| `explorer/build_explorer.py` | Builds the interactive yield projections explorer page |

## Data flow

```
Literature search (WoS, Scopus, OpenAlex)
        ↓
yield_predictions_data.xlsx
        ↓
adding_climate.R, adding_spam.R  →  climate deltas and baseline yields added
        ↓
model.R          →  GAMM results
bootstrapping.R  →  figures in Yield-projections-legumes_files/
build_explorer.py →  Yield-projections_explorer.html
```

## Reports

- Legumes: https://namita-j.github.io/climate-impacts-legumes/Yield-projections_legumes.html
- Other crops: https://namita-j.github.io/climate-impacts-legumes/Yield-projections_other-crops.html
- Yield projections explorer: https://namita-j.github.io/climate-impacts-legumes/Yield-projections_explorer.html
