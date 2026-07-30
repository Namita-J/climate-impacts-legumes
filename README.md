# climate-impacts-legumes

Reproducible analysis for Chapter 1 of my PhD thesis: a systematic meta-analysis of crop model projections for legumes in Africa. Manuscript in revision for *Global Environmental Change* (submission August 2026).

---

## Systematic Literature Review

A PRISMA-compliant search of Web of Science and Scopus identified **8,197 records**. After title/abstract and full-text screening, **35 legume-focused studies** and **65 cereal benchmark studies** were retained.

The OpenAlex API was also used to cross-validate the search. The script `Yield_Predictions/OpenAlex/OpenAlex_query.R` constructs a Boolean query combining climate, agriculture, and African region terms, executes it via the `openalexR` package, and saves results to `OpenAlex/search_data/`. `WoS_OpenAlex_test.R` cross-validates the OpenAlex hits against the manual WoS screening by matching on title and DOI, and plots relevance score distributions to identify a threshold for automated screening.

The final dataset comprises **658 individual model runs** across 10 legume species, 22 African countries, and three crop modelling frameworks (DSSAT/CROPGRO, APSIM, SARRA-H).

---

## Scripts

### `r_scripts/adding_climate.R` — Climate delta extraction

Extracts temperature and precipitation deltas (ΔT in °C; ΔP in mm and %) for each study site from two raster sources:

- **WorldClim 2.1 CMIP6** — maps each study's emissions scenario (including SRES → SSP equivalences: B1→SSP1-2.6, B2→SSP2-4.5, A1B→SSP3-7.0, A2→SSP5-8.5) and future time window to the appropriate GCM ensemble raster. Extracts future BIO1 (mean annual temperature) and BIO12 (annual precipitation) at study coordinates using `terra::extract()`, subtracts the historical baseline to compute deltas.
- **NEX-GDDP-CMIP6 (35 GCMs)** — a study-specific approach where the baseline is defined by each study's own reported baseline year. Loops over SSP × baseline-year × future-window combinations and computes ΔT and ΔP% from yearly ensemble mean rasters.

**Key packages:** `terra`, `readxl`, `writexl`, `dplyr`

---

### `r_scripts/adding_spam.R` — Baseline yield gap-filling

Fills missing baseline yield values (t/ha) using the **MapSPAM 2020** crop yield dataset for Sub-Saharan Africa (`excel_data/spam2020V1r2_SSA_Y_TL.csv`). For each study with a missing baseline, identifies the nearest MapSPAM grid cell by Haversine distance (`geosphere::distHaversine()`) and assigns that cell's yield. Only fills cells with non-zero MapSPAM values.

Covers six legume crops: Common Bean, Groundnut, Soybean, Chickpea, Cowpea, Lentil.

**Key packages:** `geosphere`, `readxl`, `dplyr`, `janitor`

---

### `r_scripts/model.R` — GAMM

Fits the main **Generalised Additive Mixed Model** for legume yield response (n = 469 runs, 25 studies, 18 countries).

A preliminary GAM with smooths on both ΔT and ΔP confirmed that temperature response is approximately linear (edf ≈ 3.25, p = 0.052), so ΔT enters as a parametric fixed effect while ΔP is modelled as a penalised regression spline `s(precip)`. CO₂ fertilisation status and adaptation (binary: No-adaptation vs Adaptation) are binary fixed effects. Random intercepts are specified for study and country.

```
dp ~ dt + s(precip) + adaptation + co2_group + (1|study_id) + (1|country)
```

A Shapiro-Wilk test confirmed non-normality in OLS residuals (W = 0.953, p < 0.001), supporting the GAMM specification. The full model showed ΔAIC = 60.9 vs the null; conditional R² = 0.820; country-level random effects explain 53% of total variance.

Also runs:
- Sensitivity analysis on CO₂-inclusive simulations only
- OLS comparison with clustered standard errors (HC3) and VIF diagnostics
- Exports fixed effects, smooth terms, and random effects variance to `excel_data/gamm_supplementary_table.xlsx`

**Key packages:** `mgcv`, `gamm4`, `lme4`, `broom.mixed`, `performance`, `sandwich`, `lmtest`

---

### `r_scripts/bootstrapping.R` — Bootstrap regression and figures

The main figure-generation script (~1,300 lines). All regressions follow the Challinor et al. (2014) approach: **second-degree polynomial regression** with adaptation as an interaction term, uncertainty quantified via study-stratified bootstrap replicates (500–1,000), with 95% confidence intervals from the 2.5th/97.5th percentiles.

Analysis blocks:

| Block | Description |
|---|---|
| Adaptation summary | Pivots adaptation practice columns to a per-study summary CSV |
| Geographic map | Africa choropleth of paper counts per country with site locations overlaid |
| Legumes ΔT bootstrap | Single-panel bootstrap: `dp ~ poly(dt, 2) * adaptation`, 500 replicates |
| Legumes vs Maize × CO₂ (ΔT) | 2×2 faceted bootstrap across crop type and CO₂ group, 1,000 replicates |
| Legumes vs Maize × CO₂ (ΔP) | Same 2×2 structure for precipitation (ΔP%) |
| Legumes ΔP absolute | Single-panel bootstrap for ΔP in mm from NEX-GDDP ensemble deltas |
| Crop model bar chart | Studies by crop model × warming level (binned ΔT) × adaptation strategy |
| Adaptation effect | Paired no-adaptation vs adaptation runs; bootstrap mean Δyield per practice type |
| Protein yield heatmap | Digestible protein change (t/ha) by species × time period, bootstrapped with uncertainty encoding |

Outlier removal uses a **3×IQR threshold** within each crop × CO₂ × adaptation subgroup (removing 2.6% of records; a 1.5×IQR threshold was tested but found to remove too many valid maize observations).

For the protein analysis, species-specific crude protein content and true digestibility factors are applied to absolute yield projections from the 15 studies reporting baseline and future t/ha values.

**Key packages:** `boot`, `ggplot2`, `patchwork`, `sf`, `rnaturalearth`, `biscale`, `purrr`, `tidyr`

---

### `Yield_Predictions/Yield projections_legumes.Rmd` — Rendered report

Self-contained R Markdown notebook that downloads the primary dataset from GitHub at render time and reproduces the full visualisation pipeline. Renders to the public HTML report.

- Legumes report: https://namita-j.github.io/climate-impacts-legumes/Yield-projections_legumes.html
- Other crops report: https://namita-j.github.io/climate-impacts-legumes/Yield-projections_other-crops.html

---

## Data Flow

```
OpenAlex_query.R + manual WoS screening
        ↓
yield_predictions_data.xlsx  (658 model runs, 10 legume species, 22 countries)
        ↓
adding_climate.R  →  ΔT and ΔP extracted from WorldClim / NEX-GDDP rasters
adding_spam.R     →  missing baseline yields filled from MapSPAM 2020
        ↓
yield_predictions_data_with_nextgddp_yearly_baseline_deltas.xlsx  (main model input)
        ↓
model.R          →  GAMM  →  gamm_supplementary_table.xlsx
bootstrapping.R  →  bootstrap figures  →  Yield-projections-legumes_files/
Rmd notebooks    →  HTML reports
```

---

## Repository Structure

```
r_scripts/               Main analysis scripts
excel_data/              Input/output datasets
Yield_Predictions/       R Markdown notebooks and OpenAlex scripts
OpenAlex/search_data/    Literature search outputs
Yield-projections-legumes_files/   Output figures
```
