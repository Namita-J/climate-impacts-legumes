# climate-impacts-legumes

This repository contains the code and data associated with **Chapter 1 of my PhD thesis**: a systematic meta-analysis of crop model projections for legumes in Africa.

Work conducted under the WUR–Alliance Bioversity-CIAT collaborative agreement (Deliverables 1.1–1.4). Manuscript in revision; target submission to *Global Environmental Change* (August 2026).

---

## Overview

A systematic review of the published crop simulation literature for legumes in Africa was conducted following PRISMA guidelines. Searches of Web of Science and Scopus identified 8,197 records, screened down to **35 legume studies** and **65 cereal benchmark studies**. The final dataset comprises **658 individual model runs** across 10 legume species, 22 African countries, and multiple crop modelling frameworks (DSSAT, APSIM, SARRA-H).

Climate deltas (ΔT, ΔP) were extracted from the **NEX-GDDP-CMIP6** dataset (35 GCMs) and harmonised to a common SSP framework.

---

## Key Analyses

- **Bootstrap polynomial regression** — yield response to ΔT and ΔP, with adaptation as an interaction term; 1,000 study-stratified bootstrap replicates (Challinor et al., 2014 approach)
- **GAMM** — fitted to all legume records (n = 469, 25 studies, 18 countries); temperature as parametric fixed effect, precipitation as penalised spline; random intercepts for study and country
  - Adaptation effect: β = 12.84 (p < 0.001)
  - Temperature effect: β = −4.31 (p = 0.006)
  - Conditional R² = 0.820; country-level random effects explained 53% of variance
- **Protein yield analysis** — species-level digestible protein yield projections for 15 studies reporting absolute yields

---

## Visualisations

- Yield projections (other crops): https://namita-j.github.io/climate-impacts-legumes/Yield-projections_other-crops.html
- Yield projections (legumes): https://namita-j.github.io/climate-impacts-legumes/Yield-projections_legumes.html

---

## Repository Structure

```
Yield Predictions/    # Chapter 1 analysis scripts and outputs
```

---

## Status

| Deliverable | Description | Status |
|---|---|---|
| 1.1 | Dataset documentation & harmonisation | Complete |
| 1.2 | Statistical refinement for publication | Complete |
| 1.3 | Manuscript preparation | Complete |
| 1.3 | Submission to *Global Environmental Change* | In progress (Aug 2026) |
| 1.4 | Reproducible scripts | Complete |
| 1.4 | Dataset archiving | Pending (Sep 2026) |
