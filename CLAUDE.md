# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project purpose

This repo investigates why the share of Spanish households reporting pet-related expenses in the INE "Encuesta de Presupuestos Familiares" (EPF, family budget survey) appeared to jump from ~26-28% (2022-2023) to ~54% in the raw 2024 microdata, despite pet spending itself only rising modestly. The working conclusion (see `Comparativa.md` and `INFORME_ANALISIS_DUPLICACION_MASCOTAS.md`) is that the increase is a methodology/coding artifact, not a real behavioral change — COICOP expense codes for pets were renumbered between years and 2024 captured many more near-zero / imputed expense records.

**2024 correction (Método 2, applied in the `reports/` Excel).** The fix that reproduces the historical trend is to compute the *real* spend `GASTO_real = GASTO * (1 - PORCENDES/100) * (1 - PORCENIMP/100)` and **discard every household whose real spend is 0** (i.e. expenses that come only from imputations/desgloses), while still reporting the original `GASTO` for the households that survive the filter (for homogeneity with prior years). An alternative that was tried and rejected (Método 1) was dropping households by a low-amount threshold (10-200 €): it barely moved the proportion. With Método 2 the updated 2024 reports drop to **~22,8% de hogares con mascotas** and **≈ 5.403 M€** of spend, in line with 2016-2023. The `reports/*.xlsm` `TablaMaestra` sheets already encode this filtered 2024, so `dashboard/prep_data.py` picks it up directly — no recomputation in the dashboard.

## Commands

```bash
# Python dashboard (interactive Streamlit app)
pip3 install -r requirements.txt
python3 dashboard/prep_data.py               # one-time: extract reports/*.xlsm -> dashboard/data/*.csv
./iniciar_dashboard.sh                        # runs prep_data.py if CSVs missing, then the dashboard
python3 -m streamlit run dashboard/app.py     # run directly; serves http://localhost:8501

# R analysis scripts (run from repo root, produce PDFs into Outputs/)
Rscript src/analisis_comparativa_mascotas.R   # main comparative analysis 2022-2024
Rscript src/analisis_umbral_gastos.R          # threshold sensitivity analysis, reads Outputs/resultado.csv

# Extract a given year's pet expenses from EPF microdata into Outputs/<year>/
Rscript src/ExtractResultFromMicrodataEpf.R Inputs/2024 Outputs/2024
Rscript src/ExtractResultFromMicrodataEpf.R Inputs/2025 Outputs/2025
```

There is no test suite, linter, or build step in this repo. To sanity-check the dashboard end-to-end,
drive it with Streamlit's `AppTest` (`from streamlit.testing.v1 import AppTest`) and assert no
exceptions. Sanity (with the Método-2-corrected 2024 reports): año foco 2024 → gasto ≈ 5.403 M€,
% hogares con mascotas ≈ 22,8%; año foco 2025 → gasto ≈ 5.840 M€, % ≈ 23,2%. Both now sit on the
2016-2023 trend (~26-36%), i.e. the raw-2024 spike is gone once the filter is applied.

## Data pipeline architecture

Data flows through three stages, each with its own file format and script:

1. **Raw INE microdata → R datasets** (`src/Convert txt file to r dataset.R`)
   Fixed-width `.txt` extracts from INE (`Inputs/<year>/`) are parsed with `read.fwf()` into a data frame (`ANOENC, NUMERO, CODIGO, GASTO, PORCENDES, PORCENIMP, ...`) and saved as `.rds`. Monetary/quantity fields are stored ×100 and `FACTOR` ×1e6 in the raw files, so they're divided back down during conversion.

2. **R datasets → filtered/joined Excel outputs** (`src/ExtractResultFromMicrodataEpf.R <input_dir> <output_dir>`)
   A single script parametrized by input/output directory (e.g. `Inputs/2025 Outputs/2025`), replacing the old per-year hardcoded scripts. Loads `<input_dir>/R/EPFgastos_*.RData` (expense microdata) and `EPFhogar_*.RData` (household microdata) — the variable saved inside can be named `Microdatos`/`Metadatos` (2024) or `df_micro`/`df_meta` (2025), both are handled. Filters expenses to pet-related COICOP codes (`09321`, `09322`, `09450`), computes `GastoEfectivo = GASTO * (1 - PORCENDES/100) * (1 - PORCENIMP/100)` (net of discounts/taxes) per INE guidance, joins to household attributes by `NUMERO`, and relabels coded categorical columns (CCAA, TAMANO, TIPHOGAR1, TIPHOGAR7, INTERIN, etc.) via `rename_using_epf_labels()`. Produces two outputs in `<output_dir>/`:
   - `EPFOnlyPetsResult.xlsx` — detail rows of pet expenses joined to household data
   - `EPFOnlyPetsProportionResult.xlsx` — one row per household, classified into `Clasificacion` (no pet / products / services / both) based on which expense codes are >0

   Note that the pet COICOP codes differ between 2024 (`09321`/`09322`/`09450`) and 2022-2023 (`09341`/`09342`/`09350`) — this recoding is the crux of the investigation (see `Comparativa.md` for the full code equivalence table and household totals per year).

3. **Combined data → analysis/visualization**
   - `GastosSoloMascotas22a24.csv` (merged 2022-2024 extract): feeds the R scripts `src/analisis_comparativa_mascotas.R` / `src/analisis_umbral_gastos.R` (R/ggplot statistical analysis, output PDFs to `Outputs/`) and the legacy Dash app `dashboard_mascotas.py` (kept for reference, no longer the primary dashboard).
   - **Streamlit dashboard** (`dashboard/`): the current dashboard. `dashboard/prep_data.py` reads the `TablaMaestra` sheet of `reports/2024/ComparativaMascotas16a24.xlsm` (gasto microdata, 1 row per household×COICOP code) and `reports/2024/ComparativaMascotas16a24Proporcion.xlsm` (1 row per household with `CLASIFICACION`: sin mascotas / Productos / Servicios / Productos y Servicios), **appends 2025** from `Outputs/2025/EPFOnlyPetsResult.xlsx` and `EPFOnlyPetsProportionResult.xlsx`, and writes the stacked CSVs `dashboard/data/gastos_16a25.csv` and `dashboard/data/proporcion_16a25.csv` (2016-2025, all variables). The 2025 Excel arrives with broken encoding (accents and `€` rendered as `?` at the byte level); `prep_data.py` repairs it by matching each value's byte-level "skeleton" against the correct 2016-2024 vocabulary (plus an override mapping INTERIN `0` → `Sin registro de ingresos`). `dashboard/app.py` loads those CSVs (`@st.cache_data`) and offers KPIs + sidebar filters (años, año foco/base, códigos, CCAA) and 5 tabs: Resumen, Análisis por variable (selector over TAMANO/TIPHOGAR1/TIPHOGAR7/INTERIN/CCAA/CAPROV/TAMAMU plus the crosses CCAA×CAPROV and CCAA×TAMAMU), Mapa de España, Comparativa temporal (heatmap/slope/diverging/small-multiples), and Datos.
     - **Important:** in these Excel tables `GASTO` is the **weighted national estimate in €** (sum ≈ 3.15 B€ in 2016 → ~5.4 B€ in 2024 after the Método-2 filter), not a per-household euro amount, and household counts are the unweighted sample (~19-22k/year). So a valid "gasto medio por hogar" cannot be derived without elevation factors — the dashboard intentionally reports only `gasto_total` and `pct_con_mascota`, never a per-household average.

When modifying analysis logic, keep in mind the year-over-year GASTO format inconsistency and the COICOP code remapping — both are load-bearing for every household/percentage calculation in this repo. The raw 2024 microdata showed `% hogares con mascotas` spiking to ~53,6% (the recoding artifact this project investigates); the `reports/` now ship the Método-2-filtered 2024 (~22,8%), so regenerating the CSVs with `prep_data.py` yields the corrected trend.
