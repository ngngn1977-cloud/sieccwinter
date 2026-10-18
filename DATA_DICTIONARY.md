# Data dictionary: canonical input CSV

This file describes the structure of the input only. It contains no study values. The shipped synthetic file `inst/extdata/synthetic_siecc_winter.csv` is a template with this exact structure (artificial codes `SYN001`…, invented values).

The authoritative machine-readable definitions are `siw_input_spec()`, `siw_daily_columns()` and `siw_cumulative_columns()` in `R/import.R`, plus `config/sieccwinter_config.json`.

**Format.** One row per participant. UTF-8 CSV with a header row. An empty cell means missing (`NA` and `NaN` are also read as missing). Rows are not sorted, filtered or imputed. SPSS files (`.sav`, `.zsav`, `.por`) are refused; convert them privately with `prepare_private_csv.ps1` first.

## 1. Scalar columns (22, all required)

| Column | Type | Allowed values | Role | Used as |
|---|---|---|---|---|
| `participant_code` | text | unique, non-blank | identifier | never written to any output |
| `UCLA_Lonliness_SCORE` | numeric | any finite | outcome | UCLA loneliness (Gaussian). The spelling is historical and kept. |
| `Isolation_cont` | numeric | any finite | outcome | objective isolation (Gaussian) |
| `integration_score` | numeric | 1, 2, 3, 4 | outcome | ordinal integration (Batch B ordinal models) |
| `int_dummy` | numeric | 0, 1 | outcome | high integration = score 4 (binomial; check V10) |
| `Sex` | numeric | 0, 1 | covariate | M1 and higher tiers |
| `Age` | numeric | any finite | covariate | years, as recorded; no participant is filtered by age |
| `assessment_day` | numeric | index | covariate | centred within the sample (`AssessmentDay_centered`) |
| `Area` | numeric | 1, 2, 3 | covariate | `Locality_3group` (three locality groups) |
| `Econ_STS` | numeric | 0, 1 | covariate | economic status |
| `Jewish`, `Muslim`, `Christian` | numeric | 0, 1 | covariate | `Ethnicity` = `Jewish` (consistency check V11) |
| `PSQI` | numeric | any finite | covariate | M3, M4-CC |
| `Subjective_Health` | numeric | 0–4 | covariate | M3 |
| `Memory` | numeric | 1–5 | covariate | M3 |
| `Memory_Changed` | numeric | 1, 2, 3 | covariate | M3 |
| `Subjective_Wellbeing` | numeric | 0–5 | covariate | M3 |
| `Outdoor_Mobility` | numeric | any finite | covariate | M4-CC |
| `PedestLength_m` | numeric | metres | covariate | converted to km (`PedestLength_km`) for M4-CC |
| `Memory_assess` | numeric | 0, 1 | covariate | M4-CC (consistency check V12) |
| `chronic_domain_count` | numeric | count | covariate | M4-CC |

## 2. Daily exposure columns (required)

Name pattern: `<PREFIX>_L01` … `<PREFIX>_L56`, where L01 is the day before assessment. The final analysis needs lags 1–56. The archived branch and the validations of the archived programme (archived DLM, daily layer, cold burden, ANOM/ALT) need lags 1–59.

| Metric (configuration name) | Daily prefix | Unit | Representative (final DLM, q90/q9) |
|---|---|---|---|
| UTCI | `UTCI` | °C | yes |
| MRT | `MRT` | °C | no |
| ERA5_air_temperature | `T2M` | °C | no |
| ERA5_relative_humidity | `RH` | % (0–100; check V09) | no |
| IMS_temperature | `IMS_IDW_TD_MEAN` | °C | no |
| IMS_relative_humidity | `IMS_IDW_RH_MEAN` | % | yes |
| IMS_rainfall_total | `IMS_IDW_RAIN_TOTAL` | mm (≥ 0; check V08) | no |
| IMS_rainy_day_count_ge_1_mm | `IMS_IDW_RAIN_DAYS1` | indicator 0/1 per day (check V08) | yes |

Total: 8 × 56 = 448 columns (472 with lags to 59).

**Rainfall missingness.** A missing rainfall total on a day whose rainy-day indicator is 0 is reported (check V14 and `rainfall_anomaly_report.csv`) and kept missing. Treating it as 0 mm is an optional, separately labelled sensitivity (`-RainfallSensitivity`); it is never applied silently.

## 3. Cumulative exposure columns (optional: supply all 80 or none)

Name pattern: `<CUMULATIVE PREFIX><window>D` for windows 1, 3, 7, 14, 21, 28, 35, 42, 49 and 56 days. Example: `UTCI_AVG_7D`. Prefixes: `UTCI_AVG_`, `MRT_AVG_`, `AIR_TEMP_AVG_`, `HUMIDITY_AVG_`, `IMS_TEMP_IDW_AVG_`, `IMS_RH_IDW_AVG_` (available-day means); `IMS_RAIN_IDW_TOTAL_`, `IMS_RAIN_DAYS_GE1MM_IDW_` (available-day sums).

If they are supplied, they are used as given and checked against a reconstruction from the daily fields (check V13, a report only). If they are absent, they are reconstructed. Partial supply fails (V13).

## 4. Derived analysis variables (built by `siw_prepare_data`; never written back)

`AssessmentDay_centered`, `Locality_3group` (factor; levels as in the configuration), `Ethnicity`, `Int_dummy`, `PedestLength_km`.

## 5. Model tiers (covariates per tier)

| Tier | Covariates |
|---|---|
| M0 | none |
| M1 | Sex, Age, AssessmentDay_centered |
| **M2-L (primary)** | M1 + Locality_3group + Econ_STS |
| M2-E | M1 + Ethnicity + Econ_STS |
| M2-EL | M1 + Locality_3group + Ethnicity + Econ_STS |
| M3 | M2-L + PSQI, Subjective_Health, Memory, Memory_Changed, Subjective_Wellbeing |
| M4-CC | M2-L + PSQI, Memory_assess, chronic_domain_count, Outdoor_Mobility, PedestLength_km |

Each model uses the complete cases of its outcome, exposure and tier covariates. The final DLMs also need a complete history: every lag L01–L56.

## 6. Input checks (`input_validation_report.csv`; nothing is corrected)

V00 structure; V01 required variables; V02 participant codes (missing or duplicated); V03 numeric parsing; V04_* allowed values; V05 daily columns present; V06 non-finite lag values; V07 all-missing lag columns (WARN); V08 rainfall validity; V09 relative-humidity range (WARN); V10 `int_dummy` vs `integration_score`; V11 ethnicity indicators (WARN); V12 `Memory_assess` vs `Memory` (WARN); V13 cumulative fields; V14 rainfall total missing with indicator 0 (WARN). A FAIL stops the run. Reports give counts and column names only, never row values.
