# Dependency gaps (nothing was installed)

R 4.4.3. Required (Imports), all installed and sufficient: MASS 7.3-65, jsonlite 2.0.0, splines, stats, tools, utils.
The core workflow (`run.R demo|paper|validate`, the `.ps1` scripts) needs nothing else.

Declared Suggests (Phase 2A.1 removed the unused `tsModel`, `digest`, `sandwich`, `logistf`):

| Package | Status | Effect of absence | Substitute used |
|---|---|---|---|
| testthat (>= 3.2.0) | not installed | standard `test_check()` cannot run | `tests/testthat.R` falls back to a base-R shim (`test_that`, `expect_*`) running the same test files |
| dlnm (>= 2.4.7) | not installed | `validation/legacy_equivalence_synthetic.R` (legacy comparison) cannot run in a clean environment | the package uses `splines::ns(1:L, df, intercept = TRUE)`, the basis dlnm builds for `arglag = list(fun = "ns", df = 3)`; the unit test checks it |

Not R packages: `tools::sha256sum` is unavailable in R 4.4.3, so `siw_file_hash()` uses `certutil` (Windows) or `sha256sum`, falling back to MD5 (algorithm is recorded in every manifest).

Not part of the package: Firth logistic regression (`logistf`) and external HC3 (`sandwich`) cross-checks belong to earlier audit phases; HC3 is verified here against a direct matrix calculation.
