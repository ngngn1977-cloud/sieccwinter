# Shared test setup (used by testthat and by the base-R shim in tests/testthat.R). Works from a source checkout or an installed package.
if (!exists("siw_load_config", mode = "function")) {
  root <- getOption("sieccwinter.root")
  if (is.null(root)) root <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/")
  source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
}
ROOT <- siw_root()
CFG <- siw_load_config()
SYN <- siw_make_synthetic(config = CFG)                 # raw canonical synthetic data frame
.cand <- file.path(ROOT, c("inst/extdata", "extdata"), "synthetic_siecc_winter.csv")
SYN_CSV <- .cand[file.exists(.cand)][1L]
prep <- function(raw = SYN) { d <- siw_prepare_data(raw, CFG); siw_attach_cumulative(d, CFG)$data }
DAT <- prep()
# Source-audit tests read R/, config/, scripts/, validation/ and run_*.ps1, which an installed package (R CMD check) does not contain.
# There they are recorded as SKIPPED (source checkout only), never counted as passed; in a source checkout they run unchanged.
SIW_SOURCE_CHECKOUT <- all(dir.exists(file.path(ROOT, c("R", "config", "scripts", "validation"))))
SIW_SOURCE_ONLY_SKIPS <- new.env(); SIW_SOURCE_ONLY_SKIPS$items <- character()
siw_skip_source_only <- function(label) {
  if (SIW_SOURCE_CHECKOUT) return(FALSE)
  SIW_SOURCE_ONLY_SKIPS$items <- c(SIW_SOURCE_ONLY_SKIPS$items, label); message("SKIPPED (source checkout only): ", label); TRUE
}
# Declared hashes of the full (private validation kit) declarations and of frozen reference files live in private_constants.json, which the public tarball
# excludes. In a public build, or without that file, every check of such a value is recorded as SKIPPED (private validation kit only), never counted as passed.
SIW_PUBLIC_BUILD <- siw_public_build()
SIW_PRIVATE_CONSTANTS <- if (!SIW_PUBLIC_BUILD && file.exists("private_constants.json")) jsonlite::fromJSON("private_constants.json", simplifyVector = TRUE) else NULL
siw_skip_private <- function(label) {
  if (!is.null(SIW_PRIVATE_CONSTANTS)) return(FALSE)
  SIW_SOURCE_ONLY_SKIPS$items <- c(SIW_SOURCE_ONLY_SKIPS$items, paste0(label, " [private validation kit only]")); message("SKIPPED (private validation kit only): ", label); TRUE
}
siw_skip_public <- function(label) {                                                                 # a check of a declared value that the public build redacts
  if (!SIW_PUBLIC_BUILD) return(FALSE)
  SIW_SOURCE_ONLY_SKIPS$items <- c(SIW_SOURCE_ONLY_SKIPS$items, paste0(label, " [redacted in the public build; private validation kit only]")); message("SKIPPED (redacted in the public build): ", label); TRUE
}
expect_private <- function(actual, key) { if (siw_skip_private(paste0("declared value ", key))) return(invisible(TRUE)); expect_identical(actual, SIW_PRIVATE_CONSTANTS[[key]]) }
