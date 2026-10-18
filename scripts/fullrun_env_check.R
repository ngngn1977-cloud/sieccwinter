# Preflight of run_all.ps1, in a fresh R process: R version, required packages, configuration hash, package source fingerprint and the SHA-256 of the input file.
# The input file is only hashed; no value is read or printed. Prints KEY=VALUE lines; exit 0 when ENVIRONMENT_STATUS=PASS, 1 otherwise.
# Usage: Rscript scripts/fullrun_env_check.R <package_root> <input_csv> <expected_input_sha256>
a <- commandArgs(trailingOnly = TRUE)
if (length(a) != 3L) { cat("usage: Rscript scripts/fullrun_env_check.R <package_root> <input_csv> <expected_input_sha256>\n"); quit(status = 2L) }
root <- normalizePath(a[1L], winslash = "/", mustWork = TRUE)
EXPECTED_R <- "4.4.3"
EXPECTED_CONFIG_SHA <- "5A8C605469AE0AA8133215E8E1666182BEAE17F92F01A00CD0BDE0612E3DC5DE"
out <- function(k, v) cat(k, "=", paste(as.character(v), collapse = ";"), "\n", sep = "")
ok <- TRUE
rv <- paste(R.version$major, R.version$minor, sep = "."); rv_ok <- identical(rv, EXPECTED_R)
out("R_VERSION", rv); out("R_VERSION_REQUIRED", EXPECTED_R); out("R_VERSION_OK", rv_ok); ok <- ok && rv_ok
out("R_PLATFORM", R.version$platform)
req <- list(MASS = "7.3-64", jsonlite = "2.0.0", splines = NA, stats = NA, tools = NA, utils = NA)
for (p in names(req)) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  good <- !is.na(v) && (is.na(req[[p]]) || utils::compareVersion(v, req[[p]]) >= 0)
  out(paste0("IMPORT_", p), if (is.na(v)) "NOT_INSTALLED" else v); out(paste0("IMPORT_", p, "_OK"), good); ok <- ok && good
}
for (p in c("testthat", "numDeriv", "dlnm")) {
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NOT_INSTALLED")
  out(paste0("SUGGESTS_", p), paste0(v, if (identical(v, "NOT_INSTALLED")) " (optional; not used by the manual run)" else ""))
}
loaded <- tryCatch({ source(file.path(root, "scripts", "load_package.R")); suppressMessages(siw_dev_load(root)); TRUE }, error = function(e) { out("PACKAGE_LOAD_ERROR", conditionMessage(e)); FALSE })
out("PACKAGE_LOAD_OK", loaded); ok <- ok && loaded
if (loaded) {
  cfg <- siw_load_config(); ch <- toupper(cfg$config_hash); cok <- identical(ch, EXPECTED_CONFIG_SHA)
  out("CONFIG_SHA256", ch); out("CONFIG_SHA256_EXPECTED", EXPECTED_CONFIG_SHA); out("CONFIG_OK", cok); ok <- ok && cok
  out("PACKAGE_VERSION", siw_pkg_version())
  sm <- siw_source_manifest(root); out("SOURCE_FINGERPRINT", toupper(sm$fingerprint)); out("SOURCE_FILES", sm$n_files)
  out("ARCHIVED_GLM_EPSILON", siw_archived_glm_control(cfg)$glm_epsilon); out("FINAL_DLM_GLM_EPSILON", cfg$dlm$final$glm_epsilon)
}
h <- tryCatch(siw_file_hash(a[2L]), error = function(e) list(algorithm = "none", value = ""))
hok <- identical(h$algorithm, "sha256") && identical(toupper(h$value), toupper(a[3L]))
out("INPUT_HASH_ALGORITHM", h$algorithm); out("INPUT_SHA256", toupper(h$value)); out("INPUT_SHA256_EXPECTED", toupper(a[3L])); out("INPUT_SHA256_OK", hok); ok <- ok && hok
out("ENVIRONMENT_STATUS", if (ok) "PASS" else "FAIL")
quit(status = if (ok) 0L else 1L)
