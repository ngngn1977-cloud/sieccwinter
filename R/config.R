# Configuration loading. The JSON file is the single source of truth for design, seed and tolerances.

#' Load the analysis configuration.
#' @param path Optional path to a configuration JSON file. Default: config/sieccwinter_config.json in the repository
#'   (or inst/extdata/sieccwinter_config.json in an installed package).
#' @export
siw_load_config <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root()
    cand <- c(file.path(root, "config", "sieccwinter_config.json"), file.path(root, "extdata", "sieccwinter_config.json"), file.path(root, "inst", "extdata", "sieccwinter_config.json"))
    path <- cand[file.exists(cand)][1L]
    if (is.na(path)) stop("SIW_CONFIG_NOT_FOUND", call. = FALSE)
  }
  cfg <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  cfg$tiers <- lapply(cfg$tiers, function(v) as.character(unlist(v)))
  cfg$outcomes <- as.data.frame(cfg$outcomes, stringsAsFactors = FALSE)
  cfg$metrics <- as.data.frame(cfg$metrics, stringsAsFactors = FALSE)
  cfg$windows <- as.integer(cfg$windows)
  cfg$representative_metrics <- cfg$metrics$metric[cfg$metrics$representative]
  cfg$config_path <- normalizePath(path, winslash = "/")
  cfg$config_hash <- siw_file_hash(path)$value
  cfg
}

#' Deterministic per-model seed: seed + 100 * metric_index + outcome_index (indices within the configured lists).
#' @export
siw_model_seed <- function(config, metric, outcome) {
  as.integer(config$seed + 100L * match(metric, config$metrics$metric) + match(outcome, config$outcomes$outcome))
}

siw_daily_names <- function(prefix, lag_end) paste0(prefix, "_L", sprintf("%02d", seq_len(lag_end)))
siw_cum_name <- function(prefix, window) paste0(prefix, window, "D")

#' Load the declared validation plan (which references are required, expected record counts, required columns).
#' Stored in config/validation_plan.json (inst/extdata in an installed package); kept separate from the analysis configuration.
#' @export
siw_load_validation_plan <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root()
    cand <- c(file.path(root, "config", "validation_plan.json"), file.path(root, "extdata", "validation_plan.json"), file.path(root, "inst", "extdata", "validation_plan.json"))
    path <- cand[file.exists(cand)][1L]
    if (is.na(path)) stop("SIW_VALIDATION_PLAN_NOT_FOUND", call. = FALSE)
  }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  p$plan_path <- normalizePath(path, winslash = "/"); p$plan_hash <- siw_file_hash(path)$value
  p
}
