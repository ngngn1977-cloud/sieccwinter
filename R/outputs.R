# Output schemas, deterministic writing, manifest, dependency report and run metadata.

.siw_meta_cols <- c("result_id", "outcome", "outcome_family", "metric", "exposure", "exposure_unit", "window_or_lag", "transformation", "model_tier", "primary_role", "status",
                    "not_estimable_reason", "separation_status", "convergence")
#' Fixed column order of every output table (the machine-readable schemas in output_schema/ are generated from these).
#' @export
.siw_core_output_columns <- function() {
  list(
    cumulative_results = c(.siw_meta_cols, .siw_cum_stats, "q10", "q80", "q90", "multiplicity_family_q10", "multiplicity_family_q80", "multiplicity_family_q90"),
    complete_case_accounting = c("result_id", "n_input", "n_complete", "n_excluded", "excluded_missing_outcome", "excluded_missing_exposure", "excluded_missing_covariate", "covariates_with_missing"),
    cumulative_design_audit = c("model_tier", "covariates", "design_columns", "n_columns_including_exposure", "factor_reference_levels", "n_rows_complete_covariates"),
    dlm_final_summary = c("result_id", "outcome", "outcome_family", "metric", "model_tier", "n", "event_count", "lag_start", "lag_end", "lag_df", "global_test_method", "global_statistic", "global_df1", "global_df2",
      "global_p", "q9", "multiplicity_family_q9", "cumulative_effect", "cumulative_se", "cumulative_ci_lower", "cumulative_ci_upper", "peak_lag", "peak_effect", "simultaneous_critical", "simultaneous_draws",
      "seed", "band_method", "covariance_method", "n_lags_pointwise_excl_null", "any_pointwise_band_excludes_null", "n_lags_simultaneous_excl_null", "any_simultaneous_band_excludes_null",
      "parameter_count", "residual_df", "rank", "convergence", "separation", "status"),
    dlm_final_lag_curves = c("result_id", "lag", "estimate", "standard_error", "lower_95", "upper_95", "simultaneous_lower_95", "simultaneous_upper_95"),
    dlm_final_cumulative_curves = c("result_id", "lag", "cumulative_effect", "cumulative_se", "cumulative_lower_95", "cumulative_upper_95"),
    input_validation_report = c("check_id", "category", "status", "n_affected", "detail"),
    rainfall_anomaly_report = c("item", "value", "note"),
    archived_dlm_summary = c("result_id", "outcome", "outcome_family", "metric", "model_tier", "n", "event_count", "lag_start", "lag_end", "lag_df", "global_test_method", "global_statistic", "global_df1", "global_df2",
      "global_p", "q24", "q9", "multiplicity_family_q24", "multiplicity_family_q9", "cumulative_effect", "cumulative_se", "cumulative_ci_lower", "cumulative_ci_upper", "peak_lag", "peak_effect",
      "simultaneous_critical", "simultaneous_draws", "seed", "band_method", "covariance_method", "n_lags_pointwise_excl_null", "any_pointwise_band_excludes_null", "n_lags_simultaneous_excl_null",
      "any_simultaneous_band_excludes_null", "parameter_count", "residual_df", "rank", "convergence", "separation", "status"),
    archived_dlm_sensitivity_summary = c("result_id", "outcome", "outcome_family", "metric", "model_tier", "n", "event_count", "lag_start", "lag_end", "lag_df", "sensitivity_specification", "global_test_method",
      "global_statistic", "global_df1", "global_df2", "global_p", "q216", "multiplicity_family_q216", "cumulative_effect", "cumulative_se", "peak_lag", "convergence", "separation", "status"),
    rainfall_sensitivity_summary = c("result_id", "outcome", "metric", "model_tier", "analysis", "window_or_lag", "n_primary", "n_sensitivity", "effect_primary", "effect_sensitivity", "p_primary", "p_sensitivity", "q_primary", "q_sensitivity", "note"),
    validation_summary = c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_nonfinite", "reference_file", "n_duplicate", "n_missing_required", "n_unclassified", "n_records_reference", "n_records_results", "expected_records", "required", "reason"),
    validation_detail = c("reference", "key", "statistic", "new_value", "reference_value", "abs_diff", "rel_diff", "class")
  )
}

#' Conform a data frame to a schema: fixed column order, missing columns added as NA, extra columns dropped.
#' @export
siw_conform <- function(df, columns) {
  if (anyDuplicated(columns)) stop("SIW_DUPLICATE_COLUMN_NAMES_IN_SCHEMA: ", paste(columns[duplicated(columns)], collapse = ", "), call. = FALSE)
  if (anyDuplicated(names(df))) stop("SIW_DUPLICATE_COLUMN_NAMES_IN_TABLE: ", paste(names(df)[duplicated(names(df))], collapse = ", "), call. = FALSE)
  for (n in setdiff(columns, names(df))) df[[n]] <- rep(NA, nrow(df))
  df[, columns, drop = FALSE]
}

#' Write one output table with its schema order.
#' @export
siw_write_table <- function(df, name, out_dir) {
  cols <- siw_output_columns()[[name]]
  if (is.null(cols)) stop("SIW_UNKNOWN_OUTPUT_TABLE: ", name, call. = FALSE)
  df <- siw_conform(df, cols); rownames(df) <- NULL
  siw_write_csv(df, file.path(out_dir, paste0(name, ".csv")))
}

#' Column-type description used for the committed JSON schemas.
siw_col_type <- function(x) if (is.integer(x)) "integer" else if (is.numeric(x)) "number" else if (is.logical(x)) "boolean" else "string"

#' Write machine-readable schemas (JSON) from data frames produced by a run.
#' @export
siw_write_schemas <- function(tables, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  keys <- list(cumulative_results = "result_id", complete_case_accounting = "result_id", cumulative_design_audit = "model_tier", dlm_final_summary = "result_id",
               dlm_final_lag_curves = c("result_id", "lag"), dlm_final_cumulative_curves = c("result_id", "lag"), input_validation_report = "check_id", rainfall_anomaly_report = "item",
               archived_dlm_summary = "result_id", archived_dlm_sensitivity_summary = c("result_id", "sensitivity_specification"), rainfall_sensitivity_summary = c("result_id", "analysis"),
               validation_summary = "reference", validation_detail = c("reference", "key", "statistic"))
  for (nm in names(tables)) {
    df <- tables[[nm]]; cols <- siw_output_columns()[[nm]]
    sch <- list(name = nm, file = paste0(nm, ".csv"), primary_key = keys[[nm]], ordering = "deterministic: configuration order of outcomes, metrics, windows/lags, tiers",
                columns = lapply(cols, function(cn) list(name = cn, type = if (cn %in% names(df)) siw_col_type(df[[cn]]) else "string")))
    jsonlite::write_json(sch, file.path(dir, paste0(nm, ".schema.json")), auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  invisible(dir)
}

#' Check that a data frame follows a committed schema (names and order; type classes where the column has data).
#' @export
siw_check_schema <- function(df, schema_path) {
  sch <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  want <- vapply(sch$columns, function(z) z$name, character(1))
  ok_names <- identical(names(df), want)
  list(ok = ok_names, missing = setdiff(want, names(df)), extra = setdiff(names(df), want), order_ok = ok_names)
}

#' Hash only the important final artifacts (CSV, JSON, text reports) in the output folder; checkpoints and logs are excluded.
#' @export
siw_write_manifest <- function(out_dir) {
  f <- list.files(out_dir, pattern = "\\.(csv|json|txt|md)$", full.names = TRUE)
  f <- f[!basename(f) %in% c("MANIFEST.csv")]; f <- sort(f)
  h <- lapply(f, siw_file_hash)
  m <- data.frame(file = basename(f), bytes = file.info(f)$size, algorithm = vapply(h, function(z) z$algorithm, character(1)), hash = vapply(h, function(z) z$value, character(1)), stringsAsFactors = FALSE)
  siw_write_csv(m, file.path(out_dir, "MANIFEST.csv")); m
}

#' Dependency report: required and suggested packages against the installed versions.
#' @export
siw_dependency_report <- function(desc_path = file.path(siw_root(), "DESCRIPTION")) {
  d <- read.dcf(desc_path, fields = c("Depends", "Imports", "Suggests"))
  parse <- function(s, kind) {
    if (is.na(s)) return(NULL)
    p <- trimws(strsplit(gsub("\n", " ", s), ",")[[1]]); p <- p[nzchar(p)]
    data.frame(package = sub("\\s*\\(.*$", "", p), requirement = ifelse(grepl("\\(", p), sub("^.*\\((.*)\\).*$", "\\1", p), ""), declared_in = kind, stringsAsFactors = FALSE)
  }
  req <- rbind(parse(d[1, "Imports"], "Imports"), parse(d[1, "Suggests"], "Suggests"))
  req <- req[req$package != "R", ]
  ip <- utils::installed.packages()
  req$installed_version <- ifelse(req$package %in% rownames(ip), ip[match(req$package, rownames(ip)), "Version"], NA)
  base_pkgs <- rownames(ip)[!is.na(ip[, "Priority"]) & ip[, "Priority"] %in% c("base", "recommended")]
  req$status <- vapply(seq_len(nrow(req)), function(i) {
    v <- req$installed_version[i]; if (is.na(v)) return("MISSING")
    m <- regmatches(req$requirement[i], regexec(">=\\s*([0-9.-]+)", req$requirement[i]))[[1]]
    if (length(m) == 2L && utils::compareVersion(v, m[2]) < 0) "TOO_OLD" else "OK"
  }, character(1))
  req$needed_for <- ifelse(req$declared_in == "Imports", "core workflow", "optional cross-checks and tests")
  req[order(req$declared_in, req$package), ]
}

#' Run metadata (no participant-level content).
#' @export
siw_write_metadata <- function(out_dir, meta) {
  jsonlite::write_json(meta, file.path(out_dir, "run_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
}

#' Batch A table schemas (fixed column order). Kept separate from the core schemas, which are unchanged.
#' @export
siw_batch_a_output_columns <- function() {
  list(
    batch_a_input_checks = c("check_id", "category", "status", "n_affected", "detail"),
    cumulative_diagnostics = c("result_id", "outcome", "metric", "window_or_lag", "model_tier", "n", "parameter_count", "rank", .siw_diag_columns, "dropped_terms", "diagnostic_status", "undefined_diagnostics"),
    bivariate_cumulative = c("result_id", "outcome", "outcome_family", "metric", "exposure_field", "window_or_lag", "correlation_type", "n", "pearson_or_point_biserial_r", "pearson_p", "spearman_rho", "spearman_p", "model_p", "status",
                             "q_pearson_global", "q_pearson_outcome", "q_pearson_outcome_metric", "q_spearman_global", "q_spearman_outcome", "q_spearman_outcome_metric", "q_model_global", "q_model_outcome", "q_model_outcome_metric"),
    descriptive_table1 = c("characteristic", "type", "n_or_count", "percent", "mean", "sd", "median", "iqr", "minimum", "maximum", "missing"),
    descriptive_si_s2 = c("section", "variable", "level", "coding_or_unit", "n", "count", "percent", "mean", "sd", "median", "iqr", "minimum", "maximum", "missing"),
    descriptive_group_summaries = c("outcome", "factor", "level", "n", "mean_or_proportion", "se", "ci_lower", "ci_upper"),
    descriptive_group_tests = c("outcome", "factor", "test", "statistic", "df", "p", "q_within_outcome"),
    exposure_participant_level = c("metric", "window_days", "unit", "unit_of_analysis", "n", "missing", "mean", "sd", "median", "iqr", "minimum", "maximum"),
    weather_participant_day = c("component", "metric", "category", "n", "percent", "note"),
    descriptive_coding_notes = c("item", "value", "note"),
    batch_a_validation_summary = c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_nonfinite", "reference_file", "n_duplicate", "n_missing_required", "n_unclassified", "n_records_reference", "n_records_results", "expected_records", "required", "reason"),
    batch_a_validation_detail = c("reference", "key", "statistic", "new_value", "reference_value", "abs_diff", "rel_diff", "class")
  )
}

#' Fixed column order of every output table (core, Batch A, Batch B and the ordinal refinement step).
#' @export
siw_output_columns <- function() c(.siw_core_output_columns(), siw_batch_a_output_columns(), siw_batch_b_output_columns(), siw_ordinal_refinement_output_columns(), siw_batch_c_output_columns())
