# Batch B orchestration: the documented model-based sensitivity analyses (Firth 560, ordinal 80, transformations 1,080, nonlinear forms 360), their independent
# mathematical verification, and the separate comparison with the Batch B reference bundle. The core pipeline and Batch A are called unchanged
# (siw_run_batch_a); Batch B is an additional stage that reads the same prepared data and writes only new tables.

#' Batch B table schemas (fixed column order). Kept separate from the core and Batch A schemas, which are unchanged.
#' @export
siw_batch_b_output_columns <- function() {
  list(
    firth_sensitivity = c("result_id", "outcome", "outcome_family", "metric", "exposure", "window_or_lag", "model_tier", "n", "event_count", "non_event_count", "parameter_count", "status", "not_estimable_reason",
      "converged", "iterations", "step_halvings", "adjusted_score_max_abs", "scaled_score_norm", "penalised_loglik", "max_abs_linear_predictor", "separation_status", "dropped_terms",
      "coefficient", "standard_error", "ci_lower", "ci_upper", "covariance_row_norm_exposure", "estimator", "interval_method"),
    firth_verification = c("result_id", "converged", "status", "adjusted_score_max_abs", "scaled_score_norm", "gradient_analytic_vs_numerical_at_solution", "gradient_analytic_vs_numerical_off_solution",
      "hessian_max_eigenvalue", "penalised_loglik_hand_difference", "augmentation_converged", "coefficient_difference_vs_augmentation", "se_relative_difference_vs_augmentation", "random_search_improvement", "result"),
    ordinal_integration = c("result_id", "outcome", "outcome_family", "metric", "exposure", "window_or_lag", "model_tier", "n", "categories_used", "dropped_terms", "status", "not_estimable_reason", "convergence",
      "coefficient", "se", "odds_ratio", "p", "log_likelihood", "parameter_count", "exposure_sd", "newton_decrement", "fit_warnings", "q80_ordinal", "multiplicity_family_q80_ordinal"),
    ordinal_verification = c("result_id", "status", "reason", "loglik_difference", "coefficient_difference", "se_difference", "independent_loglik_minus_polr", "independent_coefficient", "independent_se",
      "independent_gradient_max_abs", "polr_newton_decrement"),
    exposure_transformations = c("result_id", "outcome", "outcome_family", "metric", "source_exposure", "window_or_lag", "transformation", "model_tier", "status", "not_estimable_reason", "separation_status", "convergence",
      "dropped_terms", .siw_cum_stats, "q_transformation_outcome", "q_transformation_global", "multiplicity_family_q_transformation_outcome", "multiplicity_family_q_transformation_global"),
    transformation_verification = c("result_id", "coefficient_difference", "se_difference", "p_difference", "n_difference", "status"),
    nonlinear_forms = c("result_id", "outcome", "outcome_family", "metric", "exposure", "window_or_lag", "model_tier", "form", "n", "status", "not_estimable_reason", "convergence", "parameter_count", "dropped_terms",
      "joint_statistic", "joint_df", "joint_p", "aic", "aicc", "q_outcome_form", "q_global", "multiplicity_family_q_outcome_form", "multiplicity_family_q_global"),
    nonlinear_verification = c("result_id", "statistic_difference", "p_difference", "aic_difference", "aicc_difference", "n_difference", "status"),
    batch_b_math_verification = c("component", "n_specifications", "n_verified_pass", "n_documented_limitation", "n_fail", "n_not_verified", "status", "note"),
    batch_b_validation_summary = c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_nonfinite", "reference_file", "n_duplicate", "n_missing_required", "n_unclassified",
      "n_records_reference", "n_records_results", "expected_records", "required", "reason", "n_explained_by_legacy_bound"),
    batch_b_validation_detail = c("reference", "key", "statistic", "new_value", "reference_value", "abs_diff", "rel_diff", "class"),
    firth_attribution = c("reference", "key", "statistic", "new_value", "frozen_value", "abs_diff", "frozen_adjusted_score_max_abs", "bound", "abs_diff_over_bound", "explained")
  )
}

#' Batch B result tables from a prepared data frame.
#' @param core_results results table of the core cumulative grid (supplies the ordinary-fit separation flag to the Firth table and the M2-L cross-check)
#' @param verify run the independent mathematical verification of every specification
#' @return named list of data frames with attribute timings (seconds per stage)
#' @export
siw_batch_b_tables <- function(dat, config, core_results = NULL, verify = TRUE) {
  tm <- list(); tick <- function(name, expr) { t0 <- proc.time()[["elapsed"]]; v <- force(expr); tm[[name]] <<- proc.time()[["elapsed"]] - t0; v }
  t <- list(); can <- isTRUE(verify) && requireNamespace("numDeriv", quietly = TRUE)
  fg <- tick("firth", siw_firth_grid(dat, config, core_results, verify = verify)); t$firth_sensitivity <- fg$results; t$firth_verification <- fg$verification
  t$ordinal_integration <- tick("ordinal", siw_ordinal_grid(dat, config))
  t$exposure_transformations <- tick("transformations", siw_transformation_grid(dat, config))
  t$nonlinear_forms <- tick("nonlinear", siw_nonlinear_grid(dat, config))
  if (isTRUE(verify)) {
    t$ordinal_verification <- if (can) tick("ordinal_verification", siw_ordinal_verify(dat, config, t$ordinal_integration)) else data.frame()
    t$transformation_verification <- tick("transformation_verification", siw_transformation_verify(dat, config, t$exposure_transformations))
    t$nonlinear_verification <- tick("nonlinear_verification", siw_nonlinear_verify(dat, config, t$nonlinear_forms))
  } else { t$ordinal_verification <- t$transformation_verification <- t$nonlinear_verification <- data.frame() }
  t$batch_b_math_verification <- siw_batch_b_math_summary(t, can, verify)
  attr(t, "timings") <- unlist(tm); t
}

#' Summary of the independent verification, one row per component. Status values: PASS, PASS_WITH_DOCUMENTED_POLR_LIMITATION, FAIL, NOT_TESTED (never counted as PASS).
#' @export
siw_batch_b_math_summary <- function(t, numDeriv_available = TRUE, verify = TRUE) {
  row <- function(component, nspec, ver, pass_col, note, limitation = character(), extra_fail = 0L) {
    if (!isTRUE(verify) || !is.data.frame(ver) || !nrow(ver)) return(data.frame(component = component, n_specifications = nspec, n_verified_pass = 0L, n_documented_limitation = 0L, n_fail = 0L, n_not_verified = nspec,
      status = "NOT_TESTED", note = paste0(note, if (!isTRUE(verify)) " Verification was not requested." else " No verification rows were produced."), stringsAsFactors = FALSE))
    st <- ver[[pass_col]]; np <- sum(st == "PASS"); nl <- sum(st %in% limitation); nf <- sum(!(st %in% c("PASS", limitation, "NOT_VERIFIED_NOT_ESTIMABLE"))) + extra_fail
    nv <- nspec - np - nl - nf
    data.frame(component = component, n_specifications = nspec, n_verified_pass = np, n_documented_limitation = nl, n_fail = nf, n_not_verified = nv,
               status = if (nf > 0L || nv > 0L) (if (nf > 0L) "FAIL" else "NOT_TESTED") else if (nl > 0L) "PASS_WITH_DOCUMENTED_POLR_LIMITATION" else "PASS", note = note, stringsAsFactors = FALSE)
  }
  fr <- t$firth_sensitivity; nfir <- sum(fr$status %in% c("COMPLETED", "NOT_CONVERGED")); fv <- t$firth_verification
  rows <- list(
    row("firth", nfir, fv, "result", "Independent checks of every completed Firth fit: scaled adjusted score <= 1e-8, analytic vs numerical gradient (<= 1e-6, at and off the solution), numerical Hessian negative definite, hand-evaluated penalised likelihood, independent pseudo-data augmentation estimator (coefficients <= 1e-7, relative SE <= 1e-7), local random search. logistf/brglm2 are not installed: agreement with a Firth package is NOT TESTED. A fit that did not converge counts as FAIL.", extra_fail = sum(fr$status == "NOT_CONVERGED")),
    row("ordinal", sum(t$ordinal_integration$status == "COMPLETED"), t$ordinal_verification, "status", "Independent maximum likelihood with analytic derivatives (log-likelihood <= 1e-6, coefficient and SE <= 1e-4, the accuracy of polr's own optimiser). POLR_NOT_AT_MAXIMUM marks fits at which MASS::polr stopped short of the maximum (documented polr limitation; polr values are reported as returned).", "POLR_NOT_AT_MAXIMUM"),
    row("transformations", sum(t$exposure_transformations$status == "COMPLETED"), t$transformation_verification, "status", "Every completed specification recomputed with stats::lm / stats::glm and a hand-written HC3 / information (relative differences <= 1e-8)."),
    row("nonlinear", sum(t$nonlinear_forms$status == "COMPLETED"), t$nonlinear_verification, "status", "Every completed specification recomputed with stats::lm / stats::glm, splines::ns and a direct Wald quadratic form (relative differences <= 1e-8)."))
  if (!isTRUE(numDeriv_available) && isTRUE(verify)) for (i in c(1L, 2L)) { rows[[i]]$status <- "NOT_TESTED"; rows[[i]]$note <- paste0(rows[[i]]$note, " numDeriv is not installed: derivative-based checks were not run.") }
  do.call(rbind, rows)
}

#' Overall mathematical-verification status from the component summary.
#' @export
siw_batch_b_math_status <- function(m) {
  if (any(m$status == "FAIL")) "FAIL" else if (any(m$status == "NOT_TESTED")) "NOT_TESTED" else if (any(m$status == "PASS_WITH_DOCUMENTED_POLR_LIMITATION")) "PASS_WITH_DOCUMENTED_LIMITATION" else "PASS"
}

#' Internal consistency checks of the Batch B tables (counts, identifiers, family sizes, cross-check of the raw transformation against the core M2-L results, schema order).
#' @export
siw_batch_b_self_checks <- function(tables, config, core_results = NULL) {
  R <- list(); add <- function(check, ok, detail) R[[length(R) + 1L]] <<- data.frame(check = check, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = detail, stringsAsFactors = FALSE)
  nb <- sum(config$outcomes$family == "binomial"); nm <- nrow(config$metrics); nw <- length(config$windows); no <- nrow(config$outcomes); nt <- length(config$tier_order); nmean <- sum(config$metrics$aggregation == "mean")
  f <- tables$firth_sensitivity; o <- tables$ordinal_integration; x <- tables$exposure_transformations; n <- tables$nonlinear_forms
  add("firth_count", nrow(f) == nb * nm * nw * nt, paste0(nrow(f), " rows; expected ", nb * nm * nw * nt)); add("firth_ids_unique", !anyDuplicated(f$result_id), "result_id unique")
  add("firth_no_pvalue_or_family", !any(c("p", "nominal_p", "q") %in% names(f)), "no p-value and no multiplicity family are produced for the Firth sensitivity (none is documented)")
  add("firth_status_reported", all(nzchar(f$status)), paste0("COMPLETED=", sum(f$status == "COMPLETED"), "; NOT_CONVERGED=", sum(f$status == "NOT_CONVERGED"), "; NOT_ESTIMABLE=", sum(f$status == "NOT_ESTIMABLE"), " (reported per model, none hidden)"))
  add("ordinal_count", nrow(o) == nm * nw, paste0(nrow(o), " rows; expected ", nm * nw)); add("ordinal_ids_unique", !anyDuplicated(o$result_id), "result_id unique")
  add("ordinal_q80_family_size", sum(is.finite(o$q80_ordinal)) == sum(is.finite(o$p)), "q80_ordinal over all finite ordinal p-values (own family, separate from the primary cumulative q80)")
  add("ordinal_q80_column_distinct_from_primary", !"q80" %in% names(o), "the ordinal family is stored as q80_ordinal")
  ntr <- no * (sum(config$metrics$aggregation == "mean") * 4L + sum(config$metrics$aggregation == "sum") * 6L) * nw
  add("transformation_count", nrow(x) == ntr, paste0(nrow(x), " rows; expected ", ntr)); add("transformation_ids_unique", !anyDuplicated(x$result_id), "result_id unique")
  add("transformation_q_family_sizes", all(vapply(split(x$q_transformation_outcome, x$outcome), function(v) sum(is.finite(v)), 1L) == vapply(split(x$nominal_p, x$outcome), function(v) sum(is.finite(v)), 1L)) && sum(is.finite(x$q_transformation_global)) == sum(is.finite(x$nominal_p)), "outcome and global families cover all finite p-values")
  add("transformation_status_reported", all(nzchar(x$status)), paste0("COMPLETED=", sum(x$status == "COMPLETED"), "; NOT_ESTIMABLE=", sum(x$status == "NOT_ESTIMABLE"), " (domain violations and other failures reported per specification)"))
  add("nonlinear_count", nrow(n) == no * nmean * nw * 2L, paste0(nrow(n), " rows; expected ", no * nmean * nw * 2L)); add("nonlinear_ids_unique", !anyDuplicated(n$result_id), "result_id unique")
  add("nonlinear_q_family_sizes", sum(is.finite(n$q_outcome_form)) == sum(is.finite(n$joint_p)) && sum(is.finite(n$q_global)) == sum(is.finite(n$joint_p)), "outcome x form and global families cover all finite joint p-values")
  if (!is.null(core_results)) {
    r <- x[x$transformation == "raw", ]; m <- core_results[core_results$model_tier == config$primary_tier, ]; i <- match(paste(r$outcome, r$metric, r$window_or_lag), paste(m$outcome, m$metric, m$window_or_lag))
    d <- if (anyNA(i)) Inf else max(abs(r$coefficient - m$coefficient[i]), abs(r$nominal_p - m$nominal_p[i]), abs(r$n - m$n[i]), na.rm = TRUE)
    add("raw_transformation_equals_core_M2L", is.finite(d) && d <= 1e-12, paste0("max abs difference to the validated cumulative M2-L results: ", format(d, digits = 3)))
  }
  for (nm_ in names(tables)) { cols <- siw_output_columns()[[nm_]]; if (!is.null(cols) && is.data.frame(tables[[nm_]]) && nrow(tables[[nm_]])) add(paste0("schema_columns_", nm_), identical(names(siw_conform(tables[[nm_]], cols)), cols), "fixed column order") }
  do.call(rbind, R)
}

#' Run the core analyses, Batch A and Batch B on one canonical CSV, then compare the core, Batch A and Batch B results with their reference bundles (corrected strict validator).
#' Execution status, input-check status, the three comparison statuses and the mathematical-verification status are reported separately; nothing is changed to fit a reference.
#' @param core_reference_dir accepted core bundle; batch_a_reference_dir Batch A bundle; batch_b_reference_dir Batch B bundle
#' @param expected_assessment_days expected number of distinct assessment_day groups (33 for the study data; NA = report only)
#' @export
siw_run_batch_b <- function(data_csv, out_dir, core_reference_dir, batch_a_reference_dir, batch_b_reference_dir, config = siw_load_config(), expected_assessment_days = NA_integer_, label = NULL) {
  t_all <- proc.time()[["elapsed"]]
  a <- siw_run_batch_a(data_csv, out_dir, core_reference_dir, batch_a_reference_dir, config, expected_assessment_days, label)
  options(sieccwinter.log_file = file.path(out_dir, "run.log"))
  raw <- siw_read_study_csv(data_csv, config); dat <- siw_attach_cumulative(siw_prepare_data(raw, config), config)$data
  siw_log("Batch B start")
  bt <- siw_batch_b_tables(dat, config, a$core$tables$cumulative_results, verify = TRUE)
  for (nm in names(bt)) if (is.data.frame(bt[[nm]]) && nrow(bt[[nm]])) siw_write_table(bt[[nm]], nm, out_dir)
  sc <- siw_batch_b_self_checks(bt, config, a$core$tables$cumulative_results); siw_write_csv(sc, file.path(out_dir, "batch_b_self_checks.csv"))
  tim <- attr(bt, "timings"); siw_write_csv(data.frame(stage = names(tim), seconds = round(unname(tim), 3), stringsAsFactors = FALSE), file.path(out_dir, "batch_b_timings.csv"))
  bplan <- siw_load_batch_b_plan(); bv <- siw_validate_batch_b(bt, config, batch_b_reference_dir, bplan)
  siw_write_table(bv$summary, "batch_b_validation_summary", out_dir); siw_write_table(bv$detail, "batch_b_validation_detail", out_dir)
  if (nrow(bv$attribution)) siw_write_table(bv$attribution, "firth_attribution", out_dir)
  base <- siw_overall_validation_status(bv$summary); limited <- any(bv$summary$status == "PASS_EXPLAINED_BY_LEGACY_CONVERGENCE")
  b_status <- if (base == "PASS") (if (limited) "PASS_WITH_DOCUMENTED_LEGACY_LIMITATION" else "PASS") else "FAIL"
  m_status <- siw_batch_b_math_status(bt$batch_b_math_verification)
  exec_ok <- identical(a$statuses[["EXECUTION_STATUS"]], "PASS") && !any(sc$status == "FAIL")
  st <- c(EXECUTION_STATUS = if (exec_ok) "PASS" else "FAIL", INPUT_CHECK_STATUS = a$statuses[["INPUT_CHECK_STATUS"]], CORE_COMPARISON_STATUS = a$statuses[["CORE_COMPARISON_STATUS"]], BATCH_A_COMPARISON_STATUS = a$statuses[["BATCH_A_COMPARISON_STATUS"]],
          BATCH_B_COMPARISON_STATUS = b_status, BATCH_B_MATH_VERIFICATION_STATUS = m_status)
  m <- bt$batch_b_math_verification; ms <- stats::setNames(m$status, paste0("MATH_", toupper(m$component), "_STATUS"))
  writeLines(c(paste0(names(st), "=", st), paste0(names(ms), "=", ms), paste0("BATCH_B_REFERENCES_NOT_PASSED=", sum(bv$summary$required %in% TRUE & !grepl("^PASS", bv$summary$status))),
               paste0("BATCH_B_REFERENCES_WITH_DOCUMENTED_LEGACY_LIMITATION=", sum(bv$summary$status == "PASS_EXPLAINED_BY_LEGACY_CONVERGENCE")), "Results were not altered to fit any reference."), file.path(out_dir, "BATCH_B_STATUS.txt"))
  ref_files <- unlist(lapply(bplan$references, function(r) unlist(r$files))); ref_files <- ref_files[file.exists(file.path(batch_b_reference_dir, ref_files))]
  meta <- list(package = "sieccwinter", package_version = siw_pkg_version(), batch = "B", core_plan_hash = siw_load_validation_plan()$plan_hash, batch_a_plan_hash = siw_load_batch_a_plan()$plan_hash, batch_b_plan_hash = bplan$plan_hash,
               config_hash = config$config_hash, tolerances = config$tolerances, input_label = label %||% basename(data_csv), batch_b_reference_files = as.list(stats::setNames(vapply(file.path(batch_b_reference_dir, ref_files), function(p) siw_file_hash(p)$value, ""), ref_files)),
               statuses = as.list(st), math_verification = as.list(ms), firth = list(estimator = "Jeffreys_adjusted_score_Firth_logistic", tolerance_scaled_score = 1e-10, interval_method = "Wald_from_penalised_expected_information"),
               ordinal = list(function_ = "MASS::polr", method = "logistic", Hess = TRUE, MASS_version = as.character(utils::packageVersion("MASS"))), numDeriv_available = requireNamespace("numDeriv", quietly = TRUE),
               r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), batch_b_seconds = round(proc.time()[["elapsed"]] - t_all, 2))
  jsonlite::write_json(meta, file.path(out_dir, "batch_b_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  siw_write_manifest(out_dir); siw_log("Batch B done: ", paste(names(st), st, sep = "=", collapse = "; ")); options(sieccwinter.log_file = NULL)
  ok <- c("PASS", "PASS_WITH_DOCUMENTED_LEGACY_LIMITATION", "PASS_WITH_DOCUMENTED_LIMITATION")
  list(out_dir = out_dir, statuses = st, math = ms, batch_a = a, batch_tables = bt, batch_validation = bv, self_checks = sc,
       status = if (st[["EXECUTION_STATUS"]] == "FAIL") "EXECUTION_FAILURE" else if (all(st == "PASS")) "OK" else if (all(st %in% ok)) "OK_WITH_DOCUMENTED_LIMITATIONS" else "COMPARISON_OR_INPUT_FAILURE")
}
