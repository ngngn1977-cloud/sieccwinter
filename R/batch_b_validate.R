# Batch B validation: separate plan (config/validation_plan_batchB.json), the corrected strict validator, and the declared treatment of the legacy Firth convergence limit.
# Two requirements are kept apart:
#   (1) agreement with the frozen estimates (siw_validate_batch_b), judged with the existing 'cumulative' tolerance set (exact 1e-12; within 1e-7 abs + 1e-6 rel);
#   (2) independent mathematical correctness of the new implementations (siw_firth_verify, siw_ordinal_verify, siw_transformation_verify, siw_nonlinear_verify).
# Firth: the frozen estimates were produced by an iteration that stops with an adjusted score of up to about 1e-4 (the frozen output records adjusted_score_max_abs of every fit),
# so they are only the penalised maximum to within I^-1 U. A difference between the new (tightly converged) estimate and a frozen one is NOT forced to agree:
# it is classified DIFFERENT, and then - only for the coefficient and its interval limits - checked against the bound implied by the frozen fit's own recorded score,
#     |b_new - b_frozen| <= sqrt(p) * ||row_exposure(I^-1)||_2 * U_frozen        (first-order: b_frozen - b_new = I^-1 U; U_frozen = max |adjusted score| recorded by the frozen run)
# Cells inside the bound are labelled DIFFERENT_EXPLAINED_BY_LEGACY_CONVERGENCE and the reference is reported PASS_EXPLAINED_BY_LEGACY_CONVERGENCE (never plain PASS);
# cells outside it stay DIFFERENT and the reference FAILS. Standard errors are never excused.

#' Load the Batch B validation plan (config/validation_plan_batchB.json): references with their own files, keys, result tables, tolerance names and required columns.
#' @export
siw_load_batch_b_plan <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root(); cand <- c(file.path(root, "config", "validation_plan_batchB.json"), file.path(root, "extdata", "validation_plan_batchB.json"), file.path(root, "inst", "extdata", "validation_plan_batchB.json"))
    path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_BATCH_B_PLAN_NOT_FOUND", call. = FALSE)
  }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$plan_path <- normalizePath(path, winslash = "/"); p$plan_hash <- siw_file_hash(path)$value; p
}

#' Apply the declared legacy-convergence bound to the DIFFERENT cells of the Firth comparison (see the header of this file).
#' @return the validation result (summary, detail) with an added summary column n_explained_by_legacy_bound, plus the attribution table
.siw_apply_firth_bound <- function(r, tables, config, reference_path, plan_entry) {
  keys <- unlist(plan_entry$keys); res <- tables[[plan_entry$result]]
  ref <- utils::read.csv(reference_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  ref$window_or_lag <- suppressWarnings(as.integer(ref$window_or_lag)); res$window_or_lag <- as.integer(res$window_or_lag)
  kf <- function(d) do.call(paste, c(lapply(d[keys], as.character), sep = "|"))
  r$summary$n_explained_by_legacy_bound <- 0L; attribution <- data.frame()
  d <- r$detail; if (!"class" %in% names(d) || !any(d$class == "DIFFERENT")) return(list(result = r, attribution = attribution))
  if (!"legacy_adjusted_score_max_abs" %in% names(ref)) return(list(result = r, attribution = attribution))
  tol <- config$tolerances; i <- which(d$class == "DIFFERENT" & d$statistic %in% c("coefficient", "ci_lower", "ci_upper"))
  rr <- match(d$key[i], kf(ref)); sr <- match(d$key[i], kf(res)); ok <- !is.na(rr) & !is.na(sr)
  U <- ref$legacy_adjusted_score_max_abs[rr]; rn <- res$covariance_row_norm_exposure[sr]; pc <- res$parameter_count[sr]
  se_ref <- ref$standard_error[rr]
  bound <- sqrt(pc) * rn * U
  bound <- ifelse(d$statistic[i] == "coefficient", bound, bound + 1.96 * (tol$cumulative_abs + tol$cumulative_rel * abs(se_ref)))
  explained <- ok & is.finite(bound) & d$abs_diff[i] <= bound
  d$class[i[explained]] <- "DIFFERENT_EXPLAINED_BY_LEGACY_CONVERGENCE"
  attribution <- data.frame(key = d$key[i], statistic = d$statistic[i], new_value = d$new_value[i], frozen_value = d$reference_value[i], abs_diff = d$abs_diff[i], frozen_adjusted_score_max_abs = U, bound = bound,
                            abs_diff_over_bound = ifelse(is.finite(bound) & bound > 0, d$abs_diff[i] / bound, NA_real_), explained = explained, stringsAsFactors = FALSE)
  s <- r$summary; k <- sum(explained); s$n_explained_by_legacy_bound <- as.integer(k); s$n_different <- as.integer(s$n_different - k)
  other <- s$n_unmatched + s$n_nonfinite + s$n_duplicate + s$n_missing_required + s$n_unclassified + as.integer(grepl("record count", s$reason) | grepl("nothing was compared", s$reason))
  if (s$n_different == 0L && other == 0L && k > 0L) {
    s$status <- "PASS_EXPLAINED_BY_LEGACY_CONVERGENCE"
    s$reason <- paste0(k, " coefficient / interval-limit cells differ from the frozen values beyond the cumulative tolerance but within the bound implied by the frozen fits' own recorded adjusted score (the frozen Firth iteration stopped short of the maximum); all other cells exact or within tolerance")
  } else if (s$n_different > 0L) s$reason <- sub("^[0-9]+ cells outside tolerance", paste0(s$n_different, " cells outside tolerance and outside the legacy-convergence bound"), s$reason)
  r$summary <- s; r$detail <- d
  list(result = r, attribution = attribution)
}

#' Compare the Batch B result tables with the Batch B reference bundle, following the Batch B plan. Same strict rules as the core and Batch A validators:
#' an absent, undeclared or incomplete reference FAILS; unmatched, duplicated or blank keys, missing required columns, record-count mismatches and non-finite disagreements FAIL.
#' @return list(summary, detail, attribution)
#' @export
siw_validate_batch_b <- function(tables, config, reference_dir, plan = siw_load_batch_b_plan()) {
  S <- list(); D <- list(); A <- list()
  for (pl in plan$references) {
    r <- .siw_validate_one(tables, config, reference_dir, list(name = pl$name, files = pl$files, keys = pl$keys, result = pl$result, tolerance = pl$tolerance, aliases = list(), plan = pl))
    if (isTRUE(pl$legacy_convergence_bound) && grepl("^(PASS|FAIL)", r$summary$status) && nzchar(r$summary$reference_file) && is.data.frame(tables[[pl$result]])) {
      rp <- file.path(reference_dir, r$summary$reference_file)
      if (file.exists(rp)) { b <- .siw_apply_firth_bound(r, tables, config, rp, pl); r <- b$result; if (nrow(b$attribution)) A[[length(A) + 1L]] <- cbind(reference = pl$name, b$attribution) }
    }
    if (!"n_explained_by_legacy_bound" %in% names(r$summary)) r$summary$n_explained_by_legacy_bound <- 0L
    S[[length(S) + 1L]] <- r$summary; D[[length(D) + 1L]] <- r$detail
  }
  list(summary = do.call(rbind, S), detail = siw_bind_rows(D), attribution = if (length(A)) do.call(rbind, A) else data.frame())
}

#' Map the frozen (legacy-format) Batch B tables to the layout of the reference bundle: the same function builds the real bundle and the synthetic
#' end-to-end test, so both exercise the same mapping. Values are never changed; columns are selected and renamed.
#' @param legacy list(firth, ordinal, transformation, nonlinear) as written by the original analysis (cumulative_Firth_sensitivity, ordinal_integration_grid, transformation_grid, nonlinearity_grid)
#' @export
siw_batch_b_reference_tables <- function(legacy) {
  out <- list()
  f <- legacy$firth; parts <- strsplit(f$model_id, "__", fixed = TRUE)
  stopifnot(all(lengths(parts) == 6L), all(vapply(parts, function(z) z[1L], "") == "RAW_CUMULATIVE"), all(vapply(parts, function(z) z[6L], "") == "raw"))
  out$firth <- data.frame(outcome = vapply(parts, function(z) z[2L], ""), metric = vapply(parts, function(z) z[3L], ""), window_or_lag = as.integer(sub("^W", "", vapply(parts, function(z) z[4L], ""))),
    model_tier = vapply(parts, function(z) z[5L], ""), coefficient = f$exposure_coefficient, standard_error = f$exposure_standard_error, ci_lower = f$exposure_ci_lower, ci_upper = f$exposure_ci_upper,
    legacy_adjusted_score_max_abs = f$adjusted_score_max_abs, legacy_iterations = f$iterations, stringsAsFactors = FALSE)
  o <- legacy$ordinal
  out$ordinal <- data.frame(outcome = o$outcome, metric = o$metric, window_or_lag = as.integer(o$window), n = o$n, coefficient = o$coefficient, se = o$se, odds_ratio = o$odds_ratio, p = o$p, q80_ordinal = o$q80, stringsAsFactors = FALSE)
  t <- legacy$transformation
  keep <- intersect(c("outcome", "metric", "window_or_lag", "transformation", .siw_cum_stats, "q_transformation_outcome", "q_transformation_global"), names(t))
  out$transformation <- t[, keep, drop = FALSE]; out$transformation$window_or_lag <- as.integer(out$transformation$window_or_lag)
  n <- legacy$nonlinear
  out$nonlinear <- data.frame(outcome = n$outcome, metric = n$metric, window_or_lag = as.integer(n$window), form = n$form, n = n$n, joint_statistic = n$joint_statistic, joint_df = n$joint_df, joint_p = n$joint_p,
                              aic = n$aic, aicc = n$aicc, q_outcome_form = n$q_outcome_form, q_global = n$q_global, stringsAsFactors = FALSE)
  out
}
