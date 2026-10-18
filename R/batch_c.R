# Batch C orchestration: primary cumulative bootstrap run, independent verification, descriptive scientific-validity assessment, strict comparison with the Batch C reference bundle,
# target-level resumption, progress reporting and the package-source fingerprint. Core, Batch A and Batch B are NOT rerun (only the 90 M2-L point estimates are recomputed, for context and as a check).

#' Batch C table schemas (fixed column order).
#' @export
siw_batch_c_output_columns <- function() {
  list(
    bootstrap_summary = c("result_id", "outcome", "outcome_family", "metric", "exposure", "window_or_lag", "resampling_type", "target_index", "seed", "replicates_requested", "replicates_attempted", "replicates_successful", "replicates_failed",
      "estimate_median", "ci_lower", "ci_upper", "positive_fraction", "negative_fraction", "interval_excludes_null"),
    bootstrap_failures = c("result_id", "kind", "reason", "count", "fraction_of_attempted"),
    bootstrap_diagnostics = c("result_id", "replicates_attempted", "replicates_successful", "replicates_failed", "glm_not_converged", "level_absent_resamples", "other_covariate_aliased_resamples", "min_complete_cases", "mean_complete_cases",
      "bootstrap_sd", "estimate_min", "estimate_max", "seconds"),
    bootstrap_point_estimates = c("outcome", "metric", "window_or_lag", "model_tier", "n", "coefficient", "robust_hc3_se", "model_based_se", "nominal_p"),
    bootstrap_validity = c("result_id", "success_fraction", "glm_not_converged_fraction", "bootstrap_sd", "reference_se", "bootstrap_sd_to_reference_se", "primary_coefficient", "ci_contains_primary_estimate", "median_minus_primary", "level_absent_fraction",
      "flag_success_fraction", "flag_glm_convergence", "flag_sd_ratio", "flag_interval_contains_primary", "validity_status", "review_reasons"),
    bootstrap_verification = c("result_id", "status", "failed_checks", "sampling_replicates_checked", "sampling_violations", "sampling_uniformity_max_abs_z", "legacy_style_refits_compared", "legacy_style_max_relative_difference", "legacy_style_mismatches",
      "median_relative_difference", "ci_lower_relative_difference", "ci_upper_relative_difference", "seed_rule_match", "conservation_ok", "full_sample_estimate_relative_difference", "determinism_recomputed", "determinism_identical"),
    bootstrap_math_verification = c("check", "n_targets", "n_pass", "n_fail", "status", "note"),
    batch_c_validation_summary = siw_batch_b_output_columns()$batch_b_validation_summary[-length(siw_batch_b_output_columns()$batch_b_validation_summary)],
    batch_c_validation_detail = siw_batch_b_output_columns()$batch_b_validation_detail,
    source_manifest = c("file", "bytes", "sha256")
  )
}

#' Load the Batch C validation plan (config/validation_plan_batchC.json).
#' @export
siw_load_batch_c_plan <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root(); cand <- c(file.path(root, "config", "validation_plan_batchC.json"), file.path(root, "extdata", "validation_plan_batchC.json"), file.path(root, "inst", "extdata", "validation_plan_batchC.json"))
    path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_BATCH_C_PLAN_NOT_FOUND", call. = FALSE)
  }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$plan_path <- normalizePath(path, winslash = "/"); p$plan_hash <- siw_file_hash(path)$value; p
}

#' Compare the Batch C bootstrap summary with the Batch C reference bundle (strict validator, plan config/validation_plan_batchC.json).
#' A required reference that is absent, undeclared or incomplete FAILS; unmatched, duplicated or blank keys, missing required columns, record-count mismatches and non-finite disagreements FAIL.
#' @export
siw_validate_batch_c <- function(tables, config, reference_dir, plan = siw_load_batch_c_plan()) {
  S <- list(); D <- list()
  for (pl in plan$references) { r <- .siw_validate_one(tables, config, reference_dir, list(name = pl$name, files = pl$files, keys = pl$keys, result = pl$result, tolerance = pl$tolerance, aliases = list(), plan = pl)); S[[length(S) + 1L]] <- r$summary; D[[length(D) + 1L]] <- r$detail }
  list(summary = do.call(rbind, S), detail = siw_bind_rows(D))
}

# ---------------------------------------------------------------- checkpoints (target-level resumption) ----------------------------------------------------------------
.siw_ckpt_path <- function(dir, target_id) file.path(dir, paste0("CKPT__", target_id, ".rds"))
.siw_ckpt_signature <- function(parts) siw_string_hash(paste(parts, collapse = "|"))
.siw_ckpt_read <- function(dir, target_id, signature) {
  f <- .siw_ckpt_path(dir, target_id); if (!file.exists(f)) return(NULL)
  obj <- tryCatch(readRDS(f), error = function(e) NULL); if (is.null(obj) || !identical(obj$signature, signature) || !identical(obj$target_id, target_id)) return(NULL); obj$value
}
.siw_ckpt_write <- function(dir, target_id, signature, value) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE); f <- .siw_ckpt_path(dir, target_id); tmp <- paste0(f, ".part"); saveRDS(list(signature = signature, target_id = target_id, value = value), tmp)
  if (file.exists(f)) unlink(f); invisible(file.rename(tmp, f))
}

#' Run the bootstrap targets (all 180 by default) with progress reporting and target-level resumption. Results carry the per-replicate classification; nothing is redrawn.
#' @param targets optional integer positions in the plan (development use); resume reuse of valid checkpoints from ckpt_dir
#' @return list(plan, results = named list by target_id, status_by_target)
#' @export
siw_bootstrap_run_targets <- function(dat, config, replicates = 2000L, ckpt_dir = NULL, signature_parts = character(), resume = TRUE, targets = NULL, log = TRUE) {
  plan <- siw_bootstrap_plan(config, replicates); if (!is.null(targets)) plan <- plan[targets, , drop = FALSE]; gr <- siw_bootstrap_groups(dat); res <- list(); st <- character(); t_all <- proc.time()[["elapsed"]]; done_seconds <- 0; nt <- nrow(plan)
  for (i in seq_len(nt)) {
    p <- plan[i, ]; sig <- .siw_ckpt_signature(c(signature_parts, p$target_id, p$seed, replicates)); v <- if (!is.null(ckpt_dir) && resume) .siw_ckpt_read(ckpt_dir, p$target_id, sig) else NULL
    if (!is.null(v)) { st[p$target_id] <- "RESTORED"; res[[p$target_id]] <- v }
    else {
      spec <- siw_bootstrap_design_spec(dat, config, p$outcome, p$outcome_family, p$exposure); v <- siw_bootstrap_target(gr, spec, p$resampling_type, replicates, p$seed); v$summary <- siw_bootstrap_summarise(v$estimates, replicates)
      if (!is.null(ckpt_dir)) .siw_ckpt_write(ckpt_dir, p$target_id, sig, v); st[p$target_id] <- "COMPUTED"; res[[p$target_id]] <- v; done_seconds <- done_seconds + v$seconds
    }
    if (isTRUE(log)) { n_comp <- sum(st == "COMPUTED"); el <- proc.time()[["elapsed"]] - t_all; eta <- if (n_comp > 0L) round(done_seconds / n_comp * (nt - i)) else NA
      siw_log(sprintf("bootstrap [%d/%d] %s: %s; successful %d/%d; %.1f s; elapsed %.0f s; ETA %s s", i, nt, p$target_id, st[[p$target_id]], v$summary$replicates_successful, replicates, v$seconds %||% 0, el, if (is.na(eta)) "n/a" else eta)) }
  }
  list(plan = plan, results = res, status_by_target = st)
}

# ---------------------------------------------------------------- tables ----------------------------------------------------------------
.siw_bootstrap_tables <- function(run, point) {
  plan <- run$plan; S <- list(); Fl <- list(); Dg <- list()
  for (i in seq_len(nrow(plan))) {
    p <- plan[i, ]; r <- run$results[[p$target_id]]; s <- r$summary; ok <- is.finite(r$estimates); att <- length(r$estimates)
    S[[i]] <- cbind(data.frame(result_id = p$target_id, outcome = p$outcome, outcome_family = p$outcome_family, metric = p$metric, exposure = p$exposure, window_or_lag = p$window_or_lag, resampling_type = p$resampling_type, target_index = p$target_index, seed = p$seed, stringsAsFactors = FALSE), s)
    tb <- table(r$reason[nzchar(r$reason)]); if (length(tb)) Fl[[length(Fl) + 1L]] <- data.frame(result_id = p$target_id, kind = "failure", reason = names(tb), count = as.integer(tb), fraction_of_attempted = as.integer(tb) / att, stringsAsFactors = FALSE)
    for (dn in c("glm_not_converged", "level_absent", "other_aliased")) if (sum(r[[dn]]) > 0L) Fl[[length(Fl) + 1L]] <- data.frame(result_id = p$target_id, kind = "diagnostic_not_a_failure", reason = dn, count = as.integer(sum(r[[dn]])), fraction_of_attempted = sum(r[[dn]]) / att, stringsAsFactors = FALSE)
    Dg[[i]] <- data.frame(result_id = p$target_id, replicates_attempted = att, replicates_successful = sum(ok), replicates_failed = att - sum(ok), glm_not_converged = sum(r$glm_not_converged), level_absent_resamples = sum(r$level_absent), other_covariate_aliased_resamples = sum(r$other_aliased),
      min_complete_cases = min(r$n_complete), mean_complete_cases = mean(r$n_complete), bootstrap_sd = if (sum(ok) > 1L) stats::sd(r$estimates[ok]) else NA_real_, estimate_min = if (any(ok)) min(r$estimates[ok]) else NA_real_, estimate_max = if (any(ok)) max(r$estimates[ok]) else NA_real_, seconds = round(r$seconds, 3), stringsAsFactors = FALSE)
  }
  list(summary = do.call(rbind, S), failures = if (length(Fl)) do.call(rbind, Fl) else data.frame(result_id = character(), kind = character(), reason = character(), count = integer(), fraction_of_attempted = numeric()), diagnostics = do.call(rbind, Dg))
}

#' Descriptive scientific-validity assessment (declared thresholds in config/bootstrap_protocol.json). Reported separately from legacy agreement; never changes or redraws a result.
#' @export
siw_bootstrap_validity <- function(summary, diagnostics, point, protocol) {
  sv <- protocol$scientific_validity; k <- paste(point$outcome, point$metric, point$window_or_lag); rows <- list()
  for (i in seq_len(nrow(summary))) {
    s <- summary[i, ]; d <- diagnostics[i, ]; j <- match(paste(s$outcome, s$metric, s$window_or_lag), k); prim <- if (is.na(j)) NA_real_ else point$coefficient[j]
    ref <- if (is.na(j)) NA_real_ else if (s$outcome_family == "gaussian") point$robust_hc3_se[j] else point$model_based_se[j]; sf <- s$replicates_successful / s$replicates_attempted; cf <- d$glm_not_converged / max(1L, d$replicates_successful)
    ratio <- d$bootstrap_sd / ref; inside <- is.finite(prim) && is.finite(s$ci_lower) && prim >= s$ci_lower && prim <= s$ci_upper
    f1 <- sf >= sv$min_success_fraction; f2 <- s$outcome_family == "gaussian" || cf <= sv$max_glm_nonconverged_fraction; f3 <- is.finite(ratio) && ratio >= sv$bootstrap_sd_to_reference_se_ratio_range[[1L]] && ratio <= sv$bootstrap_sd_to_reference_se_ratio_range[[2L]]
    f4 <- !isTRUE(sv$interval_should_contain_primary_estimate) || inside; rs <- c(if (!f1) "success fraction below declared minimum", if (!f2) "glm non-convergence above declared maximum", if (!f3) "bootstrap SD outside declared ratio range of the reference SE", if (!f4) "interval does not contain the primary estimate")
    rows[[i]] <- data.frame(result_id = s$result_id, success_fraction = sf, glm_not_converged_fraction = cf, bootstrap_sd = d$bootstrap_sd, reference_se = ref, bootstrap_sd_to_reference_se = ratio, primary_coefficient = prim, ci_contains_primary_estimate = inside,
      median_minus_primary = s$estimate_median - prim, level_absent_fraction = d$level_absent_resamples / d$replicates_attempted, flag_success_fraction = f1, flag_glm_convergence = f2, flag_sd_ratio = f3, flag_interval_contains_primary = f4,
      validity_status = if (f1 && f2 && f3 && f4) "OK" else "REVIEW", review_reasons = paste(rs, collapse = "; "), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

#' Independent verification of every target (see config/bootstrap_protocol.json, mathematical_verification).
#' @export
siw_bootstrap_verify <- function(dat, config, run, point, protocol, replicates = 2000L, recompute_targets = protocol$mathematical_verification$determinism_targets_recomputed) {
  mv <- protocol$mathematical_verification; gr <- siw_bootstrap_groups(dat); plan <- run$plan; covs <- config$tiers[[config$primary_tier]]; rows <- list(); fam <- stats::binomial(); pos <- unlist(mv$legacy_style_refit_replicates); pos <- pos[pos <= replicates]
  nw <- length(config$windows); nm <- sum(config$metrics$representative); rel <- function(a, b) { if (is.na(a) && is.na(b)) 0 else if (is.na(a) || is.na(b)) Inf else abs(a - b) / max(1e-300, abs(b), 1e-12) }
  for (i in seq_len(nrow(plan))) {
    p <- plan[i, ]; r <- run$results[[p$target_id]]; ix <- siw_bootstrap_indices(gr, p$resampling_type, replicates, p$seed); chk <- siw_bootstrap_check_sampling(gr, p$resampling_type, replicates, p$seed, indices = ix)
    spec <- siw_bootstrap_design_spec(dat, config, p$outcome, p$outcome_family, p$exposure); fails <- character()
    ls <- vapply(pos, function(b) siw_bootstrap_legacy_style_fit(dat, ix[[b]], p$outcome, p$outcome_family, p$exposure, covs), 0); mine <- r$estimates[pos]
    d_ls <- mapply(rel, mine, ls); lsmax <- max(d_ls); lsmis <- sum(!(d_ls <= mv$legacy_style_refit_relative_tolerance))
    ok <- is.finite(r$estimates); s <- r$summary; q <- if (sum(ok) >= 20L) siw_quantile8(r$estimates[ok], c(0.025, 0.975)) else c(NA_real_, NA_real_); med <- if (any(ok)) { x <- sort(r$estimates[ok]); n <- length(x); if (n %% 2L) x[(n + 1L) / 2L] else (x[n / 2L] + x[n / 2L + 1L]) / 2 } else NA_real_
    dm <- rel(s$estimate_median, med); dl <- rel(s$ci_lower, q[1L]); du <- rel(s$ci_upper, q[2L])
    oi <- match(p$outcome, config$outcomes$outcome); mi <- match(p$metric, config$metrics$metric[config$metrics$representative]); wi <- match(p$window_or_lag, config$windows); es <- siw_bootstrap_expected_seed(config, oi, mi, wi, p$resampling_type)
    seed_ok <- identical(as.integer(es[["seed"]]), as.integer(p$seed)) && identical(as.integer(es[["index"]]), as.integer(p$target_index))
    cons <- s$replicates_attempted == replicates && s$replicates_successful + s$replicates_failed == s$replicates_attempted && sum(!nzchar(r$reason)) == s$replicates_successful && sum(nzchar(r$reason)) == s$replicates_failed
    full <- .siw_boot_fit(seq_len(nrow(dat)), spec, fam)$est; j <- match(paste(p$outcome, p$metric, p$window_or_lag), paste(point$outcome, point$metric, point$window_or_lag)); dfull <- if (is.na(j)) NA_real_ else rel(full, point$coefficient[j])
    det_done <- i <= recompute_targets; det_ok <- NA
    if (det_done) { again <- siw_bootstrap_target(gr, spec, p$resampling_type, replicates, p$seed); det_ok <- identical(again$estimates, r$estimates) && identical(again$reason, r$reason) }
    if (chk$violations > 0L) fails <- c(fails, "sampling_invariants"); if (chk$uniformity_max_abs_z > mv$sampling_uniformity_max_abs_z) fails <- c(fails, "sampling_uniformity"); if (lsmis > 0L) fails <- c(fails, "legacy_style_refit")
    if (!(dm <= mv$quantile_relative_tolerance && dl <= mv$quantile_relative_tolerance && du <= mv$quantile_relative_tolerance)) fails <- c(fails, "percentile_median"); if (!seed_ok) fails <- c(fails, "seed_rule"); if (!cons) fails <- c(fails, "conservation")
    if (!(is.finite(dfull) && dfull <= mv$point_estimate_relative_tolerance)) fails <- c(fails, "full_sample_estimate"); if (det_done && !isTRUE(det_ok)) fails <- c(fails, "determinism")
    rows[[i]] <- data.frame(result_id = p$target_id, status = if (length(fails)) "FAIL" else "PASS", failed_checks = paste(fails, collapse = "|"), sampling_replicates_checked = chk$replicates_checked, sampling_violations = chk$violations, sampling_uniformity_max_abs_z = chk$uniformity_max_abs_z,
      legacy_style_refits_compared = length(pos), legacy_style_max_relative_difference = lsmax, legacy_style_mismatches = lsmis, median_relative_difference = dm, ci_lower_relative_difference = dl, ci_upper_relative_difference = du, seed_rule_match = seed_ok, conservation_ok = cons,
      full_sample_estimate_relative_difference = dfull, determinism_recomputed = det_done, determinism_identical = det_ok, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

#' Summary of the verification table, one row per check family. NOT_TESTED is never PASS.
#' @export
siw_bootstrap_math_summary <- function(ver) {
  one <- function(check, bad, note, applicable = rep(TRUE, nrow(ver))) { n <- sum(applicable); nf <- sum(bad[applicable]); data.frame(check = check, n_targets = n, n_pass = n - nf, n_fail = nf, status = if (n == 0L) "NOT_TESTED" else if (nf == 0L) "PASS" else "FAIL", note = note, stringsAsFactors = FALSE) }
  m <- do.call(rbind, list(
    one("sampling_invariants_and_uniformity", ver$sampling_violations > 0L | ver$sampling_uniformity_max_abs_z > 5, "every replicate of every target: participant vectors valid; cluster vectors equal the concatenated full row sets of the sampled dates (all participants of every sampled occurrence retained); selection uniformity |z| <= 5"),
    one("legacy_style_refits_on_identical_resamples", ver$legacy_style_mismatches > 0L, "lm()/glm() through the formula interface on the resampled data frame vs the direct lm.fit/glm.fit estimate, 6 replicates per target, relative difference <= 1e-12"),
    one("independent_percentiles_and_median", !(ver$median_relative_difference <= 1e-12 & ver$ci_lower_relative_difference <= 1e-12 & ver$ci_upper_relative_difference <= 1e-12), "independent type-8 quantile (from the definition) and median vs the reported values, relative difference <= 1e-12"),
    one("seed_and_target_index_rule", !ver$seed_rule_match, "independent index/seed formula vs the plan"),
    one("conservation_of_replicates", !ver$conservation_ok, "attempted = requested; successful + failed = attempted; failure reasons account for every failure"),
    one("full_sample_estimate_equals_validated_estimate", !(is.finite(ver$full_sample_estimate_relative_difference) & ver$full_sample_estimate_relative_difference <= 1e-9), "the fit engine on the unresampled data equals the validated cumulative M2-L estimate, relative difference <= 1e-9"),
    one("deterministic_recomputation", !(ver$determinism_identical %in% TRUE), "two targets recomputed from scratch give identical estimates and failure classes", applicable = ver$determinism_recomputed %in% TRUE)))
  m
}

#' Run Batch C on one canonical CSV: bootstrap (180 targets), independent verification, validity assessment and strict comparison with the Batch C reference bundle.
#' Core, Batch A and Batch B are not rerun. Writes only to out_dir; the package-source manifest and fingerprint are recorded at the start and rechecked at the end.
#' @param accepted_run_dir optional accepted run folder whose cumulative_results.csv must equal the 90 recomputed M2-L point estimates (read only)
#' @param resume reuse valid target checkpoints found in out_dir/checkpoints; replicates/targets are for development tests (the manual run uses the configured 2,000 and all 180)
#' @export
siw_run_batch_c <- function(data_csv, out_dir, batch_c_reference_dir, config = siw_load_config(), accepted_run_dir = NULL, resume = FALSE, replicates = NULL, targets = NULL, label = NULL) {
  t_all <- proc.time()[["elapsed"]]; tm <- list(); tick <- function(name, expr) { t0 <- proc.time()[["elapsed"]]; v <- force(expr); tm[[name]] <<- proc.time()[["elapsed"]] - t0; v }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); options(sieccwinter.log_file = file.path(out_dir, "run.log")); protocol <- siw_bootstrap_protocol(); R <- as.integer(replicates %||% protocol$scope$replicates_requested)
  sm0 <- tick("source_manifest_start", siw_source_manifest()); siw_log("Batch C bootstrap; protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint, " (", sm0$n_files, " files)")
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = config$dlm$final$lag_end); siw_write_table(rep, "input_validation_report", out_dir)
  if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_attach_cumulative(siw_prepare_data(raw, config), config)$data
  point <- tick("point_estimates", { g <- siw_cumulative_grid(dat, config, tiers = config$primary_tier)$results; g <- g[g$metric %in% config$metrics$metric[config$metrics$representative], ]
    data.frame(outcome = g$outcome, metric = g$metric, window_or_lag = g$window_or_lag, model_tier = g$model_tier, n = g$n, coefficient = g$coefficient, robust_hc3_se = g$robust_hc3_se, model_based_se = g$model_based_se, nominal_p = g$nominal_p, stringsAsFactors = FALSE) })
  groups_n <- length(unique(dat$assessment_day)); siw_log("assessment-date groups: ", groups_n)
  sig <- c(input = ih$value, config = config$config_hash, protocol = protocol$protocol_hash, source = sm0$fingerprint)
  run <- tick("bootstrap", siw_bootstrap_run_targets(dat, config, R, ckpt_dir = file.path(out_dir, "checkpoints"), signature_parts = sig, resume = resume, targets = targets))
  tabs <- .siw_bootstrap_tables(run, point); tabs$validity <- siw_bootstrap_validity(tabs$summary, tabs$diagnostics, point, protocol)
  ver <- tick("verification", siw_bootstrap_verify(dat, config, run, point, protocol, replicates = R)); msum <- siw_bootstrap_math_summary(ver)
  bplan <- siw_load_batch_c_plan(); bv <- siw_validate_batch_c(list(bootstrap_summary = tabs$summary), config, batch_c_reference_dir, bplan)
  for (nm in c("bootstrap_point_estimates")) siw_write_table(point, nm, out_dir)
  siw_write_table(tabs$summary, "bootstrap_summary", out_dir); siw_write_table(tabs$failures, "bootstrap_failures", out_dir); siw_write_table(tabs$diagnostics, "bootstrap_diagnostics", out_dir); siw_write_table(tabs$validity, "bootstrap_validity", out_dir)
  siw_write_table(ver, "bootstrap_verification", out_dir); siw_write_table(msum, "bootstrap_math_verification", out_dir); siw_write_table(bv$summary, "batch_c_validation_summary", out_dir); siw_write_table(bv$detail, "batch_c_validation_detail", out_dir)
  # optional identity check of the point estimates with an accepted run (read only)
  pe_same <- NA
  if (!is.null(accepted_run_dir)) { f <- file.path(accepted_run_dir, "cumulative_results.csv"); if (file.exists(f)) { a <- utils::read.csv(f, stringsAsFactors = FALSE); a <- a[a$model_tier == config$primary_tier, ]; j <- match(paste(point$outcome, point$metric, point$window_or_lag), paste(a$outcome, a$metric, a$window_or_lag)); pe_same <- !anyNA(j) && all(abs(a$coefficient[j] - point$coefficient) <= 1e-12 * pmax(1, abs(point$coefficient))) } else pe_same <- FALSE }
  sm1 <- siw_source_manifest(); sd <- siw_source_manifest_diff(sm0, sm1); changed <- !isTRUE(sd$identical_fingerprint)
  siw_write_table(sm0$manifest, "source_manifest", out_dir); writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("ROOT_KIND=", sm0$root_kind), paste0("SCOPE=", sm0$scope), paste0("SOURCE_CHANGED_DURING_RUN=", toupper(as.character(changed))),
    if (changed) paste0("CHANGED_FILES=", paste(c(sd$changed, sd$added, sd$removed), collapse = ";")), "Computed at the start of this run and rechecked at its end. It identifies the package source of THIS run only; it is not assigned to earlier runs."), file.path(out_dir, "SOURCE_FINGERPRINT.txt"))
  siw_write_csv(data.frame(stage = names(tm), seconds = round(unlist(tm), 3)), file.path(out_dir, "batch_c_timings.csv"))
  base <- siw_overall_validation_status(bv$summary); repro <- if (base == "PASS") "PASS" else "FAIL"; mstat <- if (all(msum$status == "PASS")) "PASS" else if (any(msum$status == "FAIL")) "FAIL" else "NOT_TESTED"
  n_rev <- sum(tabs$validity$validity_status == "REVIEW"); full_scope <- is.null(targets) && identical(R, as.integer(protocol$scope$replicates_requested))
  st <- c(EXECUTION_STATUS = if (full_scope) "PASS" else "PASS_DEVELOPMENT_SUBSET", INPUT_CHECK_STATUS = if (any(rep$status == "FAIL")) "FAIL" else "PASS", NUMERICAL_REPRODUCTION_STATUS = repro, MATHEMATICAL_VERIFICATION_STATUS = mstat,
          SCIENTIFIC_VALIDITY_STATUS = if (n_rev == 0L) "PASS" else "REVIEW", SOURCE_INTEGRITY_STATUS = if (changed) "FAIL" else "PASS")
  writeLines(c(paste0(names(st), "=", st), paste0("TARGETS=", nrow(run$plan)), paste0("TARGETS_REVIEW=", n_rev), paste0("TARGETS_RESTORED_FROM_CHECKPOINT=", sum(run$status_by_target == "RESTORED")), paste0("REFERENCES_NOT_PASSED=", sum(bv$summary$required %in% TRUE & !grepl("^PASS", bv$summary$status))),
               paste0("POINT_ESTIMATES_EQUAL_ACCEPTED_RUN=", if (is.na(pe_same)) "NOT_REQUESTED" else toupper(as.character(pe_same))), "Results were not altered, redrawn or replaced to fit any reference."), file.path(out_dir, "BATCH_C_STATUS.txt"))
  ref_f <- unlist(lapply(bplan$references, function(r) unlist(r$files))); ref_f <- ref_f[file.exists(file.path(batch_c_reference_dir, ref_f))]
  meta <- list(package = "sieccwinter", package_version = siw_pkg_version(), batch = "C", protocol_hash = protocol$protocol_hash, batch_c_plan_hash = bplan$plan_hash, config_hash = config$config_hash, tolerances = config$tolerances, input_label = label %||% basename(data_csv), input_hash_algorithm = ih$algorithm,
               input_hash = ih$value, n_participants = nrow(raw), assessment_date_groups = groups_n, replicates_requested = R, targets = nrow(run$plan), full_scope = full_scope, rng = config$rng, seed = config$seed, statuses = as.list(st),
               batch_c_reference_files = as.list(stats::setNames(vapply(file.path(batch_c_reference_dir, ref_f), function(p) siw_file_hash(p)$value, ""), ref_f)), source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, source_scope = sm0$scope, source_root_kind = sm0$root_kind,
               source_changed_during_run = changed, r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), seconds = round(proc.time()[["elapsed"]] - t_all, 2))
  jsonlite::write_json(meta, file.path(out_dir, "batch_c_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA); writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "session_info.txt"))
  siw_write_manifest(out_dir); siw_log("Batch C done: ", paste(names(st), st, sep = "=", collapse = "; ")); options(sieccwinter.log_file = NULL)
  list(out_dir = out_dir, statuses = st, run = run, tables = tabs, verification = ver, math = msum, validation = bv, point = point, source = sm0, source_changed = changed,
       status = if (!full_scope) "DEVELOPMENT_SUBSET" else if (all(st[c("NUMERICAL_REPRODUCTION_STATUS", "MATHEMATICAL_VERIFICATION_STATUS", "SOURCE_INTEGRITY_STATUS")] == "PASS") && st[["INPUT_CHECK_STATUS"]] == "PASS") "OK" else "VERIFICATION_OR_REPRODUCTION_FAILURE")
}
