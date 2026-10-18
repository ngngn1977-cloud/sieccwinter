# Phase 3D orchestration: validation of the nine final L01-L56 distributed-lag models (lag-specific estimates and SEs, cumulative curves, pointwise and simultaneous bands, critical values, descriptive peaks).
# Reuses the accepted package fits (siw_final_dlm) and adds: exact reproduction of the accepted run's outputs, strict comparison with the audited L56 reconstruction, the independent verification route (R/dlm_verify.R),
# Monte Carlo stability of the simultaneous critical value, manuscript concordance, and honest NOT_TESTED rows for what has no reference. Declared in config/dlm_validation_protocol.json and config/validation_plan_dlm.json.

#' Load the declared DLM validation protocol (config/dlm_validation_protocol.json).
#' @export
siw_dlm_validation_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "dlm_validation_protocol.json"), file.path(root, "extdata", "dlm_validation_protocol.json"), file.path(root, "inst", "extdata", "dlm_validation_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_DLM_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

#' Load the DLM reference plan (config/validation_plan_dlm.json): which references are required, their keys, tolerance classes and record counts.
#' @export
siw_load_dlm_plan <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "validation_plan_dlm.json"), file.path(root, "extdata", "validation_plan_dlm.json"), file.path(root, "inst", "extdata", "validation_plan_dlm.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_DLM_PLAN_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$plan_path <- normalizePath(path, winslash = "/"); p$plan_hash <- siw_file_hash(path)$value; p
}

.siw_dlm_group_status <- function(x) if (!nrow(x)) "NOT_TESTED" else if (any(x$status == "FAIL")) "FAIL" else if (all(x$status == "PASS")) "PASS" else if (all(x$status %in% c("PASS", "NOT_TESTED"))) "PASS_PARTIAL_NOT_TESTED_ROWS" else "FAIL"

# manuscript statements about the final DLMs (Results, Distributed-lag associations); compared at their own rounding, never reconciled
.siw_dlm_manuscript <- function(s) {
  r3 <- function(v) round(v, 3L); r2 <- function(v) round(v, 2L); row <- function(st, ms, rec, ok) data.frame(statement = st, manuscript = ms, recomputed = rec, consistent_at_manuscript_rounding = ok, stringsAsFactors = FALSE)
  u <- function(o) s[s$outcome == o & s$metric == "UTCI", ]; out <- list()
  out[[1]] <- row("range of the nine global p values", "0.176 to 0.804", paste(r3(min(s$global_p)), "to", r3(max(s$global_p))), r3(min(s$global_p)) == 0.176 && r3(max(s$global_p)) == 0.804)
  out[[2]] <- row("range of q9", "0.616 to 0.804", paste(r3(min(s$q9)), "to", r3(max(s$q9))), r3(min(s$q9)) == 0.616 && r3(max(s$q9)) == 0.804)
  out[[3]] <- row("no q9 below 0.05 / all global p above 0.05", "none supported", paste("minimum q9", r3(min(s$q9))), all(s$q9 > 0.05) && all(s$global_p > 0.05))
  out[[4]] <- row("descriptive peak lags range", "21 to 56 days", paste(min(s$peak_lag), "to", max(s$peak_lag), "days"), min(s$peak_lag) == 21L && max(s$peak_lag) == 56L)
  out[[5]] <- row("no simultaneous band excludes zero in any of the nine models", "none", paste(sum(s$any_simultaneous_band_excludes_null %in% TRUE), "models"), !any(s$any_simultaneous_band_excludes_null %in% TRUE))
  for (k in list(list("UCLA_Lonliness_SCORE", -0.57, -1.40, 0.26), list("Isolation_cont", -1.34, -4.17, 1.49), list("Int_dummy", 1.53, -0.49, 3.50))) { z <- u(k[[1]])
    out[[length(out) + 1L]] <- row(paste0("cumulative UTCI estimate and 95% CI, ", k[[1]]), sprintf("%.2f (%.2f to %.2f)", k[[2]], k[[3]], k[[4]]), if (nrow(z)) sprintf("%.2f (%.2f to %.2f)", r2(z$cumulative_effect), r2(z$cumulative_ci_lower), r2(z$cumulative_ci_upper)) else "n/a",
      nrow(z) == 1L && r2(z$cumulative_effect) == k[[2]] && r2(z$cumulative_ci_lower) == k[[3]] && r2(z$cumulative_ci_upper) == k[[4]]) }
  do.call(rbind, out)
}

#' Run the Phase 3D DLM output validation on one canonical CSV. Reads the accepted run (optional) and the DLM reference bundle read-only; writes only to out_dir.
#' @param dlm_reference_dir reference bundle with the audited L56 reconstruction files named in config/validation_plan_dlm.json
#' @param accepted_run_dir optional accepted core run folder (dlm_final_summary.csv, dlm_final_lag_curves.csv, dlm_final_cumulative_curves.csv); without it ACCEPTED_FIT_REPRODUCTION is NOT_TESTED
#' @param mc run the Monte Carlo stability analysis (default TRUE)
#' @param manuscript_concordance compare with the manuscript statements (default TRUE; FALSE only for synthetic dry runs, where the comparison is meaningless: the status is then NOT_TESTED, never PASS)
#' @export
siw_run_dlm_validation <- function(data_csv, out_dir, dlm_reference_dir, config = siw_load_config(), accepted_run_dir = NULL, label = NULL, mc = TRUE, manuscript_concordance = TRUE) {
  t_all <- proc.time()[["elapsed"]]; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); options(sieccwinter.log_file = file.path(out_dir, "run.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE)
  protocol <- siw_dlm_validation_protocol(); plan <- siw_load_dlm_plan(); sm0 <- siw_source_manifest(); siw_log("Phase 3D DLM validation; protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint, " (", sm0$n_files, " files)")
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = config$dlm$final$lag_end); siw_write_csv(rep, file.path(out_dir, "input_validation_report.csv"))
  if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_prepare_data(raw, config); f <- config$dlm$final; xc <- siw_tier_matrix(dat, config$tiers[[f$tier]]); aud <- as.integer(protocol$criteria$audited_reconstruction$draws)
  siw_log("fitting the nine final DLMs at ", f$draws, " draws (accepted specification) and at ", aud, " draws (audited reconstruction)")
  fin <- siw_final_dlm(dat, config, f$draws); fin10 <- siw_final_dlm(dat, config, aud); S <- fin$summary
  if (nrow(S) != 9L || any(S$status != "COMPLETED")) stop("SIW_DLM_FINAL_NOT_ESTIMABLE: expected nine COMPLETED models", call. = FALSE)
  tol <- config$tolerances; W <- function(x, nm) siw_write_csv(x, file.path(out_dir, nm))
  # ---- 1. accepted-fit reproduction (read only)
  acc_s <- data.frame(); acc_d <- data.frame(); acc_status <- "NOT_TESTED"
  if (!is.null(accepted_run_dir)) {
    rdacc <- function(nm) { p <- file.path(accepted_run_dir, nm); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")) else NULL }
    pair <- list(list("dlm_final_summary", S, "result_id", 9L), list("dlm_final_lag_curves", fin$lag_curves, c("result_id", "lag"), 504L), list("dlm_final_cumulative_curves", fin$cumulative_curves, c("result_id", "lag"), 504L)); sm <- list(); dt <- list()
    for (z in pair) { a <- rdacc(paste0(z[[1]], ".csv")); tl <- tol; tl$name <- "exact"
      r <- if (is.null(a)) list(summary = .siw_summary_row(z[[1]], "FAIL", "", "accepted-run file is missing", TRUE), detail = data.frame()) else siw_compare_tables(z[[2]], a, z[[3]], tl, name = z[[1]], reference_file = paste0(z[[1]], ".csv"), expected_records = z[[4]], required = TRUE)
      sm[[length(sm) + 1L]] <- r$summary; dt[[length(dt) + 1L]] <- r$detail }
    acc_s <- do.call(rbind, sm); acc_d <- siw_bind_rows(dt); acc_status <- if (all(grepl("^PASS", acc_s$status))) "PASS" else "FAIL"
  }
  W(acc_s, "dlm_accepted_run_comparison.csv"); W(acc_d, "dlm_accepted_run_comparison_detail.csv")
  # ---- 2. strict comparison with the audited L56 reconstruction (draws = audited count for the discrete and critical parts)
  s10 <- fin10$summary; tabs <- list(dlm_estimates = S[, c("outcome", "metric", "n", "global_statistic", "global_p", "q9", "cumulative_effect", "cumulative_se", "cumulative_ci_lower", "cumulative_ci_upper", "peak_effect")],
    dlm_discrete = s10[, c("outcome", "metric", "peak_lag", "n_lags_pointwise_excl_null", "n_lags_simultaneous_excl_null")], dlm_critical_10000 = s10[, c("outcome", "metric", "simultaneous_critical")])
  cfg2 <- config; cfg2$tolerances$seeded_abs <- 1e-9; cfg2$tolerances$seeded_rel <- 1e-9; rs <- list(); rd_ <- list()
  for (pl in plan$references) { r <- .siw_validate_one(tabs, cfg2, dlm_reference_dir, list(name = pl$name, files = pl$files, keys = pl$keys, result = pl$result, tolerance = pl$tolerance, aliases = list(), plan = pl)); rs[[length(rs) + 1L]] <- r$summary; rd_[[length(rd_) + 1L]] <- r$detail }
  ref_s <- do.call(rbind, rs); ref_d <- siw_bind_rows(rd_); ref_status <- siw_overall_validation_status(ref_s); ref_s$evidence_class <- vapply(plan$references, function(z) z$evidence_class, "")[match(ref_s$reference, vapply(plan$references, function(z) z$name, ""))]
  W(ref_s, "dlm_reference_validation_summary.csv"); W(ref_d, "dlm_reference_validation_detail.csv")
  # ---- 3. lag basis
  bas <- siw_dlm_verify_basis(f$lag_end, f$lag_df); W(bas, "dlm_basis_verification.csv"); is_dlnm <- grepl("dlnm", bas$check)
  # ---- 4. independent verification, model by model
  chk <- list(); crit <- list(); peak <- list(); bands <- list(); mcl <- list(); mcs <- list()
  for (i in seq_len(nrow(S))) { id <- S$result_id[i]; siw_log("independent verification [", i, "/9] ", id)
    pkg <- list(summary = S[i, , drop = FALSE], lag_curve = fin$lag_curves[fin$lag_curves$result_id == id, ], cumulative_curve = fin$cumulative_curves[fin$cumulative_curves$result_id == id, ])
    v <- siw_dlm_verify_model(dat, config, pkg, s10[s10$result_id == id, , drop = FALSE], S$outcome[i], S$metric[i], protocol, mc = mc, x_cov = xc)
    chk[[i]] <- v$checks; crit[[i]] <- v$critical; peak[[i]] <- v$peak; bands[[i]] <- v$bands; mcl[[i]] <- v$mc }
  C <- do.call(rbind, chk); W(C, "dlm_verification_checks.csv"); W(siw_bind_rows(crit), "dlm_critical_values.csv"); Pk <- siw_bind_rows(peak); W(Pk, "dlm_peak_lags.csv"); W(siw_bind_rows(bands), "dlm_band_exclusions.csv"); M <- siw_bind_rows(mcl); W(M, "dlm_mc_stability.csv")
  grp <- function(g) .siw_dlm_group_status(C[C$group == g, ])
  mc_status <- if (!isTRUE(mc)) "NOT_TESTED" else if (nrow(M) == 9L && all(M$status == "PASS") && .siw_dlm_group_status(C[C$group == "SIMULTANEOUS_MC_STABILITY", ]) == "PASS") "PASS" else "FAIL"
  man <- if (isTRUE(manuscript_concordance)) .siw_dlm_manuscript(S) else data.frame(statement = "all manuscript statements", manuscript = "not compared", recomputed = "not compared", consistent_at_manuscript_rounding = NA, stringsAsFactors = FALSE); W(man, "dlm_manuscript_concordance.csv")
  man_status <- if (!isTRUE(manuscript_concordance)) "NOT_TESTED" else if (all(man$consistent_at_manuscript_rounding)) "PASS" else "REVIEW"
  nt <- data.frame(component = c("Figure 6 panel reproduction", "Figure 6 provenance (plotting data, panel-to-model mapping)", "dlnm basis equality (if dlnm is not installed)", if (!isTRUE(manuscript_concordance)) "Manuscript concordance (skipped for a synthetic dry run)"),
    status = c("NOT_TESTED", "NOT_TESTED", if (is.na(bas$value[is_dlnm])) "NOT_TESTED" else bas$status[is_dlnm], if (!isTRUE(manuscript_concordance)) "NOT_TESTED"),
    reason = c("no figure reference or deposited plotting data; figures are not redrawn in this batch", "no deposited provenance for the figure data; the curves and bands underlying the figure are verified numerically, the figure itself is not", if (is.na(bas$value[is_dlnm])) "dlnm is not installed in the validation environment; equality with dlnm is recorded evidence from Phase 1 (1.8e-12), not re-tested" else "tested", if (!isTRUE(manuscript_concordance)) "synthetic dry run: the manuscript statements cannot apply to synthetic data; the comparison was not made and is not counted as passed"), stringsAsFactors = FALSE)
  W(nt, "dlm_not_tested.csv"); W(data.frame(id = vapply(protocol$conflicts_and_gaps_reported_not_resolved, function(z) z$id, ""), statement = vapply(protocol$conflicts_and_gaps_reported_not_resolved, function(z) z$statement, ""), handling = vapply(protocol$conflicts_and_gaps_reported_not_resolved, function(z) z$handling, ""), stringsAsFactors = FALSE), "dlm_conflicts_reported.csv")
  W(data.frame(file = vapply(protocol$document_versions, function(z) z$file, ""), sha256_recorded = vapply(protocol$document_versions, function(z) z$sha256, ""), bytes = vapply(protocol$document_versions, function(z) z$bytes, 0), core_modified = vapply(protocol$document_versions, function(z) z$core_modified, ""),
    core_revision = vapply(protocol$document_versions, function(z) as.numeric(z$core_revision), 0), note = "recorded in the declared protocol; not recomputed in this run (documents are not an input of the run)", stringsAsFactors = FALSE), "dlm_document_provenance.csv")
  # ---- 5. source integrity and statuses
  sm1 <- siw_source_manifest(); sdiff <- siw_source_manifest_diff(sm0, sm1); changed <- !isTRUE(sdiff$identical_fingerprint); W(sm0$manifest, "source_manifest.csv")
  basis_rows <- bas[!is_dlnm, ]; basis_rows$group <- "LAG_BASIS_VERIFICATION"
  st <- c(EXECUTION_STATUS = "PASS", INPUT_CHECK_STATUS = if (any(rep$status == "FAIL")) "FAIL" else "PASS", ACCEPTED_FIT_REPRODUCTION_STATUS = acc_status, REFERENCE_COMPARISON_STATUS = ref_status, LAG_BASIS_VERIFICATION_STATUS = .siw_dlm_group_status(basis_rows),
          CURVE_VERIFICATION_STATUS = grp("CURVE_VERIFICATION"), CUMULATIVE_VERIFICATION_STATUS = grp("CUMULATIVE_VERIFICATION"), PEAK_LAG_VERIFICATION_STATUS = grp("PEAK_LAG_VERIFICATION"), SIMULTANEOUS_EXACT_SEEDED_STATUS = grp("SIMULTANEOUS_EXACT_SEEDED"),
          SIMULTANEOUS_MC_STABILITY_STATUS = mc_status, CONVERGENCE_SENSITIVITY_STATUS = grp("CONVERGENCE_SENSITIVITY"), MANUSCRIPT_CONCORDANCE_STATUS = man_status, FIGURE_PROVENANCE_STATUS = "NOT_TESTED", DLNM_EQUALITY_STATUS = nt$status[3L], SOURCE_INTEGRITY_STATUS = if (changed) "FAIL" else "PASS")
  writeLines(c(paste0(names(st), "="  , st), paste0("CRITICAL_VALUES_EXACT_AGAINST_INDEPENDENT_ROUTE_AT_ACCEPTED_EPSILON=", sum(vapply(crit, function(z) identical(z$cross_implementation_class, "EXACT"), NA)), "_OF_9"), "L56_REFERENCE_EVIDENCE=AUDITED_L56_RECONSTRUCTION (Phase 1 legacy-engine reconstruction cross-checked by independent re-derivation; not an original deposited result)", paste0("MODELS=", nrow(S)), paste0("ARCHIVED_L59_ANALYSES=NOT_TOUCHED"), "Results were not altered, redrawn or replaced to fit any reference."), file.path(out_dir, "DLM_VALIDATION_STATUS.txt"))
  writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("ROOT_KIND=", sm0$root_kind), paste0("SCOPE=", sm0$scope), paste0("SOURCE_CHANGED_DURING_RUN=", toupper(as.character(changed))), if (changed) paste0("CHANGED_FILES=", paste(c(sdiff$changed, sdiff$added, sdiff$removed), collapse = ";"))), file.path(out_dir, "SOURCE_FINGERPRINT.txt"))
  meta <- list(package = "sieccwinter", package_version = siw_pkg_version(), batch = "3D_DLM_output_validation", protocol_hash = protocol$protocol_hash, plan_hash = plan$plan_hash, config_hash = config$config_hash, input_label = label %||% basename(data_csv), input_hash_algorithm = ih$algorithm, input_hash = ih$value, n_participants = nrow(raw),
    dlm_final = config$dlm$final, rng = config$rng, seed = config$seed, tolerances = tol, reference_files = as.list(stats::setNames(vapply(file.path(dlm_reference_dir, unique(unlist(lapply(plan$references, function(r) unlist(r$files))))), function(p) if (file.exists(p)) siw_file_hash(p)$value else NA_character_, ""), unique(unlist(lapply(plan$references, function(r) unlist(r$files)))))),
    accepted_run_supplied = !is.null(accepted_run_dir), accepted_run = if (is.null(accepted_run_dir)) NA else basename(accepted_run_dir), statuses = as.list(st), source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, source_root_kind = sm0$root_kind, source_changed_during_run = changed, r_version = R.version.string, mc_run = isTRUE(mc),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), seconds = round(proc.time()[["elapsed"]] - t_all, 2))
  jsonlite::write_json(meta, file.path(out_dir, "dlm_validation_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA); writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "session_info.txt"))
  L <- c("# Phase 3D DLM output validation (nine final L01-L56 M2-L models)", "", paste0("* Protocol hash `", protocol$protocol_hash, "`; plan hash `", plan$plan_hash, "`; package source fingerprint `", sm0$fingerprint, "` (", sm0$n_files, " files)"), paste0("* Input hash `", ih$value, "`; participants: ", nrow(raw)), "",
    "## Statuses (separate; NOT_TESTED is never PASS)", "", paste0("* ", names(st), ": ", st), "", "## Evidence labels", "", "* L56 reference = AUDITED_L56_RECONSTRUCTION (Phase 1 reconstruction, not an original deposited result); archived L59 analyses are not touched; figures are not redrawn.",
    "* Exact seeded reproduction (same seed and draw count) is reported separately from Monte Carlo stability (alternative seeds and a 100,000-draw value).", "", "## Not tested", "", paste0("* ", nt$component, ": ", nt$status, " - ", nt$reason), "", "## Manuscript concordance (descriptive; discrepancies reported, never reconciled)", "", paste0("* ", man$statement, ": manuscript ", man$manuscript, "; recomputed ", man$recomputed, "; consistent: ", man$consistent_at_manuscript_rounding),
    "", "Nothing in this folder contains participant-level values.")
  writeLines(L, file.path(out_dir, "RUN_SUMMARY.md"))
  list(out_dir = out_dir, statuses = st, summary = S, checks = C, reference = list(summary = ref_s, detail = ref_d), accepted = acc_s, basis = bas, mc = M, peak = Pk, manuscript = man, not_tested = nt, source_changed = changed,
       status = if (any(st %in% c("FAIL", "REVIEW"))) "VERIFICATION_FAILURE_OR_REVIEW" else if (any(st == "NOT_TESTED" | grepl("^PASS_PARTIAL", st))) "OK_WITH_NOT_TESTED" else "OK")
}
