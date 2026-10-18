# Phase 3D validation protocol V2 (candidate), declared in config/dlm_validation_protocol_v2.json. Introduced AFTER the orientation investigation (PHASE3D2-PHASE3D4); V1 is preserved unchanged.
# V2 reuses the existing V1 validation (siw_run_dlm_validation) and the existing simulation diagnostic (siw_run_dlm_simdiag) without changing any calculation, tolerance, criterion, seed or draw count, and re-expresses their evidence as four separate checks:
#   A production reproduction, B deterministic agreement, C orientation-aligned simulation agreement, D native-orientation Monte Carlo agreement; plus the reconstructed reference split into deterministic and simulated parts.
# A reference value is never validated by a matching sign pattern alone and no claim is made about the historical engine.

#' Load the V2 candidate protocol (config/dlm_validation_protocol_v2.json).
#' @export
siw_dlm_validation_protocol_v2 <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "dlm_validation_protocol_v2.json"), file.path(root, "extdata", "dlm_validation_protocol_v2.json"), file.path(root, "inst", "extdata", "dlm_validation_protocol_v2.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_DLM_PROTOCOL_V2_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

.siw_v2_rows <- function(model, cls, check, value, criterion, ok) data.frame(model = model, check_class = cls, check = check, value = value, criterion = criterion, status = ifelse(is.na(ok), "NOT_TESTED", ifelse(ok, "PASS", "FAIL")), stringsAsFactors = FALSE)
.siw_v2_all <- function(rows) if (!nrow(rows)) "NOT_TESTED" else if (any(rows$status == "FAIL")) "FAIL" else if (all(rows$status == "PASS")) "PASS" else "NOT_TESTED"

# B: deterministic agreement (V1 fitted-quantity checks, band formula rows, covariance on the standard-deviation scale, q9)
.siw_v2_status_B <- function(checks, basis, cov, q9, fam, v1p) {
  ci <- v1p$criteria$independent_route; keep <- checks$group %in% c("CURVE_VERIFICATION", "CUMULATIVE_VERIFICATION", "PEAK_LAG_VERIFICATION", "CONVERGENCE_SENSITIVITY") | (checks$group == "SIMULTANEOUS_EXACT_SEEDED" & !grepl("critical_value", checks$quantity)); ck <- checks[keep, ]
  rows <- .siw_v2_rows(ck$result_id, "B", paste(ck$group, ck$quantity, sep = ": "), ck$max_rel_diff, ck$criterion, ck$status == "PASS")
  b <- basis[!grepl("dlnm", basis$check), ]; rows <- rbind(rows, .siw_v2_rows("ALL", "B", paste("LAG_BASIS:", b$check), b$value, b$criterion, b$status == "PASS"))
  tol <- ifelse(fam[cov$result_id] == "binomial", ci$binomial_fitted_quantities_rel_accepted_epsilon, ci$fitted_quantities_rel)
  rows <- rbind(rows, .siw_v2_rows(cov$result_id, "B", "lag-space covariance, standard-deviation scale (half of the largest relative difference)", cov$lag_space_relative_difference / 2, paste0("<= ", tol, " (existing relative fitted-quantity tolerance)"), cov$lag_space_relative_difference / 2 <= tol))
  ok_q9 <- abs(q9$package - q9$independent) <= ci$fitted_quantities_abs + ci$fitted_quantities_rel * abs(q9$independent); rows <- rbind(rows, .siw_v2_rows(q9$result_id, "B", "q9 (BH of the independent p values) equals the package q9", abs(q9$package - q9$independent), "abs <= 1e-8 + 1e-7 x |reference|", ok_q9))
  list(status = .siw_v2_all(rows), rows = rows)
}

# A: production reproduction
.siw_v2_status_A <- function(accepted_status, replica) {
  ok <- replica$standard_normals_identical_between_routes & replica$production_draws_reproduced_bitwise & replica$production_critical_reproduced_bitwise & replica$independent_clone_critical_difference <= 1e-12 & (is.na(replica$difference_to_saved_production) | replica$difference_to_saved_production <= 1e-12)
  rows <- .siw_v2_rows(replica$result_id, "A", paste0("production draws and critical value reproduced bitwise (", replica$draws, " draws)"), replica$production_critical, "bitwise; saved production value within 1e-12", ok)
  rows <- rbind(.siw_v2_rows("ALL", "A", "accepted-run tables reproduced (exact class 1e-12)", NA_real_, "9 / 504 / 504 records exact", if (identical(accepted_status, "NOT_TESTED")) NA else identical(accepted_status, "PASS")), rows)
  list(status = .siw_v2_all(rows), rows = rows)
}

# C: orientation-aligned simulation agreement
.siw_v2_status_C <- function(rec, mech, align, crit, tol_rec = 1e-12) {
  m <- merge(crit[, c("result_id", "draws", "difference_aligned", "difference_production")], mech[, c("result_id", "draws", "allowance_for_continuous_covariance_effect", "discrepancy_present", "explained_by_orientation")], by = c("result_id", "draws"))
  rows <- .siw_v2_rows(m$result_id, "C", paste0("aligned-eigen critical values of the two routes agree within the covariance allowance (", m$draws, " draws)"), m$difference_aligned, paste0("<= ", signif(m$allowance_for_continuous_covariance_effect, 3)), m$difference_aligned <= m$allowance_for_continuous_covariance_effect)
  dis <- m[m$discrepancy_present, ]; if (nrow(dis)) rows <- rbind(rows, .siw_v2_rows(dis$result_id, "C", paste0("production cross-route difference explained by orientation (", dis$draws, " draws)"), dis$difference_production, "flip of the lag-space cosine -1 components reproduces the production value within the allowance", dis$explained_by_orientation %in% TRUE))
  rr <- stats::aggregate(cbind(reconstruction_relative_error_coefficient_space, reconstruction_relative_error_lag_space) ~ result_id, rec, max); rows <- rbind(rows, .siw_v2_rows(rr$result_id, "C", "every covariance factor reconstructs its covariance", pmax(rr$reconstruction_relative_error_coefficient_space, rr$reconstruction_relative_error_lag_space), paste0("<= ", tol_rec), pmax(rr$reconstruction_relative_error_coefficient_space, rr$reconstruction_relative_error_lag_space) <= tol_rec))
  amb <- align[align$class == "ORIENTATION_DIFFERS", ]; rows <- rbind(rows, .siw_v2_rows("ALL", "C", "no component with an unresolved orientation (lag-space cosine neither +1 nor -1)", nrow(amb), "0 components", nrow(amb) == 0L))
  list(status = .siw_v2_all(rows), rows = rows)
}

# D: native-orientation Monte Carlo agreement (V1 criteria unchanged)
.siw_v2_status_D <- function(mc, checks, mc_run) {
  if (!isTRUE(mc_run)) return(list(status = "NOT_TESTED", rows = .siw_v2_rows("ALL", "D", "Monte Carlo stability not run (--no-mc)", NA_real_, "V1 stability criteria", NA)))
  rows <- .siw_v2_rows(mc$result_id, "D", "Monte Carlo stability (deviation from the 100,000-draw value <= 5%, spread <= 0.2, identical exclusion counts)", mc$relative_deviation_package_vs_large, "V1 criteria, unchanged", mc$status == "PASS")
  g <- checks[checks$group == "SIMULTANEOUS_MC_STABILITY", ]; rows <- rbind(rows, .siw_v2_rows(g$result_id, "D", paste0("cross-implementation Monte Carlo class (", g$classification, "): ", sub("^critical_value_", "", g$quantity)), g$max_rel_diff, g$criterion, g$status == "PASS"))
  list(status = .siw_v2_all(rows), rows = rows)
}

# E: reconstructed reference
.siw_v2_status_E <- function(refsum, crit, v1p) {
  det <- refsum[refsum$reference %in% c("dlmL56_audited_estimates", "dlmL56_audited_discrete"), ]; rowsD <- .siw_v2_rows(det$reference, "E", "deterministic reference comparison at the existing tolerances", det$n_different, "no difference beyond the V1 tolerance class; required", grepl("^PASS", det$status))
  bound <- v1p$criteria$simultaneous_mc_stability$max_relative_difference_cross_implementation_not_exact; z <- crit[crit$draws == max(crit$draws), ]; rel <- abs(z$reference_critical - z$production_package) / z$production_package; discrete_ok <- all(grepl("^PASS", det$status[det$reference == "dlmL56_audited_discrete"]))
  rowsS <- .siw_v2_rows(z$result_id, "E", "audited critical value (10,000 draws) within the Monte Carlo bound of the production value; band-exclusion counts identical (discrete reference)", rel, paste0("relative difference <= ", bound, " and discrete reference PASS"), if (all(is.na(rel))) NA else is.finite(rel) & rel <= bound & discrete_ok)
  list(det = list(status = .siw_v2_all(rowsD), rows = rowsD), sim = list(status = .siw_v2_all(rowsS), rows = rowsS))
}

#' Run the Phase 3D validation protocol V2 candidate on one canonical CSV. Reuses the V1 validation and the simulation diagnostic (nested, unchanged) and writes aggregate outputs to a new folder; V1 results and records are not altered.
#' @param dlm_reference_dir DLM reference bundle; accepted_run_dir accepted core run (optional: without it production reproduction is NOT_TESTED)
#' @export
siw_run_dlm_validation_v2 <- function(data_csv, out_dir, dlm_reference_dir, config = siw_load_config(), accepted_run_dir = NULL, label = NULL, mc = TRUE, manuscript_concordance = TRUE) {
  t0 <- proc.time()[["elapsed"]]; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); p2 <- siw_dlm_validation_protocol_v2(); v1p <- siw_dlm_validation_protocol(); plan <- siw_load_dlm_plan(); sdp <- siw_dlm_simdiag_protocol(); sm0 <- siw_source_manifest()
  options(sieccwinter.log_file = file.path(out_dir, "run_v2.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE); siw_log("Phase 3D validation V2 candidate; protocol ", p2$protocol_hash, "; source fingerprint ", sm0$fingerprint)
  v1dir <- file.path(out_dir, "v1_component"); sddir <- file.path(out_dir, "simulation_diagnostic")
  v1 <- siw_run_dlm_validation(data_csv, v1dir, dlm_reference_dir, config, accepted_run_dir = accepted_run_dir, label = label, mc = mc, manuscript_concordance = manuscript_concordance)
  sdg <- siw_run_dlm_simdiag(data_csv, sddir, v1dir, dlm_reference_dir, config, label = label); options(sieccwinter.log_file = file.path(out_dir, "run_v2.log"))
  # aggregate matrices, eigen orientation, RNG metadata (re-using the existing capture and independent fit)
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); dat <- siw_prepare_data(raw, config); xc <- siw_tier_matrix(dat, config$tiers[[config$dlm$final$tier]]); S <- v1$summary; draws_vec <- c(as.integer(config$dlm$final$draws), as.integer(v1p$criteria$audited_reconstruction$draws))
  covl <- list(); eig <- list(); rng <- list(); q9i <- numeric(nrow(S)); pind <- numeric(nrow(S)); fam <- stats::setNames(S$outcome_family, S$result_id)
  for (i in seq_len(nrow(S))) { o <- S$outcome[i]; m <- S$metric[i]; id <- S$result_id[i]; cp <- siw_dlm_sim_capture(dat, config, o, m, draws_vec[1L], xc); fi <- siw_dlm_indep_fit(dat, config, o, m, config$dlm$final$lag_end, config$dlm$final$lag_df, config$dlm$final$tier, x_cov = xc); pind[i] <- fi$global_p
    rts <- list(production = list(S = cp$Sigma, B = cp$B), independent = list(S = fi$Vtheta, B = fi$B)); vs <- list()
    for (r in names(rts)) { Sg <- rts[[r]]$S; Bm <- rts[[r]]$B; covl[[length(covl) + 1L]] <- data.frame(result_id = id, route = r, row = rep(1:3, 3L), col = rep(1:3, each = 3L), value = as.vector(Sg), stringsAsFactors = FALSE)
      e <- eigen(Sg, symmetric = TRUE); b <- Bm %*% e$vectors; li <- apply(b, 2L, function(z) which.max(abs(z))); cs <- vapply(seq_len(3L), function(j) sign(b[li[j], j]), 0); vs[[r]] <- b
      eig[[length(eig) + 1L]] <- data.frame(result_id = id, route = r, component = 1:3, eigenvalue = e$values, lag_space_largest_component_lag = li, native_sign_of_largest_component = cs, native_orientation_satisfies_convention = cs > 0, aligned_orientation_flip_applied = cs < 0, stringsAsFactors = FALSE) }
    cs2 <- vapply(1:3, function(j) sum(vs$production[, j] * vs$independent[, j]) / sqrt(sum(vs$production[, j]^2) * sum(vs$independent[, j]^2)), 0); idx <- which(vapply(eig, function(z) z$result_id[1L] == id, NA)); for (k in idx) eig[[k]]$cosine_lag_space_between_routes <- cs2; for (k in idx) eig[[k]]$flipped_between_routes <- cs2 < 0
    rk <- siw_with_seed(as.integer(S$seed[i]), RNGkind()); rng[[i]] <- data.frame(result_id = id, seed = S$seed[i], seed_rule = config$dlm$final$seed_rule, draws_final = draws_vec[1L], draws_audited = draws_vec[2L], rng_kind = rk[1L], normal_kind = rk[2L], sample_kind = rk[3L], mvn_method = config$dlm$final$mvn_method, quantile_type = config$dlm$final$quantile_type, confidence = config$dlm$final$confidence, standard_normal_layout = "matrix(rnorm(p * n), n), row i = draw i", sign_convention = "lag space: component of B v with the largest absolute value positive", stringsAsFactors = FALSE) }
  q9i <- siw_bh(pind); q9 <- data.frame(result_id = S$result_id, package = S$q9, independent = q9i, stringsAsFactors = FALSE)
  envj <- list(r_version = R.version.string, platform = R.version$platform, blas = unname(extSoftVersion()["BLAS"]), lapack_library = tryCatch(La_library(), error = function(e) NA_character_), lapack_version = tryCatch(La_version(), error = function(e) NA_character_), locale = Sys.getlocale("LC_COLLATE"), package_version = siw_pkg_version())
  siw_write_csv(do.call(rbind, covl), file.path(out_dir, "dlm_v2_covariance_matrices.csv")); EG <- do.call(rbind, eig); siw_write_csv(EG, file.path(out_dir, "dlm_v2_eigen_orientation.csv")); siw_write_csv(do.call(rbind, rng), file.path(out_dir, "dlm_v2_rng_metadata.csv")); jsonlite::write_json(envj, file.path(out_dir, "dlm_v2_environment.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  # the four checks and the reference
  stA <- .siw_v2_status_A(v1$statuses[["ACCEPTED_FIT_REPRODUCTION_STATUS"]], sdg$replica); stB <- .siw_v2_status_B(v1$checks, v1$basis, sdg$cov, q9, fam, v1p); stC <- .siw_v2_status_C(sdg$rec, sdg$mech, sdg$align, sdg$crit); stD <- .siw_v2_status_D(v1$mc, v1$checks, mc); stE <- .siw_v2_status_E(v1$reference$summary, sdg$crit, v1p)
  checks <- rbind(stA$rows, stB$rows, stC$rows, stD$rows, stE$det$rows, stE$sim$rows); siw_write_csv(checks, file.path(out_dir, "dlm_v2_checks.csv"))
  # exact differences stay visible, with orientation explanation; reference pattern is annotation only
  cr <- sdg$crit; ann <- vapply(seq_len(nrow(cr)), function(j) { if (is.na(cr$reference_critical[j])) return(NA_character_); z <- sdg$pat[sdg$pat$result_id == cr$result_id[j] & sdg$pat$draws == cr$draws[j] & sdg$pat$route == "production" & (sdg$pat$matches_reference_critical %in% TRUE), ]; if (nrow(z)) paste(z$sign_pattern, collapse = ",") else "none" }, "")
  mm <- merge(cr, sdg$mech[, c("result_id", "draws", "allowance_for_continuous_covariance_effect", "discrepancy_present", "explained_by_orientation")], by = c("result_id", "draws"), sort = FALSE); bnd <- v1p$criteria$simultaneous_mc_stability$max_relative_difference_cross_implementation_not_exact
  X <- data.frame(result_id = mm$result_id, family = mm$family, draws = mm$draws, seed = mm$seed, production_package = mm$production_package, production_independent = mm$production_independent, exact_difference_production_routes = mm$difference_production, v1_exactness_band = mm$production_band, aligned_difference = mm$difference_aligned, covariance_allowance = mm$allowance_for_continuous_covariance_effect,
    flipped_components_between_routes = mm$flipped_components, difference_beyond_allowance = mm$discrepancy_present, explained_by_orientation = mm$explained_by_orientation, reference_critical = mm$reference_critical, exact_difference_reference_vs_production = mm$difference_reference_vs_production, reference_v1_band = vapply(mm$difference_reference_vs_production, .siw_sd_band, ""), reference_orientation_annotation_only = ann[match(paste(mm$result_id, mm$draws), paste(cr$result_id, cr$draws))],
    native_relative_difference = mm$difference_production / mm$production_package, native_monte_carlo_bound = bnd, native_within_monte_carlo_bound = mm$difference_production / mm$production_package <= bnd, aligned_within_allowance = mm$difference_aligned <= mm$allowance_for_continuous_covariance_effect, stringsAsFactors = FALSE)
  siw_write_csv(X, file.path(out_dir, "dlm_v2_exact_differences_visible.csv"))
  siw_write_csv(data.frame(v1_check = vapply(p2$check_crosswalk, function(z) z$v1_check, ""), v2_check = vapply(p2$check_crosswalk, function(z) z$v2_check, ""), change = vapply(p2$check_crosswalk, function(z) z$change, ""), rationale = vapply(p2$check_crosswalk, function(z) z$rationale, ""), criteria_changed = vapply(p2$check_crosswalk, function(z) z$criteria_changed, NA), stringsAsFactors = FALSE), file.path(out_dir, "dlm_v2_check_crosswalk.csv"))
  sm1 <- siw_source_manifest(); changed <- !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint)
  st <- c(EXECUTION_STATUS = "PASS", INPUT_CHECK_STATUS = v1$statuses[["INPUT_CHECK_STATUS"]], PRODUCTION_REPRODUCTION_STATUS = stA$status, DETERMINISTIC_AGREEMENT_STATUS = stB$status, ORIENTATION_ALIGNED_SIMULATION_STATUS = stC$status, NATIVE_ORIENTATION_MC_STATUS = stD$status, REFERENCE_DETERMINISTIC_STATUS = stE$det$status, REFERENCE_SIMULATION_MC_STATUS = stE$sim$status,
    SOURCE_INTEGRITY_STATUS = if (changed || v1$statuses[["SOURCE_INTEGRITY_STATUS"]] != "PASS" || sdg$statuses[["SOURCE_INTEGRITY_STATUS"]] != "PASS") "FAIL" else "PASS", MANUSCRIPT_CONCORDANCE_STATUS = v1$statuses[["MANUSCRIPT_CONCORDANCE_STATUS"]], FIGURE_PROVENANCE_STATUS = v1$statuses[["FIGURE_PROVENANCE_STATUS"]], DLNM_EQUALITY_STATUS = v1$statuses[["DLNM_EQUALITY_STATUS"]])
  req <- st[unlist(p2$statuses$required)]; rep <- st[unlist(p2$statuses$reported_separately)]; v1rec <- c(V1_REFERENCE_COMPARISON_STATUS = v1$statuses[["REFERENCE_COMPARISON_STATUS"]], V1_SIMULTANEOUS_EXACT_SEEDED_STATUS = v1$statuses[["SIMULTANEOUS_EXACT_SEEDED_STATUS"]])
  overall <- if (any(req == "FAIL")) "VERIFICATION_FAILURE" else if (any(req == "NOT_TESTED") || any(rep %in% c("REVIEW", "NOT_TESTED"))) "OK_WITH_REVIEW_OR_NOT_TESTED" else "OK"
  writeLines(c(paste0(names(st), "=", st), paste0(names(v1rec), "=", v1rec), paste0("DISCREPANCIES_BEYOND_ALLOWANCE=", sum(X$difference_beyond_allowance)), paste0("EXPLAINED_BY_ORIENTATION=", sum(X$difference_beyond_allowance & X$explained_by_orientation %in% TRUE)), paste0("EXACT_DIFFERENCES_VISIBLE_IN=dlm_v2_exact_differences_visible.csv"),
    "L56_REFERENCE_EVIDENCE=AUDITED_L56_RECONSTRUCTION (not an original deposited result; no claim of exact historical reproduction)", "V1_RECORD_LINES=computed with the unchanged V1 criteria for the record; they do not decide V2 acceptance", "Results were not altered, redrawn or replaced to fit any reference."), file.path(out_dir, "DLM_V2_STATUS.txt"))
  meta <- list(package_version = siw_pkg_version(), batch = "3D5_validation_v2_candidate", v2_protocol_hash = p2$protocol_hash, v1_protocol_hash = v1p$protocol_hash, v1_plan_hash = plan$plan_hash, simulation_diagnostic_protocol_hash = sdp$protocol_hash, config_hash = config$config_hash, input_hash = ih$value, accepted_run_supplied = !is.null(accepted_run_dir), source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, source_changed_during_run = changed, statuses = as.list(st), v1_record = as.list(v1rec), overall = overall, r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), seconds = round(proc.time()[["elapsed"]] - t0, 2))
  jsonlite::write_json(meta, file.path(out_dir, "dlm_v2_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA); siw_write_csv(sm0$manifest, file.path(out_dir, "source_manifest_v2.csv")); writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("SOURCE_CHANGED_DURING_RUN=", toupper(as.character(changed)))), file.path(out_dir, "SOURCE_FINGERPRINT.txt"))
  writeLines(c("# Phase 3D validation V2 candidate (nine final L01-L56 M2-L models)", "", paste0("* V2 protocol `", p2$protocol_hash, "` (candidate, introduced AFTER the orientation investigation); V1 protocol `", v1p$protocol_hash, "` and V1 plan `", plan$plan_hash, "` preserved"), paste0("* Source fingerprint `", sm0$fingerprint, "` (", sm0$n_files, " files); input hash `", ih$value, "`"), "",
    "## Required statuses", "", paste0("* ", names(req), ": ", req), "", "## Reported separately (not acceptance requirements of A-E)", "", paste0("* ", names(rep), ": ", rep), "", "## V1 criteria computed for the record (do not decide V2 acceptance)", "", paste0("* ", names(v1rec), ": ", v1rec), "",
    "## Notes", "", "* Exact differences of simulated critical values, their V1 band, the orientation explanation and the reference sign-pattern annotation are in `dlm_v2_exact_differences_visible.csv`; a sign pattern alone never validates a reference value and says nothing about the historical engine.", "* Aggregate covariance matrices, eigenvalues, sign convention and RNG metadata are saved (`dlm_v2_*`).", "* NOT_TESTED is never PASS; manuscript 3.50 versus 3.55 is reported by MANUSCRIPT_CONCORDANCE and stays REVIEW.", "", "Nothing in this folder contains participant-level values."), file.path(out_dir, "RUN_SUMMARY.md"))
  list(out_dir = out_dir, statuses = st, v1_record = v1rec, checks = checks, exact_differences = X, v1 = v1, diagnostic = sdg, status = overall, matrices = list(eigen = EG), seconds = proc.time()[["elapsed"]] - t0)
}
