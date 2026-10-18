# Phase 3D.3: DIAGNOSTIC of the simultaneous critical-value discrepancies (declared in config/dlm_simulation_diagnostic_protocol.json).
# Identical standard-normal draws are turned into multivariate-normal draws with (1) the production factor (eigen decomposition exactly as MASS::mvrnorm), (2) the same eigen decomposition with signs fixed in lag space, (3) the symmetric square root (diagnostic only).
# Never changes a production fit, epsilon, simulation method, seed, draw count, reference, criterion or status; reads the study CSV, a validation run's aggregate tables and optionally the DLM reference bundle; writes only to a new folder.

#' Load the declared simulation-diagnostic protocol (config/dlm_simulation_diagnostic_protocol.json).
#' @export
siw_dlm_simdiag_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "dlm_simulation_diagnostic_protocol.json"), file.path(root, "extdata", "dlm_simulation_diagnostic_protocol.json"), file.path(root, "inst", "extdata", "dlm_simulation_diagnostic_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_DLM_SIMDIAG_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

#' Standard normals exactly as MASS::mvrnorm draws them: matrix(rnorm(p * n), n) under the seeded Mersenne-Twister / Inversion stream.
#' @export
siw_dlm_sim_std_normals <- function(n, p, seed) siw_with_seed(seed, matrix(stats::rnorm(p * n), n))

#' Covariance factor F (p x p) of a covariance matrix: draws are Z = t(F %*% t(X)). Methods: "production_eigen" (signs as eigen() returns them, as MASS::mvrnorm), "aligned_eigen" (sign of each eigenvector fixed in lag space so that the component of B v with the largest absolute value is positive), "symmetric_sqrt" (V D V', diagnostic only).
#' @param Sigma covariance of the coefficients; B lag basis (L x p) of the same coordinates; signs optional +-1 vector applied to the eigenvector columns (sign-pattern search)
#' @return list(F, V, values)
#' @export
siw_dlm_sim_factor <- function(Sigma, B, method = c("production_eigen", "aligned_eigen", "symmetric_sqrt"), signs = NULL) {
  method <- match.arg(method); p <- ncol(Sigma); e <- eigen(Sigma, symmetric = TRUE); V <- e$vectors; d <- sqrt(pmax(e$values, 0))
  if (method == "aligned_eigen") { Bv <- B %*% V; s <- apply(Bv, 2L, function(b) if (b[which.max(abs(b))] >= 0) 1 else -1); V <- V %*% diag(s, p) }
  if (!is.null(signs)) V <- V %*% diag(signs, p)
  F <- if (method == "symmetric_sqrt") V %*% diag(d, p) %*% t(V) else V %*% diag(d, p)
  list(F = F, V = V, values = e$values)
}

#' Critical value from draws: type-8 0.95 quantile of max |t| (production expression).
#' @export
siw_dlm_sim_critical <- function(Z, B, lse) as.numeric(stats::quantile(apply(abs(sweep(Z %*% t(B), 2L, lse, "/")), 1L, max), 0.95, names = FALSE, type = 8))

#' Capture the covariance and draws that the UNCHANGED production function siw_fit_dlm passes to and receives from siw_mvn_draws (a closure copy whose siw_mvn_draws records and returns the same values).
#' @return list(fit = production fit, Sigma, Z, B)
#' @export
siw_dlm_sim_capture <- function(dat, config, outcome, metric, draws, x_cov = NULL) {
  cap <- new.env(); orig <- siw_mvn_draws; f <- siw_fit_dlm; e <- new.env(parent = environment(siw_fit_dlm))
  e$siw_mvn_draws <- function(n, Sigma, method = "MASS_mvrnorm") { Z <- orig(n, Sigma, method); cap$Sigma <- Sigma; cap$Z <- Z; Z }; environment(f) <- e
  fit <- f(dat, config, outcome, metric, config$dlm$final$lag_end, config$dlm$final$lag_df, config$dlm$final$tier, draws = draws, x_cov = x_cov)
  list(fit = fit, Sigma = cap$Sigma, Z = cap$Z, B = siw_lag_basis(config$dlm$final$lag_end, config$dlm$final$lag_df))
}

#' Relative critical-value change, relative change of the simultaneous band half-width, change of the endpoint in standard-error units and the absolute endpoint change at a given SE (protocol: metrics_defined).
#' @export
siw_dlm_simdiag_metrics <- function(c_a, c_b, se = NA_real_) data.frame(relative_critical_value_change = abs(c_a - c_b) / c_b, relative_band_half_width_change = abs(c_a - c_b) / c_b, change_relative_to_pointwise_half_width = abs(c_a - c_b) / 1.96, endpoint_change_in_se_units = abs(c_a - c_b), endpoint_change_absolute_at_se = abs(c_a - c_b) * se, stringsAsFactors = FALSE)

.siw_sd_band <- function(d) if (!is.finite(d)) NA_character_ else if (d <= 1e-9) "EXACT" else if (d <= 1e-6) "NEAR_EXACT" else "REALISATION_DIFFERENCE"
.siw_sd_rel <- function(a, b) max(abs(a - b)) / max(abs(b), .Machine$double.xmin)

# one model: both routes, both draw counts
.siw_sd_model <- function(dat, config, outcome, metric, x_cov, draws_vec, ref_critical, saved) {
  fi <- siw_dlm_indep_fit(dat, config, outcome, metric, config$dlm$final$lag_end, config$dlm$final$lag_df, config$dlm$final$tier, x_cov = x_cov); p <- ncol(fi$Vtheta); out <- list(); sg <- fi$Vtheta; Bi <- fi$B; Ci <- Bi %*% sg %*% t(Bi); lse_i <- sqrt(pmax(0, diag(Ci)))
  cap <- lapply(draws_vec, function(d) siw_dlm_sim_capture(dat, config, outcome, metric, d, x_cov)); s0 <- cap[[1]]$fit$summary; id <- s0$result_id; seed <- as.integer(s0$seed); Sp <- cap[[1]]$Sigma; Bp <- cap[[1]]$B; Cp <- Bp %*% Sp %*% t(Bp); lse_p <- sqrt(pmax(0, diag(Cp)))
  R <- solve(crossprod(Bp), crossprod(Bp, Bi)); ep <- eigen(Sp, symmetric = TRUE); ei <- eigen(sg, symmetric = TRUE)
  trace <- do.call(rbind, lapply(list(list("production", Sp, ep$values), list("independent", sg, ei$values)), function(z) { v <- z[[3]]; data.frame(result_id = id, route = z[[1]], family = s0$outcome_family, seed = seed, draws = paste(draws_vec, collapse = "/"), dimension = p, eigenvalue_1 = v[1], eigenvalue_2 = v[2], eigenvalue_3 = v[3],
    ratio_2_to_1 = v[2] / v[1], ratio_3_to_2 = v[3] / v[2], max_adjacent_ratio = max(v[-1] / v[-length(v)]), near_repeated_eigenvalues = max(v[-1] / v[-length(v)]) > 0.99, condition_ratio_1_to_3 = v[1] / v[p], ill_conditioned = v[1] / v[p] > 1e6, negative_eigenvalues = sum(v < 0),
    covariance_asymmetry_max_relative = max(abs(z[[2]] - t(z[[2]]))) / max(abs(z[[2]])), covariance_source = if (z[[1]] == "production") "captured from siw_fit_dlm (Sigma passed to siw_mvn_draws)" else "siw_dlm_indep_fit$Vtheta", decomposition = if (z[[1]] == "production") "eigen(Sigma, symmetric = TRUE) inside MASS::mvrnorm" else "eigen(Sigma, symmetric = TRUE) in the own mvrnorm clone", stringsAsFactors = FALSE) }))
  cov_agree <- data.frame(result_id = id, lag_space_relative_difference = .siw_sd_rel(Cp, Ci), coefficient_space_relative_difference_after_rotation = .siw_sd_rel(Sp, R %*% sg %*% t(R)), rotation_orthogonality_max_abs = max(abs(crossprod(R) - diag(p))), rotation_max_deviation_from_identity = max(abs(R - diag(p))), max_relative_difference_of_lag_SE = max(abs(lse_p - lse_i) / lse_p),
    max_relative_eigenvalue_difference = max(abs(ep$values - ei$values) / abs(ep$values)), production_se_equals_replicate_se = isTRUE(all.equal(cap[[1]]$fit$lag_curve$standard_error, lse_p, tolerance = 1e-14)), stringsAsFactors = FALSE)
  low <- function(S) { S[upper.tri(S)] <- t(S)[upper.tri(S)]; S }; rec <- list(); crit <- list(); pat <- list(); ali <- NULL; mech <- list(); rep_rows <- list()
  # lag-space eigenvector correspondence (production orientation)
  bp <- Bp %*% ep$vectors; bi <- Bi %*% ei$vectors; cosj <- vapply(seq_len(p), function(j) sum(bp[, j] * bi[, j]) / sqrt(sum(bp[, j]^2) * sum(bi[, j]^2)), 0)
  ali <- data.frame(result_id = id, component = seq_len(p), eigenvalue_production = ep$values, eigenvalue_independent = ei$values, cosine_lag_space = cosj, class = ifelse(abs(cosj - 1) < 1e-6, "SAME_SIGN", ifelse(abs(cosj + 1) < 1e-6, "SIGN_FLIPPED", "ORIENTATION_DIFFERS")), stringsAsFactors = FALSE)
  flips <- which(ali$class == "SIGN_FLIPPED")
  for (k in seq_along(draws_vec)) { d <- draws_vec[k]; Xp <- siw_dlm_sim_std_normals(d, p, seed); cpk <- cap[[k]]; crit_prod_fit <- cpk$fit$summary$simultaneous_critical
    Fp <- lapply(c("production_eigen", "aligned_eigen", "symmetric_sqrt"), function(m) siw_dlm_sim_factor(Sp, Bp, m)); names(Fp) <- c("production_eigen", "aligned_eigen", "symmetric_sqrt"); Fi <- lapply(c("production_eigen", "aligned_eigen", "symmetric_sqrt"), function(m) siw_dlm_sim_factor(sg, Bi, m)); names(Fi) <- names(Fp)
    for (m in names(Fp)) for (r in c("production", "independent")) { fa <- if (r == "production") Fp[[m]] else Fi[[m]]; S_ <- low(if (r == "production") Sp else sg); S_raw <- if (r == "production") Sp else sg; B_ <- if (r == "production") Bp else Bi; lse_ <- if (r == "production") lse_p else lse_i
      rec[[length(rec) + 1L]] <- data.frame(result_id = id, route = r, method = m, draws = d, reconstruction_relative_error_coefficient_space = .siw_sd_rel(fa$F %*% t(fa$F), S_), reconstruction_relative_error_lag_space = .siw_sd_rel(B_ %*% fa$F %*% t(fa$F) %*% t(B_), B_ %*% S_ %*% t(B_)), error_against_supplied_covariance_coefficient_space = .siw_sd_rel(fa$F %*% t(fa$F), S_raw), covariance_asymmetry_max_relative = max(abs(S_raw - t(S_raw))) / max(abs(S_raw)), eigenvector_orthogonality_max_abs = max(abs(crossprod(fa$V) - diag(p))), eigenvalues_non_increasing = all(diff(fa$values) <= 0), stringsAsFactors = FALSE) }
    zc <- function(fa, X) t(fa$F %*% t(X)); cr <- function(fa, X, B_, lse_) siw_dlm_sim_critical(zc(fa, X), B_, lse_)
    c_pp <- cr(Fp$production_eigen, Xp, Bp, lse_p); c_pi <- cr(Fi$production_eigen, Xp, Bi, lse_i); c_ap <- cr(Fp$aligned_eigen, Xp, Bp, lse_p); c_ai <- cr(Fi$aligned_eigen, Xp, Bi, lse_i); c_sp <- cr(Fp$symmetric_sqrt, Xp, Bp, lse_p); c_si <- cr(Fi$symmetric_sqrt, Xp, Bi, lse_i); c_sim <- cr(Fi$symmetric_sqrt, Xp %*% R, Bi, lse_i)   # the symmetric root is covariant only if the standard normals are rotated with the coordinates (X R); identical when R is the identity
    # flip reproduction: orient the independent factor like the production factor (components with cosine -1)
    sgn <- rep(1, p); sgn[flips] <- -1; c_if <- siw_dlm_sim_critical(zc(siw_dlm_sim_factor(sg, Bi, "production_eigen", signs = sgn), Xp), Bi, lse_i)
    ref_c <- if (d == max(draws_vec) && !is.na(ref_critical)) ref_critical else NA_real_
    # sign-pattern search (distinct patterns up to a global sign)
    pats <- as.matrix(expand.grid(rep(list(c(1, -1)), p - 1L))); pats <- cbind(1, pats); pc <- list()
    for (r in c("production", "independent")) { S_ <- if (r == "production") Sp else sg; B_ <- if (r == "production") Bp else Bi; lse_ <- if (r == "production") lse_p else lse_i
      for (i in seq_len(nrow(pats))) { cv <- siw_dlm_sim_critical(zc(siw_dlm_sim_factor(S_, B_, "production_eigen", signs = pats[i, ]), Xp), B_, lse_); pc[[length(pc) + 1L]] <- data.frame(result_id = id, route = r, draws = d, sign_pattern = paste(ifelse(pats[i, ] > 0, "+", "-"), collapse = ""), critical = cv,
        matches_production_critical = abs(cv - crit_prod_fit) <= 1e-9, matches_independent_production_critical = abs(cv - c_pi) <= 1e-9, matches_reference_critical = if (is.na(ref_c)) NA else abs(cv - ref_c) <= 1e-9, stringsAsFactors = FALSE) } }
    pat[[k]] <- do.call(rbind, pc)
    sv <- if (!is.null(saved)) saved else NULL; sv_p <- if (is.null(sv)) NA_real_ else if (k == 1L) sv$critical_package else sv$audited_critical_package; sv_i <- if (is.null(sv)) NA_real_ else if (k == 1L) sv$critical_independent else sv$audited_critical_independent
    zrep <- isTRUE(all.equal(zc(Fp$production_eigen, Xp), cpk$Z, tolerance = 0, check.attributes = FALSE)); xid <- isTRUE(all.equal(siw_with_seed(seed, .siw_dv_mvn(d, sg)), zc(Fi$production_eigen, Xp), tolerance = 0, check.attributes = FALSE)); ci_clone <- siw_dlm_indep_critical(sg, Bi, lse_i, d, seed)
    rep_rows[[k]] <- data.frame(result_id = id, draws = d, standard_normals_identical_between_routes = xid,   # the independent clone's draws are reproduced bitwise from the same standard normals that reproduce the production draws
       production_draws_reproduced_bitwise = zrep, production_critical_reproduced_bitwise = identical(c_pp, crit_prod_fit), independent_clone_critical_difference = abs(c_pi - ci_clone), production_critical = crit_prod_fit,
      saved_production_critical = sv_p, difference_to_saved_production = if (is.na(sv_p)) NA_real_ else abs(crit_prod_fit - sv_p), saved_independent_critical = sv_i, difference_to_saved_independent = if (is.na(sv_i)) NA_real_ else abs(c_pi - sv_i), stringsAsFactors = FALSE)
    crit[[k]] <- data.frame(result_id = id, family = s0$outcome_family, draws = d, seed = seed, production_package = c_pp, production_independent = c_pi, difference_production = abs(c_pp - c_pi), aligned_package = c_ap, aligned_independent = c_ai, difference_aligned = abs(c_ap - c_ai), symmetric_sqrt_package = c_sp, symmetric_sqrt_independent = c_si, difference_symmetric_sqrt = abs(c_sp - c_si), difference_symmetric_sqrt_coordinate_matched = abs(c_sp - c_sim),
      independent_with_package_orientation = c_if, difference_after_orientation_flip = abs(c_if - c_pp), flipped_components = paste(flips, collapse = ","), reference_critical = ref_c, difference_reference_vs_production = if (is.na(ref_c)) NA_real_ else abs(ref_c - c_pp), production_band = .siw_sd_band(abs(c_pp - c_pi)), stringsAsFactors = FALSE)
    bound <- max(1e-9, 10 * cov_agree$lag_space_relative_difference * c_pp); present <- abs(c_pp - c_pi) > bound
    explained <- if (!present) NA else (abs(c_ap - c_ai) <= bound && abs(c_if - c_pp) <= bound)
    mech[[k]] <- data.frame(result_id = id, draws = d, production_difference = abs(c_pp - c_pi), allowance_for_continuous_covariance_effect = bound, discrepancy_present = present, flipped_components = paste(flips, collapse = ","), orientation_differs_components = paste(which(ali$class == "ORIENTATION_DIFFERS"), collapse = ","), near_repeated_eigenvalues = trace$near_repeated_eigenvalues[1L], aligned_difference = abs(c_ap - c_ai), difference_after_flip = abs(c_if - c_pp), explained_by_orientation = explained, stringsAsFactors = FALSE) }
  list(trace = trace, cov = cov_agree, rec = do.call(rbind, rec), align = ali, crit = do.call(rbind, crit), pat = do.call(rbind, pat), mech = do.call(rbind, mech), replica = do.call(rbind, rep_rows), se_max = max(lse_p), id = id)
}

#' Exactness accounting of an earlier validation run (aggregate tables only): which critical values are exact, near exact or realisation-different, and how that relates to the status line and to the failed checks.
#' @param run_dir a dlmvalidate_* folder
#' @export
siw_dlm_simdiag_accounting <- function(run_dir) {
  rd <- function(f) { p <- file.path(run_dir, f); if (!file.exists(p) || file.info(p)$size < 5) data.frame(statistic = character(), key = character(), new_value = numeric(), reference_value = numeric(), abs_diff = numeric(), rel_diff = numeric(), class = character()) else utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) }; K <- rd("dlm_critical_values.csv"); C <- rd("dlm_verification_checks.csv"); rf <- rd("dlm_reference_validation_detail.csv")
  cc <- C[endsWith(C$quantity, "(exact seeded)") & C$group == "SIMULTANEOUS_EXACT_SEEDED", ]; i <- match(K$result_id, cc$result_id)
  ref_d <- rf$abs_diff[rf$statistic == "simultaneous_critical"]
  A <- data.frame(result_id = K$result_id, family = K$family, abs_diff_package_vs_independent_5000 = K$abs_diff, band_at_accepted_epsilon = vapply(K$abs_diff, .siw_sd_band, ""), production_label_cross_implementation_class = K$cross_implementation_class,
    exact_check_compares = ifelse(K$family == "binomial", "package at epsilon 1e-14 vs independent", "package (accepted epsilon, no epsilon in the continuous fit) vs independent"), exact_check_abs_diff = cc$max_abs_diff[i], exact_check_status = cc$status[i],
    audited_10000_package_vs_independent_abs_diff = abs(K$audited_critical_package - K$audited_critical_independent), audited_10000_band = vapply(abs(K$audited_critical_package - K$audited_critical_independent), .siw_sd_band, ""), stringsAsFactors = FALSE)
  cnt <- rbind(data.frame(view = "status line CRITICAL_VALUES_EXACT_AGAINST_INDEPENDENT_ROUTE_AT_ACCEPTED_EPSILON (production_label == EXACT)", count = sum(A$production_label_cross_implementation_class == "EXACT"), of = nrow(A), stringsAsFactors = FALSE),
    data.frame(view = "absolute difference <= 1e-9 (EXACT band), accepted epsilon, 5,000 draws", count = sum(A$band_at_accepted_epsilon == "EXACT"), of = nrow(A)), data.frame(view = "1e-9 < difference <= 1e-6 (NEAR_EXACT: a continuous effect of a covariance difference, within tolerance but not exact)", count = sum(A$band_at_accepted_epsilon == "NEAR_EXACT"), of = nrow(A)),
    data.frame(view = "difference > 1e-6 (REALISATION_DIFFERENCE)", count = sum(A$band_at_accepted_epsilon == "REALISATION_DIFFERENCE"), of = nrow(A)), data.frame(view = "failed SIMULTANEOUS_EXACT_SEEDED critical-value checks (the check compares the tight-epsilon package for binary models)", count = sum(A$exact_check_status == "FAIL", na.rm = TRUE), of = nrow(A)),
    data.frame(view = "reference comparison cells outside the exact-seeded class (10,000 draws)", count = length(ref_d), of = nrow(A)))
  list(table = A, counts = cnt, reference_cells = rf[rf$statistic == "simultaneous_critical", c("key", "new_value", "reference_value", "abs_diff", "rel_diff", "class")])
}

#' Run the simulation diagnostic for the nine final DLMs on one canonical CSV; writes aggregate tables to a new folder.
#' @param validation_run_dir an earlier dlmvalidate_* run folder (read only); dlm_reference_dir optional DLM reference bundle (read only)
#' @export
siw_run_dlm_simdiag <- function(data_csv, out_dir, validation_run_dir, dlm_reference_dir = NULL, config = siw_load_config(), label = NULL) {
  t0 <- proc.time()[["elapsed"]]; dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); options(sieccwinter.log_file = file.path(out_dir, "run.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE)
  protocol <- siw_dlm_simdiag_protocol(); sm0 <- siw_source_manifest(); siw_log("Phase 3D.3 simulation diagnostic; protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint)
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = config$dlm$final$lag_end); if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED", call. = FALSE)
  dat <- siw_prepare_data(raw, config); xc <- siw_tier_matrix(dat, config$tiers[[config$dlm$final$tier]]); draws_vec <- c(as.integer(config$dlm$final$draws), 10000L)
  K <- utils::read.csv(file.path(validation_run_dir, "dlm_critical_values.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  ref <- if (!is.null(dlm_reference_dir) && file.exists(file.path(dlm_reference_dir, "dlmL56_audited_critical_10000.csv"))) utils::read.csv(file.path(dlm_reference_dir, "dlmL56_audited_critical_10000.csv"), stringsAsFactors = FALSE) else NULL
  fin <- siw_final_dlm(dat, config, 0L)$summary; res <- list()
  for (i in seq_len(nrow(fin))) { o <- fin$outcome[i]; m <- fin$metric[i]; siw_log("diagnostic [", i, "/9] ", fin$result_id[i]); rc <- if (is.null(ref)) NA_real_ else ref$simultaneous_critical[match(paste(o, m), paste(ref$outcome, ref$metric))]; sv <- K[K$result_id == fin$result_id[i], , drop = FALSE]
    res[[i]] <- .siw_sd_model(dat, config, o, m, xc, draws_vec, rc, if (nrow(sv)) sv else NULL) }
  cb <- function(nm) siw_bind_rows(lapply(res, function(z) z[[nm]])); W <- function(x, f) siw_write_csv(x, file.path(out_dir, f))
  trace <- cb("trace"); cov <- cb("cov"); rec <- cb("rec"); ali <- cb("align"); crit <- cb("crit"); pat <- cb("pat"); mech <- cb("mech"); replica <- cb("replica")
  # metrics clarification: reference vs production (10,000), production vs independent (5,000 and 10,000), with each model's largest lag SE
  se_max <- vapply(res, function(z) z$se_max, 0); names(se_max) <- vapply(res, function(z) z$id, ""); mets <- list()
  for (j in seq_len(nrow(crit))) { r <- crit[j, ]; se <- se_max[[r$result_id]]
    if (!is.na(r$reference_critical)) mets[[length(mets) + 1L]] <- cbind(data.frame(result_id = r$result_id, comparison = "audited reference vs production, 10,000 draws", critical_a = r$reference_critical, critical_b = r$production_package, max_lag_se = se, stringsAsFactors = FALSE), siw_dlm_simdiag_metrics(r$reference_critical, r$production_package, se))
    mets[[length(mets) + 1L]] <- cbind(data.frame(result_id = r$result_id, comparison = paste0("independent route vs production, ", r$draws, " draws"), critical_a = r$production_independent, critical_b = r$production_package, max_lag_se = se, stringsAsFactors = FALSE), siw_dlm_simdiag_metrics(r$production_independent, r$production_package, se)) }
  Mx <- do.call(rbind, mets); acc <- siw_dlm_simdiag_accounting(validation_run_dir)
  W(trace, "dlm_sd_covariance_trace.csv"); W(cov, "dlm_sd_covariance_agreement.csv"); W(rec, "dlm_sd_factor_reconstruction.csv"); W(ali, "dlm_sd_sign_alignment.csv"); W(crit, "dlm_sd_critical_comparison.csv"); W(pat, "dlm_sd_sign_pattern_search.csv"); W(mech, "dlm_sd_mechanism_summary.csv"); W(replica, "dlm_sd_production_replica.csv"); W(Mx, "dlm_sd_metrics_clarification.csv")
  W(acc$table, "dlm_sd_exactness_accounting.csv"); W(acc$counts, "dlm_sd_exactness_counts.csv"); W(acc$reference_cells, "dlm_sd_reference_cells_from_validation_run.csv")
  # statuses
  rep_ok <- all(replica$standard_normals_identical_between_routes & replica$production_draws_reproduced_bitwise & replica$production_critical_reproduced_bitwise & replica$independent_clone_critical_difference <= 1e-12) && all(replica$difference_to_saved_production <= 1e-12, na.rm = TRUE) && all(replica$difference_to_saved_independent <= 1e-12, na.rm = TRUE)
  rec_ok <- all(rec$reconstruction_relative_error_coefficient_space <= protocol$criteria$reconstruction_relative_tolerance & rec$reconstruction_relative_error_lag_space <= protocol$criteria$reconstruction_relative_tolerance)
  need <- mech$discrepancy_present; ex <- mech$explained_by_orientation[need]; mech_status <- if (!any(need)) "NOT_APPLICABLE" else if (all(ex %in% TRUE)) "PASS" else "PARTIAL"
  mism <- !is.na(crit$reference_critical) & crit$difference_reference_vs_production > 1e-9; hit <- vapply(which(mism), function(j) { z <- pat[pat$result_id == crit$result_id[j] & pat$draws == crit$draws[j] & pat$route == "production", ]; any(z$matches_reference_critical %in% TRUE) }, NA)
  ref_status <- if (is.null(ref)) "NOT_TESTED" else if (!any(mism)) "NOT_APPLICABLE" else if (all(hit)) "PASS" else "PARTIAL"
  sm1 <- siw_source_manifest(); changed <- !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint)
  st <- c(EXECUTION_STATUS = "PASS", PRODUCTION_REPLICA_STATUS = if (rep_ok) "PASS" else "FAIL", FACTOR_RECONSTRUCTION_STATUS = if (rec_ok) "PASS" else "FAIL", MECHANISM_EXPLANATION_STATUS = mech_status, REFERENCE_PATTERN_STATUS = ref_status, SOURCE_INTEGRITY_STATUS = if (changed) "FAIL" else "PASS")
  unexpl <- mech[need & !(mech$explained_by_orientation %in% TRUE), c("result_id", "draws", "production_difference", "allowance_for_continuous_covariance_effect", "flipped_components", "orientation_differs_components", "near_repeated_eigenvalues", "aligned_difference", "difference_after_flip")]; W(unexpl, "dlm_sd_unexplained_differences.csv")
  W(sm0$manifest, "source_manifest.csv"); writeLines(c(paste0(names(st), "=", st), paste0("MODELS=", length(res)), paste0("DISCREPANCIES_PRODUCTION_ROUTES=", sum(need)), paste0("EXPLAINED_BY_ORIENTATION=", sum(ex %in% TRUE)), paste0("UNEXPLAINED=", nrow(unexpl)), paste0("REFERENCE_MISMATCHES=", sum(mism)), paste0("REFERENCE_MISMATCHES_REPRODUCED_BY_A_SIGN_PATTERN=", sum(hit)),
    "DIAGNOSTIC ONLY: no production fit, epsilon, simulation method, seed, draw count, reference, criterion or earlier status was changed."), file.path(out_dir, "DLM_SIMDIAG_STATUS.txt"))
  writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("SOURCE_CHANGED_DURING_RUN=", toupper(as.character(changed)))), file.path(out_dir, "SOURCE_FINGERPRINT.txt"))
  meta <- list(package_version = siw_pkg_version(), batch = "3D3_simulation_diagnostic", protocol_hash = protocol$protocol_hash, config_hash = config$config_hash, input_hash = ih$value, validation_run = basename(validation_run_dir), reference_dir_supplied = !is.null(dlm_reference_dir), source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, statuses = as.list(st), r_version = R.version.string, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), seconds = round(proc.time()[["elapsed"]] - t0, 2))
  jsonlite::write_json(meta, file.path(out_dir, "dlm_simdiag_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA); writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "session_info.txt"))
  writeLines(c("# Phase 3D.3 simulation diagnostic (diagnostic only)", "", paste0("* Protocol hash `", protocol$protocol_hash, "`; source fingerprint `", sm0$fingerprint, "` (", sm0$n_files, " files); input hash `", ih$value, "`"), "", "## Statuses", "", paste0("* ", names(st), ": ", st), "",
    "## Counts", "", paste0("* production cross-route differences beyond the continuous-covariance allowance: ", sum(need), "; explained by orientation: ", sum(ex %in% TRUE), "; unexplained: ", nrow(unexpl)), paste0("* reference mismatches: ", sum(mism), "; reproduced by a sign pattern of the production factor: ", sum(hit)), "", "Nothing in this folder contains participant-level values."), file.path(out_dir, "RUN_SUMMARY.md"))
  list(out_dir = out_dir, statuses = st, trace = trace, cov = cov, rec = rec, align = ali, crit = crit, pat = pat, mech = mech, replica = replica, metrics = Mx, accounting = acc, unexplained = unexpl, status = if (any(st %in% c("FAIL"))) "DIAGNOSTIC_CHECK_FAILED" else if (any(st %in% c("PARTIAL", "NOT_TESTED"))) "OK_WITH_PARTIAL_OR_NOT_TESTED" else "OK")
}
