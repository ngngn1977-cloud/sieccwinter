# Phase 3D.3: simulation diagnostic for the DLM simultaneous critical values. Synthetic data only.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
if (!exists("expect_lt", mode = "function")) expect_lt <- function(a, b) expect_true(a < b)
SDP <- siw_dlm_simdiag_protocol(); B56 <- siw_lag_basis(56L, 3L)
rand_orth <- function(seed, p = 3L) { set.seed(seed); qr.Q(qr(matrix(stats::rnorm(p * p), p))) }
rand_cov <- function(seed, p = 3L, spread = c(1, 0.05, 0.02)) { set.seed(seed); Q <- rand_orth(seed + 1000L, p); Q %*% diag(spread, p) %*% t(Q) }
low <- function(S) { S[upper.tri(S)] <- t(S)[upper.tri(S)]; S }
lag_draws <- function(F, X, B) t(F %*% t(X)) %*% t(B)

test_that("declared protocol: methods, coordinates, criteria, metric definitions and the erratum note on 0.42%", {
  expect_true(nchar(SDP$protocol_hash) == 64L); expect_identical(vapply(SDP$methods_compared, function(z) z$id, ""), c("production_eigen", "aligned_eigen", "symmetric_sqrt")); expect_match(SDP$declared, "never changes a fit"); expect_match(SDP$criteria$no_pass_fail_on_production, "none changes a validation status")
  expect_match(SDP$metrics_defined$note, "mislabelled"); expect_match(SDP$saved_matrices, "none"); expect_equal(SDP$criteria$reconstruction_relative_tolerance, 1e-12); expect_match(SDP$statuses$rule, "NOT_TESTED is never PASS"); expect_match(SDP$criteria$explained, "allowance")
  expect_identical(SDP$criteria$exactness_bands_descriptive$EXACT, "absolute difference <= 1e-9")
})

test_that("metrics: relative critical-value change, band half-width change and endpoint change in SE units are distinguished (known values: reference 2.526865 vs package 2.516245, SE 0.0126816)", {
  m <- siw_dlm_simdiag_metrics(2.526865, 2.516245, 0.0126816)
  expect_equal(m$relative_critical_value_change, 0.01062 / 2.516245, tolerance = 1e-6); expect_equal(m$relative_critical_value_change, 0.0042203, tolerance = 1e-4); expect_equal(m$relative_band_half_width_change, m$relative_critical_value_change)
  expect_equal(m$endpoint_change_in_se_units, 0.01062, tolerance = 1e-9); expect_equal(m$endpoint_change_absolute_at_se, 0.01062 * 0.0126816, tolerance = 1e-9); expect_equal(m$change_relative_to_pointwise_half_width, 0.01062 / 1.96, tolerance = 1e-9)
  expect_true(abs(m$relative_critical_value_change - m$endpoint_change_in_se_units) > 0.005)                                              # 0.42% (relative) and 0.0106 SE are different quantities
  m0 <- siw_dlm_simdiag_metrics(2, 2, 3); expect_equal(c(m0$relative_critical_value_change, m0$endpoint_change_in_se_units, m0$endpoint_change_absolute_at_se), c(0, 0, 0))
})

test_that("every factor reconstructs the covariance that eigen(symmetric = TRUE) uses (lower triangle), including a slightly asymmetric input; eigenvectors orthonormal; eigenvalues non-increasing", {
  for (sd in 1:6) { S <- rand_cov(sd); S2 <- S; S2[1, 3] <- S2[1, 3] + 1e-10; Su <- low(S2)
    for (m in c("production_eigen", "aligned_eigen", "symmetric_sqrt")) { f <- siw_dlm_sim_factor(S2, B56, m); expect_true(max(abs(f$F %*% t(f$F) - Su)) / max(abs(Su)) < 1e-14); expect_true(max(abs(crossprod(f$V) - diag(3))) < 1e-14); expect_true(all(diff(f$values) <= 0)) }
    expect_true(max(abs(siw_dlm_sim_factor(S2, B56, "symmetric_sqrt")$F - t(siw_dlm_sim_factor(S2, B56, "symmetric_sqrt")$F))) < 1e-15) }
  expect_true(max(abs(low(matrix(c(1, 2, 3, 2, 1, 0, 3, 5, 1), 3)) - t(low(matrix(c(1, 2, 3, 2, 1, 0, 3, 5, 1), 3))))) == 0)
})

test_that("aligned signs are coordinate free: for identical standard normals the aligned-eigen and symmetric-root draws (mapped to lag space) are identical across rotated coordinates; production eigen draws can differ by a component sign", {
  X <- siw_dlm_sim_std_normals(2000L, 3L, 20260830L); found <- 0L
  for (sd in 1:60) { S <- rand_cov(sd); R <- rand_orth(sd + 500L); Bi <- B56 %*% R; Si <- t(R) %*% S %*% R                              # B_ind = B R, Sigma_ind = R' Sigma R (same lag-space covariance)
    za <- lag_draws(siw_dlm_sim_factor(S, B56, "aligned_eigen")$F, X, B56); zb <- lag_draws(siw_dlm_sim_factor(Si, Bi, "aligned_eigen")$F, X, Bi); expect_true(max(abs(za - zb)) < 1e-9)
    za <- lag_draws(siw_dlm_sim_factor(S, B56, "symmetric_sqrt")$F, X, B56); zb <- lag_draws(siw_dlm_sim_factor(Si, Bi, "symmetric_sqrt")$F, X %*% R, Bi); expect_true(max(abs(za - zb)) < 1e-9)   # the symmetric root is covariant when the normals are rotated with the coordinates (X R)
    pa <- siw_dlm_sim_factor(S, B56, "production_eigen"); pb <- siw_dlm_sim_factor(Si, Bi, "production_eigen"); bp <- B56 %*% pa$V; bi <- Bi %*% pb$V; cs <- vapply(1:3, function(j) sum(bp[, j] * bi[, j]) / sqrt(sum(bp[, j]^2) * sum(bi[, j]^2)), 0)
    expect_true(all(abs(abs(cs) - 1) < 1e-8))                                                                                                       # same eigen-structure: every component matches up to sign
    if (any(cs < 0)) { found <- found + 1L; sg <- ifelse(cs < 0, -1, 1); zp <- lag_draws(pa$F, X, B56); zi <- lag_draws(pb$F, X, Bi); lse <- sqrt(diag(B56 %*% S %*% t(B56)))
      expect_gt(max(abs(zp - zi)), 1e-3); crit_p <- siw_dlm_sim_critical(t(pa$F %*% t(X)), B56, lse); crit_i <- siw_dlm_sim_critical(t(pb$F %*% t(X)), Bi, lse); crit_f <- siw_dlm_sim_critical(t(siw_dlm_sim_factor(Si, Bi, "production_eigen", signs = sg)$F %*% t(X)), Bi, lse)
      expect_true(abs(crit_f - crit_p) < 1e-9); if (abs(crit_p - crit_i) > 1e-6) expect_true(abs(crit_p - crit_i) > abs(crit_f - crit_p)) } }
  expect_gt(found, 5L)                                                                                                                              # sign flips between rotated coordinates occur in this synthetic set, and flipping the flagged components reproduces the other route's draws
})

test_that("near-repeated eigenvalues: signs cannot align an eigenspace, the symmetric root is unaffected; diagnostics flag the ratio", {
  S <- rand_cov(3L, spread = c(1, 0.3, 0.3 + 1e-12)); R <- rand_orth(77L); Bi <- B56 %*% R; Si <- t(R) %*% S %*% R; X <- siw_dlm_sim_std_normals(1000L, 3L, 5L)
  ev <- eigen(S, symmetric = TRUE)$values; expect_true(ev[3] / ev[2] > 0.99); za <- lag_draws(siw_dlm_sim_factor(S, B56, "symmetric_sqrt")$F, X, B56); zb <- lag_draws(siw_dlm_sim_factor(Si, Bi, "symmetric_sqrt")$F, X %*% R, Bi); expect_true(max(abs(za - zb)) < 1e-6)
  pa <- siw_dlm_sim_factor(S, B56, "aligned_eigen"); pb <- siw_dlm_sim_factor(Si, Bi, "aligned_eigen"); bp <- B56 %*% pa$V; bi <- Bi %*% pb$V; cs <- abs(vapply(2:3, function(j) sum(bp[, j] * bi[, j]) / sqrt(sum(bp[, j]^2) * sum(bi[, j]^2)), 0))
  expect_true(all(cs <= 1 + 1e-12))                                                                                                                  # within a (nearly) repeated eigenspace the eigenvectors are only defined up to rotation; the diagnostic reports it as ORIENTATION_DIFFERS instead of calling it a sign flip
})

test_that("production capture: the closure copy returns exactly what the unchanged production function computes and records the covariance and draws it used", {
  xc <- siw_tier_matrix(DAT, CFG$tiers$M2_L); cp <- siw_dlm_sim_capture(DAT, CFG, "Int_dummy", "UTCI", 5000L, xc); plain <- siw_fit_dlm(DAT, CFG, "Int_dummy", "UTCI", draws = 5000L, x_cov = xc)
  expect_identical(cp$fit$summary, plain$summary); expect_identical(cp$fit$lag_curve, plain$lag_curve); expect_identical(cp$fit$cumulative_curve, plain$cumulative_curve)
  seed <- cp$fit$summary$seed; z <- siw_with_seed(seed, MASS::mvrnorm(5000L, rep(0, 3), cp$Sigma)); expect_true(isTRUE(all.equal(unname(z), unname(cp$Z), tolerance = 0))); expect_equal(dim(cp$Sigma), c(3L, 3L))
  X <- siw_dlm_sim_std_normals(5000L, 3L, seed); f <- siw_dlm_sim_factor(cp$Sigma, cp$B, "production_eigen"); expect_true(isTRUE(all.equal(t(f$F %*% t(X)), unname(cp$Z), tolerance = 0, check.attributes = FALSE)))
  lse <- sqrt(pmax(0, diag(cp$B %*% cp$Sigma %*% t(cp$B)))); expect_identical(siw_dlm_sim_critical(t(f$F %*% t(X)), cp$B, lse), plain$summary$simultaneous_critical)
})

test_that("exactness accounting separates exact, near-exact and realisation-different values, the status-line count and the failed checks", {
  d <- tempfile("fakeval_"); dir.create(d); ids <- paste0("DLM__M", 1:6)
  K <- data.frame(result_id = ids, family = c(rep("gaussian", 3), rep("binomial", 3)), abs_diff = c(1e-13, 2e-12, 1.2e-2, 3e-13, 9.2e-9, 1.3e-2), cross_implementation_class = c("EXACT", "EXACT", "MC_CONSISTENT_NOT_EXACT", "EXACT", "MC_CONSISTENT_NOT_EXACT", "MC_CONSISTENT_NOT_EXACT"), audited_critical_package = 2.5, audited_critical_independent = 2.5 + c(0, 0, 2e-3, 0, 3e-7, 5e-3), stringsAsFactors = FALSE)
  utils::write.csv(K, file.path(d, "dlm_critical_values.csv"), row.names = FALSE)
  ck <- data.frame(result_id = ids, group = "SIMULTANEOUS_EXACT_SEEDED", quantity = "critical_value (exact seeded)", n_compared = 1, max_abs_diff = c(1e-13, 2e-12, 1.2e-2, 3e-13, 2e-13, 1.1e-2), max_rel_diff = 0, criterion = "x", classification = NA, status = c("PASS", "PASS", "FAIL", "PASS", "PASS", "FAIL"), stringsAsFactors = FALSE); utils::write.csv(ck, file.path(d, "dlm_verification_checks.csv"), row.names = FALSE)
  writeLines("", file.path(d, "dlm_reference_validation_detail.csv")); a <- siw_dlm_simdiag_accounting(d); ct <- a$counts
  expect_equal(ct$count[1], 3); expect_equal(ct$count[2], 3); expect_equal(ct$count[3], 1); expect_equal(ct$count[4], 2); expect_equal(ct$count[5], 2); expect_equal(sum(ct$count[2:4]), 6)
  expect_identical(a$table$band_at_accepted_epsilon, c("EXACT", "EXACT", "REALISATION_DIFFERENCE", "EXACT", "NEAR_EXACT", "REALISATION_DIFFERENCE")); expect_identical(a$table$exact_check_status, ck$status); expect_match(a$table$exact_check_compares[4:6], "epsilon 1e-14"); expect_match(a$table$exact_check_compares[1:3], "no epsilon")
  expect_false(identical(which(a$table$band_at_accepted_epsilon == "NEAR_EXACT"), which(ck$status == "FAIL")))                                         # a near-exact value is neither exact nor a failed check
})

# ---- end-to-end on a synthetic validation run and a synthetic reference
SD_DIR <- tempfile("sd_"); dir.create(SD_DIR); SD_CSV <- file.path(SD_DIR, "syn.csv"); utils::write.csv(SYN, SD_CSV, row.names = FALSE, na = "")
SD_F5 <- siw_final_dlm(DAT, CFG, 5000L); SD_F10 <- siw_final_dlm(DAT, CFG, 10000L); s10 <- SD_F10$summary
write_ref <- function(dir, crit) { dir.create(dir); utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, simultaneous_critical = crit), file.path(dir, "dlmL56_audited_critical_10000.csv"), row.names = FALSE)
  utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, n = s10$n, global_statistic = s10$global_statistic, global_p = s10$global_p, q9 = s10$q9, cumulative_effect = s10$cumulative_effect, cumulative_se = s10$cumulative_se, cumulative_ci_lower = s10$cumulative_ci_lower, cumulative_ci_upper = s10$cumulative_ci_upper, peak_effect = s10$peak_effect), file.path(dir, "dlmL56_audited_estimates.csv"), row.names = FALSE)
  utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, peak_lag = s10$peak_lag, n_lags_pointwise_excl_null = s10$n_lags_pointwise_excl_null, n_lags_simultaneous_excl_null = s10$n_lags_simultaneous_excl_null), file.path(dir, "dlmL56_audited_discrete.csv"), row.names = FALSE) }
SD_REF <- file.path(SD_DIR, "ref"); write_ref(SD_REF, s10$simultaneous_critical)
SD_VAL <- file.path(SD_DIR, "val"); invisible(suppressMessages(siw_run_dlm_validation(SD_CSV, SD_VAL, SD_REF, CFG, accepted_run_dir = NULL, mc = FALSE, manuscript_concordance = FALSE)))
h_before <- unlist(lapply(sort(list.files(c(SD_VAL, SD_REF), recursive = TRUE, full.names = TRUE)), function(f) siw_file_hash(f)$value))
SD_R <- suppressMessages(siw_run_dlm_simdiag(SD_CSV, file.path(SD_DIR, "out1"), SD_VAL, SD_REF, CFG))

test_that("synthetic end-to-end: self-checks PASS, replica bitwise, every factor reconstructs, the orientation mechanism explains every discrepancy, nothing in production or the validation run changes", {
  st <- SD_R$statuses; expect_identical(st[["EXECUTION_STATUS"]], "PASS"); expect_identical(st[["PRODUCTION_REPLICA_STATUS"]], "PASS"); expect_identical(st[["FACTOR_RECONSTRUCTION_STATUS"]], "PASS"); expect_identical(st[["SOURCE_INTEGRITY_STATUS"]], "PASS")
  expect_identical(st[["MECHANISM_EXPLANATION_STATUS"]], "PASS"); expect_identical(st[["REFERENCE_PATTERN_STATUS"]], "NOT_APPLICABLE"); expect_true(all(SD_R$replica$production_critical_reproduced_bitwise)); expect_true(all(SD_R$replica$production_draws_reproduced_bitwise)); expect_true(all(SD_R$replica$standard_normals_identical_between_routes))
  expect_equal(nrow(SD_R$crit), 18L); expect_equal(length(unique(SD_R$crit$result_id)), 9L); expect_true(any(SD_R$mech$discrepancy_present)); expect_true(all(SD_R$mech$explained_by_orientation[SD_R$mech$discrepancy_present]))
  d <- SD_R$mech[SD_R$mech$discrepancy_present, ]; expect_true(all(d$flipped_components != "")); expect_true(all(d$difference_after_flip <= d$allowance_for_continuous_covariance_effect)); expect_true(all(d$aligned_difference <= d$allowance_for_continuous_covariance_effect)); expect_true(all(d$production_difference > d$allowance_for_continuous_covariance_effect))
  expect_true(all(SD_R$crit$difference_symmetric_sqrt < 1e-6)); expect_true(all(SD_R$rec$reconstruction_relative_error_coefficient_space <= 1e-12)); expect_true(any(SD_R$rec$covariance_asymmetry_max_relative > 0)); expect_equal(nrow(SD_R$trace), 18L); expect_true(all(c("eigenvalue_1", "near_repeated_eigenvalues", "ill_conditioned", "covariance_source", "decomposition") %in% names(SD_R$trace)))
  expect_identical(h_before, unlist(lapply(sort(list.files(c(SD_VAL, SD_REF), recursive = TRUE, full.names = TRUE)), function(f) siw_file_hash(f)$value)))     # inputs read only
  expect_identical(siw_final_dlm(DAT, CFG, 5000L)$summary, SD_F5$summary); expect_identical(siw_final_dlm(DAT, CFG, 5000L)$lag_curves, SD_F5$lag_curves)             # production unchanged
  fl <- list.files(SD_R$out_dir); expect_true(all(c("dlm_sd_covariance_trace.csv", "dlm_sd_factor_reconstruction.csv", "dlm_sd_sign_alignment.csv", "dlm_sd_critical_comparison.csv", "dlm_sd_sign_pattern_search.csv", "dlm_sd_mechanism_summary.csv", "dlm_sd_metrics_clarification.csv", "dlm_sd_exactness_accounting.csv", "dlm_sd_exactness_counts.csv", "DLM_SIMDIAG_STATUS.txt", "dlm_simdiag_metadata.json", "SOURCE_FINGERPRINT.txt", "RUN_SUMMARY.md") %in% fl))
  meta <- jsonlite::fromJSON(file.path(SD_R$out_dir, "dlm_simdiag_metadata.json")); expect_match(meta$source_fingerprint_sha256, "^[0-9A-F]{64}$"); expect_identical(meta$protocol_hash, SDP$protocol_hash); expect_identical(meta$config_hash, CFG$config_hash)
  S <- paste(readLines(file.path(SD_R$out_dir, "DLM_SIMDIAG_STATUS.txt"), warn = FALSE), collapse = " "); expect_match(S, "DIAGNOSTIC ONLY"); expect_false(any(c("participant_code", "assessment_day") %in% unlist(lapply(list.files(SD_R$out_dir, pattern = "[.]csv$", full.names = TRUE), function(f) if (file.info(f)$size > 5) names(utils::read.csv(f, nrows = 1)) else character()))))
})

test_that("a reference value that is another sign pattern of the production factor is reproduced by the pattern search; a value that is no pattern leaves the status PARTIAL; neither alters any production number", {
  xc <- siw_tier_matrix(DAT, CFG$tiers$M2_L); cp <- siw_dlm_sim_capture(DAT, CFG, "Int_dummy", "UTCI", 10000L, xc); seed <- cp$fit$summary$seed; X <- siw_dlm_sim_std_normals(10000L, 3L, seed); lse <- sqrt(pmax(0, diag(cp$B %*% cp$Sigma %*% t(cp$B))))
  pats <- as.matrix(expand.grid(rep(list(c(1, -1)), 2L))); pats <- cbind(1, pats); vals <- vapply(seq_len(nrow(pats)), function(i) siw_dlm_sim_critical(t(siw_dlm_sim_factor(cp$Sigma, cp$B, "production_eigen", signs = pats[i, ])$F %*% t(X)), cp$B, lse), 0)
  other <- vals[abs(vals - cp$fit$summary$simultaneous_critical) > 1e-6]; expect_gt(length(other), 0L)
  i7 <- which(s10$outcome == "Int_dummy" & s10$metric == "UTCI"); crit <- s10$simultaneous_critical; crit[i7] <- other[1L]; rA <- file.path(SD_DIR, "refA"); write_ref(rA, crit)
  RA <- suppressMessages(siw_run_dlm_simdiag(SD_CSV, file.path(SD_DIR, "outA"), SD_VAL, rA, CFG)); expect_identical(RA$statuses[["REFERENCE_PATTERN_STATUS"]], "PASS"); expect_equal(sum(!is.na(RA$crit$reference_critical) & RA$crit$difference_reference_vs_production > 1e-9), 1L)
  pa <- RA$pat[RA$pat$result_id == s10$result_id[i7] & RA$pat$draws == 10000L & RA$pat$route == "production", ]; expect_true(any(pa$matches_reference_critical %in% TRUE)); expect_true(any(pa$matches_production_critical %in% TRUE)); expect_equal(nrow(pa), 4L)
  crit2 <- s10$simultaneous_critical; crit2[i7] <- crit2[i7] + 0.0123; rB <- file.path(SD_DIR, "refB"); write_ref(rB, crit2); RB <- suppressMessages(siw_run_dlm_simdiag(SD_CSV, file.path(SD_DIR, "outB"), SD_VAL, rB, CFG))
  expect_identical(RB$statuses[["REFERENCE_PATTERN_STATUS"]], "PARTIAL"); expect_identical(RB$crit$production_package, SD_R$crit$production_package); expect_identical(RA$crit$production_package, SD_R$crit$production_package)
  RN <- suppressMessages(siw_run_dlm_simdiag(SD_CSV, file.path(SD_DIR, "outN"), SD_VAL, NULL, CFG)); expect_identical(RN$statuses[["REFERENCE_PATTERN_STATUS"]], "NOT_TESTED"); expect_false(RN$statuses[["REFERENCE_PATTERN_STATUS"]] == "PASS")
})

test_that("the diagnostic code changes nothing: no fitting-control edits, no deletions, no figure code, no installs, no writes into the validation run or reference", {
  if (siw_skip_source_only("test-20-dlm-simulation-diagnostic: the diagnostic code changes nothing: no fitting-control edits, no dele")) return(invisible(NULL))
  src <- paste(readLines(file.path(ROOT, "R", "dlm_simdiag.R"), warn = FALSE), collapse = "\n"); code <- paste(sub("#.*$", "", strsplit(src, "\n")[[1]]), collapse = "\n")
  for (bad in c("unlink(", "file.remove(", "file.rename(", "install.packages", "plot(", "ggplot", "png(", "glm_epsilon <-", "config$dlm$final$draws <-", "set.seed(", "archived")) expect_false(grepl(bad, code, fixed = TRUE))
  expect_false(grepl("write_csv\\([^\n]*validation_run_dir", code)); expect_false(grepl("saveRDS|writeLines\\([^\n]*validation_run_dir", code)); expect_true(identical(CFG$dlm$final$glm_epsilon, 1e-12) && identical(CFG$dlm$final$draws, 5000L))
})
