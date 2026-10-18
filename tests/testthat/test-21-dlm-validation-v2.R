# Phase 3D validation protocol V2 (candidate). Synthetic data only: separation of production reproduction, deterministic agreement, orientation-aligned simulation agreement and native-orientation Monte Carlo agreement;
# valid orientation differences and deliberately incorrect calculations; V1 preserved.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
P2 <- siw_dlm_validation_protocol_v2(); P1 <- siw_dlm_validation_protocol(); PL <- siw_load_dlm_plan(); PS <- siw_dlm_simdiag_protocol()
hsh <- function(f) siw_file_hash(file.path(ROOT, f))$value

test_that("V2 is a candidate revision introduced after the orientation investigation; V1 protocol, plan, diagnostic protocol and source files are preserved byte for byte", {
  if (siw_skip_source_only("test-21-dlm-validation-v2: V2 is a candidate revision introduced after the orientation investigat")) return(invisible(NULL))
  expect_match(P2$version, "candidate"); expect_match(P2$status_of_this_document, "AFTER the orientation investigation"); expect_true(nchar(P2$protocol_hash) == 64L);
  expect_private(P1$protocol_hash, "dlm_validation_protocol"); expect_identical(PL$plan_hash, "1AE045B8CC9E538D9F86621C8E583FBE2A54BB6F745B0A7A839E9B69168A7348"); expect_identical(PS$protocol_hash, "917AD6792CBFBE36ABE835F145E90B21654DBF41383A29587CABC10212D39027")
  expect_identical(hsh("R/dlm_verify.R"), "C0D09AF96543DD769C598B22B6ECB9FB9D5128E92BDE1B87FD3A1E67A199E99A"); expect_identical(hsh("R/dlm_validate.R"), "6E1467C5DE4027C35E35F4A3A95EC2B4D057D175FAC1709E872FC4BCA00126A2"); expect_identical(hsh("R/dlm_simdiag.R"), "928460680FDA5A17A6318D6A7663398C54C5585F9194EB5679D80F0E5EF00C27"); expect_identical(hsh("R/ordinal.R"), "010882F8DFD0756A8577429F8A3E5423F45220F2D39AA0A4067CD2ED73704441")
  expect_match(paste(P2$status_of_this_document, collapse = " "), "REFERENCE_COMPARISON = FAIL"); expect_match(paste(P2$status_of_this_document, collapse = " "), "SIMULTANEOUS_EXACT_SEEDED = FAIL")
  rep_dir <- file.path(ROOT, "..", "reports"); for (f in c("PHASE3D2_MANUAL_DLM_VALIDATION_REVIEW_PRE_ERRATUM.md", "PHASE3D3_SIMULATION_DIAGNOSTIC_BUILD_REPORT.md", "PHASE3D4_MANUAL_SIMULATION_DIAGNOSTIC_REVIEW.md")) if (dir.exists(rep_dir)) expect_true(file.exists(file.path(rep_dir, f)))
  expect_true(length(P2$introduced_after$evidence) >= 3L); expect_true(length(P2$introduced_after$what_V2_does_not_claim) == 3L); expect_match(P2$reconstructed_reference$limits, "never the basis of validity")
})

test_that("old/new check crosswalk: every V1 status has a V2 destination, no criterion is changed, and no number of V1 is altered", {
  if (siw_skip_source_only("test-21-dlm-validation-v2: old/new check crosswalk: every V1 status has a V2 destination, no crit")) return(invisible(NULL))
  cw <- P2$check_crosswalk; v1 <- vapply(cw, function(z) z$v1_check, ""); expect_false(any(vapply(cw, function(z) isTRUE(z$criteria_changed), NA)))
  for (k in c("ACCEPTED_FIT_REPRODUCTION", "REFERENCE_COMPARISON", "LAG_BASIS_VERIFICATION", "CURVE_VERIFICATION", "CUMULATIVE_VERIFICATION", "PEAK_LAG_VERIFICATION", "CONVERGENCE_SENSITIVITY", "SIMULTANEOUS_EXACT_SEEDED", "SIMULTANEOUS_MC_STABILITY", "MANUSCRIPT_CONCORDANCE", "FIGURE_PROVENANCE", "DLNM_EQUALITY", "SOURCE_INTEGRITY")) expect_true(any(grepl(k, v1, fixed = TRUE)))
  expect_true(all(nzchar(vapply(cw, function(z) z$rationale, "")))); expect_true(all(nzchar(vapply(cw, function(z) z$v2_check, ""))))
  st <- P1$criteria$simultaneous_mc_stability; expect_equal(st$alternative_seeds, 20); expect_equal(st$large_draw_count, 1e5); expect_equal(st$max_relative_deviation_of_seeded_from_large, 0.05); expect_equal(st$max_spread_across_alternative_seeds_absolute, 0.2); expect_equal(st$max_relative_difference_cross_implementation_not_exact, 0.05)
  ci <- P1$criteria$independent_route; expect_equal(ci$fitted_quantities_abs, 1e-8); expect_equal(ci$fitted_quantities_rel, 1e-7); expect_equal(ci$binomial_fitted_quantities_rel_accepted_epsilon, 1e-6); expect_equal(ci$critical_seeded_abs, 1e-9)
  src <- paste(readLines(file.path(ROOT, "R", "dlm_v2.R"), warn = FALSE), collapse = "\n"); expect_false(grepl("0.05", sub("#[^\n]*", "", src), fixed = TRUE)); expect_match(src, "v1p\\$criteria")                     # V2 reads the criteria from V1; it hard-codes none of them
  expect_identical(c(P2$statuses$required[[1]], P2$statuses$required[[9]]), c("EXECUTION_STATUS", "SOURCE_INTEGRITY_STATUS")); expect_true(all(c("MANUSCRIPT_CONCORDANCE_STATUS", "FIGURE_PROVENANCE_STATUS", "DLNM_EQUALITY_STATUS") %in% unlist(P2$statuses$reported_separately)))
})

mk_mech <- function(difference_production = 0.012, aligned = 1e-12, allowance = 1e-9, present = TRUE, explained = TRUE) data.frame(result_id = "M1", draws = 5000L, production_difference = difference_production, allowance_for_continuous_covariance_effect = allowance, discrepancy_present = present, explained_by_orientation = if (present) explained else NA, stringsAsFactors = FALSE)
mk_crit <- function(aligned = 1e-12, prod_diff = 0.012) data.frame(result_id = "M1", draws = 5000L, difference_aligned = aligned, difference_production = prod_diff, stringsAsFactors = FALSE)
mk_rec <- function(err = 1e-15) data.frame(result_id = rep("M1", 2L), reconstruction_relative_error_coefficient_space = err, reconstruction_relative_error_lag_space = err, stringsAsFactors = FALSE)
mk_align <- function(cls = c("SAME_SIGN", "SIGN_FLIPPED", "SAME_SIGN")) data.frame(result_id = "M1", component = 1:3, class = cls, stringsAsFactors = FALSE)

test_that("C orientation-aligned simulation: a valid orientation difference PASSES; unexplained difference, aligned disagreement, factor reconstruction error and unresolved orientation FAIL", {
  ok <- .siw_v2_status_C(mk_rec(), mk_mech(), mk_align(), mk_crit()); expect_identical(ok$status, "PASS"); expect_true(any(grepl("explained by orientation", ok$rows$check)))
  expect_identical(.siw_v2_status_C(mk_rec(), mk_mech(explained = FALSE), mk_align(), mk_crit())$status, "FAIL")                                          # difference that orientation does not explain
  expect_identical(.siw_v2_status_C(mk_rec(), mk_mech(), mk_align(), mk_crit(aligned = 5e-6))$status, "FAIL")                                             # aligned implementations disagree beyond the covariance allowance (genuine covariance/sampling problem)
  expect_identical(.siw_v2_status_C(mk_rec(1e-9), mk_mech(), mk_align(), mk_crit())$status, "FAIL")                                                       # a factor does not reconstruct its covariance
  expect_identical(.siw_v2_status_C(mk_rec(), mk_mech(), mk_align(c("SAME_SIGN", "ORIENTATION_DIFFERS", "SAME_SIGN")), mk_crit())$status, "FAIL")        # unresolved orientation
  nodis <- .siw_v2_status_C(mk_rec(), mk_mech(present = FALSE, difference_production = 1e-13), mk_align(rep("SAME_SIGN", 3L)), mk_crit(prod_diff = 1e-13)); expect_identical(nodis$status, "PASS")
})

test_that("A production reproduction, B deterministic agreement, D native Monte Carlo and E reference: each can fail on its own genuine failure; NOT_TESTED is never PASS", {
  rp <- data.frame(result_id = "M1", draws = 5000L, standard_normals_identical_between_routes = TRUE, production_draws_reproduced_bitwise = TRUE, production_critical_reproduced_bitwise = TRUE, independent_clone_critical_difference = 0, production_critical = 2.5, difference_to_saved_production = 1e-15, stringsAsFactors = FALSE)
  expect_identical(.siw_v2_status_A("PASS", rp)$status, "PASS"); expect_identical(.siw_v2_status_A("NOT_TESTED", rp)$status, "NOT_TESTED"); expect_false(.siw_v2_status_A("NOT_TESTED", rp)$status == "PASS"); expect_identical(.siw_v2_status_A("FAIL", rp)$status, "FAIL")
  rp2 <- rp; rp2$production_draws_reproduced_bitwise <- FALSE; expect_identical(.siw_v2_status_A("PASS", rp2)$status, "FAIL"); rp3 <- rp; rp3$difference_to_saved_production <- 1e-6; expect_identical(.siw_v2_status_A("PASS", rp3)$status, "FAIL")
  ck <- data.frame(result_id = "M1", group = "CURVE_VERIFICATION", quantity = "lag_effects", max_rel_diff = 1e-12, criterion = "x", status = "PASS", stringsAsFactors = FALSE); bs <- data.frame(check = "own basis", value = 1e-16, criterion = "<= 1e-12", status = "PASS", stringsAsFactors = FALSE)
  cv <- function(v) data.frame(result_id = "M1", lag_space_relative_difference = v, stringsAsFactors = FALSE); q9 <- function(a, b) data.frame(result_id = "M1", package = a, independent = b, stringsAsFactors = FALSE)
  fm <- c(M1 = "gaussian"); fb <- c(M1 = "binomial"); expect_identical(.siw_v2_status_B(ck, bs, cv(1e-12), q9(0.6, 0.6), fm, P1)$status, "PASS")
  ck2 <- ck; ck2$status <- "FAIL"; expect_identical(.siw_v2_status_B(ck2, bs, cv(1e-12), q9(0.6, 0.6), fm, P1)$status, "FAIL")                                # a fitting failure
  expect_identical(.siw_v2_status_B(ck, bs, cv(4e-7), q9(0.6, 0.6), fm, P1)$status, "FAIL"); expect_identical(.siw_v2_status_B(ck, bs, cv(4e-7), q9(0.6, 0.6), fb, P1)$status, "PASS"); expect_identical(.siw_v2_status_B(ck, bs, cv(4e-6), q9(0.6, 0.6), fb, P1)$status, "FAIL")   # covariance on the SD scale at the existing tolerances (continuous 1e-7, binary 1e-6)
  expect_identical(.siw_v2_status_B(ck, bs, cv(1e-12), q9(0.6, 0.7), fm, P1)$status, "FAIL"); bs2 <- bs; bs2$status <- "FAIL"; expect_identical(.siw_v2_status_B(ck, bs2, cv(1e-12), q9(0.6, 0.6), fm, P1)$status, "FAIL")
  mc <- data.frame(result_id = "M1", relative_deviation_package_vs_large = 0.01, status = "PASS", stringsAsFactors = FALSE); gk <- data.frame(result_id = "M1", group = "SIMULTANEOUS_MC_STABILITY", classification = "MC_CONSISTENT_NOT_EXACT", quantity = "critical_value_5000", max_rel_diff = 0.004, criterion = "x", status = "PASS", stringsAsFactors = FALSE)
  expect_identical(.siw_v2_status_D(mc, gk, TRUE)$status, "PASS"); mc2 <- mc; mc2$status <- "FAIL"; expect_identical(.siw_v2_status_D(mc2, gk, TRUE)$status, "FAIL"); expect_identical(.siw_v2_status_D(mc, gk, FALSE)$status, "NOT_TESTED")
  rs <- data.frame(reference = c("dlmL56_audited_estimates", "dlmL56_audited_discrete", "dlmL56_audited_critical_10000"), status = c("PASS_EXACT", "PASS_EXACT", "FAIL"), n_different = c(0, 0, 4), stringsAsFactors = FALSE)
  cr <- function(rel) data.frame(result_id = "M1", draws = 10000L, production_package = 2.5, reference_critical = 2.5 * (1 + rel), stringsAsFactors = FALSE)
  e <- .siw_v2_status_E(rs, cr(0.004), P1); expect_identical(e$det$status, "PASS"); expect_identical(e$sim$status, "PASS")                                       # exact difference 0.01 is visible elsewhere; the declared Monte Carlo criterion governs
  expect_identical(.siw_v2_status_E(rs, cr(0.06), P1)$sim$status, "FAIL"); rs2 <- rs; rs2$status[1] <- "FAIL"; expect_identical(.siw_v2_status_E(rs2, cr(0.004), P1)$det$status, "FAIL"); rs3 <- rs; rs3$status[2] <- "FAIL"; expect_identical(.siw_v2_status_E(rs3, cr(0.004), P1)$sim$status, "FAIL")
  expect_false(grepl("pat", paste(deparse(body(.siw_v2_status_E)), collapse = " "), fixed = TRUE)); expect_false("pat" %in% names(formals(.siw_v2_status_E)))                  # a sign pattern is never an input of the validity decision
})

# ---- end-to-end on synthetic data: valid orientation differences (generator seed 1; V1 exact-seeded FAIL but Monte Carlo PASS)
V2_DIR <- tempfile("v2_"); dir.create(V2_DIR)
make_v2_inputs <- function(seed, name) { d <- file.path(V2_DIR, name); dir.create(d); raw <- siw_make_synthetic(seed = seed, config = CFG); csv <- file.path(d, "syn.csv"); utils::write.csv(raw, csv, row.names = FALSE, na = ""); dat <- siw_prepare_data(raw, CFG)
  f5 <- siw_final_dlm(dat, CFG, 5000L); f10 <- siw_final_dlm(dat, CFG, 10000L); acc <- file.path(d, "acc"); ref <- file.path(d, "ref"); dir.create(acc); dir.create(ref)
  utils::write.csv(f5$summary, file.path(acc, "dlm_final_summary.csv"), row.names = FALSE); utils::write.csv(f5$lag_curves, file.path(acc, "dlm_final_lag_curves.csv"), row.names = FALSE); utils::write.csv(f5$cumulative_curves, file.path(acc, "dlm_final_cumulative_curves.csv"), row.names = FALSE); s <- f5$summary; s10 <- f10$summary
  utils::write.csv(data.frame(outcome = s$outcome, metric = s$metric, n = s$n, global_statistic = s$global_statistic, global_p = s$global_p, q9 = s$q9, cumulative_effect = s$cumulative_effect, cumulative_se = s$cumulative_se, cumulative_ci_lower = s$cumulative_ci_lower, cumulative_ci_upper = s$cumulative_ci_upper, peak_effect = s$peak_effect), file.path(ref, "dlmL56_audited_estimates.csv"), row.names = FALSE)
  utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, peak_lag = s10$peak_lag, n_lags_pointwise_excl_null = s10$n_lags_pointwise_excl_null, n_lags_simultaneous_excl_null = s10$n_lags_simultaneous_excl_null), file.path(ref, "dlmL56_audited_discrete.csv"), row.names = FALSE)
  utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, simultaneous_critical = s10$simultaneous_critical), file.path(ref, "dlmL56_audited_critical_10000.csv"), row.names = FALSE); list(csv = csv, acc = acc, ref = ref, dat = dat, s10 = s10, f5 = f5) }
IN1 <- make_v2_inputs(1L, "pos"); R1 <- suppressMessages(siw_run_dlm_validation_v2(IN1$csv, file.path(V2_DIR, "out_pos"), IN1$ref, CFG, accepted_run_dir = IN1$acc, mc = TRUE, manuscript_concordance = FALSE))

test_that("positive end-to-end (valid orientation differences): every required status PASS, V1 exact-seeded FAIL kept for the record, exact differences and explanations visible, NOT_TESTED items explicit", {
  st <- R1$statuses; for (k in unlist(P2$statuses$required)) expect_identical(st[[k]], "PASS")
  expect_identical(st[["MANUSCRIPT_CONCORDANCE_STATUS"]], "NOT_TESTED"); expect_identical(st[["FIGURE_PROVENANCE_STATUS"]], "NOT_TESTED"); expect_identical(st[["DLNM_EQUALITY_STATUS"]], "NOT_TESTED"); expect_identical(R1$status, "OK_WITH_REVIEW_OR_NOT_TESTED"); expect_false(R1$status == "OK")
  expect_identical(R1$v1_record[["V1_SIMULTANEOUS_EXACT_SEEDED_STATUS"]], "FAIL"); expect_identical(R1$v1$statuses[["SIMULTANEOUS_EXACT_SEEDED_STATUS"]], "FAIL")                      # the V1 criterion is applied unchanged and its FAIL is not hidden
  X <- R1$exact_differences; d <- X[X$difference_beyond_allowance, ]; expect_gt(nrow(d), 0L); expect_true(all(d$explained_by_orientation)); expect_true(all(d$v1_exactness_band == "REALISATION_DIFFERENCE")); expect_true(all(d$flipped_components_between_routes != "")); expect_true(all(d$aligned_within_allowance)); expect_true(all(d$native_within_monte_carlo_bound)); expect_equal(nrow(X), 18L)
  expect_true(all(c("exact_difference_production_routes", "v1_exactness_band", "aligned_difference", "covariance_allowance", "reference_orientation_annotation_only", "native_relative_difference") %in% names(X)))
  cw <- utils::read.csv(file.path(R1$out_dir, "dlm_v2_check_crosswalk.csv")); expect_true(nrow(cw) >= 14L); expect_false(any(cw$criteria_changed)); ck <- R1$checks; expect_true(all(ck$status == "PASS")); expect_true(all(c("A", "B", "C", "D", "E") %in% ck$check_class))
  expect_true(all(c("v1_component", "simulation_diagnostic") %in% list.files(R1$out_dir))); S <- paste(readLines(file.path(R1$out_dir, "RUN_SUMMARY.md"), warn = FALSE), collapse = " "); expect_match(S, "never validates a reference value"); expect_match(S, "NOT_TESTED is never PASS")
})

test_that("aggregate covariance matrices, eigen orientation with the sign convention, and RNG/environment metadata are saved for every model", {
  cv <- utils::read.csv(file.path(R1$out_dir, "dlm_v2_covariance_matrices.csv")); expect_equal(nrow(cv), 9L * 2L * 9L); expect_equal(length(unique(cv$result_id)), 9L)
  for (id in unique(cv$result_id)) for (r in c("production", "independent")) { z <- cv[cv$result_id == id & cv$route == r, ]; M <- matrix(z$value[order(z$col, z$row)], 3L); expect_true(max(abs(M - t(M))) / max(abs(M)) < 1e-9); ev <- eigen(M, symmetric = TRUE)$values; e <- utils::read.csv(file.path(R1$out_dir, "dlm_v2_eigen_orientation.csv")); e <- e[e$result_id == id & e$route == r, ]; expect_equal(e$eigenvalue, ev, tolerance = 1e-9) }
  e <- utils::read.csv(file.path(R1$out_dir, "dlm_v2_eigen_orientation.csv")); expect_equal(nrow(e), 54L); expect_identical(e$native_orientation_satisfies_convention, e$native_sign_of_largest_component > 0); expect_identical(e$aligned_orientation_flip_applied, !e$native_orientation_satisfies_convention); expect_true(all(abs(e$cosine_lag_space_between_routes) > 1 - 1e-6))
  X <- R1$exact_differences; expect_equal(sum(e$flipped_between_routes[e$route == "production"]), sum(vapply(unique(X$result_id), function(id) length(strsplit(X$flipped_components_between_routes[X$result_id == id][1], ",")[[1]][nzchar(strsplit(X$flipped_components_between_routes[X$result_id == id][1], ",")[[1]])]), 0L)))
  rg <- utils::read.csv(file.path(R1$out_dir, "dlm_v2_rng_metadata.csv")); expect_equal(nrow(rg), 9L); expect_true(all(rg$rng_kind == "Mersenne-Twister" & rg$normal_kind == "Inversion" & rg$sample_kind == "Rejection")); expect_true(all(rg$draws_final == 5000L & rg$draws_audited == 10000L)); expect_match(rg$sign_convention[1], "largest absolute value positive")
  ev <- jsonlite::fromJSON(file.path(R1$out_dir, "dlm_v2_environment.json")); expect_true(all(c("r_version", "blas", "lapack_version", "platform") %in% names(ev)))
  for (f in list.files(R1$out_dir, pattern = "^dlm_v2_.*[.]csv$", full.names = TRUE)) expect_false(any(c("participant_code", "assessment_day") %in% names(utils::read.csv(f, nrows = 1))))
})

test_that("negative end-to-end: a threshold-sensitive band conclusion (generator seed 1234) is a genuine stability failure and stays FAIL", {
  IN2 <- make_v2_inputs(1234L, "thr"); R2 <- suppressMessages(siw_run_dlm_validation_v2(IN2$csv, file.path(V2_DIR, "out_thr"), IN2$ref, CFG, accepted_run_dir = IN2$acc, mc = TRUE, manuscript_concordance = FALSE))
  expect_identical(R2$statuses[["NATIVE_ORIENTATION_MC_STATUS"]], "FAIL"); expect_identical(R2$status, "VERIFICATION_FAILURE"); expect_identical(R2$statuses[["ORIENTATION_ALIGNED_SIMULATION_STATUS"]], "PASS"); expect_identical(R2$statuses[["DETERMINISTIC_AGREEMENT_STATUS"]], "PASS")
})

test_that("deliberately incorrect inputs fail the right checks: tampered accepted run (A), wrong reference estimate (E deterministic), reference critical value 20% off (E simulated); the V1 record is kept", {
  d <- file.path(V2_DIR, "bad"); dir.create(d); acc <- file.path(d, "acc"); ref <- file.path(d, "ref"); dir.create(acc); dir.create(ref); file.copy(list.files(IN1$acc, full.names = TRUE), acc); file.copy(list.files(IN1$ref, full.names = TRUE), ref)
  a <- utils::read.csv(file.path(acc, "dlm_final_lag_curves.csv")); a$estimate[100] <- a$estimate[100] + 1e-7; utils::write.csv(a, file.path(acc, "dlm_final_lag_curves.csv"), row.names = FALSE)
  e <- utils::read.csv(file.path(ref, "dlmL56_audited_estimates.csv")); e$cumulative_effect[2] <- e$cumulative_effect[2] + 1e-3; utils::write.csv(e, file.path(ref, "dlmL56_audited_estimates.csv"), row.names = FALSE)
  k <- utils::read.csv(file.path(ref, "dlmL56_audited_critical_10000.csv")); k$simultaneous_critical[4] <- k$simultaneous_critical[4] * 1.2; utils::write.csv(k, file.path(ref, "dlmL56_audited_critical_10000.csv"), row.names = FALSE)
  RB <- suppressMessages(siw_run_dlm_validation_v2(IN1$csv, file.path(V2_DIR, "out_bad"), ref, CFG, accepted_run_dir = acc, mc = FALSE, manuscript_concordance = FALSE)); st <- RB$statuses
  expect_identical(st[["PRODUCTION_REPRODUCTION_STATUS"]], "FAIL"); expect_identical(st[["REFERENCE_DETERMINISTIC_STATUS"]], "FAIL"); expect_identical(st[["REFERENCE_SIMULATION_MC_STATUS"]], "FAIL"); expect_identical(st[["NATIVE_ORIENTATION_MC_STATUS"]], "NOT_TESTED"); expect_identical(RB$status, "VERIFICATION_FAILURE")
  expect_identical(st[["DETERMINISTIC_AGREEMENT_STATUS"]], "PASS"); expect_identical(st[["ORIENTATION_ALIGNED_SIMULATION_STATUS"]], "PASS"); expect_identical(RB$v1_record[["V1_REFERENCE_COMPARISON_STATUS"]], "FAIL")
})

test_that("a reference critical value that is an orientation variant of the production simulation is judged by the Monte Carlo criterion (PASS), with the exact difference and the sign pattern visible as annotation, and the V1 reference FAIL kept", {
  xc <- siw_tier_matrix(IN1$dat, CFG$tiers$M2_L); i <- which(IN1$s10$outcome == "Int_dummy" & IN1$s10$metric == "UTCI"); cp <- siw_dlm_sim_capture(IN1$dat, CFG, "Int_dummy", "UTCI", 10000L, xc); X <- siw_dlm_sim_std_normals(10000L, 3L, cp$fit$summary$seed); lse <- sqrt(pmax(0, diag(cp$B %*% cp$Sigma %*% t(cp$B))))
  pats <- cbind(1, as.matrix(expand.grid(rep(list(c(1, -1)), 2L)))); vals <- vapply(seq_len(nrow(pats)), function(j) siw_dlm_sim_critical(t(siw_dlm_sim_factor(cp$Sigma, cp$B, "production_eigen", signs = pats[j, ])$F %*% t(X)), cp$B, lse), 0); other <- vals[abs(vals - cp$fit$summary$simultaneous_critical) > 1e-6]; expect_gt(length(other), 0L)
  d <- file.path(V2_DIR, "variant"); dir.create(d); ref <- file.path(d, "ref"); dir.create(ref); file.copy(list.files(IN1$ref, full.names = TRUE), ref); k <- utils::read.csv(file.path(ref, "dlmL56_audited_critical_10000.csv")); k$simultaneous_critical[i] <- other[1L]; utils::write.csv(k, file.path(ref, "dlmL56_audited_critical_10000.csv"), row.names = FALSE)
  RV <- suppressMessages(siw_run_dlm_validation_v2(IN1$csv, file.path(V2_DIR, "out_variant"), ref, CFG, accepted_run_dir = IN1$acc, mc = FALSE, manuscript_concordance = FALSE)); expect_identical(RV$statuses[["REFERENCE_SIMULATION_MC_STATUS"]], "PASS"); expect_identical(RV$v1_record[["V1_REFERENCE_COMPARISON_STATUS"]], "FAIL")
  Xv <- RV$exact_differences; z <- Xv[Xv$result_id == IN1$s10$result_id[i] & Xv$draws == 10000L, ]; expect_gt(z$exact_difference_reference_vs_production, 1e-6); expect_identical(z$reference_v1_band, "REALISATION_DIFFERENCE"); expect_true(!is.na(z$reference_orientation_annotation_only) && z$reference_orientation_annotation_only != "none")
  S <- paste(readLines(file.path(RV$out_dir, "RUN_SUMMARY.md"), warn = FALSE), collapse = " "); expect_match(S, "never validates a reference value"); expect_false(grepl("exact historical reproduction", S) && !grepl("no claim", paste(readLines(file.path(RV$out_dir, "DLM_V2_STATUS.txt"), warn = FALSE), collapse = " ")))
})

test_that("V2 code changes nothing in production: accepted outputs identical, no deletions, no figure code, no installs, no seed or epsilon edits", {
  if (siw_skip_source_only("test-21-dlm-validation-v2: V2 code changes nothing in production: accepted outputs identical, no ")) return(invisible(NULL))
  expect_identical(siw_final_dlm(IN1$dat, CFG, 5000L)$summary, IN1$f5$summary); expect_identical(siw_final_dlm(IN1$dat, CFG, 5000L)$lag_curves, IN1$f5$lag_curves)
  src <- paste(readLines(file.path(ROOT, "R", "dlm_v2.R"), warn = FALSE), collapse = "\n"); code <- paste(sub("#.*$", "", strsplit(src, "\n")[[1]]), collapse = "\n")
  for (bad in c("unlink(", "file.remove(", "file.rename(", "install.packages", "plot(", "ggplot", "png(", "glm_epsilon <-", "config$dlm$final$draws <-", "set.seed(", "$seed <-")) expect_false(grepl(bad, code, fixed = TRUE))
})
