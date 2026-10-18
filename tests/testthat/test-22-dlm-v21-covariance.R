# Phase 3D V2.1 candidate: corrected covariance rule (exact worst-case relative standard-error difference over all linear contrasts). Synthetic matrices and a synthetic V2 run folder only; no study data, no refit.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
if (!exists("expect_lt", mode = "function")) expect_lt <- function(a, b) expect_true(a < b)
if (!exists("expect_null", mode = "function")) expect_null <- function(x) expect_true(is.null(x))
P21 <- siw_dlm_validation_protocol_v21(); P1 <- siw_dlm_validation_protocol(); P2 <- siw_dlm_validation_protocol_v2(); hsh <- function(f) siw_file_hash(file.path(ROOT, f))$value
rspd <- function(seed, p = 3L, spread = c(1, 0.1, 0.01)) { set.seed(seed); Q <- qr.Q(qr(matrix(stats::rnorm(p * p), p))); Q %*% diag(spread, p) %*% t(Q) }
ratio_se <- function(a, Vc, Vr) abs(sqrt(drop(t(a) %*% Vc %*% a) / drop(t(a) %*% Vr %*% a)) - 1)

test_that("V2.1 is declared as a post hoc candidate correction; V1, V2 and their files are preserved; tolerances come from the V1 protocol fields named in the protocol", {
  if (siw_skip_source_only("test-22-dlm-v21-covariance: V2.1 is declared as a post hoc candidate correction; V1, V2 and their ")) return(invisible(NULL))
  expect_match(P21$version, "candidate"); expect_match(P21$status_of_this_document, "POST HOC"); expect_match(P21$status_of_this_document, "NOT a prespecified criterion"); expect_match(P21$status_of_this_document, "NOT an unchanged criterion"); expect_identical(P21$statuses$adoption, "PENDING_REVIEW")
  expect_private(P1$protocol_hash, "dlm_validation_protocol"); expect_private(P2$protocol_hash, "dlm_validation_protocol_v2"); expect_identical(hsh("R/dlm_v2.R"), "C5A14991661A7A3C3A95728128A5EBDBD87C0AD3ED5F5D78B918868F6F05E652"); expect_identical(hsh("R/dlm_simdiag.R"), "928460680FDA5A17A6318D6A7663398C54C5585F9194EB5679D80F0E5EF00C27"); expect_identical(hsh("R/dlm_validate.R"), "6E1467C5DE4027C35E35F4A3A95EC2B4D057D175FAC1709E872FC4BCA00126A2")
  tl <- P21$new_rule$tolerance; expect_equal(tl$continuous_outcomes$value, P1$criteria$independent_route$fitted_quantities_rel); expect_equal(tl$binary_outcome_at_accepted_epsilon$value, P1$criteria$independent_route$binomial_fitted_quantities_rel_accepted_epsilon); expect_identical(tl$continuous_outcomes$source_field, "criteria.independent_route.fitted_quantities_rel"); expect_identical(tl$binary_outcome_at_accepted_epsilon$source_field, "criteria.independent_route.binomial_fitted_quantities_rel_accepted_epsilon")
  expect_equal(c(tl$continuous_outcomes$value, tl$binary_outcome_at_accepted_epsilon$value), c(1e-7, 1e-6)); expect_match(tl$note, "not inferred from observed results"); expect_match(P21$new_rule$invalid_inputs, "No jitter"); expect_match(P21$replaced_rule$why_invalid, "single largest matrix element")
  expect_true(file.exists(file.path(ROOT, "config", "validation_plan_dlm.json")) && is.null(jsonlite::fromJSON(file.path(ROOT, "config", "validation_plan_dlm.json"))$independent_route))           # the reference plan declares no independent-route SE tolerance; the field source is the V1 protocol
})

test_that("delta_SE: equality gives 0; a common variance scaling s^2 gives |s - 1| exactly; boundaries are inclusive", {
  V <- rspd(1); d <- siw_dlm_delta_se(V, V); expect_true(d$ok); expect_lt(d$delta_se, 1e-14); expect_equal(d$lambda, rep(1, 3), tolerance = 1e-12)
  for (s in c(0.9, 1.0001, 1 + 1e-7, 2)) { d <- siw_dlm_delta_se(s^2 * V, V); expect_true(abs(d$delta_se - abs(s - 1)) < 1e-13); expect_equal(d$lambda, rep(s^2, 3), tolerance = 1e-10) }
  tol <- 1e-7; expect_true(siw_dlm_delta_se((1 + 0.99e-7)^2 * V, V)$delta_se <= tol); expect_true(siw_dlm_delta_se((1 + 1.01e-7)^2 * V, V)$delta_se > tol); expect_true(abs(siw_dlm_delta_se((1 + 1e-6)^2 * V, V)$delta_se - 1e-6) < 1e-12)
  expect_equal(siw_dlm_delta_se(V * 1.21, V)$delta_se, 0.1, tolerance = 1e-12); expect_equal(siw_dlm_delta_se(V, V * 1.21)$delta_se, 1 - 1 / 1.1, tolerance = 1e-12); expect_equal(siw_dlm_delta_se(V * 1.21, V)$delta_se_swapped, 1 - 1 / 1.1, tolerance = 1e-12)   # the value is not symmetric in the roles; both reported
})

test_that("delta_SE is the exact supremum over all nonzero contrasts: no direction exceeds it, a generalized eigenvector attains it", {
  Vr <- rspd(2); set.seed(9); E <- matrix(stats::rnorm(9, 0, 0.02), 3); Vc <- Vr + (E + t(E)) / 2 * max(diag(Vr)); d <- siw_dlm_delta_se(Vc, Vr); expect_true(d$ok)
  A <- matrix(stats::rnorm(3 * 20000L), 3); sup <- max(apply(A, 2L, function(a) ratio_se(a, Vc, Vr))); expect_true(sup <= d$delta_se * (1 + 1e-12)); expect_gt(sup, 0.5 * d$delta_se)
  R <- chol(Vr); M <- t(backsolve(R, t(backsolve(R, Vc, transpose = TRUE)), transpose = TRUE)); ev <- eigen((M + t(M)) / 2, symmetric = TRUE); j <- which.max(abs(sqrt(ev$values) - 1)); a <- backsolve(R, ev$vectors[, j]); expect_equal(ratio_se(a, Vc, Vr), d$delta_se, tolerance = 1e-10)
})

test_that("off-diagonal errors and errors in small-variance directions are caught; the old max-element proxy misses them", {
  Vr <- matrix(c(1, 0.99, 0.99, 1), 2); Vc <- Vr; Vc[1, 2] <- Vc[2, 1] <- 0.995; d <- siw_dlm_delta_se(Vc, Vr); proxy <- max(abs(Vc - Vr)) / max(abs(Vr)) / 2
  expect_gt(d$delta_se, 20 * proxy); expect_equal(d$delta_se, max(abs(sqrt(eigen(solve(Vr) %*% Vc)$values) - 1)), tolerance = 1e-10)                                      # an off-diagonal error is large for the contrast a = (1, -1)
  Vr2 <- diag(c(1, 1e-6)); Vc2 <- diag(c(1, 1.2e-6)); d2 <- siw_dlm_delta_se(Vc2, Vr2); expect_equal(d2$delta_se, sqrt(1.2) - 1, tolerance = 1e-10); proxy2 <- max(abs(Vc2 - Vr2)) / max(abs(Vr2)) / 2; expect_lt(proxy2, 1e-6); expect_gt(d2$delta_se / proxy2, 1e5)       # 9.5% error in a small-variance direction, invisible to the proxy
  expect_equal(siw_dlm_delta_se(diag(c(1, 1e-6)) * c(1, 1), Vr2)$delta_se, 0, tolerance = 1e-15)
})

test_that("invariance under a common invertible coordinate transformation; the symmetric part is used and asymmetry is recorded without changing contrast variances", {
  Vr <- rspd(3); set.seed(4); E <- matrix(stats::rnorm(9, 0, 0.01), 3); Vc <- Vr + (E + t(E)) / 2; d <- siw_dlm_delta_se(Vc, Vr)$delta_se
  for (sd in 1:5) { set.seed(sd); T <- matrix(stats::rnorm(9), 3) + diag(3) * 2; expect_equal(siw_dlm_delta_se(T %*% Vc %*% t(T), T %*% Vr %*% t(T))$delta_se, d, tolerance = 1e-9) }
  Va <- Vc; Va[1, 3] <- Va[1, 3] + 1e-3; a <- siw_dlm_delta_se(Va, Vr); expect_true(a$ok); expect_gt(a$asymmetry_candidate, 0); expect_gt(a$asymmetry_affine_candidate, 0); expect_equal(a$delta_se, siw_dlm_delta_se((Va + t(Va)) / 2, Vr)$delta_se, tolerance = 1e-14)
  set.seed(5); for (k in 1:5) { x <- stats::rnorm(3); expect_equal(drop(t(x) %*% Va %*% x), drop(t(x) %*% ((Va + t(Va)) / 2) %*% x), tolerance = 1e-12) }
  expect_equal(siw_dlm_delta_se(Vc, Vr)$asymmetry_candidate, 0)
})

test_that("invalid matrices FAIL without repair: non-finite, non-square, mismatched, indefinite, singular (rank deficient), zero; reasons are stated", {
  V <- rspd(6); inv <- function(a, b) siw_dlm_delta_se(a, b)
  for (bad in list(inv(V * NA, V), inv(V, replace(V, 1, Inf)), inv(V[1:2, ], V), inv(V, V[1:2, 1:2]), inv(V, diag(c(1, -1, 1))), inv(diag(c(1, -1, 1)), V), inv(V, matrix(1, 3, 3)), inv(V, matrix(0, 3, 3)), inv("a", V), inv(V, matrix(numeric(0), 0, 0)))) { expect_false(bad$ok); expect_true(is.na(bad$delta_se)); expect_true(nzchar(bad$reason)) }
  expect_match(inv(V, diag(c(1, -1, 1)))$reason, "not positive definite"); expect_match(inv(V, matrix(1, 3, 3))$reason, "not positive definite"); expect_match(inv(V * NA, V)$reason, "non-finite")
  S56 <- siw_lag_basis(56L, 3L) %*% V %*% t(siw_lag_basis(56L, 3L)); expect_false(inv(S56, S56)$ok)                                                                                    # the singular 56 x 56 lag covariance is rejected: only the full-rank 3 x 3 matrices are valid input
  src <- paste(deparse(body(siw_dlm_delta_se)), collapse = " "); for (w in c("jitter", "pmax", "pmin", "diag(", "+ 1e-")) expect_false(grepl(w, src, fixed = TRUE))
})

# ---- end-to-end on a synthetic V2 run folder (aggregate files only are read by the V2.1 tool)
V21_DIR <- tempfile("v21_"); dir.create(V21_DIR)
make_manifest <- function(dir) { f <- sort(list.files(dir, recursive = TRUE)); f <- f[!(f %in% c("RUN_MANIFEST.csv", "finalise_console.log"))]; h <- lapply(file.path(dir, f), siw_file_hash); utils::write.csv(data.frame(file = f, bytes = file.info(file.path(dir, f))$size, algorithm = vapply(h, function(z) z$algorithm, ""), hash = vapply(h, function(z) z$value, ""), stringsAsFactors = FALSE), file.path(dir, "RUN_MANIFEST.csv"), row.names = FALSE) }
SYN4 <- siw_make_synthetic(seed = 4L, config = CFG); CSV4 <- file.path(V21_DIR, "syn.csv"); utils::write.csv(SYN4, CSV4, row.names = FALSE, na = ""); DAT4 <- siw_prepare_data(SYN4, CFG); F5 <- siw_final_dlm(DAT4, CFG, 5000L); F10 <- siw_final_dlm(DAT4, CFG, 10000L); s <- F5$summary; s10 <- F10$summary
acc <- file.path(V21_DIR, "acc"); ref <- file.path(V21_DIR, "ref"); dir.create(acc); dir.create(ref); utils::write.csv(F5$summary, file.path(acc, "dlm_final_summary.csv"), row.names = FALSE); utils::write.csv(F5$lag_curves, file.path(acc, "dlm_final_lag_curves.csv"), row.names = FALSE); utils::write.csv(F5$cumulative_curves, file.path(acc, "dlm_final_cumulative_curves.csv"), row.names = FALSE)
utils::write.csv(data.frame(outcome = s$outcome, metric = s$metric, n = s$n, global_statistic = s$global_statistic, global_p = s$global_p, q9 = s$q9, cumulative_effect = s$cumulative_effect, cumulative_se = s$cumulative_se, cumulative_ci_lower = s$cumulative_ci_lower, cumulative_ci_upper = s$cumulative_ci_upper, peak_effect = s$peak_effect), file.path(ref, "dlmL56_audited_estimates.csv"), row.names = FALSE)
utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, peak_lag = s10$peak_lag, n_lags_pointwise_excl_null = s10$n_lags_pointwise_excl_null, n_lags_simultaneous_excl_null = s10$n_lags_simultaneous_excl_null), file.path(ref, "dlmL56_audited_discrete.csv"), row.names = FALSE); utils::write.csv(data.frame(outcome = s10$outcome, metric = s10$metric, simultaneous_critical = s10$simultaneous_critical), file.path(ref, "dlmL56_audited_critical_10000.csv"), row.names = FALSE)
V2R <- file.path(V21_DIR, "v2run"); invisible(suppressMessages(siw_run_dlm_validation_v2(CSV4, V2R, ref, CFG, accepted_run_dir = acc, mc = TRUE, manuscript_concordance = FALSE))); make_manifest(V2R); copy_run <- function(name) { d <- file.path(V21_DIR, name); dir.create(d); file.copy(list.files(V2R, full.names = TRUE), d, recursive = TRUE); d }
h_before <- unlist(lapply(sort(list.files(V2R, recursive = TRUE, full.names = TRUE)), function(f) siw_file_hash(f)$value)); R21 <- suppressMessages(siw_run_dlm_covariance_v21(V2R, file.path(V21_DIR, "out_ok"), CFG))

test_that("synthetic V2 run: provenance verified, covariance rule PASS for all nine at the V1 tolerances, V2 folder untouched, earlier statuses carried forward with explicit labels", {
  st <- R21$statuses; expect_identical(st[["EXECUTION_STATUS"]], "PASS"); expect_identical(st[["INPUT_PROVENANCE_STATUS"]], "PASS"); expect_identical(st[["COVARIANCE_RULE_V21_STATUS"]], "PASS"); expect_identical(st[["SOURCE_INTEGRITY_STATUS"]], "PASS"); expect_true(all(R21$provenance$ok)); expect_identical(R21$status, "OK_WITH_CARRIED_FORWARD_OPEN_ITEMS")
  K <- R21$checks; expect_equal(nrow(K), 9L); expect_equal(length(unique(K$result_id)), 9L); expect_true(all(K$status == "PASS")); expect_true(all(K$tolerance == ifelse(K$family == "binomial", 1e-6, 1e-7))); expect_true(all(grepl("^criteria[.]independent_route", K$tolerance_source_field))); expect_true(all(K$delta_se <= K$tolerance)); expect_true(all(K$symmetric_part_used))
  expect_true(all(K$delta_se_at_least_direct_se_differences)); expect_true(all(c("delta_se_swapped_roles", "asymmetry_candidate_affine", "v2_proxy_half_max_element_metric", "v2_proxy_over_delta_se", "direct_max_rel_lag_se_v1", "direct_max_rel_cumulative_se_v1") %in% names(K)))
  expect_identical(h_before, unlist(lapply(sort(list.files(V2R, recursive = TRUE, full.names = TRUE)), function(f) siw_file_hash(f)$value)))
  cf <- utils::read.csv(file.path(R21$out_dir, "dlm_v21_carried_forward_v2_statuses.csv")); expect_true(all(grepl("previously reviewed evidence", cf$label))); expect_true(all(grepl("not recomputed", cf$label))); d <- utils::read.csv(file.path(R21$out_dir, "dlm_v21_carried_forward_direct_se_checks.csv")); expect_true(all(grepl("retained unchanged", d$label))); expect_true(all(c("lag_standard_errors", "cumulative_se_curve") %in% d$quantity))
  S <- paste(readLines(file.path(R21$out_dir, "DLM_V21_STATUS.txt"), warn = FALSE), collapse = " "); expect_match(S, "ADOPTION=PENDING_REVIEW"); expect_match(S, "post hoc correction, not a prespecified or unchanged criterion"); expect_match(S, "previously reviewed evidence, not recomputed")
  mcT <- utils::read.csv(file.path(R21$out_dir, "dlm_v21_monte_carlo_rule_actuals.csv")); expect_equal(nrow(mcT), 9L); expect_true(all(mcT$threshold_deviation == 0.05 & mcT$threshold_spread == 0.2)); expect_true(all(mcT$deviation_from_100000_draw_value <= mcT$threshold_deviation)); x2 <- utils::read.csv(file.path(R21$out_dir, "dlm_v21_cross_implementation_actuals.csv")); expect_true(all(x2$native_monte_carlo_bound == 0.05))
  tl <- utils::read.csv(file.path(R21$out_dir, "dlm_v21_tolerance_source.csv")); expect_equal(tl$value, c(1e-7, 1e-6)); expect_true(all(tl$source_file == "config/dlm_validation_protocol.json")); expect_identical(tl$v1_protocol_hash[1], P1$protocol_hash)
})

test_that("environment metadata is captured; unavailable Windows library paths are labelled, never guessed", {
  e <- jsonlite::fromJSON(file.path(R21$out_dir, "dlm_v21_environment.json"), simplifyVector = FALSE); expect_true(file.exists(file.path(R21$out_dir, "dlm_v21_sessionInfo.txt"))); expect_true(any(grepl("R version", readLines(file.path(R21$out_dir, "dlm_v21_sessionInfo.txt"), warn = FALSE))))
  expect_identical(e$la_version$available, nzchar(La_version())); expect_identical(e$la_library$available, nzchar(tryCatch(La_library(), error = function(x) ""))); expect_identical(e$blas$available, nzchar(unname(extSoftVersion()["BLAS"])))
  for (f in list(e$la_library, e$blas)) if (!f$available) { expect_match(f$note, "UNAVAILABLE"); expect_match(f$note, "not inferred"); expect_identical(f$value, "") }
  expect_true(is.list(e$extSoftVersion)); expect_true(all(c("r_version", "platform") %in% names(e)))
})

test_that("missing inputs are reported exactly and nothing is refitted; tampered or inconsistent inputs fail provenance", {
  d1 <- copy_run("miss"); file.remove(file.path(d1, "dlm_v2_covariance_matrices.csv")); file.remove(file.path(d1, "simulation_diagnostic", "dlm_sd_covariance_trace.csv")); r1 <- suppressMessages(siw_run_dlm_covariance_v21(d1, file.path(V21_DIR, "out_miss"), CFG))
  expect_identical(sort(r1$missing), sort(c("dlm_v2_covariance_matrices.csv", "simulation_diagnostic/dlm_sd_covariance_trace.csv"))); expect_identical(r1$statuses[["INPUT_PROVENANCE_STATUS"]], "FAIL"); expect_identical(r1$statuses[["COVARIANCE_RULE_V21_STATUS"]], "NOT_TESTED"); expect_false(r1$statuses[["COVARIANCE_RULE_V21_STATUS"]] == "PASS"); expect_null(r1$checks)
  S <- paste(readLines(file.path(V21_DIR, "out_miss", "DLM_V21_STATUS.txt"), warn = FALSE), collapse = " "); expect_match(S, "MISSING_FILES=dlm_v2_covariance_matrices.csv;simulation_diagnostic/dlm_sd_covariance_trace.csv"); expect_match(S, "NOTHING WAS REFITTED"); expect_false(file.exists(file.path(V21_DIR, "out_miss", "dlm_v21_covariance_checks.csv")))
  d2 <- copy_run("tamper"); cm <- utils::read.csv(file.path(d2, "dlm_v2_covariance_matrices.csv")); cm$value[1] <- cm$value[1] * 1.001; utils::write.csv(cm, file.path(d2, "dlm_v2_covariance_matrices.csv"), row.names = FALSE); r2 <- suppressMessages(siw_run_dlm_covariance_v21(d2, file.path(V21_DIR, "out_tamper"), CFG)); expect_identical(r2$statuses[["INPUT_PROVENANCE_STATUS"]], "FAIL"); expect_true(any(!r2$provenance$ok & grepl("manifest", r2$provenance$item)))
  d3 <- copy_run("coord"); ag <- utils::read.csv(file.path(d3, "simulation_diagnostic", "dlm_sd_covariance_agreement.csv")); ag$rotation_max_deviation_from_identity[2] <- 0.3; utils::write.csv(ag, file.path(d3, "simulation_diagnostic", "dlm_sd_covariance_agreement.csv"), row.names = FALSE); make_manifest(d3); r3 <- suppressMessages(siw_run_dlm_covariance_v21(d3, file.path(V21_DIR, "out_coord"), CFG)); expect_identical(r3$statuses[["INPUT_PROVENANCE_STATUS"]], "FAIL"); expect_true(any(!r3$provenance$ok & grepl("identical coordinates", r3$provenance$item)))
  d4 <- copy_run("proto"); m4 <- jsonlite::fromJSON(file.path(d4, "dlm_v2_metadata.json"), simplifyVector = FALSE); m4$v2_protocol_hash <- "00"; writeLines(jsonlite::toJSON(m4, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), file.path(d4, "dlm_v2_metadata.json")); make_manifest(d4); r4 <- suppressMessages(siw_run_dlm_covariance_v21(d4, file.path(V21_DIR, "out_proto"), CFG)); expect_identical(r4$statuses[["INPUT_PROVENANCE_STATUS"]], "FAIL")
})

test_that("invalid or out-of-tolerance matrices inside a run fail that model only, without repair; the tolerance boundary is inclusive", {
  d <- copy_run("badmat"); cm <- utils::read.csv(file.path(d, "dlm_v2_covariance_matrices.csv")); id1 <- unique(cm$result_id)[1]; id2 <- unique(cm$result_id)[2]; id3 <- unique(cm$result_id)[3]; get <- function(id, r) { z <- cm[cm$result_id == id & cm$route == r, ]; matrix(z$value[order(z$col, z$row)], 3L) }
  put <- function(id, r, M) { i <- which(cm$result_id == id & cm$route == r); o <- order(cm$col[i], cm$row[i]); cm$value[i][o] <<- as.vector(M) }
  Vr1 <- get(id1, "independent"); put(id1, "production", (1 + 0.99e-7)^2 * Vr1); put(id2, "production", diag(c(1, -1, 1))); Vr3 <- get(id3, "independent"); put(id3, "production", (1 + 1.5e-7)^2 * Vr3)
  utils::write.csv(cm, file.path(d, "dlm_v2_covariance_matrices.csv"), row.names = FALSE); make_manifest(d); r <- suppressMessages(siw_run_dlm_covariance_v21(d, file.path(V21_DIR, "out_badmat"), CFG)); K <- r$checks; fam <- stats::setNames(K$family, K$result_id)
  expect_identical(r$statuses[["INPUT_PROVENANCE_STATUS"]], "PASS"); expect_identical(r$statuses[["COVARIANCE_RULE_V21_STATUS"]], "FAIL"); expect_identical(r$status, "VERIFICATION_FAILURE")
  k1 <- K[K$result_id == id1, ]; k2 <- K[K$result_id == id2, ]; k3 <- K[K$result_id == id3, ]; if (identical(fam[[id1]], "gaussian")) expect_identical(k1$status, "PASS"); expect_identical(k2$status, "FAIL"); expect_true(is.na(k2$delta_se)); expect_match(k2$invalid_reason, "not positive definite"); if (identical(fam[[id3]], "gaussian")) expect_identical(k3$status, "FAIL")
  expect_equal(k1$delta_se, 0.99e-7, tolerance = 1e-4); expect_equal(k3$delta_se, 1.5e-7, tolerance = 1e-4); expect_true(sum(K$status == "PASS") == 7L)
})

test_that("the V2.1 tool reads aggregate files only: no fitting, no data import, no jitter, no deletions, and V1/V2 outputs are never written", {
  if (siw_skip_source_only("test-22-dlm-v21-covariance: the V2.1 tool reads aggregate files only: no fitting, no data import, ")) return(invisible(NULL))
  src <- paste(readLines(file.path(ROOT, "R", "dlm_v21.R"), warn = FALSE), collapse = "\n"); code <- paste(sub("#.*$", "", strsplit(src, "\n")[[1]]), collapse = "\n")
  for (bad in c("siw_fit_dlm", "siw_final_dlm", "siw_read_study_csv", "siw_prepare_data", "siw_dlm_indep_fit", "siw_make_synthetic", "glm(", "lm(", "jitter", "unlink(", "file.remove(", "file.rename(", "install.packages", "set.seed(")) expect_false(grepl(bad, code, fixed = TRUE))
  expect_false(grepl("write_csv\\([^\n]*v2_run_dir", code)); expect_match(src, "SIW_V21_OUTPUT_INSIDE_V2_RUN")
  expect_identical(CFG$dlm$final$glm_epsilon, 1e-12); expect_identical(CFG$dlm$final$draws, 5000L)
})
