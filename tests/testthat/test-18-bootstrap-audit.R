# Phase 3C.2: diagnostic-only audit of saved Batch C bootstrap estimates. Synthetic checkpoints only; no study data, no participant data, no refitting.
if (!exists("expect_null", mode = "function")) expect_null <- function(x) expect_true(is.null(x))
if (!exists("expect_gt", mode = "function")) expect_gt <- function(a, b) expect_true(a > b)
if (!exists("expect_lt", mode = "function")) expect_lt <- function(a, b) expect_true(a < b)
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_length", mode = "function")) expect_length <- function(x, n) expect_true(length(x) == n)
RB <- 100L; PLAN18 <- siw_bootstrap_plan(CFG, RB)
SYN_PRIMARY <- 0.1; SYN_SE <- 0.05

# Build a synthetic Batch C run folder with the real checkpoint schema (list(signature, target_id, value = list(estimates, reason, glm_not_converged, level_absent, other_aliased, n_complete, seconds, summary))).
# `mutate(i, id, est)` may alter the estimates of target i before the summary is computed; `summary_mutate(i, s)` may alter the stored checkpoint summary afterwards (to test reconstruction).
make_audit_run <- function(dir, mutate = function(i, id, est) est, flagged = 1:37, summary_mutate = function(i, s) s, run_summary_mutate = function(i, s) s) {
  dir.create(file.path(dir, "checkpoints"), recursive = TRUE); set.seed(11); parts <- c("INPUTHASH", "CONFIGHASH", "PROTOCOLHASH", "SOURCEFP"); S <- list(); Y <- list()
  for (i in seq_len(nrow(PLAN18))) {
    p <- PLAN18[i, ]; est <- mutate(i, p$target_id, stats::rnorm(RB, SYN_PRIMARY, SYN_SE)); reason <- ifelse(is.finite(est), "", "EXPOSURE_ALIASED"); sm <- siw_bootstrap_summarise(est, RB)
    val <- list(estimates = est, reason = reason, glm_not_converged = rep(FALSE, RB), level_absent = rep(FALSE, RB), other_aliased = rep(FALSE, RB), n_complete = rep(50L, RB), seconds = 1, summary = summary_mutate(i, sm))
    saveRDS(list(signature = siw_string_hash(paste(c(parts, p$target_id, p$seed, RB), collapse = "|")), target_id = p$target_id, value = val), file.path(dir, "checkpoints", paste0("CKPT__", p$target_id, ".rds")))
    S[[i]] <- cbind(data.frame(result_id = p$target_id, outcome_family = p$outcome_family, resampling_type = p$resampling_type, seed = p$seed, stringsAsFactors = FALSE), run_summary_mutate(i, sm))
    Y[[i]] <- data.frame(result_id = p$target_id, primary_coefficient = SYN_PRIMARY, reference_se = SYN_SE, bootstrap_sd_to_reference_se = stats::sd(est[is.finite(est)]) / SYN_SE, validity_status = if (i %in% flagged) "REVIEW" else "OK", stringsAsFactors = FALSE)
  }
  utils::write.csv(do.call(rbind, S), file.path(dir, "bootstrap_summary.csv"), row.names = FALSE); utils::write.csv(do.call(rbind, Y), file.path(dir, "bootstrap_validity.csv"), row.names = FALSE)
  writeLines(jsonlite::toJSON(list(package = "sieccwinter", batch = "C", input_hash = "INPUTHASH", config_hash = "CONFIGHASH", protocol_hash = "PROTOCOLHASH", source_fingerprint_sha256 = "SOURCEFP", source_files = 1, replicates_requested = RB, seed = 20260729, rng = list(kind = "Mersenne-Twister"), created_utc = "synthetic"), auto_unbox = TRUE), file.path(dir, "batch_c_metadata.json"))
  utils::write.csv(siw_source_manifest()$manifest, file.path(dir, "source_manifest.csv"), row.names = FALSE)
  f <- c("bootstrap_summary.csv", "bootstrap_validity.csv", "batch_c_metadata.json", "source_manifest.csv"); h <- lapply(file.path(dir, f), siw_file_hash)
  utils::write.csv(data.frame(file = f, bytes = file.info(file.path(dir, f))$size, algorithm = "sha256", hash = vapply(h, function(z) z$value, ""), stringsAsFactors = FALSE), file.path(dir, "RUN_MANIFEST.csv"), row.names = FALSE); invisible(dir)
}
audit <- function(dir, out = paste0(dir, "_audit")) suppressMessages(siw_run_bootstrap_audit(dir, out))
BP <- siw_bootstrap_audit_protocol()
AD1 <- tempfile("synrun_"); make_audit_run(AD1); AR1 <- audit(AD1)

test_that("protocol: declared definitions, primary definition, labelling and hash; original Batch C flag thresholds unchanged", {
  expect_identical(vapply(BP$extremeness_diagnostics$definitions, function(z) z$id, ""), c("X1_abs_gt_1e6", "X2_gt_100_se", "X3_gt_10_se", "X4_gt_10_mad", "X5_gt_1000_se")); expect_identical(BP$extremeness_diagnostics$primary_definition, "X2_gt_100_se")
  expect_match(BP$extremeness_diagnostics$label, "NOT confirmed separation"); expect_true(nchar(BP$protocol_hash) == 64L); expect_identical(BP$scope$targets, 180L)
  sv <- siw_bootstrap_protocol()$scientific_validity; expect_equal(sv$min_success_fraction, 0.95); expect_equal(sv$max_glm_nonconverged_fraction, 0.01); expect_equal(unlist(sv$bootstrap_sd_to_reference_se_ratio_range), c(0.5, 2))
})

test_that("extremeness flags follow the declared definitions exactly (including strict inequalities and X4 not evaluable at MAD = 0)", {
  e <- c(0.1, 0.1 + 10 * 0.05, 0.1 + 10 * 0.05 + 1e-9, 0.1 + 100 * 0.05 + 1e-6, 2e6, 0.1 + 1000 * 0.05 + 1e-3, 0.09, 0.11, 0.1)
  f <- siw_bootstrap_extreme_flags(e, 0.1, 0.05); expect_identical(colnames(f), c("X1_abs_gt_1e6", "X2_gt_100_se", "X3_gt_10_se", "X4_gt_10_mad", "X5_gt_1000_se"))
  expect_identical(unname(f[, "X3_gt_10_se"]), c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)); expect_identical(unname(f[, "X2_gt_100_se"]), c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(unname(f[, "X1_abs_gt_1e6"]), c(rep(FALSE, 4), TRUE, rep(FALSE, 4))); expect_identical(unname(f[, "X5_gt_1000_se"]), c(rep(FALSE, 4), TRUE, TRUE, rep(FALSE, 3)))
  g <- siw_bootstrap_extreme_flags(c(rep(0.1, 60), 6), 0.1, 0.05); expect_true(all(is.na(g[, "X4_gt_10_mad"]))); expect_true(g[61, "X2_gt_100_se"])                    # MAD = 0: not evaluable, never silently FALSE
  h <- siw_bootstrap_extreme_flags(c(0.1, 0.2), 0.1, NA_real_); expect_true(all(is.na(h[, "X2_gt_100_se"])))                                                               # no usable reference SE: not evaluable
})

test_that("clean synthetic run: all statuses PASS, scope counts, aggregate outputs, nothing flagged by X1/X2 in normal data", {
  expect_identical(AR1$status, "OK"); st <- unlist(AR1$statuses); expect_true(all(st == "PASS")); expect_identical(sort(names(st)), sort(c("EXECUTION_STATUS", "CHECKPOINT_INTEGRITY_STATUS", "CHECKPOINTS_UNCHANGED_STATUS", "ACCOUNTING_STATUS", "QUANTILE_RECONSTRUCTION_STATUS", "ORIGINAL_RUN_PROVENANCE_STATUS", "SOURCE_INTEGRITY_STATUS", "OUTPUT_COMPLETENESS_STATUS")))
  sc <- AR1$tables$scope; expect_equal(nrow(sc), 180L); expect_equal(sum(sc$binary_target), 60L); expect_equal(sum(sc$previously_flagged_review), 37L)
  expect_true(all(c("audit_scope.csv", "audit_target_distribution.csv", "audit_extremeness_by_target.csv", "audit_extremeness_by_definition.csv", "audit_quantile_reconstruction.csv", "audit_tail_ranks.csv", "audit_omission_sensitivity_DIAGNOSTIC_ONLY.csv", "audit_checkpoint_checks.csv", "audit_checkpoint_hashes.csv", "audit_definitions.csv", "audit_provenance.json", "BOOTSTRAP_DIAGNOSTICS_STATUS.txt", "RUN_SUMMARY.md") %in% list.files(AR1$out_dir)))
  x2 <- AR1$tables$extreme; expect_equal(nrow(x2), 180L * 5L); expect_equal(sum(x2$n_extreme[x2$definition %in% c("X1_abs_gt_1e6", "X2_gt_100_se", "X5_gt_1000_se")], na.rm = TRUE), 0)
  pj <- jsonlite::fromJSON(file.path(AR1$out_dir, "audit_provenance.json"), simplifyVector = FALSE); expect_identical(pj$original_run$input_hash, "INPUTHASH"); expect_equal(pj$checkpoints, 180L); expect_true(length(pj$provenance_evidence_unavailable) >= 6L); expect_false(pj$tool_source$changed_during_audit)
  expect_identical(length(pj$tool_source$fingerprint_start), 1L); expect_match(pj$tool_source$fingerprint_start, "^[0-9A-F]{64}$")
})

test_that("outputs are aggregate only: no replicate-level estimates, every table has one row per target (or per target and definition)", {
  for (f in list.files(AR1$out_dir, pattern = "[.]csv$")) { d <- utils::read.csv(file.path(AR1$out_dir, f), stringsAsFactors = FALSE); expect_false(any(c("estimates", "estimate", "replicate", "replicate_index") %in% names(d)))
    if ("result_id" %in% names(d) && !f %in% c("audit_extremeness_by_target.csv", "audit_tail_ranks.csv", "audit_omission_sensitivity_DIAGNOSTIC_ONLY.csv")) expect_equal(nrow(d), 180L) }
  expect_equal(nrow(utils::read.csv(file.path(AR1$out_dir, "audit_omission_sensitivity_DIAGNOSTIC_ONLY.csv"))), 900L)
})

test_that("known tail contamination: 3% contamination reaches the upper type-8 limit, 1% does not; omission changes the limit in the first case only", {
  n <- RB; mk <- function(k) function(i, id, est) if (i == 1L) { est[order(est)[(n - k + 1L):n]] <- 1e10; est } else est
  d3 <- tempfile("syn3_"); make_audit_run(d3, mutate = mk(3L)); r3 <- audit(d3); d1 <- tempfile("syn1_"); make_audit_run(d1, mutate = mk(1L)); r1 <- audit(d1)
  jh <- function(k) floor(n * 0.975 + (0.975 + 1) / 3 + 4 * .Machine$double.eps); expect_equal(jh(), 98)                                                                                       # n = 100: defining order statistics 98 and 99
  rk3 <- r3$tables$ranks; rk3 <- rk3[rk3$result_id == PLAN18$target_id[1L] & rk3$definition == "X2_gt_100_se", ]; expect_equal(rk3$n_flagged, 3L); expect_true(rk3$upper_defining_flagged); expect_false(rk3$lower_defining_flagged); expect_equal(rk3$n_flagged_above, 3L); expect_equal(rk3$n_flagged_below, 0L)
  rk1 <- r1$tables$ranks; rk1 <- rk1[rk1$result_id == PLAN18$target_id[1L] & rk1$definition == "X2_gt_100_se", ]; expect_equal(rk1$n_flagged, 1L); expect_false(rk1$upper_defining_flagged)
  q3 <- r3$tables$reconstruction[1L, ]; expect_gt(q3$upper_reconstructed, 1e9); q1 <- r1$tables$reconstruction[1L, ]; expect_lt(q1$upper_reconstructed, 1)                                        # with 3 estimates the original limit is itself extreme; with 1 it is not
  o3 <- r3$tables$omission; o3 <- o3[o3$result_id == PLAN18$target_id[1L] & o3$definition == "X2_gt_100_se", ]; expect_equal(o3$n_omitted, 3L); expect_equal(o3$n_remaining, 97L); expect_lt(o3$filtered_upper, 1); expect_gt(o3$original_upper, 1e9); expect_gt(abs(o3$relative_change_upper), 0.99)
  e <- r3$tables$extreme; e <- e[e$result_id == PLAN18$target_id[1L], ]; expect_equal(e$n_extreme[e$definition == "X1_abs_gt_1e6"], 3L); expect_equal(e$n_extreme[e$definition == "X2_gt_100_se"], 3L); expect_equal(e$n_extreme_high[e$definition == "X2_gt_100_se"], 3L); expect_equal(e$proportion_extreme[e$definition == "X2_gt_100_se"], 0.03)
  t3 <- r3$tables$target[1L, ]; expect_gt(t3$sd, 1e8); expect_lt(t3$sd_without_X2, 1); expect_lt(t3$mad_scaled, 0.2); expect_equal(t3$maximum, 1e10)
  expect_true(all(unlist(r3$statuses) == "PASS")); expect_true(r3$tables$reconstruction$agree_checkpoint[1L])                                                                                      # extreme estimates do not break reconstruction: the original limits still equal the stored ones
  expect_identical(AR1$tables$target$run_sd_ratio, AR1$tables$target$run_sd_ratio)
})

test_that("an omission-sensitivity result never replaces the original limits (original limits equal the stored summary in every row)", {
  n <- RB; d3 <- tempfile("synomit_"); make_audit_run(d3, mutate = function(i, id, est) if (i == 2L) { est[order(est)[(n - 4L):n]] <- 1e8; est } else est); r <- audit(d3)
  o <- r$tables$omission; q <- r$tables$reconstruction; m <- merge(o[o$evaluable, c("result_id", "definition", "original_lower", "original_upper")], q[, c("result_id", "lower_checkpoint", "upper_checkpoint")], by = "result_id")
  expect_true(all(abs(m$original_lower - m$lower_checkpoint) <= 1e-12 * pmax(1, abs(m$lower_checkpoint)))); expect_true(all(abs(m$original_upper - m$upper_checkpoint) <= 1e-12 * pmax(1, abs(m$upper_checkpoint))))
  expect_match(names(which(file.exists(file.path(r$out_dir, list.files(r$out_dir, pattern = "omission"))))), "DIAGNOSTIC_ONLY")
})

test_that("non-finite estimates are counted by class (NA, NaN, +Inf, -Inf) and the accounting adds up", {
  cl <- siw_bootstrap_estimate_classes(c(1, 2, NA, NaN, NaN, Inf, -Inf, -Inf, -Inf)); expect_equal(unname(cl), c(9, 2, 1, 2, 1, 3)); expect_equal(sum(cl[-1L]), cl[["n_attempted"]])
  dn <- tempfile("synnf_"); make_audit_run(dn, mutate = function(i, id, est) { if (i == 3L) { est[1:2] <- NA; est[3] <- NaN; est[4] <- Inf; est[5] <- -Inf }; est }); r <- audit(dn)
  tg <- r$tables$target[r$tables$target$result_id == PLAN18$target_id[3L], ]; expect_equal(c(tg$n_finite, tg$n_na, tg$n_nan, tg$n_pos_inf, tg$n_neg_inf), c(95, 2, 1, 1, 1)); expect_equal(tg$n_attempted, 100)
  expect_true(all(r$tables$checks$accounting_ok)); expect_identical(r$statuses$ACCOUNTING_STATUS, "PASS"); expect_true(r$tables$reconstruction$agree_checkpoint[3L])
})

test_that("type-8 reconstruction: equals stats::quantile and the independent implementation, bitwise equal to the stored summary; tampered summaries FAIL, they are never repaired", {
  q <- AR1$tables$reconstruction; expect_true(all(q$agree_checkpoint & q$agree_run_summary & q$agree_independent)); expect_true(all(q$bitwise_checkpoint)); expect_identical(AR1$statuses$QUANTILE_RECONSTRUCTION_STATUS, "PASS")
  ck <- readRDS(file.path(AD1, "checkpoints", paste0("CKPT__", PLAN18$target_id[1L], ".rds")))$value; expect_equal(q$lower_reconstructed[1L], unname(stats::quantile(ck$estimates[is.finite(ck$estimates)], 0.025, type = 8)), tolerance = 1e-15)
  dt <- tempfile("syntamper_"); make_audit_run(dt, summary_mutate = function(i, s) { if (i == 4L) s$ci_lower <- s$ci_lower + 1e-6; s }); rt <- audit(dt); expect_identical(rt$statuses$QUANTILE_RECONSTRUCTION_STATUS, "FAIL"); expect_false(rt$tables$reconstruction$agree_checkpoint[4L]); expect_equal(sum(!rt$tables$reconstruction$agree_checkpoint), 1L)
  expect_identical(rt$status, "CHECKS_FAILED")
  dr <- tempfile("synrun_"); make_audit_run(dr, run_summary_mutate = function(i, s) { if (i == 5L) s$estimate_median <- s$estimate_median * 1.001; s }); rr <- audit(dr); expect_identical(rr$statuses$QUANTILE_RECONSTRUCTION_STATUS, "FAIL"); expect_false(rr$tables$reconstruction$agree_run_summary[5L])
  tiny <- siw_quantile8(1:10, 0.5); expect_equal(tiny, 5.5); expect_equal(siw_quantile8(c(5, 1, 3, 2, 4), c(0, 1)), c(1, 5))
})

test_that("accounting failures are detected: the run summary disagreeing with the saved estimates", {
  da <- tempfile("synacc_"); make_audit_run(da, run_summary_mutate = function(i, s) { if (i == 6L) { s$replicates_successful <- s$replicates_successful - 1L; s$replicates_failed <- s$replicates_failed + 1L }; s }); r <- audit(da)
  expect_identical(r$statuses$ACCOUNTING_STATUS, "FAIL"); expect_equal(sum(!r$tables$checks$accounting_ok), 1L); expect_false(r$tables$checks$accounting_ok[6L]); expect_identical(r$statuses$EXECUTION_STATUS, "PASS")
})

test_that("malformed checkpoints are rejected one by one with a reason; the audit completes and reports CHECKPOINT_INTEGRITY FAIL", {
  ids <- PLAN18$target_id; p <- function(d, i) file.path(d, "checkpoints", paste0("CKPT__", ids[i], ".rds")); sig <- function(i) siw_string_hash(paste(c("INPUTHASH", "CONFIGHASH", "PROTOCOLHASH", "SOURCEFP", ids[i], PLAN18$seed[i], RB), collapse = "|"))
  good <- function(i) readRDS(p(AD1, i)); chk <- function(path, i) siw_bootstrap_audit_read_checkpoint(path, ids[i], sig(i), RB); tf <- function(obj) { f <- tempfile(fileext = ".rds"); saveRDS(obj, f); f }
  expect_true(chk(p(AD1, 1L), 1L)$ok); expect_identical(chk(tempfile(), 1L)$issues, "missing"); bf <- tempfile(fileext = ".rds"); writeBin(as.raw(1:50), bf); expect_identical(chk(bf, 1L)$issues, "unreadable")
  expect_false(chk(tf(list(1, 2)), 1L)$ok); expect_false(chk(tf(data.frame(a = 1)), 1L)$ok)
  g <- good(1L); g2 <- g; g2$signature <- "X"; r <- chk(tf(g2), 1L); expect_false(r$ok); expect_false(r$signature_ok); expect_match(r$issues, "signature")
  g2 <- g; g2$target_id <- ids[2L]; expect_match(chk(tf(g2), 1L)$issues, "target id")
  g2 <- g; g2$value$estimates <- g2$value$estimates[-1L]; expect_match(chk(tf(g2), 1L)$issues, "length")
  g2 <- g; g2$value$participants <- data.frame(participant_id = 1:3, age = c(30, 40, 50)); r <- chk(tf(g2), 1L); expect_false(r$ok); expect_match(r$issues, "undeclared fields"); expect_match(r$issues, "data frame other than the summary"); expect_null(r$value)
  g2 <- g; g2$value$summary <- NULL; expect_match(chk(tf(g2), 1L)$issues, "missing fields"); g2 <- g; g2$value$summary <- g$value$summary[, 1:3]; expect_match(chk(tf(g2), 1L)$issues, "summary")
  g2 <- g; g2$value$reason[1L] <- if (nzchar(g2$value$reason[1L])) "" else "X"; expect_match(chk(tf(g2), 1L)$issues, "inconsistent")
  g2 <- g; g2$value$glm_not_converged <- as.numeric(g2$value$glm_not_converged); expect_match(chk(tf(g2), 1L)$issues, "glm_not_converged"); g2 <- g; g2$value$estimates <- as.character(g2$value$estimates); expect_match(chk(tf(g2), 1L)$issues, "numeric")
  db <- tempfile("synbad_"); make_audit_run(db); unlink(p(db, 7L)); writeBin(as.raw(1:20), p(db, 8L)); g3 <- good(1L); g3$value$estimates <- g3$value$estimates[1:50]; saveRDS(g3, p(db, 9L)); file.copy(p(db, 12L), file.path(db, "checkpoints", "CKPT__UNEXPECTED.rds")); rb <- audit(db)
  expect_identical(rb$statuses$CHECKPOINT_INTEGRITY_STATUS, "FAIL"); expect_identical(rb$statuses$EXECUTION_STATUS, "PASS"); ck <- rb$tables$checks; expect_false(ck$present[7L]); expect_false(ck$schema_and_content_valid[8L]); expect_false(ck$schema_and_content_valid[9L]); expect_equal(sum(!ck$schema_and_content_valid), 3L)
  expect_equal(nrow(rb$tables$target), 177L); expect_false(unname(rb$statuses$ACCOUNTING_STATUS == "PASS")); expect_false(rb$tables$hashes$listed_in_run_summary[rb$tables$hashes$checkpoint_file == "CKPT__UNEXPECTED.rds"])
})

test_that("read-only: every checkpoint and run file is byte-identical after the audit; the output folder must be new and outside the audited run", {
  expect_true(all(AR1$tables$hashes$unchanged)); expect_equal(nrow(AR1$tables$hashes), 180L); expect_identical(AR1$statuses$CHECKPOINTS_UNCHANGED_STATUS, "PASS")
  h0 <- unlist(lapply(list.files(AD1, recursive = TRUE, full.names = TRUE), function(f) siw_file_hash(f)$value)); invisible(audit(AD1, tempfile("again_"))); h1 <- unlist(lapply(list.files(AD1, recursive = TRUE, full.names = TRUE), function(f) siw_file_hash(f)$value)); expect_identical(h0, h1)
  expect_error(suppressMessages(siw_run_bootstrap_audit(AD1, file.path(AD1, "inside"))), "SIW_AUDIT_OUTPUT_INSIDE_AUDITED_RUN"); ne <- tempfile("nonempty_"); dir.create(ne); writeLines("x", file.path(ne, "a.txt")); expect_error(suppressMessages(siw_run_bootstrap_audit(AD1, ne)), "SIW_AUDIT_OUTPUT_FOLDER_NOT_EMPTY")
  expect_equal(length(list.files(file.path(AD1, "checkpoints"))), 180L); expect_false(any(grepl("[.]part$", list.files(file.path(AD1, "checkpoints")))))
})

test_that("provenance: checkpoint hashes recorded before and after, signature matches the run metadata, original-run manifest verified; tampering is detected", {
  h <- AR1$tables$hashes; expect_true(all(grepl("^[0-9A-F]{64}$", h$sha256_before))); expect_identical(h$sha256_before, h$sha256_after); expect_true(all(AR1$tables$checks$signature_matches)); expect_identical(AR1$statuses$ORIGINAL_RUN_PROVENANCE_STATUS, "PASS")
  dm <- tempfile("synman_"); make_audit_run(dm); writeLines("tampered", file.path(dm, "bootstrap_validity.csv")); expect_error(audit(dm))                                                                  # an unreadable validity table stops the audit rather than guessing
  d2 <- tempfile("synman2_"); make_audit_run(d2); m <- utils::read.csv(file.path(d2, "RUN_MANIFEST.csv")); m$hash[1L] <- paste(rep("0", 64), collapse = ""); utils::write.csv(m, file.path(d2, "RUN_MANIFEST.csv"), row.names = FALSE); r2 <- audit(d2); expect_identical(r2$statuses$ORIGINAL_RUN_PROVENANCE_STATUS, "FAIL")
  d3 <- tempfile("synmeta_"); make_audit_run(d3); jj <- jsonlite::fromJSON(file.path(d3, "batch_c_metadata.json"), simplifyVector = FALSE); jj$input_hash <- "OTHER"; writeLines(jsonlite::toJSON(jj, auto_unbox = TRUE), file.path(d3, "batch_c_metadata.json")); r3 <- audit(d3)
  expect_identical(r3$statuses$CHECKPOINT_INTEGRITY_STATUS, "FAIL"); expect_false(any(r3$tables$checks$signature_matches))                                                                           # metadata that does not reproduce the signature
})

test_that("the audit code contains no fitting, resampling or data-import calls and never touches the SAV or participant CSV", {
  if (siw_skip_source_only("test-18-bootstrap-audit: the audit code contains no fitting, resampling or data-import calls an")) return(invisible(NULL))
  src <- readLines(file.path(ROOT, "R", "bootstrap_audit.R"), warn = FALSE); code <- src[!grepl("^\\s*#", src)]; code <- sub("#.*$", "", code); code <- paste(code, collapse = "\n")
  for (bad in c("lm\\(", "glm\\(", "lm\\.fit", "glm\\.fit", "polr", "siw_bootstrap_target", "siw_bootstrap_indices", "siw_bootstrap_groups", "siw_bootstrap_design_spec", "\\.siw_boot_fit", "sample\\.int", "sample\\(", "set\\.seed", "read_sav", "readSav", "read\\.spss", "siw_read_", "siw_load_data", "siw_prepare")) expect_false(grepl(bad, code))
  expect_false(grepl("canonical|\\.sav", code, ignore.case = TRUE))
})

test_that("diagnostic outputs label coefficient-based flags as extremeness diagnostics, not separation", {
  s <- paste(readLines(file.path(AR1$out_dir, "RUN_SUMMARY.md"), warn = FALSE), collapse = " "); expect_match(s, "NOT confirmed separation"); expect_match(s, "never replace the original intervals"); d <- utils::read.csv(file.path(AR1$out_dir, "audit_definitions.csv")); expect_equal(nrow(d), 5L)
  expect_false(any(grepl("separation detected|confirmed separation", names(AR1$tables$target), ignore.case = TRUE)))
})

# ---- Phase 3C.4: regression tests for the empty IQR / quantile-range / IQR-ratio columns (quantile lookup by name used the wrong, common-width names) ----
test_that("IQR, quantile ranges and IQR ratio are populated and equal independently known values (estimates 1..100: type-7 quantiles 1 + 99 p)", {
  dk <- tempfile("synknown_"); make_audit_run(dk, mutate = function(i, id, est) if (i == 1L) as.numeric(1:RB) else est); rk <- audit(dk); t1 <- rk$tables$target[1L, ]
  expect_equal(t1$q0.250, 25.75); expect_equal(t1$q0.500, 50.5); expect_equal(t1$q0.750, 75.25); expect_equal(t1$q0.025, 3.475); expect_equal(t1$q0.975, 97.525)                         # exported quantile columns keep their names
  expect_equal(t1$iqr_type7, 49.5); expect_equal(t1$range_q975_minus_median, 47.025); expect_equal(t1$range_median_minus_q025, 47.025); expect_equal(t1$range_max_minus_q975, 100 - 97.525); expect_equal(t1$range_q025_minus_min, 3.475 - 1)
  expect_equal(t1$ratio_iqr_scaled_to_se, (49.5 / 1.349) / SYN_SE); expect_equal(t1$mad_raw, 25); expect_equal(t1$mad_scaled, 1.4826 * 25); expect_equal(t1$ratio_mad_scaled_to_se, 1.4826 * 25 / SYN_SE); expect_equal(t1$ratio_q_width_to_wald_width, (97.525 - 3.475) / (2 * 1.96 * SYN_SE))
  expect_equal(t1$median, 50.5); expect_equal(t1$sd, stats::sd(1:RB)); expect_identical(rk$statuses$OUTPUT_COMPLETENESS_STATUS, "PASS")
})

test_that("for every target of a synthetic run the four formerly empty columns equal values recomputed from the exported quantile columns, and none is empty", {
  tg <- AR1$tables$target; for (nm in c("iqr_type7", "range_q975_minus_median", "range_median_minus_q025", "ratio_iqr_scaled_to_se")) expect_false(anyNA(tg[[nm]]))
  expect_equal(tg$iqr_type7, tg$q0.750 - tg$q0.250); expect_equal(tg$range_q975_minus_median, tg$q0.975 - tg$q0.500); expect_equal(tg$range_median_minus_q025, tg$q0.500 - tg$q0.025); expect_equal(tg$ratio_iqr_scaled_to_se, tg$iqr_type7 / 1.349 / tg$reference_se)
  csv <- utils::read.csv(file.path(AR1$out_dir, "audit_target_distribution.csv")); expect_false(anyNA(csv$iqr_type7)); expect_false(anyNA(csv$ratio_iqr_scaled_to_se)); expect_true(all(c("q0.250", "q0.500", "q0.750") %in% names(csv)))
  fc <- AR1$tables$completeness; expect_equal(sum(fc$n_unexpected_missing), 0); expect_true("audit_field_completeness.csv" %in% list.files(AR1$out_dir)); expect_true(all(c("iqr_type7", "range_q975_minus_median", "range_median_minus_q025", "ratio_iqr_scaled_to_se") %in% fc$column))
})

test_that("unexpected empty fields FAIL verification; legitimately undefined quantities are identified explicitly with their rule", {
  tg <- AR1$tables$target; qt <- AR1$tables$reconstruction; bad <- tg; bad$iqr_type7 <- NA_real_; fc <- siw_bootstrap_audit_field_completeness(bad, qt)
  r <- fc[fc$column == "iqr_type7", ]; expect_equal(r$n_unexpected_missing, 180); expect_equal(r$n_expected_missing, 0); expect_true(sum(fc$n_unexpected_missing) == 180)
  bad2 <- tg; bad2$ratio_iqr_scaled_to_se[5L] <- NA_real_; expect_equal(sum(siw_bootstrap_audit_field_completeness(bad2, qt)$n_unexpected_missing), 1)
  du <- tempfile("synundef_"); make_audit_run(du, mutate = function(i, id, est) { if (i == 1L) est[] <- NA_real_ else if (i == 2L) { est[-1L] <- NA_real_ } else if (i == 3L) { est[-(1:10)] <- NA_real_ }; est }); ru <- audit(du)
  expect_identical(ru$statuses$OUTPUT_COMPLETENESS_STATUS, "PASS"); expect_identical(ru$statuses$EXECUTION_STATUS, "PASS"); t1 <- ru$tables$target
  expect_equal(t1$n_finite[1:3], c(0, 1, 10)); expect_true(is.na(t1$median[1L]) && is.na(t1$iqr_type7[1L]) && is.na(t1$sd[2L])); expect_false(is.na(t1$iqr_type7[2L])); expect_false(is.na(t1$iqr_type7[3L])); expect_false(is.na(t1$sd[3L]))
  fcu <- ru$tables$completeness; expect_true(all(nzchar(fcu$rule))); iq <- fcu[fcu$column == "iqr_type7", ]; expect_equal(iq$n_missing, 1); expect_equal(iq$n_expected_missing, 1); expect_equal(iq$n_unexpected_missing, 0)
  lw <- fcu[fcu$column == "lower_reconstructed", ]; expect_equal(lw$n_missing, 3); expect_equal(lw$n_expected_missing, 3); expect_equal(sum(fcu$n_unexpected_missing), 0)                       # fewer than 20 finite estimates: no percentile limits, by the original rule
  expect_identical(ru$statuses$QUANTILE_RECONSTRUCTION_STATUS, "PASS"); expect_identical(ru$statuses$ACCOUNTING_STATUS, "PASS")
})

test_that("the quantile lookup is by declared probability: an undeclared probability stops, previously correct quantiles are unchanged", {
  if (siw_skip_source_only("test-18-bootstrap-audit: the quantile lookup is by declared probability: an undeclared probabil")) return(invisible(NULL))
  tg <- AR1$tables$target; ck <- readRDS(file.path(AD1, "checkpoints", paste0("CKPT__", PLAN18$target_id[7L], ".rds")))$value$estimates
  expect_equal(unname(unlist(tg[7L, c("q0.025", "q0.250", "q0.500", "q0.750", "q0.975")])), unname(stats::quantile(ck, c(0.025, 0.25, 0.5, 0.75, 0.975), type = 7)), tolerance = 1e-12)
  expect_equal(tg$median, tg$q0.500); expect_equal(tg$range_max_minus_q975, tg$maximum - tg$q0.975); expect_equal(tg$range_q025_minus_min, tg$q0.025 - tg$minimum)
  src <- paste(readLines(file.path(ROOT, "R", "bootstrap_audit.R"), warn = FALSE), collapse = "\n"); expect_false(grepl("q7[\"q0", src, fixed = TRUE)); expect_match(src, "SIW_AUDIT_UNDECLARED_QUANTILE_PROBABILITY")
})
