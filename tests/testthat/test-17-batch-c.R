# Batch C: primary cumulative bootstrap (participant and assessment-date cluster), verification, validator, resumption and the package-source fingerprint. Synthetic data only.
`%nin%` <- function(x, y) !(x %in% y)                                                                           # helpers for the base-R test shim (testthat provides the first three)
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
if (!exists("expect_gte", mode = "function")) expect_gte <- function(a, b) expect_true(a >= b)
if (!exists("expect_setequal", mode = "function")) expect_setequal <- function(a, b) expect_true(setequal(a, b))
if (!exists("expect_length", mode = "function")) expect_length <- function(x, n) expect_true(length(x) == n)
PROT <- siw_bootstrap_protocol()
PLAN17 <- siw_bootstrap_plan(CFG)
GR <- siw_bootstrap_groups(DAT)
fam <- stats::binomial()
spec_of <- function(dat, i, plan = PLAN17) { p <- plan[i, ]; siw_bootstrap_design_spec(dat, CFG, p$outcome, p$outcome_family, p$exposure) }
legacy_fit <- function(dat, idx, p) siw_bootstrap_legacy_style_fit(dat, idx, p$outcome, p$outcome_family, p$exposure, CFG$tiers$M2_L)

test_that("protocol and plan: 90 targets x 2 schemes, original target indices and seeds, 2,000 resamples, nothing from the DLM branch", {
  expect_identical(PROT$scope$combinations, 180L); expect_identical(PROT$scope$replicates_requested, 2000L); expect_identical(PROT$original_rules_preserved$rng, "Mersenne-Twister, Inversion, Rejection (R 4.4.3 defaults; set.seed(seed) at the start of every target and scheme)")
  expect_equal(nrow(PLAN17), 180L); expect_false(anyDuplicated(PLAN17$target_id) > 0); expect_equal(nrow(unique(PLAN17[, c("outcome", "metric", "window_or_lag")])), 90L); expect_equal(sort(unique(PLAN17$window_or_lag)), sort(CFG$windows))
  expect_true(all(PLAN17$replicates_requested == 2000L)); expect_equal(sort(unique(PLAN17$metric)), sort(c("UTCI", "IMS_relative_humidity", "IMS_rainy_day_count_ge_1_mm"))); expect_false(59L %in% PLAN17$window_or_lag)
  expect_identical(PLAN17$resampling_type, rep(c("participant", "assessment_date"), 90L))
  idx <- unique(PLAN17$target_index); expect_identical(idx[1:12], c(1:10, 12:13)); expect_true(all(c(11L, 22L, 33L, 44L) %nin% idx)); expect_equal(max(idx), 98L)          # one index skipped after every outcome-metric block (the legacy DLM target)
  for (i in seq_len(nrow(PLAN17))) { p <- PLAN17[i, ]; e <- siw_bootstrap_expected_seed(CFG, match(p$outcome, CFG$outcomes$outcome), match(p$metric, CFG$metrics$metric[CFG$metrics$representative]), match(p$window_or_lag, CFG$windows), p$resampling_type)
    expect_equal(c(p$target_index, p$seed), unname(c(e[["index"]], e[["seed"]]))) }
  expect_identical(PLAN17$seed[1:2], c(20260829L, 20260830L)); expect_identical(siw_bootstrap_plan(CFG, 100L)$seed, PLAN17$seed)                                      # seeds do not depend on the replicate count
})

test_that("date groups come from assessment_day in order of first appearance and keep every participant", {
  expect_equal(length(GR$groups), length(unique(DAT$assessment_day))); expect_identical(GR$groups, unique(as.character(DAT$assessment_day))); expect_equal(sum(lengths(GR$group_rows)), nrow(DAT))
  for (g in GR$groups) expect_true(all(as.character(DAT$assessment_day[GR$group_rows[[g]]]) == g)); expect_true(all(diff(GR$group_rows[[GR$groups[1L]]]) > 0))
})

test_that("participant sampling reproduces the original draw sequence and is deterministic; the caller's RNG state is restored", {
  seed <- 20260829L; ix <- siw_bootstrap_indices(GR, "participant", 25L, seed); old <- RNGkind()
  set.seed(seed); manual <- lapply(1:25, function(b) sample.int(nrow(DAT), nrow(DAT), replace = TRUE)); expect_identical(ix, manual)
  expect_identical(siw_bootstrap_indices(GR, "participant", 25L, seed), ix); expect_false(identical(siw_bootstrap_indices(GR, "participant", 25L, seed + 1L), ix))
  set.seed(5); a <- stats::runif(1); set.seed(5); invisible(siw_bootstrap_indices(GR, "participant", 3L, seed)); expect_identical(stats::runif(1), a); expect_identical(RNGkind(), old)
  chk <- siw_bootstrap_check_sampling(GR, "participant", 200L, seed); expect_equal(chk$violations, 0L); expect_true(chk$uniformity_max_abs_z < 5)
})

test_that("assessment-date cluster sampling retains every participant of each sampled occurrence, repeated dates repeat them, in sampled order", {
  seed <- 20260830L; ix <- siw_bootstrap_indices(GR, "assessment_date", 40L, seed)
  set.seed(seed); for (b in 1:40) {                                                                                                  # independent manual construction in the original form
    sampled <- sample(GR$groups, length(GR$groups), replace = TRUE); manual <- unlist(lapply(sampled, function(g) which(as.character(DAT$assessment_day) == g)), use.names = FALSE); expect_identical(ix[[b]], manual) }
  v <- ix[[1L]]; days <- as.character(DAT$assessment_day[v]); times <- table(factor(days, levels = GR$groups)); sizes <- lengths(GR$group_rows[GR$groups])
  expect_true(all(times %% sizes == 0L)); expect_equal(sum(times / sizes), length(GR$groups))                                         # every occurrence of a date contributes ALL its participants
  chk <- siw_bootstrap_check_sampling(GR, "assessment_date", 300L, seed); expect_equal(chk$violations, 0L); expect_true(chk$uniformity_max_abs_z < 5)
  bad <- ix; bad[[2L]] <- bad[[2L]][-1L]; expect_equal(siw_bootstrap_check_sampling(GR, "assessment_date", 40L, seed, indices = bad)$violations, 1L)               # a dropped participant is detected
  bad2 <- ix; bad2[[3L]] <- rev(bad2[[3L]]); expect_gte(siw_bootstrap_check_sampling(GR, "assessment_date", 40L, seed, indices = bad2)$violations, 1L)
  expect_identical(siw_bootstrap_indices(GR, "assessment_date", 40L, seed), ix)
})

test_that("engine equals the original lm()/glm() fits on identical resamples, bitwise, for continuous and binary outcomes and both schemes", {
  for (i in c(1L, 2L, 27L, 60L, 61L, 62L, 121L, 122L, 179L, 180L)) {
    p <- PLAN17[i, ]; sp <- spec_of(DAT, i); ix <- siw_bootstrap_indices(GR, p$resampling_type, 40L, p$seed)
    mine <- vapply(ix, function(v) .siw_boot_fit(v, sp, fam)$est, 0); ref <- vapply(ix, function(v) legacy_fit(DAT, v, p), 0); expect_identical(mine, ref)
  }
})

test_that("failure handling: stress data reproduce the original failure classification; failed samples are counted, classified, never redrawn", {
  set.seed(77); base <- DAT; base$Int_dummy <- as.integer(base$Int_dummy); R <- 150L
  stress <- list(heavy_missing = { d <- base[sample(nrow(base), 40), ]; d$Age[sample(40, 24)] <- NA; d },
    rare_locality = { d <- base; lv <- levels(d$Locality_3group); d$Locality_3group[] <- lv[1L]; d$Locality_3group[1:2] <- lv[2L]; d$Locality_3group[3] <- lv[3L]; d },
    sparse_events = { d <- base; d$Int_dummy <- 0L; d$Int_dummy[sample(nrow(d), 3)] <- 1L; d }, constant_exposure = { d <- base; d$UTCI_AVG_7D[-(1:6)] <- 5; d })
  seen <- character(); nonconv <- 0L; absent <- 0L
  for (nm in names(stress)) for (oc in c("UCLA_Lonliness_SCORE", "Int_dummy")) for (sch in c("participant", "assessment_date")) {
    d <- stress[[nm]]; gr <- siw_bootstrap_groups(d); famy <- CFG$outcomes$family[CFG$outcomes$outcome == oc]; sp <- siw_bootstrap_design_spec(d, CFG, oc, famy, "UTCI_AVG_7D"); seed <- 424242L
    v <- siw_bootstrap_target(gr, sp, sch, R, seed); ix <- siw_bootstrap_indices(gr, sch, R, seed); p <- data.frame(outcome = oc, outcome_family = famy, exposure = "UTCI_AVG_7D")
    ref <- vapply(ix, function(i) legacy_fit(d, i, p), 0); expect_identical(v$estimates, ref)                                              # same estimates and the same failures as the original, replicate by replicate
    expect_equal(length(v$estimates), R); s <- siw_bootstrap_summarise(v$estimates, R); expect_equal(s$replicates_attempted, R); expect_equal(s$replicates_successful + s$replicates_failed, R)       # nothing redrawn
    expect_equal(sum(nzchar(v$reason)), s$replicates_failed); expect_true(all(is.finite(v$estimates[!nzchar(v$reason)]))); expect_true(all(is.na(v$estimates[nzchar(v$reason)])))
    seen <- union(seen, unique(sub(":.*", "", v$reason[nzchar(v$reason)]))); nonconv <- nonconv + sum(v$glm_not_converged); absent <- absent + sum(v$level_absent)
  }
  expect_true(all(c("TOO_FEW_COMPLETE_CASES", "FIT_ERROR") %in% seen)); expect_true(nonconv > 0L); expect_true(absent > 0L)                  # failure classes, glm non-convergence (recorded, kept) and absent locality levels all occurred
  d <- stress$constant_exposure; sp <- siw_bootstrap_design_spec(d, CFG, "UCLA_Lonliness_SCORE", "gaussian", "UTCI_AVG_7D"); v <- siw_bootstrap_target(siw_bootstrap_groups(d), sp, "assessment_date", 300L, 5L); expect_true("EXPOSURE_ALIASED" %in% v$reason)
  # a single locality level after droplevels is a failed refit, as in the original (contrasts error)
  fx <- .siw_boot_fit(which(as.integer(DAT$Locality_3group) == 1L)[1:12], spec_of(DAT, 1L), fam); expect_match(fx$reason, "contrasts can be applied only to factors with 2 or more levels"); expect_true(is.na(fx$est))
})

test_that("percentile calculation: independent type-8 quantile equals stats::quantile; the interval needs 20 successes; sign fractions and median as in the original", {
  set.seed(3); for (n in c(20L, 21L, 57L, 500L, 2000L)) { x <- stats::rnorm(n); expect_equal(siw_quantile8(x, c(0.025, 0.975)), stats::quantile(x, c(0.025, 0.975), type = 8, names = FALSE), tolerance = 1e-12) }
  x <- c(rep(1, 30), 2); expect_equal(siw_quantile8(x, c(0.025, 0.975)), stats::quantile(x, c(0.025, 0.975), type = 8, names = FALSE), tolerance = 1e-12)
  est <- c(stats::rnorm(1980), rep(NA_real_, 20)); s <- siw_bootstrap_summarise(est, 2000L); expect_equal(c(s$replicates_requested, s$replicates_attempted, s$replicates_successful, s$replicates_failed), c(2000, 2000, 1980, 20))
  ok <- est[is.finite(est)]; expect_equal(s$estimate_median, stats::median(ok)); expect_equal(c(s$ci_lower, s$ci_upper), stats::quantile(ok, c(0.025, 0.975), type = 8, names = FALSE)); expect_equal(s$positive_fraction, mean(ok > 0)); expect_equal(s$negative_fraction, mean(ok < 0))
  s19 <- siw_bootstrap_summarise(c(stats::rnorm(19), rep(NA_real_, 5)), 24L); expect_true(is.na(s19$ci_lower) && is.na(s19$ci_upper)); expect_false(s19$interval_excludes_null); expect_false(is.na(s19$estimate_median))
  s20 <- siw_bootstrap_summarise(stats::rnorm(20), 20L); expect_false(is.na(s20$ci_lower)); sa <- siw_bootstrap_summarise(rep(NA_real_, 50), 50L); expect_true(is.na(sa$estimate_median) && sa$replicates_successful == 0L)
  expect_equal(siw_bootstrap_summarise(c(rep(1, 25), Inf), 26L)$replicates_failed, 1L)                                                                   # a non-finite estimate is a failure
  expect_true(siw_bootstrap_summarise(c(rep(1, 40) + 0:39 / 100), 40L)$interval_excludes_null)
})

test_that("deterministic repetition and target-level resumption (signature-checked checkpoints)", {
  d <- file.path(tempdir(), "bc_ckpt"); unlink(d, recursive = TRUE); sig <- c("input", "cfg", "prot", "src")
  r1 <- siw_bootstrap_run_targets(DAT, CFG, 30L, ckpt_dir = d, signature_parts = sig, targets = 1:4, log = FALSE); r2 <- siw_bootstrap_run_targets(DAT, CFG, 30L, ckpt_dir = d, signature_parts = sig, targets = 1:4, resume = TRUE, log = FALSE)
  expect_true(all(r1$status_by_target == "COMPUTED")); expect_true(all(r2$status_by_target == "RESTORED")); for (id in names(r1$results)) { expect_identical(r1$results[[id]]$estimates, r2$results[[id]]$estimates); expect_identical(r1$results[[id]]$reason, r2$results[[id]]$reason) }
  r3 <- siw_bootstrap_run_targets(DAT, CFG, 30L, ckpt_dir = tempfile(), signature_parts = sig, targets = 1:4, log = FALSE); for (id in names(r1$results)) expect_identical(r1$results[[id]]$estimates, r3$results[[id]]$estimates)    # fresh recomputation identical
  r4 <- siw_bootstrap_run_targets(DAT, CFG, 30L, ckpt_dir = d, signature_parts = c("input", "cfg", "prot", "OTHER_SOURCE"), targets = 1:4, resume = TRUE, log = FALSE); expect_true(all(r4$status_by_target == "COMPUTED"))   # changed source: stale checkpoints refused
  r5 <- siw_bootstrap_run_targets(DAT, CFG, 31L, ckpt_dir = d, signature_parts = sig, targets = 1:4, resume = TRUE, log = FALSE); expect_true(all(r5$status_by_target == "COMPUTED"))                                           # changed replicate count: refused
  f <- list.files(d, pattern = "R31", full.names = TRUE)[1L]; writeBin(as.raw(1:10), f); r6 <- siw_bootstrap_run_targets(DAT, CFG, 31L, ckpt_dir = d, signature_parts = sig, targets = 1:4, resume = TRUE, log = FALSE); expect_true(any(r6$status_by_target == "COMPUTED"))    # a corrupt checkpoint is recomputed
  expect_false(any(grepl("[.]part$", list.files(d))))                                                                                                                                                                            # atomic writes leave no partial files
  expect_match(PROT$implementation$parallelism, "none")
})

test_that("verification passes on correct results and detects tampering in every check family", {
  run <- siw_bootstrap_run_targets(DAT, CFG, 40L, targets = 1:4, log = FALSE); pt <- {g <- siw_cumulative_grid(DAT, CFG, tiers = "M2_L")$results; data.frame(outcome = g$outcome, metric = g$metric, window_or_lag = g$window_or_lag, coefficient = g$coefficient, robust_hc3_se = g$robust_hc3_se, model_based_se = g$model_based_se)}
  ver <- siw_bootstrap_verify(DAT, CFG, run, pt, PROT, replicates = 40L); expect_true(all(ver$status == "PASS")); expect_equal(sum(ver$determinism_recomputed), 2L); expect_true(all(ver$legacy_style_max_relative_difference == 0)); expect_true(all(ver$full_sample_estimate_relative_difference < 1e-9))
  tamper <- function(fn) { r <- run; id <- names(r$results)[3]; r$results[[id]] <- fn(r$results[[id]]); v <- siw_bootstrap_verify(DAT, CFG, r, pt, PROT, replicates = 40L); v[v$result_id == id, ] }
  v <- tamper(function(x) { x$estimates[1] <- x$estimates[1] + 1e-6; x$summary <- siw_bootstrap_summarise(x$estimates, 40L); x }); expect_identical(v$status, "FAIL"); expect_match(v$failed_checks, "legacy_style_refit")
  v <- tamper(function(x) { x$summary$ci_upper <- x$summary$ci_upper + 1e-6; x }); expect_identical(v$status, "FAIL"); expect_match(v$failed_checks, "percentile_median")
  v <- tamper(function(x) { x$summary$estimate_median <- x$summary$estimate_median * (1 + 1e-6); x }); expect_match(v$failed_checks, "percentile_median")
  v <- tamper(function(x) { x$summary$replicates_failed <- 3L; x }); expect_match(v$failed_checks, "conservation")
  rr <- run; rr$plan$seed[3] <- rr$plan$seed[3] + 1L; vv <- siw_bootstrap_verify(DAT, CFG, rr, pt, PROT, replicates = 40L); expect_match(vv$failed_checks[3], "seed_rule")
  m <- siw_bootstrap_math_summary(ver); expect_true(all(m$status == "PASS")); expect_true(nrow(m) == 7L)
  m2 <- siw_bootstrap_math_summary(rbind(ver[1, ], transform(ver[2, ], determinism_recomputed = FALSE))); expect_true(m2$status[m2$check == "deterministic_recomputation"] %in% c("PASS", "NOT_TESTED"))
  vb <- ver; vb$sampling_violations[1] <- 2L; expect_equal(siw_bootstrap_math_summary(vb)$status[1], "FAIL")
})

mk_ref17 <- function(summary, dir) { dir.create(dir, showWarnings = FALSE, recursive = TRUE); r <- summary[, c("outcome", "metric", "window_or_lag", "resampling_type", "replicates_requested", "replicates_successful", "estimate_median", "ci_lower", "ci_upper", "positive_fraction", "negative_fraction")]
  utils::write.csv(r, file.path(dir, "batchC_primary_cumulative_bootstrap_reference.csv"), row.names = FALSE, na = ""); invisible(r) }
SUM17 <- {
  rr <- siw_bootstrap_run_targets(DAT, CFG, 25L, targets = NULL, log = FALSE); tb <- .siw_bootstrap_tables(rr, data.frame()); tb$summary }

test_that("Batch C validator: exact match passes; absent, undeclared, duplicated, short, perturbed and incomplete references FAIL; the L59 DLM rows are declared out of scope", {
  d <- file.path(tempdir(), "bc_ref_ok"); r <- mk_ref17(SUM17, d); tabs <- list(bootstrap_summary = SUM17); v <- siw_validate_batch_c(tabs, CFG, d)
  expect_identical(v$summary$status[1], "PASS_EXACT"); expect_equal(v$summary$n_records_reference[1], 180L); expect_true(v$summary$required[1]); expect_identical(v$summary$status[2], "NOT_TESTED"); expect_false(v$summary$required[2]); expect_identical(siw_overall_validation_status(v$summary), "PASS")
  e <- file.path(tempdir(), "bc_ref_empty"); dir.create(e, showWarnings = FALSE); ve <- siw_validate_batch_c(tabs, CFG, e); expect_identical(ve$summary$status[1], "FAIL"); expect_identical(siw_overall_validation_status(ve$summary), "FAIL")
  u <- file.path(tempdir(), "bc_ref_undecl"); mk_ref17(SUM17, u); file.rename(file.path(u, "batchC_primary_cumulative_bootstrap_reference.csv"), file.path(u, "other.csv")); expect_identical(siw_validate_batch_c(tabs, CFG, u)$summary$status[1], "FAIL")
  w <- file.path(tempdir(), "bc_ref_x"); write <- function(x) utils::write.csv(x, file.path(w, "batchC_primary_cumulative_bootstrap_reference.csv"), row.names = FALSE, na = ""); dir.create(w, showWarnings = FALSE)
  write(r[-7, ]); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "FAIL"); expect_true(s$n_unmatched[1] >= 1L)
  write(rbind(r, r[1, ])); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "FAIL"); expect_true(s$n_duplicate[1] >= 1L)
  write(r[, setdiff(names(r), "ci_upper")]); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "FAIL"); expect_true(s$n_missing_required[1] >= 1L)
  x <- r; x$replicates_successful[5] <- x$replicates_successful[5] - 1L; write(x); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "FAIL"); expect_equal(s$n_different[1], 1L)                  # counts must agree exactly
  x <- r; x$ci_lower[9] <- x$ci_lower[9] + 1e-4; write(x); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "FAIL")
  x <- r; x$estimate_median[9] <- x$estimate_median[9] + 5e-8; write(x); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "PASS_WITHIN_TOLERANCE")                                         # the existing tolerance only
  x <- r; x$positive_fraction[3] <- NA; write(x); s <- siw_validate_batch_c(tabs, CFG, w)$summary; expect_identical(s$status[1], "FAIL")
  expect_identical(siw_validate_batch_c(list(), CFG, d)$summary$status[1], "FAIL")                                                                                                                         # a result table that was not produced
  pl <- siw_load_batch_c_plan(); expect_true(isTRUE(pl$references[[1]]$required)); expect_equal(as.integer(unlist(pl$references[[1]]$expected_records_by_file)), 180L); expect_identical(pl$references[[1]]$tolerance, "cumulative")
})

test_that("source manifest: explicit scope, relative paths, private data and generated outputs excluded, deterministic fingerprint, change detection", {
  root <- file.path(tempdir(), "srcroot"); unlink(root, recursive = TRUE); for (d in c("R", "config", "scripts", "inst/extdata", "runs/batchc_x", "private_validation", "output", "checkpoints", "reports", "tests", "man")) dir.create(file.path(root, d), recursive = TRUE)
  w <- function(f, txt = "x") writeLines(txt, file.path(root, f)); for (f in c("R/a.R", "R/b.R", "config/c.json", "scripts/s.R", "scripts/p.ps1", "inst/extdata/syn.csv", "run.R", "run_x.ps1", "DESCRIPTION", "NAMESPACE")) w(f, f)
  for (f in c("runs/batchc_x/out.csv", "private_validation/canonical.csv", "output/o.csv", "checkpoints/c.rds", "reports/r.md", "tests/t.R", "man/m.Rd", "study.sav", "R/.hidden", "scripts/log.log", "README.md", "notes.csv")) w(f, "secret")
  m <- siw_source_manifest(root); expect_equal(m$n_files, 10L); expect_identical(m$manifest$file, sort(m$manifest$file, method = "radix")); expect_false(any(grepl("^[A-Za-z]:|\\\\", m$manifest$file)))
  expect_setequal(m$manifest$file, c("DESCRIPTION", "NAMESPACE", "R/a.R", "R/b.R", "config/c.json", "inst/extdata/syn.csv", "run.R", "run_x.ps1", "scripts/p.ps1", "scripts/s.R"))
  expect_false(any(grepl("runs|private|output|checkpoints|reports|tests|man/|[.]sav|hidden|[.]log|README|notes", m$manifest$file))); expect_true(all(grepl("^[0-9A-F]{64}$", m$manifest$sha256))); expect_true(grepl("^[0-9A-F]{64}$", m$fingerprint)); expect_equal(m$manifest$sha256[m$manifest$file == "R/a.R"], siw_file_hash(file.path(root, "R/a.R"))$value)
  m2 <- siw_source_manifest(root); expect_identical(m$fingerprint, m2$fingerprint); expect_true(siw_source_manifest_diff(m, m2)$identical_fingerprint)
  w("runs/batchc_x/more.csv", "more"); w("notes2.md", "doc"); expect_identical(siw_source_manifest(root)$fingerprint, m$fingerprint)                                      # generated outputs and documentation do not alter the fingerprint
  w("R/a.R", "changed"); m3 <- siw_source_manifest(root); expect_false(identical(m3$fingerprint, m$fingerprint)); dd <- siw_source_manifest_diff(m, m3); expect_identical(dd$changed, "R/a.R"); expect_length(dd$added, 0L)
  w("R/new.R", "new"); dd2 <- siw_source_manifest_diff(m3, siw_source_manifest(root)); expect_identical(dd2$added, "R/new.R"); unlink(file.path(root, "R/b.R")); dd3 <- siw_source_manifest_diff(m3, siw_source_manifest(root)); expect_true("R/b.R" %in% dd3$removed)
  real <- siw_source_manifest(); expect_true(if (real$root_kind == "source_checkout") real$n_files > 20L else real$n_files >= 1L); expect_true(real$root_kind %in% c("source_checkout", "installed_package")); expect_false(any(grepl("private_validation|runs/|checkpoints|[.]sav", real$manifest$file))); expect_true(all(!grepl("\\\\", real$manifest$file)))
})

test_that("schemas have unique names and the Batch C tables round-trip through CSV", {
  sch <- siw_output_columns(); for (nm in names(siw_batch_c_output_columns())) { expect_true(nm %in% names(sch)); expect_false(anyDuplicated(sch[[nm]]) > 0) }
  d <- tempfile(); dir.create(d); siw_write_table(SUM17, "bootstrap_summary", d); b <- utils::read.csv(file.path(d, "bootstrap_summary.csv"), check.names = FALSE); expect_identical(names(b), siw_batch_c_output_columns()$bootstrap_summary); expect_equal(nrow(b), 180L)
  expect_equal(b$estimate_median, SUM17$estimate_median, tolerance = 1e-13); expect_identical(b$replicates_successful, SUM17$replicates_successful); expect_identical(b$interval_excludes_null, SUM17$interval_excludes_null)
})

test_that("end to end (development subset): separate statuses, source fingerprint, no participant identifiers, data and primary results untouched", {
  if (identical(Sys.getenv("SIW_SKIP_SLOW_TESTS"), "1")) return(invisible(NULL))
  tmp <- file.path(tempdir(), "bc_e2e"); unlink(tmp, recursive = TRUE); dir.create(tmp); ref <- file.path(tmp, "ref"); d0 <- DAT; core0 <- siw_cumulative_grid(DAT, CFG)$results
  r1 <- siw_run_batch_c(SYN_CSV, file.path(tmp, "run1"), ref, CFG, replicates = 12L, targets = 1:6); expect_identical(r1$statuses[["NUMERICAL_REPRODUCTION_STATUS"]], "FAIL")                    # reference absent: a required comparison cannot pass
  expect_identical(r1$statuses[["EXECUTION_STATUS"]], "PASS_DEVELOPMENT_SUBSET"); expect_identical(r1$statuses[["MATHEMATICAL_VERIFICATION_STATUS"]], "PASS"); expect_identical(r1$statuses[["SOURCE_INTEGRITY_STATUS"]], "PASS"); expect_true(r1$statuses[["SCIENTIFIC_VALIDITY_STATUS"]] %in% c("PASS", "REVIEW"))
  expect_equal(length(unique(names(r1$statuses))), length(r1$statuses)); expect_identical(r1$status, "DEVELOPMENT_SUBSET")
  mkref <- mk_ref17(r1$tables$summary, ref)                                                                                                                                                  # mechanics only: a six-row reference cannot satisfy the 180-record plan
  r2 <- siw_run_batch_c(SYN_CSV, file.path(tmp, "run2"), ref, CFG, replicates = 12L, targets = 1:6); expect_identical(r2$statuses[["NUMERICAL_REPRODUCTION_STATUS"]], "FAIL")                      # record-count mismatch fails
  out <- file.path(tmp, "run1"); for (f in c("bootstrap_summary.csv", "bootstrap_failures.csv", "bootstrap_diagnostics.csv", "bootstrap_validity.csv", "bootstrap_verification.csv", "bootstrap_math_verification.csv", "batch_c_validation_summary.csv", "bootstrap_point_estimates.csv",
              "source_manifest.csv", "SOURCE_FINGERPRINT.txt", "batch_c_metadata.json", "BATCH_C_STATUS.txt", "batch_c_timings.csv", "session_info.txt", "MANIFEST.csv", "run.log")) expect_true(file.exists(file.path(out, f)))
  meta <- jsonlite::fromJSON(file.path(out, "batch_c_metadata.json")); expect_match(meta$source_fingerprint_sha256, "^[0-9A-F]{64}$"); expect_false(meta$source_changed_during_run); expect_identical(meta$source_fingerprint_sha256, siw_source_manifest()$fingerprint); expect_identical(meta$protocol_hash, PROT$protocol_hash)
  expect_true(any(grepl("SOURCE_CHANGED_DURING_RUN=FALSE", readLines(file.path(out, "SOURCE_FINGERPRINT.txt"))))); expect_true(file.exists(file.path(out, "checkpoints")))
  all_txt <- paste(unlist(lapply(list.files(out, pattern = "[.](csv|md|txt|json)$", full.names = TRUE), function(p) readLines(p, warn = FALSE))), collapse = "\n"); expect_false(any(vapply(unique(as.character(SYN$participant_code)), function(cd) grepl(cd, all_txt, fixed = TRUE), logical(1))))
  expect_identical(DAT, d0); expect_identical(siw_cumulative_grid(DAT, CFG)$results, core0); expect_identical(siw_load_config()$config_hash, CFG$config_hash)
  # resumption through the run function: a second run in the same folder restores every target
  r3 <- siw_run_batch_c(SYN_CSV, file.path(tmp, "run1"), ref, CFG, replicates = 12L, targets = 1:6, resume = TRUE); expect_true(all(r3$run$status_by_target == "RESTORED")); expect_identical(r3$tables$summary, r1$tables$summary)
  # point estimates equal an accepted run when one is supplied (here: a core run table written from the same data)
  acc <- file.path(tmp, "accepted"); dir.create(acc); utils::write.csv(core0, file.path(acc, "cumulative_results.csv"), row.names = FALSE); r4 <- siw_run_batch_c(SYN_CSV, file.path(tmp, "run4"), ref, CFG, accepted_run_dir = acc, replicates = 12L, targets = 1:2)
  expect_true(any(grepl("POINT_ESTIMATES_EQUAL_ACCEPTED_RUN=TRUE", readLines(file.path(tmp, "run4", "BATCH_C_STATUS.txt")))))
})
