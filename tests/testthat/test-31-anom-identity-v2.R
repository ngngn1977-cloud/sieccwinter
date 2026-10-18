# ANOM identity, protocol v2 (post hoc, 12 Oct 2026). Synthetic only. The equal-n different-sample data use the mechanism of the declared diagnostic
# (validation/test_anom_sample_sd_diag_synthetic.R): a participant alone on its locality's last day (no feature for windows 1-4) and a participant first in its
# locality 30 days before the next one (no feature for windows 37-59) give two different complete-case samples of the same size 54.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
PH31 <- siw_ah_protocol(); W31 <- c(1L, 4L, 5L, 36L, 37L, 59L)
syn31 <- function(variant = c("two_sets", "one_set")) {
  variant <- match.arg(variant); raw <- siw_make_synthetic(n = 55L, seed = 21L, config = CFG); ac <- sort(unique(raw$Area))
  area <- rep(ac[3L], 55L); day <- integer(55L)
  area[1:10] <- ac[1L]; day[1:10] <- rep(c(10L, 20L, 30L, 40L, 41L), each = 2L); area[11:18] <- ac[2L]; day[11:18] <- rep(c(35L, 45L, 55L, 65L), each = 2L)
  area[19:53] <- ac[3L]; day[19:53] <- c(rep(seq(0L, 75L, by = 5L), each = 2L), rep(78L, 3L)); area[54] <- ac[1L]; day[54] <- 42L; area[55] <- ac[2L]; day[55] <- 5L
  if (variant == "one_set") { area[53] <- ac[2L]; day[53] <- 5L }
  raw$Area <- area; raw$assessment_day <- day; raw$UCLA_Lonliness_SCORE[54] <- 3; raw$UCLA_Lonliness_SCORE[55] <- 8; raw$Isolation_cont[54] <- 5; raw$Isolation_cont[55] <- 20
  dat <- siw_prepare_data(raw, CFG); sc <- siw_anomaly_scan(dat, CFG, PH31, windows = W31, diagnostics = FALSE)$results
  tr <- siw_anom_traced_sd(dat, CFG, PH31, sc[SIW_AH_V2_KEYS]); list(dat = dat, sc = sc, tr = tr)
}
v1_byn <- function(res) { i <- .siw_dl_identities(res); .siw_dl_status(i$status[i$check == SIW_AH_V1_IDENTITY_CHECK]) }
S2 <- syn31("two_sets"); S1 <- syn31("one_set")

test_that("equal n with different samples: the v1 by-n identity FAILS, the v2 per-row identity PASSES", {
  g <- S2$tr[S2$tr$n_mask == 54L & S2$tr$outcome == "UCLA_Lonliness_SCORE", ]; expect_true(length(unique(signif(g$direct_sd, 12))) >= 2L)          # two different samples of size 54
  expect_identical(v1_byn(S2$sc), "FAIL")
  v <- siw_anom_identity_v2(S2$sc, S2$tr); expect_true(all(v$checks$status == "PASS")); expect_true(all(v$rows$status == "PASS"))
  expect_equal(nrow(v$rows), sum(S2$sc$status == "COMPLETED" & S2$sc$outcome_family == "gaussian")); expect_true(max(v$rows$rel_diff) <= 1e-9); expect_true(all(v$rows$n == v$rows$n_mask))
})

test_that("identical samples: v1 and v2 both PASS", {
  expect_identical(v1_byn(S1$sc), "PASS"); v <- siw_anom_identity_v2(S1$sc, S1$tr); expect_true(all(v$checks$status == "PASS"))
})

test_that("wrong-SD negative: one row's standardised effect off by 1e-6 relative fails v2 (and only that row)", {
  sc <- S2$sc; i <- which(sc$status == "COMPLETED" & sc$outcome_family == "gaussian")[7L]; sc$standardised_effect[i] <- sc$standardised_effect[i] * (1 + 1e-6)
  v <- siw_anom_identity_v2(sc, S2$tr); expect_identical(v$checks$status[3L], "FAIL"); expect_equal(sum(v$rows$status == "FAIL"), 1L); expect_true(v$checks$max_rel_diff[3L] > 1e-9)
  expect_identical(v$checks$status[1:2], c("PASS", "PASS"))
})

test_that("missing-mask negative: a row without a traced mask fails v2, never passes silently", {
  tr <- S2$tr[-1L, ]; v <- siw_anom_identity_v2(S2$sc, tr)
  expect_identical(v$checks$status[1L], "FAIL"); expect_equal(v$checks$max_rel_diff[1L], 1); expect_equal(sum(!v$rows$mask_found), 1L); expect_identical(v$rows$status[!v$rows$mask_found], "FAIL")
})

test_that("mask-size negative: a recorded n that differs from the traced mask fails v2", {
  sc <- S2$sc; i <- which(sc$status == "COMPLETED" & sc$outcome_family == "gaussian")[3L]; sc$n[i] <- sc$n[i] + 1L
  v <- siw_anom_identity_v2(sc, S2$tr); expect_identical(v$checks$status[2L], "FAIL"); expect_equal(sum(v$rows$status == "FAIL"), 1L)
})

test_that("no gaussian row: v2 is NOT_TESTED, not PASS", {
  v <- siw_anom_identity_v2(S2$sc[S2$sc$outcome_family != "gaussian", ], S2$tr); expect_true(all(v$checks$status == "NOT_TESTED"))
})

test_that("the v1 by-n row is kept, relabelled as superseded, and does not count in v2", {
  i <- .siw_dl_identities(S2$sc); i$source <- "package"; v <- siw_anom_identity_v2(S2$sc, S2$tr)
  t <- siw_anom_identities_v2_table(i, list(package = v$checks)); sup <- grepl("^V1 identity \\(superseded, kept for the record\\): ", t$check)
  expect_true(any(sup)); expect_false(any(t$counts_in_v2[sup])); expect_true(any(t$status[sup] == "FAIL")); expect_equal(sum(t$protocol == "v2"), 3L)
  expect_equal(nrow(t), nrow(i) + 3L)
})

test_that("protocol v2 is declared, extends the unchanged v1 and records the tolerances and the near-zero scaling", {
  p2 <- siw_ah_protocol_v2(); if (SIW_PUBLIC_BUILD) expect_identical(p2$extends_check, "NOT_APPLICABLE_PUBLIC_BUILD")
  if (!siw_skip_public("test-31: v2 extends the declared v1 hash")) { expect_identical(toupper(p2$extends_protocol$sha256), toupper(PH31$protocol_hash)); expect_identical(p2$extends_check, "PASS") }
  expect_match(p2$status_of_this_document, "POST HOC"); expect_equal(p2$rule_v2$tolerance, 1e-9); expect_identical(p2$author_decision$date, "2026-10-12")
  expect_identical(p2$evidence$decision, "PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES")
  ns <- p2$near_zero_scaling_recorded$checks; expect_equal(vapply(ns, function(z) z$threshold, 0), c(1e-9, 1e-12)); expect_true(all(vapply(ns, function(z) z$relative_difference, "") == "|a - b| / max(1, |b|)"))
  if (!siw_skip_public("test-31: strict refusal of another v1")) expect_error(siw_ah_protocol_v2(v1_hash = strrep("0", 64L)), "EXTENDS_A_DIFFERENT_V1"); if (!SIW_PUBLIC_BUILD) expect_identical(siw_ah_protocol_v2(v1_hash = strrep("0", 64L), strict = FALSE)$extends_check, "NOT_APPLICABLE_SYNTHETIC_PROTOCOL_COPY")
  if (SIW_SOURCE_CHECKOUT) expect_private(toupper(siw_file_hash(file.path(ROOT, "config", "anomaly_history_validation_protocol.json"))$value), "anomaly_history_validation_protocol")
})

test_that("the v2 mask is the traced mask of validation/anom_sample_sd_diag.R (identical n and direct SD)", {
  if (siw_skip_source_only("test-31: equivalence with asd_masks needs validation/anom_sample_sd_diag.R")) return(invisible(NULL))
  e <- new.env(); sys.source(file.path(ROOT, "validation", "anom_sample_sd_diag.R"), envir = e)
  mk <- e$asd_masks(S2$dat, CFG, PH31, W31)$rows; tr <- S2$tr[S2$tr$reference_definition == "locality", ]
  k <- function(d) paste(d$outcome, d$metric, d$anomaly_method, d$burden_direction, d$window_or_lag, sep = "|"); j <- match(k(mk), k(tr))
  expect_false(anyNA(j)); expect_identical(as.integer(mk$n_mask), as.integer(tr$n_mask[j])); expect_identical(mk$direct_sd, tr$direct_sd[j]); expect_equal(nrow(mk), 240L)
})

test_that("the anomhist validator reports ANOM_VERIFICATION_STATUS_V2 alongside the v1 status and writes the v2 identity files", {
  if (siw_skip_source_only("test-31: validator wiring needs validation/make_anomaly_history_synthetic.R")) return(invisible(NULL))
  source(file.path(ROOT, "validation", "make_anomaly_history_synthetic.R"), local = TRUE)
  td <- file.path(tempdir(), paste0("ah31_", sample.int(1e6, 1))); b <- make_ah_synthetic(file.path(td, "exact"), "exact", ROOT)
  r <- siw_run_anomaly_history_validation(b$csv, b$ref, file.path(td, "out"), CFG, protocol_path = b$protocol)
  expect_true(all(c("ANOM_VERIFICATION_STATUS", "ANOM_VERIFICATION_STATUS_V2") %in% names(r$statuses))); expect_identical(unname(r$statuses[["ANOM_VERIFICATION_STATUS_V2"]]), "PASS")
  for (f in c("anom_identity_v2_rows.csv", "anom_scale_and_inference_identities_v2.csv", "anom_scale_and_inference_identities.csv")) expect_true(file.exists(file.path(r$out_dir, f)))
  st <- readLines(file.path(r$out_dir, "ANOMHIST_STATUS.txt")); expect_true(any(grepl("^ANOM_VERIFICATION_GOVERNED_BY=ANOM_VERIFICATION_STATUS_V2 ", st))); expect_true(any(grepl("^ANOM_IDENTITY_V1_BY_N=.*superseded, kept for the record", st)))
  expect_true(any(grepl("NOT_APPLICABLE_SYNTHETIC_PROTOCOL_COPY", st)))
  rows <- utils::read.csv(file.path(r$out_dir, "anom_identity_v2_rows.csv")); expect_true(all(rows$status == "PASS")); expect_true(all(c("package", "frozen") %in% rows$source))
  md <- jsonlite::fromJSON(file.path(r$out_dir, "anomhist_metadata.json")); expect_identical(md$statuses$ANOM_VERIFICATION_STATUS_V2, "PASS"); expect_match(md$protocol_v2_hash, "^[0-9A-F]{64}$")
})
