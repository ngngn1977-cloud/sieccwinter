# Post hoc protocol v2, amendment v2_4 (14 Oct 2026): step 30 PH_AGE65 (the 90 representative M2-L models without participants aged under 65). Synthetic data only.
if (!exists("expect_match", mode = "function")) expect_match <- function(x, pattern) expect_true(isTRUE(all(grepl(pattern, x))))
P34 <- siw_posthoc_protocol(); D34 <- DAT; set.seed(34); young <- sample(nrow(D34), 9L); D34$Age[young] <- c(60, 61, 62, 63, 63, 64, 64, 64, 62)    # nine participants aged under 65 (the shipped synthetic ages are all 65+)
ctx34 <- function() { td <- file.path(tempdir(), paste0("ph34_", sample.int(1e6, 1))); dir.create(td, recursive = TRUE)
  list(synthetic = FALSE, run_id = basename(td), input_sha256 = "X", fingerprint = "F", out_dir = td, W = function(x, f) siw_write_csv(x, file.path(td, f)), options = list(), raw_participant_code = SYN$participant_code) }
no_codes34 <- function(dir) !any(vapply(list.files(dir, recursive = TRUE, full.names = TRUE), function(f) any(grepl("SYN0[0-9][0-9]", readLines(f, warn = FALSE))), NA))

test_that("protocol amendment v2_4: PH_AGE65 is declared 14 Oct 2026 after the family lock as a SENSITIVITY step 30 with the expected values and no reference file", {
  expect_identical(tail(SIW_PH_ITEMS, 1L), "age65"); expect_identical(length(P34$items), 14L); it <- siw_posthoc_item(P34, "age65")
  expect_identical(it$step, "PH_AGE65"); expect_identical(it$label, "SENSITIVITY"); expect_identical(it$order, 14L); expect_match(it$declared, "after the 30 July 2026 family lock")
  expect_identical(P34$amendment_v2_4$date, "2026-10-14"); expect_match(P34$amendment_v2_4$change, "PH_AGE65"); expect_match(P34$status_of_this_document, "PH_AGE65")
  expect_equal(it$rule$minimum_age, SIW_AGE65_MINIMUM); expect_identical(SIW_AGE65_MINIMUM, 65)
  e <- it$expected; expect_identical(c(e$n, e$events, e$nominal_p_lt_05), c(51L, 26L, 0L)); expect_equal(c(e$smallest_p, e$minimum_q90), c(0.071, 0.969)); expect_identical(c(e$smallest_p_model$outcome, e$smallest_p_model$metric, as.character(e$smallest_p_model$window)), c("Int_dummy", "UTCI", "42"))
  expect_identical(it$multiplicity$families[[1]], "q90-age65"); expect_false(is.list(it$reference)); expect_identical(vapply(P34$items, function(z) z$id, "")[1:13], SIW_PH_ITEMS[1:13])
  v1 <- jsonlite::fromJSON(sub("_v2[.]json$", "_v1.json", P34$protocol_path), simplifyVector = FALSE); expect_identical(v1$items, P34$items[1:10])
})

test_that("AGE65 variant: Age < 65 removed, every other M2-L term kept; HC3 with t p for continuous outcomes, Wald p for logistic; existing variants unchanged", {
  r <- siw_s17_cumulative(D34, CFG, "UCLA_Lonliness_SCORE", "gaussian", "UTCI", 28L, "AGE65", P34); d <- D34[D34$Age >= 65 & stats::complete.cases(D34[, c("UCLA_Lonliness_SCORE", "UTCI_AVG_28D", "Sex", "Age", "AssessmentDay_centered", "Locality_3group", "Econ_STS")]), ]
  f <- stats::lm(UCLA_Lonliness_SCORE ~ UTCI_AVG_28D + Sex + Age + AssessmentDay_centered + Locality_3group + Econ_STS, data = d)
  expect_equal(r$n, nrow(d)); expect_true(r$n < sum(stats::complete.cases(D34[, c("UCLA_Lonliness_SCORE", "UTCI_AVG_28D")]))); expect_equal(r$estimate, unname(stats::coef(f)[["UTCI_AVG_28D"]]), tolerance = 1e-10)
  expect_equal(r$residual_df, stats::df.residual(f)); expect_equal(r$p_t, 2 * stats::pt(abs(r$estimate / r$standard_error), r$residual_df, lower.tail = FALSE)); expect_equal(r$p_primary, r$p_t)
  b <- siw_s17_cumulative(D34, CFG, "Int_dummy", "binomial", "UTCI", 42L, "AGE65", P34); expect_equal(b$p_primary, b$p); expect_true(is.na(b$p_t)); expect_equal(b$event_count, sum(D34$Int_dummy[D34$Age >= 65] == 1, na.rm = TRUE))
  na <- siw_s17_cumulative(D34, CFG, "UCLA_Lonliness_SCORE", "gaussian", "UTCI", 28L, "NO_APRIL", P34); expect_false(any(c("p_t", "residual_df", "p_primary") %in% names(na)))
  full <- siw_s17_cumulative(D34, CFG, "UCLA_Lonliness_SCORE", "gaussian", "UTCI", 28L, "DATE_NS_DF2", P34); expect_true(r$n < full$n)      # the other variants do not filter on age
})

test_that("(30) age65 step: 90 fits, BH across the 90 (q90-age65), independent refit PASS, SI concordance columns, aggregate output only", {
  it <- siw_posthoc_item(P34, "age65"); cx <- ctx34(); x <- .siw_ph_item_age65(D34, CFG, P34, it, list(), cx); G <- x$tables$grid
  expect_equal(nrow(G), 90L); expect_true(all(G$variant == "AGE65")); expect_equal(G$q90_age65, stats::p.adjust(G$p_primary, "BH")); expect_true(all(G$family_label == "q90-age65")); expect_true(all(x$tables$verification$status == "PASS"))
  expect_identical(unname(x$statuses[c("AGE65_COMPUTATION_STATUS", "AGE65_VERIFICATION_STATUS")]), c("PASS", "PASS")); expect_identical(unname(x$statuses[["AGE65_SI_CONCORDANCE_STATUS"]]), "REVIEW")      # synthetic values differ from the study's
  sm <- x$tables$sample; expect_equal(sm$value[sm$quantity == "participants with Age < minimum age (excluded)"], 9); expect_equal(sm$value[sm$quantity == "participants in the canonical CSV"], nrow(D34))
  si <- utils::read.csv(file.path(cx$out_dir, "age65_si_concordance.csv"), stringsAsFactors = FALSE); expect_identical(names(si)[1:6], c("item", "quantity", "package_value", "manuscript_value", "decimals", "agrees"))
  expect_true(all(c("age65_cumulative_90.csv", "age65_independent_verification.csv", "age65_sample.csv", "age65_si_concordance.csv") %in% list.files(cx$out_dir))); expect_true(no_codes34(cx$out_dir))
  expect_true(all(it$outputs %in% list.files(cx$out_dir)))
})

test_that("age65 through siw_run_posthoc on the synthetic demo: PASS with NOT_APPLICABLE_SYNTHETIC_INPUT for the study-value comparison; primary inference untouched", {
  r <- siw_run_posthoc("age65", SYN_CSV, file.path(tempdir(), paste0("ph34r_", sample.int(1e6, 1))), CFG, synthetic_demo = TRUE)
  expect_identical(r$overall, "PASS"); expect_identical(unname(r$statuses[["AGE65_SI_CONCORDANCE_STATUS"]]), SIW_PH_NA_SYN); st <- readLines(file.path(r$out_dir, "POSTHOC_STATUS.txt"))
  expect_true(any(grepl("^PRIMARY_INFERENCE_EFFECT=NONE", st))); expect_true(any(grepl("^POSTHOC_STEP=PH_AGE65", st))); expect_true(any(grepl("^POSTHOC_LABEL=SENSITIVITY", st))); expect_true(no_codes34(r$out_dir))
  expect_true(length(list.files(r$out_dir, pattern = "_si_concordance[.]csv$")) == 1L)
})
