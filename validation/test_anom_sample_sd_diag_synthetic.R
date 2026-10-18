# Synthetic tests of validation/anom_sample_sd_diag.R (Phase 3I.2 diagnostic). FULLY SYNTHETIC: no study data, frozen file or real run folder is read.
# Everything is written under tempdir(). Usage (one line, from the package root): Rscript validation/test_anom_sample_sd_diag_synthetic.R
# The synthetic design produces equal-n different samples by the suspected mechanism itself (locality-day singletons), not by missing values:
#   participant A (row 54) is the only participant of locality 1 on its day, which is that locality's last day: no locality reference at lag 1,
#     so no anomaly feature for windows 1-4 (n = 54) and a feature from window 5 (4 of 5 values finite);
#   participant B (row 55) is the first participant of locality 2, 30 days before the next one: no locality reference at lags 30-59,
#     so a feature up to window 36 (29 of 36 finite) and none for windows 37-59 (n = 54);
#   every other participant shares its locality and day with at least one other participant, so it always has a locality reference.
fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]); ROOT <- normalizePath(file.path(dirname(fa), ".."), winslash = "/")
source(file.path(ROOT, "scripts", "load_package.R")); siw_dev_load(ROOT)
source(file.path(ROOT, "validation", "make_anomaly_history_synthetic.R"))       # ah_rich_rain(); its command-line block does not run when sourced
source(file.path(ROOT, "validation", "anom_sample_sd_diag.R"))                  # functions only; the main step does not run when sourced
CFG <- siw_load_config(); PH <- siw_ah_protocol(); SM <- siw_source_manifest(ROOT); GAU <- CFG$outcomes$outcome[CFG$outcomes$family == "gaussian"]
TMP <- file.path(tempdir(), paste0("anomdiag_syn_", format(Sys.time(), "%Y%m%d%H%M%S"))); dir.create(TMP, recursive = TRUE)
RES <- data.frame(test = character(), ok = logical(), detail = character(), stringsAsFactors = FALSE)
chk <- function(name, ok, detail = "") { ok <- isTRUE(ok); RES <<- rbind(RES, data.frame(test = name, ok = ok, detail = paste(as.character(detail), collapse = " "), stringsAsFactors = FALSE)); cat(if (ok) "PASS  " else "FAIL  ", name, if (nzchar(paste(detail, collapse = ""))) paste0("  [", paste(detail, collapse = " "), "]") else "", "\n", sep = "") }
err <- function(expr) tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e))
item <- function(r, id) r$checks$status[r$checks$item == id]

syn_raw <- function(variant = c("two_sets", "one_set")) {
  variant <- match.arg(variant)
  raw <- ah_rich_rain(siw_make_synthetic(n = 55L, seed = 21L, config = CFG)); ac <- sort(unique(raw$Area)); stopifnot(length(ac) == 3L)
  area <- rep(ac[3L], 55L); day <- integer(55L)
  area[1:10] <- ac[1L]; day[1:10] <- rep(c(10L, 20L, 30L, 40L, 41L), each = 2L)                 # locality 1: pairs; the last pair on day 41
  area[11:18] <- ac[2L]; day[11:18] <- rep(c(35L, 45L, 55L, 65L), each = 2L)                    # locality 2: pairs from day 35
  area[19:53] <- ac[3L]; day[19:53] <- c(rep(seq(0L, 75L, by = 5L), each = 2L), rep(78L, 3L))   # locality 3: 16 pairs and one triple
  area[54] <- ac[1L]; day[54] <- 42L                                                            # A: alone on the last day of locality 1
  area[55] <- ac[2L]; day[55] <- 5L                                                             # B: first in locality 2, 30 days before the next
  if (variant == "one_set") { area[53] <- ac[2L]; day[53] <- 5L }                               # control: B gets a same-locality same-day partner
  raw$Area <- area; raw$assessment_day <- day
  raw$UCLA_Lonliness_SCORE[54] <- 3; raw$UCLA_Lonliness_SCORE[55] <- 8; raw$Isolation_cont[54] <- 5; raw$Isolation_cont[55] <- 20   # A and B differ, so the two n = 54 SDs differ
  raw
}

# a synthetic failed-run folder (RUN_MANIFEST.csv-verified files), a frozen-format reference folder and a protocol copy declaring its SHA-256
syn_inputs <- function(dir, raw, scan = NULL, perturb = "none") {
  dir.create(dir, recursive = TRUE); csv <- file.path(dir, "syn.csv"); utils::write.csv(raw, csv, row.names = FALSE, na = "")
  if (is.null(scan)) { dat <- siw_prepare_data(siw_read_study_csv(csv, CFG), CFG); scan <- siw_anomaly_scan(dat, CFG, PH, windows = ASD_WINDOWS, diagnostics = FALSE)$results }
  tgt <- which(scan$reference_definition == "locality" & scan$window_or_lag == 37L & scan$metric == CFG$metrics$metric[1L] & scan$anomaly_method == "stable_anomaly" & scan$burden_direction == "signed_mean" & scan$outcome == GAU[1L])
  stopifnot(length(tgt) == 1L)
  Fz <- scan; if (perturb == "frozen_sd") Fz$standardised_effect[tgt] <- Fz$standardised_effect[tgt] * (1 + 1e-6)
  Rn <- scan; if (perturb == "run_sd") Rn$standardised_effect[tgt] <- Rn$standardised_effect[tgt] * (1 + 1e-6); if (perturb == "run_n") Rn$n[tgt] <- Rn$n[tgt] + 1
  ref <- file.path(dir, "ref"); dir.create(ref); fz <- file.path(ref, "daily_anomaly_results.csv"); utils::write.csv(Fz, fz, row.names = FALSE, na = "")
  p <- jsonlite::fromJSON(PH$protocol_path, simplifyVector = FALSE); i <- which(vapply(p$reference_files, function(z) identical(z$component, "ANOM"), FALSE))
  p$reference_files[[i]]$sha256 <- siw_file_hash(fz)$value; p$reference_files[[i]]$bytes <- file.info(fz)$size
  pf <- file.path(dir, "protocol_syn.json"); jsonlite::write_json(p, pf, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  run <- file.path(dir, "anomhist_syn_run"); dir.create(run); W <- function(x, f) utils::write.csv(x, file.path(run, f), row.names = FALSE, na = "")
  W(Rn, "anom_package_results.csv"); idA <- .siw_dl_identities(Rn); idA$source <- "package"; idR <- .siw_dl_identities(Fz[Fz$status == "COMPLETED", ]); idR$source <- "frozen"
  W(rbind(idA, idR), "anom_scale_and_inference_identities.csv"); W(SM$manifest, "source_manifest_anomhist.csv")
  jsonlite::write_json(list(protocol_hash = siw_ah_protocol(pf)$protocol_hash, config_hash = CFG$config_hash, input_hash = siw_file_hash(csv)$value, source_fingerprint_sha256 = SM$fingerprint),
                       file.path(run, "anomhist_metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  writeLines(c("EXECUTION_STATUS=PASS", paste0("ANOM_VERIFICATION_STATUS=", if (any(c(idA$status, idR$status) == "FAIL")) "FAIL" else "PASS")), file.path(run, "ANOMHIST_STATUS.txt"))
  f <- sort(list.files(run)); h <- vapply(file.path(run, f), function(z) siw_file_hash(z)$value, "", USE.NAMES = FALSE)
  utils::write.csv(data.frame(file = f, bytes = file.info(file.path(run, f))$size, algorithm = "sha256", hash = h, stringsAsFactors = FALSE), file.path(run, "RUN_MANIFEST.csv"), row.names = FALSE)
  list(csv = csv, run = run, ref = ref, protocol = pf, sha = toupper(siw_file_hash(csv)$value), scan = scan, ident = rbind(idA, idR))
}
run_diag <- function(inp, out) asd_main(inp$csv, inp$run, inp$ref, out, root = ROOT, protocol_path = inp$protocol, expected_input_sha = inp$sha, quiet = TRUE)
try_diag <- function(inp, out, label) tryCatch(run_diag(inp, out), error = function(e) { chk(paste(label, "runs"), FALSE, conditionMessage(e)); NULL })
W6 <- c("1", "4", "5", "36", "37", "59")

# ---- T1: equal n, different samples (the suspected mechanism)
RAW2 <- syn_raw("two_sets"); I1 <- syn_inputs(file.path(TMP, "t1"), RAW2); O1 <- file.path(TMP, "t1_out")
chk("T1 setup: the synthetic run reproduces the failing identity (equal n, different samples)", any(I1$ident$check == ASD_IDENTITY_CHECK & I1$ident$status == "FAIL"))
r1 <- try_diag(I1, O1, "T1")
if (!is.null(r1)) {
  tb <- r1$tables; rw <- tb$anomdiag_rows
  chk("T1 decision PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES", r1$decision == "PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES", r1$decision)
  chk("T1 240 rows (6 windows x 20 metric-method-burden x 2 gaussian outcomes)", nrow(rw) == 240L, nrow(rw))
  nw <- tapply(rw$n_mask, rw$window_or_lag, function(z) paste(unique(z), collapse = ";"))
  chk("T1 n = 54 at windows 1, 4, 37, 59 and n = 55 at windows 5, 36", identical(unname(as.character(nw[W6])), c("54", "54", "55", "55", "54", "54")), paste(names(nw), nw, sep = ":"))
  lw <- tapply(rw$set_label, rw$window_or_lag, function(z) paste(unique(z), collapse = ";"))
  chk("T1 one set per window: 1/4 share S1, 37/59 share S2, 5/36 are FULL", identical(unname(as.character(lw[W6])), c("S1", "S1", "FULL", "FULL", "S2", "S2")), paste(names(lw), lw, sep = ":"))
  pr <- tb$anomdiag_set_pairs; s12 <- pr$symmetric_difference[(pr$set_a == "S1" & pr$set_b == "S2") | (pr$set_a == "S2" & pr$set_b == "S1")]
  chk("T1 the two n = 54 sets differ by two participants (counts only)", identical(as.integer(s12), 2L), s12)
  for (o in GAU) {
    v <- as.numeric(RAW2[[o]]); ex <- c(S1 = stats::sd(v[-54]), S2 = stats::sd(v[-55]), FULL = stats::sd(v)); st <- tb$anomdiag_sets[tb$anomdiag_sets$outcome == o, ]
    got <- st$direct_sd[match(names(ex), st$set_label)]
    chk(paste0("T1 direct SD of each set equals the hand-computed SD (", o, ")"), length(got) == 3L && all(is.finite(got)) && all(abs(got / ex - 1) <= 1e-12), .asd_fmt(got))
  }
  chk("T1 D1-D5 PASS", all(vapply(c("D1", "D2", "D3", "D4", "D5"), function(i) item(r1, i), "") == "PASS"), paste(r1$checks$item, r1$checks$status))
  chk("T1 D6 equal n hides two sets", item(r1, "D6") == "INFO_YES")
  chk("T1 D7 the unchanged by-n rule fails on these rows (failure reproduced)", item(r1, "D7") == "INFO_FAIL_REPRODUCED")
  chk("T1 D8 grouped by the actual set the same rule passes", item(r1, "D8") == "INFO_PASS")
  chk("T1 D9 the by-n value equals the value recorded by the synthetic run", item(r1, "D9") == "INFO_EQUAL", r1$checks$value[r1$checks$item == "D9"])
  ex <- max(vapply(GAU, function(o) { v <- as.numeric(RAW2[[o]]); abs(stats::sd(v[-55]) / stats::sd(v[-54]) - 1) }, 0))
  ic <- tb$anomdiag_identity_check; ov <- ic$max_rel_diff[ic$source == "failed_run" & ic$grouping == "by_n_unchanged_rule" & ic$group == "54"]
  chk("T1 the by-n failing value equals |SD(without B) / SD(without A) - 1| computed by hand", length(ov) == 1L && abs(ov / ex - 1) <= 1e-9, paste(.asd_fmt(ov, 8), .asd_fmt(ex, 8)))
  txt <- unlist(lapply(list.files(O1, full.names = TRUE), readLines, warn = FALSE))
  chk("T1 no participant code in any output file", !any(grepl("SYN0", txt, fixed = TRUE)))
  forb <- unlist(lapply(tb, function(t) grep("(participant|code|date|address|coord|birth|member|hash|locality|^area$|_id$)", names(t), ignore.case = TRUE, value = TRUE)))
  chk("T1 no identifying column name in any output table", !length(forb), forb)
  chk("T1 the synthetic failed-run folder is unchanged", all(asd_run_integrity(I1$run)$equal))
  chk("T1 all declared output files written", all(file.exists(file.path(O1, c("ANOMDIAG_STATUS.txt", "anomdiag_rows.csv", "anomdiag_sets.csv", "anomdiag_set_pairs.csv", "anomdiag_window_summary.csv",
    "anomdiag_identity_check.csv", "anomdiag_checks.csv", "anomdiag_failed_run_integrity.csv", "anomdiag_traced_source_files.csv", "anomdiag_metadata.json", "MANIFEST.csv")))))
}

# ---- T2: identical-sample control (B has a partner: only one n = 54 set)
I2 <- syn_inputs(file.path(TMP, "t2"), syn_raw("one_set")); r2 <- try_diag(I2, file.path(TMP, "t2_out"), "T2")
if (!is.null(r2)) {
  rw <- r2$tables$anomdiag_rows
  chk("T2 control decision NO_EQUAL_N_CONFLICT_IN_SELECTED_ROWS", r2$decision == "NO_EQUAL_N_CONFLICT_IN_SELECTED_ROWS", r2$decision)
  chk("T2 one n = 54 set (windows 1, 4); windows 37, 59 have n = 55", identical(sort(unique(rw$set_label[rw$n_mask == 54L])), "S1") && all(rw$n_mask[rw$window_or_lag %in% c(37L, 59L)] == 55L))
  chk("T2 D1-D5 PASS and the unchanged by-n rule passes", all(vapply(c("D1", "D2", "D3", "D4", "D5"), function(i) item(r2, i), "") == "PASS") && item(r2, "D7") == "INFO_PASS" && item(r2, "D6") == "INFO_NO")
}

# ---- T3-T5: wrong-SD and wrong-n negatives (one row perturbed)
r3 <- try_diag(syn_inputs(file.path(TMP, "t3"), RAW2, I1$scan, "run_sd"), file.path(TMP, "t3_out"), "T3")
if (!is.null(r3)) chk("T3 wrong implied SD in the run (x 1+1e-6, one row) -> BLOCKING_IMPLIED_SD_NOT_EXPLAINED; D4 FAIL, D5 PASS", r3$decision == "BLOCKING_IMPLIED_SD_NOT_EXPLAINED" && item(r3, "D4") == "FAIL" && item(r3, "D5") == "PASS", r3$decision)
r4 <- try_diag(syn_inputs(file.path(TMP, "t4"), RAW2, I1$scan, "frozen_sd"), file.path(TMP, "t4_out"), "T4")
if (!is.null(r4)) chk("T4 wrong implied SD in the frozen reference -> BLOCKING_IMPLIED_SD_NOT_EXPLAINED; D5 FAIL, D4 PASS", r4$decision == "BLOCKING_IMPLIED_SD_NOT_EXPLAINED" && item(r4, "D5") == "FAIL" && item(r4, "D4") == "PASS", r4$decision)
r5 <- try_diag(syn_inputs(file.path(TMP, "t5"), RAW2, I1$scan, "run_n"), file.path(TMP, "t5_out"), "T5")
if (!is.null(r5)) chk("T5 recorded n differs from the traced set (one row) -> BLOCKING_MASK_NOT_REPRODUCED; D2 FAIL", r5$decision == "BLOCKING_MASK_NOT_REPRODUCED" && item(r5, "D2") == "FAIL", r5$decision)

# ---- T6: fail-closed guards (nothing is written)
oth <- RAW2; oth$UTCI_L10[20] <- oth$UTCI_L10[20] + 0.5; dir.create(file.path(TMP, "other")); oc <- file.path(TMP, "other", "other.csv"); utils::write.csv(oth, oc, row.names = FALSE, na = "")
g <- function(label, pattern, out, ...) { e <- err(asd_main(..., out_dir = out, root = ROOT, quiet = TRUE)); chk(paste("T6", label), grepl(pattern, e, fixed = TRUE) && !dir.exists(out), e) }
g("a different data file is refused", "INPUT_DIFFERS_FROM_FAILED_RUN", file.path(TMP, "t6a"), data_csv = oc, run_dir = I1$run, reference_dir = I1$ref, protocol_path = I1$protocol, expected_input_sha = NULL)
g("a data file other than the accepted canonical CSV is refused", "INPUT_NOT_THE_ACCEPTED_CANONICAL_CSV", file.path(TMP, "t6b"), data_csv = I1$csv, run_dir = I1$run, reference_dir = I1$ref, protocol_path = I1$protocol, expected_input_sha = strrep("0", 64L))
g("an output folder inside the failed run is refused", "OUTPUT_OVERLAPS_AN_INPUT_OR_PACKAGE_FOLDER", file.path(I1$run, "x"), data_csv = I1$csv, run_dir = I1$run, reference_dir = I1$ref, protocol_path = I1$protocol, expected_input_sha = I1$sha)
e <- err(asd_main(I1$csv, I1$run, I1$ref, O1, root = ROOT, protocol_path = I1$protocol, expected_input_sha = I1$sha, quiet = TRUE)); chk("T6 a non-empty output folder is refused", grepl("OUTPUT_FOLDER_NOT_EMPTY", e, fixed = TRUE), e)
g("a protocol other than the run's is refused", "PROTOCOL_DIFFERS_FROM_FAILED_RUN", file.path(TMP, "t6d"), data_csv = I1$csv, run_dir = I1$run, reference_dir = I1$ref, protocol_path = NULL, expected_input_sha = I1$sha)
tr <- file.path(TMP, "t6e_run"); dir.create(tr); invisible(file.copy(list.files(I1$run, full.names = TRUE), tr)); cat("x\n", file = file.path(tr, "anom_package_results.csv"), append = TRUE)
g("a run file that differs from RUN_MANIFEST.csv is refused", "RUN_FILE_NOT_AS_RECORDED", file.path(TMP, "t6e"), data_csv = I1$csv, run_dir = tr, reference_dir = I1$ref, protocol_path = I1$protocol, expected_input_sha = I1$sha)
fr <- file.path(TMP, "t6f_ref"); dir.create(fr); invisible(file.copy(file.path(I1$ref, "daily_anomaly_results.csv"), fr)); cat("\n", file = file.path(fr, "daily_anomaly_results.csv"), append = TRUE)
g("a frozen file that differs from the protocol declaration is refused", "FROZEN_REFERENCE_NOT_AS_DECLARED", file.path(TMP, "t6f"), data_csv = I1$csv, run_dir = I1$run, reference_dir = fr, protocol_path = I1$protocol, expected_input_sha = I1$sha)

# ---- T7: the command-line entry point the study step will use (usage error; the accepted-CSV guard refuses synthetic data and writes nothing)
rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"); sc <- file.path(ROOT, "validation", "anom_sample_sd_diag.R")
cli <- function(a) { o <- suppressWarnings(system2(rs, c(shQuote(sc), a), stdout = TRUE, stderr = TRUE)); s <- attr(o, "status"); list(out = o, status = if (is.null(s)) 0L else as.integer(s)) }
c1 <- cli(character()); chk("T7 command line without arguments: usage, exit code 2", c1$status == 2L && any(grepl("usage:", c1$out, fixed = TRUE)), c1$status)
o7 <- file.path(TMP, "t7_out"); c2 <- cli(c("--data", shQuote(I1$csv), "--run", shQuote(I1$run), "--reference-dir", shQuote(I1$ref), "--out", shQuote(o7)))
chk("T7 command line refuses a data file other than the accepted canonical CSV: exit code 1, nothing written", c2$status == 1L && any(grepl("INPUT_NOT_THE_ACCEPTED_CANONICAL_CSV", c2$out, fixed = TRUE)) && !dir.exists(o7), c2$status)

# ---- summary
if (!is.null(r1)) { cat("\nExample status file (synthetic T1):\n"); cat(readLines(file.path(O1, "ANOMDIAG_STATUS.txt"), warn = FALSE), sep = "\n") }
cat(sprintf("\nSYNTHETIC TESTS: %d checks; %d passed; %d failed\n", nrow(RES), sum(RES$ok), sum(!RES$ok)))
for (f in c("validation/anom_sample_sd_diag.R", "validation/ANOM_SAMPLE_SD_DIAGNOSTIC_DECLARATION.md", "validation/test_anom_sample_sd_diag_synthetic.R")) cat(f, " SHA256=", siw_file_hash(file.path(ROOT, f))$value, "\n", sep = "")
cat("SOURCE_FINGERPRINT=", SM$fingerprint, " (", SM$n_files, " files; validation/ is outside the fingerprint scope)\n", sep = "")
cat("Synthetic folder (temporary): ", TMP, "\n", sep = "")
quit(status = if (all(RES$ok)) 0L else 1L)
