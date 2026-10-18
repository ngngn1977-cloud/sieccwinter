# ANOM equal-n analytic-sample diagnostic (Phase 3I.2, POST HOC). Declared in validation/ANOM_SAMPLE_SD_DIAGNOSTIC_DECLARATION.md before any study-data execution.
# Question: for the locality-reference gaussian ANOM rows of windows 1, 4, 5, 36, 37 and 59, does the recorded implied outcome SD
#   (coefficient x exposure_sd / standardised_effect) equal the directly measured outcome SD of each row's own complete-case sample,
#   and do rows with equal n use different complete-case samples?
# NO MODEL IS FITTED. Every input is read only. The failed run folder is verified against its RUN_MANIFEST.csv before and after.
# Output is aggregate only: no participant code, value, date, locality, membership list or membership hash is written or printed.
# This diagnostic changes no status, tolerance, protocol or result. ANOM_VERIFICATION_STATUS of anomhist_20261004_141846_28bd stays FAIL.
# Usage (one line, from the package root):
#   Rscript validation/anom_sample_sd_diag.R --data <canonical.csv> --run <anomhist run folder> --reference-dir <anomhist reference bundle> --out <new folder>
# The functions are sourced by validation/test_anom_sample_sd_diag_synthetic.R; the main step runs only when this file is the Rscript file.

ASD_WINDOWS <- c(1L, 4L, 5L, 36L, 37L, 59L)
ASD_REFERENCE <- "locality"
ASD_TOL <- 1e-9                                                                                    # the unchanged tolerance of the failing identity row
ASD_ACCEPTED_INPUT_SHA <- "REDACTED_PUBLIC_BUILD"     # accepted canonical CSV
ASD_FAILED_RUN_ID <- "anomhist_20261004_141846_28bd"
ASD_KEYS <- c("outcome", "metric", "reference_definition", "anomaly_method", "burden_direction", "window_or_lag")
ASD_IDENTITY_CHECK <- "gaussian standardised_effect: implied SD(outcome) constant within each outcome"
ASD_RUN_FILES <- c("anom_package_results.csv", "anom_scale_and_inference_identities.csv", "anomhist_metadata.json", "source_manifest_anomhist.csv", "ANOMHIST_STATUS.txt")
ASD_DEPENDENCIES <- c("R/anomaly_history.R", "R/cold_burden.R", "R/daily_layer.R", "R/engine.R", "R/cumulative.R", "R/import.R", "R/rainfall.R", "R/config.R", "R/utils.R",
                      "R/source_manifest.R", "scripts/load_package.R", "config/sieccwinter_config.json", "config/anomaly_history_validation_protocol.json")

.asd_stop <- function(code, detail = "") stop(paste0("SIW_ANOMDIAG_", code, if (nzchar(detail)) paste0(": ", detail) else ""), call. = FALSE)
.asd_norm <- function(p, must = TRUE) gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = must))
.asd_inside <- function(a, b) startsWith(tolower(paste0(sub("/+$", "", a), "/")), tolower(paste0(sub("/+$", "", b), "/")))   # a equals b or lies inside b
.asd_fmt <- function(x, d = 10) if (length(x)) paste(format(x, digits = d), collapse = "; ") else "none"

#' Relative difference with the scaling of the failing identity (|a - b| / |b|; 0 when the absolute difference is <= 1e-300); NA when a value is not finite.
asd_rel <- function(a, b) { d <- abs(a - b) / pmax(abs(b), 1e-300); z <- is.finite(a) & is.finite(b) & abs(a - b) <= 1e-300; d[z] <- 0; d[!(is.finite(a) & is.finite(b))] <- NA_real_; d }

#' Implied outcome SD of a gaussian results row: coefficient x exposure_sd / standardised_effect (the quantity of the failing identity row).
asd_implied_sd <- function(res) as.numeric(res$coefficient) * as.numeric(res$exposure_sd) / as.numeric(res$standardised_effect)

#' Complete-case masks exactly as siw_anomaly_scan -> .siw_cold_one builds them, locality reference only:
#'   x = siw_anomaly_features(<locality anomaly matrix of the metric and method>, min_available_fraction)[[burden]][, window];
#'   mask = tier_ok & is.finite(y) & is.finite(x), tier_ok = complete.cases(siw_tier_matrix(dat, <protocol tier covariates>)).
#' The literal per-participant feature route (siw_anomaly_feature_reference) is evaluated as a cross-check of the finite pattern.
#' Returns the rows (one per gaussian outcome x metric x method x burden x window), the masks (in memory only) and direct SD(y) over each mask.
asd_masks <- function(dat, config, protocol, windows = ASD_WINDOWS) {
  P <- protocol$parameters; L <- as.integer(P$lag_end); windows <- as.integer(windows)
  if (any(windows < 1L | windows > L)) .asd_stop("WINDOW_OUTSIDE_LAG_RANGE", paste(windows, collapse = ","))
  Xc <- siw_tier_matrix(dat, config$tiers[[P$tier]]); tier_ok <- stats::complete.cases(Xc)
  vals <- siw_anomaly_values(dat, config, protocol, ASD_REFERENCE)
  gau <- config$outcomes$outcome[config$outcomes$family == "gaussian"]
  rows <- list(); masks <- list()
  for (mi in seq_len(nrow(config$metrics))) {
    metric <- config$metrics$metric[mi]
    for (method in .siw_ah_methods(metric, P)) {
      A <- vals$metrics[[metric]][[method]]; Fe <- siw_anomaly_features(A, P$min_available_fraction)
      for (burden in c("signed_mean", "absolute_mean")) for (w in windows) {
        x <- Fe[[burden]][, w]
        xl <- vapply(seq_len(nrow(A)), function(i) siw_anomaly_feature_reference(A[i, ], w, burden, P$min_available_fraction), numeric(1L))
        for (o in gau) {
          y <- as.numeric(dat[[o]]); m <- tier_ok & is.finite(y) & is.finite(x); ml <- tier_ok & is.finite(y) & is.finite(xl)
          rows[[length(rows) + 1L]] <- data.frame(outcome = o, metric = metric, reference_definition = ASD_REFERENCE, anomaly_method = method, burden_direction = burden, window_or_lag = w,
                                                  n_mask = sum(m), n_excluded = sum(!m), mask_literal_route_equal = length(m) == length(ml) && all(m == ml),
                                                  direct_sd = if (sum(m) >= 2L) stats::sd(y[m]) else NA_real_, stringsAsFactors = FALSE)
          masks[[length(masks) + 1L]] <- m
        }
      }
    }
  }
  list(rows = do.call(rbind, rows), masks = masks, n_participants = nrow(dat))
}

#' Distinct complete-case sets, labelled in memory only: FULL = every participant; S1, S2, ... in order of first appearance. Membership is never returned.
#' Returns the label of every mask and the pairwise intersection / symmetric-difference counts of the distinct sets.
asd_label_sets <- function(masks) {
  sig <- vapply(masks, function(m) paste(which(!m), collapse = ","), "")
  u <- unique(sig); lab <- character(length(u)); full <- which(u == ""); other <- setdiff(seq_along(u), full)
  lab[full] <- "FULL"; lab[other] <- paste0("S", seq_along(other)); rep_mask <- masks[match(u, sig)]
  pairs <- list()
  if (length(u) >= 2L) for (a in seq_len(length(u) - 1L)) for (b in (a + 1L):length(u))
    pairs[[length(pairs) + 1L]] <- data.frame(set_a = lab[a], set_b = lab[b], n_a = sum(rep_mask[[a]]), n_b = sum(rep_mask[[b]]), intersection = sum(rep_mask[[a]] & rep_mask[[b]]),
                                              symmetric_difference = sum(xor(rep_mask[[a]], rep_mask[[b]])), stringsAsFactors = FALSE)
  empty <- data.frame(set_a = character(), set_b = character(), n_a = integer(), n_b = integer(), intersection = integer(), symmetric_difference = integer(), stringsAsFactors = FALSE)
  list(label = lab[match(sig, u)], pairs = if (length(pairs)) do.call(rbind, pairs) else empty)
}

#' Read a recorded ANOM results table (failed run or frozen reference) and check the columns this diagnostic needs.
asd_read_results <- function(path, what) {
  if (!file.exists(path)) .asd_stop(paste0(what, "_FILE_MISSING"), basename(path))
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  need <- c(ASD_KEYS, "outcome_family", "status", "n", "coefficient", "exposure_sd", "standardised_effect")
  miss <- setdiff(need, names(d)); if (length(miss)) .asd_stop(paste0(what, "_COLUMNS_MISSING"), paste(miss, collapse = ","))
  d
}

#' The recorded rows matching the traced rows (one per key; unmatched or duplicated keys fail).
asd_pick <- function(d, rows, what) {
  kd <- do.call(paste, c(lapply(d[ASD_KEYS], as.character), sep = "|")); kr <- do.call(paste, c(lapply(rows[ASD_KEYS], as.character), sep = "|"))
  if (anyDuplicated(kd[kd %in% kr])) .asd_stop(paste0(what, "_DUPLICATED_KEYS"))
  j <- match(kr, kd); if (anyNA(j)) .asd_stop(paste0(what, "_ROWS_UNMATCHED"), paste0(sum(is.na(j)), " of ", length(j)))
  d[j, , drop = FALSE]
}

#' Join the traced rows with the failed run and the frozen reference; direct SD versus implied SD; set labels.
asd_compare <- function(mk, run, frozen) {
  r <- mk$rows; a <- asd_pick(run, r, "RUN"); b <- asd_pick(frozen, r, "FROZEN")
  r$status_run <- as.character(a$status); r$status_frozen <- as.character(b$status); r$family_run <- as.character(a$outcome_family); r$family_frozen <- as.character(b$outcome_family)
  r$n_run <- as.integer(a$n); r$n_frozen <- as.integer(b$n)
  r$implied_sd_run <- asd_implied_sd(a); r$implied_sd_frozen <- asd_implied_sd(b)
  r$rel_diff_direct_vs_run <- asd_rel(r$direct_sd, r$implied_sd_run); r$rel_diff_direct_vs_frozen <- asd_rel(r$direct_sd, r$implied_sd_frozen)
  st <- asd_label_sets(mk$masks); r$set_label <- st$label
  list(rows = r, pairs = st$pairs, run_rows = a, frozen_rows = b)
}

#' The unchanged constancy row of siw_cold_identities, evaluated within each group (.siw_dl_identities uses groups = n).
asd_constancy <- function(res, groups, source, grouping) {
  sp <- split(seq_len(nrow(res)), as.character(groups)); out <- list()
  for (g in names(sp)) {
    s <- siw_cold_identities(res[sp[[g]], , drop = FALSE], NULL); s <- s[s$check == ASD_IDENTITY_CHECK, , drop = FALSE]
    out[[length(out) + 1L]] <- data.frame(source = source, grouping = grouping, group = g, n_models = s$n_models, max_rel_diff = s$max_rel_diff, tolerance = s$tolerance, status = s$status, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

#' Declared checks and decision (see the declaration). D1-D5 are required; D6-D9 are descriptive.
asd_decide <- function(cmp, old, regrouped, recorded, tol = ASD_TOL) {
  r <- cmp$rows; ck <- list(); pf <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
  add <- function(id, what, value, status) ck[[length(ck) + 1L]] <<- data.frame(item = id, check = what, value = as.character(value), status = status, stringsAsFactors = FALSE)
  ok1 <- all(r$status_run == "COMPLETED" & r$status_frozen == "COMPLETED" & r$family_run == "gaussian" & r$family_frozen == "gaussian")
  add("D1", "every selected row is a COMPLETED gaussian model in the failed run and in the frozen reference", paste0(nrow(r), " rows"), pf(ok1))
  bad2 <- sum(!(r$n_mask == r$n_run & r$n_mask == r$n_frozen), na.rm = TRUE) + sum(is.na(r$n_run) | is.na(r$n_frozen))
  add("D2", "traced complete-case size equals the recorded n (failed run and frozen) for every row", paste0(bad2, " rows differ"), pf(bad2 == 0L))
  bad3 <- sum(!r$mask_literal_route_equal); add("D3", "vectorised and literal feature routes give the same complete-case set for every row", paste0(bad3, " rows differ"), pf(bad3 == 0L))
  f4 <- r$rel_diff_direct_vs_run; f5 <- r$rel_diff_direct_vs_frozen; ok4 <- all(is.finite(f4)) && max(f4) <= tol; ok5 <- all(is.finite(f5)) && max(f5) <= tol
  add("D4", paste0("direct outcome SD equals the implied SD recorded in the failed run (max relative difference <= ", tol, ")"), .asd_fmt(suppressWarnings(max(f4)), 6), pf(ok4))
  add("D5", paste0("direct outcome SD equals the implied SD of the frozen reference (max relative difference <= ", tol, ")"), .asd_fmt(suppressWarnings(max(f5)), 6), pf(ok5))
  spn <- tapply(r$set_label, r$n_mask, function(z) length(unique(z))); multi <- any(spn >= 2L)
  add("D6", "a sample size shared by two or more different complete-case sets (descriptive)", paste(paste0("n=", names(spn), ": ", spn, " set(s)"), collapse = "; "), if (multi) "INFO_YES" else "INFO_NO")
  oldf <- any(old$status == "FAIL")
  add("D7", "unchanged constancy rule grouped by n, on these rows (descriptive reproduction)", paste(paste0(old$source, " n=", old$group, ": ", old$status, " (", format(old$max_rel_diff, digits = 6), ")"), collapse = "; "), if (oldf) "INFO_FAIL_REPRODUCED" else "INFO_PASS")
  regok <- all(regrouped$status == "PASS")
  add("D8", "same rule grouped by the actual complete-case set (diagnostic only; not a revision of the check)", paste(paste0(regrouped$source, " ", regrouped$group, ": ", regrouped$status), collapse = "; "), if (regok) "INFO_PASS" else "INFO_FAIL")
  ro <- old$max_rel_diff[old$source == "failed_run" & old$status == "FAIL"]; rr <- recorded$max_rel_diff[recorded$source == "recorded_package" & recorded$status == "FAIL"]
  d9 <- if (!length(ro) && !length(rr)) "INFO_NONE_FAILED" else if (length(ro) == 1L && length(rr) == 1L && isTRUE(asd_rel(ro, rr) <= 1e-9)) "INFO_EQUAL" else "INFO_DIFFERENT"
  add("D9", "the by-n value on these rows equals the failing value recorded in the failed run (descriptive)", paste0("these rows ", .asd_fmt(ro, 8), "; recorded ", .asd_fmt(rr, 8)), d9)
  decision <- if (!(ok1 && bad2 == 0L && bad3 == 0L)) "BLOCKING_MASK_NOT_REPRODUCED" else if (!(ok4 && ok5)) "BLOCKING_IMPLIED_SD_NOT_EXPLAINED" else
    if (oldf && multi && regok) "PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES" else if (oldf) "BLOCKING_INCONSISTENT" else "NO_EQUAL_N_CONFLICT_IN_SELECTED_ROWS"
  list(checks = do.call(rbind, ck), decision = decision)
}

asd_meaning <- function(decision) switch(decision,
  PREMISE_DISPROVEN_EQUAL_N_DIFFERENT_SAMPLES = "The recorded implied SDs equal the directly measured SDs of each row's own complete-case sample, and equal n hides different samples. The identity row's premise (equal n = same sample) is false for these rows. Any revised check needs a new declared post hoc protocol, regression tests and an author decision; the failed run stays FAIL.",
  BLOCKING_IMPLIED_SD_NOT_EXPLAINED = "Directly measured SDs do not reproduce the recorded implied SDs. The ANOM failure stays blocking; investigate without widening any tolerance.",
  BLOCKING_MASK_NOT_REPRODUCED = "The traced complete-case sets do not reproduce the recorded rows (status, family or n). Tracing or inputs differ; nothing is concluded.",
  BLOCKING_INCONSISTENT = "The failure is reproduced but is not explained by different equal-n samples. The ANOM failure stays blocking.",
  NO_EQUAL_N_CONFLICT_IN_SELECTED_ROWS = "No equal-n conflict in the selected rows.",
  "UNKNOWN")

#' Aggregate output tables (no identifiers, no participant values, no membership).
asd_tables <- function(cmp, old, regrouped, recorded, dec) {
  r <- cmp$rows
  rows <- r[, c("outcome", "metric", "anomaly_method", "burden_direction", "window_or_lag", "set_label", "n_mask", "n_excluded", "n_run", "n_frozen", "status_run", "status_frozen",
                "mask_literal_route_equal", "direct_sd", "implied_sd_run", "implied_sd_frozen", "rel_diff_direct_vs_run", "rel_diff_direct_vs_frozen")]
  key <- paste(r$set_label, r$outcome, sep = "|"); sets <- list()
  for (k in unique(key)) {
    z <- r[key == k, , drop = FALSE]; ds <- unique(z$direct_sd)
    sets[[length(sets) + 1L]] <- data.frame(set_label = z$set_label[1L], outcome = z$outcome[1L], n = z$n_mask[1L], n_excluded = z$n_excluded[1L], windows = paste(sort(unique(z$window_or_lag)), collapse = ";"),
      rows = nrow(z), direct_sd = if (length(ds) == 1L) ds else NA_real_, direct_sd_distinct_values = length(ds), implied_sd_run_min = min(z$implied_sd_run), implied_sd_run_max = max(z$implied_sd_run),
      implied_sd_frozen_min = min(z$implied_sd_frozen), implied_sd_frozen_max = max(z$implied_sd_frozen), max_rel_diff_direct_vs_run = max(z$rel_diff_direct_vs_run),
      max_rel_diff_direct_vs_frozen = max(z$rel_diff_direct_vs_frozen), stringsAsFactors = FALSE)
  }
  wk <- paste(r$outcome, r$window_or_lag, sep = "|"); win <- list()
  for (k in unique(wk)) {
    z <- r[wk == k, , drop = FALSE]
    win[[length(win) + 1L]] <- data.frame(outcome = z$outcome[1L], window_or_lag = z$window_or_lag[1L], rows = nrow(z), n_values = paste(sort(unique(z$n_mask)), collapse = ";"),
      set_labels = paste(unique(z$set_label), collapse = ";"), direct_sd_min = min(z$direct_sd), direct_sd_max = max(z$direct_sd), implied_sd_run_min = min(z$implied_sd_run),
      implied_sd_run_max = max(z$implied_sd_run), max_rel_diff_direct_vs_implied = max(c(z$rel_diff_direct_vs_run, z$rel_diff_direct_vs_frozen)), stringsAsFactors = FALSE)
  }
  list(anomdiag_rows = rows, anomdiag_sets = do.call(rbind, sets), anomdiag_set_pairs = cmp$pairs, anomdiag_window_summary = do.call(rbind, win),
       anomdiag_identity_check = rbind(old, regrouped, recorded), anomdiag_checks = dec$checks)
}

#' Fail closed if an output table has an identifying column name or a cell equal to a participant code.
asd_privacy_guard <- function(tables, codes) {
  codes <- unique(as.character(codes)); codes <- codes[!is.na(codes) & grepl("[A-Za-z]", codes)]
  for (nm in names(tables)) {
    t <- tables[[nm]]; hit <- grep("(participant|code|date|address|coord|latitude|longitude|birth|member|hash|locality|^area$|^id$|_id$)", names(t), ignore.case = TRUE, value = TRUE)
    if (length(hit)) .asd_stop("PRIVACY_GUARD_COLUMN", paste0(nm, ": ", paste(hit, collapse = ",")))
    for (cn in names(t)) if (is.character(t[[cn]]) && any(t[[cn]] %in% codes)) .asd_stop("PRIVACY_GUARD_VALUE", paste0(nm, ": ", cn))
  }
  invisible(TRUE)
}

#' The failed run's files read here, verified against its RUN_MANIFEST.csv (fails closed).
asd_run_integrity <- function(run_dir, files = ASD_RUN_FILES) {
  mf <- file.path(run_dir, "RUN_MANIFEST.csv"); if (!file.exists(mf)) .asd_stop("RUN_MANIFEST_MISSING", basename(run_dir))
  man <- utils::read.csv(mf, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"); if (!all(c("file", "hash") %in% names(man))) .asd_stop("RUN_MANIFEST_COLUMNS_MISSING")
  out <- data.frame(file = files, recorded_sha256 = toupper(man$hash[match(files, man$file)]), found_sha256 = NA_character_, stringsAsFactors = FALSE)
  for (i in seq_along(files)) { p <- file.path(run_dir, files[i]); if (file.exists(p)) out$found_sha256[i] <- toupper(siw_file_hash(p)$value) }
  out$equal <- !is.na(out$recorded_sha256) & !is.na(out$found_sha256) & out$recorded_sha256 == out$found_sha256
  if (!all(out$equal)) .asd_stop("RUN_FILE_NOT_AS_RECORDED", paste(out$file[!out$equal], collapse = ", "))
  out
}

#' Main step. Fails closed before writing anything if an input, the protocol, the configuration or a traced source file differs from the failed run.
asd_main <- function(data_csv, run_dir, reference_dir, out_dir, root = siw_root(), protocol_path = NULL, expected_input_sha = ASD_ACCEPTED_INPUT_SHA, windows = ASD_WINDOWS, quiet = FALSE) {
  t0 <- proc.time()[["elapsed"]]
  data_csv <- .asd_norm(data_csv); run_dir <- .asd_norm(run_dir); reference_dir <- .asd_norm(reference_dir); root <- .asd_norm(root); out_dir <- .asd_norm(out_dir, must = FALSE)
  for (p in c(run_dir, reference_dir, dirname(data_csv), root)) if (.asd_inside(out_dir, p) || .asd_inside(p, out_dir)) .asd_stop("OUTPUT_OVERLAPS_AN_INPUT_OR_PACKAGE_FOLDER", p)
  if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE))) .asd_stop("OUTPUT_FOLDER_NOT_EMPTY", out_dir)
  options(sieccwinter.root = root); config <- siw_load_config(); protocol <- siw_ah_protocol(protocol_path); P <- protocol$parameters
  # 1. failed run folder: the files read are verified against its RUN_MANIFEST.csv (again at the end)
  run_list_before <- sort(list.files(run_dir, recursive = TRUE, all.files = TRUE)); integ <- asd_run_integrity(run_dir)
  meta_run <- jsonlite::fromJSON(file.path(run_dir, "anomhist_metadata.json"), simplifyVector = TRUE)
  st_lines <- readLines(file.path(run_dir, "ANOMHIST_STATUS.txt"), warn = FALSE); av <- grep("^ANOM_VERIFICATION_STATUS=", st_lines, value = TRUE)
  run_anom_verification <- if (length(av)) sub("^ANOM_VERIFICATION_STATUS=", "", av[1L]) else "MISSING"
  # 2. the data file must be the input of the failed run and the accepted canonical CSV
  ih <- toupper(siw_file_hash(data_csv)$value)
  if (!identical(ih, toupper(as.character(meta_run$input_hash)))) .asd_stop("INPUT_DIFFERS_FROM_FAILED_RUN", paste0("found ", ih, "; run ", meta_run$input_hash))
  if (!is.null(expected_input_sha) && !identical(ih, toupper(expected_input_sha))) .asd_stop("INPUT_NOT_THE_ACCEPTED_CANONICAL_CSV", paste0("found ", ih, "; accepted ", expected_input_sha))
  # 3. protocol, configuration and every traced source file as in the failed run
  if (!identical(toupper(protocol$protocol_hash), toupper(as.character(meta_run$protocol_hash)))) .asd_stop("PROTOCOL_DIFFERS_FROM_FAILED_RUN", paste0("found ", protocol$protocol_hash, "; run ", meta_run$protocol_hash))
  if (!identical(toupper(config$config_hash), toupper(as.character(meta_run$config_hash)))) .asd_stop("CONFIG_DIFFERS_FROM_FAILED_RUN", paste0("found ", config$config_hash, "; run ", meta_run$config_hash))
  smr <- utils::read.csv(file.path(run_dir, "source_manifest_anomhist.csv"), stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"); smn <- siw_source_manifest(root)
  dep <- data.frame(file = ASD_DEPENDENCIES, sha256_run = toupper(smr$sha256[match(ASD_DEPENDENCIES, smr$file)]), sha256_now = toupper(smn$manifest$sha256[match(ASD_DEPENDENCIES, smn$manifest$file)]), stringsAsFactors = FALSE)
  dep$equal <- !is.na(dep$sha256_run) & !is.na(dep$sha256_now) & dep$sha256_run == dep$sha256_now
  if (!all(dep$equal)) .asd_stop("TRACED_SOURCE_CHANGED_SINCE_FAILED_RUN", paste(dep$file[!dep$equal], collapse = ", "))
  fp_same <- identical(toupper(smn$fingerprint), toupper(as.character(meta_run$source_fingerprint_sha256)))
  dd <- siw_source_manifest_diff(list(manifest = data.frame(file = smr$file, sha256 = toupper(smr$sha256), stringsAsFactors = FALSE), fingerprint = ""),
                                 list(manifest = data.frame(file = smn$manifest$file, sha256 = toupper(smn$manifest$sha256), stringsAsFactors = FALSE), fingerprint = ""))
  src_changed <- c(dd$changed, if (length(dd$added)) paste0("+", dd$added), if (length(dd$removed)) paste0("-", dd$removed))
  # 4. the frozen reference as declared in the protocol
  rf <- Filter(function(z) identical(z$component, "ANOM"), protocol$reference_files)[[1L]]; ff <- file.path(reference_dir, rf$file)
  if (!file.exists(ff)) .asd_stop("FROZEN_REFERENCE_MISSING", rf$file)
  fh <- toupper(siw_file_hash(ff)$value); if (!identical(fh, toupper(rf$sha256))) .asd_stop("FROZEN_REFERENCE_NOT_AS_DECLARED", paste0("found ", fh, "; declared ", rf$sha256))
  # 5. traced complete-case sets (no model is fitted) and the recorded values
  raw <- siw_read_study_csv(data_csv, config); vr <- siw_validate_input(raw, config, lag_end = P$lag_end)
  if (any(vr$status == "FAIL")) .asd_stop("INPUT_VALIDATION_FAILED", paste(vr$check_id[vr$status == "FAIL"], collapse = ", "))
  dat <- siw_prepare_data(raw, config); mk <- asd_masks(dat, config, protocol, windows)
  cmp <- asd_compare(mk, asd_read_results(file.path(run_dir, "anom_package_results.csv"), "RUN"), asd_read_results(ff, "FROZEN"))
  old <- rbind(asd_constancy(cmp$run_rows, cmp$run_rows$n, "failed_run", "by_n_unchanged_rule"), asd_constancy(cmp$frozen_rows, cmp$frozen_rows$n, "frozen", "by_n_unchanged_rule"))
  reg <- rbind(asd_constancy(cmp$run_rows, cmp$rows$set_label, "failed_run", "by_actual_set_diagnostic_only"), asd_constancy(cmp$frozen_rows, cmp$rows$set_label, "frozen", "by_actual_set_diagnostic_only"))
  idf <- utils::read.csv(file.path(run_dir, "anom_scale_and_inference_identities.csv"), stringsAsFactors = FALSE, check.names = FALSE); rec <- idf[idf$check == ASD_IDENTITY_CHECK, , drop = FALSE]
  if (!nrow(rec)) .asd_stop("RECORDED_IDENTITY_ROW_MISSING", ASD_IDENTITY_CHECK)
  recorded <- data.frame(source = paste0("recorded_", rec$source), grouping = rep("by_n_as_recorded_full_grid", nrow(rec)), group = paste0("n_models=", rec$n_models), n_models = rec$n_models,
                         max_rel_diff = rec$max_rel_diff, tolerance = rec$tolerance, status = rec$status, stringsAsFactors = FALSE)
  dec <- asd_decide(cmp, old, reg, recorded); tabs <- asd_tables(cmp, old, reg, recorded, dec); asd_privacy_guard(tabs, dat$participant_code)
  # 6. the failed run folder must be unchanged
  integ2 <- asd_run_integrity(run_dir); run_list_after <- sort(list.files(run_dir, recursive = TRUE, all.files = TRUE))
  if (!(identical(integ$found_sha256, integ2$found_sha256) && identical(run_list_before, run_list_after))) .asd_stop("FAILED_RUN_FOLDER_CHANGED_DURING_DIAGNOSTIC")
  # 7. write aggregate output only
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(tabs)) siw_write_csv(tabs[[nm]], file.path(out_dir, paste0(nm, ".csv")))
  integ$found_sha256_after <- integ2$found_sha256; siw_write_csv(integ, file.path(out_dir, "anomdiag_failed_run_integrity.csv")); siw_write_csv(dep, file.path(out_dir, "anomdiag_traced_source_files.csv"))
  hp <- function(f) if (file.exists(f)) toupper(siw_file_hash(f)$value) else NA_character_
  meta <- list(diagnostic = "ANOM equal-n analytic-sample diagnostic (Phase 3I.2, post hoc; no model fitted)", decision = dec$decision, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
               r_version = R.version.string, reference_definition = ASD_REFERENCE, windows = windows, tolerance = ASD_TOL, rows = nrow(cmp$rows), participants_in_input = mk$n_participants,
               input_sha256 = ih, accepted_input_sha256 = if (is.null(expected_input_sha)) NA_character_ else toupper(expected_input_sha),
               failed_run_folder = basename(run_dir), failed_run_is_declared_run = identical(basename(run_dir), ASD_FAILED_RUN_ID), failed_run_anom_verification_status_unchanged = run_anom_verification,
               failed_run_files_verified_before_and_after = TRUE, protocol_sha256 = toupper(protocol$protocol_hash), config_sha256 = toupper(config$config_hash),
               run_source_fingerprint = toupper(as.character(meta_run$source_fingerprint_sha256)), current_source_fingerprint = toupper(smn$fingerprint), source_fingerprint_identical = fp_same,
               source_files_changed_since_run = src_changed, traced_source_files_identical = TRUE, frozen_file = rf$file, frozen_sha256 = fh,
               diagnostic_script_sha256 = hp(file.path(root, "validation", "anom_sample_sd_diag.R")), declaration_sha256 = hp(file.path(root, "validation", "ANOM_SAMPLE_SD_DIAGNOSTIC_DECLARATION.md")),
               seconds = round(proc.time()[["elapsed"]] - t0, 1))
  jsonlite::write_json(meta, file.path(out_dir, "anomdiag_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  ck <- dec$checks
  lines <- c(paste0("ANOMDIAG_DECISION=", dec$decision), paste0("MEANING=", asd_meaning(dec$decision)), paste0(ck$item, "=", ck$status, " | ", ck$check, " | ", ck$value),
             paste0("FAILED_RUN=", basename(run_dir), " (declared ", ASD_FAILED_RUN_ID, "; match ", identical(basename(run_dir), ASD_FAILED_RUN_ID), ")"),
             paste0("FAILED_RUN_ANOM_VERIFICATION_STATUS=", run_anom_verification, " (recorded; unchanged by this diagnostic)"), "FAILED_RUN_FILES_VERIFIED_BEFORE_AND_AFTER=TRUE",
             paste0("INPUT_SHA256=", ih), paste0("SOURCE_FINGERPRINT_IDENTICAL_TO_RUN=", fp_same, if (length(src_changed)) paste0(" (changed since run: ", paste(src_changed, collapse = ", "), ")") else ""),
             "TRACED_SOURCE_FILES_IDENTICAL_TO_RUN=TRUE", paste0("ROWS=", nrow(cmp$rows), "; WINDOWS=", paste(windows, collapse = ","), "; REFERENCE=", ASD_REFERENCE),
             "NO MODEL WAS FITTED. NO STATUS, TOLERANCE, PROTOCOL OR RESULT WAS CHANGED.", "Aggregate output only: no participant code, value, date, locality, membership list or membership hash.")
  writeLines(lines, file.path(out_dir, "ANOMDIAG_STATUS.txt")); siw_write_manifest(out_dir)
  res <- list(decision = dec$decision, checks = ck, tables = tabs, out_dir = out_dir, meta = meta)
  if (!quiet) asd_print(res)
  invisible(res)
}

asd_print <- function(res) {
  cat("ANOM equal-n analytic-sample diagnostic: no model fitted; aggregate output only\n\n")
  cat(readLines(file.path(res$out_dir, "ANOMDIAG_STATUS.txt"), warn = FALSE), sep = "\n")
  s <- res$tables$anomdiag_sets; s <- s[order(s$outcome, s$set_label), , drop = FALSE]
  cat("\nCOMPLETE-CASE SETS: direct outcome SD versus recorded implied SD\n")
  print(data.frame(outcome = s$outcome, set = s$set_label, n = s$n, excluded = s$n_excluded, windows = s$windows, rows = s$rows, direct_sd = format(s$direct_sd, digits = 10),
                   implied_run_min = format(s$implied_sd_run_min, digits = 10), implied_run_max = format(s$implied_sd_run_max, digits = 10),
                   max_rel_diff = format(pmax(s$max_rel_diff_direct_vs_run, s$max_rel_diff_direct_vs_frozen), digits = 3), stringsAsFactors = FALSE), row.names = FALSE, right = FALSE)
  cat("\nSET PAIRS (counts only)\n"); print(res$tables$anomdiag_set_pairs, row.names = FALSE)
  cat("\nOutput folder: ", res$out_dir, "\n", sep = "")
}

if (!interactive() && length(grep("^--file=.*anom_sample_sd_diag[.]R$", commandArgs(FALSE)))) {
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]); root <- normalizePath(file.path(dirname(fa), ".."), winslash = "/")
  source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
  args <- commandArgs(trailingOnly = TRUE); opt <- function(f) { i <- match(f, args); if (is.na(i) || i == length(args)) NA_character_ else args[i + 1L] }
  a <- c(data = opt("--data"), run = opt("--run"), ref = opt("--reference-dir"), out = opt("--out"))
  if (anyNA(a)) { cat("usage: Rscript validation/anom_sample_sd_diag.R --data <canonical.csv> --run <anomhist run folder> --reference-dir <reference bundle> --out <new folder>\n"); quit(status = 2L) }
  r <- tryCatch(asd_main(a[["data"]], a[["run"]], a[["ref"]], a[["out"]], root = root), error = function(e) { cat("ANOMDIAG_ERROR=", conditionMessage(e), "\nNothing was concluded.\n", sep = ""); NULL })
  quit(status = if (is.null(r)) 1L else 0L)
}
