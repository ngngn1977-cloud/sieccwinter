# Phase 3E: validation of the archived L01-L59 DLM grid (168 models, seven tiers; q24, q9) and the archived lag-form sensitivity (216 models; q216) against the frozen V1.4 outputs.
# Declared in config/archived_dlm_validation_protocol.json BEFORE any manual run. Separate from the V1/V2/V2.1 validation of the nine final L01-L56 models: nothing here extends that acceptance.
# Reuses the implemented branches (siw_archived_dlm_grid, siw_archived_dlm_sensitivity, siw_fit_dlm) unchanged; the only thing set differently is the glm epsilon of a COPY of the configuration (the archived specification's value).
# Deterministic quantities only; simultaneous critical values, simultaneous bands and the evidence status derived from them are NOT compared and are reported as requiring a separate protocol.

#' Load the declared archived-DLM validation protocol (config/archived_dlm_validation_protocol.json).
#' @export
siw_archived_validation_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "archived_dlm_validation_protocol.json"), file.path(root, "extdata", "archived_dlm_validation_protocol.json"), file.path(root, "inst", "extdata", "archived_dlm_validation_protocol.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_ARCHIVED_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

# configuration COPY with the archived specification's glm control; the package function siw_fit_dlm and the final models' configuration are untouched
.siw_av_config <- function(config, protocol) { b <- protocol$specification_trace$binary_fit_control; cfg <- config; cfg$dlm$final$glm_epsilon <- b$glm_epsilon; cfg$dlm$final$glm_maxit <- b$glm_maxit; cfg }

#' Fit the archived grid (168) and the lag-form sensitivity (216) with the unchanged package functions and keep their lag-specific and cumulative curves. No simulated draws (draws = 0): no critical value or simultaneous band is produced.
#' @return list(grid, sensitivity, lag_curves_168, cumulative_curves_168, lag_curves_216, summary_from_curves_168, summary_from_curves_216, config_used)
#' @export
siw_archived_validation_fits <- function(dat, config, protocol = siw_archived_validation_protocol()) {
  cfg <- .siw_av_config(config, protocol); a <- cfg$dlm$archived
  grid <- siw_archived_dlm_grid(dat, cfg, draws = 0L); sens <- siw_archived_dlm_sensitivity(dat, cfg)
  tag <- function(d, o, m, t, extra = NULL) { if (!nrow(d)) return(d); d$outcome <- o; d$metric <- m; d$model_tier <- t; if (!is.null(extra)) d$sensitivity_specification <- extra; d }
  cache <- .siw_new_cache(); xc <- lapply(stats::setNames(cfg$tier_order, cfg$tier_order), function(t) siw_tier_matrix(dat, cfg$tiers[[t]])); S1 <- LC1 <- CC1 <- list()
  for (oi in seq_len(nrow(cfg$outcomes))) for (m in cfg$metrics$metric) for (t in cfg$tier_order) {
    o <- cfg$outcomes$outcome[oi]; r <- siw_fit_dlm(dat, cfg, o, m, a$lag_end, a$lag_df, t, draws = 0L, cache = cache, return_curves = TRUE, x_cov = xc[[t]])
    S1[[length(S1) + 1L]] <- r$summary; LC1[[length(LC1) + 1L]] <- tag(r$lag_curve, o, m, t); CC1[[length(CC1) + 1L]] <- tag(r$cumulative_curve, o, m, t)
  }
  cache2 <- .siw_new_cache(); xs <- siw_tier_matrix(dat, cfg$tiers[[a$tier]]); S2 <- LC2 <- list()
  for (oi in seq_len(nrow(cfg$outcomes))) for (m in cfg$metrics$metric) for (L in a$sensitivity_lag_ends) for (d in a$sensitivity_lag_dfs) {
    o <- cfg$outcomes$outcome[oi]; r <- siw_fit_dlm(dat, cfg, o, m, L, d, a$tier, draws = 0L, cache = cache2, return_curves = TRUE, x_cov = xs); sp <- paste0("L", L, "_DF", d)
    s <- r$summary; s$sensitivity_specification <- sp; S2[[length(S2) + 1L]] <- s; LC2[[length(LC2) + 1L]] <- tag(r$lag_curve, o, m, a$tier, sp)
  }
  list(grid = grid, sensitivity = sens, lag_curves_168 = siw_bind_rows(LC1), cumulative_curves_168 = siw_bind_rows(CC1), lag_curves_216 = siw_bind_rows(LC2),
       summary_from_curves_168 = siw_bind_rows(S1), summary_from_curves_216 = siw_bind_rows(S2), config_used = list(glm_epsilon = cfg$dlm$final$glm_epsilon, glm_maxit = cfg$dlm$final$glm_maxit, draws = 0L))
}

.siw_av_key <- function(d, keys) do.call(paste, c(lapply(d[keys], as.character), sep = "|"))

# the expected complete key grids, from the package configuration (identical to the legacy configuration for outcomes, metrics, tiers)
.siw_av_expected_keys <- function(config, protocol) {
  a <- config$dlm$archived; o <- config$outcomes$outcome; m <- config$metrics$metric; specs <- as.vector(outer(a$sensitivity_lag_ends, a$sensitivity_lag_dfs, function(L, d) paste0("L", L, "_DF", d)))
  g <- expand.grid(outcome = o, metric = m, model_tier = config$tier_order, stringsAsFactors = FALSE); s <- expand.grid(outcome = o, metric = m, sensitivity_specification = specs, stringsAsFactors = FALSE)
  gl <- merge(g, data.frame(lag = seq_len(a$lag_end))); sl <- do.call(rbind, lapply(specs, function(sp) { L <- as.integer(sub("^L([0-9]+)_DF[0-9]+$", "\\1", sp)); merge(s[s$sensitivity_specification == sp, ], data.frame(lag = seq_len(L))) }))
  list(archived_grid_summary = .siw_av_key(g, c("outcome", "metric", "model_tier")), sensitivity_summary = .siw_av_key(s, c("outcome", "metric", "sensitivity_specification")), archived_grid_lag_curves = .siw_av_key(gl, c("outcome", "metric", "model_tier", "lag")),
       archived_grid_cumulative_curves = .siw_av_key(gl, c("outcome", "metric", "model_tier", "lag")), sensitivity_lag_curves = .siw_av_key(sl, c("outcome", "metric", "sensitivity_specification", "lag")))
}

#' Locate, hash and check the five frozen reference files named in the protocol (existence, SHA-256, record count, keys, required columns, complete expected key set). Read only.
#' @return list(tables = named list of data frames (NULL where absent or unreadable), provenance = data frame, ok = logical)
#' @export
siw_archived_read_references <- function(reference_dir, protocol = siw_archived_validation_protocol(), config = siw_load_config()) {
  ek <- .siw_av_expected_keys(config, protocol); comp <- protocol$comparisons; tabs <- list(); rows <- list()
  add <- function(file, item, value, ok) rows[[length(rows) + 1L]] <<- data.frame(file = file, item = item, value = as.character(value), ok = isTRUE(ok), stringsAsFactors = FALSE)
  for (rf in protocol$reference_files) {
    f <- file.path(reference_dir, rf$file); ex <- file.exists(f); add(rf$file, "file exists", ex, ex); if (!ex) next
    h <- siw_file_hash(f)$value; add(rf$file, "SHA-256 equals the declared value", paste0("found ", h, "; declared ", rf$sha256), identical(toupper(h), toupper(rf$sha256))); add(rf$file, "bytes equal the declared value", paste0("found ", file.info(f)$size, "; declared ", rf$bytes), file.info(f)$size == rf$bytes)
    d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")), error = function(e) NULL); add(rf$file, "file is readable", !is.null(d), !is.null(d)); if (is.null(d)) next
    keys <- unlist(rf$keys); haskeys <- all(keys %in% names(d)); add(rf$file, "key columns present", paste(keys, collapse = ","), haskeys); tabs[[rf$name]] <- d; if (!haskeys) next
    kv <- .siw_av_key(d, keys); blank <- sum(Reduce(`|`, lapply(d[keys], function(z) is.na(z) | !nzchar(trimws(as.character(z))))))
    add(rf$file, "records equal the declared count", paste0("found ", nrow(d), "; expected ", rf$expected_records), nrow(d) == rf$expected_records); add(rf$file, "no blank or missing keys", blank, blank == 0L); add(rf$file, "keys unique", sum(duplicated(kv)), !anyDuplicated(kv))
    add(rf$file, "key set equals the complete expected grid", paste0("missing ", length(setdiff(ek[[rf$name]], kv)), "; unexpected ", length(setdiff(kv, ek[[rf$name]]))), setequal(kv, ek[[rf$name]]))
    cm <- Filter(function(z) identical(z$reference, rf$name), comp); for (cc in cm) { need <- setdiff(unlist(cc$fields), c("global_df", "q24", "q9", "q216")); if (identical(cc$id, "SUMMARY_168")) need <- c(need, "global_df", "q24", "q9") else if (identical(cc$id, "SUMMARY_216")) need <- c(need, "global_df", "q216")
      miss <- setdiff(c(need, if (grepl("^SUMMARY", cc$id)) c("peak_lag", "status", "convergence", "separation")), names(d)); add(rf$file, paste0("required columns present for ", cc$id), if (length(miss)) paste("missing:", paste(miss, collapse = ",")) else "all present", !length(miss)) }
  }
  prov <- if (length(rows)) do.call(rbind, rows) else data.frame(file = character(), item = character(), value = character(), ok = logical())
  nfound <- vapply(protocol$reference_files, function(z) file.exists(file.path(reference_dir, z$file)), NA)
  if (!all(nfound)) prov <- rbind(prov, data.frame(file = vapply(protocol$reference_files[!nfound], function(z) z$file, ""), item = "REQUIRED REFERENCE ABSENT: nothing is compared", value = "absent", ok = FALSE, stringsAsFactors = FALSE))
  list(tables = tabs, provenance = prov, ok = nrow(prov) > 0L && all(prov$ok) && all(nfound))
}

# peak lag: exact integer match, or a numerical tie of |estimate| (rule declared in the protocol); returns one row per model
.siw_av_peak <- function(new, ref, refcurve, keys, ckeys, tol) {
  kn <- .siw_av_key(new, keys); kr <- .siw_av_key(ref, keys); j <- match(kr, kn); out <- list()
  for (i in seq_along(kr)) {
    lr <- as.integer(ref$peak_lag[i]); ln <- if (is.na(j[i])) NA_integer_ else as.integer(new$peak_lag[j[i]])
    cls <- if (is.na(ln) || is.na(lr)) "NONFINITE_OR_UNMATCHED" else if (ln == lr) "EXACT" else {
      sel <- which(.siw_av_key(refcurve, setdiff(ckeys, "lag")) == kr[i]); ea <- abs(refcurve$estimate[sel][match(c(lr, ln), refcurve$lag[sel])])
      if (anyNA(ea)) "DIFFERENT" else if (abs(ea[1L] - ea[2L]) <= tol$dlm_abs + tol$dlm_rel * ea[1L]) "PEAK_LAG_TIE_WITHIN_TOLERANCE" else "DIFFERENT" }
    out[[i]] <- data.frame(key = kr[i], peak_lag_package = ln, peak_lag_reference = lr, class = cls, stringsAsFactors = FALSE)
  }
  r <- do.call(rbind, out); r
}

# flags: status, convergence, separation must be identical
.siw_av_flags <- function(new, ref, keys) {
  kn <- .siw_av_key(new, keys); kr <- .siw_av_key(ref, keys); j <- match(kr, kn); out <- list()
  for (f in c("status", "convergence", "separation")) { a <- as.character(new[[f]][j]); b <- as.character(ref[[f]]); same <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b); out[[f]] <- data.frame(key = kr, field = f, package = a, reference = b, class = ifelse(same & !is.na(j), "EXACT", "DIFFERENT"), stringsAsFactors = FALSE) }
  do.call(rbind, out)
}

# per-model location of differences with the strata declared in the protocol (reference separation flag, outcome family, tier or specification)
.siw_av_model_level <- function(detail, ref, keys, spec_col) {
  kr <- .siw_av_key(ref, keys); d <- detail[!is.na(detail$statistic) & detail$statistic != "" & !(detail$statistic %in% c("ROW", "STRUCTURE", "RECORDS", "REQUIRED_COLUMN", "CLASSIFICATION")), , drop = FALSE]
  fam <- ref$outcome_family; if (is.null(fam)) fam <- NA_character_
  out <- data.frame(key = kr, outcome_family = fam, tier_or_specification = ref[[spec_col]], reference_separation = as.character(ref$separation), stringsAsFactors = FALSE)
  out$n_cells_not_exact <- vapply(kr, function(k) sum(d$key == k), 0L); out$n_cells_outside_tolerance <- vapply(kr, function(k) sum(d$key == k & d$class %in% c("DIFFERENT", "NONFINITE_ONE_SIDED", "NONFINITE_BOTH_SIDES", "UNPARSEABLE_REFERENCE_VALUE")), 0L)
  out$max_abs_diff <- vapply(kr, function(k) { v <- d$abs_diff[d$key == k]; if (length(v) && any(is.finite(v))) max(v, na.rm = TRUE) else 0 }, 0); out$max_rel_diff <- vapply(kr, function(k) { v <- d$rel_diff[d$key == k]; if (length(v) && any(is.finite(v))) max(v, na.rm = TRUE) else 0 }, 0)
  out$worst_class <- ifelse(out$n_cells_outside_tolerance > 0L, "OUTSIDE_TOLERANCE", ifelse(out$n_cells_not_exact > 0L, "WITHIN_TOLERANCE", "EXACT")); out
}

.siw_av_cmp_group <- function(cc, new, ref, keys, tol, refname, extra_new = list()) {
  new$global_df <- if ("global_df1" %in% names(new)) new$global_df1 else new$global_df
  fields <- unlist(cc$fields); nn <- new[, intersect(c(keys, fields), names(new)), drop = FALSE]
  siw_compare_tables(nn, ref, keys, tol, name = cc$id, reference_file = refname, required_columns = fields, expected_records = as.integer(cc$expected_records), required = TRUE, skip = character())
}

.siw_av_p_from_stat <- function(s) { g <- s$outcome_family == "gaussian"; ifelse(g, stats::pf(s$global_statistic, s$global_df, s$residual_df, lower.tail = FALSE), stats::pchisq(s$global_statistic, s$global_df, lower.tail = FALSE)) }

# multiplicity and internal-consistency checks (reference and package)
.siw_av_internal <- function(ref168, ref216, pk168, pk216, config) {
  rows <- list(); add <- function(check, value, criterion, ok) rows[[length(rows) + 1L]] <<- data.frame(check = check, value = value, criterion = criterion, status = if (is.na(ok)) "NOT_TESTED" else if (ok) "PASS" else "FAIL", stringsAsFactors = FALSE)
  rep9 <- config$representative_metrics; mx <- function(a, b) if (!length(a) || anyNA(a) || anyNA(b)) NA_real_ else max(abs(a - b))
  fam <- function(d, tier = TRUE) list(q24 = d$model_tier == config$primary_tier, q9 = d$model_tier == config$primary_tier & d$metric %in% rep9)
  for (src in c("reference", "package")) { d <- if (src == "reference") ref168 else pk168; s <- if (src == "reference") ref216 else pk216; f <- fam(d)
    n24 <- sum(is.finite(d$q24)); n9 <- sum(is.finite(d$q9)); n216 <- sum(is.finite(s$q216))
    add(paste0(src, ": q24 has exactly 24 finite values, all in M2_L"), paste0(n24, "; all in M2_L: ", all(which(is.finite(d$q24)) %in% which(f$q24))), "24 finite q24 values on the 24 M2_L rows (3 outcomes x 8 metrics)", n24 == 24L && setequal(which(is.finite(d$q24)), which(f$q24)))
    add(paste0(src, ": q9 has exactly 9 finite values on the M2_L representative-metric rows"), n9, "9 finite q9 values on the 9 M2_L rows with the three representative metrics", n9 == 9L && setequal(which(is.finite(d$q9)), which(f$q9)))
    add(paste0(src, ": q216 has 216 finite values"), n216, "all 216 sensitivity rows carry q216", n216 == 216L)
    r24 <- mx(d$q24[f$q24], stats::p.adjust(d$global_p[f$q24], "BH")); r9 <- mx(d$q9[f$q9], stats::p.adjust(d$global_p[f$q9], "BH")); r216 <- mx(s$q216, stats::p.adjust(s$global_p, "BH"))
    add(paste0(src, ": q24 equals stats::p.adjust BH of its 24 p values (max abs diff)"), r24, "<= 1e-12", is.finite(r24) && r24 <= 1e-12); add(paste0(src, ": q9 equals stats::p.adjust BH of its 9 p values (max abs diff)"), r9, "<= 1e-12", is.finite(r9) && r9 <= 1e-12)
    add(paste0(src, ": q216 equals stats::p.adjust BH of its 216 p values (max abs diff)"), r216, "<= 1e-12", is.finite(r216) && r216 <= 1e-12)
    for (nm in c("grid", "sensitivity")) { z <- if (nm == "grid") d else s; pf <- .siw_av_p_from_stat(z); rel <- max(abs(pf - z$global_p) / pmax(abs(z$global_p), 1e-300)); add(paste0(src, ": ", nm, " global_p equals the F / chi-square tail probability of global_statistic (max rel diff)"), rel, "<= 1e-9", is.finite(rel) && rel <= 1e-9)
      add(paste0(src, ": ", nm, " global_df equals the lag basis dimension lag_df in every row"), sum(z$global_df != z$lag_df), "0 rows differ", all(z$global_df == z$lag_df)); add(paste0(src, ": ", nm, " rank equals parameter_count in every row (full rank)"), sum(z$rank != z$parameter_count), "0 rows differ", all(z$rank == z$parameter_count)) }
    m2 <- d[d$model_tier == config$primary_tier, ]; l59 <- s[s$sensitivity_specification == "L59_DF3", ]; k <- merge(m2, l59, by = c("outcome", "metric"), suffixes = c(".g", ".s"))
    dd <- c(mx(k$global_p.g, k$global_p.s), mx(k$global_statistic.g, k$global_statistic.s), mx(k$cumulative_effect.g, k$cumulative_effect.s), mx(k$cumulative_se.g, k$cumulative_se.s), mx(k$n.g, k$n.s))
    add(paste0(src, ": the 24 sensitivity specifications L59_DF3 equal the 24 M2_L grid models (n, global statistic, p, cumulative effect, SE; max abs diff)"), if (nrow(k) == 24L) max(dd) else NA_real_, "24 pairs; <= 1e-12", nrow(k) == 24L && all(is.finite(dd)) && max(dd) <= 1e-12) }
  do.call(rbind, rows)
}

#' Verify the lag bases used by the archived specifications: the nine (L, df) combinations, with the independent construction. dlnm equality is NOT_TESTED where dlnm is not installed.
#' @export
siw_archived_verify_bases <- function(config = siw_load_config()) {
  a <- config$dlm$archived; combos <- unique(rbind(data.frame(L = a$lag_end, df = a$lag_df), expand.grid(L = a$sensitivity_lag_ends, df = a$sensitivity_lag_dfs))); out <- list()
  for (i in seq_len(nrow(combos))) { L <- combos$L[i]; d <- combos$df[i]; v <- siw_dlm_verify_basis(L, d); B <- siw_lag_basis(L, d); v$check <- as.character(v$check)
    v <- rbind(v, data.frame(check = "package basis has df columns", value = ncol(B), criterion = paste("equals", d), status = if (ncol(B) == d) "PASS" else "FAIL", stringsAsFactors = FALSE), data.frame(check = "package basis has full column rank", value = qr(B)$rank, criterion = paste("equals", d), status = if (qr(B)$rank == d) "PASS" else "FAIL", stringsAsFactors = FALSE))
    out[[i]] <- cbind(lag_end = L, lag_df = d, v, stringsAsFactors = FALSE) }
  do.call(rbind, out)
}

.siw_av_status <- function(x) if (!length(x)) "NOT_TESTED" else if (any(x == "FAIL")) "FAIL" else if (any(x == "NOT_TESTED")) "NOT_TESTED" else "PASS"

#' Run the archived-DLM validation on one canonical CSV against the frozen V1.4 reference files. Reads data and references read-only; writes only to out_dir; never changes a result to fit a reference.
#' @param protocol_path optional alternative protocol file (used only by the synthetic tests); the default is the declared protocol of the package
#' @param reference_dir folder containing the five frozen reference files named in the protocol (the V1.4 run's `models` folder)
#' @export
siw_run_archived_dlm_validation <- function(data_csv, reference_dir, out_dir, config = siw_load_config(), protocol_path = NULL) {
  t0 <- proc.time()[["elapsed"]]; reference_dir <- normalizePath(reference_dir, winslash = "/", mustWork = TRUE); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); out_dir <- normalizePath(out_dir, winslash = "/")
  if (startsWith(out_dir, paste0(reference_dir, "/")) || startsWith(reference_dir, paste0(out_dir, "/"))) stop("SIW_ARCHIVED_OUTPUT_AND_REFERENCE_FOLDERS_OVERLAP", call. = FALSE)
  options(sieccwinter.log_file = file.path(out_dir, "run.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE)
  protocol <- siw_archived_validation_protocol(protocol_path); sm0 <- siw_source_manifest(); W <- function(x, f) siw_write_csv(x, file.path(out_dir, f)); siw_log("Phase 3E archived DLM validation; protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint, " (", sm0$n_files, " files)")
  env <- .siw_v21_env(); jsonlite::write_json(env, file.path(out_dir, "archived_environment.json"), auto_unbox = TRUE, pretty = TRUE, null = "null"); writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "archived_sessionInfo.txt"))
  refs <- siw_archived_read_references(reference_dir, protocol, config); W(refs$provenance, "archived_reference_provenance.csv")
  carried <- c(FINAL_NINE_L56_NUMERICAL_VALIDATION = "ACCEPTED_UNDER_V2_1 (PHASE3D8; not recomputed, not extended to the archived analyses)", MANUSCRIPT_CONCORDANCE_STATUS = "REVIEW", FIGURE_PROVENANCE_STATUS = "NOT_TESTED", DLNM_EQUALITY_STATUS = "NOT_TESTED")
  nv <- do.call(rbind, lapply(protocol$not_deterministic_or_not_validated_here, function(z) data.frame(item = z$item, status = "NOT_VALIDATED_SEPARATE_VALIDATION_REQUIRED", reason = z$reason, needed = z$needed, stringsAsFactors = FALSE))); W(nv, "archived_not_validated.csv")
  finish <- function(st, status, extra = character(), msg = NULL) {
    writeLines(c(paste0(names(st), "=", st), extra, paste0(names(carried), "=", carried), "SIMULTANEOUS_CRITICAL_VALUES=NOT_VALIDATED_SEPARATE_PROTOCOL_REQUIRED (not deterministic; not compared)", "SIMULTANEOUS_BANDS_AND_EVIDENCE_STATUS=NOT_VALIDATED_SEPARATE_PROTOCOL_REQUIRED",
                 "SCOPE=archived L01-L59 grid (168) and lag-form sensitivity (216); separate from the nine final L01-L56 models; does not extend the V2.1 acceptance", "Results were not altered, redrawn or replaced to fit any reference."), file.path(out_dir, "ARCHIVED_DLM_STATUS.txt"))
    sm1 <- siw_source_manifest(); W(sm0$manifest, "source_manifest_archived.csv"); writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("SOURCE_CHANGED_DURING_RUN=", !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint))), file.path(out_dir, "SOURCE_FINGERPRINT.txt"))
    if (!is.null(msg)) writeLines(msg, file.path(out_dir, "RUN_SUMMARY.md")); list(out_dir = out_dir, statuses = st, status = status) }
  if (!refs$ok) {
    st <- c(EXECUTION_STATUS = "PASS", INPUT_PROVENANCE_STATUS = "FAIL", ARCHIVED_168_STATUS = "NOT_TESTED", LAG_FORM_216_STATUS = "NOT_TESTED", GLOBAL_TESTS_STATUS = "NOT_TESTED", MULTIPLICITY_STATUS = "NOT_TESTED", CURVES_STATUS = "NOT_TESTED", LAG_BASIS_STATUS = "NOT_TESTED", COVERAGE_STATUS = "FAIL", SOURCE_INTEGRITY_STATUS = "PASS")
    bad <- refs$provenance[!refs$provenance$ok, ]; fr <- finish(st, "VERIFICATION_FAILURE", c(paste0("PROVENANCE_PROBLEMS=", nrow(bad)), "NOTHING WAS COMPARED: required reference evidence is missing or does not match the declaration; nothing was passed."),
      c("# Archived DLM validation: reference provenance FAILED", "", paste0("* Reference folder: `", reference_dir, "`"), paste0("* Problems: ", nrow(bad), " (see archived_reference_provenance.csv)"), "", "No comparison was made and nothing was passed."))
    fr$provenance <- refs$provenance; return(fr)
  }
  ih <- siw_file_hash(data_csv); raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = config$dlm$archived$lag_end); W(rep, "input_validation_report.csv"); if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_prepare_data(raw, config); rt <- siw_daily_names("IMS_IDW_RAIN_TOTAL", config$dlm$archived$lag_end); rd <- siw_daily_names("IMS_IDW_RAIN_DAYS1", config$dlm$archived$lag_end); rain0 <- dat[, intersect(c(rt, rd), names(dat)), drop = FALSE]; ra <- siw_rainfall_anomalies(dat, config, config$dlm$archived$lag_end)
  siw_log("fitting 168 archived grid models and 216 lag-form sensitivity models (draws = 0; archived glm epsilon ", protocol$specification_trace$binary_fit_control$glm_epsilon, ")")
  fit <- siw_archived_validation_fits(dat, config, protocol); g <- fit$grid; s <- fit$sensitivity
  rain_preserved <- identical(rain0, dat[, intersect(c(rt, rd), names(dat)), drop = FALSE])
  tol <- config$tolerances; tol$name <- protocol$tolerance$class; R <- refs$tables; ref168 <- R$archived_grid_summary; ref216 <- R$sensitivity_summary
  cmpg <- list(); G <- function(id) Filter(function(z) identical(z$id, id), protocol$comparisons)[[1L]]
  cmpg$SUMMARY_168 <- .siw_av_cmp_group(G("SUMMARY_168"), g, ref168, c("outcome", "metric", "model_tier"), tol, "fixed_DLM_summary.csv")
  cmpg$SUMMARY_216 <- .siw_av_cmp_group(G("SUMMARY_216"), s, ref216, c("outcome", "metric", "sensitivity_specification"), tol, "DLM_lag_length_df_sensitivity_summary.csv")
  cmpg$LAG_CURVES_168 <- .siw_av_cmp_group(G("LAG_CURVES_168"), fit$lag_curves_168, R$archived_grid_lag_curves, c("outcome", "metric", "model_tier", "lag"), tol, "fixed_DLM_lag_curves.csv")
  cmpg$CUMULATIVE_CURVES_168 <- .siw_av_cmp_group(G("CUMULATIVE_CURVES_168"), fit$cumulative_curves_168, R$archived_grid_cumulative_curves, c("outcome", "metric", "model_tier", "lag"), tol, "fixed_DLM_cumulative_curves.csv")
  cmpg$LAG_CURVES_216 <- .siw_av_cmp_group(G("LAG_CURVES_216"), fit$lag_curves_216, R$sensitivity_lag_curves, c("outcome", "metric", "sensitivity_specification", "lag"), tol, "DLM_lag_length_df_sensitivity_curves.csv")
  summ <- do.call(rbind, lapply(cmpg, function(z) z$summary)); det <- siw_bind_rows(lapply(cmpg, function(z) z$detail)); W(summ, "archived_comparison_summary.csv"); W(det, "archived_comparison_detail.csv")
  exp_cells <- vapply(protocol$comparisons, function(z) as.integer(z$expected_compared_cells), 0L); names(exp_cells) <- vapply(protocol$comparisons, function(z) z$id, "")
  summ$expected_compared_cells <- exp_cells[summ$reference]; summ$cells_as_declared <- summ$n_compared == summ$expected_compared_cells
  pk <- list(SUMMARY_168 = .siw_av_peak(g, ref168, R$archived_grid_lag_curves, c("outcome", "metric", "model_tier"), c("outcome", "metric", "model_tier", "lag"), tol), SUMMARY_216 = .siw_av_peak(s, ref216, R$sensitivity_lag_curves, c("outcome", "metric", "sensitivity_specification"), c("outcome", "metric", "sensitivity_specification", "lag"), tol))
  peak <- rbind(cbind(comparison = "SUMMARY_168", pk$SUMMARY_168, stringsAsFactors = FALSE), cbind(comparison = "SUMMARY_216", pk$SUMMARY_216, stringsAsFactors = FALSE)); W(peak, "archived_peak_lag_comparison.csv")
  fl <- rbind(cbind(comparison = "SUMMARY_168", .siw_av_flags(g, ref168, c("outcome", "metric", "model_tier")), stringsAsFactors = FALSE), cbind(comparison = "SUMMARY_216", .siw_av_flags(s, ref216, c("outcome", "metric", "sensitivity_specification")), stringsAsFactors = FALSE)); W(fl, "archived_flag_comparison.csv")
  ml <- rbind(cbind(comparison = "SUMMARY_168", .siw_av_model_level(det[det$reference == "SUMMARY_168", ], ref168, c("outcome", "metric", "model_tier"), "model_tier"), stringsAsFactors = FALSE), cbind(comparison = "SUMMARY_216", .siw_av_model_level(det[det$reference == "SUMMARY_216", ], ref216, c("outcome", "metric", "sensitivity_specification"), "sensitivity_specification"), stringsAsFactors = FALSE)); W(ml, "archived_model_level_comparison.csv")
  strat <- stats::aggregate(cbind(models = 1, models_outside_tolerance = as.integer(ml$n_cells_outside_tolerance > 0L), models_not_exact = as.integer(ml$n_cells_not_exact > 0L)) ~ comparison + outcome_family + reference_separation, data = ml, FUN = sum); W(strat, "archived_stratum_summary.csv")
  g2 <- g; g2$global_df <- g2$global_df1; s2 <- s; s2$global_df <- s2$global_df1; internal <- .siw_av_internal(ref168, ref216, g2, s2, config)
  cons <- do.call(rbind, lapply(c("outcome", "metric", "model_tier", "n", "global_statistic", "global_p", "cumulative_effect", "cumulative_se", "peak_lag", "peak_effect"), function(cn) { a <- fit$summary_from_curves_168; b <- g; j <- match(.siw_av_key(a, c("outcome", "metric", "model_tier")), .siw_av_key(b, c("outcome", "metric", "model_tier")))
    a2 <- fit$summary_from_curves_216; b2 <- s; j2 <- match(.siw_av_key(a2, c("outcome", "metric", "sensitivity_specification")), .siw_av_key(b2, c("outcome", "metric", "sensitivity_specification")))
    same <- function(x, y) isTRUE(all((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y))); data.frame(check = paste0("package: archived functions and curve-producing fits give the identical '", cn, "' (168 and 216)"), value = same(a[[cn]], b[[cn]][j]) && same(a2[[cn]], b2[[cn]][j2]), criterion = "identical", status = if (same(a[[cn]], b[[cn]][j]) && same(a2[[cn]], b2[[cn]][j2])) "PASS" else "FAIL", stringsAsFactors = FALSE) }))
  cons$value <- as.character(cons$value); internal$value <- as.character(internal$value); internal <- rbind(internal, cons); W(internal, "archived_internal_consistency_and_multiplicity.csv")
  # q comparison package vs reference is part of SUMMARY_168 / SUMMARY_216 (required columns q24, q9, q216)
  bases <- siw_archived_verify_bases(config); W(bases, "archived_lag_basis_verification.csv")
  glob_bad <- det[det$reference %in% c("SUMMARY_168", "SUMMARY_216") & det$statistic %in% c("global_statistic", "global_df", "global_p") & det$class %in% c("DIFFERENT", "NONFINITE_ONE_SIDED", "NONFINITE_BOTH_SIDES", "UNPARSEABLE_REFERENCE_VALUE", "REQUIRED_COLUMN_MISSING_RESULTS", "REQUIRED_COLUMN_MISSING_REFERENCE", "REQUIRED_COLUMN_NOT_COMPARED"), ]
  glob_int <- internal$status[grepl("global_p equals|global_df equals", internal$check)]
  s168 <- summ[summ$reference == "SUMMARY_168", ]; s216 <- summ[summ$reference == "SUMMARY_216", ]; okc <- function(r) grepl("^PASS", r$status) && isTRUE(r$cells_as_declared)
  pkok <- function(id) all(peak$class[peak$comparison == id] %in% c("EXACT", "PEAK_LAG_TIE_WITHIN_TOLERANCE")); flok <- function(id) all(fl$class[fl$comparison == id] == "EXACT")
  mult_checks <- internal[grepl("q24|q9|q216|L59_DF3", internal$check), ]; mult_cmp <- det[det$reference %in% c("SUMMARY_168", "SUMMARY_216") & det$statistic %in% c("q24", "q9", "q216") & !det$class %in% c("EXACT", "WITHIN_TOLERANCE"), ]
  curves_ok <- all(vapply(c("LAG_CURVES_168", "CUMULATIVE_CURVES_168", "LAG_CURVES_216"), function(id) okc(summ[summ$reference == id, ]), NA))
  st <- c(EXECUTION_STATUS = "PASS", INPUT_PROVENANCE_STATUS = "PASS",
          ARCHIVED_168_STATUS = if (okc(s168) && pkok("SUMMARY_168") && flok("SUMMARY_168")) "PASS" else "FAIL", LAG_FORM_216_STATUS = if (okc(s216) && pkok("SUMMARY_216") && flok("SUMMARY_216")) "PASS" else "FAIL",
          GLOBAL_TESTS_STATUS = if (!nrow(glob_bad) && all(glob_int == "PASS") && okc(s168) && okc(s216)) "PASS" else "FAIL", MULTIPLICITY_STATUS = if (all(mult_checks$status == "PASS") && !nrow(mult_cmp) && okc(s168) && okc(s216)) "PASS" else "FAIL",
          CURVES_STATUS = if (curves_ok) "PASS" else "FAIL", LAG_BASIS_STATUS = if (all(bases$status[bases$status != "NOT_TESTED"] == "PASS")) "PASS" else "FAIL")
  st[["COVERAGE_STATUS"]] <- if (all(summ$n_unclassified == 0L) && all(summ$cells_as_declared) && nrow(summ) == length(protocol$comparisons) && all(summ$status != "NOT_TESTED") && all(summ$n_records_reference == stats::setNames(vapply(protocol$comparisons, function(z) as.integer(z$expected_records), 0L), vapply(protocol$comparisons, function(z) z$id, ""))[summ$reference]) &&all(summ$n_records_results == summ$n_records_reference)) "PASS" else "FAIL"
  sm1 <- siw_source_manifest(); changed <- !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint); st[["SOURCE_INTEGRITY_STATUS"]] <- if (changed) "FAIL" else "PASS"
  ep <- fit$config_used$glm_epsilon; sep_flag <- ref168$separation %in% TRUE; sep_flag2 <- ref216$separation %in% TRUE
  info <- c(paste0("ARCHIVED_168_MODELS_COMPARED=", nrow(ref168)), paste0("LAG_FORM_216_MODELS_COMPARED=", nrow(ref216)), paste0("COMPARED_CELLS_SUMMARY_168=", s168$n_compared, " (declared ", s168$expected_compared_cells, ")"), paste0("COMPARED_CELLS_SUMMARY_216=", s216$n_compared, " (declared ", s216$expected_compared_cells, ")"),
            paste0("MODELS_WITH_CELLS_OUTSIDE_TOLERANCE_168=", sum(ml$n_cells_outside_tolerance[ml$comparison == "SUMMARY_168"] > 0L), " (separation-flagged in the reference: ", sum(ml$n_cells_outside_tolerance[ml$comparison == "SUMMARY_168"] > 0L & ml$reference_separation[ml$comparison == "SUMMARY_168"] == "TRUE"), ")"),
            paste0("MODELS_WITH_CELLS_OUTSIDE_TOLERANCE_216=", sum(ml$n_cells_outside_tolerance[ml$comparison == "SUMMARY_216"] > 0L), " (separation-flagged in the reference: ", sum(ml$n_cells_outside_tolerance[ml$comparison == "SUMMARY_216"] > 0L & ml$reference_separation[ml$comparison == "SUMMARY_216"] == "TRUE"), ")"),
            paste0("SEPARATION_FLAGGED_IN_REFERENCE=", sum(sep_flag), " of 168; ", sum(sep_flag2), " of 216"), paste0("RAINFALL_CELLS_MISSING_SUPPLIED=", ra$total_missing_cells %||% NA, "; INDICATOR_CELLS_MISSING=", ra$indicator_missing_cells %||% NA, "; DATA_PASSED_TO_FITS_UNCHANGED=", rain_preserved), paste0("GLM_EPSILON_USED=", ep, "; DRAWS=0"))
  overall <- if (any(st == "FAIL") || !rain_preserved) "VERIFICATION_FAILURE" else "OK_WITH_SEPARATE_VALIDATION_ITEMS_OPEN"
  meta <- list(package_version = siw_pkg_version(), batch = "3E_archived_DLM_validation", protocol_hash = protocol$protocol_hash, config_hash = config$config_hash, input_hash = ih$value, source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files,
               reference_hashes = vapply(protocol$reference_files, function(z) z$sha256, ""), glm_epsilon_used = ep, draws = 0L, statuses = as.list(st), rainfall_missing_cells_supplied = ra$total_missing_cells %||% NA, data_passed_to_fits_unchanged = rain_preserved, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), seconds = round(proc.time()[["elapsed"]] - t0, 2))
  jsonlite::write_json(meta, file.path(out_dir, "archived_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  cov <- summ[, c("reference", "expected_records", "n_records_reference", "n_records_results", "expected_compared_cells", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_nonfinite", "n_unmatched", "n_duplicate", "n_missing_required", "n_unclassified", "cells_as_declared", "status", "reason")]; W(cov, "archived_comparison_coverage.csv")
  fin <- finish(st, overall, info, c("# Archived L01-L59 grid (168) and lag-form sensitivity (216): validation against the frozen V1.4 outputs", "", paste0("* Protocol `", protocol$protocol_hash, "`; source fingerprint `", sm0$fingerprint, "` (", sm0$n_files, " files)"), "* Separate from the nine final L01-L56 models; the V2.1 acceptance is not extended to these analyses.", "", "## Statuses", "", paste0("* ", names(st), ": ", st), "", "## Counts", "", paste0("* ", info), "",
    "## Not deterministic / not validated here", "", paste0("* ", nv$item), "", "Nothing in this folder contains participant-level values."))
  fin$summary <- summ; fin$detail <- det; fin$peak <- peak; fin$flags <- fl; fin$model_level <- ml; fin$internal <- internal; fin$bases <- bases; fin$fits <- fit; fin$provenance <- refs$provenance; fin
}
