# Post hoc steps declared in config/posthoc_sensitivity_protocol_v2.json (version 1 of 13 Oct 2026 unchanged plus steps 27-30 of 14 Oct 2026; after the 30 July 2026 family lock).
# Every step is labelled SENSITIVITY, POST_HOC or RECONSTRUCTION. None changes an estimator, inclusion or complete-case rule, seed, draw count, tolerance, family or
# accepted result, and none can change q90, q9 or any accepted status: the primary results are read, never written. Statuses are PASS / REVIEW / NOT_TESTED / FAIL,
# reported verbatim; NOT_APPLICABLE_SYNTHETIC_INPUT replaces a comparison with study values only when the input is the shipped synthetic CSV (demo).
# Dates come from one declared constant (assessment_day 0 = 2026-01-29); no date file is read. Outputs are aggregate; identifiers in a reference are matched inside R only.

SIW_PH_ITEMS <- c("exposure_export", "deletion", "l59_bootstrap", "no_april", "source_agreement", "exposure_residuals", "profile_likelihood", "early_late", "h3_nested", "nearest_station", "exposure_qa", "vif_summary", "calendar_spline", "age65")
SIW_PH_NA_SYN <- "NOT_APPLICABLE_SYNTHETIC_INPUT"

#' Load the declared post hoc protocol (config/posthoc_sensitivity_protocol_v2.json; version 1 is kept unchanged beside it).
#' @export
siw_posthoc_protocol <- function(path = NULL) {
  if (is.null(path)) { root <- siw_root(); cand <- c(file.path(root, "config", "posthoc_sensitivity_protocol_v2.json"), file.path(root, "extdata", "posthoc_sensitivity_protocol_v2.json"), file.path(root, "inst", "extdata", "posthoc_sensitivity_protocol_v2.json")); path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_POSTHOC_PROTOCOL_NOT_FOUND", call. = FALSE) }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$protocol_path <- normalizePath(path, winslash = "/"); p$protocol_hash <- siw_file_hash(path)$value; p
}

#' One item of the post hoc protocol.
#' @export
siw_posthoc_item <- function(protocol, id) { it <- Filter(function(z) identical(z$id, id), protocol$items); if (!length(it)) stop("SIW_POSTHOC_UNKNOWN_ITEM: ", id, call. = FALSE); it[[1L]] }

.siw_ph_anchor <- function(protocol) as.Date(protocol$calendar_anchor$assessment_day_0)

#' Interview (L00) date of each participant: anchor + assessment_day (declared constant; no date file is read).
#' @export
siw_interview_date <- function(assessment_day, protocol = siw_posthoc_protocol()) .siw_ph_anchor(protocol) + as.integer(assessment_day)

#' Date of lag k for each participant: anchor + assessment_day - k. With several lags, a matrix of days since the anchor (rows participants, columns lags) is returned
#' when as_offset = TRUE, otherwise a Date vector (one lag) or a character matrix of ISO dates.
#' @export
siw_lag_date <- function(assessment_day, lag, protocol = siw_posthoc_protocol(), as_offset = FALSE) {
  off <- outer(as.integer(assessment_day), as.integer(lag), "-")
  if (as_offset) return(off)
  if (length(lag) == 1L) return(.siw_ph_anchor(protocol) + off[, 1L])
  matrix(format(.siw_ph_anchor(protocol) + as.vector(off)), nrow(off))
}

#' Early / Late assessment groups (assessment_day <= 54 Early; >= 55 Late), a factor with levels Early, Late.
#' @export
siw_early_late <- function(assessment_day) factor(ifelse(as.numeric(assessment_day) <= 54, "Early", "Late"), levels = c("Early", "Late"))

# ---- statuses --------------------------------------------------------------------------------------------------------------------------------------------------

.siw_ph_combine <- function(x) { x <- unlist(x); if (!length(x)) "NOT_TESTED" else if (any(x == "FAIL")) "FAIL" else if (any(x == "NOT_TESTED")) "NOT_TESTED" else if (any(x == "REVIEW")) "REVIEW" else if (all(x == SIW_PH_NA_SYN)) SIW_PH_NA_SYN else "PASS" }

#' Overall class of a post hoc step: FAIL if any status FAILS; REVIEW if any is REVIEW or NOT_TESTED; otherwise PASS. NOT_APPLICABLE_SYNTHETIC_INPUT is neutral.
#' @export
siw_posthoc_overall <- function(statuses) { v <- unname(unlist(statuses)); if (any(v == "FAIL")) "FAIL" else if (any(v %in% c("REVIEW", "NOT_TESTED"))) "REVIEW" else if (all(v %in% c("PASS", SIW_PH_NA_SYN))) "PASS" else "FAIL" }

# |x - target| within half a unit of the last stated decimal (x rounds to target irrespective of the tie rule)
.siw_ph_rounds_to <- function(x, target, decimals) is.finite(x) & is.finite(target) & abs(x - target) <= 0.5 * 10^(-decimals) * (1 + 1e-9) + 1e-12

# one row per SI / manuscript comparison
.siw_ph_si_row <- function(item, quantity, value, target, decimals, note = "") data.frame(item = item, quantity = quantity, package_value = as.numeric(value), si_value = as.numeric(target), decimals = as.integer(decimals),
  agrees = .siw_ph_rounds_to(as.numeric(value), as.numeric(target), decimals), note = note, stringsAsFactors = FALSE)

# ---- references ---------------------------------------------------------------------------------------------------------------------------------------------

#' Declared specification of a core_reference file: SHA-256 and bytes from the core_reference bundle of the input-identification declaration
#' (project_inputs_declaration_v1.json), record count from validation_plan.json (a "records" entry of the declaration, when present, takes precedence).
#' Returns NULL when neither declares the file; sha256 is NA when the declaration is not installed (public build) or its identification status is not accepted.
#' @param declaration path of the declaration; default: protocol$core_reference_declaration, else the installed config/project_inputs_declaration_v1.json
#' @export
siw_core_reference_spec <- function(file, protocol = NULL, declaration = NULL) {
  root <- siw_root(); cfgf <- function(n) { c0 <- file.path(root, c("config", "extdata", file.path("inst", "extdata")), n); c0[file.exists(c0)][1L] }
  pl <- cfgf("validation_plan.json"); rec <- NA_integer_
  if (!is.na(pl)) for (r in jsonlite::fromJSON(pl, simplifyVector = FALSE)$references) if (isTRUE(r$required) && !is.null(r$expected_records_by_file[[file]])) rec <- as.integer(r$expected_records_by_file[[file]])
  if (is.null(declaration)) declaration <- protocol$core_reference_declaration %||% cfgf("project_inputs_declaration_v1.json")
  ent <- NULL; why <- "the input-identification declaration (project_inputs_declaration_v1.json) is not installed"
  if (!is.null(declaration) && !is.na(declaration) && file.exists(declaration)) {
    why <- "the core_reference bundle of the input-identification declaration does not list this file"
    for (b in jsonlite::fromJSON(declaration, simplifyVector = FALSE)$bundles) if (identical(b$role, "core_reference")) for (x in b$files) if (identical(x$file, file)) ent <- x
  }
  if (is.null(ent) && is.na(rec)) return(NULL)
  okst <- c("CONFIRMED_AT_ACCEPTANCE", "CONFIRMED_BY_FROZEN_SOURCE", "AUTHOR_ACCEPTED_EVIDENCE"); acc <- !is.null(ent) && (ent$status %||% "") %in% okst
  if (!is.null(ent) && !acc) why <- paste0("identification status ", ent$status %||% "missing", " is not accepted")
  list(role = "core_reference", file = file, sha256 = if (acc) toupper(ent$sha256) else NA_character_, bytes = if (acc) ent$bytes else NA, expected_records = if (!is.null(ent$records)) as.integer(ent$records) else rec,
       declared_in = "project_inputs_declaration_v1.json (core_reference bundle) and validation_plan.json", identification = if (acc) ent$status else why)
}

#' Locate and verify one declared reference file (existence, SHA-256, bytes, readable, declared record count). Read only. Returns list(table, provenance, ok, status).
#' Files of the post hoc reference roles are declared in the protocol; files of the core_reference role in the input-identification declaration (siw_core_reference_spec).
#' status is NOT_TESTED when the role folder was not given, FAIL when the file is missing or does not match the declaration.
#' @export
siw_posthoc_reference <- function(dir, protocol, file, row_filter = NULL) {
  spec <- Filter(function(z) identical(z$file, file), protocol$reference_files); rows <- list(); add <- function(item, value, ok) rows[[length(rows) + 1L]] <<- data.frame(file = file, item = item, value = as.character(value), ok = isTRUE(ok), stringsAsFactors = FALSE)
  out <- function(tab, status) list(table = tab, provenance = if (length(rows)) do.call(rbind, rows) else data.frame(file = file, item = "reference folder", value = "not given", ok = FALSE, stringsAsFactors = FALSE), ok = identical(status, "PASS"), status = status)
  spec <- if (length(spec)) spec[[1L]] else siw_core_reference_spec(file, protocol)
  if (is.null(spec)) stop("SIW_POSTHOC_REFERENCE_NOT_DECLARED: ", file, call. = FALSE)
  if (is.null(dir) || !nzchar(dir)) return(out(NULL, "NOT_TESTED"))
  if (!is.null(spec$declared_in)) add("declared in", spec$declared_in, TRUE)
  if (!is.null(spec$identification)) { ida <- !is.na(spec$sha256); add("identification status accepted", spec$identification, ida); if (!ida) return(out(NULL, "FAIL")) }
  f <- file.path(dir, file); ex <- file.exists(f); add("file exists", ex, ex); if (!ex) return(out(NULL, "FAIL"))
  h <- siw_file_hash(f)$value; add("SHA-256 equals the declared value", paste0("found ", h, "; declared ", spec$sha256), identical(toupper(h), toupper(spec$sha256)))
  b <- file.info(f)$size; add("bytes equal the declared value", paste0("found ", b, "; declared ", spec$bytes), isTRUE(b == spec$bytes))
  d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")), error = function(e) NULL); add("file is readable", !is.null(d), !is.null(d))
  if (is.null(d)) return(out(NULL, "FAIL"))
  add("records equal the declared count", paste0("found ", nrow(d), "; declared ", spec$expected_records), nrow(d) == spec$expected_records)
  if (!is.null(row_filter)) for (k in names(row_filter)) { has <- k %in% names(d); add(paste0("filter column ", k, " present"), has, has); if (!has) return(out(NULL, "FAIL")); d <- d[!is.na(d[[k]]) & as.character(d[[k]]) == as.character(row_filter[[k]]), , drop = FALSE] }
  prov <- do.call(rbind, rows); out(d, if (all(prov$ok)) "PASS" else "FAIL")
}

#' Compare a package table with a reference table on declared keys and fields with an existing tolerance class (siw_compare_tables; nothing is altered).
#' @export
siw_posthoc_compare <- function(new, ref, keys, fields, config, tolerance = "cumulative", name = "reference", reference_file = "", expected = NA_integer_) {
  tol <- config$tolerances; tol$name <- tolerance
  nn <- new[, intersect(c(keys, fields), names(new)), drop = FALSE]; rr <- ref[, intersect(c(keys, fields), names(ref)), drop = FALSE]
  for (k in keys) { nn[[k]] <- as.character(nn[[k]]); if (k %in% names(rr)) rr[[k]] <- as.character(rr[[k]]) }
  siw_compare_tables(nn, rr, keys, tol, name = name, reference_file = reference_file, required_columns = fields, expected_records = as.integer(expected), required = TRUE, skip = character())
}

.siw_ph_cmp_status <- function(r) if (grepl("^PASS", r$summary$status[1L])) "PASS" else "FAIL"

# categorical columns compared as text (TRUE/FALSE, labels)
.siw_ph_cat <- function(new, ref, keys, fields) {
  kn <- do.call(paste, c(lapply(new[keys], as.character), sep = "|")); kr <- do.call(paste, c(lapply(ref[keys], as.character), sep = "|")); j <- match(kr, kn)
  do.call(rbind, lapply(fields, function(f) { a <- toupper(as.character(new[[f]][j])); b <- toupper(as.character(ref[[f]])); a[is.na(a)] <- ""; b[is.na(b)] <- ""; data.frame(field = f, n_compared = length(b), n_equal = sum(a == b & !is.na(j)), n_different = sum(a != b | is.na(j)), stringsAsFactors = FALSE) }))
}

# ---- run ----------------------------------------------------------------------------------------------------------------------------------------------------

.siw_ph_synthetic_sha <- function() { cand <- file.path(siw_root(), c("inst/extdata", "extdata"), "synthetic_siecc_winter.csv"); p <- cand[file.exists(cand)][1L]; if (is.na(p)) NA_character_ else siw_file_hash(p)$value }

#' Run one post hoc step on one canonical CSV.
#' @param item one of SIW_PH_ITEMS
#' @param refs named list of reference folders by role (posthoc_v14_reference, archive_engine_reference, core_reference, nearest_station_histories); absent roles are NOT_TESTED.
#'   nearest_station_histories is one CSV, several CSVs separated by "|" (one run per declared variant), or a folder of CSVs
#' @param synthetic_demo TRUE only for the shipped synthetic CSV (checked by SHA-256): comparisons with study values become NOT_APPLICABLE_SYNTHETIC_INPUT
#' @param protocol_path alternative protocol (synthetic tests only)
#' @param options named list of test options (for example replicates for l59_bootstrap); never used by the declared study run
#' @export
siw_run_posthoc <- function(item, data_csv, out_dir, config = siw_load_config(), refs = list(), synthetic_demo = FALSE, protocol_path = NULL, options = list()) {
  t0 <- proc.time()[["elapsed"]]; if (!item %in% SIW_PH_ITEMS) stop("SIW_POSTHOC_UNKNOWN_ITEM: ", item, call. = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); out_dir <- normalizePath(out_dir, winslash = "/")
  for (r in unlist(strsplit(as.character(unlist(refs)), "|", fixed = TRUE))) if (nzchar(r)) { if (file.exists(r) && !dir.exists(r)) r <- dirname(r); rr <- normalizePath(r, winslash = "/", mustWork = TRUE); if (startsWith(out_dir, paste0(rr, "/")) || identical(out_dir, rr)) stop("SIW_POSTHOC_OUTPUT_INSIDE_A_REFERENCE_FOLDER", call. = FALSE) }
  options(sieccwinter.log_file = file.path(out_dir, "run.log")); on.exit(options(sieccwinter.log_file = NULL), add = TRUE)
  protocol <- siw_posthoc_protocol(protocol_path); it <- siw_posthoc_item(protocol, item); sm0 <- siw_source_manifest(); W <- function(x, f) siw_write_csv(x, file.path(out_dir, f))
  siw_log("post hoc step ", it$step, " (", it$label, "); protocol ", protocol$protocol_hash, "; source fingerprint ", sm0$fingerprint)
  ih <- siw_file_hash(data_csv)$value
  if (isTRUE(synthetic_demo) && !identical(toupper(ih), toupper(.siw_ph_synthetic_sha()))) stop("SIW_POSTHOC_SYNTHETIC_DEMO_REQUIRES_THE_SHIPPED_SYNTHETIC_CSV", call. = FALSE)
  lag_end <- if (item == "l59_bootstrap") 59L else 56L
  raw <- siw_read_study_csv(data_csv, config); rep <- siw_validate_input(raw, config, lag_end = lag_end); W(rep, "input_validation_report.csv")
  if (any(rep$status == "FAIL")) stop("SIW_INPUT_VALIDATION_FAILED: see input_validation_report.csv", call. = FALSE)
  dat <- siw_attach_cumulative(siw_prepare_data(raw, config), config)$data; dat$EarlyLate <- siw_early_late(dat$AssessmentDay)
  ctx <- list(synthetic = isTRUE(synthetic_demo), run_id = basename(out_dir), input_sha256 = ih, fingerprint = sm0$fingerprint, out_dir = out_dir, W = W, options = options, data_csv = data_csv, raw_participant_code = raw$participant_code)
  fun <- get(paste0(".siw_ph_item_", item), mode = "function")
  res <- fun(dat, config, protocol, it, refs, ctx)
  st <- c(EXECUTION_STATUS = "PASS", res$statuses)
  sm1 <- siw_source_manifest(); st[["SOURCE_INTEGRITY_STATUS"]] <- if (isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint)) "PASS" else "FAIL"
  overall <- siw_posthoc_overall(st[names(st) != "EXECUTION_STATUS"])
  W(sm0$manifest, "source_manifest_posthoc.csv"); writeLines(c(paste0("SOURCE_FINGERPRINT_SHA256=", sm0$fingerprint), paste0("FILES=", sm0$n_files), paste0("SOURCE_CHANGED_DURING_RUN=", !isTRUE(siw_source_manifest_diff(sm0, sm1)$identical_fingerprint))), file.path(out_dir, "SOURCE_FINGERPRINT.txt"))
  secs <- round(proc.time()[["elapsed"]] - t0, 1)
  lines <- c(paste0(names(st), "=", st), paste0("POSTHOC_OVERALL=", overall), paste0("POSTHOC_ITEM=", item), paste0("POSTHOC_STEP=", it$step), paste0("POSTHOC_LABEL=", it$label),
             "PRIMARY_INFERENCE_EFFECT=NONE (q90, q9 and every accepted status are unchanged; this step cannot change them)", paste0("POSTHOC_PROTOCOL_SHA256=", protocol$protocol_hash),
             paste0("INPUT_SHA256=", ih), paste0("INPUT_KIND=", if (ctx$synthetic) "SHIPPED_SYNTHETIC_CSV (demo; no comparison with study values is possible)" else "STUDY_OR_TEST_CSV"), res$info, paste0("RUNTIME_SECONDS=", secs),
             "Results were not altered, redrawn or replaced to fit any reference.")
  writeLines(lines, file.path(out_dir, "POSTHOC_STATUS.txt"))
  jsonlite::write_json(list(package_version = siw_pkg_version(), item = item, step = it$step, label = it$label, protocol = protocol$protocol_name, protocol_version = protocol$protocol_version, protocol_status = protocol$status_of_this_document,
                            protocol_hash = protocol$protocol_hash, config_hash = config$config_hash, input_hash = ih, source_fingerprint_sha256 = sm0$fingerprint, source_files = sm0$n_files, synthetic_demo = ctx$synthetic,
                            statuses = as.list(st), overall = overall, seconds = secs, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), file.path(out_dir, "posthoc_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(utils::capture.output(print(utils::sessionInfo())), file.path(out_dir, "session_info.txt"))
  writeLines(c(paste0("# ", it$step, " (", it$label, ")"), "", it$what, "", paste0("* Protocol `", basename(protocol$protocol_path), "` (version ", protocol$protocol_version, ") SHA-256 `", protocol$protocol_hash, "`"), paste0("* Source fingerprint `", sm0$fingerprint, "`"),
               "* Primary inference unchanged: q90, q9 and every accepted status are read-only for this step.", "", "## Statuses", "", paste0("* ", names(st), ": ", st), paste0("* POSTHOC_OVERALL: ", overall), "", "## Notes", "", paste0("* ", res$info), "",
               "Every file in this folder is aggregate; no participant code, date or row value is written."), file.path(out_dir, "RUN_SUMMARY.md"))
  list(out_dir = out_dir, statuses = st, overall = overall, status = overall, tables = res$tables, info = res$info)
}
