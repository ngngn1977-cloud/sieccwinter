# Comparison of package results with frozen aggregate reference tables. Discrepancies are reported, never suppressed or corrected.
# Phase 2D: the validation fails (never passes) when a required reference is absent, required columns or expected keys are missing,
# keys are duplicated or blank, expected comparisons are skipped or unclassified, or missing / non-finite values disagree.
# Which references are required, their expected record counts and required columns are declared in config/validation_plan.json.

.siw_classify <- function(new, ref, abs_exact, rel_exact, abs_tol, rel_tol) {
  d <- abs(new - ref)
  ifelse(d <= abs_exact + rel_exact * abs(ref), "EXACT", ifelse(d <= abs_tol + rel_tol * abs(ref), "WITHIN_TOLERANCE", "DIFFERENT"))
}

.siw_detail_row <- function(name, key, statistic, cls) data.frame(reference = name, key = key, statistic = statistic, new_value = NA_real_, reference_value = NA_real_, abs_diff = NA_real_, rel_diff = NA_real_, class = cls, stringsAsFactors = FALSE)

.siw_summary_row <- function(name, status, reference_file = "", reason = "", required = NA, nc = 0L, ex = 0L, wt = 0L, df = 0L, unm = 0L, nf = 0L, dup = 0L, mreq = 0L, uncl = 0L, nref = NA_integer_, nnew = NA_integer_, expn = NA_integer_) {
  data.frame(reference = name, status = status, n_compared = as.integer(nc), n_exact = as.integer(ex), n_within_tolerance = as.integer(wt), n_different = as.integer(df), n_unmatched = as.integer(unm), n_nonfinite = as.integer(nf),
             reference_file = reference_file, n_duplicate = as.integer(dup), n_missing_required = as.integer(mreq), n_unclassified = as.integer(uncl), n_records_reference = as.integer(nref), n_records_results = as.integer(nnew),
             expected_records = as.integer(expn), required = required, reason = reason, stringsAsFactors = FALSE)
}

#' Compare a result table with a reference table using strict key matching.
#' @param subset_reference If TRUE the reference is a declared subset: result rows absent from it are counted, not failed.
#' @param required_columns Statistic columns that must exist in both tables and be compared on at least one cell, otherwise the comparison FAILS.
#' @param expected_records Expected number of unique reference records (and, unless subset_reference, of result records); NA = not checked.
#' @return list(summary = one-row data frame, detail = every non-exact comparison and every problem found)
#' @export
siw_compare_tables <- function(new, ref, keys, tol, aliases = list(), name = "reference", subset_reference = FALSE, reference_file = "", skip = c("seed", "simultaneous_draws"),
                               required_columns = character(), expected_records = NA_integer_, required = NA) {
  det <- list(); fail_now <- function(reason, cls) { det[[length(det) + 1L]] <<- .siw_detail_row(name, "TABLE", "STRUCTURE", cls)
    list(summary = .siw_summary_row(name, "FAIL", reference_file, reason, required, expn = expected_records), detail = siw_bind_rows(det)) }
  missing_k_new <- setdiff(keys, names(new)); missing_k_ref <- setdiff(keys, names(ref))
  if (length(missing_k_new) || length(missing_k_ref))
    return(fail_now(paste0("key columns missing; results: ", paste(missing_k_new, collapse = ","), "; reference: ", paste(missing_k_ref, collapse = ",")), "KEY_COLUMN_MISSING"))
  kn <- do.call(paste, c(lapply(new[keys], as.character), sep = "|")); kr <- do.call(paste, c(lapply(ref[keys], as.character), sep = "|"))
  badk <- function(tab) sum(Reduce(`|`, lapply(tab[keys], function(z) is.na(z) | !nzchar(trimws(as.character(z))))))
  n_badkey <- badk(new) + badk(ref)
  if (n_badkey) det[[length(det) + 1L]] <- .siw_detail_row(name, "BLANK_OR_MISSING_KEY", "ROW", "MISSING_KEY")
  dn <- unique(kn[duplicated(kn)]); dr <- unique(kr[duplicated(kr)]); n_dup <- length(dn) + length(dr)
  for (k in dn) det[[length(det) + 1L]] <- .siw_detail_row(name, k, "ROW", "DUPLICATE_KEY_RESULTS")
  for (k in dr) det[[length(det) + 1L]] <- .siw_detail_row(name, k, "ROW", "DUPLICATE_KEY_REFERENCE")
  ex <- 0L; wt <- 0L; df <- 0L; nf <- 0L; nc <- 0L
  only_ref <- setdiff(kr, kn); only_new <- setdiff(kn, kr)
  for (k in only_ref) det[[length(det) + 1L]] <- .siw_detail_row(name, k, "ROW", "UNMATCHED_REFERENCE_ONLY")
  if (!subset_reference) for (k in only_new) det[[length(det) + 1L]] <- .siw_detail_row(name, k, "ROW", "UNMATCHED_RESULT_ONLY")
  n_rec_mismatch <- 0L
  nref <- length(unique(kr)); nnew <- length(unique(kn))
  if (!is.na(expected_records)) {
    if (nref != expected_records) { n_rec_mismatch <- n_rec_mismatch + 1L; det[[length(det) + 1L]] <- .siw_detail_row(name, paste0("reference records=", nref, "; expected=", expected_records), "RECORDS", "RECORD_COUNT_MISMATCH") }
    if (!subset_reference && nnew != expected_records) { n_rec_mismatch <- n_rec_mismatch + 1L; det[[length(det) + 1L]] <- .siw_detail_row(name, paste0("result records=", nnew, "; expected=", expected_records), "RECORDS", "RECORD_COUNT_MISMATCH") }
  }
  common <- intersect(kn, kr); i <- match(common, kn); j <- match(common, kr)
  stat_cols <- setdiff(names(new)[vapply(new, is.numeric, logical(1))], c(keys, skip))
  col_n <- stats::setNames(integer(0), character(0))
  for (s in stat_cols) {
    rc <- intersect(c(s, aliases[[s]]), names(ref))[1L]; if (is.na(rc)) next
    a <- as.numeric(new[[s]][i]); b <- suppressWarnings(as.numeric(ref[[rc]][j]))
    unp <- !is.na(ref[[rc]][j]) & is.na(b)            # a reference cell that is present but cannot be read as a number
    both_na <- is.na(a) & is.na(b) & !unp; one_na <- xor(is.na(a), is.na(b)) | unp; nonfin <- !one_na & !both_na & !(is.finite(a) & is.finite(b))
    fin <- is.finite(a) & is.finite(b); cls <- rep(NA_character_, length(a))
    cls[fin] <- .siw_classify(a[fin], b[fin], tol$exact_abs, tol$exact_rel, tol[[paste0(tol$name, "_abs")]], tol[[paste0(tol$name, "_rel")]])
    cls[one_na] <- "NONFINITE_ONE_SIDED"; cls[nonfin] <- "NONFINITE_BOTH_SIDES"; cls[unp] <- "UNPARSEABLE_REFERENCE_VALUE"
    col_n[s] <- sum(!both_na); nc <- nc + sum(!both_na); ex <- ex + sum(cls == "EXACT", na.rm = TRUE); wt <- wt + sum(cls == "WITHIN_TOLERANCE", na.rm = TRUE)
    df <- df + sum(cls == "DIFFERENT", na.rm = TRUE); nf <- nf + sum(cls %in% c("NONFINITE_ONE_SIDED", "NONFINITE_BOTH_SIDES", "UNPARSEABLE_REFERENCE_VALUE"), na.rm = TRUE)
    bad <- which(!is.na(cls) & cls != "EXACT")
    if (length(bad)) det[[length(det) + 1L]] <- data.frame(reference = name, key = common[bad], statistic = s, new_value = a[bad], reference_value = b[bad], abs_diff = abs(a[bad] - b[bad]),
      rel_diff = abs(a[bad] - b[bad]) / pmax(abs(b[bad]), 1e-300), class = cls[bad], stringsAsFactors = FALSE)
  }
  # required columns: present in both tables (numeric in the results) and compared on at least one cell
  n_mreq <- 0L
  for (s in required_columns) {
    in_new <- s %in% names(new) && is.numeric(new[[s]]); rc <- intersect(c(s, aliases[[s]]), names(ref))[1L]
    if (!in_new) { n_mreq <- n_mreq + 1L; det[[length(det) + 1L]] <- .siw_detail_row(name, s, "REQUIRED_COLUMN", "REQUIRED_COLUMN_MISSING_RESULTS"); next }
    if (is.na(rc)) { n_mreq <- n_mreq + 1L; det[[length(det) + 1L]] <- .siw_detail_row(name, s, "REQUIRED_COLUMN", "REQUIRED_COLUMN_MISSING_REFERENCE"); next }
    if (is.na(col_n[s]) || col_n[s] == 0L) { n_mreq <- n_mreq + 1L; det[[length(det) + 1L]] <- .siw_detail_row(name, s, "REQUIRED_COLUMN", "REQUIRED_COLUMN_NOT_COMPARED") }
  }
  uncl <- nc - (ex + wt + df + nf)
  if (uncl != 0L) det[[length(det) + 1L]] <- .siw_detail_row(name, paste0("unclassified cells=", uncl), "CLASSIFICATION", "UNCLASSIFIED")
  detail <- siw_bind_rows(det); if (!nrow(detail)) detail <- .siw_detail_row(name, "", "", "")[0, ]
  unm <- length(only_ref) + if (subset_reference) 0L else length(only_new)
  problems <- df + nf + unm + n_dup + n_badkey + n_mreq + n_rec_mismatch + abs(uncl)
  reason <- c(if (df) paste0(df, " cells outside tolerance"), if (nf) paste0(nf, " missing/non-finite disagreements"), if (unm) paste0(unm, " unmatched records"), if (n_dup) paste0(n_dup, " duplicated keys"),
              if (n_badkey) "blank or missing keys", if (n_mreq) paste0(n_mreq, " required columns missing or not compared"), if (n_rec_mismatch) "record count differs from the plan", if (uncl != 0L) "unclassified comparisons",
              if (nc == 0L) "nothing was compared")
  status <- if (problems > 0L || nc == 0L) "FAIL" else if (wt > 0L) "PASS_WITHIN_TOLERANCE" else "PASS_EXACT"
  list(summary = .siw_summary_row(name, status, reference_file, paste(reason, collapse = "; "), required, nc, ex, wt, df, unm, nf, n_dup + n_badkey, n_mreq, abs(uncl), nref, nnew, expected_records), detail = detail)
}

#' Validate one declared reference: locate the file, enforce the plan, compare. Used by the core and the Batch A validations.
#' @param spec list(name, files, keys, result (table name), tolerance, aliases, plan = plan entry with required, scope, expected_records_by_file, required_columns)
.siw_validate_one <- function(tables, config, reference_dir, spec, subset_files = character()) {
  files <- unlist(spec$files); path <- file.path(reference_dir, files)[file.exists(file.path(reference_dir, files))][1L]
  tabname <- spec$result; pl <- spec$plan; req <- isTRUE(pl$required)
  if (is.na(path)) return(list(summary = .siw_summary_row(spec$name, if (req) "FAIL" else "NOT_TESTED", "no reference file found", if (req) "required reference file is absent" else paste0("out of scope: ", pl$scope), req), detail = data.frame()))
  if (is.null(tables[[tabname]])) return(list(summary = .siw_summary_row(spec$name, if (req) "FAIL" else "NOT_TESTED", basename(path), if (req) "required result table was not produced" else paste0("out of scope: ", pl$scope), req), detail = data.frame()))
  exp_n <- pl$expected_records_by_file[[basename(path)]]
  if (is.null(exp_n)) return(list(summary = .siw_summary_row(spec$name, "FAIL", basename(path), "reference file is not declared in the validation plan", req), detail = data.frame()))
  tryCatch({
    ref <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
    tol <- config$tolerances; tol$name <- spec$tolerance
    keys <- unlist(spec$keys); new <- tables[[tabname]]
    if ("window_or_lag" %in% keys && "window_or_lag" %in% names(ref)) { ref$window_or_lag <- suppressWarnings(as.integer(ref$window_or_lag)); new$window_or_lag <- as.integer(new$window_or_lag) }
    # the core cumulative references carry a transformation column of which only the raw rows are compared; a plan entry that declares retain_all_rows keeps every row (Batch B transformations)
    if ("transformation" %in% names(ref) && !isTRUE(pl$retain_all_rows)) ref <- ref[ref$transformation %in% "raw" | is.na(ref$transformation), , drop = FALSE]
    if ("lag_end" %in% names(ref) && identical(spec$name, "dlm_archived_L01_L59")) ref <- ref[ref$lag_end %in% config$dlm$archived$lag_end, , drop = FALSE]
    siw_compare_tables(new, ref, keys, tol, aliases = spec$aliases %||% list(), name = spec$name, subset_reference = basename(path) %in% subset_files, reference_file = basename(path),
                       required_columns = unlist(pl$required_columns), expected_records = as.integer(exp_n), required = req)
  }, error = function(e) list(summary = .siw_summary_row(spec$name, "FAIL", basename(path), paste0("comparison could not be completed: ", substr(conditionMessage(e), 1L, 120)), req), detail = data.frame()))
}

#' Validate a run against the frozen reference outputs in a folder, following the declared validation plan.
#' A required reference that is absent, unreadable, undeclared or incomplete FAILS; only references declared out of scope are reported NOT_TESTED.
#' @export
siw_validate_results <- function(tables, config, reference_dir, plan = siw_load_validation_plan()) {
  S <- list(); D <- list()
  subset_files <- c("MASTER_CUMULATIVE_ALL_MODELS_1200.csv", "MASTER_CUMULATIVE_PRIMARY_90.csv", "MASTER_DLM_ALL_MODELS_120.csv", "MASTER_DLM_ALL_M2_24.csv", "MASTER_DLM_PRIMARY_9.csv")
  pnames <- vapply(plan$references, function(z) z$name, "")
  for (i in seq_len(nrow(config$validation$references))) {
    rf <- config$validation$references[i, ]; pl <- plan$references[[match(rf$name, pnames)]]
    if (is.null(pl)) { S[[length(S) + 1L]] <- .siw_summary_row(rf$name, "FAIL", "", "reference is not declared in the validation plan", NA); next }
    r <- .siw_validate_one(tables, config, reference_dir, list(name = rf$name, files = rf$files, keys = rf$keys, result = sub("[.]csv$", "", rf$result), tolerance = rf$tolerance, aliases = config$validation$dlm_column_aliases, plan = pl), subset_files)
    S[[length(S) + 1L]] <- r$summary; D[[length(D) + 1L]] <- r$detail
  }
  list(summary = do.call(rbind, S), detail = siw_bind_rows(D))
}

#' Load the Batch A validation plan (config/validation_plan_batchA.json): its references carry their own files, keys, result tables, tolerance names and required columns.
#' @export
siw_load_batch_a_plan <- function(path = NULL) {
  if (is.null(path)) {
    root <- siw_root(); cand <- c(file.path(root, "config", "validation_plan_batchA.json"), file.path(root, "extdata", "validation_plan_batchA.json"), file.path(root, "inst", "extdata", "validation_plan_batchA.json"))
    path <- cand[file.exists(cand)][1L]; if (is.na(path)) stop("SIW_BATCH_A_PLAN_NOT_FOUND", call. = FALSE)
  }
  p <- jsonlite::fromJSON(path, simplifyVector = FALSE); p$plan_path <- normalizePath(path, winslash = "/"); p$plan_hash <- siw_file_hash(path)$value; p
}

#' Compare the Batch A result tables with the Batch A reference bundle, following the Batch A plan. Same strict rules as the core validator.
#' @export
siw_validate_batch_a <- function(tables, config, reference_dir, plan = siw_load_batch_a_plan()) {
  S <- list(); D <- list()
  for (pl in plan$references) {
    r <- .siw_validate_one(tables, config, reference_dir, list(name = pl$name, files = pl$files, keys = pl$keys, result = pl$result, tolerance = pl$tolerance, aliases = list(), plan = pl))
    S[[length(S) + 1L]] <- r$summary; D[[length(D) + 1L]] <- r$detail
  }
  list(summary = do.call(rbind, S), detail = siw_bind_rows(D))
}

#' Overall publication-validation status from the summary: FAIL if any comparison FAILS or any required reference is not PASS; NOT_TESTED if nothing was compared; otherwise PASS.
#' @export
siw_overall_validation_status <- function(summary) {
  ok <- grepl("^PASS", summary$status); req <- isTRUE(any(summary$required %in% TRUE))
  if (any(summary$status == "FAIL") || any(summary$required %in% TRUE & !ok)) return("FAIL")
  if (all(summary$status == "NOT_TESTED")) return("NOT_TESTED")
  "PASS"
}

#' Validate mode: run the paper workflow and compare with the reference folder. Results are never changed to fit the reference.
#' @export
siw_run_validate <- function(data_csv, reference_dir, out_dir, config = siw_load_config(), resume = TRUE) {
  if (!dir.exists(reference_dir)) stop("SIW_REFERENCE_DIR_NOT_FOUND", call. = FALSE)
  arch <- any(file.exists(file.path(reference_dir, c("fixed_DLM_summary.csv", "MASTER_DLM_ALL_MODELS_120.csv", "MASTER_DLM_ALL_M2_24.csv", "MASTER_DLM_PRIMARY_9.csv"))))
  run <- siw_run_paper(data_csv, out_dir, config, resume = resume, archived = arch)
  v <- siw_validate_results(run$tables, config, reference_dir)
  siw_write_table(v$summary, "validation_summary", out_dir); siw_write_table(v$detail, "validation_detail", out_dir)
  overall <- siw_overall_validation_status(v$summary)
  writeLines(c(paste0("VALIDATION_STATUS=", overall), paste0("REFERENCES_FAIL=", sum(v$summary$status == "FAIL")), paste0("REFERENCES_NOT_TESTED=", sum(v$summary$status == "NOT_TESTED")),
               paste0("REQUIRED_REFERENCES_NOT_PASSED=", sum(v$summary$required %in% TRUE & !grepl("^PASS", v$summary$status))),
               "Results were not altered to fit any reference."), file.path(out_dir, "VALIDATION_STATUS.txt"))
  siw_write_manifest(out_dir)
  c(run, list(validation = v$summary, validation_detail = v$detail, overall = overall))
}
