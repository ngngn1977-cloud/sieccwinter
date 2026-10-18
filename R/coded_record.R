# Coded model record, version 2: stable identifiers and declared multiplicity families for the cumulative grid (1,680), the nine final L01-L56 DLMs and
# the archived L01-L59 programme (grid 168, lag-form sensitivity 216). Codes are derived from the configuration only, so they do not depend on a run.
# Run statuses are attached when a run folder is given. Always written as NEW files; the legacy SIECC_All_Models_with_Codes.csv (37,348 rows) is never read or overwritten.

SIW_CODED_RECORD_VERSION <- "2"
.SIW_LEGACY_RECORD_NAME <- "SIECC_All_Models_with_Codes.csv"

#' Coded model record (version 2) of the cumulative, final L01-L56 and archived L01-L59 models, with declared multiplicity families.
#' @param paper_dir optional paper run folder (cumulative_results.csv, dlm_final_summary.csv) whose statuses, n and in-run families are attached
#' @param archived_dir optional archived DLM validation run folder (archived_model_level_comparison.csv) whose per-model validation class is attached
#' @export
siw_coded_model_record <- function(config = siw_load_config(), paper_dir = NULL, archived_dir = NULL) {
  oc <- config$outcomes; mc <- config$metrics; pt <- config$primary_tier; rep <- config$representative_metrics; f <- config$dlm$final; a <- config$dlm$archived
  ag <- siw_archived_glm_control(config)
  grid <- function(...) { g <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE); g }
  base <- function(oi, mi) data.frame(outcome = oc$outcome[oi], outcome_code = oc$code[oi], outcome_family = oc$family[oi], metric = mc$metric[mi], metric_code = mc$code[mi], stringsAsFactors = FALSE)
  fam <- function(...) { x <- c(...); x <- x[nzchar(x)]; if (length(x)) paste(x, collapse = ";") else "" }
  R <- list()
  g <- grid(window = config$windows, tier = config$tier_order, mi = seq_len(nrow(mc)), oi = seq_len(nrow(oc)))
  b <- base(g$oi, g$mi); prim <- g$tier == pt
  R$cum <- data.frame(scope = ifelse(prim, "CUMULATIVE_PRIMARY_M2_L", "CUMULATIVE_OTHER_TIER"), b, model_tier = g$tier, window_days = as.integer(g$window), lag_end = NA_integer_, lag_df = NA_integer_, specification = "",
    model_code = sprintf("SIW2.CUM.%s.%s.%s.W%02d", b$outcome_code, b$metric_code, g$tier, as.integer(g$window)), package_result_id = siw_cum_id(b$outcome_code, b$metric_code, g$window, g$tier),
    declared_families = vapply(seq_len(nrow(g)), function(i) if (!prim[i]) "" else fam(paste0("RAW_CUMULATIVE_q10_", b$outcome[i], "_", b$metric[i]), paste0("RAW_CUMULATIVE_q80_", b$outcome[i]),
      if (b$metric[i] %in% rep) "RAW_CUMULATIVE_q90_all_primary_pairs" else ""), ""),
    binary_glm_epsilon = config$glm$cumulative_epsilon, source_step = "PAPER", source_file = "cumulative_results.csv", same_model_as = "", stringsAsFactors = FALSE)
  g <- grid(mi = which(mc$metric %in% rep), oi = seq_len(nrow(oc))); b <- base(g$oi, g$mi)
  R$fin <- data.frame(scope = "FINAL_DLM_L01_L56", b, model_tier = f$tier, window_days = NA_integer_, lag_end = as.integer(f$lag_end), lag_df = as.integer(f$lag_df), specification = "",
    model_code = sprintf("SIW2.DLM56.%s.%s.%s.L%02dDF%d", b$outcome_code, b$metric_code, f$tier, as.integer(f$lag_end), as.integer(f$lag_df)), package_result_id = siw_dlm_id(b$outcome_code, b$metric_code, f$lag_end, f$lag_df, f$tier),
    declared_families = "DLM_FINAL_L01_L56_M2_L_q9", binary_glm_epsilon = f$glm_epsilon, source_step = "PAPER", source_file = "dlm_final_summary.csv", same_model_as = "", stringsAsFactors = FALSE)
  g <- grid(tier = config$tier_order, mi = seq_len(nrow(mc)), oi = seq_len(nrow(oc))); b <- base(g$oi, g$mi); prim <- g$tier == a$tier
  R$arch <- data.frame(scope = "ARCHIVED_DLM_L01_L59_GRID", b, model_tier = g$tier, window_days = NA_integer_, lag_end = as.integer(a$lag_end), lag_df = as.integer(a$lag_df), specification = "",
    model_code = sprintf("SIW2.ARCH59.%s.%s.%s.L%02dDF%d", b$outcome_code, b$metric_code, g$tier, as.integer(a$lag_end), as.integer(a$lag_df)), package_result_id = siw_dlm_id(b$outcome_code, b$metric_code, a$lag_end, a$lag_df, g$tier),
    declared_families = vapply(seq_len(nrow(g)), function(i) if (!prim[i]) "" else fam("ARCHIVED_L01_L59_M2_L_q24", if (b$metric[i] %in% rep) "ARCHIVED_L01_L59_M2_L_q9" else ""), ""),
    binary_glm_epsilon = ag$glm_epsilon, source_step = "ARCHIVED_DLM", source_file = "archived_model_level_comparison.csv (SUMMARY_168)", same_model_as = "", stringsAsFactors = FALSE)
  g <- grid(df = a$sensitivity_lag_dfs, L = a$sensitivity_lag_ends, mi = seq_len(nrow(mc)), oi = seq_len(nrow(oc))); b <- base(g$oi, g$mi)
  same <- ifelse(g$L == a$lag_end & g$df == a$lag_df, sprintf("SIW2.ARCH59.%s.%s.%s.L%02dDF%d", b$outcome_code, b$metric_code, a$tier, as.integer(a$lag_end), as.integer(a$lag_df)), "")
  R$lf <- data.frame(scope = "ARCHIVED_DLM_LAG_FORM_SENSITIVITY", b, model_tier = a$tier, window_days = NA_integer_, lag_end = as.integer(g$L), lag_df = as.integer(g$df), specification = paste0("L", g$L, "_DF", g$df),
    model_code = sprintf("SIW2.ARCHLF.%s.%s.%s.L%02dDF%d", b$outcome_code, b$metric_code, a$tier, as.integer(g$L), as.integer(g$df)), package_result_id = siw_dlm_id(b$outcome_code, b$metric_code, g$L, g$df, a$tier),
    declared_families = "ARCHIVED_DLM_LAG_FORM_q216", binary_glm_epsilon = ag$glm_epsilon, source_step = "ARCHIVED_DLM", source_file = "archived_model_level_comparison.csv (SUMMARY_216)", same_model_as = same, stringsAsFactors = FALSE)
  rec <- do.call(rbind, R); rownames(rec) <- NULL
  rec$binary_glm_epsilon[rec$outcome_family != "binomial"] <- NA_real_
  rec <- cbind(record_version = SIW_CODED_RECORD_VERSION, rec, stringsAsFactors = FALSE)
  rec$run_found <- FALSE; rec$run_status <- ""; rec$run_n <- NA_integer_; rec$run_families <- ""; rec$run_archived_validation_class <- ""
  if (!is.null(paper_dir)) {
    rd <- function(fn) { p <- file.path(paper_dir, fn); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")) else NULL }
    cr <- rd("cumulative_results.csv"); fs <- rd("dlm_final_summary.csv")
    for (t in list(list(cr, c("multiplicity_family_q10", "multiplicity_family_q80", "multiplicity_family_q90")), list(fs, "multiplicity_family_q9"))) {
      d <- t[[1L]]; if (is.null(d)) next
      j <- match(rec$package_result_id, d$result_id); i <- which(!is.na(j) & rec$source_step == "PAPER"); j <- j[i]
      rec$run_found[i] <- TRUE; rec$run_status[i] <- as.character(d$status[j]); rec$run_n[i] <- as.integer(d$n[j])
      rec$run_families[i] <- vapply(j, function(k) fam(vapply(t[[2L]], function(cn) { v <- d[[cn]][k]; if (is.null(v) || is.na(v)) "" else as.character(v) }, "")), "")
    }
  }
  if (!is.null(archived_dir)) {
    p <- file.path(archived_dir, "archived_model_level_comparison.csv")
    if (file.exists(p)) {
      ml <- utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
      k168 <- paste("SUMMARY_168", rec$outcome, rec$metric, rec$model_tier, sep = "|"); k216 <- paste("SUMMARY_216", rec$outcome, rec$metric, rec$specification, sep = "|")
      key <- ifelse(rec$scope == "ARCHIVED_DLM_L01_L59_GRID", k168, ifelse(rec$scope == "ARCHIVED_DLM_LAG_FORM_SENSITIVITY", k216, NA_character_))
      j <- match(key, paste(ml$comparison, ml$key, sep = "|")); i <- which(!is.na(j))
      rec$run_found[i] <- TRUE; rec$run_archived_validation_class[i] <- as.character(ml$worst_class[j[i]])
    }
  }
  fams <- unique(unlist(strsplit(rec$declared_families[nzchar(rec$declared_families)], ";", fixed = TRUE)))
  famtab <- data.frame(family = fams, scope = vapply(fams, function(x) rec$scope[grepl(x, rec$declared_families, fixed = TRUE)][1L], ""),
    declared_members = vapply(fams, function(x) sum(vapply(strsplit(rec$declared_families, ";", fixed = TRUE), function(z) x %in% z, logical(1))), 0L),
    members_in_run = vapply(fams, function(x) sum(vapply(strsplit(rec$run_families, ";", fixed = TRUE), function(z) x %in% z, logical(1))), 0L), stringsAsFactors = FALSE, row.names = NULL)
  famtab$definition <- vapply(famtab$family, function(x) {
    k <- if (grepl("_q10_", x)) "q10" else if (grepl("_q80_", x)) "q80" else if (grepl("_q90_", x)) "q90" else if (grepl("^DLM_FINAL.*q9$", x)) "q9" else if (grepl("q24$", x)) "q24" else if (grepl("q216$", x)) "q216" else ""
    d <- if (nzchar(k)) config$families[[k]] else NULL; if (is.null(d)) "archived L01-L59 M2-L global tests of the three representative metrics (archived q9)" else d }, "")
  list(record = rec, families = famtab)
}

#' Internal checks of a coded model record: counts, unique codes, declared family sizes.
#' @export
siw_coded_model_record_checks <- function(x, config = siw_load_config()) {
  r <- x$record; ft <- x$families; no <- nrow(config$outcomes); nm <- nrow(config$metrics); nr <- length(config$representative_metrics); a <- config$dlm$archived
  sz <- function(pat) ft$declared_members[grepl(pat, ft$family)]
  ck <- function(id, ok, detail) data.frame(check = id, status = if (isTRUE(ok)) "PASS" else "FAIL", detail = detail, stringsAsFactors = FALSE)
  rbind(
    ck("cumulative_count", sum(grepl("^CUMULATIVE", r$scope)) == no * nm * length(config$windows) * length(config$tier_order), paste0(sum(grepl("^CUMULATIVE", r$scope)), " rows")),
    ck("final_dlm_count", sum(r$scope == "FINAL_DLM_L01_L56") == no * nr, paste0(sum(r$scope == "FINAL_DLM_L01_L56"), " rows")),
    ck("archived_grid_count", sum(r$scope == "ARCHIVED_DLM_L01_L59_GRID") == no * nm * length(config$tier_order), paste0(sum(r$scope == "ARCHIVED_DLM_L01_L59_GRID"), " rows")),
    ck("archived_lag_form_count", sum(r$scope == "ARCHIVED_DLM_LAG_FORM_SENSITIVITY") == no * nm * length(a$sensitivity_lag_ends) * length(a$sensitivity_lag_dfs), paste0(sum(r$scope == "ARCHIVED_DLM_LAG_FORM_SENSITIVITY"), " rows")),
    ck("model_codes_unique", !anyDuplicated(r$model_code), "model_code unique"), ck("package_result_ids_unique", !anyDuplicated(paste(r$scope, r$package_result_id)), "package_result_id unique within scope"),
    ck("final_and_archived_codes_disjoint", !length(intersect(r$model_code[r$scope == "FINAL_DLM_L01_L56"], r$model_code[grepl("^ARCHIVED", r$scope)])), "final L01-L56 and archived codes never coincide"),
    ck("q10_family_sizes", all(sz("_q10_") == length(config$windows)) && length(sz("_q10_")) == no * nm, "each q10 family: 10 windows"),
    ck("q80_family_sizes", all(sz("_q80_") == nm * length(config$windows)) && length(sz("_q80_")) == no, "each q80 family: 80 tests"),
    ck("q90_family_size", identical(as.integer(sz("_q90_")), as.integer(no * nr * length(config$windows))), "q90: 90 tests"),
    ck("final_q9_family_size", identical(as.integer(sz("^DLM_FINAL_L01_L56_M2_L_q9$")), as.integer(no * nr)), "final q9: 9 tests"),
    ck("archived_q24_family_size", identical(as.integer(sz("q24$")), as.integer(no * nm)), "archived q24: 24 tests"),
    ck("archived_q9_family_size", identical(as.integer(sz("^ARCHIVED_L01_L59_M2_L_q9$")), as.integer(no * nr)), "archived q9: 9 tests"),
    ck("archived_q216_family_size", identical(as.integer(sz("q216$")), 216L), "q216: 216 tests"),
    ck("lag_form_L59_DF3_linked_to_grid", sum(nzchar(r$same_model_as)) == no * nm && all(r$same_model_as[nzchar(r$same_model_as)] %in% r$model_code), "the 24 L59_DF3 lag-form rows point to their grid model"),
    ck("in_run_families_match_declared", all(r$run_families[r$run_found & nzchar(r$run_families)] == r$declared_families[r$run_found & nzchar(r$run_families)]), "families found in the run equal the declared families (estimable models)"))
}

#' Write the coded model record (version 2) to a NEW or empty folder. Never writes or overwrites the legacy SIECC_All_Models_with_Codes.csv.
#' @export
siw_write_coded_model_record <- function(out_dir, config = siw_load_config(), paper_dir = NULL, archived_dir = NULL, run_label = "") {
  if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE))) stop("SIW_CODED_RECORD_OUTPUT_NOT_EMPTY: ", out_dir, call. = FALSE)
  x <- siw_coded_model_record(config, paper_dir, archived_dir); ck <- siw_coded_model_record_checks(x, config)
  files <- c(record = "SIECC_coded_model_record_v2.csv", families = "SIECC_coded_model_record_v2_families.csv", checks = "SIECC_coded_model_record_v2_checks.csv")
  if (any(tolower(files) == tolower(.SIW_LEGACY_RECORD_NAME))) stop("SIW_CODED_RECORD_WOULD_OVERWRITE_LEGACY", call. = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  siw_write_csv(x$record, file.path(out_dir, files[["record"]])); siw_write_csv(x$families, file.path(out_dir, files[["families"]])); siw_write_csv(ck, file.path(out_dir, files[["checks"]]))
  sm <- siw_source_manifest()
  meta <- list(record_version = SIW_CODED_RECORD_VERSION, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), run_label = run_label, config_sha256 = toupper(config$config_hash), source_fingerprint = toupper(sm$fingerprint),
    rows = nrow(x$record), rows_by_scope = as.list(table(x$record$scope)), families = nrow(x$families), rows_found_in_run = sum(x$record$run_found), checks_failed = sum(ck$status == "FAIL"),
    paper_run_folder = if (is.null(paper_dir)) NA_character_ else basename(paper_dir), archived_run_folder = if (is.null(archived_dir)) NA_character_ else basename(archived_dir),
    scope_covered = "cumulative grid (1,680), final L01-L56 DLMs (9), archived L01-L59 grid (168) and lag-form sensitivity (216)",
    scope_not_coded = "Batch A bivariate, Batch B sensitivity families, refined ordinal, Batch C bootstrap targets, cold burden, daily layer, ANOM and ALT: identified in their own run tables",
    legacy_record = paste0(.SIW_LEGACY_RECORD_NAME, " (37,348 rows) is a separate legacy file; it is never read, modified or replaced by this record"),
    code_format = "SIW2.<CUM|DLM56|ARCH59|ARCHLF>.<outcome code>.<metric code>.<tier>.<W<window>|L<lag end>DF<lag df>>; derived from the configuration only (stable across runs)")
  jsonlite::write_json(meta, file.path(out_dir, "SIECC_coded_model_record_v2_metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  writeLines(c(paste0("CODED_MODEL_RECORD_STATUS=", if (any(ck$status == "FAIL")) "FAIL" else "PASS"), paste0("RECORD_VERSION=", SIW_CODED_RECORD_VERSION), paste0("ROWS=", nrow(x$record)),
               paste0("ROWS_FOUND_IN_RUN=", sum(x$record$run_found)), paste0("CHECKS_FAILED=", sum(ck$status == "FAIL")), paste0("LEGACY_RECORD_UNTOUCHED=", .SIW_LEGACY_RECORD_NAME)), file.path(out_dir, "CODED_MODEL_RECORD_STATUS.txt"))
  siw_write_manifest(out_dir)
  invisible(list(out_dir = out_dir, record = x$record, families = x$families, checks = ck, status = if (any(ck$status == "FAIL")) "FAIL" else "PASS"))
}
