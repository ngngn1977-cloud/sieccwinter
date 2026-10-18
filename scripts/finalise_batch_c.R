# Hand-off files of a Batch C run folder (called by run_batch_c.ps1 after run.R batchc):
#   RUN_SUMMARY.md, RUN_MANIFEST.csv, SESSION_INFO.txt, VALIDATION_STATUS.csv, RUNTIME.csv
# Usage: Rscript scripts/finalise_batch_c.R <run_dir> <run_R_exit_code> <wall_seconds>   |   Rscript scripts/finalise_batch_c.R --verify <run_dir>
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), ".."), winslash = "/")
args <- commandArgs(TRUE)
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
# Not listed in RUN_MANIFEST.csv: checkpoints/ (target checkpoints; resumption only), the manifest itself, finalise_console.log, temporary files, hidden files.
.manifest_excluded <- function(f) grepl("^checkpoints/", f) | f %in% c("RUN_MANIFEST.csv", "finalise_console.log", "finalise_err.tmp") | grepl("[.](tmp|lock|part)$", f, ignore.case = TRUE) | grepl("~$", f) | grepl("(^|/)[.]", f)
.verify_manifest <- function(out) {
  mp <- file.path(out, "RUN_MANIFEST.csv"); if (!file.exists(mp)) return("RUN_MANIFEST.csv is missing")
  m <- utils::read.csv(mp, stringsAsFactors = FALSE); bad <- character()
  for (i in seq_len(nrow(m))) { p <- file.path(out, m$file[i])
    if (!file.exists(p)) { bad <- c(bad, paste0(m$file[i], ": listed but missing")); next }
    if (file.info(p)$size != m$bytes[i]) { bad <- c(bad, paste0(m$file[i], ": size differs")); next }
    h <- siw_file_hash(p); if (!identical(toupper(h$value), toupper(m$hash[i])) || !identical(h$algorithm, m$algorithm[i])) bad <- c(bad, paste0(m$file[i], ": hash differs")) }
  bad
}
if (length(args) == 2L && args[1] == "--verify") { bad <- .verify_manifest(normalizePath(args[2], winslash = "/")); if (length(bad)) { cat("MANIFEST_VERIFY=FAIL\n", paste0("  ", bad, "\n"), sep = ""); quit(status = 1L) }; cat("MANIFEST_VERIFY=OK\n"); quit(status = 0L) }
if (length(args) < 3L) { cat("usage: finalise_batch_c.R <run_dir> <exit_code> <wall_seconds>\n"); quit(status = 2L) }
out <- normalizePath(args[1], winslash = "/"); rc <- suppressWarnings(as.integer(args[2])); wall <- suppressWarnings(as.numeric(args[3]))
rd <- function(f) { p <- file.path(out, f); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL }
stat <- if (file.exists(file.path(out, "BATCH_C_STATUS.txt"))) { l <- readLines(file.path(out, "BATCH_C_STATUS.txt")); l <- l[grepl("^[A-Z_0-9]+=", l)]; kv <- strsplit(l, "="); stats::setNames(vapply(kv, function(z) z[2L], ""), vapply(kv, function(z) z[1L], "")) } else character()
g <- function(k) if (k %in% names(stat)) stat[[k]] else "FAIL"
exec <- if (rc %in% c(0L, 4L)) g("EXECUTION_STATUS") else "FAIL"
meta <- if (file.exists(file.path(out, "batch_c_metadata.json"))) jsonlite::fromJSON(file.path(out, "batch_c_metadata.json"), simplifyVector = FALSE) else list()
src <- list.files(out, pattern = "^session_info[.]txt$", ignore.case = TRUE)
txt <- if (length(src)) readLines(file.path(out, src[1L]), warn = FALSE) else utils::capture.output(print(utils::sessionInfo()))
if (!any(nzchar(txt))) { cat("FINALISE_ERROR: empty session information\n"); quit(status = 1L) }
tmp <- file.path(out, "SESSION_INFO.txt.tmp"); writeLines(txt, tmp); unlink(file.path(out, src)); unlink(file.path(out, "SESSION_INFO.txt")); invisible(file.rename(tmp, file.path(out, "SESSION_INFO.txt")))
tm <- rd("batch_c_timings.csv"); rt <- data.frame(item = "wall_clock_seconds_including_R_startup", seconds = round(wall, 2), stringsAsFactors = FALSE)
if (!is.null(tm)) rt <- rbind(rt, data.frame(item = paste0("stage_", tm$stage), seconds = tm$seconds, stringsAsFactors = FALSE)); siw_write_csv(rt, file.path(out, "RUNTIME.csv"))
VS <- data.frame(component = c("EXECUTION", "INPUT_CHECKS", "NUMERICAL_REPRODUCTION", "MATHEMATICAL_VERIFICATION", "SCIENTIFIC_VALIDITY", "SOURCE_INTEGRITY"),
  status = c(exec, g("INPUT_CHECK_STATUS"), g("NUMERICAL_REPRODUCTION_STATUS"), g("MATHEMATICAL_VERIFICATION_STATUS"), g("SCIENTIFIC_VALIDITY_STATUS"), g("SOURCE_INTEGRITY_STATUS")),
  detail = c(paste0("run.R exit code ", rc), "input validation (no FAIL)", "bootstrap summaries vs the frozen references (strict validator, unchanged tolerances)", "independent checks of sampling, fits on identical resamples, percentiles, seeds, conservation, determinism",
             "descriptive, declared thresholds; REVIEW never changes a result", "source files unchanged during the run; fingerprint recorded"), stringsAsFactors = FALSE)
siw_write_csv(VS, file.path(out, "VALIDATION_STATUS.csv"))
overall <- if (all(VS$status[c(1, 2, 3, 4, 6)] == "PASS")) "PASS" else "FAIL"
vs <- rd("batch_c_validation_summary.csv"); ms <- rd("bootstrap_math_verification.csv")
tb <- function(df) if (is.null(df) || !nrow(df)) "_none_" else paste(c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"), apply(df, 1L, function(r) paste0("| ", paste(gsub("[|]", "/", substr(as.character(r), 1, 140)), collapse = " | "), " |"))), collapse = "\n")
L <- c(paste0("# Batch C run summary: ", overall), "", paste0("* Run folder: `", out, "`"), paste0("* Finished (UTC): ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("* Package version: ", meta$package_version %||% "unknown", "; ", meta$r_version %||% R.version.string), paste0("* Input hash (", meta$input_hash_algorithm %||% "n/a", "): `", meta$input_hash %||% "n/a", "`; participants (rows): ", meta$n_participants %||% "n/a", "; assessment-date groups: ", meta$assessment_date_groups %||% "n/a"),
  paste0("* Configuration hash: `", meta$config_hash %||% "n/a", "`; protocol hash: `", meta$protocol_hash %||% "n/a", "`; Batch C plan hash: `", meta$batch_c_plan_hash %||% "n/a", "`"),
  paste0("* **Package-source fingerprint (SHA-256, ", meta$source_files %||% "n/a", " files, this run only):** `", meta$source_fingerprint_sha256 %||% "n/a", "`; source changed during the run: ", meta$source_changed_during_run %||% "n/a"),
  paste0("* Replicates requested per target: ", meta$replicates_requested %||% "n/a", "; targets: ", meta$targets %||% "n/a"), paste0("* Wall clock: ", round(wall, 1), " s"), "",
  "## Statuses (separate)", "", paste0("* EXECUTION: ", exec), paste0("* INPUT CHECKS: ", g("INPUT_CHECK_STATUS")), paste0("* NUMERICAL REPRODUCTION: ", g("NUMERICAL_REPRODUCTION_STATUS")), paste0("* MATHEMATICAL VERIFICATION: ", g("MATHEMATICAL_VERIFICATION_STATUS")),
  paste0("* SCIENTIFIC VALIDITY (descriptive): ", g("SCIENTIFIC_VALIDITY_STATUS")), paste0("* SOURCE INTEGRITY: ", g("SOURCE_INTEGRITY_STATUS")), "", "## Comparison with the frozen references", "", tb(vs[, intersect(c("reference", "status", "n_compared", "n_exact", "n_within_tolerance", "n_different", "n_unmatched", "n_duplicate", "n_missing_required", "n_records_reference", "n_records_results", "required"), names(vs)), drop = FALSE]), "",
  "## Mathematical verification", "", tb(ms[, intersect(c("check", "n_targets", "n_pass", "n_fail", "status"), names(ms)), drop = FALSE]), "", "## Runtime", "", tb(rt), "",
  "## Notes", "", "* Results were not altered, redrawn or replaced to fit any reference; failed refits are counted and classified (bootstrap_failures.csv), never redrawn.",
  "* SCIENTIFIC VALIDITY is descriptive (declared thresholds in config/bootstrap_protocol.json) and is assessed separately from agreement with the frozen results.",
  "* The source fingerprint identifies the package source of this run only (scope in SOURCE_FINGERPRINT.txt, files in source_manifest.csv); it is not assigned to earlier runs.", "* Nothing in this folder contains participant-level values.")
writeLines(L, file.path(out, "RUN_SUMMARY.md"))
f <- sort(list.files(out, recursive = TRUE, full.names = FALSE)); f <- f[!.manifest_excluded(f)]; h <- lapply(file.path(out, f), siw_file_hash)
siw_write_csv(data.frame(file = f, bytes = file.info(file.path(out, f))$size, algorithm = vapply(h, function(z) z$algorithm, ""), hash = vapply(h, function(z) z$value, ""), stringsAsFactors = FALSE), file.path(out, "RUN_MANIFEST.csv"))
bad <- .verify_manifest(out); if (length(bad)) { cat("FINALISE_ERROR: manifest inconsistent\n", paste0("  ", bad, "\n"), sep = ""); quit(status = 1L) }
cat("FINAL_STATUS=", overall, "\n", sep = ""); quit(status = if (overall == "PASS") 0L else if (exec != "PASS") 1L else 4L)
