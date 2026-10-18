# Writes the standard hand-off files of a run folder (called by run_demo.ps1, run_paper.ps1, run_validate.ps1 after run.R):
#   RUN_SUMMARY.md, RUN_MANIFEST.csv, SESSION_INFO.txt, INPUT_VALIDATION.csv, VALIDATION_STATUS.csv, RUNTIME.csv
# Usage: Rscript scripts/finalise_run.R <run_dir> <demo|paper|validate> <run_R_exit_code> <wall_seconds>
# Aggregate information only: no participant code or row value is read into, or written by, this script.
fa <- grep("--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("--file=", "", fa[1])), ".."), winslash = "/")
args <- commandArgs(TRUE)
# Files that are NOT listed in RUN_MANIFEST.csv because they are transient, open, or written after the manifest:
#   checkpoints/        aggregate stage caches (not deliverables)
#   RUN_MANIFEST.csv    the manifest itself
#   finalise_console.log, finalise_err.tmp   console capture of THIS script, appended to / deleted by the wrapper after it exits
#   *.tmp, *.lock, *.part, *~, .~lock*, hidden dot-files   temporary, lock or working files
.manifest_excluded <- function(f) grepl("^checkpoints/", f) | f %in% c("RUN_MANIFEST.csv", "finalise_console.log", "finalise_err.tmp") |
  grepl("\\.(tmp|lock|part)$", f, ignore.case = TRUE) | grepl("~$", f) | grepl("(^|/)\\.", f)
# Verify RUN_MANIFEST.csv: every listed file exists and matches its stored size and hash. Returns a character vector of problems (empty = OK).
.verify_manifest <- function(out) {
  mp <- file.path(out, "RUN_MANIFEST.csv"); if (!file.exists(mp)) return("RUN_MANIFEST.csv is missing")
  m <- utils::read.csv(mp, stringsAsFactors = FALSE); bad <- character()
  for (i in seq_len(nrow(m))) {
    p <- file.path(out, m$file[i])
    if (!file.exists(p)) { bad <- c(bad, paste0(m$file[i], ": listed but missing")); next }
    if (file.info(p)$size != m$bytes[i]) { bad <- c(bad, paste0(m$file[i], ": size differs")); next }
    h <- siw_file_hash(p)
    if (!identical(toupper(h$value), toupper(m$hash[i])) || !identical(h$algorithm, m$algorithm[i])) bad <- c(bad, paste0(m$file[i], ": hash differs"))
  }
  bad
}
if (length(args) == 2L && args[1] == "--verify") {   # Usage: finalise_run.R --verify <run_dir>
  source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
  bad <- .verify_manifest(normalizePath(args[2], winslash = "/")); if (length(bad)) { cat("MANIFEST_VERIFY=FAIL\n", paste0("  ", bad, "\n"), sep = ""); quit(status = 1L) }
  cat("MANIFEST_VERIFY=OK\n"); quit(status = 0L)
}
if (length(args) < 4L) { cat("usage: finalise_run.R <run_dir> <mode> <exit_code> <wall_seconds>\n       finalise_run.R --verify <run_dir>\n"); quit(status = 2L) }
out <- normalizePath(args[1], winslash = "/"); mode <- args[2]; rc <- suppressWarnings(as.integer(args[3])); wall <- suppressWarnings(as.numeric(args[4]))
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
rd <- function(f) { p <- file.path(out, f); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL }
meta <- if (file.exists(file.path(out, "run_metadata.json"))) jsonlite::fromJSON(file.path(out, "run_metadata.json"), simplifyVector = FALSE) else list()
iv <- rd("input_validation_report.csv"); sc <- rd("self_checks.csv"); tm <- rd("timings.csv"); vs <- rd("validation_summary.csv")
ran <- length(meta) > 0L

# ---- copies under the standard hand-off names ----
if (!is.null(iv)) siw_write_csv(iv, file.path(out, "INPUT_VALIDATION.csv"))
# Session information: ONE canonical delivered file, SESSION_INFO.txt. Windows file names are case-insensitive, so session_info.txt (written by the
# analytical run) and SESSION_INFO.txt are the SAME file: never copy one onto the other. Read the content first, write it to a temporary name,
# remove any case-variant of the name, then rename the temporary file (rename is atomic on the same volume).
.finalise_session_info <- function(out) {
  src <- list.files(out, pattern = "^session_info[.]txt$", ignore.case = TRUE)
  txt <- if (length(src)) {
    s <- readLines(file.path(out, src[1L]), warn = FALSE)
    if (!any(nzchar(s))) stop("SESSION_INFO source file is empty; refusing to deliver an empty file", call. = FALSE)
    s
  } else utils::capture.output(print(utils::sessionInfo()))
  tmp <- file.path(out, "SESSION_INFO.txt.tmp"); writeLines(txt, tmp)
  unlink(file.path(out, src)); unlink(file.path(out, "SESSION_INFO.txt"))
  if (!file.rename(tmp, file.path(out, "SESSION_INFO.txt"))) stop("could not create SESSION_INFO.txt", call. = FALSE)
  chk <- file.path(out, "SESSION_INFO.txt"); got <- if (file.exists(chk)) readLines(chk, warn = FALSE) else character()
  if (!file.exists(chk) || file.info(chk)$size == 0L) stop("SESSION_INFO.txt is absent or empty", call. = FALSE)
  if (!any(grepl("^R version ", got)) || !any(grepl("attached base packages", got)) || !any(grepl("loaded via a namespace", got)))
    stop("SESSION_INFO.txt lacks R version or package/library version sections", call. = FALSE)
  invisible(chk)
}
.si <- tryCatch(.finalise_session_info(out), error = function(e) { cat("FINALISE_ERROR: ", conditionMessage(e), "\n", sep = ""); quit(status = 1L) })
rt <- data.frame(item = "wall_clock_seconds_including_R_startup", seconds = round(wall, 2), stringsAsFactors = FALSE)
if (!is.null(tm)) rt <- rbind(rt, data.frame(item = paste0("stage_", tm$stage), seconds = tm$seconds, stringsAsFactors = FALSE))
siw_write_csv(rt, file.path(out, "RUNTIME.csv"))

# ---- status ----
comp <- list(); add <- function(component, status, detail) comp[[length(comp) + 1L]] <<- data.frame(component = component, status = status, detail = detail, stringsAsFactors = FALSE)
add("run_exit_code", if (identical(rc, 0L)) "PASS" else "FAIL", paste0("run.R exit code ", rc))
if (!is.null(iv)) add("input_validation", if (any(iv$status == "FAIL")) "FAIL" else if (any(iv$status == "WARN")) "WARN" else "PASS",
  paste0(sum(iv$status == "PASS"), " PASS, ", sum(iv$status == "WARN"), " WARN, ", sum(iv$status == "FAIL"), " FAIL")) else add("input_validation", "FAIL", "no input validation report produced")
if (!is.null(sc)) add("self_checks", if (any(sc$status == "FAIL")) "FAIL" else "PASS", paste0(sum(sc$status == "PASS"), " PASS, ", sum(sc$status == "FAIL"), " FAIL")) else add("self_checks", "FAIL", "no self-check table produced")
if (mode == "validate") {
  if (is.null(vs)) add("reference_comparison", "FAIL", "no validation summary produced") else {
    for (i in seq_len(nrow(vs))) add(paste0("reference:", vs$reference[i]), if (grepl("^PASS", vs$status[i])) "PASS" else if (vs$status[i] == "NOT_TESTED") "NOT_TESTED" else "FAIL",
      paste0(vs$status[i], "; compared=", vs$n_compared[i], "; different=", vs$n_different[i], "; unmatched=", vs$n_unmatched[i], "; nonfinite=", vs$n_nonfinite[i], "; file=", vs$reference_file[i]))
    if (any(vs$status == "NOT_TESTED") && !all(vs$status == "NOT_TESTED")) add("reference_coverage", "WARN", paste0(sum(vs$status == "NOT_TESTED"), " of ", nrow(vs), " declared references had no reference file and were NOT compared"))
    if (all(vs$status == "NOT_TESTED")) add("reference_comparison", "FAIL", "no reference file was found, so nothing was compared")
  }
}
VS <- do.call(rbind, comp); siw_write_csv(VS, file.path(out, "VALIDATION_STATUS.csv"))
overall <- if (!any(VS$status == "FAIL")) "PASS" else "FAIL"   # in validate mode NOT_TESTED references are listed as such; if none was compared the run FAILs

# ---- summary ----
tb <- function(df) if (is.null(df) || !nrow(df)) "_none_" else paste(c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
  apply(df, 1L, function(r) paste0("| ", paste(gsub("\\|", "/", as.character(r)), collapse = " | "), " |"))), collapse = "\n")
key <- c("cumulative_results.csv", "complete_case_accounting.csv", "dlm_final_summary.csv", "dlm_final_lag_curves.csv", "dlm_final_cumulative_curves.csv", "rainfall_anomaly_report.csv", "validation_summary.csv", "validation_detail.csv", "run_metadata.json", "run.log", "run_console.log")
L <- c(paste0("# Run summary: ", overall), "",
  paste0("* Mode: `", mode, "`"), paste0("* Run folder: `", out, "`"),
  paste0("* Finished (UTC): ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("* Package version: ", meta$package_version %||% "unknown", "; ", meta$r_version %||% R.version.string),
  paste0("* Input label: ", meta$input_label %||% "n/a", "; participants (rows): ", meta$n_participants %||% "n/a"),
  paste0("* Input hash (", meta$input_hash_algorithm %||% "n/a", "): `", meta$input_hash %||% "n/a", "`"),
  paste0("* Configuration hash: `", meta$config_hash %||% "n/a", "`"),
  paste0("* Cumulative fields: ", meta$cumulative_source %||% "n/a"),
  paste0("* Seed: ", meta$seed %||% "n/a", "; final-DLM draws: ", meta$dlm_final$draws %||% "n/a", "; MVN method: ", meta$dlm_final$mvn_method %||% "n/a"),
  paste0("* Wall clock: ", round(wall, 1), " s"), "",
  "## Status components", "", tb(VS), "",
  "## Input validation", "", tb(iv[, intersect(c("check_id", "status", "n_affected", "detail"), names(iv)), drop = FALSE]), "",
  "## Runtime", "", tb(rt), "",
  if (mode == "validate" && !is.null(vs)) c("## Reference comparison", "", tb(vs), "") else NULL,
  "## Files", "", paste0("* ", key[file.exists(file.path(out, key))]), "",
  "## Notes", "",
  "* WARN items in input validation are reported, never corrected; review them.",
  if (mode == "demo") "* Synthetic data test the workflow only; the estimates carry no scientific meaning and cannot reproduce the article." else
    "* Nothing in this folder contains participant-level values. Do not add any before sending files for review.")
writeLines(L, file.path(out, "RUN_SUMMARY.md"))

# ---- manifest (hash of every final artifact, after all files above exist) ----
f <- sort(list.files(out, recursive = TRUE, full.names = FALSE)); f <- f[!.manifest_excluded(f)]
h <- lapply(file.path(out, f), siw_file_hash)
siw_write_csv(data.frame(file = f, bytes = file.info(file.path(out, f))$size, algorithm = vapply(h, function(z) z$algorithm, ""), hash = vapply(h, function(z) z$value, ""), stringsAsFactors = FALSE), file.path(out, "RUN_MANIFEST.csv"))
.bad <- .verify_manifest(out)   # immediate post-generation verification
if (length(.bad)) { cat("FINALISE_ERROR: manifest inconsistent\n", paste0("  ", .bad, "\n"), sep = ""); quit(status = 1L) }
cat("FINAL_STATUS=", overall, "\n", sep = "")
quit(status = if (overall == "PASS") 0L else 1L)
