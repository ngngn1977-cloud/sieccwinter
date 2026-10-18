# Hand-off files of a Batch A run folder (called by run_batch_a.ps1 after run.R batcha):
#   RUN_SUMMARY.md, RUN_MANIFEST.csv, SESSION_INFO.txt, INPUT_VALIDATION.csv, VALIDATION_STATUS.csv, RUNTIME.csv
# Usage: Rscript scripts/finalise_batch_a.R <run_dir> <run_R_exit_code> <wall_seconds>      |      Rscript scripts/finalise_batch_a.R --verify <run_dir>
# Execution status, input-check status and the core / Batch A comparison statuses are reported separately. Aggregate information only.
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), ".."), winslash = "/")
args <- commandArgs(TRUE)
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
# Not listed in RUN_MANIFEST.csv: checkpoints/, the manifest itself, finalise_console.log, finalise_err.tmp, *.tmp, *.lock, *.part, files ending in ~, hidden files.
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
if (length(args) < 3L) { cat("usage: finalise_batch_a.R <run_dir> <exit_code> <wall_seconds>\n"); quit(status = 2L) }
out <- normalizePath(args[1], winslash = "/"); rc <- suppressWarnings(as.integer(args[2])); wall <- suppressWarnings(as.numeric(args[3]))
rd <- function(f) { p <- file.path(out, f); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL }
stat <- if (file.exists(file.path(out, "BATCH_A_STATUS.txt"))) { l <- readLines(file.path(out, "BATCH_A_STATUS.txt")); kv <- strsplit(l[grepl("=", l)], "="); stats::setNames(vapply(kv, function(z) z[2L], ""), vapply(kv, function(z) z[1L], "")) } else character()
g <- function(k) if (k %in% names(stat)) stat[[k]] else "FAIL"
exec <- if (identical(rc, 0L) || identical(rc, 4L)) g("EXECUTION_STATUS") else "FAIL"
iv <- rd("input_validation_report.csv"); bi <- rd("batch_a_input_checks.csv"); cs <- rd("validation_summary.csv"); bs <- rd("batch_a_validation_summary.csv"); sc <- rd("batch_a_self_checks.csv"); csc <- rd("self_checks.csv")
meta <- if (file.exists(file.path(out, "run_metadata.json"))) jsonlite::fromJSON(file.path(out, "run_metadata.json"), simplifyVector = FALSE) else list()
bmeta <- if (file.exists(file.path(out, "batch_a_metadata.json"))) jsonlite::fromJSON(file.path(out, "batch_a_metadata.json"), simplifyVector = FALSE) else list()

# --- canonical hand-off copies (session information: read first, write a temporary name, remove case variants, rename) ---
inp <- do.call(rbind, list(if (!is.null(iv)) iv, if (!is.null(bi)) bi)); if (!is.null(inp)) siw_write_csv(inp, file.path(out, "INPUT_VALIDATION.csv"))
src <- list.files(out, pattern = "^session_info[.]txt$", ignore.case = TRUE)
txt <- if (length(src)) readLines(file.path(out, src[1L]), warn = FALSE) else utils::capture.output(print(utils::sessionInfo()))
if (!any(nzchar(txt))) { cat("FINALISE_ERROR: empty session information\n"); quit(status = 1L) }
tmp <- file.path(out, "SESSION_INFO.txt.tmp"); writeLines(txt, tmp); unlink(file.path(out, src)); unlink(file.path(out, "SESSION_INFO.txt")); file.rename(tmp, file.path(out, "SESSION_INFO.txt"))
tm1 <- rd("timings.csv"); tm2 <- rd("batch_a_timings.csv")
rt <- data.frame(item = "wall_clock_seconds_including_R_startup", seconds = round(wall, 2), stringsAsFactors = FALSE)
if (!is.null(tm1)) rt <- rbind(rt, data.frame(item = paste0("core_stage_", tm1$stage), seconds = tm1$seconds, stringsAsFactors = FALSE))
if (!is.null(tm2)) rt <- rbind(rt, data.frame(item = paste0("batch_a_stage_", tm2$stage), seconds = tm2$seconds, stringsAsFactors = FALSE))
siw_write_csv(rt, file.path(out, "RUNTIME.csv"))

# --- statuses ---
comp <- list(); add <- function(component, status, detail) comp[[length(comp) + 1L]] <<- data.frame(component = component, status = status, detail = detail, stringsAsFactors = FALSE)
add("EXECUTION", exec, paste0("run.R exit code ", rc, "; core self-checks ", if (!is.null(csc)) sum(csc$status == "FAIL") else NA, " FAIL; Batch A self-checks ", if (!is.null(sc)) sum(sc$status == "FAIL") else NA, " FAIL"))
add("INPUT_CHECKS", g("INPUT_CHECK_STATUS"), paste0(if (!is.null(inp)) paste0(sum(inp$status == "PASS"), " PASS, ", sum(inp$status == "WARN"), " WARN, ", sum(inp$status == "FAIL"), " FAIL") else "no input report"))
refrow <- function(prefix, s) if (is.null(s)) add(paste0(prefix, ":summary"), "FAIL", "no validation summary produced") else for (i in seq_len(nrow(s)))
  add(paste0(prefix, ":", s$reference[i]), if (grepl("^PASS", s$status[i])) "PASS" else if (s$status[i] == "NOT_TESTED") "NOT_TESTED" else "FAIL",
      paste0(s$status[i], "; compared=", s$n_compared[i], "; exact=", s$n_exact[i], "; within=", s$n_within_tolerance[i], "; different=", s$n_different[i], "; unmatched=", s$n_unmatched[i], "; nonfinite=", s$n_nonfinite[i], "; duplicates=", s$n_duplicate[i], "; missing_required=", s$n_missing_required[i], "; file=", s$reference_file[i], if (!is.na(s$reason[i]) && nzchar(s$reason[i])) paste0("; ", s$reason[i]) else ""))
refrow("CORE", cs); add("CORE_COMPARISON", g("CORE_COMPARISON_STATUS"), "all required core references PASS (exact or within tolerance)")
refrow("BATCH_A", bs); add("BATCH_A_COMPARISON", g("BATCH_A_COMPARISON_STATUS"), "all required Batch A references PASS (exact or within tolerance)")
VS <- do.call(rbind, comp); siw_write_csv(VS, file.path(out, "VALIDATION_STATUS.csv"))
overall <- if (!any(VS$status[VS$component %in% c("EXECUTION", "INPUT_CHECKS", "CORE_COMPARISON", "BATCH_A_COMPARISON")] != "PASS") && !any(VS$status == "FAIL")) "PASS" else "FAIL"

# --- summary ---
tb <- function(df) if (is.null(df) || !nrow(df)) "_none_" else paste(c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
  apply(df, 1L, function(r) paste0("| ", paste(gsub("[|]", "/", as.character(r)), collapse = " | "), " |"))), collapse = "\n")
L <- c(paste0("# Batch A run summary: ", overall), "",
  paste0("* Run folder: `", out, "`"), paste0("* Finished (UTC): ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("* Package version: ", meta$package_version %||% "unknown", "; ", meta$r_version %||% R.version.string),
  paste0("* Input label: ", meta$input_label %||% "n/a", "; participants (rows): ", meta$n_participants %||% "n/a"),
  paste0("* Input hash (", meta$input_hash_algorithm %||% "n/a", "): `", meta$input_hash %||% "n/a", "`"), paste0("* Configuration hash: `", meta$config_hash %||% "n/a", "`"),
  paste0("* Core plan hash: `", bmeta$core_plan_hash %||% "n/a", "`; Batch A plan hash: `", bmeta$batch_a_plan_hash %||% "n/a", "`"),
  paste0("* Expected assessment-day groups: ", bmeta$expected_assessment_day_groups %||% "not set"), paste0("* Wall clock: ", round(wall, 1), " s"), "",
  "## Statuses (reported separately)", "", paste0("* EXECUTION: ", g("EXECUTION_STATUS")), paste0("* INPUT CHECKS: ", g("INPUT_CHECK_STATUS")), paste0("* CORE NUMERICAL COMPARISON: ", g("CORE_COMPARISON_STATUS")), paste0("* BATCH A NUMERICAL COMPARISON: ", g("BATCH_A_COMPARISON_STATUS")), "",
  "## Status components", "", tb(VS), "", "## Input checks", "", tb(inp[, intersect(c("check_id", "status", "n_affected", "detail"), names(inp)), drop = FALSE]), "", "## Runtime", "", tb(rt), "",
  "## Notes", "", "* A FAIL in a comparison means the numerical comparison failed or was incomplete; results are never altered to fit a reference. See validation_detail.csv and batch_a_validation_detail.csv.",
  "* Items without an authoritative frozen reference (e.g. SI Table S2 rows beyond the original cohort table, participant-day weather distributions) are produced but NOT TESTED; see the Batch A reference bundle's BATCHA_NOT_TESTED.csv.",
  "* Nothing in this folder contains participant-level values.")
writeLines(L, file.path(out, "RUN_SUMMARY.md"))
f <- sort(list.files(out, recursive = TRUE, full.names = FALSE)); f <- f[!.manifest_excluded(f)]; h <- lapply(file.path(out, f), siw_file_hash)
siw_write_csv(data.frame(file = f, bytes = file.info(file.path(out, f))$size, algorithm = vapply(h, function(z) z$algorithm, ""), hash = vapply(h, function(z) z$value, ""), stringsAsFactors = FALSE), file.path(out, "RUN_MANIFEST.csv"))
bad <- .verify_manifest(out); if (length(bad)) { cat("FINALISE_ERROR: manifest inconsistent\n", paste0("  ", bad, "\n"), sep = ""); quit(status = 1L) }
cat("FINAL_STATUS=", overall, "\n", sep = ""); quit(status = if (overall == "PASS") 0L else if (exec != "PASS") 1L else 4L)
