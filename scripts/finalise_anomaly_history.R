# Hand-off files of a Phase 3I anomaly / alternative-history run folder (called by run_anomaly_history_validation.ps1): SESSION_INFO.txt and RUN_MANIFEST.csv (SHA-256 of every file in the folder except itself, the finalise log and HANDOFF_OUTCOME.txt, which the wrapper writes AFTER the manifest and which is deliberately not listed, so that saving the R and wrapper outcomes never invalidates the manifest).
# Usage: Rscript scripts/finalise_dlm_validation.R <out_dir> <run_R_exit_code> <wall_seconds>
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), ".."), winslash = "/")
args <- commandArgs(TRUE)
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
if (length(args) < 3L) { cat("usage: finalise_dlm_validation.R <out_dir> <exit_code> <wall_seconds>\n"); quit(status = 2L) }
out <- normalizePath(args[1], winslash = "/"); wall <- suppressWarnings(as.numeric(args[3]))
src <- list.files(out, pattern = "^session_info[.]txt$", ignore.case = TRUE); txt <- if (length(src)) readLines(file.path(out, src[1L]), warn = FALSE) else utils::capture.output(print(utils::sessionInfo()))
unlink(file.path(out, src)); writeLines(c(txt, paste0("wall_clock_seconds_including_R_startup=", round(wall, 2))), file.path(out, "SESSION_INFO.txt"))
f <- sort(list.files(out, recursive = TRUE, full.names = FALSE)); f <- f[!(f %in% c("RUN_MANIFEST.csv", "finalise_console.log", "finalise_err.tmp", "HANDOFF_OUTCOME.txt")) & !grepl("[.](tmp|part|lock)$", f)]; h <- lapply(file.path(out, f), siw_file_hash)
siw_write_csv(data.frame(file = f, bytes = file.info(file.path(out, f))$size, algorithm = vapply(h, function(z) z$algorithm, ""), hash = vapply(h, function(z) z$value, ""), stringsAsFactors = FALSE), file.path(out, "RUN_MANIFEST.csv"))
cat("FINALISED\n"); quit(status = 0L)
