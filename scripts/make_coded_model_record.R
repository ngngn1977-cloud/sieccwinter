# Coded model record, version 2 (step 16 of run_all.ps1). Reads only aggregate run tables; writes a NEW folder; never touches the legacy SIECC_All_Models_with_Codes.csv.
# Usage: Rscript scripts/make_coded_model_record.R --out <new folder> [--paper-dir <paper run folder>] [--archived-dir <archived DLM validation run folder>] [--label <text>]
# Exit codes: 0 record written and every internal check PASS; 4 written but a check FAILED; 1 error (nothing concluded); 2 usage.
fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]); root <- normalizePath(file.path(dirname(fa), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
args <- commandArgs(trailingOnly = TRUE); opt <- function(f) { i <- match(f, args); if (is.na(i) || i == length(args)) NULL else args[i + 1L] }
out <- opt("--out"); if (is.null(out)) { cat("usage: Rscript scripts/make_coded_model_record.R --out <new folder> [--paper-dir DIR] [--archived-dir DIR] [--label TEXT]\n"); quit(status = 2L) }
outn <- normalizePath(out, winslash = "/", mustWork = FALSE)
if (startsWith(tolower(paste0(outn, "/")), tolower(paste0(root, "/")))) { cat("ERROR: the output folder must be outside the package folder\n"); quit(status = 2L) }
for (d in c(opt("--paper-dir"), opt("--archived-dir"))) if (!dir.exists(d)) { cat("ERROR: folder not found: ", d, "\n", sep = ""); quit(status = 2L) }
r <- tryCatch(siw_write_coded_model_record(out, siw_load_config(), opt("--paper-dir"), opt("--archived-dir"), run_label = opt("--label") %||% ""), error = function(e) { cat("ERROR: ", conditionMessage(e), "\n", sep = ""); NULL })
if (is.null(r)) quit(status = 1L)
cat(readLines(file.path(r$out_dir, "CODED_MODEL_RECORD_STATUS.txt")), sep = "\n")
print(r$checks[, c("check", "status")], row.names = FALSE)
cat("Output folder: ", r$out_dir, "\n", sep = "")
quit(status = if (identical(r$status, "PASS")) 0L else 4L)
