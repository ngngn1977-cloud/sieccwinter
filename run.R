#!/usr/bin/env Rscript
# sieccwinter entry point.
#   Rscript run.R demo [--out DIR] [--archived]
#   Rscript run.R paper path/to/study_data.csv [--out DIR] [--no-resume] [--archived] [--rainfall-sensitivity]
#   Rscript run.R validate path/to/study_data.csv path/to/reference_folder [--out DIR] [--no-resume]
#   Rscript run.R batcha path/to/study_data.csv --core-reference DIR --batch-reference DIR [--expected-assessment-days N] [--out DIR]
#   Rscript run.R batchc path/to/study_data.csv --batch-c-reference DIR [--accepted-run DIR] [--resume] [--out DIR]
#   Rscript run.R bootdiag path/to/batchc_run_folder [--out DIR]     (diagnostic audit of the saved Batch C checkpoints; reads no participant data)
#   Rscript run.R dlmvalidate path/to/study_data.csv --dlm-reference DIR [--accepted-run DIR] [--no-mc] [--out DIR]
#   Rscript run.R dlmvalidatev2 path/to/study_data.csv --dlm-reference DIR [--accepted-run DIR] [--no-mc] [--out DIR]   (V2 candidate)
#   Rscript run.R dlmv21cov path/to/dlmvalidatev2_run_folder [--out DIR]   (V2.1 candidate covariance check; reads aggregate files only)
#   Rscript run.R dlmarchived path/to/study_data.csv --reference-dir DIR [--out DIR]   (archived L01-L59 grid and lag-form sensitivity validation against the frozen V1.4 outputs)
#   Rscript run.R coldburden path/to/study_data.csv --reference-dir DIR [--out DIR]   (cold-burden scan: reproduction against the frozen V1.4 output and mathematical verification)
#   Rscript run.R dailylayer path/to/study_data.csv --reference-dir DIR [--out DIR]   (archived daily-lag layer: XYD, DAY, MAXT)
#   Rscript run.R anomhist path/to/study_data.csv --reference-dir DIR [--out DIR]   (exploratory anomaly ANOM 7,080 and alternative-history ALT 312 families)
#   Rscript run.R posthoc <item> path/to/study_data.csv [--v14-reference DIR] [--archive-reference DIR] [--core-reference DIR] [--nearest-histories FILE[|FILE]] [--gis-reference DIR|FILE[|FILE]] [--synthetic-demo] [--out DIR]
#       (labelled SENSITIVITY / POST_HOC / RECONSTRUCTION steps of config/posthoc_sensitivity_protocol_v2.json; exit 0 PASS, 3 REVIEW, 4 FAIL)
#   Rscript run.R dlmsimdiag path/to/study_data.csv --validation-run DIR [--dlm-reference DIR] [--out DIR]   (diagnostic only)
#   Rscript run.R ordinalrefine path/to/study_data.csv --batch-b-reference DIR [--legacy-run DIR] [--out DIR]
#   Rscript run.R batchb path/to/study_data.csv --core-reference DIR --batch-a-reference DIR --batch-b-reference DIR [--expected-assessment-days N] [--out DIR]
# No SPSS file is ever read. Participant rows are never printed or written to logs or aggregate outputs.
args <- commandArgs(trailingOnly = TRUE)
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- if (length(script_arg)) normalizePath(dirname(script_arg[1L]), winslash = "/") else normalizePath(getwd(), winslash = "/")
if (requireNamespace("sieccwinter", quietly = TRUE)) {
  options(sieccwinter.root = root); suppressMessages(library(sieccwinter))
} else {
  source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
}

opt <- function(flag, default = NULL) { i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[i + 1L] }
flag <- function(f) f %in% args
usage <- function() { cat("Usage:\n  Rscript run.R demo [--out DIR] [--archived]\n  Rscript run.R paper <study_data.csv> [--out DIR] [--no-resume] [--archived] [--rainfall-sensitivity]\n  Rscript run.R validate <study_data.csv> <reference_folder> [--out DIR] [--no-resume]\n  Rscript run.R batcha <study_data.csv> --core-reference DIR --batch-reference DIR [--expected-assessment-days N] [--out DIR]\n  Rscript run.R batchb <study_data.csv> --core-reference DIR --batch-a-reference DIR --batch-b-reference DIR [--expected-assessment-days N] [--out DIR]\n  Rscript run.R ordinalrefine <study_data.csv> --batch-b-reference DIR [--legacy-run DIR] [--out DIR]\n  Rscript run.R batchc <study_data.csv> --batch-c-reference DIR [--accepted-run DIR] [--resume] [--out DIR]\n  Rscript run.R bootdiag <batchc_run_folder> [--out DIR]\n  Rscript run.R dlmvalidate <study_data.csv> --dlm-reference DIR [--accepted-run DIR] [--no-mc] [--out DIR]\n  Rscript run.R dlmsimdiag <study_data.csv> --validation-run DIR [--dlm-reference DIR] [--out DIR]\n  Rscript run.R dlmvalidatev2 <study_data.csv> --dlm-reference DIR [--accepted-run DIR] [--no-mc] [--out DIR]\n  Rscript run.R dlmv21cov <dlmvalidatev2_run_folder> [--out DIR]\n  Rscript run.R dlmarchived <study_data.csv> --reference-dir DIR [--out DIR]\n  Rscript run.R coldburden <study_data.csv> --reference-dir DIR [--out DIR]\n  Rscript run.R dailylayer <study_data.csv> --reference-dir DIR [--out DIR]\n"); quit(status = 2L) }
mode <- if (length(args)) args[1L] else usage()
positional <- character(); skip <- FALSE
for (i in seq_along(args)[-1L]) { if (skip) { skip <- FALSE; next }; if (args[i] %in% c("--out", "--core-reference", "--batch-reference", "--batch-a-reference", "--batch-b-reference", "--batch-c-reference", "--accepted-run", "--legacy-run", "--dlm-reference", "--validation-run", "--expected-assessment-days", "--reference-dir", "--protocol-file", "--v14-reference", "--archive-reference", "--nearest-histories", "--gis-reference")) { skip <- TRUE; next }; if (startsWith(args[i], "--")) next; positional <- c(positional, args[i]) }
stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
config <- siw_load_config()
res <- tryCatch({
  if (mode == "demo") {
    out <- opt("--out", file.path(root, "output", paste0("demo_", stamp)))
    r <- siw_run_demo(out, config, resume = FALSE, archived = flag("--archived"))
  } else if (mode == "paper") {
    if (length(positional) < 1L) usage()
    out <- opt("--out", file.path(root, "output", paste0("paper_", stamp)))
    r <- siw_run_paper(positional[1L], out, config, resume = !flag("--no-resume"), archived = flag("--archived"), rainfall_sensitivity = flag("--rainfall-sensitivity"))
  } else if (mode == "validate") {
    if (length(positional) < 2L) usage()
    out <- opt("--out", file.path(root, "output", paste0("validate_", stamp)))
    r <- siw_run_validate(positional[1L], positional[2L], out, config, resume = !flag("--no-resume"))
    cat("VALIDATION_STATUS=", r$overall, "\n", sep = "")
  } else if (mode == "batcha") {
    if (length(positional) < 1L || is.null(opt("--core-reference")) || is.null(opt("--batch-reference"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("batcha_", stamp)))
    eg <- suppressWarnings(as.integer(opt("--expected-assessment-days", NA)))
    r <- siw_run_batch_a(positional[1L], out, opt("--core-reference"), opt("--batch-reference"), config, expected_assessment_days = eg)
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "EXECUTION_FAILURE") 1L else 4L)
  } else if (mode == "batchc") {
    if (length(positional) < 1L || is.null(opt("--batch-c-reference"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("batchc_", stamp)))
    r <- siw_run_batch_c(positional[1L], out, opt("--batch-c-reference"), config, accepted_run_dir = opt("--accepted-run"), resume = flag("--resume"))
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else 4L)
  } else if (mode == "bootdiag") {
    if (length(positional) < 1L) usage()
    out <- opt("--out", file.path(root, "output", paste0("bootdiag_", stamp)))
    r <- siw_run_bootstrap_audit(positional[1L], out)
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "
", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "
STATUS=", r$status, "
", sep = "")
    quit(status = if (r$status == "OK") 0L else 4L)
  } else if (mode == "dlmvalidate") {
    if (length(positional) < 1L || is.null(opt("--dlm-reference"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("dlmvalidate_", stamp)))
    r <- siw_run_dlm_validation(positional[1L], out, opt("--dlm-reference"), config, accepted_run_dir = opt("--accepted-run"), mc = !flag("--no-mc"), manuscript_concordance = !flag("--synthetic-no-manuscript-concordance"))
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_NOT_TESTED") 3L else 4L)
  } else if (mode == "dlmvalidatev2") {
    if (length(positional) < 1L || is.null(opt("--dlm-reference"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("dlmvalidatev2_", stamp)))
    r <- siw_run_dlm_validation_v2(positional[1L], out, opt("--dlm-reference"), config, accepted_run_dir = opt("--accepted-run"), mc = !flag("--no-mc"), manuscript_concordance = !flag("--synthetic-no-manuscript-concordance"))
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    for (nm in names(r$v1_record)) cat(nm, "=", r$v1_record[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_REVIEW_OR_NOT_TESTED") 3L else 4L)
  } else if (mode == "dlmv21cov") {
    if (length(positional) < 1L) usage()
    out <- opt("--out", file.path(root, "output", paste0("dlmv21cov_", stamp)))
    r <- siw_run_dlm_covariance_v21(positional[1L], out, config)
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    if (!is.null(r$missing)) cat("MISSING_FILES=", paste(r$missing, collapse = ";"), "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_CARRIED_FORWARD_OPEN_ITEMS") 3L else 4L)
  } else if (mode == "dlmarchived") {
    if (length(positional) < 1L || is.null(opt("--reference-dir"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("dlmarchived_", stamp)))
    r <- siw_run_archived_dlm_validation(positional[1L], opt("--reference-dir"), out, config, protocol_path = opt("--protocol-file"))        # --protocol-file is for the synthetic tests only; the manual run uses the declared protocol
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_SEPARATE_VALIDATION_ITEMS_OPEN") 3L else 4L)
  } else if (mode == "coldburden") {
    if (length(positional) < 1L || is.null(opt("--reference-dir"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("coldburden_", stamp)))
    r <- siw_run_cold_burden_validation(positional[1L], opt("--reference-dir"), out, config, protocol_path = opt("--protocol-file"))        # --protocol-file is for the synthetic tests only
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_INTERPRETATION_NOT_ASSESSED") 3L else 4L)
  } else if (mode == "dailylayer") {
    if (length(positional) < 1L || is.null(opt("--reference-dir"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("dailylayer_", stamp)))
    r <- siw_run_daily_layer_validation(positional[1L], opt("--reference-dir"), out, config, protocol_path = opt("--protocol-file"))        # --protocol-file is for the synthetic tests only
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_ASSUMPTIONS_UNDER_REVIEW") 3L else 4L)
  } else if (mode == "anomhist") {
    if (length(positional) < 1L || is.null(opt("--reference-dir"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("anomhist_", stamp)))
    r <- siw_run_anomaly_history_validation(positional[1L], opt("--reference-dir"), out, config, protocol_path = opt("--protocol-file"))        # --protocol-file is for the synthetic tests only
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else 4L)
  } else if (mode == "posthoc") {
    # Rscript run.R posthoc <item> <study_data.csv> [--v14-reference DIR] [--archive-reference DIR] [--core-reference DIR] [--nearest-histories FILE[|FILE]] [--gis-reference DIR|FILE[|FILE]] [--synthetic-demo] [--out DIR]
    if (length(positional) < 2L) usage()
    out <- opt("--out", file.path(root, "output", paste0("posthoc_", positional[1L], "_", stamp)))
    refs <- list(posthoc_v14_reference = opt("--v14-reference"), archive_engine_reference = opt("--archive-reference"), core_reference = opt("--core-reference"), nearest_station_histories = opt("--nearest-histories"), gis_ims_v8_1_reference = opt("--gis-reference"))
    r <- siw_run_posthoc(positional[1L], positional[2L], out, config, refs = Filter(Negate(is.null), refs), synthetic_demo = flag("--synthetic-demo"), protocol_path = opt("--protocol-file"))
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("POSTHOC_OVERALL=", r$overall, "\nOUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$overall == "PASS") 0L else if (r$overall == "REVIEW") 3L else 4L)
  } else if (mode == "dlmsimdiag") {
    if (length(positional) < 1L || is.null(opt("--validation-run"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("dlmsimdiag_", stamp)))
    r <- siw_run_dlm_simdiag(positional[1L], out, opt("--validation-run"), opt("--dlm-reference"), config)
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "
", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "
STATUS=", r$status, "
", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_PARTIAL_OR_NOT_TESTED") 3L else 4L)
  } else if (mode == "ordinalrefine") {
    if (length(positional) < 1L || is.null(opt("--batch-b-reference"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("ordinalrefine_", stamp)))
    r <- siw_run_ordinal_refinement(positional[1L], out, opt("--batch-b-reference"), config, legacy_run_dir = opt("--legacy-run"))
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else 4L)
  } else if (mode == "batchb") {
    if (length(positional) < 1L || is.null(opt("--core-reference")) || is.null(opt("--batch-a-reference")) || is.null(opt("--batch-b-reference"))) usage()
    out <- opt("--out", file.path(root, "output", paste0("batchb_", stamp)))
    eg <- suppressWarnings(as.integer(opt("--expected-assessment-days", NA)))
    r <- siw_run_batch_b(positional[1L], out, opt("--core-reference"), opt("--batch-a-reference"), opt("--batch-b-reference"), config, expected_assessment_days = eg)
    for (nm in names(r$statuses)) cat(nm, "=", r$statuses[[nm]], "\n", sep = "")
    for (nm in names(r$math)) cat(nm, "=", r$math[[nm]], "\n", sep = "")
    cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = "")
    quit(status = if (r$status == "OK") 0L else if (r$status == "OK_WITH_DOCUMENTED_LIMITATIONS") 3L else if (r$status == "EXECUTION_FAILURE") 1L else 4L)
  } else usage()
  cat("OUTPUT_DIR=", normalizePath(out, winslash = "/"), "\nSTATUS=", r$status, "\n", sep = ""); r
}, error = function(e) { cat("ERROR: ", conditionMessage(e), "\n", sep = ""); quit(status = 1L) })
if (!identical(res$status, "OK")) quit(status = 1L)
