# Public build and the end-to-end synthetic demo of the public package.
# The PUBLIC package (source tarball) carries code, documentation, the synthetic CSV and the synthetic tests only. Its shipped declarations are redacted copies:
# the SHA-256 values, sizes and locations of frozen references and of the accepted study input are held in the PRIVATE validation kit (local only), which
# restores the full declarations for the author's local study run. A public build is marked by inst/extdata/PUBLIC_BUILD.json.

SIW_REDACTED <- "REDACTED_PUBLIC_BUILD"

#' TRUE in a public build (the shipped declarations are redacted; the frozen-reference declarations are held in the private validation kit).
#' @export
siw_public_build <- function() { root <- siw_root(); any(file.exists(file.path(root, c("inst/extdata", "extdata"), "PUBLIC_BUILD.json"))) }

.siw_synthetic_csv <- function() { cand <- file.path(siw_root(), c("inst/extdata", "extdata"), "synthetic_siecc_winter.csv"); p <- cand[file.exists(cand)][1L]; if (is.na(p)) stop("SIW_SYNTHETIC_CSV_NOT_FOUND", call. = FALSE); p }

#' End-to-end synthetic demo of the public package: the paper workflow (1,680 cumulative specifications, the nine final L01-L56 DLMs, the rainfall sensitivity) and the
#' ten post hoc steps on the shipped fully synthetic CSV. Comparisons with study references or manuscript values are NOT_APPLICABLE_SYNTHETIC_INPUT. Synthetic numbers have no scientific meaning.
#' @param steps post hoc items to run (default all ten)
#' @return list(out_dir, statuses, overall) and DEMO_STATUS.txt / DEMO_SUMMARY.csv in out_dir
#' @export
siw_run_public_demo <- function(out_dir, config = siw_load_config(), steps = SIW_PH_ITEMS) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); out_dir <- normalizePath(out_dir, winslash = "/"); csv <- .siw_synthetic_csv(); rows <- list()
  p <- siw_run_demo(file.path(out_dir, "paper_demo"), config); rows[[1L]] <- data.frame(step = "PAPER_DEMO", label = "PRIMARY_WORKFLOW (synthetic)", status = if (identical(p$status, "OK")) "PASS" else "FAIL", stringsAsFactors = FALSE)
  for (it in steps) { r <- tryCatch(siw_run_posthoc(it, csv, file.path(out_dir, paste0("ph_", it)), config, synthetic_demo = TRUE), error = function(e) list(overall = "EXECUTION_FAILURE", error = conditionMessage(e)))
    rows[[length(rows) + 1L]] <- data.frame(step = siw_posthoc_item(siw_posthoc_protocol(), it)$step, label = siw_posthoc_item(siw_posthoc_protocol(), it)$label, status = r$overall, stringsAsFactors = FALSE) }
  s <- do.call(rbind, rows); siw_write_csv(s, file.path(out_dir, "DEMO_SUMMARY.csv"))
  overall <- if (any(s$status %in% c("FAIL", "EXECUTION_FAILURE"))) "FAIL" else if (any(s$status != "PASS")) "REVIEW" else "PASS"
  writeLines(c(paste0("DEMO_", s$step, "=", s$status), paste0("DEMO_OVERALL=", overall), paste0("PUBLIC_BUILD=", siw_public_build()), "INPUT=the shipped fully synthetic CSV (no scientific meaning)",
               "REVIEW and NOT_TESTED are never counted as PASS; the post hoc steps cannot change q90, q9 or any accepted status."), file.path(out_dir, "DEMO_STATUS.txt"))
  list(out_dir = out_dir, statuses = s, overall = overall)
}
