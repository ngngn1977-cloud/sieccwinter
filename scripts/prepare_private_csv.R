# Converts the confidential SPSS file to the canonical private CSV used by run_paper.ps1 / run_validate.ps1.
# Usage: Rscript scripts/prepare_private_csv.R --sav <file.sav> --out-dir <private dir> [--overwrite] [--expected-sav-sha256 <hex>] [--expected-rows <n>]
# Exit codes: 0 success; 1 conversion or validation failure; 2 invalid arguments, unavailable software, or refusal to overwrite / unsafe output location.
# Writes only inside --out-dir. Prints and stores aggregate information only (no participant records, no absolute paths in the reports).
fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1L])), ".."), winslash = "/")
args <- commandArgs(TRUE)
die <- function(code, msg) { cat(if (code == 2L) "INVALID_OR_UNAVAILABLE: " else "CONVERSION_FAILED: ", msg, "\n", sep = ""); quit(status = code) }
opt <- function(flag) { i <- match(flag, args); if (is.na(i) || i == length(args)) NULL else args[i + 1L] }
known <- c("--sav", "--out-dir", "--overwrite", "--expected-sav-sha256", "--expected-rows")
unk <- args[startsWith(args, "--") & !(args %in% known)]; if (length(unk)) die(2L, paste("unknown option:", paste(unk, collapse = " ")))
sav <- opt("--sav"); outd <- opt("--out-dir"); overwrite <- "--overwrite" %in% args
exp_hash <- opt("--expected-sav-sha256"); exp_rows <- { v <- opt("--expected-rows"); if (is.null(v)) NA_integer_ else suppressWarnings(as.integer(v)) }
if (is.null(sav) || is.null(outd)) die(2L, "both --sav and --out-dir are required")
if (!requireNamespace("foreign", quietly = TRUE)) die(2L, "required R package 'foreign' is not installed (nothing was installed)")
if (!requireNamespace("jsonlite", quietly = TRUE)) die(2L, "required R package 'jsonlite' is not installed (nothing was installed)")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
source(file.path(root, "scripts", "private_conversion_lib.R"))
if (!file.exists(sav) || dir.exists(sav)) die(2L, "the SAV file was not found")
if (!grepl("[.]sav$", sav, ignore.case = TRUE)) die(2L, "the input must be an SPSS .sav file")
sav <- normalizePath(sav, winslash = "/"); outd <- normalizePath(outd, winslash = "/", mustWork = FALSE)
if (siw_priv_inside(outd, root)) die(2L, "the private output directory must be outside the package repository")
if (siw_priv_inside(outd, dirname(sav))) die(2L, "the private output directory must not be inside the folder that holds the SAV file")
if (dir.exists(outd) || file.exists(outd)) { if (!dir.exists(outd)) die(2L, "the output path exists and is not a directory") }
names_out <- siw_priv_output_names(); tgt <- file.path(outd, names_out)
if (any(file.exists(tgt)) && !overwrite) die(2L, paste0("a conversion already exists in the output directory (", paste(names_out[file.exists(tgt)], collapse = ", "), "); use --overwrite to replace it"))
if (!dir.exists(outd)) dir.create(outd, recursive = TRUE)
stopifnot(all(vapply(tgt, function(p) identical(siw_priv_norm(dirname(p)), siw_priv_norm(outd)), logical(1))))   # every output is directly inside the chosen directory

config <- siw_load_config()
say <- function(...) cat(..., "\n", sep = "")
sav_hash <- siw_file_hash(sav); sav_size <- file.info(sav)$size
say("Source: ", basename(sav), " (", sav_size, " bytes, ", sav_hash$algorithm, " ", sav_hash$value, ")")
pre <- list()
if (!is.null(exp_hash)) pre[[1L]] <- data.frame(check_id = "C01", category = "source_hash", status = if (identical(toupper(exp_hash), toupper(sav_hash$value)) && sav_hash$algorithm == "sha256") "PASS" else "FAIL",
  observed = sav_hash$value, detail = "compared with the expected SHA-256 supplied on the command line", stringsAsFactors = FALSE) else
  pre[[1L]] <- data.frame(check_id = "C01", category = "source_hash", status = "INFO", observed = sav_hash$value, detail = "no expected hash supplied", stringsAsFactors = FALSE)

res <- NULL; audit <- NULL; rt_ok <- FALSE; partial <- character()
read_err <- NULL
if (pre[[1L]]$status != "FAIL") {
  res <- tryCatch({
    r1 <- siw_priv_read_sav(sav, use_missings = TRUE); r2 <- siw_priv_read_sav(sav, use_missings = FALSE)
    cv <- siw_priv_convert(r1$data, config, raw_keep = r2$data, expected_rows = exp_rows)
    cv$warn <- siw_priv_redact_warnings(unique(c(r1$warnings, r2$warnings))); rm(r1, r2); cv
  }, error = function(e) { read_err <<- conditionMessage(e); NULL })
}
if (!is.null(read_err)) pre[[length(pre) + 1L]] <- data.frame(check_id = "C00", category = "read_or_convert", status = "FAIL", observed = "error", detail = substr(gsub("[\r\n]+", " ", read_err), 1L, 300L), stringsAsFactors = FALSE)
if (!is.null(res)) pre[[length(pre) + 1L]] <- data.frame(check_id = "C12", category = "read_warnings", status = "INFO", observed = sum(res$warn$n), detail = paste0("distinct redacted warning categories=", nrow(res$warn)), stringsAsFactors = FALSE)
audit <- do.call(rbind, c(pre, if (!is.null(res)) list(res$audit)))
fail <- any(audit$status == "FAIL")

tmp <- function(n) file.path(outd, paste0(n, ".partial"))
if (!fail) {
  csv_tmp <- tmp(names_out[1L]); siw_priv_write_canonical_csv(res$data, csv_tmp); partial <- c(partial, csv_tmp)
  rt <- siw_priv_roundtrip_check(res$data, csv_tmp, config)
  audit <- rbind(audit, data.frame(check_id = "C14", category = "csv_round_trip", status = if (rt$ok) "PASS" else "FAIL", observed = rt$n_rows,
    detail = "CSV read back with the package reader is identical to the converted data (numerics exact, identifiers identical, column order equal)", stringsAsFactors = FALSE))
  fail <- any(audit$status == "FAIL")
}
status <- if (fail) "FAIL" else "PASS"
audit <- audit[order(match(audit$check_id, audit$check_id)), ]; rownames(audit) <- NULL
siw_write_csv(audit, tmp(names_out[3L])); partial <- c(partial, tmp(names_out[3L]))
if (!fail) { jsonlite::write_json(siw_priv_dictionary(res$map, res$data, res$n_na), tmp(names_out[2L]), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA); partial <- c(partial, tmp(names_out[2L])) }
writeLines(utils::capture.output(print(utils::sessionInfo())), tmp(names_out[5L])); partial <- c(partial, tmp(names_out[5L]))
csv_info <- if (!fail) { h <- siw_file_hash(tmp(names_out[1L])); list(bytes = file.info(tmp(names_out[1L]))$size, hash = h$value, alg = h$algorithm) } else NULL
S <- c(paste0("# Conversion summary: ", status), "",
  paste0("* Source file (basename only): `", basename(sav), "`; ", sav_size, " bytes; ", sav_hash$algorithm, " `", sav_hash$value, "`"),
  paste0("* Package version: ", siw_pkg_version(), "; ", R.version.string, "; foreign ", as.character(utils::packageVersion("foreign"))),
  paste0("* Import rule: foreign::read.spss(use.value.labels = FALSE, to.data.frame = TRUE, use.missings = TRUE, trim.factor.names = FALSE, trim_values = FALSE, reencode = NA), as in the legacy code; value labels not applied; user-defined missing values become NA; rows in file order."),
  if (!fail) paste0("* Rows: ", nrow(res$data), "; canonical columns: ", ncol(res$data), " (22 scalar, 472 daily L01-L59, 80 cumulative)") else "* No canonical CSV was written because the conversion failed.",
  if (!fail) paste0("* Canonical CSV: `", names_out[1L], "`; ", csv_info$bytes, " bytes; ", csv_info$alg, " `", csv_info$hash, "`") else NULL,
  paste0("* Audit: ", sum(audit$status == "PASS"), " PASS, ", sum(audit$status == "WARN"), " WARN, ", sum(audit$status == "FAIL"), " FAIL, ", sum(audit$status == "INFO"), " INFO (see `CONVERSION_AUDIT.csv`)"), "",
  if (fail) c("## Failed checks", "", paste0("* ", audit$check_id[audit$status == "FAIL"], ": ", audit$category[audit$status == "FAIL"], " - ", audit$detail[audit$status == "FAIL"]), "") else NULL,
  if (any(audit$status == "WARN")) c("## Warnings (reported, not corrected)", "", paste0("* ", audit$check_id[audit$status == "WARN"], ": ", audit$detail[audit$status == "WARN"]), "") else NULL,
  "## Privacy", "", "This summary, the audit, the dictionary and the manifest contain only variable names and aggregate counts. The canonical CSV contains pseudonymous participant codes and study values: keep it private, never commit or upload it.")
writeLines(S, tmp(names_out[4L])); partial <- c(partial, tmp(names_out[4L]))
# manifest of the other files (input by basename)
lst <- sub("[.]partial$", "", basename(partial)); pp <- partial
hh <- lapply(pp, siw_file_hash)
man <- rbind(data.frame(role = "input", file = basename(sav), bytes = sav_size, algorithm = sav_hash$algorithm, sha256_or_hash = sav_hash$value, stringsAsFactors = FALSE),
  data.frame(role = "output", file = lst, bytes = file.info(pp)$size, algorithm = vapply(hh, function(z) z$algorithm, ""), sha256_or_hash = vapply(hh, function(z) z$value, ""), stringsAsFactors = FALSE))
man <- man[order(man$role, man$file), ]
siw_write_csv(man, tmp(names_out[6L])); partial <- c(partial, tmp(names_out[6L]))
# publish: rename partial files into place (replaces an existing conversion only with --overwrite, checked above)
for (p in partial) { dest <- sub("[.]partial$", "", p); if (file.exists(dest)) unlink(dest); if (!file.rename(p, dest)) die(1L, paste("could not write", basename(dest))) }
say("Audit: ", sum(audit$status == "PASS"), " PASS, ", sum(audit$status == "WARN"), " WARN, ", sum(audit$status == "FAIL"), " FAIL")
say("Files written: ", paste(sub("[.]partial$", "", basename(partial)), collapse = ", "))
say("CONVERSION_STATUS=", status)
quit(status = if (fail) 1L else 0L)
