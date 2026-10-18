# Reproducible package-source manifest and SHA-256 fingerprint for new run metadata (introduced with Batch C; NOT assigned retrospectively to earlier runs).
#
# File scope (explicit; paths relative to the package root, forward slashes, sorted by byte order of the path):
#   INCLUDED   R/*.R; config/*.json; inst/extdata/* (shipped configuration, plans and the fully synthetic example CSV); scripts/*.R and scripts/*.ps1;
#              run.R; run_*.ps1; DESCRIPTION; NAMESPACE.
#   EXCLUDED   everything else, in particular: private_validation, runs/, output(s)/, checkpoints/, reports/, tests/, validation/, man/, documentation (*.md), logs,
#              any *.rds, *.log, *.sav, *.zsav, *.xlsx, any study data (a CSV outside inst/extdata), hidden files and the manifest itself.
# Fingerprint: SHA-256 of the text "<relative path> <SHA-256 of the file> <bytes>\n" lines in sorted order (LF line endings), computed with the same operating-system
# hashing as the run manifests. The manifest is computed at the start of a run and recomputed at its end; any difference is reported (source_changed_during_run).

.siw_source_scope <- function(root) {
  rel <- character()
  add <- function(dir, pattern, recursive = FALSE) { p <- file.path(root, dir); if (dir.exists(p)) rel <<- c(rel, file.path(dir, list.files(p, pattern = pattern, recursive = recursive))) }
  add("R", "[.]R$"); add("config", "[.]json$"); add("inst/extdata", ".*"); add("scripts", "[.](R|ps1)$")
  top <- list.files(root, pattern = "^(run[.]R|run_.*[.]ps1|DESCRIPTION|NAMESPACE)$"); rel <- c(rel, top)
  rel <- unique(gsub("\\\\", "/", rel)); rel <- rel[file.exists(file.path(root, rel)) & !dir.exists(file.path(root, rel))]
  rel <- rel[!grepl("(^|/)[.]", rel) & !grepl("[.](rds|log|sav|zsav|xlsx|tmp|lock)$", rel, ignore.case = TRUE)]
  rel[order(rel, method = "radix")]
}

# SHA-256 of many files with one operating-system call where possible (PowerShell on Windows, sha256sum elsewhere); falls back to siw_file_hash per file.
.siw_hash_many <- function(paths) {
  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows" && nzchar(Sys.which("powershell"))) {
    lst <- tempfile(fileext = ".txt"); on.exit(unlink(lst), add = TRUE); writeLines(paths, lst, useBytes = TRUE)
    cmd <- sprintf("Get-Content -LiteralPath '%s' -Encoding UTF8 | ForEach-Object { (Get-FileHash -LiteralPath $_ -Algorithm SHA256).Hash }", gsub("'", "''", normalizePath(lst, winslash = "\\")))
    out <- suppressWarnings(tryCatch(system2("powershell", c("-NoProfile", "-NonInteractive", "-Command", shQuote(cmd)), stdout = TRUE, stderr = FALSE), error = function(e) character()))
    out <- toupper(trimws(out)); if (length(out) == length(paths) && all(grepl("^[0-9A-F]{64}$", out))) return(out)
  }
  vapply(paths, function(p) siw_file_hash(p)$value, "", USE.NAMES = FALSE)
}

#' Package-source manifest (relative paths, SHA-256, bytes) and its SHA-256 fingerprint. See the header of R/source_manifest.R for the exact file scope.
#' @param root package root (default: the root in use)
#' @return list(manifest = data.frame(file, bytes, sha256), fingerprint, scope, root_kind, n_files)
#' @export
siw_source_manifest <- function(root = siw_root()) {
  rel <- .siw_source_scope(root); abs <- file.path(root, rel); h <- if (length(rel)) .siw_hash_many(abs) else character(); b <- as.numeric(file.info(abs)$size)
  m <- data.frame(file = rel, bytes = b, sha256 = h, stringsAsFactors = FALSE); canon <- paste0(m$file, " ", m$sha256, " ", format(m$bytes, scientific = FALSE, trim = TRUE), "\n", collapse = "")
  tf <- tempfile(fileext = ".txt"); on.exit(unlink(tf), add = TRUE); writeBin(charToRaw(canon), tf)
  list(manifest = m, fingerprint = siw_file_hash(tf)$value, n_files = nrow(m), root_kind = if (file.exists(file.path(root, "R", "utils.R"))) "source_checkout" else "installed_package",
       scope = "R/*.R; config/*.json; inst/extdata/*; scripts/*.R|*.ps1; run.R; run_*.ps1; DESCRIPTION; NAMESPACE. Excluded: private data, runs, outputs, checkpoints, reports, tests, validation, man, documentation, logs, *.rds/*.sav and hidden files.")
}

#' Compare two source manifests; returns the changed, added and removed relative paths (empty when identical).
#' @export
siw_source_manifest_diff <- function(a, b) {
  ma <- a$manifest; mb <- b$manifest; both <- intersect(ma$file, mb$file)
  list(changed = both[ma$sha256[match(both, ma$file)] != mb$sha256[match(both, mb$file)]], added = setdiff(mb$file, ma$file), removed = setdiff(ma$file, mb$file), identical_fingerprint = identical(a$fingerprint, b$fingerprint))
}
