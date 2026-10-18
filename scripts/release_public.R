# Public / private release tooling (13 Oct 2026; public source tree added 14 Oct 2026). Run from the package source checkout; never reads participant-level data.
#   Rscript scripts/release_public.R build <release_dir>        public\: R source tarball + redacted full source tree (launcher, wrappers, guides); private_kit\; privacy scans
#   Rscript scripts/release_public.R scan <tarball_or_dir> --denylist <privacy_denylist.json> [--report FILE]
# PUBLIC (public\): (1) the R source tarball: code, help pages, the fully synthetic CSV, synthetic tests and REDACTED declarations; (2) sieccwinter_source\: the full source
#   tree (launcher, wrappers, validation scripts, guides) with every private value redacted (REDACTED_PUBLIC_BUILD / <private>), the private local configuration replaced by
#   its template (config/private_local.template.json), and inst/extdata/PUBLIC_BUILD.json added.
# PRIVATE (private_kit\, local only): every file that is absent from or differs in the public source tree (the full declarations, the input declaration, the local
#   configuration, the test constants, the development log), as an overlay restoring the development tree on top of the public source tree (install_private_kit.ps1;
#   the source fingerprint is checked). The deny list of the scan is read from config/private_local.json (git-ignored); no private value is written in this script.
args <- commandArgs(TRUE); fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), ".."), winslash = "/")
source(file.path(root, "scripts", "load_package.R")); siw_dev_load(root)
opt <- function(flag, default = NULL) { i <- match(flag, args); if (is.na(i) || i == length(args)) default else args[i + 1L] }
REDACT <- "REDACTED_PUBLIC_BUILD"; SYNTHETIC <- "synthetic_siecc_winter.csv"

# ---- privacy scan ---------------------------------------------------------------------------------------------------------------------------------------------
GENERIC_RULES <- list(
  user_path_windows = "(?i)\\b[A-Z]:[\\\\/]+Users[\\\\/]+[^\\\\/\\s\"']+",
  user_path_unix = "(?i)(^|[\\s\"'(=])/(Users|home)/[^/\\s\"']+",
  cloud_sync_folder = "(?i)O(?:n)eDrive")                                            # written so that this file does not match its own rule
DATA_EXT <- "(?i)[.](sav|zsav|por|rds|rda|rdata|xlsx|xls|dta|sas7bdat|parquet|feather)$"

.is_text <- function(f) { b <- readBin(f, "raw", n = 8000L); !any(b == as.raw(0)) }
.lines <- function(f) suppressWarnings(readLines(f, warn = FALSE, encoding = "UTF-8"))
.esc <- function(x) gsub("([.\\\\+*?^$(){}|\\[\\]])", "\\\\\\1", x)
# the maintainer address is allowed only in the declared files (DESCRIPTION, README.md); every other occurrence of the domain still fails
.mask_allowed <- function(L, rel, dl) { a <- dl$allowed_maintainer_email; if (length(a) && nzchar(a) && basename(rel) %in% dl$allowed_email_files) gsub(a, "", L, fixed = TRUE) else L }

#' Privacy scan of a public tarball or folder. Returns list(status, findings, rules). Matched private text is never printed (file, rule and line only).
scan_public <- function(target, denylist) {
  dir <- target; if (grepl("[.]tar[.]gz$", target)) { dir <- file.path(tempdir(), paste0("pubscan_", sample.int(1e6, 1))); dir.create(dir); utils::untar(target, exdir = dir) }
  dl <- if (is.list(denylist)) denylist else jsonlite::fromJSON(denylist, simplifyVector = TRUE); files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE); rel <- substring(files, nchar(dir) + 2L)
  F <- list(); add <- function(rule, file, line, note) F[[length(F) + 1L]] <<- data.frame(rule = rule, file = file, line = as.integer(line), note = note, stringsAsFactors = FALSE)
  named <- c(stats::setNames(unlist(GENERIC_RULES), names(GENERIC_RULES)))
  word <- function(x) paste0("(?i)", .esc(x))
  for (k in seq_along(dl$account_names)) named[[paste0("account_name_", k)]] <- paste0("(?i)(^|[^A-Za-z0-9])", dl$account_names[k], "([^A-Za-z0-9]|$)")
  for (k in seq_along(dl$emails)) named[[paste0("email_", k)]] <- word(dl$emails[k])
  for (k in seq_along(dl$machine_folders)) named[[paste0("private_folder_", k)]] <- word(dl$machine_folders[k])
  for (k in seq_along(dl$private_data_names)) named[[paste0("private_data_name_", k)]] <- word(dl$private_data_names[k])
  hashes <- toupper(unique(dl$hashes)); pcp <- dl$participant_code_patterns
  for (i in seq_along(files)) { f <- files[i]; r <- rel[i]
    if (grepl(DATA_EXT, r)) add("data_file", r, 0L, "binary or data file type not allowed in the public package")
    if (grepl("(?i)[.](csv|tsv)$", r) && basename(r) != SYNTHETIC) add("data_file", r, 0L, "only the shipped synthetic CSV may be a data file")
    if (!.is_text(f)) next
    L <- .mask_allowed(.lines(f), r, dl)
    for (nm in names(named)) { h <- grep(named[[nm]], L, perl = TRUE); for (j in h) add(nm, r, j, "match (text not printed)") }
    if (length(hashes)) { hx <- regmatches(L, gregexpr("[0-9A-Fa-f]{64}", L)); for (j in seq_along(hx)) for (v in toupper(hx[[j]])) if (v %in% hashes) add("private_hash", r, j, paste0("deny-listed SHA-256 ", substr(v, 1, 8), "...")) }
    if (length(pcp)) for (p in pcp) { h <- grep(p, L, perl = TRUE); for (j in h) add("participant_code_pattern", r, j, "match (text not printed)") }
    if (basename(r) == SYNTHETIC) { d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, nrows = 1000L)
      if (!all(grepl("^SYN[0-9]{3}$", d$participant_code))) add("participant_code_synthetic", r, 0L, "the synthetic CSV holds a participant_code that is not SYNnnn")
      dc <- grep("(?i)date", names(d), value = TRUE, perl = TRUE); if (length(dc)) add("individual_dates", r, 0L, paste0("date columns in the synthetic CSV: ", length(dc))) } }
  fd <- if (length(F)) do.call(rbind, F) else data.frame(rule = character(), file = character(), line = integer(), note = character())
  rules <- data.frame(rule = c(names(named), "private_hash", "data_file", "participant_code_synthetic", "individual_dates", "participant_code_pattern"),
                      applied = c(rep(TRUE, length(named)), length(hashes) > 0L, TRUE, TRUE, TRUE, length(pcp) > 0L), stringsAsFactors = FALSE)
  rules$findings <- vapply(rules$rule, function(z) sum(fd$rule == z), 0L)
  rules$status <- ifelse(rules$findings > 0L, "FAIL", ifelse(rules$applied, "PASS", "NOT_TESTED"))
  st <- if (any(rules$status == "FAIL")) "FAIL" else if (any(rules$status == "NOT_TESTED")) "REVIEW" else "PASS"
  list(status = st, findings = fd, rules = rules, files = length(files), hashes = length(hashes))
}

write_scan <- function(sc, report, target) {
  dir.create(dirname(report), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(paste0("PRIVACY_SCAN_STATUS=", sc$status), paste0("PRIVACY_SCAN_TARGET=", basename(target)), paste0("FILES_SCANNED=", sc$files), paste0("DENY_LISTED_HASHES=", sc$hashes),
               paste0("RULE_", toupper(sc$rules$rule), "=", sc$rules$status, " (", sc$rules$findings, " findings)"),
               if (any(sc$rules$status == "NOT_TESTED")) "NOTE=participant_code_pattern is NOT_TESTED: add the participant-code regular expressions to config/private_local.json (privacy_denylist) and rescan." else character(),
               "Matched private text is never printed; findings list the file, the rule and the line only."), report)
  utils::write.csv(sc$findings, sub("[.]txt$", "_findings.csv", report), row.names = FALSE)
}

# ---- redaction ------------------------------------------------------------------------------------------------------------------------------------------------
redact_value <- function(x, deny) {
  if (is.list(x)) { has_sha <- !is.null(names(x)) && "sha256" %in% names(x); for (k in seq_along(x)) { nm <- if (is.null(names(x))) "" else names(x)[k]
      if (has_sha && nm %in% c("bytes", "size_bytes") && is.numeric(x[[k]])) x[[k]] <- -1L else if (!is.null(x[[k]])) x[[k]] <- redact_value(x[[k]], deny) }; return(x) }
  if (is.character(x) && length(x) == 1L) {
    if (grepl("^[0-9A-Fa-f]{64}$", x) || grepl("WINTER_SOURCE/", x, fixed = TRUE) || any(vapply(deny, function(d) grepl(d, x, ignore.case = TRUE, fixed = FALSE), NA)) || grepl("(?i)\\b[A-Z]:[\\\\/]+Users[\\\\/]", x, perl = TRUE)) return(REDACT)
    return(gsub("[0-9A-Fa-f]{64}", REDACT, x))                                     # a SHA-256 embedded in a longer text
  }
  x
}
# rewrites the file only when redaction changes a value (unchanged declarations keep their bytes and hashes); returns TRUE when redacted
redact_json <- function(src, dst, deny) {
  p0 <- jsonlite::fromJSON(src, simplifyVector = FALSE); p <- redact_value(p0, deny); if (identical(p, p0)) return(FALSE)
  if (!is.null(p$status_of_this_document)) p$public_build_note <- paste0("PUBLIC BUILD: every SHA-256, size and location of the accepted input and of the frozen references is replaced by ", REDACT, "; the full declaration is held in the private validation kit.")
  jsonlite::write_json(p, dst, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"); TRUE
}
# text files of the public source tree (guides, wrappers, validation scripts): deny-listed hashes -> REDACTED_PUBLIC_BUILD; private names, account names, user paths and
# participant-code patterns -> <private>; rewritten only when something changes; returns TRUE when redacted
redact_text_file <- function(f, rel, dl) {
  if (!.is_text(f)) return(FALSE); L0 <- .lines(f); L <- L0; a <- dl$allowed_maintainer_email; keep <- length(a) && nzchar(a) && basename(rel) %in% dl$allowed_email_files
  if (keep) L <- gsub(a, "\001MAINTAINER\001", L, fixed = TRUE)
  for (h in dl$hashes) L <- gsub(h, REDACT, L, ignore.case = TRUE)
  for (w in c(dl$machine_folders, dl$private_data_names, dl$emails)) L <- gsub(paste0("(?i)", .esc(w)), "<private>", L, perl = TRUE)
  for (w in dl$account_names) L <- gsub(paste0("(?i)(^|[^A-Za-z0-9])", .esc(w), "(?=[^A-Za-z0-9]|$)"), "\\1<private>", L, perl = TRUE)
  for (g in GENERIC_RULES[c("user_path_windows", "cloud_sync_folder")]) L <- gsub(g, "<private>", L, perl = TRUE)
  for (p in dl$participant_code_patterns) L <- gsub(p, "<private>", L, perl = TRUE)
  if (keep) L <- gsub("\001MAINTAINER\001", a, L, fixed = TRUE)
  if (identical(L, L0)) return(FALSE); con <- file(f, open = "wb"); on.exit(close(con)); writeLines(enc2utf8(L), con, sep = "\n", useBytes = TRUE); TRUE
}

# R CMD build writes "Packaged: <time>; <account>" into DESCRIPTION; the account name is replaced (the tarball is rebuilt with utils::tar, gzip)
neutralise_packaged <- function(tb) {
  ex <- file.path(tempdir(), paste0("retar_", sample.int(1e6, 1))); dir.create(ex); utils::untar(tb, exdir = ex); d <- file.path(ex, "sieccwinter", "DESCRIPTION"); L <- readLines(d)
  L <- sub("^(Packaged: [^;]*);.*$", "\\1; public build", L); writeLines(L, d); old <- setwd(ex); on.exit(setwd(old), add = TRUE)
  unlink(tb); utils::tar(tb, files = "sieccwinter", compression = "gzip", tar = "internal"); invisible(tb)
}

# ---- build -----------------------------------------------------------------------------------------------------------------------------------------------------
PRIVATE_FILES <- c("config/private_local.json", "config/project_inputs_declaration_v1.json", "inst/extdata/project_inputs_declaration_v1.json", "tests/testthat/private_constants.json")
# development-tree files that belong to neither release (run outputs, caches, private folders and filled paths files)
.tree_files <- function(root) {
  top <- list.files(root, all.files = TRUE, no.. = TRUE)
  top <- top[!top %in% c("runs", "output", "outputs", "checkpoints", "demo_output", ".Rproj.user", "private_validation", "private_kit", "manual_run_paths.json") & !grepl("^(private_out|fullrun_|PUBLIC_RELEASE)|[.](tmp|partial|log|sav|SAV|zsav|xlsx|rds)$|^siecc_winter_canonical[.]csv$", top)]
  unlist(lapply(top, function(x) if (dir.exists(file.path(root, x))) file.path(x, list.files(file.path(root, x), recursive = TRUE, all.files = TRUE)) else x))
}

denylist_from <- function(root) {
  pl <- file.path(root, "config", "private_local.json")
  if (!file.exists(pl)) stop("RELEASE_NEEDS_THE_PRIVATE_LOCAL_CONFIGURATION: config/private_local.json (see config/private_local.template.json)", call. = FALSE)
  pd <- jsonlite::fromJSON(pl, simplifyVector = TRUE)$privacy_denylist; if (is.null(pd) || any(grepl("^<", unlist(pd)))) stop("RELEASE_PRIVATE_LOCAL_CONFIGURATION_INCOMPLETE: fill privacy_denylist in config/private_local.json", call. = FALSE)
  decl <- list.files(file.path(root, "config"), pattern = "[.]json$", full.names = TRUE); decl <- decl[basename(decl) != "sieccwinter_config.json"]
  hx <- unique(toupper(unlist(lapply(c(decl, file.path(root, "scripts", c("_run_full.ps1", "fullrun_env_check.R")), file.path(root, "tests", "testthat", "private_constants.json"), file.path(root, "validation", "anom_sample_sd_diag.R")),
                                     function(f) if (file.exists(f)) { L <- .lines(f); unlist(regmatches(L, gregexpr("[0-9A-Fa-f]{64}", L))) } else character()))))
  c(list(`_comment` = "PRIVATE (local only): deny list of the privacy scan, built from config/private_local.json. Never copy into the public package."), pd, list(hashes = hx))
}

build_release <- function(release_dir) {
  stopifnot(!dir.exists(release_dir) || !length(list.files(release_dir)))
  pub <- file.path(release_dir, "public"); kit <- file.path(release_dir, "private_kit"); stg <- file.path(release_dir, "staging", "sieccwinter"); src <- file.path(pub, "sieccwinter_source", "sieccwinter")
  for (d in c(pub, kit, stg, src)) dir.create(d, recursive = TRUE)
  dl <- denylist_from(root); dl$hashes <- setdiff(dl$hashes, toupper(siw_file_hash(file.path(root, "inst", "extdata", SYNTHETIC))$value))
  deny <- c(dl$machine_folders, dl$private_data_names, dl$account_names)
  # 1. staging tree of the R package: R package parts only; redacted declarations; no input declaration; no test constants
  for (d in c("R", "man", "tests", "inst")) file.copy(file.path(root, d), stg, recursive = TRUE)
  for (f in c("DESCRIPTION", "NAMESPACE", "LICENSE", "README.md", ".Rbuildignore")) file.copy(file.path(root, f), file.path(stg, f))
  unlink(file.path(stg, PRIVATE_FILES[startsWith(PRIVATE_FILES, "tests/") | startsWith(PRIVATE_FILES, "inst/")]))
  red <- character(); for (f in list.files(file.path(stg, "inst", "extdata"), pattern = "[.]json$", full.names = TRUE)) { if (basename(f) == "sieccwinter_config.json") next; if (redact_json(f, f, deny)) red <- c(red, basename(f)) }
  # deny list: hashes declared in the private declarations and constants, plus the hashes of the full (unredacted) declarations; minus the hash of any file shipped publicly
  dl$hashes <- unique(c(dl$hashes, toupper(vapply(file.path(root, "config", red), function(f) if (file.exists(f)) siw_file_hash(f)$value else "", ""))))
  pubh <- toupper(vapply(list.files(stg, recursive = TRUE, full.names = TRUE, all.files = TRUE), function(f) siw_file_hash(f)$value, "")); dl$hashes <- setdiff(dl$hashes[nzchar(dl$hashes)], pubh)
  marker <- list(public_build = TRUE, built = format(Sys.Date()), redacted_declarations = red, removed = PRIVATE_FILES, note = "The full declarations, the input declaration, the local configuration and the test constants are held in the private validation kit (local only).")
  jsonlite::write_json(marker, file.path(stg, "inst", "extdata", "PUBLIC_BUILD.json"), auto_unbox = TRUE, pretty = TRUE)
  # 2. R source tarball
  rexe <- file.path(R.home("bin"), "R"); old <- setwd(pub); on.exit(setwd(old), add = TRUE)
  out <- system2(rexe, c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(stg)), stdout = TRUE, stderr = TRUE); writeLines(out, file.path(pub, "R_CMD_BUILD.log"))
  tb <- list.files(pub, pattern = "[.]tar[.]gz$", full.names = TRUE); if (length(tb) != 1L) stop("RELEASE_BUILD_FAILED: see R_CMD_BUILD.log", call. = FALSE)
  setwd(old); neutralise_packaged(normalizePath(tb, winslash = "/"))
  # 3. public source tree: the whole development tree (launcher, wrappers, validation scripts, guides), private files removed, declarations and text redacted
  tf <- .tree_files(root); tf <- tf[!tf %in% PRIVATE_FILES & !grepl("^DEVELOPMENT_LOG_.*[.]md$", tf)]
  for (x in tf) { d <- file.path(src, x); dir.create(dirname(d), recursive = TRUE, showWarnings = FALSE); file.copy(file.path(root, x), d, overwrite = TRUE, copy.date = TRUE) }
  for (f in list.files(file.path(src, c("config", "inst/extdata")), pattern = "[.]json$", full.names = TRUE)) { if (basename(f) %in% c("sieccwinter_config.json", "private_local.template.json")) next; redact_json(f, f, deny) }
  txt <- 0L; for (x in tf) { f <- file.path(src, x); if (!file.exists(f) || grepl("[.]json$", x) && dirname(x) %in% c("config", "inst/extdata")) next; if (redact_text_file(f, x, dl)) txt <- txt + 1L }
  jsonlite::write_json(marker, file.path(src, "inst", "extdata", "PUBLIC_BUILD.json"), auto_unbox = TRUE, pretty = TRUE)
  sf <- list.files(src, recursive = TRUE, all.files = TRUE); utils::write.csv(data.frame(file = sf, bytes = file.info(file.path(src, sf))$size, sha256 = vapply(file.path(src, sf), function(f) siw_file_hash(f)$value, "", USE.NAMES = FALSE)), file.path(pub, "SOURCE_TREE_MANIFEST.csv"), row.names = FALSE)
  # 4. private validation kit: every development-tree file that is absent from or differs in the public source tree
  ov <- file.path(kit, "overlay"); dir.create(ov); n_ov <- 0L
  for (x in .tree_files(root)) { a <- file.path(root, x); b <- file.path(src, x); if (file.exists(b) && identical(siw_file_hash(a)$value, siw_file_hash(b)$value)) next
    d <- file.path(ov, x); dir.create(dirname(d), recursive = TRUE, showWarnings = FALSE); file.copy(a, d, copy.date = TRUE); n_ov <- n_ov + 1L }
  jsonlite::write_json(dl, file.path(kit, "privacy_denylist.json"), auto_unbox = FALSE, pretty = TRUE)
  ovf <- list.files(ov, recursive = TRUE, all.files = TRUE); h <- vapply(file.path(ov, ovf), function(f) siw_file_hash(f)$value, "", USE.NAMES = FALSE)
  utils::write.csv(data.frame(file = ovf, bytes = file.info(file.path(ov, ovf))$size, sha256 = h, stringsAsFactors = FALSE), file.path(kit, "PRIVATE_KIT_MANIFEST.csv"), row.names = FALSE)
  writeLines(c(paste0("DEV_SOURCE_FINGERPRINT=", siw_source_manifest(root)$fingerprint), paste0("PUBLIC_TARBALL=", basename(tb)), paste0("PUBLIC_TARBALL_SHA256=", siw_file_hash(tb)$value),
               "KIT_TARGET=public\\sieccwinter_source\\sieccwinter (a copy of the public source tree)"), file.path(kit, "KIT_FINGERPRINT.txt"))
  file.copy(file.path(root, "scripts", "install_private_kit.ps1"), kit); file.copy(file.path(root, "scripts", "PRIVATE_KIT_README.md"), kit)
  # 5. privacy scans of the public tarball and of the public source tree
  sc <- scan_public(tb, dl); write_scan(sc, file.path(pub, "PRIVACY_SCAN_REPORT.txt"), tb)
  ss <- scan_public(src, dl); write_scan(ss, file.path(pub, "PRIVACY_SCAN_REPORT_SOURCE_TREE.txt"), src)
  pf <- list.files(pub, full.names = TRUE); pf <- pf[!dir.exists(pf)]
  utils::write.csv(data.frame(file = basename(pf), bytes = file.info(pf)$size, sha256 = vapply(pf, function(f) siw_file_hash(f)$value, "", USE.NAMES = FALSE)), file.path(release_dir, "PUBLIC_RELEASE_MANIFEST.csv"), row.names = FALSE)
  unlink(file.path(release_dir, "staging"), recursive = TRUE)
  st <- if ("FAIL" %in% c(sc$status, ss$status)) "FAIL" else if ("REVIEW" %in% c(sc$status, ss$status)) "REVIEW" else "PASS"
  cat("PUBLIC_TARBALL=", tb, "\nPRIVACY_SCAN_STATUS_TARBALL=", sc$status, "\nPRIVACY_SCAN_STATUS_SOURCE_TREE=", ss$status, "\nPRIVACY_SCAN_STATUS=", st, "\nREDACTED_DECLARATIONS=", length(red),
      "\nREDACTED_TEXT_FILES=", txt, "\nSOURCE_TREE_FILES=", length(sf), "\nKIT_OVERLAY_FILES=", length(ovf), "\n", sep = "")
  invisible(list(status = st))
}

if (length(args) && args[1] == "build") { if (length(args) < 2L) stop("usage: release_public.R build <release_dir>"); sc <- build_release(normalizePath(args[2], winslash = "/", mustWork = FALSE)); quit(status = if (sc$status == "FAIL") 4L else if (sc$status == "REVIEW") 3L else 0L) }
if (length(args) && args[1] == "scan") { t <- args[2]; d <- opt("--denylist"); if (is.null(d)) stop("--denylist is required"); rep <- opt("--report", file.path(dirname(t), "PRIVACY_SCAN_REPORT.txt")); sc <- scan_public(t, d); write_scan(sc, rep, t); cat("PRIVACY_SCAN_STATUS=", sc$status, "\n", sep = ""); quit(status = if (sc$status == "FAIL") 4L else if (sc$status == "REVIEW") 3L else 0L) }
