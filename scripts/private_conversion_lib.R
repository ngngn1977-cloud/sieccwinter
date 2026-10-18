# Private-data conversion library: original SPSS (.sav) -> canonical sieccwinter study CSV.
# Sourced by scripts/prepare_private_csv.R and by the tests. Requires the sieccwinter functions (siw_dev_load) and, for reading, foreign.
# Legacy import semantics (audited read-only in Phase 2B, WINTER_SOURCE R/020_import_validate.R):
#   foreign::read.spss(path, use.value.labels = FALSE, to.data.frame = TRUE, trim.factor.names = FALSE, trim_values = FALSE, reencode = NA)
#   use.missings is left at its default (= to.data.frame = TRUE): user-defined missing values become NA, like system-missing values.
#   Labelled numeric variables stay numeric codes (value labels are NOT applied). Rows keep file order. Names matched case-insensitively.
# Aggregate information only: nothing here prints or stores participant-level values.

# ---- mapping from SAV variable names to the canonical CSV --------------------------------------------------------------
siw_priv_mapping <- function(config) {
  spec <- siw_input_spec()
  src <- stats::setNames(spec$column, spec$column)
  src[c("Econ_STS", "Subjective_Health", "Memory", "Memory_Changed", "Subjective_Wellbeing", "Outdoor_Mobility", "PedestLength_m")] <-
    c("economic_vulnerabilityref.elsepoorfairavaerage1", "Subjective_helth", "Howwouldyourateyourmemory",
      "Comparedwiththepreviousseasonsummerwinterisyourmemor", "Subjective_wellbieng", "outdoor_mobility", "PedestLengthm")
  m <- data.frame(canonical = spec$column, source = unname(src[spec$column]), type = spec$type, allowed = spec$allowed, role = spec$role, stringsAsFactors = FALSE)
  m$transform <- ifelse(m$canonical == "participant_code", "trimws(as.character())", ifelse(m$canonical == m$source, "as.numeric()", "renamed; as.numeric()"))
  dn <- siw_daily_columns(config, 59L)
  cn <- siw_cumulative_columns(config)
  ex <- data.frame(canonical = c(dn, cn), source = c(dn, cn), type = "numeric", allowed = NA_character_,
                   role = c(rep("daily_exposure", length(dn)), rep("cumulative_exposure", length(cn))), transform = "as.numeric()", stringsAsFactors = FALSE)
  rbind(m, ex)
}

# ---- reading (legacy arguments) ----------------------------------------------------------------------------------------
siw_priv_read_sav <- function(path, use_missings = TRUE) {
  if (!requireNamespace("foreign", quietly = TRUE)) stop("SIW_PACKAGE_UNAVAILABLE: foreign", call. = FALSE)
  warns <- character()
  val <- withCallingHandlers(
    foreign::read.spss(path, use.value.labels = FALSE, to.data.frame = TRUE, use.missings = use_missings, trim.factor.names = FALSE, trim_values = FALSE, reencode = NA),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(data = val, warnings = warns)
}

# Warning texts may quote labels; keep only a redacted category and a count.
siw_priv_redact_warnings <- function(w) {
  if (!length(w)) return(data.frame(category = character(), n = integer()))
  cat <- gsub("[0-9]+", "#", gsub("'[^']*'|\"[^\"]*\"", "<x>", w)); cat <- substr(cat, 1L, 80L)
  t <- table(cat); data.frame(category = names(t), n = as.integer(t), stringsAsFactors = FALSE)
}

# ---- name matching as in the legacy code (exact, else case-insensitive; ambiguity is an error) -------------------------
siw_priv_match_names <- function(have, wanted) {
  out <- character(length(wanted)); how <- character(length(wanted))
  for (i in seq_along(wanted)) {
    ex <- have[have == wanted[i]]
    if (length(ex) == 1L) { out[i] <- ex; how[i] <- "exact"; next }
    ci <- have[tolower(have) == tolower(wanted[i])]
    if (length(ci) == 1L) { out[i] <- ci; how[i] <- "case_insensitive" } else if (length(ci) > 1L) { out[i] <- NA_character_; how[i] <- "ambiguous" } else { out[i] <- NA_character_; how[i] <- "missing" }
  }
  data.frame(source = wanted, matched = out, how = how, stringsAsFactors = FALSE)
}

# ---- conversion of an imported data frame (pure function; no file access) ----------------------------------------------
# raw: data frame as returned by read.spss (use.missings = TRUE); raw_keep: same read with use.missings = FALSE (optional, for the audit)
# Returns list(data, audit) with audit rows (check_id, category, status, observed, detail); status FAIL means the CSV must not be written.
siw_priv_convert <- function(raw, config, raw_keep = NULL, expected_rows = NA_integer_) {
  A <- list(); add <- function(id, cat, status, observed, detail = "") A[[length(A) + 1L]] <<- data.frame(check_id = id, category = cat, status = status, observed = as.character(observed), detail = detail, stringsAsFactors = FALSE)
  map <- siw_priv_mapping(config); nm <- names(raw)
  add("C02", "rows", if (is.na(expected_rows)) "INFO" else if (nrow(raw) == expected_rows) "PASS" else "FAIL", nrow(raw), if (is.na(expected_rows)) "no expected row count supplied" else paste0("expected ", expected_rows))
  add("C03", "columns", "INFO", length(nm), "source variables in the SAV")
  mt <- siw_priv_match_names(nm, map$source)
  bad <- mt$how %in% c("missing", "ambiguous")
  add("C04", "required_source_variables", if (any(bad)) "FAIL" else "PASS", sum(!bad), if (any(bad)) paste0("missing or ambiguous (", sum(bad), "): ", paste(utils::head(mt$source[bad], 20), collapse = ", ")) else paste0("all ", nrow(map), " required source variables found"))
  add("C05", "name_matching", "INFO", sum(mt$how == "case_insensitive"), "number of source names matched only case-insensitively")
  if (any(bad)) return(list(data = NULL, audit = do.call(rbind, A), map = map))
  out <- list(); ntype_bad <- character()
  for (i in seq_len(nrow(map))) {
    x <- raw[[mt$matched[i]]]
    if (map$canonical[i] == "participant_code") { out[[map$canonical[i]]] <- trimws(as.character(x)); next }
    if (is.factor(x) || is.character(x)) { ntype_bad <- c(ntype_bad, map$source[i]); next }
    out[[map$canonical[i]]] <- as.numeric(x)
  }
  add("C07", "source_types", if (length(ntype_bad)) "FAIL" else "PASS", length(ntype_bad), if (length(ntype_bad)) paste0("non-numeric source variables: ", paste(utils::head(ntype_bad, 20), collapse = ", ")) else "all numeric sources are numeric")
  if (length(ntype_bad)) return(list(data = NULL, audit = do.call(rbind, A), map = map))
  dat <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE); rownames(dat) <- NULL
  want <- siw_priv_canonical_columns(config)
  dat <- dat[, want, drop = FALSE]
  add("C15", "column_order", if (identical(names(dat), want) && ncol(dat) == length(want)) "PASS" else "FAIL", ncol(dat), "canonical columns in the schema order")
  pc <- dat$participant_code; nb <- sum(is.na(pc) | !nzchar(pc)); nd <- sum(duplicated(pc[!is.na(pc)]))
  add("C06", "participant_identifier", if (nb || nd) "FAIL" else "PASS", nrow(dat), paste0("missing or blank identifiers=", nb, "; duplicated identifiers=", nd))
  # row order: canonical rows are the source rows in file order
  add("C13", "row_order", if (identical(pc, trimws(as.character(raw[[mt$matched[map$canonical == "participant_code"]]])))) "PASS" else "FAIL", nrow(dat), "identifier sequence equals the source sequence (no sorting, no filtering)")
  # user-defined missing values (aggregate counts only)
  if (!is.null(raw_keep)) {
    cells <- 0L; vars <- 0L
    for (i in which(map$type == "numeric")) { a <- raw[[mt$matched[i]]]; b <- raw_keep[[mt$matched[i]]]; k <- sum(is.na(a) & !is.na(b)); if (k) { cells <- cells + k; vars <- vars + 1L } }
    add("C09", "user_defined_missing", "INFO", cells, paste0("cells converted to NA because they carry a user-defined missing code, in ", vars, " canonical source variables (legacy rule: use.missings = TRUE)"))
  } else add("C09", "user_defined_missing", "INFO", NA, "not assessed")
  nna <- vapply(dat, function(v) sum(is.na(v)), 1L)
  add("C10", "missing_cells", "INFO", sum(nna), paste0("NA cells across canonical columns; columns with any NA=", sum(nna > 0)))
  lab <- sum(vapply(unique(mt$matched), function(n) !is.null(attr(raw[[n]], "value.labels")), logical(1)))
  add("C11", "value_labels", "INFO", lab, "canonical source variables carrying SPSS value labels; labels are not applied, numeric codes are kept (legacy: use.value.labels = FALSE)")
  add("C16", "excluded_variables", "INFO", length(setdiff(nm, unique(mt$matched))), "source variables not exported (not required by the publication analyses; includes dates, addresses, coordinates and other unused fields)")
  pat <- "address|coord|latitude|longitude|street|(^|_)lat($|_)|(^|_)lon($|_)|date|phone|name$"
  add("C17", "confidential_field_names", if (any(grepl(pat, names(dat), ignore.case = TRUE))) "FAIL" else "PASS", sum(grepl(pat, names(dat), ignore.case = TRUE)), "canonical column names matching address/coordinate/date patterns")
  # package validation (aggregate report)
  v <- siw_validate_input(dat, config, lag_end = 59L)
  for (i in seq_len(nrow(v))) add(paste0("VAL_", v$check_id[i]), paste0("input_validation/", v$category[i]), v$status[i], v$n_affected[i], v$detail[i])
  audit <- do.call(rbind, A)
  list(data = dat, audit = audit, map = map, n_na = nna)
}

siw_priv_canonical_columns <- function(config) c(siw_input_spec()$column, siw_daily_columns(config, 59L), siw_cumulative_columns(config))

# ---- exact-round-trip number formatting (shortest of 15/16/17 significant digits that parses back to the same double) ---
siw_priv_fmt_num <- function(x) {
  s <- rep("", length(x)); ok <- is.finite(x)
  if (any(!ok & !is.na(x))) stop("SIW_NONFINITE_VALUE", call. = FALSE)
  v <- x[ok]; r <- sprintf("%.15g", v); bad <- as.numeric(r) != v
  if (any(bad)) { r[bad] <- sprintf("%.16g", v[bad]); bad2 <- as.numeric(r) != v; if (any(bad2)) r[bad2] <- sprintf("%.17g", v[bad2]) }
  s[ok] <- r; s
}

siw_priv_write_canonical_csv <- function(dat, path) {
  cols <- lapply(dat, function(v) if (is.numeric(v)) siw_priv_fmt_num(v) else ifelse(is.na(v), "", v))
  df <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  utils::write.table(df, path, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE, na = "", qmethod = "double", fileEncoding = "UTF-8", eol = "\n")
  invisible(path)
}

# Read the written CSV back with the package reader and compare with the in-memory data (exact for numerics).
siw_priv_roundtrip_check <- function(dat, path, config) {
  back <- siw_read_study_csv(path, config)
  same_names <- identical(names(back), names(dat))
  same_num <- same_names && all(vapply(names(dat), function(n) if (is.numeric(dat[[n]])) identical(as.numeric(back[[n]]), as.numeric(dat[[n]])) else identical(back[[n]], dat[[n]]), logical(1)))
  list(ok = isTRUE(same_num) && nrow(back) == nrow(dat), n_rows = nrow(back))
}

# ---- data dictionary (JSON; variable names and aggregate counts only) --------------------------------------------------
siw_priv_dictionary <- function(map, dat, n_na) {
  cols <- lapply(seq_len(nrow(map)), function(i) list(name = map$canonical[i], source_variable = map$source[i], type = map$type[i], role = map$role[i],
    allowed_values = if (is.na(map$allowed[i])) NULL else as.numeric(strsplit(map$allowed[i], ",")[[1]]), transform = map$transform[i],
    n_rows = nrow(dat), n_missing = unname(n_na[map$canonical[i]])))
  list(schema = "sieccwinter canonical study CSV", version = "1.0.0", n_rows = nrow(dat), n_columns = ncol(dat), row_order = "source (SAV file) order",
       missing_encoding = "empty cell", numeric_format = "shortest of 15/16/17 significant digits that round-trips exactly", columns = cols)
}

# ---- path safety ------------------------------------------------------------------------------------------------------
siw_priv_norm <- function(p) tolower(gsub("/+$", "", normalizePath(p, winslash = "/", mustWork = FALSE)))
siw_priv_inside <- function(child, parent) { c <- siw_priv_norm(child); p <- siw_priv_norm(parent); c == p || startsWith(c, paste0(p, "/")) }
siw_priv_output_names <- function() c("siecc_winter_canonical.csv", "canonical_data_dictionary.json", "CONVERSION_AUDIT.csv", "CONVERSION_SUMMARY.md", "SESSION_INFO.txt", "CONVERSION_MANIFEST.csv")
