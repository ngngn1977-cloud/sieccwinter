# CSV import, type enforcement, validation (report only; values are never corrected) and derivation of analysis variables.

#' Specification of the canonical study CSV (scalar columns). Daily and cumulative exposure columns are added from the configuration.
#' @export
siw_input_spec <- function() {
  s <- function(column, type, allowed = NA_character_, role) data.frame(column = column, type = type, allowed = allowed, role = role, stringsAsFactors = FALSE)
  rbind(
    s("participant_code", "character", NA, "identifier"),
    s("UCLA_Lonliness_SCORE", "numeric", NA, "outcome"),
    s("Isolation_cont", "numeric", NA, "outcome"),
    s("integration_score", "numeric", "1,2,3,4", "outcome"),
    s("int_dummy", "numeric", "0,1", "outcome"),
    s("Sex", "numeric", "0,1", "covariate"),
    s("Age", "numeric", NA, "covariate"),
    s("assessment_day", "numeric", NA, "covariate"),
    s("Area", "numeric", "1,2,3", "covariate"),
    s("Econ_STS", "numeric", "0,1", "covariate"),
    s("Jewish", "numeric", "0,1", "covariate"),
    s("Muslim", "numeric", "0,1", "covariate"),
    s("Christian", "numeric", "0,1", "covariate"),
    s("PSQI", "numeric", NA, "covariate"),
    s("Subjective_Health", "numeric", "0,1,2,3,4", "covariate"),
    s("Memory", "numeric", "1,2,3,4,5", "covariate"),
    s("Memory_Changed", "numeric", "1,2,3", "covariate"),
    s("Subjective_Wellbeing", "numeric", "0,1,2,3,4,5", "covariate"),
    s("Outdoor_Mobility", "numeric", NA, "covariate"),
    s("PedestLength_m", "numeric", NA, "covariate"),
    s("Memory_assess", "numeric", "0,1", "covariate"),
    s("chronic_domain_count", "numeric", NA, "covariate")
  )
}

#' Names of the required daily exposure columns for the first `lag_end` lags of every metric.
#' @export
siw_daily_columns <- function(config, lag_end = 56L) {
  unlist(lapply(config$metrics$daily_prefix, siw_daily_names, lag_end = lag_end), use.names = FALSE)
}

#' Names of the optional cumulative exposure columns.
#' @export
siw_cumulative_columns <- function(config) {
  unlist(lapply(config$metrics$cumulative_prefix, function(p) siw_cum_name(p, config$windows)), use.names = FALSE)
}

#' Read a canonical study CSV. Numeric columns are coerced; the number of entries that could not be parsed is recorded, not hidden.
#' No SPSS file is ever read.
#' @export
siw_read_study_csv <- function(path, config) {
  if (grepl("\\.(sav|zsav|por)$", path, ignore.case = TRUE)) stop("SIW_SPSS_FILES_NOT_SUPPORTED: convert to the canonical CSV first", call. = FALSE)
  if (!file.exists(path)) stop("SIW_INPUT_NOT_FOUND", call. = FALSE)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "NaN"), colClasses = "character")
  spec <- siw_input_spec()
  num_cols <- intersect(c(spec$column[spec$type == "numeric"], siw_daily_columns(config, 59L), siw_cumulative_columns(config)), names(raw))
  parse_fail <- integer()
  for (cn in num_cols) {
    v <- raw[[cn]]; nv <- suppressWarnings(as.numeric(v))
    bad <- sum(!is.na(v) & is.na(nv)); if (bad) parse_fail[cn] <- bad
    raw[[cn]] <- nv
  }
  if ("participant_code" %in% names(raw)) raw$participant_code <- trimws(raw$participant_code)
  attr(raw, "parse_failures") <- parse_fail
  raw
}

.chk <- function(id, category, status, n, detail) data.frame(check_id = id, category = category, status = status, n_affected = as.integer(n), detail = detail, stringsAsFactors = FALSE)

#' Validate an imported study data frame. Returns a report; nothing is corrected and no row values are reported.
#' @param lag_end Highest lag that must be present (56 for the final analysis, 59 for the archived branch).
#' @export
siw_validate_input <- function(raw, config, lag_end = 56L) {
  R <- list(); add <- function(x) R[[length(R) + 1L]] <<- x
  spec <- siw_input_spec()
  add(.chk("V00", "structure", "PASS", nrow(raw), paste0("rows=", nrow(raw), "; columns=", ncol(raw))))
  miss <- setdiff(spec$column, names(raw))
  add(.chk("V01", "required_variables", if (length(miss)) "FAIL" else "PASS", length(miss), if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else "all required scalar variables present"))
  if ("participant_code" %in% names(raw)) {
    pc <- raw$participant_code; nbad <- sum(is.na(pc) | !nzchar(pc)); nd <- sum(duplicated(pc[!is.na(pc)]))
    add(.chk("V02", "participant_codes", if (nbad || nd) "FAIL" else "PASS", nbad + nd, paste0("missing or blank codes=", nbad, "; duplicated codes=", nd)))
  }
  pf <- attr(raw, "parse_failures")
  add(.chk("V03", "type_enforcement", if (length(pf)) "FAIL" else "PASS", sum(pf), if (length(pf)) paste0("non-numeric entries in: ", paste(names(pf), collapse = ", ")) else "all numeric columns parse"))
  for (i in which(!is.na(spec$allowed) & spec$column %in% names(raw))) {
    col <- spec$column[i]; al <- as.numeric(strsplit(spec$allowed[i], ",")[[1]]); v <- raw[[col]]
    bad <- sum(!is.na(v) & !(v %in% al))
    add(.chk(paste0("V04_", col), "allowed_values", if (bad) "FAIL" else "PASS", bad, paste0(col, ": allowed {", spec$allowed[i], "}; unexpected values=", bad, "; missing=", sum(is.na(v)))))
  }
  dn <- siw_daily_columns(config, lag_end); dmiss <- setdiff(dn, names(raw))
  add(.chk("V05", "lag_fields_present", if (length(dmiss)) "FAIL" else "PASS", length(dmiss), if (length(dmiss)) paste0("missing daily columns: ", length(dmiss), " (first: ", paste(utils::head(dmiss, 3), collapse = ", "), ")") else paste0("all ", length(dn), " daily columns L01-L", sprintf("%02d", lag_end), " present")))
  have <- intersect(dn, names(raw))
  if (length(have)) {
    H <- as.matrix(raw[, have, drop = FALSE]); storage.mode(H) <- "double"
    add(.chk("V06", "lag_fields_finite", if (any(is.infinite(H))) "FAIL" else "PASS", sum(is.infinite(H)), paste0("infinite values=", sum(is.infinite(H)), "; missing cells=", sum(is.na(H)), " of ", length(H))))
    allna <- have[colSums(!is.na(H)) == 0L]
    add(.chk("V07", "lag_fields_all_missing", if (length(allna)) "WARN" else "PASS", length(allna), paste0("columns that are entirely missing=", length(allna))))
    rt <- intersect(siw_daily_names("IMS_IDW_RAIN_TOTAL", lag_end), have); ri <- intersect(siw_daily_names("IMS_IDW_RAIN_DAYS1", lag_end), have)
    neg <- if (length(rt)) sum(as.matrix(raw[, rt, drop = FALSE]) < 0, na.rm = TRUE) else 0L
    nb <- if (length(ri)) sum(!(as.matrix(raw[, ri, drop = FALSE]) %in% c(0, 1)) & !is.na(as.matrix(raw[, ri, drop = FALSE]))) else 0L
    add(.chk("V08", "rainfall_field_validity", if (neg || nb) "FAIL" else "PASS", neg + nb, paste0("negative rainfall totals=", neg, "; rainy-day indicators outside {0,1}=", nb)))
    rh <- intersect(c(siw_daily_names("RH", lag_end), siw_daily_names("IMS_IDW_RH_MEAN", lag_end)), have)
    nrh <- if (length(rh)) sum(as.matrix(raw[, rh, drop = FALSE]) < 0 | as.matrix(raw[, rh, drop = FALSE]) > 100, na.rm = TRUE) else 0L
    add(.chk("V09", "relative_humidity_range", if (nrh) "WARN" else "PASS", nrh, paste0("values outside 0-100=", nrh)))
  }
  if (all(c("int_dummy", "integration_score") %in% names(raw))) {
    d <- sum(!is.na(raw$int_dummy) & !is.na(raw$integration_score) & raw$int_dummy != as.integer(raw$integration_score == 4))
    add(.chk("V10", "outcome_coding", if (d) "FAIL" else "PASS", d, paste0("high integration (int_dummy) disagrees with integration_score == 4 for ", d, " participants")))
  }
  if (all(c("Jewish", "Muslim", "Christian") %in% names(raw))) {
    nj <- !is.na(raw$Jewish) & raw$Jewish == 0
    bad <- sum(nj & !((raw$Muslim %in% 1 & raw$Christian %in% 0) | (raw$Muslim %in% 0 & raw$Christian %in% 1))) + sum(!is.na(raw$Jewish) & raw$Jewish == 1 & (raw$Muslim %in% 1 | raw$Christian %in% 1))
    add(.chk("V11", "ethnicity_coding", if (bad) "WARN" else "PASS", bad, paste0("participants whose Jewish / Muslim / Christian indicators are inconsistent (non-Jewish not exactly one of Muslim or Christian, or Jewish also coded Muslim or Christian)=", bad)))
  }
  if (all(c("Memory_assess", "Memory") %in% names(raw))) {
    d <- sum(!is.na(raw$Memory_assess) & !is.na(raw$Memory) & raw$Memory_assess != as.integer(raw$Memory %in% 1:3))
    add(.chk("V12", "memory_assess_coding", if (d) "WARN" else "PASS", d, paste0("Memory_assess disagrees with (self-rated memory in 1-3) for ", d, " participants")))
  }
  cn <- siw_cumulative_columns(config); have_c <- intersect(cn, names(raw))
  if (length(have_c) == 0L) add(.chk("V13", "cumulative_fields", "PASS", 0, "no cumulative fields supplied: they are reconstructed from the daily fields"))
  else if (length(have_c) < length(cn)) add(.chk("V13", "cumulative_fields", "FAIL", length(cn) - length(have_c), paste0("cumulative fields partially supplied (", length(have_c), " of ", length(cn), "): supply all or none")))
  else if (!length(dmiss)) {
    rec <- siw_reconstruct_cumulative(raw, config); d <- 0L
    for (nm in cn) { a <- raw[[nm]]; b <- rec[[nm]]; d <- d + sum(!((is.na(a) & is.na(b)) | (is.finite(a) & is.finite(b) & abs(a - b) <= 1e-7 + 1e-9 * abs(b)))) }
    add(.chk("V13", "cumulative_fields", if (d) "WARN" else "PASS", d, paste0("supplied cumulative cells that differ from the reconstruction from daily fields=", d, " (supplied values are used as given)")))
  }
  an <- siw_rainfall_anomalies(raw, config, lag_end = lag_end)
  add(.chk("V14", "rainfall_total_missing_with_indicator_zero", if (an$n_cells) "WARN" else "PASS", an$n_cells, paste0("cells where the rainfall total is missing but the rainy-day indicator is 0: ", an$n_cells, " (see rainfall_anomaly_report.csv); values are not altered")))
  out <- do.call(rbind, R); rownames(out) <- NULL; out
}

#' Derive the analysis variables from the canonical columns. Inputs are never overwritten and nothing is silently corrected.
#' @export
siw_prepare_data <- function(raw, config) {
  d <- raw
  d$Sex <- as.numeric(raw$Sex)
  d$Age <- as.numeric(raw$Age)
  d$AssessmentDay <- as.numeric(raw$assessment_day)
  d$AssessmentDay_centered <- d$AssessmentDay - mean(d$AssessmentDay, na.rm = TRUE)
  d$Locality_3group <- factor(raw$Area, levels = config$coding$locality_codes, labels = config$coding$locality_levels)
  d$Econ_STS <- as.numeric(raw$Econ_STS)
  d$Ethnicity <- as.numeric(raw$Jewish)
  d$Int_dummy <- as.numeric(raw$int_dummy)
  d$PedestLength_km <- as.numeric(raw$PedestLength_m) / 1000
  d$Memory_assess <- as.numeric(raw$Memory_assess)
  for (cn in c("PSQI", "Subjective_Health", "Memory", "Memory_Changed", "Subjective_Wellbeing", "Outdoor_Mobility", "chronic_domain_count", "UCLA_Lonliness_SCORE", "Isolation_cont")) d[[cn]] <- as.numeric(raw[[cn]])
  d
}
