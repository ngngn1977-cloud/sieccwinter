# Post hoc steps 27-29 of config/posthoc_sensitivity_protocol_v2.json (declared 14 Oct 2026, after the 30 July 2026 family lock):
#   PH_EXPOSURE_QA (RECONSTRUCTION) station coverage, distances and the DST / 1-minute-rain rules for the analysed IMS run (25 Jul 2026 toolkit V8.1), with the required
#     source-identity check (the run's daily IDW TD, RH and rainfall equal the canonical CSV);
#   PH_VIF_SUMMARY (RECONSTRUCTION) maximum and per-column VIFs of the M2-L cumulative models (the Batch A definition);
#   PH_CALENDAR_SPLINE (SENSITIVITY) the 90 representative M2-L models with splines::ns(assessment_day, df = 2, 3) for the linear assessment-day term.
# Read only for q90, q9, q9-nested and every accepted status. Every output is aggregate: participant codes and dates are used only to join inside R.

SIW_GIS_ROLE <- "gis_ims_v8_1_reference"

# SI / manuscript concordance rows of the v2 steps (columns item, quantity, package_value, manuscript_value, decimals, agrees, note)
.siw_ph_ms <- function(sic, synthetic) { if (synthetic) sic$agrees <- NA; names(sic)[names(sic) == "si_value"] <- "manuscript_value"; rownames(sic) <- NULL; sic }
.siw_ph_flag <- function(item, quantity, ok, note = "") .siw_ph_si_row(item, quantity, if (is.na(ok)) NA_real_ else as.numeric(isTRUE(ok)), 1, 0, note)

# ---- reference files of a role: a folder (declared names) or several files separated by "|" (matched by SHA-256) ------------------------------------------------

#' Declared files of one post hoc reference role resolved to paths. `ref` is a folder holding the declared names, or files separated by "|" that are matched to the
#' declared files by SHA-256 (the launcher passes the identified files; participant-level files are never staged or copied). Unresolved files are NA.
#' @export
siw_posthoc_role_files <- function(ref, protocol, role) {
  specs <- Filter(function(z) identical(z$role, role), protocol$reference_files); nm <- vapply(specs, function(z) z$file, ""); out <- stats::setNames(rep(NA_character_, length(nm)), nm)
  if (is.null(ref) || !nzchar(ref)) return(out)
  p <- unlist(strsplit(ref, "|", fixed = TRUE)); p <- p[nzchar(p)]
  if (length(p) == 1L && dir.exists(p)) { f <- file.path(p, nm); out[file.exists(f)] <- f[file.exists(f)]; return(out) }
  sh <- toupper(vapply(specs, function(z) z$sha256, "")); for (x in p[file.exists(p) & !dir.exists(p)]) { j <- match(toupper(siw_file_hash(x)$value), sh); if (!is.na(j)) out[j] <- x }
  out
}

# existence, SHA-256 and bytes of every declared file of a role; status NOT_TESTED when the role was not given at all
.siw_ph_verify_role <- function(paths, protocol, role, given) {
  specs <- Filter(function(z) identical(z$role, role), protocol$reference_files); rows <- list()
  if (!given) return(list(status = "NOT_TESTED", provenance = data.frame(file = names(paths), item = "reference role", value = "not given", ok = FALSE, stringsAsFactors = FALSE)))
  for (s in specs) { f <- paths[[s$file]]; ex <- !is.na(f) && file.exists(f); rows[[length(rows) + 1L]] <- data.frame(file = s$file, item = "file identified", value = as.character(ex), ok = ex, stringsAsFactors = FALSE)
    if (ex) { h <- siw_file_hash(f)$value; b <- file.info(f)$size
      rows[[length(rows) + 1L]] <- data.frame(file = s$file, item = c("SHA-256 equals the declared value", "bytes equal the declared value"), value = c(paste0("found ", h, "; declared ", s$sha256), paste0("found ", b, "; declared ", s$bytes)),
                                              ok = c(identical(toupper(h), toupper(s$sha256)), isTRUE(b == s$bytes)), stringsAsFactors = FALSE) } }
  pv <- do.call(rbind, rows); list(status = if (all(pv$ok)) "PASS" else "FAIL", provenance = pv)
}

# ---- (27) exposure QA ------------------------------------------------------------------------------------------------------------------------------------------

.siw_num <- function(x) suppressWarnings(as.numeric(x))
.siw_read_cols <- function(f, cols) { d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character", na.strings = c("", "NA"))
  miss <- setdiff(cols, names(d)); if (length(miss)) stop("SIW_EXPOSURE_QA_SCHEMA: ", basename(f), " lacks ", paste(miss, collapse = ", "), call. = FALSE); d[, cols, drop = FALSE] }

#' Non-missing value counts per station and Jerusalem local date of one raw 10-minute IMS file (CSV or .csv.gz): columns station_id, JERUSALEM_LOCAL_DATE (first 10
#' characters), n_td (TD_C), n_rh (RH_PCT), n_rain (RAIN_MM). Only these five columns are read; nothing else is kept.
#' @export
siw_raw_station_day_counts <- function(f) {
  con <- if (grepl("[.]gz$", f)) gzfile(f, "rt") else file(f, "rt"); h <- tryCatch(readLines(con, n = 1L), finally = close(con)); h <- gsub('"', "", strsplit(h, ",", fixed = TRUE)[[1L]])
  keep <- c("station_id", "JERUSALEM_LOCAL_DATE", "TD_C", "RH_PCT", "RAIN_MM"); if (length(setdiff(keep, h))) stop("SIW_EXPOSURE_QA_SCHEMA: ", basename(f), " lacks ", paste(setdiff(keep, h), collapse = ", "), call. = FALSE)
  cc <- ifelse(h %in% keep, "character", "NULL"); d <- utils::read.csv(f, colClasses = cc, na.strings = c("", "NA"), check.names = FALSE)
  key <- paste(trimws(d$station_id), substr(d$JERUSALEM_LOCAL_DATE, 1L, 10L), sep = "|"); m <- cbind(n_td = is.finite(.siw_num(d$TD_C)), n_rh = is.finite(.siw_num(d$RH_PCT)), n_rain = is.finite(.siw_num(d$RAIN_MM))) * 1
  s <- rowsum(m, key, reorder = FALSE); k <- do.call(rbind, strsplit(rownames(s), "|", fixed = TRUE))
  data.frame(station_id = k[, 1L], date = k[, 2L], n_td = s[, "n_td"], n_rh = s[, "n_rh"], n_rain = s[, "n_rain"], stringsAsFactors = FALSE, row.names = NULL)
}

#' Read the analysed GIS inputs (declared files of role gis_ims_v8_1_reference) into the minimal tables used by siw_exposure_qa (no other column is kept).
#' Each declared file carries a content tag in the protocol (daily, audit, plan, period_coverage, raw_10min); file names are never hard-coded (they are private).
#' @export
siw_read_gis_inputs <- function(paths, protocol, role = SIW_GIS_ROLE) {
  tg <- .siw_role_tags(protocol, role); g <- function(tag) { f <- paths[names(tg)[tg == tag]]; if (length(f) != 1L || is.na(f)) stop("SIW_EXPOSURE_QA_INPUT: one declared file with content tag ", tag, " is required", call. = FALSE); unname(f) }
  raw <- paths[names(tg)[tg == "raw_10min"]]; if (!length(raw) || anyNA(raw)) stop("SIW_EXPOSURE_QA_INPUT: the raw 10-minute files are required", call. = FALSE)
  cnt <- do.call(rbind, lapply(raw, siw_raw_station_day_counts)); cnt <- stats::aggregate(cbind(n_td, n_rh, n_rain) ~ station_id + date, data = cnt, FUN = sum)
  list(daily = .siw_read_cols(g("daily"), c("FEATURE_ID", "METHOD", "LOCAL_DATE", "TD_MEAN", "RH_MEAN", "RAIN_TOTAL", "TD_VALID", "RH_VALID")),
       audit = .siw_read_cols(g("audit"), c("FEATURE_ID", "VARIABLE", "METHOD", "STATION_ID", "DISTANCE_M", "PERIOD_COVERAGE_PCT", "PLAN_INCLUDED")),
       plan = .siw_read_cols(g("plan"), c("FEATURE_ID", "VARIABLE", "METHOD", "AVAILABLE_STATIONS")),
       coverage = .siw_read_cols(g("period_coverage"), c("STATION_ID", "VARIABLE_CODE", "valid_count", "coverage_pct")), counts = cnt,
       stations = siw_read_station_stages(g("stage_a"), g("stage_b")))
}

#' Station counts of the analysed IMS run from its two station-level session records (stage A coordinate metadata, stage B observation retrieval).
#' Stops if either file holds a participant-level field (participant, feature id, address, coordinates).
#' @export
siw_read_station_stages <- function(stage_a, stage_b) {
  A <- jsonlite::fromJSON(stage_a, simplifyVector = FALSE); B <- jsonlite::fromJSON(stage_b, simplifyVector = FALSE)
  nm <- c(names(unlist(A)), names(unlist(B))); bad <- grepl("(?i)participant|feature_id|address|latitude|longitude|(^|[._])(lat|lon|x|y)$", nm, perl = TRUE)
  if (any(bad)) stop("SIW_EXPOSURE_QA_STATION_FILE_HAS_PARTICIPANT_FIELD: ", sum(bad), " field(s)", call. = FALSE)
  fs <- B$details$fetch_status; st <- setdiff(names(fs), "candidate_expansion"); n <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  data.frame(quantity = c("coordinate_valid_stations_stage_a", "station_inventory_stage_a", "stations_requested_stage_a", "metadata_candidate_union_stage_a", "stations_returned_stage_b", "fetch_status_stations_stage_b", "fetch_status_range_ok_stage_b"),
             value = c(n(A$details$coordinate_valid_stations), n(A$details$station_inventory), n(A$details$download_station_ids), n(A$details$metadata_candidate_union), n(B$details$completed_stations), length(st), sum(vapply(fs[st], function(z) identical(as.character(z), "range_ok"), NA))),
             stringsAsFactors = FALSE)
}

# content tag of every declared file of a role (named by the declared file name)
.siw_role_tags <- function(protocol, role) { s <- Filter(function(z) identical(z$role, role), protocol$reference_files); stats::setNames(vapply(s, function(z) z$content %||% "", ""), vapply(s, function(z) z$file, "")) }

.siw_var_class <- function(v) { u <- toupper(trimws(as.character(v))); ifelse(grepl("^TD", u), "TD", ifelse(grepl("^RH", u), "RH", ifelse(grepl("RAIN", u), "RAIN", "OTHER"))) }
.siw_is_idw <- function(m) { u <- toupper(trimws(as.character(m))); if (any(u == "IDW", na.rm = TRUE)) u %in% "IDW" else rep(TRUE, length(u)) }

#' Exposure QA of the analysed IMS run (post hoc step 27). `gis` as returned by siw_read_gis_inputs (or the synthetic equivalent); `it` the protocol item exposure_qa
#' (rules: DST day, 1-minute rain stations, coverage threshold). Returns aggregate tables only.
#' @export
siw_exposure_qa <- function(dat, gis, it, protocol = siw_posthoc_protocol(), lag_end = 56L) {
  r <- it$rules; dst <- r$dst_day; one_min <- as.character(unlist(r$one_minute_rain_stations)); thr <- r$coverage_threshold
  pc <- trimws(as.character(dat$participant_code)); ld <- siw_lag_date(dat$AssessmentDay, seq_len(lag_end), protocol)
  pd <- data.frame(pid = rep(pc, lag_end), lag = rep(seq_len(lag_end), each = length(pc)), date = as.vector(ld), stringsAsFactors = FALSE)
  pd$group <- if (!is.null(dat$Locality_3group)) rep(as.character(dat$Locality_3group), lag_end) else "ALL"
  # source identity: the run's daily IDW values equal the canonical CSV
  dl <- gis$daily[.siw_is_idw(gis$daily$METHOD), , drop = FALSE]; dk <- paste(trimws(dl$FEATURE_ID), substr(dl$LOCAL_DATE, 1L, 10L)); j <- match(paste(pd$pid, pd$date), dk)
  idn <- do.call(rbind, lapply(list(c("TD_MEAN", "IMS_IDW_TD_MEAN"), c("RH_MEAN", "IMS_IDW_RH_MEAN"), c("RAIN_TOTAL", "IMS_IDW_RAIN_TOTAL")), function(v) {
    a <- .siw_num(dl[[v[1]]])[j]; b <- as.vector(as.matrix(dat[, siw_daily_names(v[2], lag_end), drop = FALSE])); ff <- is.finite(a) & is.finite(b); d <- abs(a - b)[ff]
    q <- function(p) if (length(d)) unname(stats::quantile(d, p, type = 7)) else NA_real_; r8a <- abs(round(a, 8) - b)[ff]; r8b <- abs(a - round(b, 8))[ff]
    data.frame(gis_column = v[1], canonical_prefix = v[2], participant_days = nrow(pd), rows_found = sum(!is.na(j)), both_finite = sum(ff), equal_1e_9 = sum(d <= 1e-9), missing_pattern_mismatch = sum(is.finite(a) != is.finite(b)),
               max_abs_difference = if (length(d)) max(d) else NA_real_, max_abs_difference_text = if (length(d)) sprintf("%.3e", max(d)) else "", differ_gt_1e_9 = sum(d > 1e-9), exactly_equal = sum(d == 0),
               diff_median = q(0.5), diff_p99 = q(0.99), differ_gt_5e_9 = sum(d > 5e-9), differ_gt_1e_8 = sum(d > 1e-8), differ_gt_1e_6 = sum(d > 1e-6),
               equal_after_rounding_run_to_8_decimals = sum(r8a <= 1e-12), equal_after_rounding_canonical_to_8_decimals = sum(r8b <= 1e-12), equal_after_rounding_both_to_8_decimals = sum(abs(round(a, 8) - round(b, 8))[ff] <= 1e-12), stringsAsFactors = FALSE) }))
  base_ok <- idn$rows_found == idn$participant_days & idn$missing_pattern_mismatch == 0L & idn$both_finite > 0L
  idn$strict_1e9_status <- ifelse(base_ok & idn$equal_1e_9 == idn$both_finite, "PASS", "FAIL")                      # informational, kept for the record
  idn$stored_precision_status <- ifelse(base_ok & !is.na(idn$max_abs_difference) & idn$max_abs_difference <= 1e-8, "PASS", "FAIL")   # governing (author decisions 14 Oct 2026; second amendment: the 8-decimal rounding-equality condition was ill-posed at rounding boundaries; rounding counts informational)
  # assigned stations (METHOD IDW, PLAN_INCLUDED 1): TD (also used for RH) and rain
  au <- gis$audit; au <- au[.siw_is_idw(au$METHOD) & toupper(trimws(as.character(au$PLAN_INCLUDED))) %in% c("1", "1.0", "TRUE") & trimws(au$FEATURE_ID) %in% pc, , drop = FALSE]
  au$class <- .siw_var_class(au$VARIABLE); au$km <- .siw_num(au$DISTANCE_M) / 1000; au$STATION_ID <- trimws(as.character(au$STATION_ID)); au$FEATURE_ID <- trimws(au$FEATURE_ID)
  labels <- as.data.frame(table(variable_label = au$VARIABLE, class = au$class), stringsAsFactors = FALSE); labels <- labels[labels$Freq > 0, , drop = FALSE]; names(labels)[3] <- "assigned_rows"
  cn <- gis$counts; ckey <- paste(trimws(cn$station_id), cn$date)
  cover <- function(cls, col) { a <- au[au$class == cls, , drop = FALSE]; st <- split(a$STATION_ID, a$FEATURE_ID)
    vapply(seq_len(nrow(pd)), function(i) { s <- st[[pd$pid[i]]]; if (is.null(s)) return(NA_integer_); e <- ifelse(s %in% one_min & col == "n_rain", if (pd$date[i] == dst) r$expected_1min_dst else r$expected_1min, if (pd$date[i] == dst) r$expected_10min_dst else r$expected_10min)
      n <- cn[[col]][match(paste(s, pd$date[i]), ckey)]; n[is.na(n)] <- 0; as.integer(sum(n / e >= thr)) }, 0L) }
  pd$td_cover <- cover("TD", "n_td"); pd$rh_cover <- cover("TD", "n_rh"); pd$rain_cover <- cover("RAIN", "n_rain")
  tab <- function(v, g = NULL) { k <- if (is.null(g)) rep("ALL", length(v)) else g; do.call(rbind, lapply(split(v, k), function(z) data.frame(n_0 = sum(z == 0, na.rm = TRUE), n_1 = sum(z == 1, na.rm = TRUE), n_2 = sum(z == 2, na.rm = TRUE), n_3 = sum(z == 3, na.rm = TRUE), not_assigned = sum(is.na(z))))) }
  cov_tab <- do.call(rbind, lapply(c("ALL", "BY_GROUP"), function(m) do.call(rbind, lapply(c(td = "td_cover", rh = "rh_cover", rain = "rain_cover"), function(v) { t <- tab(pd[[v]], if (m == "ALL") NULL else pd$group); data.frame(scope = m, group = rownames(t), variable = sub("_cover", "", v), t, participant_days = rowSums(t), stringsAsFactors = FALSE, row.names = NULL) }))))
  cov_tab$share_with_3 <- cov_tab$n_3 / cov_tab$participant_days
  # interpolated-value support
  tv <- .siw_num(dl$TD_VALID)[j]; rv <- .siw_num(dl$RH_VALID)[j]; dstd <- pd$date == dst
  sup <- data.frame(quantity = c("min_TD_VALID_all_days", "min_TD_VALID_dst_day", "min_RH_VALID_all_days", "min_RH_VALID_dst_day", "dst_day_participant_days"),
                    value = c(suppressWarnings(min(tv, na.rm = TRUE)), suppressWarnings(min(tv[dstd], na.rm = TRUE)), suppressWarnings(min(rv, na.rm = TRUE)), suppressWarnings(min(rv[dstd], na.rm = TRUE)), sum(dstd)), stringsAsFactors = FALSE)
  sup$value[!is.finite(sup$value)] <- NA_real_
  # distances
  dsum <- function(cls, g = NULL) { a <- au[au$class == cls, , drop = FALSE]; if (!is.null(g)) a <- a[a$FEATURE_ID %in% pd$pid[pd$group == g], , drop = FALSE]; if (!nrow(a)) return(NULL)
    near <- tapply(a$km, a$FEATURE_ID, min); far <- tapply(a$km, a$FEATURE_ID, max)
    data.frame(variable = cls, group = g %||% "ALL", assigned_rows = nrow(a), participants = length(near), km_min = min(a$km), km_max = max(a$km), nearest_median = stats::median(near), nearest_min = min(near), nearest_max = max(near),
               farthest_median = stats::median(far), distinct_stations = length(unique(a$STATION_ID)), min_period_coverage_pct = suppressWarnings(min(.siw_num(a$PERIOD_COVERAGE_PCT), na.rm = TRUE)), stringsAsFactors = FALSE) }
  grp <- sort(unique(pd$group)); dist <- siw_bind_rows(c(list(dsum("TD"), dsum("RAIN")), unlist(lapply(grp, function(g) list(dsum("TD", g), dsum("RAIN", g))), recursive = FALSE)))
  # station pool: requested (coverage file stations), returned (any valid value), eligible (period coverage >= 80 %); AVAILABLE_STATIONS of the plan file
  cv <- gis$coverage; cv$class <- .siw_var_class(cv$VARIABLE_CODE); pct <- .siw_num(cv$coverage_pct); if (all(pct <= 1, na.rm = TRUE)) pct <- 100 * pct; cv$pct <- pct
  elig <- function(cls) length(unique(cv$STATION_ID[cv$class == cls & cv$pct >= 100 * thr]))
  pl <- gis$plan; pl <- pl[.siw_is_idw(pl$METHOD) & trimws(pl$FEATURE_ID) %in% pc, , drop = FALSE]; pl$class <- .siw_var_class(pl$VARIABLE); av <- function(cls) { v <- unique(.siw_num(pl$AVAILABLE_STATIONS[pl$class == cls])); if (!length(v)) NA_real_ else max(v) }
  avn <- function(cls) length(unique(.siw_num(pl$AVAILABLE_STATIONS[pl$class == cls])))
  pool <- data.frame(quantity = c("stations_requested_coverage_file", "stations_returned_any_valid", "eligible_TD_coverage_file", "eligible_RH_coverage_file", "eligible_rain_coverage_file", "eligible_TD_plan_file", "eligible_rain_plan_file", "plan_file_distinct_values_TD", "plan_file_distinct_values_rain"),
                     value = c(length(unique(cv$STATION_ID)), length(unique(cv$STATION_ID[.siw_num(cv$valid_count) > 0])), elig("TD"), elig("RH"), elig("RAIN"), av("TD"), av("RAIN"), avn("TD"), avn("RAIN")), stringsAsFactors = FALSE)
  if (!is.null(gis$stations)) pool <- rbind(pool, gis$stations)
  ok <- all(c("TD", "RAIN") %in% au$class) && all(table(au$FEATURE_ID[au$class == "TD"]) == 3L) && all(table(au$FEATURE_ID[au$class == "RAIN"]) == 3L) && length(unique(au$FEATURE_ID[au$class == "TD"])) == length(pc) && all(!is.na(j))
  list(identity = idn, coverage = cov_tab, support = sup, distances = dist, pool = pool, labels = labels, computation_ok = ok, participant_days = nrow(pd))
}

.siw_exposure_qa_si <- function(q, it) {
  e <- it$expected; a <- q$coverage[q$coverage$scope == "ALL", ]; g <- function(v, k) a[[k]][a$variable == v]; d <- q$distances[q$distances$group == "ALL", ]; dd <- function(v, k) { x <- d[[k]][d$variable == v]; if (length(x)) x else NA_real_ }
  pv <- function(k) q$pool$value[q$pool$quantity == k]; pv1 <- function(k) { x <- pv(k); if (length(x)) x else NA_real_ }; sv <- function(k) q$support$value[q$support$quantity == k]; I <- "exposure_qa"; S <- .siw_ph_si_row
  rows <- list(S(I, "participant-days analysed", q$participant_days, e$participant_days, 0),
    S(I, "source identity: TD both finite", q$identity$both_finite[1], e$identity_both_finite[[1]], 0), S(I, "source identity: RH both finite", q$identity$both_finite[2], e$identity_both_finite[[2]], 0), S(I, "source identity: rainfall both finite", q$identity$both_finite[3], e$identity_both_finite[[3]], 0),
    S(I, "TD participant-days with 3 stations covering", g("td", "n_3"), e$td_3, 0), S(I, "TD participant-days with 2 stations covering", g("td", "n_2"), e$td_2, 0), S(I, "TD share with 3 (percent)", 100 * g("td", "share_with_3"), e$td_share_3_pct, 1),
    S(I, "RH participant-days with 3 stations covering", g("rh", "n_3"), e$rh_3, 0), S(I, "RH participant-days with 2 stations covering", g("rh", "n_2"), e$rh_2, 0),
    S(I, "rain participant-days with 3 stations covering", g("rain", "n_3"), e$rain_3, 0), S(I, "rain participant-days with 2 stations covering", g("rain", "n_2"), e$rain_2, 0), S(I, "rain participant-days with 1 station covering", g("rain", "n_1"), e$rain_1, 0),
    S(I, "rain share with 3 (percent)", 100 * g("rain", "share_with_3"), e$rain_share_3_pct, 1),
    S(I, "minimum TD_VALID over analysed days", sv("min_TD_VALID_all_days"), e$min_td_valid, 0), S(I, "minimum TD_VALID on the DST day", sv("min_TD_VALID_dst_day"), e$min_td_valid_dst, 0),
    S(I, "TD/RH assigned rows", dd("TD", "assigned_rows"), e$td_assigned_rows, 0), S(I, "TD/RH distance min (km)", dd("TD", "km_min"), e$td_km_min, 1), S(I, "TD/RH distance max (km)", dd("TD", "km_max"), e$td_km_max, 1),
    S(I, "TD/RH nearest station per participant: median (km)", dd("TD", "nearest_median"), e$td_nearest_median, 1), S(I, "TD/RH nearest station per participant: min (km)", dd("TD", "nearest_min"), e$td_nearest_min, 1), S(I, "TD/RH nearest station per participant: max (km)", dd("TD", "nearest_max"), e$td_nearest_max, 1),
    S(I, "TD/RH farthest of three per participant: median (km)", dd("TD", "farthest_median"), e$td_farthest_median, 1),
    S(I, "rain distance min (km)", dd("RAIN", "km_min"), e$rain_km_min, 1), S(I, "rain distance max (km)", dd("RAIN", "km_max"), e$rain_km_max, 1),
    S(I, "distinct assigned stations TD/RH", dd("TD", "distinct_stations"), e$td_distinct, 0), S(I, "distinct assigned stations rain", dd("RAIN", "distinct_stations"), e$rain_distinct, 0),
    S(I, "minimum period coverage of assigned stations (percent)", suppressWarnings(min(d$min_period_coverage_pct, na.rm = TRUE)), e$min_period_coverage_pct, 1, "record only"),
    S(I, "coordinate-valid stations (stage A)", pv1("coordinate_valid_stations_stage_a"), e$coordinate_valid_stations, 0),
    S(I, "stations requested (stage A download_station_ids)", pv1("stations_requested_stage_a"), e$stations_requested, 0), S(I, "stations requested (stations in the stage B fetch_status)", pv1("fetch_status_stations_stage_b"), e$stations_requested, 0),
    S(I, "stations returned (stage B completed_stations)", pv1("stations_returned_stage_b"), e$stations_returned, 0),
    S(I, "stations in the period-coverage file", pv("stations_requested_coverage_file"), e$stations_requested, 0, "record only"),
    S(I, "stations with any valid value in the period-coverage file", pv("stations_returned_any_valid"), e$stations_returned, 0, "record only"),
    S(I, "eligible stations TD (coverage file, >= 80 %)", pv("eligible_TD_coverage_file"), e$eligible_td_rh, 0), S(I, "eligible stations RH (coverage file, >= 80 %)", pv("eligible_RH_coverage_file"), e$eligible_td_rh, 0),
    S(I, "eligible stations rain (coverage file, >= 80 %)", pv("eligible_rain_coverage_file"), e$eligible_rain, 0), S(I, "eligible stations TD (plan file AVAILABLE_STATIONS)", pv("eligible_TD_plan_file"), e$eligible_td_rh, 0),
    S(I, "eligible stations rain (plan file AVAILABLE_STATIONS)", pv("eligible_rain_plan_file"), e$eligible_rain, 0))
  do.call(rbind, rows)
}

# synthetic GIS inputs for the demo and the tests (derived from the shipped synthetic CSV; fixed artificial stations and gaps; no real data)
.siw_exposure_qa_synthetic <- function(dat, protocol, lag_end = 56L) {
  it <- siw_posthoc_item(protocol, "exposure_qa"); r <- it$rules; pc <- as.character(dat$participant_code); n <- length(pc); ld <- siw_lag_date(dat$AssessmentDay, seq_len(lag_end), protocol)
  g <- function(p) as.vector(as.matrix(dat[, siw_daily_names(p, lag_end), drop = FALSE])); dates <- as.vector(ld)
  daily <- data.frame(FEATURE_ID = rep(pc, lag_end), METHOD = "IDW", LOCAL_DATE = dates, TD_MEAN = g("IMS_IDW_TD_MEAN"), RH_MEAN = g("IMS_IDW_RH_MEAN"), RAIN_TOTAL = g("IMS_IDW_RAIN_TOTAL"),
                      TD_VALID = ifelse(dates == r$dst_day, r$expected_10min_dst, 144 - (seq_along(dates) %% 7L)), RH_VALID = ifelse(dates == r$dst_day, r$expected_10min_dst, 140), stringsAsFactors = FALSE)
  tds <- as.character(101:108); rns <- c(as.character(101:104), one_min <- as.character(unlist(r$one_minute_rain_stations))[1:2])
  audit <- do.call(rbind, lapply(seq_len(n), function(i) { ti <- tds[(i + 0:2) %% length(tds) + 1L]; ri <- rns[(i + 0:2) %% length(rns) + 1L]
    data.frame(FEATURE_ID = pc[i], VARIABLE = rep(c("TD", "Rain"), each = 3), METHOD = "IDW", STATION_ID = c(ti, ri), DISTANCE_M = c(800 + 37 * i, 4000 + 11 * i, 9000 + 5 * i, 600 + 41 * i, 5000 + 13 * i, 11000 + 7 * i),
               PERIOD_COVERAGE_PCT = 90, PLAN_INCLUDED = "1", stringsAsFactors = FALSE) }))
  plan <- data.frame(FEATURE_ID = rep(pc, 2), VARIABLE = rep(c("TD", "Rain"), each = n), METHOD = "IDW", AVAILABLE_STATIONS = rep(c(8, 6), each = n), stringsAsFactors = FALSE)
  coverage <- data.frame(STATION_ID = rep(c(tds, "109"), each = 3), VARIABLE_CODE = rep(c("TD", "RH", "Rain"), 9), valid_count = c(rep(1000, 24), 0, 0, 0), coverage_pct = c(rep(90, 24), 0, 0, 0), stringsAsFactors = FALSE)
  coverage <- rbind(coverage, data.frame(STATION_ID = rep(one_min, each = 3), VARIABLE_CODE = rep(c("TD", "RH", "Rain"), 2), valid_count = 1000, coverage_pct = 85, stringsAsFactors = FALSE))
  ud <- sort(unique(dates)); st <- unique(c(tds, rns))
  counts <- do.call(rbind, lapply(st, function(s) { k <- match(s, st); full <- ifelse(ud == r$dst_day, r$expected_10min_dst, r$expected_10min); gap <- (seq_along(ud) + k) %% 9L == 0L
    rain_full <- if (s %in% one_min) ifelse(ud == r$dst_day, r$expected_1min_dst, r$expected_1min) else full
    data.frame(station_id = s, date = ud, n_td = ifelse(gap, round(0.5 * full), full), n_rh = ifelse(gap & k %% 2L == 0L, round(0.7 * full), full), n_rain = ifelse((seq_along(ud) + 2L * k) %% 11L == 0L, round(0.6 * rain_full), rain_full), stringsAsFactors = FALSE) }))
  stations <- data.frame(quantity = c("coordinate_valid_stations_stage_a", "station_inventory_stage_a", "stations_requested_stage_a", "metadata_candidate_union_stage_a", "stations_returned_stage_b", "fetch_status_stations_stage_b", "fetch_status_range_ok_stage_b"), value = c(40, 41, 11, 11, 10, 11, 9), stringsAsFactors = FALSE)
  list(daily = daily, audit = audit, plan = plan, coverage = coverage, counts = counts, stations = stations)
}

.siw_ph_item_exposure_qa <- function(dat, config, protocol, it, refs, ctx) {
  given <- !is.null(refs[[SIW_GIS_ROLE]]) && nzchar(refs[[SIW_GIS_ROLE]]); paths <- siw_posthoc_role_files(refs[[SIW_GIS_ROLE]], protocol, SIW_GIS_ROLE)
  if (ctx$synthetic) { gis <- .siw_exposure_qa_synthetic(dat, protocol); prov <- SIW_PH_NA_SYN; ctx$W(data.frame(file = names(paths), item = "reference", value = "not applicable: shipped synthetic input", ok = TRUE), "posthoc_reference_provenance.csv") }
  else { vr <- .siw_ph_verify_role(paths, protocol, SIW_GIS_ROLE, given); ctx$W(vr$provenance, "posthoc_reference_provenance.csv"); prov <- vr$status
    if (prov != "PASS") return(list(statuses = c(INPUT_PROVENANCE_STATUS = prov, EXPOSURE_SOURCE_IDENTITY_STORED_PRECISION_STATUS = "NOT_TESTED", EXPOSURE_QA_COMPUTATION_STATUS = "NOT_TESTED", EXPOSURE_QA_SI_CONCORDANCE_STATUS = "NOT_TESTED"),
                                     info = paste0("EXPOSURE_QA_NOT_RUN=the declared GIS inputs of the analysed IMS run (role ", SIW_GIS_ROLE, ") were ", if (given) "not all identified by SHA-256 (see posthoc_reference_provenance.csv)" else "not supplied"), tables = list()))
    gis <- siw_read_gis_inputs(paths, protocol) }
  q <- siw_exposure_qa(dat, gis, it, protocol)
  ctx$W(q$identity, "exposure_source_identity.csv"); ctx$W(q$coverage, "exposure_qa_station_coverage.csv"); ctx$W(q$support, "exposure_qa_interpolation_support.csv"); ctx$W(q$distances, "exposure_qa_distances.csv")
  ctx$W(q$pool, "exposure_qa_station_pool.csv"); ctx$W(q$labels, "exposure_qa_variable_labels.csv")
  sic <- .siw_ph_ms(.siw_exposure_qa_si(q, it), ctx$synthetic); ctx$W(sic, "exposure_qa_si_concordance.csv")
  cmp <- sic[!grepl("^record only", sic$note), , drop = FALSE]
  st <- c(INPUT_PROVENANCE_STATUS = prov, EXPOSURE_SOURCE_IDENTITY_STORED_PRECISION_STATUS = if (all(q$identity$stored_precision_status == "PASS")) "PASS" else "FAIL", EXPOSURE_QA_COMPUTATION_STATUS = if (q$computation_ok) "PASS" else "FAIL",
          EXPOSURE_QA_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(cmp$agrees %in% TRUE)) "PASS" else "REVIEW")
  info <- c(paste0("EXPOSURE_SOURCE_IDENTITY_STRICT_1E9=", if (all(q$identity$strict_1e9_status == "PASS")) "PASS" else "FAIL", " (informational, kept for the record; |difference| <= 1e-9 on every participant-day; per variable ", paste0(q$identity$gis_column, " ", q$identity$strict_1e9_status, collapse = ", "), ")"),
            paste0("EXPOSURE_SOURCE_IDENTITY_STORED_PRECISION_RULE=governs step 27 (author decisions 14 Oct 2026; one source stores 8 decimals, observed maximum difference about 5e-9; amended because rounding-equality fails at rounding boundaries): all rows present, same missing pattern, max |difference| <= 1e-8 (one unit in the 8th decimal); the rounding-equality counts are informational; per variable ", paste0(q$identity$gis_column, " ", q$identity$stored_precision_status, collapse = ", ")),
            paste0("EXPOSURE_SOURCE_IDENTITY=", paste0(q$identity$gis_column, " ", q$identity$equal_1e_9, "/", q$identity$both_finite, " equal (rows found ", q$identity$rows_found, "/", q$identity$participant_days, ")", collapse = "; ")),
            paste0("EXPOSURE_QA_RULES=coverage threshold ", it$rules$coverage_threshold, "; expected ", it$rules$expected_10min, " values per local day (", it$rules$expected_10min_dst, " on ", it$rules$dst_day, ", DST); 1-minute rain stations ", paste(unlist(it$rules$one_minute_rain_stations), collapse = ", "), ": ", it$rules$expected_1min, " (", it$rules$expected_1min_dst, ")"),
            paste0("EXPOSURE_QA_SI_AGREEING=", sum(cmp$agrees %in% TRUE), "/", nrow(cmp), " (record-only rows excluded: ", sum(grepl("^record only", sic$note)), ")"),
            paste0("EXPOSURE_SOURCE_IDENTITY_DIFFERENCES=", paste0(q$identity$gis_column, " max ", q$identity$max_abs_difference_text, ", ", q$identity$differ_gt_1e_9, " > 1e-9, ", q$identity$differ_gt_5e_9, " > 5e-9, equal after rounding both to 8 decimals ", q$identity$equal_after_rounding_both_to_8_decimals, "/", q$identity$both_finite, collapse = "; ")))
  list(statuses = st, info = info, tables = c(q, list(si = sic)))
}

# ---- (28) VIF summary ------------------------------------------------------------------------------------------------------------------------------------------

#' Per-column VIFs of a design matrix: diag(solve(cor(X without intercept))) (the Batch A siecc_max_vif definition, kept per column).
#' @export
siw_vif_columns <- function(X) {
  P <- X[, colnames(X) != "(Intercept)", drop = FALSE]; if (ncol(P) <= 1L) return(stats::setNames(rep(1, ncol(P)), colnames(P)))
  s <- apply(P, 2L, stats::sd); if (any(!is.finite(s) | s == 0)) return(stats::setNames(rep(Inf, ncol(P)), colnames(P)))
  inv <- tryCatch(solve(stats::cor(P)), error = function(e) NULL); if (is.null(inv)) return(stats::setNames(rep(Inf, ncol(P)), colnames(P))); stats::setNames(diag(inv), colnames(P))
}

.siw_vif_term <- function(nm) ifelse(nm == "exposure", "exposure", ifelse(grepl("AssessmentDay", nm), "assessment_day", ifelse(grepl("^Locality", nm), "locality", ifelse(grepl("^Sex", nm), "sex", ifelse(grepl("^Age", nm), "age", ifelse(grepl("^Econ", nm), "economic_status", "other"))))))

#' VIFs of every M2-L cumulative model (same samples and design as siw_cumulative_diagnostics): per model the maximum VIF and the term attaining it, and every per-column VIF.
#' @export
siw_vif_summary <- function(dat, config, tier = config$primary_tier) {
  Xt <- siw_tier_matrix(dat, config$tiers[[tier]]); ok <- stats::complete.cases(Xt); mod <- list(); col <- list()
  for (oi in seq_len(nrow(config$outcomes))) { o <- config$outcomes[oi, ]; y <- as.numeric(dat[[o$outcome]])
    for (mi in seq_len(nrow(config$metrics))) { sp <- config$metrics[mi, ]
      for (w in config$windows) { x <- as.numeric(dat[[siw_cum_name(sp$cumulative_prefix, w)]]); m <- ok & is.finite(y) & is.finite(x); Xs <- Xt[m, , drop = FALSE]
        Xs <- Xs[, !(grepl("^Locality_3group", colnames(Xs)) & colSums(Xs != 0) == 0L), drop = FALSE]; X <- cbind(Xs[, 1L, drop = FALSE], exposure = x[m], Xs[, -1L, drop = FALSE]); colnames(X)[1L] <- "(Intercept)"
        v <- siw_vif_columns(X); k <- which.max(v); id <- siw_cum_id(o$code, sp$code, w, tier)
        mod[[length(mod) + 1L]] <- data.frame(result_id = id, outcome = o$outcome, metric = sp$metric, window = as.integer(w), model_tier = tier, representative = sp$metric %in% config$representative_metrics, n = nrow(X),
                                               maximum_vif = max(v), max_vif_column = names(v)[k], max_vif_term = .siw_vif_term(names(v)[k]), stringsAsFactors = FALSE)
        col[[length(col) + 1L]] <- data.frame(result_id = id, outcome = o$outcome, metric = sp$metric, window = as.integer(w), column = names(v), term = .siw_vif_term(names(v)), vif = unname(v), stringsAsFactors = FALSE) } } }
  list(models = do.call(rbind, mod), columns = do.call(rbind, col))
}

.siw_ph_item_vif_summary <- function(dat, config, protocol, it, refs, ctx) {
  vs <- siw_vif_summary(dat, config); md <- vs$models; ctx$W(md, "vif_per_model_m2l.csv"); ctx$W(vs$columns, "vif_per_column_m2l.csv")
  dg <- siw_cumulative_diagnostics(dat, config, tiers = config$primary_tier); j <- match(md$result_id, dg$result_id); cons <- abs(md$maximum_vif - dg$maximum_vif[j])
  ck <- data.frame(check = c("models", "representative models", "finite maximum VIF", "maximum VIF equals the Batch A siw_cumulative_diagnostics value (1e-10)"),
                   value = c(nrow(md), sum(md$representative), sum(is.finite(md$maximum_vif)), sum(cons <= 1e-10, na.rm = TRUE)), expected = c(it$expected$models_all, it$expected$models_representative, nrow(md), nrow(md)), stringsAsFactors = FALSE)
  ck$status <- ifelse(ck$value == ck$expected, "PASS", "FAIL"); ctx$W(ck, "vif_computation_checks.csv")
  rp <- md[md$representative, ]; e <- it$expected; I <- "vif_summary"; S <- .siw_ph_si_row
  mx <- function(d) if (nrow(d)) d[which.max(d$maximum_vif), , drop = FALSE] else NULL
  uw <- function(w) { d <- md[md$metric == e$utci_metric & md$window == w, ]; c(max(d$maximum_vif), all(d$max_vif_term == "exposure")) }
  rows <- list(S(I, "representative M2-L: minimum of per-model max VIF", min(rp$maximum_vif), e$representative_range[[1]], 2), S(I, "representative M2-L: maximum of per-model max VIF", max(rp$maximum_vif), e$representative_range[[2]], 2))
  for (k in seq_along(e$utci_windows)) { w <- e$utci_windows[[k]]; u <- uw(w); rows <- c(rows, list(S(I, paste0("UTCI ", w, "-day max VIF"), u[1], e$utci_max_vif[[k]], 2), .siw_ph_flag(I, paste0("UTCI ", w, "-day: the maximum-VIF term is the exposure (1 = yes)"), u[2] == 1))) }
  rh <- mx(md[md$metric == e$rh_metric & md$representative, ]); rd <- mx(md[md$metric == e$rainy_metric & md$representative, ]); ov <- mx(md)
  rows <- c(rows, list(S(I, "IMS RH: largest per-model max VIF", rh$maximum_vif, e$rh_largest, 2), S(I, "IMS RH: window of the largest", rh$window, e$rh_largest_window, 0),
    S(I, "IMS rainy-day count: largest per-model max VIF", rd$maximum_vif, e$rainy_largest, 2), S(I, "IMS rainy-day count: window of the largest", rd$window, e$rainy_largest_window, 0),
    S(I, "all M2-L fits: minimum of per-model max VIF", min(md$maximum_vif), e$all_range[[1]], 2), S(I, "all M2-L fits: maximum of per-model max VIF", max(md$maximum_vif), e$all_range[[2]], 2),
    .siw_ph_flag(I, paste0("overall maximum: metric is ", e$overall_metric, " (1 = yes)"), ov$metric == e$overall_metric), S(I, "overall maximum: window", ov$window, e$overall_window, 0),
    .siw_ph_flag(I, paste0("overall maximum: term is ", e$overall_term, " (1 = yes)"), ov$max_vif_term == e$overall_term)))
  sic <- .siw_ph_ms(do.call(rbind, rows), ctx$synthetic); ctx$W(sic, "vif_si_concordance.csv")
  st <- c(VIF_COMPUTATION_STATUS = if (all(ck$status == "PASS")) "PASS" else "FAIL", VIF_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees %in% TRUE)) "PASS" else "REVIEW")
  info <- c(paste0("VIF_DEFINITION=diag(solve(cor(design columns without the intercept))) per M2-L model (Batch A siecc_max_vif, kept per column); ", nrow(md), " models (", sum(md$representative), " representative)"),
            paste0("VIF_OVERALL_MAXIMUM=", sprintf("%.2f", ov$maximum_vif), " (", ov$metric, ", ", ov$window, " days, term ", ov$max_vif_term, ")"), "VIF_INPUT=recomputed from the canonical CSV with the Batch A design; equality with siw_cumulative_diagnostics (the function behind Batch A cumulative_diagnostics.csv) is checked")
  list(statuses = st, info = info, tables = list(models = md, columns = vs$columns, checks = ck, si = sic))
}

# ---- (29) calendar spline ---------------------------------------------------------------------------------------------------------------------------------------

.siw_ph_item_calendar_spline <- function(dat, config, protocol, it, refs, ctx) {
  gr <- list(); vr <- list(); for (k in unlist(it$df)) { v <- paste0("DATE_NS_DF", k); g <- siw_s17_grid(dat, config, v, protocol); g$q90_calspline <- siw_bh(g$p_primary); g$family_label <- paste0("q90-calspline-df", k); gr[[v]] <- g
    vr[[v]] <- do.call(rbind, lapply(seq_len(nrow(g)), function(i) { r <- g[i, ]; iv <- .siw_s17_independent(dat, config, r, protocol); data.frame(model_id = r$model_id, estimate_rel_diff = abs(r$estimate - iv[["estimate"]]) / max(abs(iv[["estimate"]]), 1e-12), se_rel_diff = abs(r$standard_error - iv[["standard_error"]]) / max(abs(iv[["standard_error"]]), 1e-12), stringsAsFactors = FALSE) })) }
  G <- do.call(rbind, gr); rownames(G) <- NULL; V <- do.call(rbind, vr); V$status <- ifelse(V$estimate_rel_diff <= 1e-6 & V$se_rel_diff <= 1e-6, "PASS", "FAIL"); ctx$W(G, "calendar_spline_cumulative_180.csv"); ctx$W(V, "calendar_spline_independent_verification.csv")
  rs <- it$reference; ref <- .siw_ph_ref(ctx, refs, protocol, rs$role, rs$file); ctx$W(ref$provenance, "posthoc_reference_provenance.csv"); rstat <- .siw_ph_dep(ref)
  if (is.null(rstat)) { R <- .siw_s17_ref(ref$table); R <- R[R$variant %in% names(gr), , drop = FALSE]; kk <- function(d) paste(d$outcome, d$metric, d$window, d$variant); j <- match(kk(G), kk(R))
    acc <- data.frame(variant = G$variant, model_id = G$model_id, matched = !is.na(j), estimate_abs_diff = abs(G$estimate - .siw_num(R$estimate[j])), se_abs_diff = abs(G$standard_error - .siw_num(R$standard_error[j])), p_normal_abs_diff = abs(G$p - .siw_num(R$p[j])), stringsAsFactors = FALSE)
    tl <- rs$acceptance; acc$status <- ifelse(acc$matched & acc$estimate_abs_diff <= tl$estimate_abs & acc$se_abs_diff <= tl$standard_error_abs & acc$p_normal_abs_diff <= tl$p_abs, "PASS", "FAIL")
    sm <- do.call(rbind, lapply(split(acc, acc$variant), function(a) data.frame(variant = a$variant[1], reference_rows = sum(R$variant == a$variant[1]), compared = sum(a$matched), max_estimate_abs_diff = max(a$estimate_abs_diff, na.rm = TRUE), max_se_abs_diff = max(a$se_abs_diff, na.rm = TRUE),
                                                                                 max_p_normal_abs_diff = max(a$p_normal_abs_diff, na.rm = TRUE), passed = sum(a$status == "PASS"), stringsAsFactors = FALSE)))
    ctx$W(acc, "calendar_spline_reference_comparison_detail.csv"); ctx$W(sm, "calendar_spline_reference_comparison_summary.csv"); rstat <- if (all(acc$status == "PASS") && all(sm$reference_rows == rs$expected_per_variant) && nrow(acc) == 90L * length(gr)) "PASS" else "FAIL" }
  e <- it$expected; I <- "calendar_spline"; S <- .siw_ph_si_row; rows <- list()
  for (k in unlist(it$df)) { g <- gr[[paste0("DATE_NS_DF", k)]]; x <- e[[paste0("df", k)]]; im <- which.min(g$p_primary); gc <- g[g$family == "gaussian", ]
    rows <- c(rows, list(S(I, paste0("df ", k, ": fits completed"), sum(g$status == "COMPLETED"), x$fits_completed, 0), S(I, paste0("df ", k, ": residual df (every model)"), if (length(unique(g$residual_df)) == 1L) unique(g$residual_df) else NA, x$residual_df, 0),
      S(I, paste0("df ", k, ": nominal p < 0.05 (t-based for continuous outcomes)"), sum(g$p_primary < 0.05, na.rm = TRUE), x$nominal_p_lt_05, 0), S(I, paste0("df ", k, ": smallest p"), g$p_primary[im], x$smallest_p, 4),
      .siw_ph_flag(I, paste0("df ", k, ": smallest p is ", x$smallest_p_model$outcome, ", ", x$smallest_p_model$metric, ", ", x$smallest_p_model$window, " days (1 = yes)"), g$outcome[im] == x$smallest_p_model$outcome && g$metric[im] == x$smallest_p_model$metric && g$window[im] == x$smallest_p_model$window),
      S(I, paste0("df ", k, ": minimum q90-calspline-df", k), min(g$q90_calspline, na.rm = TRUE), x$minimum_q90, 3))) }
  sic <- .siw_ph_ms(do.call(rbind, rows), ctx$synthetic); ctx$W(sic, "calendar_spline_si_concordance.csv")
  st <- c(INPUT_PROVENANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else ref$status, CALENDAR_SPLINE_REPRODUCTION_STATUS = rstat, CALENDAR_SPLINE_VERIFICATION_STATUS = if (all(V$status == "PASS")) "PASS" else "FAIL",
          CALENDAR_SPLINE_SI_CONCORDANCE_STATUS = if (ctx$synthetic) SIW_PH_NA_SYN else if (all(sic$agrees %in% TRUE)) "PASS" else "REVIEW")
  info <- c(paste0("CALENDAR_SPLINE_FITS=", paste0("df ", unlist(it$df), ": ", vapply(gr, function(g) sum(g$status == "COMPLETED"), 0L), "/", vapply(gr, nrow, 0L), collapse = "; ")),
            paste0("CALENDAR_SPLINE_MIN_P=", paste0("df ", unlist(it$df), ": ", vapply(gr, function(g) sprintf("%.4f", min(g$p_primary, na.rm = TRUE)), ""), collapse = "; "), " (t reference for continuous outcomes; the normal-reference p of the archive is kept in column p)"),
            "CALENDAR_SPLINE_FAMILIES=q90-calspline-df2 and q90-calspline-df3 (BH across the 90 tests within each df); distinct from q90, which is unchanged")
  list(statuses = st, info = info, tables = list(grid = G, verification = V, si = sic))
}
