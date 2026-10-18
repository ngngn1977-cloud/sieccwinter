# Rainfall anomaly detection (primary run: report only) and an explicitly named optional sensitivity.

#' Detect cells where the daily rainfall total is missing but the rainy-day indicator is present and equal to zero.
#' Aggregate report only: no participant identifier or row index is returned.
#' @export
siw_rainfall_anomalies <- function(dat, config, lag_end = 56L) {
  tn <- siw_daily_names("IMS_IDW_RAIN_TOTAL", lag_end); dn <- siw_daily_names("IMS_IDW_RAIN_DAYS1", lag_end)
  ok <- tn %in% names(dat) & dn %in% names(dat)
  if (!any(ok)) return(list(n_cells = 0L, n_participants = 0L, by_lag = data.frame(lag = integer(), n_cells = integer())))
  T <- as.matrix(dat[, tn[ok], drop = FALSE]); D <- as.matrix(dat[, dn[ok], drop = FALSE]); storage.mode(T) <- "double"; storage.mode(D) <- "double"
  cell <- is.na(T) & !is.na(D) & D == 0
  lags <- which(ok)
  list(n_cells = sum(cell), n_participants = sum(rowSums(cell) > 0),
       by_lag = data.frame(lag = lags[colSums(cell) > 0], n_cells = as.integer(colSums(cell)[colSums(cell) > 0])),
       total_missing_cells = sum(is.na(T)), indicator_missing_cells = sum(is.na(D)))
}

#' Report table for the anomaly detection.
#' @export
siw_rainfall_anomaly_report <- function(dat, config, lag_end = 56L) {
  a <- siw_rainfall_anomalies(dat, config, lag_end)
  base <- data.frame(item = c("cells_total_missing_and_indicator_zero", "participants_affected", "cells_total_missing", "cells_indicator_missing"),
                     value = c(a$n_cells, a$n_participants, a$total_missing_cells %||% 0L, a$indicator_missing_cells %||% 0L), stringsAsFactors = FALSE)
  byl <- if (nrow(a$by_lag)) data.frame(item = paste0("lag_", sprintf("%02d", a$by_lag$lag), "_cells"), value = a$by_lag$n_cells, stringsAsFactors = FALSE) else NULL
  out <- rbind(base, byl)
  out$note <- ""
  out$note[1] <- "Detected and reported only. Input values are not altered in the primary run."
  out
}

#' OPTIONAL SENSITIVITY (never used in the primary run): deterministic zero reconstruction of the rainfall total.
#' Sets the daily rainfall total to 0 exactly where it is missing and the rainy-day indicator equals 0 (the documented one-cell
#' reconstruction pattern), then rebuilds the cumulative fields. The input data frame is not modified; a modified copy is returned.
#' @export
siw_rainfall_zero_reconstruction_sensitivity <- function(dat, config, lag_end = 56L) {
  tn <- siw_daily_names("IMS_IDW_RAIN_TOTAL", lag_end); dn <- siw_daily_names("IMS_IDW_RAIN_DAYS1", lag_end)
  out <- dat; n <- 0L
  for (k in seq_len(lag_end)) {
    idx <- which(is.na(out[[tn[k]]]) & !is.na(out[[dn[k]]]) & out[[dn[k]]] == 0)
    if (length(idx)) { out[[tn[k]]][idx] <- 0; n <- n + length(idx) }
  }
  cn <- siw_cumulative_columns(config); out[cn] <- NULL
  att <- siw_attach_cumulative(out, config)
  d <- att$data
  attr(d, "sensitivity") <- list(name = "rainfall_zero_reconstruction", cells_replaced = n)
  d
}
