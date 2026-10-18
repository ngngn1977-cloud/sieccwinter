# Cumulative exposures and the natural-cubic-spline lag basis (computed once and reused).

#' Reconstruct all cumulative exposure fields from the daily fields, once, as a named data frame.
#' Rule (declared, unchanged from the study protocol): window w uses lags L01..Lw; means and sums use the available days
#' (missing days are omitted, not imputed); the value is missing only if all days in the window are missing.
#' Counts and totals are sums of the available daily values.
#' @export
siw_reconstruct_cumulative <- function(dat, config) {
  out <- list()
  for (i in seq_len(nrow(config$metrics))) {
    sp <- config$metrics[i, ]
    H <- as.matrix(dat[, siw_daily_names(sp$daily_prefix, max(config$windows)), drop = FALSE]); storage.mode(H) <- "double"
    fin <- is.finite(H)
    Hz <- H; Hz[!fin] <- 0
    cs <- t(apply(Hz, 1L, cumsum)); cn <- t(apply(fin, 1L, cumsum))       # running sums and running counts of available days
    for (w in config$windows) {
      s <- cs[, w]; n <- cn[, w]
      v <- if (sp$aggregation == "mean") s / n else s
      v[n == 0] <- NA_real_
      out[[siw_cum_name(sp$cumulative_prefix, w)]] <- as.numeric(v)
    }
  }
  as.data.frame(out, check.names = FALSE)
}

#' Attach the cumulative fields to the analysis data: supplied values are used as given; otherwise the reconstruction is used.
#' @return list(data, source) where source is "supplied" or "reconstructed".
#' @export
siw_attach_cumulative <- function(dat, config) {
  cn <- siw_cumulative_columns(config)
  if (all(cn %in% names(dat))) return(list(data = dat, source = "supplied"))
  rec <- siw_reconstruct_cumulative(dat, config)
  for (nm in cn) dat[[nm]] <- rec[[nm]]
  list(data = dat, source = "reconstructed")
}

.siw_cache <- new.env(parent = emptyenv())

#' Natural cubic spline lag basis over lags 1..L (intercept included), cached per (L, df).
#' Identical to splines::ns(1:L, df, intercept = TRUE), which is what dlnm::onebasis / crossbasis use for arglag = list(fun = "ns", df = 3).
#' @export
siw_lag_basis <- function(L, df = 3L) {
  key <- paste(L, df)
  if (!is.null(.siw_cache[[key]])) return(.siw_cache[[key]])
  B <- matrix(as.numeric(splines::ns(seq_len(L), df = df, intercept = TRUE)), nrow = L)
  colnames(B) <- paste0("lagbasis", seq_len(df))
  .siw_cache[[key]] <- B
  B
}

#' Cross-basis for a linear exposure: history (n x L) times lag basis (L x df) gives n x df.
#' @export
siw_crossbasis <- function(H, B) {
  stopifnot(ncol(H) == nrow(B))
  cb <- H %*% B; colnames(cb) <- paste0("cb", seq_len(ncol(B))); cb
}
