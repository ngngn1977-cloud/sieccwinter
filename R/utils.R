# Utilities: hashing, Benjamini-Hochberg, deterministic CSV output, identifiers, logging.
# Nothing in this file prints or stores participant-level values.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Benjamini-Hochberg adjustment within one declared family.
#' Non-finite p-values are excluded from the family (m counts finite values only) and returned as NA.
#' @export
siw_bh <- function(p) {
  p <- as.numeric(p)
  out <- rep(NA_real_, length(p))
  k <- which(is.finite(p))
  if (length(k)) {
    v <- p[k]; m <- length(v); o <- order(v)
    q <- rev(cummin(rev(v[o] * m / seq_len(m))))
    r <- numeric(m); r[o] <- pmin(1, q)
    out[k] <- r
  }
  out
}

#' Hash of a file. SHA-256 through the operating system when possible, otherwise MD5 (labelled).
#' @return list(algorithm, value)
#' @export
siw_file_hash <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type == "windows") {
    cmd <- Sys.which("certutil")
    if (nzchar(cmd)) {
      out <- suppressWarnings(tryCatch(system2(cmd, c("-hashfile", shQuote(path), "SHA256"), stdout = TRUE, stderr = TRUE), error = function(e) character()))
      hit <- toupper(gsub("[[:space:]]", "", out)); hit <- hit[grepl("^[0-9A-F]{64}$", hit)]
      if (length(hit) == 1L) return(list(algorithm = "sha256", value = hit))
    }
  } else {
    for (tool in c("sha256sum", "shasum")) {
      cmd <- Sys.which(tool)
      if (nzchar(cmd)) {
        args <- if (tool == "shasum") c("-a", "256", shQuote(path)) else shQuote(path)
        out <- suppressWarnings(tryCatch(system2(cmd, args, stdout = TRUE, stderr = TRUE), error = function(e) character()))
        hit <- toupper(sub("[[:space:]].*$", "", out[1L]))
        if (grepl("^[0-9A-F]{64}$", hit)) return(list(algorithm = "sha256", value = hit))
      }
    }
  }
  list(algorithm = "md5", value = unname(toupper(tools::md5sum(path))))
}

#' Hash of a character string (MD5; used for cache signatures, not for security).
#' @export
siw_string_hash <- function(x) {
  f <- tempfile(); on.exit(unlink(f), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste(x, collapse = "\n"))), f)
  unname(toupper(tools::md5sum(f)))
}

#' Write a data frame as a deterministic CSV (fixed column order supplied by the caller, no row names).
#' @export
siw_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

#' Stable result identifiers.
#' @export
siw_cum_id <- function(outcome_code, metric_code, window, tier) {
  paste("CUM", outcome_code, metric_code, paste0("W", window), tier, sep = "__")
}
#' @export
siw_dlm_id <- function(outcome_code, metric_code, lag_end, lag_df, tier) {
  paste("DLM", outcome_code, metric_code, paste0("L", lag_end), paste0("DF", lag_df), tier, sep = "__")
}

siw_log <- function(..., log_file = getOption("sieccwinter.log_file")) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""))
  message(line)
  if (!is.null(log_file) && nzchar(log_file)) cat(line, "\n", file = log_file, append = TRUE)
  invisible(line)
}

#' Run an expression with a fixed RNG seed and restore the caller's RNG state afterwards.
#' @export
siw_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had) assign(".Random.seed", old, envir = globalenv()) else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(expr)
}

siw_bind_rows <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (!length(rows)) return(data.frame())
  nm <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) { for (n in setdiff(nm, names(x))) x[[n]] <- NA; x[, nm, drop = FALSE] })
  do.call(rbind, rows)
}

siw_root <- function() {
  r <- getOption("sieccwinter.root")
  if (!is.null(r) && dir.exists(r)) return(normalizePath(r, winslash = "/"))
  s <- system.file(package = "sieccwinter")
  if (nzchar(s)) return(s)
  stop("SIW_ROOT_NOT_FOUND: set options(sieccwinter.root = <repository root>)", call. = FALSE)
}
