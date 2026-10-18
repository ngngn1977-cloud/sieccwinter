# Minimal SPSS (.sav) writer for TESTS ONLY (fully synthetic files). Uncompressed, little-endian, numeric and short string (<= 8 bytes) variables,
# optional value labels and up to three discrete user-defined missing values, long variable names via the type-7 subtype-13 record.
# Lets the tests exercise foreign::read.spss exactly as the legacy import does, without any real data.
write_min_sav <- function(path, vars, n) {
  con <- file(path, "wb"); on.exit(close(con))
  wi <- function(x) writeBin(as.integer(x), con, size = 4L, endian = "little")
  wd <- function(x) writeBin(as.double(x), con, size = 8L, endian = "little")
  wc <- function(s, w) writeBin(charToRaw(sprintf(paste0("%-", w, "s"), substr(s, 1L, w))), con)
  wc("$FL2", 4L); wc("@(#) synthetic test file", 60L); wi(2L); wi(length(vars)); wi(0L); wi(0L); wi(n); wd(100)
  wc("01 Jan 26", 9L); wc("00:00:00", 8L); wc("synthetic", 64L); writeBin(as.raw(c(0, 0, 0)), con)
  shorts <- sprintf("V%07d", seq_along(vars))
  for (i in seq_along(vars)) {
    v <- vars[[i]]; str <- identical(v$type, "string"); w <- if (str) v$width else 0L
    um <- if (is.null(v$user_missing)) numeric() else v$user_missing
    fmt <- if (str) 65536L + w * 256L else 5L * 65536L + 8L * 256L + 2L
    wi(2L); wi(w); wi(0L); wi(length(um)); wi(fmt); wi(fmt); wc(shorts[i], 8L)
    for (m in um) wd(m)
  }
  for (i in seq_along(vars)) {
    vl <- vars[[i]]$value_labels; if (is.null(vl)) next
    wi(3L); wi(length(vl))
    for (k in seq_along(vl)) {
      lab <- names(vl)[k]; wd(vl[[k]]); len <- nchar(lab); writeBin(as.raw(len), con)
      pad <- (8L - ((1L + len) %% 8L)) %% 8L; writeBin(charToRaw(lab), con); if (pad) writeBin(as.raw(rep(32L, pad)), con)
    }
    wi(4L); wi(1L); wi(i)
  }
  ln <- paste0(shorts, "=", vapply(vars, function(v) v$name, ""), collapse = "\t")
  wi(7L); wi(13L); wi(1L); wi(nchar(ln)); writeBin(charToRaw(ln), con)
  wi(999L); wi(0L)
  for (r in seq_len(n)) for (i in seq_along(vars)) {
    v <- vars[[i]]
    if (identical(v$type, "string")) wc(as.character(v$values[r]), 8L) else wd(if (is.na(v$values[r])) -.Machine$double.xmax else v$values[r])
  }
  invisible(path)
}

# Build a synthetic SAV that looks like the confidential file: SAV-style source names, decoy confidential-type fields that must not be exported.
make_synthetic_sav <- function(path, raw = SYN, drop = character(), override = list(), extra_missing = list()) {
  map <- siw_priv_mapping(CFG)
  vars <- list()
  for (i in seq_len(nrow(map))) {
    cn <- map$canonical[i]; if (cn %in% drop) next
    vals <- if (!is.null(override[[cn]])) override[[cn]] else raw[[cn]]
    if (cn == "PedestLength_m") vals <- raw$PedestLength_m
    if (cn == "participant_code") vars[[length(vars) + 1L]] <- list(name = map$source[i], type = "string", width = 8L, values = vals)
    else vars[[length(vars) + 1L]] <- list(name = map$source[i], type = "numeric", values = as.numeric(vals), user_missing = extra_missing[[cn]]$user_missing, value_labels = extra_missing[[cn]]$value_labels)
  }
  n <- nrow(raw)
  vars[[length(vars) + 1L]] <- list(name = "assessment_date_iso", type = "string", width = 8L, values = sprintf("DECOY%03d", seq_len(n)))
  vars[[length(vars) + 1L]] <- list(name = "home_address_raw", type = "string", width = 8L, values = sprintf("STREET%02d", seq_len(n) %% 100))
  vars[[length(vars) + 1L]] <- list(name = "latitude", type = "numeric", values = 32 + seq_len(n) / 1000)
  vars[[length(vars) + 1L]] <- list(name = "longitude", type = "numeric", values = 35 + seq_len(n) / 1000)
  write_min_sav(path, vars, n)
}
