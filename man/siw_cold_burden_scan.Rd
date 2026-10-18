\name{siw_cold_burden_scan}
\alias{siw_cold_burden_scan}
\title{Cold-burden scan}
\usage{
siw_cold_burden_scan(dat, config, protocol = siw_cold_burden_protocol(),
                     thresholds = NULL, windows = NULL, diagnostics = TRUE)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{thresholds}{See the package README and configuration file.}
\item{windows}{See the package README and configuration file.}
\item{diagnostics}{See the package README and configuration file.}
}
\value{A list with thresholds, results and timings.}
\description{Fits every cold-burden specification with the validated gaussian / logistic engine and the model diagnostics; q_global, q_outcome and q_curve are Benjamini-Hochberg within their declared families.}
