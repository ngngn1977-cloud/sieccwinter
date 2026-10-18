\name{siw_anomaly_scan}
\alias{siw_anomaly_scan}
\title{Daily-reference anomaly scan}
\usage{
siw_anomaly_scan(dat, config, protocol = siw_ah_protocol(), windows = NULL,
                 diagnostics = TRUE)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{windows}{See the package README and configuration file.}
\item{diagnostics}{See the package README and configuration file.}
}
\value{A list with results, diagnostics, features and seconds.}
\description{Fits the 7,080 adjusted M2_L anomaly models with the validated engine; q_global, q_outcome and q_curve are Benjamini-Hochberg.}
