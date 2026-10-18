\name{siw_anomaly_features_verify}
\alias{siw_anomaly_features_verify}
\title{Verify anomaly window features}
\usage{
siw_anomaly_features_verify(config, protocol, vals_all, vals_loc, windows = NULL)
}
\arguments{
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{vals_all}{See the package README and configuration file.}
\item{vals_loc}{See the package README and configuration file.}
\item{windows}{See the package README and configuration file.}
}
\value{A data frame of differences and a status.}
\description{Compares every vectorised window feature with the literal per-participant function.}
