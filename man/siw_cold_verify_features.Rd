\name{siw_cold_verify_features}
\alias{siw_cold_verify_features}
\title{Verify cold-burden features}
\usage{
siw_cold_verify_features(dat, config, thresholds, protocol = siw_cold_burden_protocol(),
                         windows = NULL)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{thresholds}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{windows}{See the package README and configuration file.}
}
\value{A data frame of maximum differences and a status.}
\description{Compares the cumulative-sum features with the independent per-participant route for all thresholds, windows and participants.}
