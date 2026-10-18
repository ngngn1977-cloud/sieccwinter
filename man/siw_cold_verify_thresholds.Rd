\name{siw_cold_verify_thresholds}
\alias{siw_cold_verify_thresholds}
\title{Verify cold-burden thresholds}
\usage{
siw_cold_verify_thresholds(dat, config, thr, protocol = siw_cold_burden_protocol())
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{thr}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
}
\value{A data frame of differences and a status.}
\description{Independent type-7 quantile and recount of participant-days below each threshold.}
