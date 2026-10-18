\name{siw_v14_dlm_bootstrap}
\alias{siw_v14_dlm_bootstrap}
\title{One archived DLM bootstrap target}
\usage{
siw_v14_dlm_bootstrap(dat, config, outcome, family, metric, replicates, seed,
                      cluster_labels = NULL, lag_end = 59L, lag_df = 3L)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{family}{See the package README and configuration file.}
\item{metric}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
\item{cluster_labels}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
\item{lag_df}{See the package README and configuration file.}
}
\value{A one-row data frame.}
\description{Participant or assessment-date-cluster resampling of the archived L01-L59 DLM cumulative effect, reproducing WINTER_SOURCE R/100_resampling.R.}
