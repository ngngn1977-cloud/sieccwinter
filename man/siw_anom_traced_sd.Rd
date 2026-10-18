\name{siw_anom_traced_sd}
\alias{siw_anom_traced_sd}
\title{Traced complete-case SDs of the gaussian ANOM rows}
\usage{
siw_anom_traced_sd(dat, config, protocol, keys)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{keys}{See the package README and configuration file.}
}
\value{A data frame of keys, n_mask and direct_sd.}
\description{For every gaussian key, the complete-case mask of validation/anom_sample_sd_diag.R (tier covariates complete, outcome finite, anomaly feature finite) and the directly measured outcome SD over it. Aggregate only.}
