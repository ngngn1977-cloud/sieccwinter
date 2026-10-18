\name{siw_bootstrap_verify}
\alias{siw_bootstrap_verify}
\title{Verify every bootstrap target}
\usage{
siw_bootstrap_verify(dat, config, run, point, protocol, replicates = 2000L,
    recompute_targets = protocol$mathematical_verification$determinism_targets_recomputed)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{run}{See the package README and configuration file.}
\item{point}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{recompute_targets}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Sampling invariants, original-style refits on identical resamples, independent percentiles, seed rule, conservation, full-sample estimate and determinism.}
