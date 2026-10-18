\name{siw_cold_independent_refit}
\alias{siw_cold_independent_refit}
\title{Independent refit of a cold-burden sample}
\usage{
siw_cold_independent_refit(dat, config, res, thresholds,
                           protocol = siw_cold_burden_protocol(), tol = config$tolerances)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{res}{See the package README and configuration file.}
\item{thresholds}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A data frame of field comparisons.}
\description{Refits a deterministic sample with a separate formula-based route and compares it with the package rows.}
