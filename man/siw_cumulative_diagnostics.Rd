\name{siw_cumulative_diagnostics}
\alias{siw_cumulative_diagnostics}
\title{Influence and design diagnostics for the cumulative grid}
\usage{
siw_cumulative_diagnostics(dat, config, tiers = config$tier_order)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{tiers}{See the package README and configuration file.}
}
\value{A data frame with one row per specification.}
\description{The 16 diagnostic columns of the original grid for every specification, with dropped terms and undefined diagnostics reported per model; computed separately from the core results.}
