\name{siw_alt_weighted}
\alias{siw_alt_weighted}
\title{Weighted cumulative feature}
\usage{
siw_alt_weighted(H, half_life = Inf, max_lag = 56L, min_fraction = 0.8)
}
\arguments{
\item{H}{See the package README and configuration file.}
\item{half_life}{See the package README and configuration file.}
\item{max_lag}{See the package README and configuration file.}
\item{min_fraction}{See the package README and configuration file.}
}
\value{A numeric vector.}
\description{Half-life weighted mean over lags 1-56, missing when fewer than 45 lags are finite.}
