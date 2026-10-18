\name{siw_xy_maxt_grid}
\alias{siw_xy_maxt_grid}
\title{Max-|r| permutation test for all outcomes}
\usage{
siw_xy_maxt_grid(dat, config, protocol = siw_daily_protocol(), replicates = NULL,
                 engine = "optimised")
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{engine}{See the package README and configuration file.}
}
\value{A list with the table, diagnostics and maxima.}
\description{80 cumulative exposures per outcome, seed = config seed + 1000 x outcome index, 10,000 replicates.}
