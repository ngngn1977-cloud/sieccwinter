\name{siw_bootstrap_summarise}
\alias{siw_bootstrap_summarise}
\title{Summary of bootstrap estimates}
\usage{
siw_bootstrap_summarise(est, replicates = length(est))
}
\arguments{
\item{est}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
}
\value{A one-row data frame.}
\description{Median, type-8 percentile 95 percent interval (at least 20 successful refits), sign fractions and counts of attempted, successful and failed refits, as in the original.}
