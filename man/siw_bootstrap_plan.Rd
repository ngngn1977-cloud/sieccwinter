\name{siw_bootstrap_plan}
\alias{siw_bootstrap_plan}
\title{The 180 bootstrap target-scheme combinations}
\usage{
siw_bootstrap_plan(config, replicates = 2000L)
}
\arguments{
\item{config}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Outcome x representative metric x window x scheme with the original target indices and per-target seeds (config seed + 100 x index, +1 for the date scheme; one index skipped after each outcome-metric block).}
