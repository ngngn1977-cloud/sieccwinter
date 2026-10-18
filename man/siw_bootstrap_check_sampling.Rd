\name{siw_bootstrap_check_sampling}
\alias{siw_bootstrap_check_sampling}
\title{Check the bootstrap sampling}
\usage{
siw_bootstrap_check_sampling(gr, scheme, replicates, seed, indices = NULL)
}
\arguments{
\item{gr}{See the package README and configuration file.}
\item{scheme}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
\item{indices}{See the package README and configuration file.}
}
\value{A one-row data frame.}
\description{Invariants on every replicate (participant: valid indices; cluster: all participants of every sampled occurrence retained, in sampled order) and uniformity of selection.}
