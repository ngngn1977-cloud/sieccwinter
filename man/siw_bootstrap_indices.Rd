\name{siw_bootstrap_indices}
\alias{siw_bootstrap_indices}
\title{Bootstrap resample indices}
\usage{
siw_bootstrap_indices(gr, scheme, replicates, seed)
}
\arguments{
\item{gr}{See the package README and configuration file.}
\item{scheme}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
}
\value{A list of integer vectors.}
\description{All resample index vectors of one target and scheme drawn in the original order: sample.int for participants; sample of the dates with all participants of each sampled date for the cluster scheme.}
