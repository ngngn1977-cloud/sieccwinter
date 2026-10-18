\name{siw_maxt_outcome}
\alias{siw_maxt_outcome}
\title{Optimised max-|r| permutation test for one outcome}
\usage{
siw_maxt_outcome(xmat, y, replicates, seed, fields = colnames(xmat),
                 outcome = NA_character_, tie_tol = 1e-12)
}
\arguments{
\item{xmat}{See the package README and configuration file.}
\item{y}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
\item{fields}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{tie_tol}{See the package README and configuration file.}
}
\value{A list with the table, maxima and accounting.}
\description{Same sampling sequence as the original; maxima from precomputed centred columns with near-tie recomputation by the original route.}
