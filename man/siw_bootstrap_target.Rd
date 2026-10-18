\name{siw_bootstrap_target}
\alias{siw_bootstrap_target}
\title{Run one bootstrap target and scheme}
\usage{
siw_bootstrap_target(gr, spec, scheme, replicates, seed)
}
\arguments{
\item{gr}{See the package README and configuration file.}
\item{spec}{See the package README and configuration file.}
\item{scheme}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
}
\value{A list with estimates, failure reasons and diagnostics.}
\description{Draws all resamples, refits with lm.fit / glm.fit and classifies failures; failed samples are not redrawn.}
