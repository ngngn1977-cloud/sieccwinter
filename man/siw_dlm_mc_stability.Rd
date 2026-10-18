\name{siw_dlm_mc_stability}
\alias{siw_dlm_mc_stability}
\title{Monte Carlo stability of the critical value}
\usage{
siw_dlm_mc_stability(fit, seed, draws, n_seeds = 20L, large = 100000L, seed_step = 1000L)
}
\arguments{
\item{fit}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
\item{draws}{See the package README and configuration file.}
\item{n_seeds}{See the package README and configuration file.}
\item{large}{See the package README and configuration file.}
\item{seed_step}{See the package README and configuration file.}
}
\value{A list with the critical values and deviations.}
\description{Alternative seeds at the declared draw count and one large-draw value.}
