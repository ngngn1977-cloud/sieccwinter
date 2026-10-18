\name{siw_fit_dlm}
\alias{siw_fit_dlm}
\title{Fit one distributed-lag model}
\usage{
siw_fit_dlm(dat, config, outcome, metric, lag_end = 56L, lag_df = 3L,
            tier = config$primary_tier, draws = 5000L, seed = NULL, cache = NULL,
            return_curves = TRUE, x_cov = NULL)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{metric}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
\item{lag_df}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
\item{draws}{See the package README and configuration file.}
\item{seed}{See the package README and configuration file.}
\item{cache}{See the package README and configuration file.}
\item{return_curves}{See the package README and configuration file.}
\item{x_cov}{See the package README and configuration file.}
}
\value{A list with summary, lag_curve and cumulative_curve.}
\description{Linear exposure, natural cubic spline over lag; nested F (continuous) or likelihood-ratio (binary) global test; simultaneous critical value from multivariate-normal draws with a fixed seed.}
