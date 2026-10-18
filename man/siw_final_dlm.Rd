\name{siw_final_dlm}
\alias{siw_final_dlm}
\title{Fit the final L01-L56 distributed-lag models}
\usage{
siw_final_dlm(dat, config, draws = config$dlm$final$draws)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{draws}{See the package README and configuration file.}
}
\value{A list with summary, lag_curves and cumulative_curves.}
\description{Nine M2-L models with global tests, pointwise and simultaneous bands and BH q9.}
