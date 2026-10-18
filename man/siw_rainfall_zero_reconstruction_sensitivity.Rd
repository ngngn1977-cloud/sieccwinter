\name{siw_rainfall_zero_reconstruction_sensitivity}
\alias{siw_rainfall_zero_reconstruction_sensitivity}
\title{Optional rainfall zero-reconstruction sensitivity}
\usage{
siw_rainfall_zero_reconstruction_sensitivity(dat, config, lag_end = 56L)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
}
\value{A modified copy of the data with attribute sensitivity.}
\description{Sets a missing daily rainfall total to 0 where the rainy-day indicator is 0 and rebuilds cumulative fields. Never used in the primary run.}
