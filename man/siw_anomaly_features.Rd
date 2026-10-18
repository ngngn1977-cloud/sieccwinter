\name{siw_anomaly_features}
\alias{siw_anomaly_features}
\title{Window features of an anomaly matrix}
\usage{
siw_anomaly_features(A, min_fraction = 0.8)
}
\arguments{
\item{A}{See the package README and configuration file.}
\item{min_fraction}{See the package README and configuration file.}
}
\value{A list of two participant-by-window matrices.}
\description{Signed and absolute window means for all windows at once, missing when fewer than 80 percent of the days are available.}
