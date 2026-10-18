\name{siw_cold_feature_curves}
\alias{siw_cold_feature_curves}
\title{Cold-burden features of every window}
\usage{
siw_cold_feature_curves(H, threshold)
}
\arguments{
\item{H}{See the package README and configuration file.}
\item{threshold}{See the package README and configuration file.}
}
\value{A list of five participants x W matrices.}
\description{Degree burden, cold-day count, persistent cold days, longest cold spell and mean cold intensity for windows 1..W from one pass over the lags; a day is cold if finite and strictly below the threshold.}
