\name{siw_nearest_substitute}
\alias{siw_nearest_substitute}
\title{Substitute nearest-station histories}
\usage{
siw_nearest_substitute(dat, config, nearest, protocol = siw_posthoc_protocol(),
                       lag_end = 56L)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{nearest}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
}
\value{The analysis data frame.}
\description{Replaces the IMS IDW daily histories with nearest-station histories keyed by participant code (declared alias FEATURE_ID) and date (LOCAL_DATE or JERUSALEM_LOCAL_DATE, ISO; lag dates from the calendar anchor) and rebuilds the IMS cumulative fields. No address or coordinate is read.}
