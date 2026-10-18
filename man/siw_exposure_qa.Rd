\name{siw_exposure_qa}
\alias{siw_exposure_qa}
\title{Exposure QA of the analysed IMS run}
\usage{
siw_exposure_qa(dat, gis, it, protocol = siw_posthoc_protocol(), lag_end = 56L)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{gis}{See the package README and configuration file.}
\item{it}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
}
\value{A list of aggregate tables.}
\description{Post hoc step 27 (RECONSTRUCTION): source identity of the run's daily IDW values with the canonical CSV, daily coverage of the three assigned stations per participant-day with the DST-day and 1-minute-rain rules, interpolated-value support, assigned-station distances and the station pool; summaries by locality group.}
