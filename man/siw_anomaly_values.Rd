\name{siw_anomaly_values}
\alias{siw_anomaly_values}
\title{Leave-one-out reference anomalies}
\usage{
siw_anomaly_values(dat, config, protocol = siw_ah_protocol(),
                   reference = c("all", "locality"))
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{reference}{See the package README and configuration file.}
}
\value{A list with the anomaly matrices per metric and the diagnostics.}
\description{Anomalies of every participant-day against the other participants of the same calendar day (all participants or same locality); stable, zero-safe, literal-ratio and z scales with the diagnostics table.}
