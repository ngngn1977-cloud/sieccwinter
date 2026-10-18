\name{siw_ah_protocol_v2}
\alias{siw_ah_protocol_v2}
\title{ANOM protocol version 2 (post hoc)}
\usage{
siw_ah_protocol_v2(path = NULL, v1_hash = siw_ah_protocol()$protocol_hash, strict = TRUE)
}
\arguments{
\item{path}{See the package README and configuration file.}
\item{v1_hash}{See the package README and configuration file.}
\item{strict}{See the package README and configuration file.}
}
\value{The protocol list with its path, SHA-256 and the extends check.}
\description{Loads config/anomaly_history_validation_protocol_v2.json (declared 12 Oct 2026). With strict = TRUE it fails closed unless the v1 protocol it extends is byte-identical to the one in use. Only the gaussian implied-SD identity is revised; everything else is v1.}
