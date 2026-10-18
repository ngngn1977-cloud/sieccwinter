\name{siw_posthoc_protocol}
\alias{siw_posthoc_protocol}
\title{Load the post hoc protocol}
\usage{
siw_posthoc_protocol(path = NULL)
}
\arguments{
\item{path}{See the package README and configuration file.}
}
\value{The protocol list with its path and SHA-256.}
\description{Reads config/posthoc_sensitivity_protocol_v2.json (version 2 of 14 Oct 2026; version 1 is kept unchanged beside it): calendar anchor, exposure provenance, reference files (names, bytes, SHA-256), each step's original implementation, comparisons and SI / manuscript targets.}
