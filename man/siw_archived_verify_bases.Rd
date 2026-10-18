\name{siw_archived_verify_bases}
\alias{siw_archived_verify_bases}
\title{Lag bases of the archived specifications}
\usage{
siw_archived_verify_bases(config = siw_load_config())
}
\arguments{
\item{config}{See the package README and configuration file.}
}
\value{A data frame of checks.}
\description{Verifies the nine lag bases (L = 35, 45, 59; df = 2, 3, 4) against the independent construction; equality with dlnm is NOT_TESTED where dlnm is not installed.}
