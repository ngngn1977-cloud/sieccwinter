\name{siw_posthoc_role_files}
\alias{siw_posthoc_role_files}
\title{Resolve the declared files of a post hoc reference role}
\usage{
siw_posthoc_role_files(ref, protocol, role)
}
\arguments{
\item{ref}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{role}{See the package README and configuration file.}
}
\value{A character vector of paths named by the declared file names (NA when not resolved).}
\description{A folder holding the declared names, or several files separated by | that are matched to the declared files by SHA-256 (participant-level inputs are passed in place, never staged or copied).}
