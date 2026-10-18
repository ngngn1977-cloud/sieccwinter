\name{siw_archived_read_references}
\alias{siw_archived_read_references}
\title{Frozen reference files for the archived DLM validation}
\usage{
siw_archived_read_references(reference_dir, protocol = siw_archived_validation_protocol(),
                             config = siw_load_config())
}
\arguments{
\item{reference_dir}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
}
\value{A list with the tables, the provenance data frame and an ok flag.}
\description{Locates and checks the five frozen reference files (existence, SHA-256, record counts, unique complete keys, required columns); read only.}
