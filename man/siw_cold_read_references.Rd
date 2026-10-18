\name{siw_cold_read_references}
\alias{siw_cold_read_references}
\title{Frozen cold-burden reference files}
\usage{
siw_cold_read_references(reference_dir, protocol = siw_cold_burden_protocol(),
                         config = siw_load_config())
}
\arguments{
\item{reference_dir}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
}
\value{A list with tables, provenance and an ok flag.}
\description{Locates and checks the two frozen reference files (SHA-256, bytes, records, unique complete keys, required columns); read only.}
