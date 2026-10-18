\name{siw_daily_read_references}
\alias{siw_daily_read_references}
\title{Frozen daily-layer reference files}
\usage{
siw_daily_read_references(reference_dir, protocol = siw_daily_protocol(),
                          components = c("XYD", "DAY", "MAXT"))
}
\arguments{
\item{reference_dir}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{components}{See the package README and configuration file.}
}
\value{A list with tables, provenance and an ok flag.}
\description{Locates and checks the three frozen reference files; read only.}
