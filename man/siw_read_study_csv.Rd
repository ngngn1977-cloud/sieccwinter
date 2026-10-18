\name{siw_read_study_csv}
\alias{siw_read_study_csv}
\title{Read the canonical study CSV}
\usage{
siw_read_study_csv(path, config)
}
\arguments{
\item{path}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Reads a CSV with numeric type enforcement. Entries that cannot be parsed are recorded in attribute parse_failures. SPSS files are refused.}
