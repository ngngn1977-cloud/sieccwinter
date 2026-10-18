\name{siw_compare_tables}
\alias{siw_compare_tables}
\title{Compare a result table with a reference table}
\usage{
siw_compare_tables(new, ref, keys, tol, aliases = list(), name = "reference",
                   subset_reference = FALSE, reference_file = "",
                   skip = c("seed", "simultaneous_draws"), required_columns = character(),
                   expected_records = NA_integer_, required = NA)
}
\arguments{
\item{new}{See the package README and configuration file.}
\item{ref}{See the package README and configuration file.}
\item{keys}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
\item{aliases}{See the package README and configuration file.}
\item{name}{See the package README and configuration file.}
\item{subset_reference}{See the package README and configuration file.}
\item{reference_file}{See the package README and configuration file.}
\item{skip}{See the package README and configuration file.}
\item{required_columns}{See the package README and configuration file.}
\item{expected_records}{See the package README and configuration file.}
\item{required}{See the package README and configuration file.}
}
\value{A list with a one-row summary and a detail table.}
\description{Strict key matching with exact / within-tolerance / different classification. Fails on duplicated or blank keys, unmatched records, missing or non-finite disagreements, missing required columns, record-count mismatches and unclassified or skipped comparisons.}
