\name{siw_posthoc_reference}
\alias{siw_posthoc_reference}
\title{Verify one declared post hoc reference}
\usage{
siw_posthoc_reference(dir, protocol, file, row_filter = NULL)
}
\arguments{
\item{dir}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{file}{See the package README and configuration file.}
\item{row_filter}{See the package README and configuration file.}
}
\value{A list with table, provenance, ok and status.}
\description{Existence, SHA-256, bytes, readability and declared record count of one reference file; an optional row filter. Files of the post hoc roles are declared in the post hoc protocol, files of the core reference role in the input-identification declaration (see siw_core_reference_spec). Read only; identifiers are never written.}
