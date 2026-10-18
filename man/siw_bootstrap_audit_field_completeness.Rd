\name{siw_bootstrap_audit_field_completeness}
\alias{siw_bootstrap_audit_field_completeness}
\title{Field completeness of the audit tables}
\usage{
siw_bootstrap_audit_field_completeness(target, reconstruction)
}
\arguments{
\item{target}{See the package README and configuration file.}
\item{reconstruction}{See the package README and configuration file.}
}
\value{A data frame with expected and unexpected missing counts per column.}
\description{Missing values are expected only where a quantity is undefined by an explicit rule; any other missing value is unexpected and fails verification.}
