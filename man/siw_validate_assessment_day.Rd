\name{siw_validate_assessment_day}
\alias{siw_validate_assessment_day}
\title{Assessment-day checks}
\usage{
siw_validate_assessment_day(raw, expected_groups = NA_integer_)
}
\arguments{
\item{raw}{See the package README and configuration file.}
\item{expected_groups}{See the package README and configuration file.}
}
\value{A data frame of checks.}
\description{Completeness, integer coding, non-negative values and the number of distinct assessment-day groups (expected 33 for the study data).}
