\name{siw_posthoc_overall}
\alias{siw_posthoc_overall}
\title{Overall class of a post hoc step}
\usage{
siw_posthoc_overall(statuses)
}
\arguments{
\item{statuses}{See the package README and configuration file.}
}
\value{A character string.}
\description{FAIL if any status fails; REVIEW if any is REVIEW or NOT_TESTED; otherwise PASS. NOT_APPLICABLE_SYNTHETIC_INPUT is neutral.}
