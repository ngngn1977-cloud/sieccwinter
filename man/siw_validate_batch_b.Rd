\name{siw_validate_batch_b}
\alias{siw_validate_batch_b}
\title{Compare Batch B results with the Batch B references}
\usage{
siw_validate_batch_b(tables, config, reference_dir, plan = siw_load_batch_b_plan())
}
\arguments{
\item{tables}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{reference_dir}{See the package README and configuration file.}
\item{plan}{See the package README and configuration file.}
}
\value{A list with summary, detail and attribution.}
\description{Same strict rules as the core and Batch A validators; Firth coefficient and interval cells beyond the cumulative tolerance are additionally checked against the declared legacy-convergence bound and labelled, never reported as plain PASS.}
