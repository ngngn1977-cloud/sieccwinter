\name{siw_validate_batch_a}
\alias{siw_validate_batch_a}
\title{Compare Batch A results with the Batch A references}
\usage{
siw_validate_batch_a(tables, config, reference_dir, plan = siw_load_batch_a_plan())
}
\arguments{
\item{tables}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{reference_dir}{See the package README and configuration file.}
\item{plan}{See the package README and configuration file.}
}
\value{A list with summary and detail.}
\description{Same strict rules as the core validator: absent, undeclared or incomplete references FAIL.}
