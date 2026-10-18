\name{siw_validate_batch_c}
\alias{siw_validate_batch_c}
\title{Compare the bootstrap summary with the Batch C reference}
\usage{
siw_validate_batch_c(tables, config, reference_dir, plan = siw_load_batch_c_plan())
}
\arguments{
\item{tables}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{reference_dir}{See the package README and configuration file.}
\item{plan}{See the package README and configuration file.}
}
\value{A list with summary and detail.}
\description{Same strict rules as the earlier validators; the DLM L59 rows are declared out of scope (NOT_TESTED).}
