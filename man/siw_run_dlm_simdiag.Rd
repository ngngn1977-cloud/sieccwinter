\name{siw_run_dlm_simdiag}
\alias{siw_run_dlm_simdiag}
\title{Simulation diagnostic for the DLM simultaneous critical values}
\usage{
siw_run_dlm_simdiag(data_csv, out_dir, validation_run_dir, dlm_reference_dir = NULL,
                    config = siw_load_config(), label = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{validation_run_dir}{See the package README and configuration file.}
\item{dlm_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses and the diagnostic tables.}
\description{Diagnostic only: identical standard-normal draws turned into multivariate-normal draws with the production eigen factor, a sign-aligned eigen factor and the symmetric square root, for both routes and all nine models; traces covariances, seeds and decompositions, checks factor reconstruction, examines eigenvalue ordering and near-repeated eigenvalues, tests whether orientation explains the discrepancies and clarifies the percentage metrics; changes nothing in production.}
