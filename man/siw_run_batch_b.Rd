\name{siw_run_batch_b}
\alias{siw_run_batch_b}
\title{Run the core analyses, Batch A and Batch B with separate comparisons}
\usage{
siw_run_batch_b(data_csv, out_dir, core_reference_dir, batch_a_reference_dir,
                batch_b_reference_dir, config = siw_load_config(),
                expected_assessment_days = NA_integer_, label = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{core_reference_dir}{See the package README and configuration file.}
\item{batch_a_reference_dir}{See the package README and configuration file.}
\item{batch_b_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{expected_assessment_days}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, math, batch_a, batch_tables, batch_validation, self_checks and status.}
\description{Runs the core workflow and Batch A unchanged, then Batch B (Firth logistic sensitivity, ordinal integration, exposure transformations, nonlinear forms and their independent mathematical verification), and compares the core, Batch A and Batch B results with their reference bundles using the strict validator. Execution, input-check, the three comparison statuses and the mathematical-verification status are reported separately.}
