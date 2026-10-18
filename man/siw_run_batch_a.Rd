\name{siw_run_batch_a}
\alias{siw_run_batch_a}
\title{Run the core analyses and Batch A with separate comparisons}
\usage{
siw_run_batch_a(data_csv, out_dir, core_reference_dir, batch_reference_dir,
                config = siw_load_config(), expected_assessment_days = NA_integer_,
                label = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{core_reference_dir}{See the package README and configuration file.}
\item{batch_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{expected_assessment_days}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, core, batch_tables, validations, self_checks and status.}
\description{Runs the core paper workflow unchanged, then Batch A (assessment-day checks, influence diagnostics, 240 bivariate tests, descriptive analyses), and compares the core and Batch A results with their reference bundles using the strict validator. Execution, input-check, core-comparison and Batch A-comparison statuses are reported separately.}
