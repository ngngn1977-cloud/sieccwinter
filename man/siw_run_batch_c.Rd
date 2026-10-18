\name{siw_run_batch_c}
\alias{siw_run_batch_c}
\title{Run Batch C}
\usage{
siw_run_batch_c(data_csv, out_dir, batch_c_reference_dir, config = siw_load_config(),
                accepted_run_dir = NULL, resume = FALSE, replicates = NULL,
                targets = NULL, label = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{batch_c_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{accepted_run_dir}{See the package README and configuration file.}
\item{resume}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{targets}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, run, tables, verification and status.}
\description{Primary cumulative bootstrap (180 target-scheme combinations x 2,000 resamples), independent verification, descriptive scientific-validity assessment, strict comparison with the Batch C reference bundle and the package-source fingerprint; does not rerun earlier batches.}
