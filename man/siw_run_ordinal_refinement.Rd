\name{siw_run_ordinal_refinement}
\alias{siw_run_ordinal_refinement}
\title{Run the ordinal refinement step}
\usage{
siw_run_ordinal_refinement(data_csv, out_dir, batch_b_reference_dir,
                           config = siw_load_config(), legacy_run_dir = NULL,
                           label = NULL)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{batch_b_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{legacy_run_dir}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, legacy, grid, verification, comparison, summary and status.}
\description{Recomputes the legacy polr table (reproduction), refits the 80 ordinal M2-L specifications with the documented scaled and tightly controlled polr protocol, checks every model against the pre-declared acceptance criteria and compares with the legacy table. Writes only to a new folder.}
