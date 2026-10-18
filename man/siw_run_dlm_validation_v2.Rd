\name{siw_run_dlm_validation_v2}
\alias{siw_run_dlm_validation_v2}
\title{Phase 3D validation protocol V2 (candidate)}
\usage{
siw_run_dlm_validation_v2(data_csv, out_dir, dlm_reference_dir,
                          config = siw_load_config(), accepted_run_dir = NULL,
                          label = NULL, mc = TRUE, manuscript_concordance = TRUE)
}
\arguments{
\item{data_csv}{See the package README and configuration file.}
\item{out_dir}{See the package README and configuration file.}
\item{dlm_reference_dir}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{accepted_run_dir}{See the package README and configuration file.}
\item{label}{See the package README and configuration file.}
\item{mc}{See the package README and configuration file.}
\item{manuscript_concordance}{See the package README and configuration file.}
}
\value{A list with out_dir, statuses, checks, exact_differences and status.}
\description{Reuses the V1 validation and the simulation diagnostic unchanged and re-expresses their evidence as production reproduction, deterministic agreement, orientation-aligned simulation agreement, native-orientation Monte Carlo agreement and a reconstructed reference split into deterministic and simulated parts; saves aggregate covariance matrices, eigen orientation and RNG metadata; keeps exact differences visible.}
