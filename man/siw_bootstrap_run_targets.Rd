\name{siw_bootstrap_run_targets}
\alias{siw_bootstrap_run_targets}
\title{Run bootstrap targets with progress and resumption}
\usage{
siw_bootstrap_run_targets(dat, config, replicates = 2000L, ckpt_dir = NULL,
                          signature_parts = character(), resume = TRUE, targets = NULL,
                          log = TRUE)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{replicates}{See the package README and configuration file.}
\item{ckpt_dir}{See the package README and configuration file.}
\item{signature_parts}{See the package README and configuration file.}
\item{resume}{See the package README and configuration file.}
\item{targets}{See the package README and configuration file.}
\item{log}{See the package README and configuration file.}
}
\value{A list with plan, results and status_by_target.}
\description{All 180 targets by default, per-target progress with elapsed time and ETA, atomic signature-checked target checkpoints.}
