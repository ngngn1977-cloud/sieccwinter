\name{siw_transformation_grid}
\alias{siw_transformation_grid}
\title{The 1,080 exposure-transformation specifications}
\usage{
siw_transformation_grid(dat, config, tier = config$primary_tier)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{tier}{See the package README and configuration file.}
}
\value{A data frame.}
\description{Raw, z, signed-log z and exp z for all metrics plus log1p and sqrt for the sum-type metrics, M2-L, with BH per outcome and global; domain violations are reported per specification.}
