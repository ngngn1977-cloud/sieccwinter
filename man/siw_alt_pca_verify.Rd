\name{siw_alt_pca_verify}
\alias{siw_alt_pca_verify}
\title{Verify trajectory PCA}
\usage{
siw_alt_pca_verify(config, protocol, features, pca_variance, tol = config$tolerances)
}
\arguments{
\item{config}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{features}{See the package README and configuration file.}
\item{pca_variance}{See the package README and configuration file.}
\item{tol}{See the package README and configuration file.}
}
\value{A data frame with a status per metric.}
\description{Variance fractions from an independent eigen decomposition and absolute scores (sign free).}
