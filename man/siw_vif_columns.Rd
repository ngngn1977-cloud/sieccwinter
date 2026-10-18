\name{siw_vif_columns}
\alias{siw_vif_columns}
\title{Per-column variance inflation factors}
\usage{
siw_vif_columns(X)
}
\arguments{
\item{X}{See the package README and configuration file.}
}
\value{A named numeric vector.}
\description{The diagonal of the inverse correlation matrix of the non-intercept design columns (the Batch A maximum-VIF definition, kept per column).}
