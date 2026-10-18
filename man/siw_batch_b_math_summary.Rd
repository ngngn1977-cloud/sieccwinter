\name{siw_batch_b_math_summary}
\alias{siw_batch_b_math_summary}
\title{Summary of the independent verification}
\usage{
siw_batch_b_math_summary(t, numDeriv_available = TRUE, verify = TRUE)
}
\arguments{
\item{t}{See the package README and configuration file.}
\item{numDeriv_available}{See the package README and configuration file.}
\item{verify}{See the package README and configuration file.}
}
\value{A data frame.}
\description{One row per component (Firth, ordinal, transformations, nonlinear) with PASS, PASS_WITH_DOCUMENTED_POLR_LIMITATION, FAIL or NOT_TESTED. NOT_TESTED is never counted as PASS.}
