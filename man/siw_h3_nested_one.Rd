\name{siw_h3_nested_one}
\alias{siw_h3_nested_one}
\title{H3 nested test for one pair}
\usage{
siw_h3_nested_one(dat, config, outcome, metric, lag_end = 56L, lag_df = 3L)
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{metric}{See the package README and configuration file.}
\item{lag_end}{See the package README and configuration file.}
\item{lag_df}{See the package README and configuration file.}
}
\value{A list with the row and the nesting checks.}
\description{Reduced W56 cumulative M2-L model versus the final L01-L56 DLM; nesting verified first; F (with an HC3 Wald) or likelihood-ratio test with 2 df.}
