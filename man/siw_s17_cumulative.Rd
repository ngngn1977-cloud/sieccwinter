\name{siw_s17_cumulative}
\alias{siw_s17_cumulative}
\title{Archive stage-17 cumulative sensitivity row}
\usage{
siw_s17_cumulative(dat, config, outcome, family, metric, window,
    variant = c("NO_APRIL", "EARLY_LATE_INTERACTION", "DATE_NS_DF2",
                "DATE_NS_DF3", "AGE65"),
                   protocol = siw_posthoc_protocol())
}
\arguments{
\item{dat}{See the package README and configuration file.}
\item{config}{See the package README and configuration file.}
\item{outcome}{See the package README and configuration file.}
\item{family}{See the package README and configuration file.}
\item{metric}{See the package README and configuration file.}
\item{window}{See the package README and configuration file.}
\item{variant}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
}
\value{A one-row data frame.}
\description{Variants NO_APRIL (interview month April removed), EARLY_LATE_INTERACTION and the calendar-spline variants DATE_NS_DF2 / DATE_NS_DF3 (splines::ns(assessment_day, df) replaces the linear centred assessment-day term; extra columns parameter_count, residual_df, p_t and p_primary) and AGE65 (participants with Age < 65 removed; same extra columns), reproduced from the archive engine.}
