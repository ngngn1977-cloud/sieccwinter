\name{siw_core_reference_spec}
\alias{siw_core_reference_spec}
\title{Declared specification of a core reference file}
\usage{
siw_core_reference_spec(file, protocol = NULL, declaration = NULL)
}
\arguments{
\item{file}{See the package README and configuration file.}
\item{protocol}{See the package README and configuration file.}
\item{declaration}{See the package README and configuration file.}
}
\value{A list (role, file, sha256, bytes, expected_records, declared_in, identification), or NULL when the file is declared nowhere.}
\description{SHA-256 and size from the core_reference bundle of the input-identification declaration (project_inputs_declaration_v1.json) and the record count from validation_plan.json; only accepted identification statuses (CONFIRMED_AT_ACCEPTANCE, CONFIRMED_BY_FROZEN_SOURCE, AUTHOR_ACCEPTED_EVIDENCE) yield a hash. Used by the H3 acceptance checks.}
