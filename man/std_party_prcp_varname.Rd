% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize-varname.R
\name{std_party_prcp_varname}
\alias{std_party_prcp_varname}
\title{Standardize name of party perception variable across different CCES datasets}
\usage{
std_party_prcp_varname(df, prefix = "party_prcp_")
}
\arguments{
\item{df}{CCES data frame with multiple "perceived vote" questions. Slimmed.}

\item{prefix}{character that recognizes the perceived vote question}
}
\value{
The same df but with variables renamed with suffixes.
}
\description{
Relies on having the office
}
