% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_exposure_stats.R
\name{prep_exposure_stats}
\alias{prep_exposure_stats}
\title{Prepare exposure statistics for inclusion in the dashboard}
\usage{
prep_exposure_stats(
  audit_file,
  investor_name,
  portfolio_name,
  pacta_sectors,
  currency_exchange_value
)
}
\arguments{
\item{audit_file}{(data.frame) Audit file contents}

\item{investor_name}{(character scalar) Investor name}

\item{portfolio_name}{(character scalar) Portfolio name}

\item{pacta_sectors}{(character vector) PACTA sectors}

\item{currency_exchange_value}{(numeric scalar) Currency exchange value to
convert to USD}
}
\value{
(data.frame) suitible for serialization to JSON (using
\code{jsonlite::toJSON}/\code{jsonlite::write_json})
}
\description{
Prepare JSON data for the exposure statistics.
}
