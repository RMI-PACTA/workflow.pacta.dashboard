% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_key_bars_portfolio.R
\name{prep_key_bars_portfolio}
\alias{prep_key_bars_portfolio}
\title{Prepare data for portfolio-level technology exposoure plot}
\usage{
prep_key_bars_portfolio(
  equity_results_portfolio,
  bonds_results_portfolio,
  portfolio_name,
  start_year,
  pacta_sectors_not_analysed,
  all_tech_levels
)
}
\arguments{
\item{equity_results_portfolio}{(data.frame) Portfolio-level results from
\code{workflow.pacta} for listed equity}

\item{bonds_results_portfolio}{(data.frame) Portfolio-level results from
\code{workflow.pacta} for corporate bonds}

\item{portfolio_name}{(character scalar) Portfolio name}

\item{start_year}{(integer scalar) Start year for analysis}

\item{pacta_sectors_not_analysed}{(character vector) PACTA sectors not to be
analysed}

\item{all_tech_levels}{(character vector) List of all technology levels}
}
\value{
(data.frame) suitible for serialization to JSON (using
\code{jsonlite::toJSON}/\code{jsonlite::write_json})
}
\description{
Prepare JSON data for the portfolio-level technology exposoure plot.
Note: This is based on pacta.portfolio.report:::prep_key_bars_portfolio, but
does not filter to allocation == "portfolio_weight"  nor by scenario and
scenario source.
}
