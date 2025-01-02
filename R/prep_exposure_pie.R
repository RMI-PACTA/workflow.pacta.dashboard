#' Prepare data for exposure pie chart
#'
#' Prepare JSON data for the exposure pie chart.
#'
#' @param data (data.frame) Audit file contents
#' @param asset_type (character scalar) Asset type (`Equity` or `Bonds`)
#' @param investor_name (character scalar) Investor name
#' @param portfolio_name (character scalar) Portfolio name
#' @param pacta_sectors (character vector) PACTA sectors
#' @param currency_exchange_value (numeric scalar) Currency exchange value to
#' convert to USD
#' @return (data.frame) suitible for serialization to JSON (using
#' `jsonlite::toJSON`/`jsonlite::write_json`)
#' @export
prep_exposure_pie <- function(
  data,
  asset_type,
  investor_name,
  portfolio_name,
  pacta_sectors,
  currency_exchange_value
) {
  data |>
    dplyr::filter(
      .data[["investor_name"]] == .env[["investor_name"]],
      .data[["portfolio_name"]] == .env[["portfolio_name"]]
    ) |>
    dplyr::filter(.data[["asset_type"]] %in% c("Bonds", "Equity")) |>
    dplyr::filter(.data[["valid_input"]]) |>
    dplyr::mutate(
      dplyr::across(c("bics_sector", "financial_sector"), as.character)
    ) |>
    dplyr::mutate(
      sector = dplyr::if_else(
        .data[["financial_sector"]] %in% .env[["pacta_sectors"]],
        .data[["financial_sector"]],
        "Other"
      )
    ) |>
    dplyr::group_by(.data[["asset_type"]], .data[["sector"]]) |>
    dplyr::summarise(
      value = (
        sum(
          .data[["value_usd"]],
          na.rm = TRUE
        ) / (
          .env[["currency_exchange_value"]]
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(exploded = .data[["sector"]] %in% .env[["pacta_sectors"]]) |>
    dplyr::arrange(
      .data[["asset_type"]],
      dplyr::desc(.data[["exploded"]]),
      .data[["sector"]]
    ) |>
    dplyr::rename(key = .data[["sector"]]) |>
    dplyr::filter(!is.na(.data[["key"]])) |>
    dplyr::ungroup() |>
    dplyr::filter(.data[["asset_type"]] == .env[["asset_type"]]) |>
    dplyr::select(-"asset_type")
}