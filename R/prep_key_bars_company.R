# prep_key_bars_company --------------------------------------------------------
# based on pacta.portfolio.report:::prep_key_bars_company, but does not filter
# to allocation == "portfolio_weight"  nor by scenario and scenario source
prep_key_bars_company <- function(
  equity_results_company,
  bonds_results_company,
  portfolio_name,
  start_year,
  pacta_sectors_not_analysed,
  all_tech_levels
) {

  equity_data_company <-
    equity_results_company |>
    dplyr::filter(.data$portfolio_name == .env$portfolio_name) |>
    dplyr::filter(.data$year %in% c(.env$start_year + 5)) |>
    dplyr::filter(
      .data$equity_market %in% c(
        "Global",
        "GlobalMarket"
      )
    ) |>
    dplyr::filter(.data$scenario_geography == "Global") |>
    dplyr::filter(
      .data$ald_sector %in% c(
        "Power",
        "Automotive"
      )
    ) |>
    dplyr::select(-"id") |>
    dplyr::rename(id = "company_name") |>
    dplyr::select(
      "id",
      "ald_sector",
      "technology",
      "plan_tech_share",
      "port_weight",
      "allocation",
      "scenario_source",
      "scenario",
      "year"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$port_weight)
    ) |>
    dplyr::mutate(asset_class = "Listed Equity") |>
    # convert the col type to character to prevent errors in case empty df is
    # binded by rows
    dplyr::mutate_at("id", as.character) |>
    # select at most 15 companies with the highest weigths per
    # sector+technology
    dplyr::group_by(.data$ald_sector, .data$technology) |>
    dplyr::arrange(
      dplyr::desc(.data$port_weight),
      .by_group = TRUE
    ) |>
    dplyr::slice(1:15)  |>
    dplyr::filter(!is.null(.data$port_weight)) |>
    dplyr::filter(!is.null(.data$plan_tech_share))

  bonds_data_company <-
    bonds_results_company |>
    dplyr::filter(.data$portfolio_name == .env$portfolio_name) |>
    dplyr::filter(.data$year %in% c(.env$start_year + 5)) |>
    dplyr::filter(
      .data$equity_market %in% c(
        "Global",
        "GlobalMarket"
      )
    ) |>
    dplyr::filter(.data$scenario_geography == "Global") |>
    dplyr::filter(
      .data$ald_sector %in% c(
        "Power",
        "Automotive"
      )
    ) |>
    dplyr::select(-"id") |>
    dplyr::rename(id = "company_name") |>
    dplyr::select(
      "id",
      "ald_sector",
      "technology",
      "plan_tech_share",
      "port_weight",
      "allocation",
      "scenario_source",
      "scenario",
      "year"
    ) |>
    dplyr::group_by(
      .data$id,
      .data$ald_sector,
      .data$technology
    ) |>
    dplyr::mutate(port_weight = sum(.data$port_weight, na.rm = TRUE)) |>
    dplyr::group_by(.data$id, .data$technology) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::filter(
      !.data$ald_sector %in% .env$pacta_sectors_not_analysed |
        !grepl("Aligned", .data$id)
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$port_weight)
    ) |>
    dplyr::mutate(asset_class = "Corporate Bonds") |>
    # convert the col type to character to prevent errors in case empty df is
    # bound by rows
    dplyr::mutate_at(
      "id",
      as.character
    ) |>
    # select at most 15 companies with the highest weigths per
    # sector+technology
    dplyr::group_by(
      .data$ald_sector,
      .data$technology
    ) |>
    dplyr::arrange(.data$port_weight, .by_group = TRUE) |>
    dplyr::slice(1:15) |>
    dplyr::group_by(.data$ald_sector) |>
    dplyr::arrange(
      factor(
        .data$technology,
        levels = .env$all_tech_levels
      )
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$port_weight),
      .by_group = TRUE
    ) |>
    dplyr::filter(!is.null(.data$port_weight)) |>
    dplyr::filter(!is.null(.data$plan_tech_share))

  dplyr::bind_rows(equity_data_company, bonds_data_company) |>
    dplyr::mutate(scenario = sub("_", " ", .data$scenario))
}
