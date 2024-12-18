# prep_key_bars_portfolio ------------------------------------------------------
# based on pacta.portfolio.report:::prep_key_bars_portfolio, but does not filter
# to allocation == "portfolio_weight" nor by scenario and scenario source

prep_key_bars_portfolio <- function(
  equity_results_portfolio,
  bonds_results_portfolio,
  portfolio_name,
  start_year,
  pacta_sectors_not_analysed,
  all_tech_levels
) {
  equity_data_portfolio <-
    equity_results_portfolio |>
    dplyr::filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    dplyr::filter(.data[["equity_market"]] %in% c("Global", "GlobalMarket")) |>
    dplyr::filter(.data[["year"]] %in% c(.env[["start_year"]] + 5L)) |>
    dplyr::filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::mutate(port_weight = 1L) |>
    dplyr::select(
      "ald_sector",
      "technology",
      "plan_tech_share",
      "scen_tech_share",
      "port_weight",
      "scenario",
      "scenario_source",
      "allocation",
      "year"
    ) |>
    tidyr::pivot_longer(
      c("plan_tech_share", "scen_tech_share"),
      names_to = "plan"
    ) |>
    dplyr::mutate(
      id = dplyr::if_else(
        .data[["plan"]] == "plan_tech_share",
        "Portfolio",
        "Aligned* Portfolio"
      )
    ) |>
    dplyr::rename(plan_tech_share = "value") |>
    dplyr::select(
      "id",
      "ald_sector",
      "technology",
      "plan_tech_share",
      "port_weight",
      "scenario",
      "scenario_source",
      "allocation",
      "year"
    ) |>
    dplyr::filter(
      !(.data[["ald_sector"]] %in% .env[["pacta_sectors_not_analysed"]]) |
        !grepl(
          pattern = "Aligned",
          x = .data[["id"]],
          fixed = TRUE
        )
    ) |>
    dplyr::mutate(asset_class = "Listed Equity") |>
    dplyr::mutate_at("id", as.character) # convert the col type to character to prevent errors in case empty df is bound by rows # nolint

  bonds_data_portfolio <-
    bonds_results_portfolio |>
    dplyr::filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    dplyr::filter(.data[["equity_market"]] %in% c("Global", "GlobalMarket")) |>
    dplyr::filter(.data[["year"]] %in% c(.env[["start_year"]] + 5L)) |>
    dplyr::filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::mutate(port_weight = 1L) |>
    dplyr::select(
      "ald_sector",
      "technology",
      "plan_tech_share",
      "scen_tech_share",
      "port_weight",
      "scenario",
      "scenario_source",
      "allocation",
      "year"
    ) |>
    tidyr::pivot_longer(
      c("plan_tech_share", "scen_tech_share"),
      names_to = "plan"
    ) |>
    dplyr::mutate(
      id = dplyr::if_else(
        .data[["plan"]] == "plan_tech_share",
        "Portfolio",
        "Aligned* Portfolio"
      )
    ) |>
    dplyr::rename(plan_tech_share = "value") |>
    dplyr::select(
      "id",
      "ald_sector",
      "technology",
      "plan_tech_share",
      "port_weight",
      "scenario",
      "scenario_source",
      "allocation",
      "year"
    ) |>
    dplyr::mutate(asset_class = "Corporate Bonds") |>
    dplyr::mutate_at("id", as.character) |>
    dplyr::arrange(
      factor(
        .data[["technology"]],
        levels = .env[["all_tech_levels"]]
      )
    )

  dplyr::bind_rows(equity_data_portfolio, bonds_data_portfolio)
}
