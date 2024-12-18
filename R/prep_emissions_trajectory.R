prep_emissions_trajectory <- function(
  equity_results_portfolio,
  bonds_results_portfolio,
  portfolio_name,
  pacta_sectors,
  year_span,
  start_year
) {
  emissions_units <-
    c(
      Automotive = "tons of CO\U00002082 per km per cars produced",
      Aviation = "tons of CO\U00002082 per passenger km per active planes",
      Cement = "tons of CO\U00002082 per tons of cement",
      Coal = "tons of CO\U00002082 per tons of coal",
      `Oil&Gas` = "tons of CO\U00002082 per GJ",
      Power = "tons of CO\U00002082 per MWh",
      Steel = "tons of CO\U00002082 per tons of steel"
    )

  list(
    `Listed Equity` = equity_results_portfolio,
    `Corporate Bonds` = bonds_results_portfolio
  ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::select(
      "asset_class",
      "allocation",
      "equity_market",
      sector = "ald_sector",
      "year",
      plan = "plan_sec_emissions_factor",
      scen = "scen_sec_emissions_factor",
      "scenario",
      "scenario_source"
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.nan(.data[["plan"]])) |>
    tidyr::pivot_longer(c("plan", "scen"), names_to = "plan") |>
    tidyr::unite("name", "sector", "plan", remove = FALSE) |>
    dplyr::mutate(disabled = !.data[["sector"]] %in% .env[["pacta_sectors"]]) |>
    dplyr::mutate(unit = .env[["emissions_units"]][.data[["sector"]]]) |>
    dplyr::group_by(.data[["asset_class"]]) |>
    dplyr::filter(!all(.data[["disabled"]])) |>
    dplyr::mutate(
      equity_market =  dplyr::case_when(
        .data[["equity_market"]] == "GlobalMarket" ~ "Global Market",
        .data[["equity_market"]] == "DevelopedMarket" ~ "Developed Market",
        .data[["equity_market"]] == "EmergingMarket" ~ "Emerging Market",
        TRUE ~ .data[["equity_market"]]
      )
    ) |>
    dplyr::filter(.data[["year"]] <= .env[["start_year"]] + .env[["year_span"]]) |>
    dplyr::arrange(
      .data[["asset_class"]],
      factor(
        .data[["equity_market"]],
        levels = c(
          "Global Market",
          "Developed Market",
          "Emerging Market"
        )
      )
    ) |>
    dplyr::ungroup()
}
