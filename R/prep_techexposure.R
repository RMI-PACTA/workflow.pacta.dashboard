prep_techexposure <- function(
  equity_results_portfolio,
  bonds_results_portfolio,
  investor_name,
  portfolio_name,
  indices_eq_results_portfolio,
  indices_cb_results_portfolio,
  peers_equity_results_portfolio,
  peers_bonds_results_portfolio,
  peer_group,
  select_scenario_other,
  select_scenario,
  start_year,
  green_techs,
  equity_market_levels,
  all_tech_levels
) {
  portfolio <- list(
    `Listed Equity` = equity_results_portfolio,
    `Corporate Bonds` = bonds_results_portfolio
  ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(
      .data[["investor_name"]] == .env[["investor_name"]],
      .data[["portfolio_name"]] == .env[["portfolio_name"]]
    ) |>
    dplyr::filter(!is.na(.data[["ald_sector"]]))

  asset_classes <- portfolio |>
    dplyr::pull("asset_class") |>
    unique()

  equity_sectors <- portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Listed Equity") |>
    dplyr::pull("ald_sector") |>
    unique()

  bonds_sectors <- portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Corporate Bonds") |>
    dplyr::pull("ald_sector") |>
    unique()

  indices <- list(
    `Listed Equity` = indices_eq_results_portfolio,
    `Corporate Bonds` = indices_cb_results_portfolio
  ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(.data[["asset_class"]] %in% .env[["asset_classes"]]) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["ald_sector"]] %in% .env[["equity_sectors"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["ald_sector"]] %in% .env[["bonds_sectors"]]
      )
    )

  peers <- list(
    `Listed Equity` = peers_equity_results_portfolio,
    `Corporate Bonds` = peers_bonds_results_portfolio
  ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(.data[["asset_class"]] %in% .env[["asset_classes"]]) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["ald_sector"]] %in% .env[["equity_sectors"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["ald_sector"]] %in% .env[["bonds_sectors"]]
      )
    ) |>
    dplyr::filter(.data[["investor_name"]] == .env[["peer_group"]])

  dplyr::bind_rows(portfolio, peers, indices) |>
    dplyr::filter(.data[["allocation"]] == "portfolio_weight") |>
    filter_scenarios_per_sector(
      select_scenario_other,
      select_scenario
    ) |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::filter(.data[["year"]] == .env[["start_year"]]) |>
    dplyr::filter(.data[["equity_market"]] == "GlobalMarket") |>
    dplyr::mutate(green = .data[["technology"]] %in% .env[["green_techs"]]) |>
    dplyr::group_by(
      .data[["asset_class"]],
      .data[["equity_market"]],
      .data[["portfolio_name"]],
      .data[["ald_sector"]]
    ) |>
    dplyr::arrange(
      .data[["asset_class"]],
      .data[["portfolio_name"]],
      factor(.data[["technology"]], levels = all_tech_levels),
      dplyr::desc(.data[["green"]])
    ) |>
    dplyr::mutate(sector_sum = sum(.data[["plan_carsten"]])) |>
    dplyr::mutate(
      sector_prcnt = .data[["plan_carsten"]] / sum(.data[["plan_carsten"]])
    ) |>
    dplyr::mutate(sector_cumprcnt = cumsum(.data[["sector_prcnt"]])) |>
    dplyr::mutate(
      sector_cumprcnt = dplyr::lag(.data[["sector_cumprcnt"]], default = 0L)
    ) |>
    dplyr::mutate(cumsum = cumsum(.data[["plan_carsten"]])) |>
    dplyr::mutate(cumsum = dplyr::lag(.data[["cumsum"]], default = 0L)) |>
    dplyr::ungroup() |>
    dplyr::group_by(
      .data[["asset_class"]],
      .data[["equity_market"]],
      .data[["portfolio_name"]],
      .data[["ald_sector"]],
      .data[["green"]]
    ) |>
    dplyr::mutate(green_sum = sum(.data[["plan_carsten"]])) |>
    dplyr::mutate(
      green_prcnt = sum(.data[["plan_carsten"]]) / .data[["sector_sum"]]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      this_portfolio = .data[["portfolio_name"]] == .env[["portfolio_name"]]
    ) |>
    dplyr::mutate(
      equity_market =  dplyr::case_when(
        .data[["equity_market"]] == "GlobalMarket" ~ "Global Market",
        .data[["equity_market"]] == "DevelopedMarket" ~ "Developed Market",
        .data[["equity_market"]] == "EmergingMarket" ~ "Emerging Market",
        TRUE ~ .data[["equity_market"]]
      )
    ) |>
    dplyr::arrange(
      .data[["asset_class"]],
      factor(.data[["equity_market"]], levels = equity_market_levels),
      dplyr::desc(.data[["this_portfolio"]]),
      .data[["portfolio_name"]],
      factor(.data[["technology"]], levels = all_tech_levels),
      dplyr::desc(.data[["green"]])
    ) |>
    dplyr::select(
      "asset_class",
      "equity_market",
      "portfolio_name",
      "this_portfolio",
      "ald_sector",
      "technology",
      "plan_carsten",
      "sector_sum",
      "sector_prcnt",
      "cumsum",
      "sector_cumprcnt",
      "green",
      "green_sum",
      "green_prcnt"
    )
}
