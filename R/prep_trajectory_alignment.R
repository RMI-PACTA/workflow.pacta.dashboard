prep_trajectory_alignment <- function(
  equity_results_portfolio,
  bonds_results_portfolio,
  peers_equity_results_portfolio,
  peers_bonds_results_portfolio,
  indices_eq_results_portfolio,
  indices_cb_results_portfolio,
  investor_name,
  portfolio_name,
  tech_roadmap_sectors,
  peer_group,
  start_year,
  year_span,
  scen_geo_levels,
  all_tech_levels
) {

  portfolio <-
    list(
      `Listed Equity` = equity_results_portfolio,
      `Corporate Bonds` = bonds_results_portfolio
    ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(
      .data[["investor_name"]] == .env[["investor_name"]],
      .data[["portfolio_name"]] == .env[["portfolio_name"]]
    ) |>
    dplyr::filter(.data[["ald_sector"]] %in% .env[["tech_roadmap_sectors"]]) |>
    dplyr::filter(.data[["scenario_geography"]] != "GlobalAggregate") |>
    dplyr::group_by(
      .data[["asset_class"]],
      .data[["allocation"]],
      .data[["equity_market"]],
      .data[["technology"]],
      .data[["scenario"]]
    ) |>
    dplyr::filter(dplyr::n() > 1L) |>
    dplyr::ungroup()

  asset_classes <-
    portfolio |>
    dplyr::pull("asset_class") |>
    unique()

  equity_markets <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Listed Equity") |>
    dplyr::pull("equity_market") |>
    unique()

  bonds_markets <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Corporate Bonds") |>
    dplyr::pull("equity_market") |>
    unique()

  equity_techs <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Listed Equity") |>
    dplyr::pull("technology") |>
    unique()

  equity_scenario_geography <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Listed Equity") |>
    dplyr::pull("scenario_geography") |>
    unique()

  bonds_scenario_geography <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Corporate Bonds") |>
    dplyr::pull("scenario_geography") |>
    unique()

  bonds_techs <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Corporate Bonds") |>
    dplyr::pull("technology") |>
    unique()

  peers <-
    list(
      `Listed Equity` = peers_equity_results_portfolio,
      `Corporate Bonds` = peers_bonds_results_portfolio
    ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(.data[["ald_sector"]] %in% .env[["tech_roadmap_sectors"]]) |>
    dplyr::filter(.data[["scenario_geography"]] != "GlobalAggregate") |>
    dplyr::filter(.data[["asset_class"]] %in% .env[["asset_classes"]]) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["equity_market"]] %in% .env[["equity_markets"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["equity_market"]] %in% .env[["bonds_markets"]]
      )
    ) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["technology"]] %in% .env[["equity_techs"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["technology"]] %in% .env[["bonds_techs"]]
      )
    ) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["scenario_geography"]] %in% .env[["equity_scenario_geography"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["scenario_geography"]] %in% .env[["bonds_scenario_geography"]]
      )
    ) |>
    dplyr::filter(.data[["investor_name"]] == .env[["peer_group"]])

  indices <-
    list(
      `Listed Equity` = indices_eq_results_portfolio,
      `Corporate Bonds` = indices_cb_results_portfolio
    ) |>
    dplyr::bind_rows(.id = "asset_class") |>
    dplyr::filter(.data[["ald_sector"]] %in% .env[["tech_roadmap_sectors"]]) |>
    dplyr::filter(.data[["scenario_geography"]] != "GlobalAggregate") |>
    dplyr::filter(.data[["asset_class"]] %in% .env[["asset_classes"]]) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["equity_market"]] %in% .env[["equity_markets"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["equity_market"]] %in% .env[["bonds_markets"]]
      )
    ) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["technology"]] %in% .env[["equity_techs"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["technology"]] %in% .env[["bonds_techs"]]
      )
    ) |>
    dplyr::filter(
      (
        .data[["asset_class"]] == "Listed Equity" &
          .data[["scenario_geography"]] %in% .env[["equity_scenario_geography"]]
      ) | (
        .data[["asset_class"]] == "Corporate Bonds" &
          .data[["scenario_geography"]] %in% .env[["bonds_scenario_geography"]]
      )
    )

  benchmark_data <- dplyr::bind_rows(peers, indices)

  cols_with_supporting_info <- c(
    "benchmark",
    "portfolio_name",
    "asset_class",
    "equity_market",
    "scenario_source",
    "scenario_geography",
    "allocation",
    "ald_sector",
    "technology",
    "year",
    "unit"
  )

  list(
    portfolio = portfolio,
    benchmark = benchmark_data
  ) |>
    dplyr::bind_rows(.id = "benchmark") |>
    dplyr::mutate(benchmark = .data[["benchmark"]] == "benchmark") |>
    dplyr::mutate(
      unit = dplyr::case_when(
        .data[["ald_sector"]] == "Power" ~ "MW",
        .data[["ald_sector"]] == "Oil&Gas" ~ "GJ/a",
        .data[["ald_sector"]] == "Coal" ~ "t/a",
        .data[["ald_sector"]] == "Automotive" ~ "number of cars",
        .data[["ald_sector"]] == "Aviation" ~ "number of planes",
        .data[["ald_sector"]] == "Cement" ~ "t/a",
        .data[["ald_sector"]] == "Steel" ~ "t/a"
      )
    ) |>
    select(
      dplyr::all_of(cols_with_supporting_info),
      "scenario",
      production = "plan_alloc_wt_tech_prod",
      "scen_alloc_wt_tech_prod"
    ) |>
    tidyr::pivot_wider(
      names_from = "scenario",
      values_from = "scen_alloc_wt_tech_prod"
    ) |>
    tidyr::pivot_longer(
      cols = -cols_with_supporting_info,
      names_to = "scenario",
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        .data[["year"]] > min(.data[["year"]] + 5L) & .data[["value"]] == 0L,
        NA_real_,
        .data[["value"]]
      )
    ) |>
    dplyr::filter(!is.na(.data[["value"]])) |>
    dplyr::filter(.data[["scenario"]] == "production" | !.data[["benchmark"]]) |>
    dplyr::mutate(
      equity_market =  dplyr::case_when(
        .data[["equity_market"]] == "GlobalMarket" ~ "Global Market",
        .data[["equity_market"]] == "DevelopedMarket" ~ "Developed Market",
        .data[["equity_market"]] == "EmergingMarket" ~ "Emerging Market",
        TRUE ~ .data[["equity_market"]]
      )
    ) |>
    dplyr::mutate(
      allocation = dplyr::case_when(
        .data[["allocation"]] == "portfolio_weight" ~ "Portfolio Weight",
        .data[["allocation"]] == "ownership_weight" ~ "Ownership Weight"
      )
    ) |>
    dplyr::filter(.data[["year"]] <= .env[["start_year"]] + .env[["year_span"]]) |>
    dplyr::arrange(
      .data[["asset_class"]],
      factor(
        .data[["equity_market"]],
        levels = c("Global Market", "Developed Market", "Emerging Market")
      ),
      factor(
        .data[["scenario_source"]],
        levels = c("WEO2021", "GECO2021", "ETP2020", "IPR2021", "ISF2021")
      ),
      factor(.data[["scenario_geography"]], levels = .env[["scen_geo_levels"]]),
      factor(.data[["technology"]], levels = .env[["all_tech_levels"]])
    )
}
