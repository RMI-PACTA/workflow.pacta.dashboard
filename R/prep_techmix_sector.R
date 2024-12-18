prep_techmix_sector <- function(
  equity_results_portfolio,
  bonds_results_portfolio,
  indices_eq_results_portfolio,
  indices_cb_results_portfolio,
  peers_equity_results_portfolio,
  peers_bonds_results_portfolio,
  investor_name,
  portfolio_name,
  start_year,
  year_span,
  peer_group,
  green_techs,
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
    dplyr::filter(!is.na(.data[["ald_sector"]]))

  asset_classes <-
    portfolio |>
    dplyr::pull("asset_class") |>
    unique()

  equity_sectors <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Listed Equity") |>
    dplyr::filter(.data[["allocation"]] == "portfolio_weight") |>
    dplyr::pull("ald_sector") |>
    unique()

  bonds_sectors <-
    portfolio |>
    dplyr::filter(.data[["asset_class"]] == "Corporate Bonds") |>
    dplyr::pull("ald_sector") |>
    unique()

  indices <-
    list(
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

  peers <-
    list(
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

  techexposure_data <-
    dplyr::bind_rows(portfolio, peers, indices) |>
    dplyr::filter(.data[["allocation"]] == "portfolio_weight") |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::filter(
      .data[["year"]] %in% c(
        .env[["start_year"]],
        .env[["start_year"]] + .env[["year_span"]]
      )
    )

  if (nrow(techexposure_data) > 0L) {
    techexposure_data <-
      techexposure_data |>
      dplyr::mutate(green = .data[["technology"]] %in% .env[["green_techs"]]) |>
      dplyr::group_by(
        .data[["asset_class"]],
        .data[["equity_market"]],
        .data[["portfolio_name"]],
        .data[["ald_sector"]],
        .data[["scenario"]],
        .data[["year"]]
      ) |>
      dplyr::mutate(
        plan_alloc_wt_sec_prod = sum(.data[["plan_alloc_wt_tech_prod"]]),
        scen_alloc_wt_sec_prod = sum(.data[["scen_alloc_wt_tech_prod"]])
      ) |>
      dplyr::mutate(
        production_plan = dplyr::if_else(
          .data[["plan_alloc_wt_tech_prod"]] > 0L,
          (
            .data[["plan_alloc_wt_tech_prod"]] /
              .data[["plan_alloc_wt_sec_prod"]]
          ),
          0L
        ),
        scenario_plan = dplyr::if_else(
          .data[["scen_alloc_wt_tech_prod"]] > 0L,
          (
            .data[["scen_alloc_wt_tech_prod"]] /
              .data[["scen_alloc_wt_sec_prod"]]
          ),
          0L
        )
      ) |>
      dplyr::group_by(
        .data[["asset_class"]],
        .data[["equity_market"]],
        .data[["portfolio_name"]],
        .data[["ald_sector"]],
        .data[["scenario"]],
        .data[["year"]],
        .data[["green"]]
      ) |>
      dplyr::mutate(
        green_sum_prod = sum(.data[["production_plan"]]),
        green_sum_scenario = sum(.data[["scenario_plan"]])
      ) |>
      dplyr::ungroup() |>
      select(
        "asset_class",
        "investor_name",
        "portfolio_name",
        "scenario_source",
        "scenario",
        "allocation",
        "equity_market",
        "year",
        "ald_sector",
        "technology",
        "production_plan",
        "scenario_plan",
        "green",
        "green_sum_prod",
        "green_sum_scenario"
      ) |>
      tidyr::pivot_longer(
        cols = -c( #nolint
          "asset_class",
          "investor_name",
          "portfolio_name",
          "scenario_source",
          "scenario",
          "allocation",
          "equity_market",
          "year",
          "ald_sector",
          "technology",
          "green",
          "green_sum_prod",
          "green_sum_scenario"
        ),
        names_to = "val_type", values_to = "value") |>
      dplyr::mutate(
        green_sum = dplyr::if_else(
          .data[["val_type"]] == "production_plan",
          .data[["green_sum_prod"]],
          .data[["green_sum_scenario"]]
        )
      ) |>
      select(-c("green_sum_prod", "green_sum_scenario")) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        this_portfolio = .data[["portfolio_name"]] == .env[["portfolio_name"]],
        val_type = dplyr::if_else(
          .data[["this_portfolio"]],
          paste0(.data[["val_type"]], "_portfolio"),
          paste0(.data[["val_type"]], "_benchmark")
        )
      ) |>
      dplyr::mutate(
        equity_market =  dplyr::case_when(
          .data[["equity_market"]] == "GlobalMarket" ~ "Global Market",
          .data[["equity_market"]] == "DevelopedMarket" ~ "Developed Market",
          .data[["equity_market"]] == "EmergingMarket" ~ "Emerging Market",
          TRUE ~ .data[["equity_market"]]
        )
      ) |>
      # no need for showing scenario mix for the benchmark
      dplyr::filter(
        .data[["val_type"]] != "scenario_plan_benchmark"
      ) |>
      dplyr::mutate(
        val_type =  dplyr::case_when(
          .data[["val_type"]] == "production_plan_portfolio" ~ "Portfolio",
          .data[["val_type"]] == "scenario_plan_portfolio" ~ "Scenario",
          .data[["val_type"]] == "production_plan_benchmark" ~ "Benchmark",
          TRUE ~ .data[["val_type"]]
        )
      ) |>
      dplyr::arrange(
        .data[["asset_class"]],
        factor(
          .data[["equity_market"]],
          levels = c("Global Market", "Developed Market", "Emerging Market")
        ),
        desc(.data[["this_portfolio"]]),
        factor(
          .data[["val_type"]],
          levels = c("Portfolio", "Scenario", "Benchmark")
        ),
        .data[["portfolio_name"]],
        factor(.data[["technology"]], levels = .env[["all_tech_levels"]])
      ) |>
      select(
        "asset_class",
        "equity_market",
        "portfolio_name",
        "scenario",
        "scenario_source",
        "this_portfolio",
        "val_type",
        "ald_sector",
        "technology",
        "value",
        "green",
        "green_sum",
        "year"
      ) |>
      dplyr::filter(
        !(
          .data[["year"]] == .env[["start_year"]] &
            .data[["val_type"]] == "Scenario"
        )
      )
  }

  techexposure_data
}
