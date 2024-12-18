# prep_company_bubble ----------------------------------------------------------
# based on pacta.portfolio.report:::prep_company_bubble, but does not filter to
# allocation == "portfolio_weight" nor by scenario and scenario source
prep_company_bubble <- function(
  equity_results_company,
  bonds_results_company,
  portfolio_name,
  start_year,
  green_techs
) {

  equity_data <-
    equity_results_company |>
    filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    filter(.data[["equity_market"]] == "GlobalMarket") |>
    filter(.data[["scenario_geography"]] == "Global") |>
    filter(
      .data[["year"]] %in% c(
        .env[["start_year"]],
        .env[["start_year"]] + 5L
      )
    ) |>
    mutate(
      plan_buildout = (
        last(
          .data[["plan_tech_prod"]],
          order_by = .data[["year"]]
        ) - first(
          .data[["plan_tech_prod"]],
          order_by = .data[["year"]]
        )
      ),
      scen_buildout = (
        last(
          .data[["scen_tech_prod"]],
          order_by = .data[["year"]]
        ) - first(
          .data[["scen_tech_prod"]],
          order_by = .data[["year"]]
        )
      ),
      .by = c(
        "company_name",
        "technology",
        "scenario_source",
        "scenario",
        "allocation"
      )
    ) |>
    filter(.data[["year"]] == .env[["start_year"]]) |>
    mutate(green = .data[["technology"]] %in% .env[["green_techs"]]) |>
    reframe(
      plan_tech_share = sum(.data[["plan_tech_share"]], na.rm = TRUE),
      plan_buildout = sum(.data[["plan_buildout"]], na.rm = TRUE),
      scen_buildout = sum(.data[["scen_buildout"]], na.rm = TRUE),
      plan_carsten = sum(.data[["plan_carsten"]], na.rm = TRUE),
      port_weight = unique(.data[["port_weight"]]),
      .by = c(
        "company_name",
        "allocation",
        "scenario_source",
        "scenario",
        "ald_sector",
        "green",
        "year"
      )
    ) |>
    mutate(y = .data[["plan_buildout"]] / .data[["scen_buildout"]]) |>
    filter(.data[["green"]]) |>
    select(-"plan_buildout", -"scen_buildout", -"green") |>
    filter(!is.na(.data[["plan_tech_share"]])) |>
    mutate(y = pmax(.data[["y"]], 0L, na.rm = TRUE)) |>
    mutate(asset_class = "Listed Equity")

  bonds_data <-
    bonds_results_company |>
    filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    filter(.data[["equity_market"]] == "GlobalMarket") |>
    filter(.data[["scenario_geography"]] == "Global") |>
    filter(
      .data[["year"]] %in% c(
        .env[["start_year"]],
        .env[["start_year"]] + 5L
      )
    ) |>
    mutate(
      plan_buildout = last(
        .data[["plan_tech_prod"]],
        order_by = .data[["year"]]
      ) - first(
        .data[["plan_tech_prod"]],
        order_by = .data[["year"]]
      ),
      scen_buildout = (
        last(
          .data[["scen_tech_prod"]],
          order_by = .data[["year"]]
        ) - first(
          .data[["scen_tech_prod"]],
          order_by = .data[["year"]]
        )
      ),
      .by = c(
        "company_name",
        "technology",
        "scenario_source",
        "scenario",
        "allocation"
      )
    ) |>
    filter(.data[["year"]] == .env[["start_year"]]) |>
    mutate(green = .data[["technology"]] %in% .env[["green_techs"]]) |>
    reframe(
      plan_tech_share = sum(.data[["plan_tech_share"]], na.rm = TRUE),
      plan_buildout = sum(.data[["plan_buildout"]], na.rm = TRUE),
      scen_buildout = sum(.data[["scen_buildout"]], na.rm = TRUE),
      plan_carsten = sum(.data[["plan_carsten"]], na.rm = TRUE),
      port_weight = unique(.data[["port_weight"]]),
      .by = c(
        "company_name",
        "allocation",
        "scenario_source",
        "scenario",
        "ald_sector",
        "green",
        "year"
      )
    ) |>
    mutate(y = .data[["plan_buildout"]] / .data[["scen_buildout"]]) |>
    filter(.data[["green"]]) |>
    select(-"plan_buildout", -"scen_buildout", -"green") |>
    filter(!is.na(.data[["plan_tech_share"]])) |>
    mutate(y = pmax(.data[["y"]], 0L, na.rm = TRUE)) |>
    mutate(asset_class = "Corporate Bonds")

  bind_rows(equity_data, bonds_data)
}
