#' Prepare data for company bubble plot
#'
#' Prepare JSON data for the company bubble plot.
#' Note: this is based on pacta.portfolio.report:::prep_company_bubble, but
#' does not filter to allocation == "portfolio_weight" nor by scenario and
#' scenario source.
#'
#' @param equity_results_company (data.frame) Company-level results from
#' `workflow.pacta` for listed equity
#' @param bonds_results_company (data.frame) Company-level results from
#' `workflow.pacta` for corporate bonds
#' @param portfolio_name (character scalar) Portfolio name
#' @param start_year (integer scalar) Start year for analysis
#' @param green_techs (character vector) List of green technologies
#' @return (data.frame) suitible for serialization to JSON (using
#' `jsonlite::toJSON`/`jsonlite::write_json`)
#' @export
prep_company_bubble <- function(
  equity_results_company,
  bonds_results_company,
  portfolio_name,
  start_year,
  green_techs
) {
  equity_data <- equity_results_company |>
    dplyr::filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    dplyr::filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    dplyr::filter(.data[["equity_market"]] == "GlobalMarket") |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::filter(
      .data[["year"]] %in% c(
        .env[["start_year"]],
        .env[["start_year"]] + 5L
      )
    ) |>
    dplyr::mutate(
      plan_buildout = (
        dplyr::last(
          .data[["plan_tech_prod"]],
          order_by = .data[["year"]]
        ) - dplyr::first(
          .data[["plan_tech_prod"]],
          order_by = .data[["year"]]
        )
      ),
      scen_buildout = (
        dplyr::last(
          .data[["scen_tech_prod"]],
          order_by = .data[["year"]]
        ) - dplyr::first(
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
    dplyr::filter(.data[["year"]] == .env[["start_year"]]) |>
    dplyr::mutate(green = .data[["technology"]] %in% .env[["green_techs"]]) |>
    dplyr::reframe(
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
    dplyr::mutate(y = .data[["plan_buildout"]] / .data[["scen_buildout"]]) |>
    dplyr::filter(.data[["green"]]) |>
    dplyr::select(-"plan_buildout", -"scen_buildout", -"green") |>
    dplyr::filter(!is.na(.data[["plan_tech_share"]])) |>
    dplyr::mutate(y = pmax(.data[["y"]], 0L, na.rm = TRUE)) |>
    dplyr::mutate(asset_class = "Listed Equity")

  bonds_data <- bonds_results_company |>
    dplyr::filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    dplyr::filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    dplyr::filter(.data[["equity_market"]] == "GlobalMarket") |>
    dplyr::filter(.data[["scenario_geography"]] == "Global") |>
    dplyr::filter(
      .data[["year"]] %in% c(
        .env[["start_year"]],
        .env[["start_year"]] + 5L
      )
    ) |>
    dplyr::mutate(
      plan_buildout = dplyr::last(
        .data[["plan_tech_prod"]],
        order_by = .data[["year"]]
      ) - dplyr::first(
        .data[["plan_tech_prod"]],
        order_by = .data[["year"]]
      ),
      scen_buildout = (
        dplyr::last(
          .data[["scen_tech_prod"]],
          order_by = .data[["year"]]
        ) - dplyr::first(
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
    dplyr::filter(.data[["year"]] == .env[["start_year"]]) |>
    dplyr::mutate(green = .data[["technology"]] %in% .env[["green_techs"]]) |>
    dplyr::reframe(
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
    dplyr::mutate(y = .data[["plan_buildout"]] / .data[["scen_buildout"]]) |>
    dplyr::filter(.data[["green"]]) |>
    dplyr::select(-"plan_buildout", -"scen_buildout", -"green") |>
    dplyr::filter(!is.na(.data[["plan_tech_share"]])) |>
    dplyr::mutate(y = pmax(.data[["y"]], 0L, na.rm = TRUE)) |>
    dplyr::mutate(asset_class = "Corporate Bonds")

  dplyr::bind_rows(equity_data, bonds_data)
}
