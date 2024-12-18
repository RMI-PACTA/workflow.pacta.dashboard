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
    filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    filter(.data[["year"]] %in% c(.env[["start_year"]] + 5L)) |>
    filter(.data[["equity_market"]] %in% c("Global", "GlobalMarket")) |>
    filter(.data[["scenario_geography"]] == "Global") |>
    filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    select(-"id") |>
    rename(id = "company_name") |>
    select(
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
    arrange(desc(.data[["port_weight"]])) |>
    mutate(asset_class = "Listed Equity") |>
    mutate_at("id", as.character) |> # convert the col type to character to prevent errors in case empty df is binded by rows #nolint
    group_by(.data[["ald_sector"]], .data[["technology"]]) |> # select at most 15 companies with the highest weigths per sector+technology #nolint
    arrange(dplyr::desc(.data[["port_weight"]]), .by_group = TRUE) |>
    slice(1L:15L)  |>
    filter(!is.null(.data[["port_weight"]])) |>
    filter(!is.null(.data[["plan_tech_share"]]))

  bonds_data_company <-
    bonds_results_company |>
    filter(.data[["portfolio_name"]] == .env[["portfolio_name"]]) |>
    filter(.data[["year"]] %in% c(.env[["start_year"]] + 5L)) |>
    filter(.data[["equity_market"]] %in% c("Global", "GlobalMarket")) |>
    filter(.data[["scenario_geography"]] == "Global") |>
    filter(.data[["ald_sector"]] %in% c("Power", "Automotive")) |>
    select(-"id") |>
    rename(id = "company_name") |>
    select(
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
    group_by(.data[["id"]], .data[["ald_sector"]], .data[["technology"]]) |>
    mutate(port_weight = sum(.data[["port_weight"]], na.rm = TRUE)) |>
    group_by(.data[["id"]], .data[["technology"]]) |>
    filter(row_number() == 1L) |>
    filter(
      !(.data[["ald_sector"]] %in% .env[["pacta_sectors_not_analysed"]]) |
        !grepl(
          pattern = "Aligned",
          x = .data[["id"]],
          fixed = TRUE
        )
    ) |>
    arrange(desc(.data[["port_weight"]])) |>
    mutate(asset_class = "Corporate Bonds") |>
    mutate_at("id", as.character) |> # convert the col type to character to prevent errors in case empty df is bound by rows #nolint
    group_by(.data[["ald_sector"]], .data[["technology"]]) |> # select at most 15 companies with the highest weigths per sector+technology #nolint
    arrange(.data[["port_weight"]], .by_group = TRUE) |>
    slice(1L:15L) |>
    group_by(.data[["ald_sector"]]) |>
    arrange(
      factor(
        .data[["technology"]],
        levels = .env[["all_tech_levels"]]
      )
    ) |>
    arrange(dplyr::desc(.data[["port_weight"]]), .by_group = TRUE) |>
    filter(!is.null(.data[["port_weight"]])) |>
    filter(!is.null(.data[["plan_tech_share"]]))

  bind_rows(equity_data_company, bonds_data_company)
}
