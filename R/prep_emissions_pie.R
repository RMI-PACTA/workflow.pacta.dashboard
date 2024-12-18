prep_emissions_pie <- function(
  data,
  asset_type,
  investor_name,
  portfolio_name,
  pacta_sectors
) {
  data |>
    ungroup() |>
    dplyr::filter(
      .data[["investor_name"]] == .env[["investor_name"]],
      .data[["portfolio_name"]] == .env[["portfolio_name"]]
    ) |>
    dplyr::filter(.data[["asset_type"]] %in% c("Bonds", "Equity")) |>
    select("asset_type", "sector", "weighted_sector_emissions") |>
    mutate(exploded = .data[["sector"]] %in% .env[["pacta_sectors"]]) |>
    arrange(
      .data[["asset_type"]],
      desc(.data[["exploded"]]),
      .data[["sector"]]
    ) |>
    rename(
      key = .data[["sector"]],
      value = .data[["weighted_sector_emissions"]]
    ) |>
    dplyr::filter(!is.na(.data[["key"]])) |>
    dplyr::filter(.data[["asset_type"]] == .env[["asset_type"]]) |>
    select(-"asset_type")
}
