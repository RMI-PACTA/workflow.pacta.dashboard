prep_exposure_stats <- function(
  audit_file,
  investor_name,
  portfolio_name,
  pacta_sectors,
  currency_exchange_value
) {
  pacta_asset_classes <- c("Bonds", "Equity")

  audit_table <- prep_audit_table(
    audit_file,
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    currency_exchange_value = currency_exchange_value
  )

  exposure_stats <- audit_file |>
    dplyr::filter(
      .data[["investor_name"]] == .env[["investor_name"]],
      .data[["portfolio_name"]] == .env[["portfolio_name"]]
    ) |>
    dplyr::filter(.data[["asset_type"]] %in% pacta_asset_classes) |>
    dplyr::filter(.data[["valid_input"]]) |>
    dplyr::mutate(dplyr::across(c("bics_sector", "financial_sector"), as.character)) |>
    dplyr::mutate(
      sector = dplyr::if_else(
        .data[["financial_sector"]] %in% .env[["pacta_sectors"]],
        .data[["financial_sector"]],
        "Other"
      )
    ) |>
    dplyr::summarise(
      value = (
        sum(
          .data[["value_usd"]],
          na.rm = TRUE
        ) / (
          .env[["currency_exchange_value"]]
        )
      ),
      .by = c("asset_type", "sector")
    ) |>
    dplyr::mutate(
      perc_asset_val_sector = (
        .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)
      ),
      .by = "asset_type"
    ) |>
    dplyr::inner_join(
      audit_table,
      by = dplyr::join_by("asset_type" == "asset_type_analysis")
    ) |>
    select(
      "asset_type",
      "percentage_value_invested",
      "sector",
      "perc_asset_val_sector"
    )

  asset_classes_in_portfolio <- intersect(
    pacta_asset_classes,
    unique(exposure_stats[["asset_type"]])
  )

  all_stats_zero_sector_exposure <- expand.grid(
    asset_type = asset_classes_in_portfolio,
    sector = pacta_sectors,
    val_sector = 0L
  ) |> dplyr::inner_join(
    dplyr::distinct(
      select(
        exposure_stats,
        c("asset_type", "percentage_value_invested")
      )
    ),
    by = dplyr::join_by("asset_type")
  )

  exposure_stats_all <- all_stats_zero_sector_exposure |>
    dplyr::left_join(
      exposure_stats,
      by = dplyr::join_by("asset_type", "sector", "percentage_value_invested")
    ) |>
    dplyr::mutate(
      perc_asset_val_sector = dplyr::if_else(
        is.na(.data[["perc_asset_val_sector"]]),
        .data[["val_sector"]],
        .data[["perc_asset_val_sector"]]
      )
    ) |>
    dplyr::mutate(
      asset_type = dplyr::case_when(
        .data[["asset_type"]] == "Bonds" ~ "Corporate Bonds",
        .data[["asset_type"]] == "Equity" ~ "Listed Equity"
      )
    ) |>
    select(
      "asset_type",
      "percentage_value_invested",
      "sector",
      "perc_asset_val_sector"
    )

  exposure_stats_all
}
