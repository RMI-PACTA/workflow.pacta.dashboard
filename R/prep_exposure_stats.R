prep_exposure_stats <- function(
  audit_file,
  investor_name,
  portfolio_name,
  pacta_sectors,
  currency_exchange_value
) {
  pacta_asset_classes <- c(
    "Bonds",
    "Equity"
  )

  audit_table <- pacta.portfolio.report:::prep_audit_table(
    audit_file,
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    currency_exchange_value = currency_exchange_value
  )

  exposure_stats <- audit_file %>%
    dplyr::filter(
      .data$investor_name == .env$investor_name &
        .data$portfolio_name == .env$portfolio_name
    ) %>%
    dplyr::filter(.data$asset_type %in% pacta_asset_classes) %>%
    dplyr::filter(.data$valid_input == TRUE) %>%
    mutate(across(c("bics_sector", "financial_sector"), as.character)) %>%
    mutate(
      sector =
        if_else(!.data$financial_sector %in% .env$pacta_sectors,
          "Other",
          .data$financial_sector
        )
    ) %>%
    summarise(
      value = sum(.data$value_usd, na.rm = TRUE) / .env$currency_exchange_value,
      .by = c("asset_type", "sector")
    ) %>%
    mutate(
      perc_asset_val_sector = .data$value / sum(.data$value, na.rm = TRUE),
      .by = c("asset_type")
    ) %>%
    inner_join(
      audit_table,
      by = join_by("asset_type" == "asset_type_analysis")
    ) %>%
    select(
      "asset_type",
      "percentage_value_invested",
      "sector",
      "perc_asset_val_sector"
    )

  asset_classes_in_portfolio <- intersect(
    pacta_asset_classes,
    unique(exposure_stats$asset_type)
  )

  all_stats_with_zero_sector_exposure <- expand.grid(
    asset_type = asset_classes_in_portfolio,
    sector = pacta_sectors,
    val_sector = 0
  ) %>%
    inner_join(
      distinct(
        select(
          exposure_stats,
          c("asset_type", "percentage_value_invested")
        )
      ),
      by = join_by("asset_type" = "asset_type")
    )

  exposure_stats_all <- all_stats_with_zero_sector_exposure %>%
    left_join(
      exposure_stats,
      by = join_by(
        "asset_type" = "asset_type",
        "sector" = "sector",
        "percentage_value_invested" = "percentage_value_invested"
      )
    ) %>%
    mutate(
      perc_asset_val_sector = if_else(
        is.na(.data$perc_asset_val_sector),
        .data$val_sector,
        .data$perc_asset_val_sector
      )
    ) %>%
    mutate(
      asset_type = case_when(
        .data$asset_type == "Bonds" ~ "Corporate Bonds",
        .data$asset_type == "Equity" ~ "Listed Equity"
      )
    ) %>%
    select(
      "asset_type",
      "percentage_value_invested",
      "sector",
      "perc_asset_val_sector"
    )

  exposure_stats_all
}
