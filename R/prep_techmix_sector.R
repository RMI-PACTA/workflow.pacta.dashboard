prep_techmix_sector <-
  function(equity_results_portfolio,
           bonds_results_portfolio,
           indices_equity_results_portfolio,
           indices_bonds_results_portfolio,
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
      list(`Listed Equity` = equity_results_portfolio,
           `Corporate Bonds` = bonds_results_portfolio) %>%
      bind_rows(.id = "asset_class") %>%
      filter(.data$investor_name == .env$investor_name,
             .data$portfolio_name == .env$portfolio_name) %>%
      filter(!is.na(.data$ald_sector))

asset_classes <-
      portfolio %>%
      pull("asset_class") %>%
      unique()

equity_sectors <-
      portfolio %>%
      filter(.data$asset_class == "Listed Equity") %>%
      filter(.data$allocation == "portfolio_weight") %>%
      pull("ald_sector") %>%
      unique()

bonds_sectors <-
      portfolio %>%
      filter(.data$asset_class == "Corporate Bonds") %>%
      pull("ald_sector") %>%
      unique()

indices <-
      list(`Listed Equity` = indices_equity_results_portfolio,
           `Corporate Bonds` = indices_bonds_results_portfolio) %>%
      bind_rows(.id = "asset_class") %>%
      filter(.data$asset_class %in% .env$asset_classes) %>%
      filter(.data$asset_class == "Listed Equity" & .data$ald_sector %in% .env$equity_sectors |
               .data$asset_class == "Corporate Bonds" & .data$ald_sector %in% .env$bonds_sectors)

peers <-
      list(`Listed Equity` = peers_equity_results_portfolio,
           `Corporate Bonds` = peers_bonds_results_portfolio) %>%
      bind_rows(.id = "asset_class") %>%
      filter(.data$asset_class %in% .env$asset_classes) %>%
      filter(.data$asset_class == "Listed Equity" & .data$ald_sector %in% .env$equity_sectors |
               .data$asset_class == "Corporate Bonds" & .data$ald_sector %in% .env$bonds_sectors) %>%
      filter(.data$investor_name == .env$peer_group)

techexposure_data <-
      bind_rows(portfolio, peers, indices) %>%
      filter(.data$allocation == "portfolio_weight") %>%
      filter(.data$scenario_geography == "Global") %>%
      filter(.data$year %in% c(.env$start_year, .env$start_year + .env$year_span))

if (nrow(techexposure_data) > 0) {
  techexposure_data <-
        techexposure_data %>%
        mutate(green = .data$technology %in% .env$green_techs) %>%
        group_by(.data$asset_class, .data$equity_market, .data$portfolio_name,
                 .data$ald_sector, .data$scenario, .data$year) %>%
        mutate(plan_alloc_wt_sec_prod = sum(.data$plan_alloc_wt_tech_prod),
               scen_alloc_wt_sec_prod = sum(.data$scen_alloc_wt_tech_prod)) %>%
        mutate(production_plan = if_else(.data$plan_alloc_wt_tech_prod > 0, .data$plan_alloc_wt_tech_prod / .data$plan_alloc_wt_sec_prod, 0),
               scenario_plan = if_else(.data$scen_alloc_wt_tech_prod > 0, .data$scen_alloc_wt_tech_prod / .data$scen_alloc_wt_sec_prod, 0)) %>%
        group_by(.data$asset_class, .data$equity_market, .data$portfolio_name,
                 .data$ald_sector, .data$scenario, .data$year, .data$green) %>%
        mutate(green_sum_prod = sum(.data$production_plan),
              green_sum_scenario = sum(.data$scenario_plan)) %>%
        ungroup() %>%
        select("asset_class", "investor_name", "portfolio_name", "scenario_source",
               "scenario", "allocation", "equity_market", "year", "ald_sector",
               "technology", "production_plan", "scenario_plan", "green",
               "green_sum_prod", "green_sum_scenario") %>%
        pivot_longer(
          cols = -c("asset_class", "investor_name", "portfolio_name", "scenario_source",
                    "scenario", "allocation", "equity_market", "year", "ald_sector",
                    "technology", "green","green_sum_prod", "green_sum_scenario"),
          names_to = "val_type", values_to = "value") %>%
        mutate(
          green_sum = if_else(.data$val_type == "production_plan", .data$green_sum_prod, .data$green_sum_scenario)
        ) %>%
        select(-c("green_sum_prod", "green_sum_scenario")) %>%
        ungroup() %>%
        mutate(this_portfolio = .data$portfolio_name == .env$portfolio_name,
               val_type = if_else(.data$this_portfolio == TRUE, paste0(.data$val_type, "_portfolio"), paste0(.data$val_type, "_benchmark"))) %>%
        mutate(equity_market =  case_when(
          .data$equity_market == "GlobalMarket" ~ "Global Market",
          .data$equity_market == "DevelopedMarket" ~ "Developed Market",
          .data$equity_market == "EmergingMarket" ~ "Emerging Market",
          TRUE ~ .data$equity_market)
        ) %>%
    # no need for showing scenario mix for the benchmark
    filter(
      .data$val_type != "scenario_plan_benchmark"
    ) %>%
    mutate(val_type =  case_when(
          .data$val_type == "production_plan_portfolio" ~ "Portfolio",
          .data$val_type == "scenario_plan_portfolio" ~ "Scenario",
          .data$val_type == "production_plan_benchmark" ~ "Benchmark",
          TRUE ~ .data$val_type)
        ) %>%
    arrange(
          .data$asset_class,
          factor(.data$equity_market, levels = c("Global Market", "Developed Market", "Emerging Market")),
          desc(.data$this_portfolio),
          factor(.data$val_type, levels = c("Portfolio", "Scenario", "Benchmark")),
          .data$portfolio_name,
          factor(.data$technology, levels = .env$all_tech_levels)
        ) %>%
    select("asset_class", "equity_market", "portfolio_name", "scenario", "scenario_source",
               "this_portfolio", "val_type", "ald_sector", "technology", "value",
               "green", "green_sum", "year") %>%
    filter(
      !(.data$year == .env$start_year & .data$val_type == "Scenario")
    )
}

techexposure_data
  }

