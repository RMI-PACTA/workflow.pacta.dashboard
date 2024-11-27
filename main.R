library(dplyr)
library(jsonlite)
library(pacta.portfolio.report)
library(pacta.portfolio.utils)
library(readr)
library(tidyr)

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

prep_exposure_stats <- function(audit_file, investor_name, portfolio_name, pacta_sectors) {
  pacta_asset_classes <- c("Bonds", "Equity")
  
  audit_table <- pacta.portfolio.report:::prep_audit_table(
      audit_file,
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      currency_exchange_value = currency_exchange_value
    )
  
  exposure_stats <- audit_file %>%
    filter(
      .data$investor_name == .env$investor_name &
      .data$portfolio_name == .env$portfolio_name) %>%
    filter(.data$asset_type %in% pacta_asset_classes) %>%
    filter(.data$valid_input == TRUE) %>%
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
    inner_join(audit_table, by = join_by(asset_type == asset_type_analysis)) %>%
    select("asset_type", "percentage_value_invested", "sector", "perc_asset_val_sector")
  
  asset_classes_in_portfolio <- intersect(pacta_asset_classes, unique(exposure_stats$asset_type))
  
  all_stats_with_zero_sector_exposure <- expand.grid(
    asset_type = asset_classes_in_portfolio,
    sector = pacta_sectors,
    val_sector = 0
    ) %>% inner_join(
      distinct(select(exposure_stats, c("asset_type", "percentage_value_invested"))),
      by = join_by(asset_type)
    )
  
  exposure_stats_all <- all_stats_with_zero_sector_exposure %>%
    left_join(exposure_stats, by = join_by(asset_type, sector, percentage_value_invested)) %>%
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
    select("asset_type", "percentage_value_invested", "sector", "perc_asset_val_sector")
  
  exposure_stats_all
}


# prep_company_bubble ----------------------------------------------------------
# based on pacta.portfolio.report:::prep_company_bubble, but does not filter to
# allocation == "portfolio_weight" nor by scenario and scenario source

prep_company_bubble <-
  function(equity_results_company,
           bonds_results_company,
           portfolio_name,
           start_year,
           green_techs) {
    
    equity_data <-
      equity_results_company %>%
      filter(.data$portfolio_name == .env$portfolio_name) %>%
      filter(.data$ald_sector %in% c("Power", "Automotive")) %>%
      filter(.data$equity_market == "GlobalMarket") %>%
      filter(.data$scenario_geography == "Global") %>%
      filter(.data$year %in% c(.env$start_year, .env$start_year + 5)) %>%
      mutate(
        plan_buildout = last(.data$plan_tech_prod, order_by = .data$year) - first(.data$plan_tech_prod, order_by = .data$year),
        scen_buildout = last(.data$scen_tech_prod, order_by = .data$year) - first(.data$scen_tech_prod, order_by = .data$year),
        .by = c("company_name", "technology", "scenario_source", "scenario", "allocation")
      ) %>%
      filter(.data$year == .env$start_year) %>%
      mutate(green = .data$technology %in% .env$green_techs) %>%
      reframe(
        plan_tech_share = sum(.data$plan_tech_share, na.rm = TRUE),
        plan_buildout = sum(.data$plan_buildout, na.rm = TRUE),
        scen_buildout = sum(.data$scen_buildout, na.rm = TRUE),
        plan_carsten = sum(.data$plan_carsten, na.rm = TRUE),
        port_weight = unique(.data$port_weight),
        .by = c("company_name", "allocation", "scenario_source",
                "scenario", "ald_sector", "green", "year")
      ) %>%
      mutate(y = .data$plan_buildout / .data$scen_buildout) %>%
      filter(.data$green) %>%
      select(-"plan_buildout", -"scen_buildout", -"green") %>%
      filter(!is.na(.data$plan_tech_share)) %>%
      mutate(y = pmax(.data$y, 0, na.rm = TRUE)) %>%
      mutate(asset_class = "Listed Equity")
    
    bonds_data <-
      bonds_results_company %>%
      filter(.data$portfolio_name == .env$portfolio_name) %>%
      filter(.data$ald_sector %in% c("Power", "Automotive")) %>%
      filter(.data$equity_market == "GlobalMarket") %>%
      filter(.data$scenario_geography == "Global") %>%
      filter(.data$year %in% c(.env$start_year, .env$start_year + 5)) %>%
      mutate(
        plan_buildout = last(.data$plan_tech_prod, order_by = .data$year) - first(.data$plan_tech_prod, order_by = .data$year),
        scen_buildout = last(.data$scen_tech_prod, order_by = .data$year) - first(.data$scen_tech_prod, order_by = .data$year),
        .by = c("company_name", "technology", "scenario_source", "scenario", "allocation")
      ) %>%
      filter(.data$year == .env$start_year) %>%
      mutate(green = .data$technology %in% .env$green_techs) %>%
      reframe(
        plan_tech_share = sum(.data$plan_tech_share, na.rm = TRUE),
        plan_buildout = sum(.data$plan_buildout, na.rm = TRUE),
        scen_buildout = sum(.data$scen_buildout, na.rm = TRUE),
        plan_carsten = sum(.data$plan_carsten, na.rm = TRUE),
        port_weight = unique(.data$port_weight),
        .by = c("company_name", "allocation", "scenario_source",
                "scenario", "ald_sector", "green", "year")
      ) %>%
      mutate(y = .data$plan_buildout / .data$scen_buildout) %>%
      filter(.data$green) %>%
      select(-"plan_buildout", -"scen_buildout", -"green") %>%
      filter(!is.na(.data$plan_tech_share)) %>%
      mutate(y = pmax(.data$y, 0, na.rm = TRUE)) %>%
      mutate(asset_class = "Corporate Bonds")
    
    bind_rows(equity_data, bonds_data)
  }


# prep_key_bars_company --------------------------------------------------------
# based on pacta.portfolio.report:::prep_key_bars_company, but does not filter 
# to allocation == "portfolio_weight"  nor by scenario and scenario source

prep_key_bars_company <-
  function(equity_results_company,
           bonds_results_company,
           portfolio_name,
           start_year,
           pacta_sectors_not_analysed,
           all_tech_levels) {
    
    equity_data_company <-
      equity_results_company %>%
      filter(.data$portfolio_name == .env$portfolio_name) %>%
      filter(.data$year %in% c(.env$start_year + 5)) %>%
      filter(.data$equity_market %in% c("Global", "GlobalMarket")) %>%
      filter(.data$scenario_geography == "Global") %>%
      filter(.data$ald_sector %in% c("Power", "Automotive")) %>%
      select(-"id") %>%
      rename(id = "company_name") %>%
      select("id", "ald_sector", "technology", "plan_tech_share", "port_weight",
             "allocation", "scenario_source", "scenario", "year") %>%
      arrange(desc(.data$port_weight)) %>%
      mutate(asset_class = "Listed Equity") %>%
      mutate_at("id", as.character) %>% # convert the col type to character to prevent errors in case empty df is binded by rows
      group_by(.data$ald_sector, .data$technology) %>% # select at most 15 companies with the highest weigths per sector+technology
      arrange(dplyr::desc(.data$port_weight), .by_group = TRUE) %>%
      slice(1:15)  %>%
      filter(!is.null(.data$port_weight)) %>%
      filter(!is.null(.data$plan_tech_share))
    
    bonds_data_company <-
      bonds_results_company %>%
      filter(.data$portfolio_name == .env$portfolio_name) %>%
      filter(.data$year %in% c(.env$start_year + 5)) %>%
      filter(.data$equity_market %in% c("Global", "GlobalMarket")) %>%
      filter(.data$scenario_geography == "Global") %>%
      filter(.data$ald_sector %in% c("Power", "Automotive")) %>%
      select(-"id") %>%
      rename(id = "company_name") %>%
      select("id", "ald_sector", "technology", "plan_tech_share", "port_weight",
             "allocation", "scenario_source", "scenario", "year") %>%
      group_by(.data$id, .data$ald_sector, .data$technology) %>%
      mutate(port_weight = sum(.data$port_weight, na.rm = TRUE)) %>%
      group_by(.data$id, .data$technology) %>%
      filter(row_number() == 1) %>%
      filter(!.data$ald_sector %in% .env$pacta_sectors_not_analysed | !grepl("Aligned", .data$id)) %>%
      arrange(desc(.data$port_weight)) %>%
      mutate(asset_class = "Corporate Bonds") %>%
      mutate_at("id", as.character) %>% # convert the col type to character to prevent errors in case empty df is bound by rows
      group_by(.data$ald_sector, .data$technology) %>% # select at most 15 companies with the highest weigths per sector+technology
      arrange(.data$port_weight, .by_group = TRUE) %>%
      slice(1:15) %>%
      group_by(.data$ald_sector) %>%
      arrange(factor(.data$technology, levels = .env$all_tech_levels)) %>%
      arrange(dplyr::desc(.data$port_weight), .by_group = TRUE) %>%
      filter(!is.null(.data$port_weight)) %>%
      filter(!is.null(.data$plan_tech_share))
    
    bind_rows(equity_data_company, bonds_data_company) %>%
      mutate(scenario = sub("_", " ", .data$scenario))
  }


# prep_key_bars_portfolio ------------------------------------------------------
# based on pacta.portfolio.report:::prep_key_bars_portfolio, but does not filter 
# to allocation == "portfolio_weight" nor by scenario and scenario source

prep_key_bars_portfolio <-
  function(equity_results_portfolio,
           bonds_results_portfolio,
           portfolio_name,
           start_year,
           pacta_sectors_not_analysed,
           all_tech_levels) {
    equity_data_portfolio <-
      equity_results_portfolio %>%
      filter(.data$portfolio_name == .env$portfolio_name) %>%
      filter(.data$equity_market %in% c("Global", "GlobalMarket")) %>%
      filter(.data$year %in% c(.env$start_year + 5)) %>%
      filter(.data$ald_sector %in% c("Power", "Automotive")) %>%
      filter(.data$scenario_geography == "Global") %>%
      mutate(port_weight = 1) %>%
      select("ald_sector", "technology", "plan_tech_share", "scen_tech_share",
             "port_weight", "scenario", "scenario_source", "allocation", "year") %>%
      pivot_longer(c("plan_tech_share", "scen_tech_share"), names_to = "plan") %>%
      mutate(id = if_else(.data$plan == "plan_tech_share", "Portfolio", "Aligned* Portfolio")) %>%
      rename(plan_tech_share = "value") %>%
      select("id", "ald_sector", "technology", "plan_tech_share", "port_weight",
             "scenario", "scenario_source", "allocation", "year") %>%
      filter(!.data$ald_sector %in% .env$pacta_sectors_not_analysed | !grepl("Aligned", .data$id)) %>%
      mutate(asset_class = "Listed Equity") %>%
      mutate_at("id", as.character) # convert the col type to character to prevent errors in case empty df is bound by rows
    
    bonds_data_portfolio <-
      bonds_results_portfolio %>%
      filter(.data$portfolio_name == .env$portfolio_name) %>%
      filter(.data$equity_market %in% c("Global", "GlobalMarket")) %>%
      filter(.data$year %in% c(.env$start_year + 5)) %>%
      filter(.data$ald_sector %in% c("Power", "Automotive")) %>%
      filter(.data$scenario_geography == "Global") %>%
      mutate(port_weight = 1) %>%
      select("ald_sector", "technology", "plan_tech_share", "scen_tech_share",
             "port_weight", "scenario", "scenario_source", "allocation", "year") %>%
      pivot_longer(c("plan_tech_share", "scen_tech_share"), names_to = "plan") %>%
      mutate(id = if_else(.data$plan == "plan_tech_share", "Portfolio", "Aligned* Portfolio")) %>%
      rename(plan_tech_share = "value") %>%
      select("id", "ald_sector", "technology", "plan_tech_share", "port_weight",
             "scenario", "scenario_source", "allocation", "year") %>%
      mutate(asset_class = "Corporate Bonds") %>%
      mutate_at("id", as.character) %>%
      arrange(factor(.data$technology, levels = .env$all_tech_levels))
    
    bind_rows(equity_data_portfolio, bonds_data_portfolio) %>%
      mutate(scenario = sub("_", " ", .data$scenario))
  }


# input and output directories -------------------------------------------------

input_dir <- "./inputs"
output_dir <- "./outputs"
data_dir <- "./data"


# portfolio/user parameters ----------------------------------------------------

investor_name <- "investor_name"
portfolio_name <- "portfolio_name"
peer_group <- "peer_group"
language_select <- "EN"

currency_exchange_value <- 1
display_currency <- "USD"

select_scenario_other <- "WEO2023_NZE_2050"
select_scenario <- "WEO2023_NZE_2050"

green_techs <- c("RenewablesCap", "HydroCap", "NuclearCap", "Hybrid", "Electric", "FuelCell", "Hybrid_HDV", "Electric_HDV", "FuelCell_HDV","Electric Arc Furnace")
tech_roadmap_sectors <- c("Automotive", "Power", "Oil&Gas", "Coal")
pacta_sectors_not_analysed <- c("Steel", "Aviation", "Cement")

power_tech_levels = c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "OilCap", "CoalCap")
oil_gas_levels = c("Oil", "Gas")
coal_levels = c("Coal")
auto_levels = c("Electric", "Electric_HDV", "FuelCell","FuelCell_HDV", "Hybrid","Hybrid_HDV", "ICE", "ICE_HDV")
cement_levels = c("Integrated facility", "Grinding")
steel_levels = c("Electric Arc Furnace", "Open Hearth Furnace", "Basic Oxygen Furnace")
aviation_levels = c("Freight", "Passenger", "Mix", "Other")
all_tech_levels = c(power_tech_levels, auto_levels, oil_gas_levels, coal_levels, cement_levels, steel_levels, aviation_levels)


# config parameters from manifest ----------------------------------------------

manifest <- jsonlite::read_json(path = file.path(input_dir, "manifest.json"))

start_year <- manifest$params$analysis$startYear
year_span <- manifest$params$analysis$timeHorizon
pacta_sectors <- unlist(manifest$params$analysis$sectorList)
equity_market_levels <- unlist(manifest$params$analysis$equityMarketList)
scen_geo_levels <- unlist(manifest$params$analysis$scenarioGeographiesList)


# load results from input directory --------------------------------------------

audit_file <- readRDS(file.path(input_dir, "audit_file.rds"))
emissions <- readRDS(file.path(input_dir, "emissions.rds"))
equity_results_portfolio <- readRDS(file.path(input_dir, "Equity_results_portfolio.rds"))
bonds_results_portfolio <- readRDS(file.path(input_dir, "Bonds_results_portfolio.rds"))
equity_results_company <- readRDS(file.path(input_dir, "Equity_results_company.rds"))
bonds_results_company <- readRDS(file.path(input_dir, "Bonds_results_company.rds"))


# data from PACTA inputs used to generate the results --------------------------

indices_bonds_results_portfolio <- readRDS(file.path(data_dir, "Indices_bonds_results_portfolio.rds"))
indices_equity_results_portfolio <- readRDS(file.path(data_dir, "Indices_equity_results_portfolio.rds"))
peers_bonds_results_portfolio <- pacta.portfolio.utils::empty_portfolio_results()
peers_equity_results_portfolio <- pacta.portfolio.utils::empty_portfolio_results()


# translations -----------------------------------------------------------------

dataframe_translations <- readr::read_csv(
  system.file("extdata/translation/dataframe_labels.csv", package = "pacta.portfolio.report"),
  col_types = readr::cols()
)

header_dictionary <- readr::read_csv(
  system.file("extdata/translation/dataframe_headers.csv", package = "pacta.portfolio.report"),
  col_types = readr::cols()
)

js_translations <- jsonlite::fromJSON(
  txt = system.file("extdata/translation/js_labels.json", package = "pacta.portfolio.report")
)

sector_order <- readr::read_csv(
  system.file("extdata/sector_order/sector_order.csv", package = "pacta.portfolio.report"),
  col_types = readr::cols()
)

dictionary <-
  pacta.portfolio.report:::choose_dictionary_language(
    data = dataframe_translations,
    language = language_select
  )

header_dictionary <- pacta.portfolio.report:::replace_contents(header_dictionary, display_currency)


# add investor_name and portfolio_name to results data frames because ----------
# pacta.portfolio.report functions expect that ---------------------------------

audit_file <-
  audit_file %>%
  mutate(
    investor_name = investor_name,
    portfolio_name = portfolio_name
  )

emissions <-
  emissions %>%
  mutate(
    investor_name = investor_name,
    portfolio_name = portfolio_name
  )

equity_results_portfolio <-
  equity_results_portfolio %>%
  mutate(
    investor_name = investor_name,
    portfolio_name = portfolio_name
  )

bonds_results_portfolio <-
  bonds_results_portfolio %>%
  mutate(
    investor_name = investor_name,
    portfolio_name = portfolio_name
  )

equity_results_company <-
  equity_results_company %>%
  mutate(
    investor_name = investor_name,
    portfolio_name = portfolio_name
  )

bonds_results_company <-
  bonds_results_company %>%
  mutate(
    investor_name = investor_name,
    portfolio_name = portfolio_name
  )


# data_included_table.json -----------------------------------------------------

audit_file %>%
  pacta.portfolio.report:::prep_audit_table(
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    currency_exchange_value = currency_exchange_value
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_included_table", dictionary, inplace = TRUE) %>%
  pacta.portfolio.report:::translate_df_headers("data_included_table", language_select, header_dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_included_table.json"))


# data_value_pie_bonds.json ----------------------------------------------------

audit_file %>%
  pacta.portfolio.report:::prep_exposure_pie(
    asset_type = "Bonds",
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors,
    currency_exchange_value = currency_exchange_value
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_value_pie_bonds", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_value_pie_bonds.json"))


# data_emissions_equity.json ---------------------------------------------------

emissions %>%
  pacta.portfolio.report:::prep_emissions_pie(
    asset_type = "Equity",
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_emissions_pie_equity", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_emissions_pie_equity.json"))


# data_emissions_bonds.json ----------------------------------------------------

emissions %>%
  pacta.portfolio.report:::prep_emissions_pie(
    asset_type = "Bonds",
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_emissions_pie_bonds", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_emissions_pie_bonds.json"))


# data_value_pie_equity.json ---------------------------------------------------

audit_file %>%
  pacta.portfolio.report:::prep_exposure_pie(
    asset_type = "Equity",
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors,
    currency_exchange_value = currency_exchange_value
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_value_pie_equity", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_value_pie_equity.json"))


# data_techmix.json ------------------------------------------------------------

pacta.portfolio.report:::prep_techexposure(
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  indices_equity_results_portfolio = indices_equity_results_portfolio,
  indices_bonds_results_portfolio = indices_bonds_results_portfolio,
  peers_equity_results_portfolio = peers_equity_results_portfolio,
  peers_bonds_results_portfolio = peers_bonds_results_portfolio,
  peer_group = peer_group,
  select_scenario_other = select_scenario_other,
  select_scenario = select_scenario,
  start_year = start_year,
  green_techs = green_techs,
  equity_market_levels = equity_market_levels,
  all_tech_levels = all_tech_levels
  ) %>%
  pacta.portfolio.report:::translate_df_contents("techexposure_data", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_techexposure.json"))


# data_techmix_sector.json -----------------------------------------------------

prep_techmix_sector(
  equity_results_portfolio,
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
  ) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_techmix_sector.json"))

# data_trajectory_alignment.json -----------------------------------------------

pacta.portfolio.report:::prep_trajectory_alignment(
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  peers_equity_results_portfolio = peers_equity_results_portfolio,
  peers_bonds_results_portfolio = peers_bonds_results_portfolio,
  indices_equity_results_portfolio = indices_equity_results_portfolio,
  indices_bonds_results_portfolio = indices_bonds_results_portfolio,
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  tech_roadmap_sectors = tech_roadmap_sectors,
  peer_group = peer_group,
  start_year = start_year,
  year_span = year_span,
  scen_geo_levels = scen_geo_levels,
  all_tech_levels = all_tech_levels
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_trajectory_alignment", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_trajectory_alignment.json"))


# data_emissions.json ----------------------------------------------------------

pacta.portfolio.report:::prep_emissions_trajectory(
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  select_scenario_other = select_scenario_other,
  select_scenario = select_scenario,
  pacta_sectors = pacta_sectors,
  year_span = year_span,
  start_year = start_year
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_emissions", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_emissions.json"))

# data_exposure_stats.json

prep_exposure_stats(
  audit_file = audit_file,
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  pacta_sectors = pacta_sectors
  ) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_exposure_stats.json"))


# data_company_bubble.json -----------------------------------------------------

prep_company_bubble(
  equity_results_company = equity_results_company,
  bonds_results_company = bonds_results_company,
  portfolio_name = portfolio_name,
  start_year = start_year,
  green_techs = green_techs
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_company_bubble", dictionary) %>% 
  jsonlite::write_json(path = file.path(output_dir, "data_company_bubble.json"))


# data_techexposure_company_companies.json -------------------------------------

prep_key_bars_company(
  equity_results_company = equity_results_company,
  bonds_results_company = bonds_results_company,
  portfolio_name = portfolio_name,
  start_year = start_year,
  pacta_sectors_not_analysed = pacta_sectors_not_analysed,
  all_tech_levels = all_tech_levels
  ) %>%
  pacta.portfolio.report:::translate_df_contents("data_key_bars_company", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_techexposure_company_companies.json"))


# data_techexposure_company_portfolio.json -------------------------------------

prep_key_bars_portfolio(
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  portfolio_name = portfolio_name,
  start_year = start_year,
  pacta_sectors_not_analysed = pacta_sectors_not_analysed,
  all_tech_levels = all_tech_levels
  ) %>% 
  pacta.portfolio.report:::translate_df_contents("data_key_bars_portfolio", dictionary) %>%
  jsonlite::write_json(path = file.path(output_dir, "data_techexposure_company_portfolio.json"))
