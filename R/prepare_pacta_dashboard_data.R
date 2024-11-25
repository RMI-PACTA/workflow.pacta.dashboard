prepare_pacta_dashboard_data <- function(
  params,
  analysis_output_dir = Sys.getenv("ANALYSIS_OUTPUT_DIR"),
  dashboard_data_dir = Sys.getenv("DASHBOARD_DATA_DIR"),
  benchmarks_dir = Sys.getenv("BENCHMARKS_DIR")
) {
  log_debug("Checking configuration.")
  if (is.null(analysis_output_dir) || analysis_output_dir == "") {
    log_error("ANALYSIS_OUTPUTS_DIR not set.")
    stop("ANALYSIS_OUTPUTS_DIR not set.")
  }
  if (is.null(benchmarks_dir) || benchmarks_dir == "") {
    log_error("BENCHMARKS_DIR not set.")
    stop("BENCHMARKS_DIR not set.")
  }
  if (is.null(dashboard_data_dir) || dashboard_data_dir == "") {
    log_error("DASHBOARD_DATA_DIR not set.")
    stop("DASHBOARD_DATA_DIR not set.")
  } else {
    if (!pacta.workflow.utils::check_dir_writable(dashboard_data_dir)) {
      log_warn("Directory \"{dashboard_data_dir}\" is not writable.")
      stop("Directory \"{dashboard_data_dir}\" is not writable.")
    }
  }

  library(dplyr)

  # input and output directories

  # TODO: Remove these renames
  input_dir <- analysis_output_dir
  output_dir <- dashboard_data_dir
  data_dir <- benchmarks_dir

  # portfolio/user parameters

  investor_name <- "investor_name"
  portfolio_name <- "portfolio_name"
  peer_group <- "peer_group"
  language_select <- "EN"

  currency_exchange_value <- 1
  display_currency <- "USD"

  select_scenario_other <- "WEO2023_NZE_2050"
  select_scenario <- "WEO2023_NZE_2050"

  green_techs <- c(
    "RenewablesCap",
    "HydroCap",
    "NuclearCap",
    "Hybrid",
    "Electric",
    "FuelCell",
    "Hybrid_HDV",
    "Electric_HDV",
    "FuelCell_HDV",
    "Electric Arc Furnace"
  )
  tech_roadmap_sectors <- c(
    "Automotive",
    "Power",
    "Oil&Gas",
    "Coal"
  )
  pacta_sectors_not_analysed <- c(
    "Steel",
    "Aviation",
    "Cement"
  )

  power_tech_levels <- c(
    "RenewablesCap",
    "HydroCap",
    "NuclearCap",
    "GasCap",
    "OilCap",
    "CoalCap"
  )
  oil_gas_levels <- c(
    "Oil",
    "Gas"
  )
  coal_levels <- c(
    "Coal"
  )
  auto_levels <- c(
    "Electric",
    "Electric_HDV",
    "FuelCell",
    "FuelCell_HDV",
    "Hybrid",
    "Hybrid_HDV",
    "ICE",
    "ICE_HDV"
  )
  cement_levels <- c(
    "Integrated facility",
    "Grinding"
  )
  steel_levels <- c(
    "Electric Arc Furnace",
    "Open Hearth Furnace",
    "Basic Oxygen Furnace"
  )
  aviation_levels <- c(
    "Freight",
    "Passenger",
    "Mix",
    "Other"
  )
  all_tech_levels <- c(
    power_tech_levels,
    auto_levels,
    oil_gas_levels,
    coal_levels,
    cement_levels,
    steel_levels,
    aviation_levels
  )


  # config parameters from manifest

  manifest <- jsonlite::read_json(
    path = file.path(input_dir, "manifest.json")
  )

  start_year <- manifest$params$analysis$startYear
  year_span <- manifest$params$analysis$timeHorizon
  pacta_sectors <- unlist(manifest$params$analysis$sectorList)
  equity_market_levels <- unlist(manifest$params$analysis$equityMarketList)
  scen_geo_levels <- unlist(manifest$params$analysis$scenarioGeographiesList)


  # load results from input directory

  audit_file <- readRDS(
    file.path(input_dir, "audit_file.rds")
  )
  emissions <- readRDS(
    file.path(input_dir, "emissions.rds")
  )
  equity_results_portfolio <- readRDS(
    file.path(input_dir, "Equity_results_portfolio.rds")
  )
  bonds_results_portfolio <- readRDS(
    file.path(input_dir, "Bonds_results_portfolio.rds")
  )
  equity_results_map <- readRDS(
    file.path(input_dir, "Equity_results_map.rds")
  )
  bonds_results_map <- readRDS(
    file.path(input_dir, "Bonds_results_map.rds")
  )
  equity_results_company <- readRDS(
    file.path(input_dir, "Equity_results_company.rds")
  )
  bonds_results_company <- readRDS(
    file.path(input_dir, "Bonds_results_company.rds")
  )


  # data from PACTA inputs used to generate the results

  indices_bonds_results_portfolio <- readRDS(
    file.path(data_dir, "Indices_bonds_results_portfolio.rds")
  )
  indices_equity_results_portfolio <- readRDS(
    file.path(data_dir, "Indices_equity_results_portfolio.rds")
  )
  peers_bonds_results_portfolio <-
    pacta.portfolio.utils::empty_portfolio_results()
  peers_equity_results_portfolio <-
    pacta.portfolio.utils::empty_portfolio_results()


  # translations

  dataframe_translations <- readr::read_csv(
    system.file(
      "extdata/translation/dataframe_labels.csv",
      package = "pacta.portfolio.report"
    ),
    col_types = readr::cols()
  )

  header_dictionary <- readr::read_csv(
    system.file(
      "extdata/translation/dataframe_headers.csv",
      package = "pacta.portfolio.report"
    ),
    col_types = readr::cols()
  )

  dictionary <-
    pacta.portfolio.report:::choose_dictionary_language(
      data = dataframe_translations,
      language = language_select
    )

  header_dictionary <- pacta.portfolio.report:::replace_contents(
    header_dictionary,
    display_currency
  )


  # add investor_name and portfolio_name to results data frames because
  # pacta.portfolio.report functions expect that

  audit_file <- audit_file %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  emissions <- emissions %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  equity_results_portfolio <- equity_results_portfolio %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  bonds_results_portfolio <- bonds_results_portfolio %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  equity_results_map <- equity_results_map %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  bonds_results_map <- bonds_results_map %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  equity_results_company <- equity_results_company %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )

  bonds_results_company <- bonds_results_company %>%
    dplyr::mutate(
      investor_name = investor_name,
      portfolio_name = portfolio_name
    )


  # data_included_table.json

  audit_file %>%
    pacta.portfolio.report:::prep_audit_table(
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      currency_exchange_value = currency_exchange_value
    ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_included_table",
      dictionary,
      inplace = TRUE
    ) %>%
    pacta.portfolio.report:::translate_df_headers(
      "data_included_table",
      language_select,
      header_dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_included_table.json")
    )


  # data_value_pie_bonds.json

  audit_file %>%
    pacta.portfolio.report:::prep_exposure_pie(
      asset_type = "Bonds",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors,
      currency_exchange_value = currency_exchange_value
    ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_value_pie_bonds",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_value_pie_bonds.json")
    )


  # data_emissions_equity.json

  emissions %>%
    pacta.portfolio.report:::prep_emissions_pie(
      asset_type = "Equity",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors
    ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_emissions_pie_equity",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_emissions_pie_equity.json")
    )


  # data_emissions_bonds.json

  emissions %>%
    pacta.portfolio.report:::prep_emissions_pie(
      asset_type = "Bonds",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors
    ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_emissions_pie_bonds",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_emissions_pie_bonds.json")
    )


  # data_value_pie_equity.json

  audit_file %>%
    pacta.portfolio.report:::prep_exposure_pie(
      asset_type = "Equity",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors,
      currency_exchange_value = currency_exchange_value
    ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_value_pie_equity",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_value_pie_equity.json")
    )


  # data_techmix.json

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
    pacta.portfolio.report:::translate_df_contents(
      "techexposure_data",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_techexposure.json")
    )


  # data_techmix_sector.json

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
    jsonlite::write_json(
      path = file.path(output_dir, "data_techmix_sector.json")
    )

  # data_map.json

  pacta.portfolio.report:::prep_exposure_map(
    equity_results_map = equity_results_map,
    bonds_results_map = bonds_results_map,
    portfolio_name = portfolio_name,
    start_year = start_year
  ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_map",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_map.json")
    )


  # data_trajectory_alignment.json

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
    pacta.portfolio.report:::translate_df_contents(
      "data_trajectory_alignment",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_trajectory_alignment.json")
    )


  # data_emissions.json

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
    pacta.portfolio.report:::translate_df_contents(
      "data_emissions",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_emissions.json")
    )

  # data_exposure_stats.json

  prep_exposure_stats(
    audit_file = audit_file,
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors
  ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_exposure_stats.json")
    )

  # data_company_bubble.json

  prep_company_bubble(
    equity_results_company = equity_results_company,
    bonds_results_company = bonds_results_company,
    portfolio_name = portfolio_name,
    start_year = start_year,
    green_techs = green_techs
  ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_company_bubble",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_company_bubble.json")
    )


  # data_techexposure_company_companies.json

  prep_key_bars_company(
    equity_results_company = equity_results_company,
    bonds_results_company = bonds_results_company,
    portfolio_name = portfolio_name,
    start_year = start_year,
    pacta_sectors_not_analysed = pacta_sectors_not_analysed,
    all_tech_levels = all_tech_levels
  ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_key_bars_company",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_techexposure_company_companies.json")
    )


  # data_techexposure_company_portfolio.json

  prep_key_bars_portfolio(
    equity_results_portfolio = equity_results_portfolio,
    bonds_results_portfolio = bonds_results_portfolio,
    portfolio_name = portfolio_name,
    start_year = start_year,
    pacta_sectors_not_analysed = pacta_sectors_not_analysed,
    all_tech_levels = all_tech_levels
  ) %>%
    pacta.portfolio.report:::translate_df_contents(
      "data_key_bars_portfolio",
      dictionary
    ) %>%
    jsonlite::write_json(
      path = file.path(output_dir, "data_techexposure_company_portfolio.json")
    )
}
