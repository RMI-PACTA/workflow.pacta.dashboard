prepare_pacta_dashboard_data <- function(
  params,
  analysis_output_dir = Sys.getenv("ANALYSIS_OUTPUT_DIR"),
  dashboard_data_dir = Sys.getenv("DASHBOARD_DATA_DIR"),
  benchmarks_dir = Sys.getenv("BENCHMARKS_DIR")
) {

  log_info("Preparing data for the PACTA dashboard.")

  # portfolio/user parameters ----------------------------------------------------
  log_debug("Reading portfolio/user parameters.")

  investor_name <- params[["user"]][["name"]]
  portfolio_name <- params[["portfolio"]][["name"]]
  peer_group <- params[["user"]][["peerGroup"]]
  language_select <- params[["user"]][["languageSelect"]]

  currency_exchange_value <- params[["user"]][["currencyExchangeValue"]]
  display_currency <- params[["user"]][["displayCurrency"]]

  select_scenario_other <- params[["reporting"]][["scenarioOther"]]
  select_scenario <- params[["reporting"]][["selectScenario"]]

  green_techs <- params[["reporting"]][["greenTechs"]]
  tech_roadmap_sectors <- params[["reporting"]][["techRoadmapSectors"]]
  pacta_sectors_not_analysed <- params[["reporting"]][["pactaSectorsNotAnalysed"]]

  power_tech_levels <- c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "OilCap", "CoalCap")
  oil_gas_levels <- c("Oil", "Gas")
  coal_levels <- c("Coal")
  auto_levels <- c("Electric", "Electric_HDV", "FuelCell", "FuelCell_HDV", "Hybrid", "Hybrid_HDV", "ICE", "ICE_HDV")
  cement_levels <- c("Integrated facility", "Grinding")
  steel_levels <- c("Electric Arc Furnace", "Open Hearth Furnace", "Basic Oxygen Furnace")
  aviation_levels <- c("Freight", "Passenger", "Mix", "Other")
  all_tech_levels <- c(power_tech_levels, auto_levels, oil_gas_levels, coal_levels, cement_levels, steel_levels, aviation_levels)


  # config parameters from manifest ----------------------------------------------
  log_debug("Reading config parameters from analysis outputs manifest.")

  manifest <- jsonlite::read_json(path = file.path(analysis_output_dir, "manifest.json"))

  start_year <- manifest$params$analysis$startYear
  year_span <- manifest$params$analysis$timeHorizon
  pacta_sectors <- unlist(manifest$params$analysis$sectorList)
  equity_market_levels <- unlist(manifest$params$analysis$equityMarketList)
  scen_geo_levels <- unlist(manifest$params$analysis$scenarioGeographiesList)


  # load results from input directory --------------------------------------------
  log_debug("Loading results from input directory.")

  audit_file <- readRDS(file.path(analysis_output_dir, "audit_file.rds"))
  emissions <- readRDS(file.path(analysis_output_dir, "emissions.rds"))
  equity_results_portfolio <- readRDS(file.path(analysis_output_dir, "Equity_results_portfolio.rds"))
  bonds_results_portfolio <- readRDS(file.path(analysis_output_dir, "Bonds_results_portfolio.rds"))
  equity_results_company <- readRDS(file.path(analysis_output_dir, "Equity_results_company.rds"))
  bonds_results_company <- readRDS(file.path(analysis_output_dir, "Bonds_results_company.rds"))


  # data from PACTA inputs used to generate the results --------------------------
  log_debug("Loading benchmark results.")

  indices_bonds_results_portfolio <- readRDS(file.path(benchmarks_dir, "Indices_bonds_results_portfolio.rds"))
  indices_equity_results_portfolio <- readRDS(file.path(benchmarks_dir, "Indices_equity_results_portfolio.rds"))

  log_debug("Loading peer results.")
  peers_bonds_results_portfolio <- pacta.portfolio.utils::empty_portfolio_results()
  peers_equity_results_portfolio <- pacta.portfolio.utils::empty_portfolio_results()


  # translations -----------------------------------------------------------------
  log_debug("Loading translations.")

  dataframe_translations <- readr::read_csv(
    system.file("extdata/translation/dataframe_labels.csv", package = "workflow.pacta.dashboard"),
    col_types = readr::cols()
  )

  header_dictionary <- readr::read_csv(
    system.file("extdata/translation/dataframe_headers.csv", package = "workflow.pacta.dashboard"),
    col_types = readr::cols()
  )

  js_translations <- jsonlite::fromJSON(
    txt = system.file("extdata/translation/js_labels.json", package = "workflow.pacta.dashboard")
  )

  sector_order <- readr::read_csv(
    system.file("extdata/sector_order/sector_order.csv", package = "workflow.pacta.dashboard"),
    col_types = readr::cols()
  )

  dictionary <-
    choose_dictionary_language(
      data = dataframe_translations,
      language = language_select
    )

  header_dictionary <- replace_contents(header_dictionary, display_currency)


  # add investor_name and portfolio_name to results data frames because ----------
  # pacta.portfolio.report functions expect that ---------------------------------
  log_debug("Adding investor_name and portfolio_name to results data frames.")

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

  log_info("Preparing data_included_table.json.")
  audit_file %>%
    prep_audit_table(
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      currency_exchange_value = currency_exchange_value
    ) %>%
    translate_df_contents("data_included_table", dictionary, inplace = TRUE) %>%
    translate_df_headers("data_included_table", language_select, header_dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_included_table.json"))


  # data_value_pie_bonds.json ----------------------------------------------------

  log_info("Preparing data_value_pie_bonds.json.")
  audit_file %>%
    prep_exposure_pie(
      asset_type = "Bonds",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors,
      currency_exchange_value = currency_exchange_value
    ) %>%
    translate_df_contents("data_value_pie_bonds", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_value_pie_bonds.json"))


  # data_emissions_equity.json ---------------------------------------------------

  log_info("Preparing data_emissions_equity.json.")
  emissions %>%
    prep_emissions_pie(
      asset_type = "Equity",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors
    ) %>%
    translate_df_contents("data_emissions_pie_equity", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_emissions_pie_equity.json"))


  # data_emissions_bonds.json ----------------------------------------------------

  log_info("Preparing data_emissions_bonds.json.")
  emissions %>%
    prep_emissions_pie(
      asset_type = "Bonds",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors
    ) %>%
    translate_df_contents("data_emissions_pie_bonds", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_emissions_pie_bonds.json"))


  # data_value_pie_equity.json ---------------------------------------------------

  log_info("Preparing data_value_pie_equity.json.")
  audit_file %>%
    prep_exposure_pie(
      asset_type = "Equity",
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      pacta_sectors = pacta_sectors,
      currency_exchange_value = currency_exchange_value
    ) %>%
    translate_df_contents("data_value_pie_equity", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_value_pie_equity.json"))


  # data_techmix.json ------------------------------------------------------------

  log_info("Preparing data_techmix.json.")
  prep_techexposure(
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
    translate_df_contents("techexposure_data", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_techexposure.json"))


  # data_techmix_sector.json -----------------------------------------------------

  log_info("Preparing data_techmix_sector.json.")
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
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_techmix_sector.json"))

  # data_trajectory_alignment.json -----------------------------------------------

  log_info("Preparing data_trajectory_alignment.json.")
  prep_trajectory_alignment(
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
    translate_df_contents("data_trajectory_alignment", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_trajectory_alignment.json"))


  # data_emissions.json ----------------------------------------------------------

  log_info("Preparing data_emissions.json.")
  prep_emissions_trajectory(
    equity_results_portfolio = equity_results_portfolio,
    bonds_results_portfolio = bonds_results_portfolio,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors,
    year_span = year_span,
    start_year = start_year
  ) %>%
    translate_df_contents("data_emissions", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_emissions.json"))

  # data_exposure_stats.json

  log_info("Preparing data_exposure_stats.json.")
  prep_exposure_stats(
    audit_file = audit_file,
    investor_name = investor_name,
    portfolio_name = portfolio_name,
    pacta_sectors = pacta_sectors,
    currency_exchange_value = currency_exchange_value
  ) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_exposure_stats.json"))


  # data_company_bubble.json -----------------------------------------------------

  log_info("Preparing data_company_bubble.json.")
  prep_company_bubble(
    equity_results_company = equity_results_company,
    bonds_results_company = bonds_results_company,
    portfolio_name = portfolio_name,
    start_year = start_year,
    green_techs = green_techs
  ) %>%
    translate_df_contents("data_company_bubble", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_company_bubble.json"))


  # data_techexposure_company_companies.json -------------------------------------

  log_info("Preparing data_techexposure_company_companies.json.")
  prep_key_bars_company(
    equity_results_company = equity_results_company,
    bonds_results_company = bonds_results_company,
    portfolio_name = portfolio_name,
    start_year = start_year,
    pacta_sectors_not_analysed = pacta_sectors_not_analysed,
    all_tech_levels = all_tech_levels
  ) %>%
    translate_df_contents("data_key_bars_company", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_techexposure_company_companies.json"))


  # data_techexposure_company_portfolio.json -------------------------------------

  log_info("Preparing data_techexposure_company_portfolio.json.")
  prep_key_bars_portfolio(
    equity_results_portfolio = equity_results_portfolio,
    bonds_results_portfolio = bonds_results_portfolio,
    portfolio_name = portfolio_name,
    start_year = start_year,
    pacta_sectors_not_analysed = pacta_sectors_not_analysed,
    all_tech_levels = all_tech_levels
  ) %>%
    translate_df_contents("data_key_bars_portfolio", dictionary) %>%
    jsonlite::write_json(path = file.path(dashboard_data_dir, "data_techexposure_company_portfolio.json"))


  # put JSON and CSV outputs into a zip archive ----------------------------------

  zip_outputs(dashboard_data_dir)

}


zip_outputs <- function(dashboard_data_dir) {
  log_debug("Preparing outputs zip archive.")
  json_filenames <- list.files(dashboard_data_dir, pattern = "[.]json$")

  zip_temp <- file.path(tempdir(), "zip_temp")
  dir.create(zip_temp, showWarnings = FALSE)

  for (json_filename in json_filenames) {
    log_trace(paste("Adding", json_filename, "to zip archive."))
    file.copy(
      from = file.path(dashboard_data_dir, json_filename),
      to = file.path(zip_temp, json_filename)
    )

    csv_filename <- sub("[.]json$", ".csv", json_filename)

    df <- jsonlite::read_json(
      path = file.path(dashboard_data_dir, json_filename),
      simplifyVector = TRUE
    )

    if (inherits(df, "data.frame")) {
      log_trace(paste("Adding", csv_filename, "to zip archive."))
      readr::write_csv(
        x = df,
        file = file.path(zip_temp, csv_filename),
        na = "",
        eol = "\n"
      )
    }
  }

  log_debug("Creating zip archive.")
  utils::zip(
    zipfile = file.path(dashboard_data_dir, "archive.zip"),
    files = list.files(zip_temp, full.names = TRUE),
    flags = "-r9Xjq"
  )
}
