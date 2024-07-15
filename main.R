library(dplyr)


# input and output directories -------------------------------------------------

input_dir <- "~/Desktop/test_pacta/test_outputs"
output_dir <- "./output"


# config parameters ------------------------------------------------------------

investor_name <- "investor_name"
portfolio_name <- "portfolio_name"
currency_exchange_value <- 1
display_currency <- "USD"
language_select <- "EN"
pacta_sectors <- c("Power", "Automotive", "Oil&Gas", "Coal", "Steel", "Cement", "Aviation")

peer_group <- "peer_group"
select_scenario_other <- "select_scenario_other"
select_scenario <- "select_scenario"
start_year <- 2023
green_techs <- "green_techs"
equity_market_levels <- "equity_market_levels"
all_tech_levels <- "all_tech_levels"

tech_roadmap_sectors <- "tech_roadmap_sectors"
year_span <- "year_span"
scen_geo_levels <- "scen_geo_levels"


# data from PACTA inputs used to generate the results --------------------------

indices_equity_results_portfolio <- NULL
indices_bonds_results_portfolio <- NULL
peers_equity_results_portfolio <- NULL
peers_bonds_results_portfolio <- NULL


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


# load results from input directory --------------------------------------------

audit_file <- readRDS(file.path(input_dir, "audit_file.rds"))
equity_results_portfolio <- readRDS(file.path(input_dir, "Equity_results_portfolio.rds"))
bonds_results_portfolio <- readRDS(file.path(input_dir, "Bonds_results_portfolio.rds"))


# add investor_name and portfolio_name to results dataframes because -----------
# pacta.portfolio.report functions expect that ---------------------------------

audit_file <-
  audit_file %>%
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
