get_scenario_source <- function(
  scenario_parameter
) {
  scenario_source <- unlist(
    stringr::str_split(scenario_parameter, "_", n = 2)
  )[1]
  scenario_source
}
