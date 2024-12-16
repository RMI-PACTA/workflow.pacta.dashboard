get_scenario_source <- function(
  scenario_parameter
) {
  source <- unlist(stringr::str_split(scenario_parameter, "_", n = 2))[1]
  source
}
