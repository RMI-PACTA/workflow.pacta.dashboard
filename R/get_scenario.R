get_scenario <- function(
  scenario_parameter
) {
  scenario <- unlist(stringr::str_split(scenario_parameter,"_", n = 2))[2]
  scenario
}
