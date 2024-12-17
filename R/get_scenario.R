get_scenario <- function(
  scenario_parameter
) {
  scenario_name <- split_scenario_parameter(
    scenario_parameter = scenario_parameter
  )[["scenario_name"]]
  return(scenario_name)
}

get_scenario_source <- function(
  scenario_parameter
) {
  scenario_source <- split_scenario_parameter(
    scenario_parameter = scenario_parameter
  )[["scenario_source"]]
  return(scenario_source)
}

split_scenario_parameter <- function(
  scenario_parameter
) {
  split_parameter <- unlist(
    strsplit(
      x = scenario_parameter,
      split = "_",
      fixed = TRUE
    )
  )
  scenario_source <- split_parameter[[1L]]
  scenario_name <- paste(split_parameter[-1L], collapse = "_")
  return(
    list(
      scenario_source = scenario_source,
      scenario_name = scenario_name
    )
  )
}
