scenarios_found_in_sectors <- function(
  data,
  select_scenario_param,
  sectors
) {
  out <- (data[["ald_sector"]] %in% sectors) &
    (data[["scenario"]] == get_scenario(select_scenario_param)) &
    (data[["scenario_source"]] == get_scenario_source(select_scenario_param))
  out
}
