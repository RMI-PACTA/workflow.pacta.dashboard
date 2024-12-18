filter_scenarios_per_sector <- function(
  data,
  select_scenario_other,
  select_scenario
) {
  special_sectors <- "Aviation"
  rest_of_sectors <- setdiff(unique(data[["ald_sector"]]), special_sectors)

  data |>
    dplyr::filter(
      scenarios_found_in_sectors(.data, select_scenario_other, "Aviation") |
        scenarios_found_in_sectors(.data, select_scenario, rest_of_sectors)
    )
}
