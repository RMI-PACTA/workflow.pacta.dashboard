filter_scenarios_per_sector <- function(
  data,
  select_scenario_other,
  select_scenario
) {
    special_sectors <- c("Aviation")
    rest_of_sectors <- setdiff(unique(data$ald_sector), special_sectors)

    data %>%
      filter(
        scenarios_found_in_sectors(.data, select_scenario_other, c("Aviation")) |
          scenarios_found_in_sectors(.data, select_scenario, rest_of_sectors)
      )
  }

