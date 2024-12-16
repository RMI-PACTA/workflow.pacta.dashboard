test_that("get_scenario_source splits on underscore", {
  scenario_string <- "SOURCE_SCENARIO"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "SOURCE"
  )
})

test_that("get_scenario_source respects case of original string", {
  scenario_string <- "source_scenario"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "source"
  )
})

test_that("get_scenario_source respects mixed case of original string", {
  scenario_string <- "sOuRcE_ScEnArIo"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "sOuRcE"
  )
})

test_that("get_scenario_source extracts source name from string with 1 underscore", {
  scenario_string <- "GECO2022_1.5C"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "GECO2022"
  )
})

test_that("get_scenario_source extracts source name from string with 2 underscores", {
  scenario_string <- "WEO2022_NZE_2050"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "WEO2022"
  )
})

# test_that("get_scenario_source throws error if no source (nothing before underscore)", {
#   scenario_string <- "_SCENARIO"
#   expect_error(
#     get_scenario_source(scenario_string),
#     regexp = "No source found in string"
#   )
# })
#
# test_that("get_scenario_source throws error if no scenario_name (nothing after underscore)", {
#   scenario_string <- "SOURCE_"
#   expect_error(
#     get_scenario_source(scenario_string),
#     regexp = "No scenario found in string"
#   )
# })
#
# test_that("get_scenario_source throws warning if multiple consecutive underscores", {
#   scenario_string <- "SOURCE__SCENARIO"
#   expect_warning(
#     {results <- get_scenario_source(scenario_string)},
#     regexp = "Multiple consecutive underscores in scenario string."
#   )
#   expect_identical(
#     object = results,
#     expected = "SOURCE"
#   )
# })
