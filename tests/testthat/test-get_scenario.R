test_that("get_scenario splits on underscore", {
  scenario_string <- "SOURCE_SCENARIO"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "SCENARIO"
  )
})

test_that("get_scenario respects case of original string", {
  scenario_string <- "source_scenario"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "scenario"
  )
})

test_that("get_scenario respects mixed case of original string", {
  scenario_string <- "sOuRcE_ScEnArIo"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "ScEnArIo"
  )
})

test_that("get_scenario extracts scneario name from string with 1 underscore", {
  scenario_string <- "GECO2022_1.5C"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "1.5C"
  )
})

test_that("get_scenario extracts scneario name from string with 2 underscores", {
  scenario_string <- "WEO2022_NZE_2050"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "NZE_2050"
  )
})

# test_that("get_scenario throws error if no scenario_name (nothing after underscore)", {
#   scenario_string <- "SOURCE_"
#   expect_error(
#     get_scenario(scenario_string),
#     regexp = "No scenario found in string"
#   )
# })
#
# test_that("get_scenario throws error if no source (nothing before underscore)", {
#   scenario_string <- "_SCENARIO"
#   expect_error(
#     get_scenario(scenario_string),
#     regexp = "No source found in string"
#   )
# })
#
# test_that("get_scenario throws warning if multiple consecutive underscores", {
#   scenario_string <- "SOURCE__SCENARIO"
#   expect_warning(
#     {results <- get_scenario(scenario_string)},
#     regexp = "Multiple consecutive underscores in scenario string."
#   )
#   expect_identical(
#     object = results,
#     expected = "_SCENARIO"
#   )
# })
#
