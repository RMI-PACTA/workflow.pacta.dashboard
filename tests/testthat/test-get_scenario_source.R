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

test_that("get_scenario_source extracts source name with 1 underscore", {
  scenario_string <- "GECO2022_1.5C"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "GECO2022"
  )
})

test_that("get_scenario_source extracts source name with 2 underscores", {
  scenario_string <- "WEO2022_NZE_2050"
  results <- get_scenario_source(scenario_string)
  expect_identical(
    object = results,
    expected = "WEO2022"
  )
})
