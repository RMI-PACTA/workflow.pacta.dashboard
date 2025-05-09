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

test_that("get_scenario extracts scenario name string with 1 underscore", {
  scenario_string <- "GECO2022_1.5C"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "1.5C"
  )
})

test_that("get_scenario extracts scenario name string with 2 underscores", {
  scenario_string <- "WEO2022_NZE_2050"
  results <- get_scenario(scenario_string)
  expect_identical(
    object = results,
    expected = "NZE_2050"
  )
})
