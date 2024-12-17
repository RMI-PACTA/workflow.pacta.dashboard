test_that("replace_contents replaces currency _CUR_ in column values", {
  test_data <- data.frame(
    id = LETTERS[1:5],
    currency = "_CUR_",
    stringsAsFactors = FALSE
  )
  result <- replace_contents(test_data, "USD")
  expect_identical(
    object = result,
    expected = data.frame(
      id = LETTERS[1:5],
      currency = "USD",
      stringsAsFactors = FALSE
    )
  )
})

test_that("replace_contents replaces currency _CUR_ as substrings", {
  test_data <- data.frame(
    id = LETTERS[1:5],
    currency = "Foo_CUR_ Bar",
    stringsAsFactors = FALSE
  )
  result <- replace_contents(test_data, "USD")
  expect_identical(
    object = result,
    expected = data.frame(
      id = LETTERS[1:5],
      currency = "FooUSD Bar",
      stringsAsFactors = FALSE
    )
  )
})

test_that("replace_contents operates on tibbles", {
  test_data <- tibble::tibble(
    id = LETTERS[1:5],
    currency = "_CUR_"
  )
  result <- replace_contents(test_data, "USD")
  expect_identical(
    object = result,
    expected = tibble::tibble(
      id = LETTERS[1:5],
      currency = "USD"
    )
  )
})

test_that("replace_contents casts all columns as character", {
  test_data <- data.frame(
    id = LETTERS[1L:5L],
    value = 1L:5L,
    currency = "_CUR_",
    stringsAsFactors = FALSE
  )
  result <- replace_contents(test_data, "USD")
  expect_true(
    all(
      vapply(
        X = result,
        FUN = is.character,
        FUN.VALUE = logical(1)
      )
    )
  )
  expect_identical(
    object = result,
    expected = data.frame(
      id = LETTERS[1:5],
      value = as.character(1L:5L),
      currency = "USD",
      stringsAsFactors = FALSE
    )
  )
})
