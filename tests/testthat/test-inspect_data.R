test_that("inspect_data returns expected structure", {

  df <- data.frame(
    x = c(1, 2, 3, 100),
    y = c(10, 11, 12, 13)
  )

  res <- inspect_data(df, print = FALSE)

  expect_type(res, "list")

  expect_named(
    res,
    c(
      "na_summary",
      "outlier_summary",
      "duplicate_rows",
      "column_types"
    )
  )
})


test_that("inspect_data correctly detects missing values", {

  df <- data.frame(
    x = c(1, NA, 3),
    y = c(NA, NA, 2)
  )

  res <- inspect_data(df, print = FALSE)

  total_na <- sum(res$na_summary$n_na)

  expect_equal(total_na, 3)
})


test_that("inspect_data detects duplicated rows", {

  df <- data.frame(
    x = c(1, 1, 2),
    y = c(3, 3, 4)
  )

  res <- inspect_data(df, print = FALSE)

  expect_equal(res$duplicate_rows, 1)
})


test_that("inspect_data returns column type information", {

  df <- data.frame(
    x = c(1, 2, 3),
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  res <- inspect_data(df, print = FALSE)

  expect_true(is.data.frame(res$column_types))
  expect_true(all(c("column", "class") %in% names(res$column_types)))
})


test_that("inspect_data is non-destructive", {

  df <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )

  df_copy <- df
  inspect_data(df, print = FALSE)

  expect_identical(df, df_copy)
})
