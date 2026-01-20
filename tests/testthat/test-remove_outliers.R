test_that("removes rows with IQR outliers in numeric columns", {
  df <- data.frame(x = c(1, 2, 3, 100))
  out <- remove_outliers(df)
  expect_equal(out$x, c(1, 2, 3))
})

test_that("does nothing when there are no numeric columns", {
  df <- data.frame(a = c("x", "y", "z"))
  out <- remove_outliers(df)
  expect_identical(out, df)
})

test_that("returns same data when fewer than 4 non-missing values", {
  df <- data.frame(x = c(1, 2, NA))
  out <- remove_outliers(df)
  expect_identical(out, df)
})

test_that("can check only selected columns by name", {
  df <- data.frame(
    x = c(1, 2, 3, 100),
    y = c(10, 11, 12, 13)
  )
  out <- remove_outliers(df, cols = "y")
  expect_identical(out, df)  # no outliers in y
})

test_that("errors on non-data-frame input", {
  expect_error(remove_outliers(1:10), "data frame")
})

test_that("errors on invalid k", {
  df <- data.frame(x = c(1, 2, 3, 4))
  expect_error(remove_outliers(df, k = -1), "k must be")
  expect_error(remove_outliers(df, k = c(1, 2)), "k must be")
})
