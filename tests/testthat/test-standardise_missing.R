test_that("standardise_missing converts missing-like strings to NA", {
  df <- data.frame(
    a = c("ok", "N/A", "  not available  ", ""),
    stringsAsFactors = FALSE
  )

  cleaned <- standardise_missing(df)

  expect_identical(cleaned$a[1], "ok")
  expect_true(is.na(cleaned$a[2]))
  expect_true(is.na(cleaned$a[3]))
  expect_true(is.na(cleaned$a[4]))
})

test_that("standardise_missing converts NaN to NA without changing numeric type", {
  df <- data.frame(
    b = c(1, NaN, 3),
    stringsAsFactors = FALSE
  )

  cleaned <- standardise_missing(df)

  expect_true(is.numeric(cleaned$b))
  expect_true(is.na(cleaned$b[2]))
  expect_identical(cleaned$b[c(1, 3)], c(1, 3))
})

test_that("standardise_missing works on factor columns", {
  df <- data.frame(
    f = factor(c("yes", "NA", "no", "null")),
    stringsAsFactors = FALSE
  )

  cleaned <- standardise_missing(df)

  expect_true(is.factor(cleaned$f))
  expect_identical(as.character(cleaned$f)[1], "yes")
  expect_true(is.na(cleaned$f[2]))
  expect_identical(as.character(cleaned$f)[3], "no")
  expect_true(is.na(cleaned$f[4]))
})

##
