#' Inspect data quality
#'
#' Reports missing values, potential outliers (IQR rule), duplicated rows,
#' and column classes without modifying the data.
#'
#' @param df A data frame.
#' @param outlier_k Numeric; multiplier for IQR fences (default 1.5).
#' @param print Logical; if TRUE prints summaries to console.
#'
#' @return A named list with elements: na_summary, outlier_summary,
#' duplicate_rows, column_types.
#' @export
inspect_data <- function(df, outlier_k = 1.5, print = TRUE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(outlier_k) || length(outlier_k) != 1L || is.na(outlier_k) || outlier_k <= 0) {
    stop("`outlier_k` must be a single positive number.", call. = FALSE)
  }
  if (!is.logical(print) || length(print) != 1L) {
    stop("`print` must be a single TRUE/FALSE.", call. = FALSE)
  }

  n <- nrow(df)

  # Missing values summary
  na_summary <- data.frame(
    column = names(df),
    n_na = vapply(df, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  na_summary$pct_na <- if (n == 0) NA_real_ else na_summary$n_na / n
  rownames(na_summary) <- NULL

  # Column classes
  column_types <- data.frame(
    column = names(df),
    class = vapply(df, function(x) class(x)[1], character(1)),
    stringsAsFactors = FALSE
  )

  # Duplicate rows
  duplicate_rows <- sum(duplicated(df))

  # Outlier summary (numeric columns only)
  is_num <- vapply(df, is.numeric, logical(1))
  num_names <- names(df)[is_num]

  outlier_summary <- if (length(num_names) == 0) {
    data.frame(column = character(0), n_outliers = integer(0), stringsAsFactors = FALSE)
  } else {
    data.frame(
      column = num_names,
      n_outliers = vapply(df[num_names], function(x) {
        x <- x[is.finite(x)]
        if (length(x) < 4) return(0L)
        q1 <- stats::quantile(x, 0.25, names = FALSE, type = 7)
        q3 <- stats::quantile(x, 0.75, names = FALSE, type = 7)
        iqr <- q3 - q1
        lo <- q1 - outlier_k * iqr
        hi <- q3 + outlier_k * iqr
        as.integer(sum(x < lo | x > hi))
      }, integer(1)),
      stringsAsFactors = FALSE
    )
  }
  rownames(outlier_summary) <- NULL


  res <- list(
    na_summary = na_summary,
    outlier_summary = outlier_summary,
    duplicate_rows = duplicate_rows,
    column_types = column_types
  )
  class(res) <- c("data_inspection", class(res))

  if (isTRUE(print)) {
    message("Data inspection summary")
    print(na_summary)
    print(outlier_summary)
    message("Duplicated rows: ", duplicate_rows)
  }

  invisible(res)
}

