#' Remove rows containing extreme outliers using the IQR rule
#'
#' This function removes rows from a data frame if one or more numeric
#' variables contain extreme values based on the interquartile range (IQR).
#' By default, it checks all numeric columns, but you can choose specific
#' columns using cols.
#'
#' @param data A data frame containing the data to be cleaned.
#' @param cols Optional vector of column names or column positions to check.
#' If NULL, all numeric columns are used.
#' @param k Numeric value controlling how strict the IQR rule is.
#'
#' @return A data frame with rows containing extreme outliers removed.
#'
#' @examples
#' df <- data.frame(x = c(1,2,3,100))
#' remove_outliers(df)
#'
#' @export
remove_outliers <- function(data, cols = NULL, k = 1.5){

  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1 || k < 0) {
    stop("k must be a single positive numeric value.", call. = FALSE)
  }

  # choose columns
  if (is.null(cols)) {
    cols_idx <- which(vapply(data, is.numeric, logical(1)))
  } else {

    if (is.character(cols)) {
      missing_cols <- setdiff(cols, names(data))

      if (length(missing_cols) > 0) {
        stop(
          paste("Unknown column(s):", paste(missing_cols, collapse = ", ")),
          call. = FALSE
        )
      }
      cols_idx <- match(cols, names(data))
    } else if (is.numeric(cols)) {
      cols_idx <- cols
    } else {
      stop("cols must be NULL, a character vector or a numeric vector.", call. = FALSE)
    }

    cols_idx <- cols_idx[cols_idx >= 1 & cols_idx <= ncol(data)]
    cols_idx <- cols_idx[vapply(data[cols_idx], is.numeric, logical(1))]
  }

  if (length(cols_idx) == 0) {
    return(data)
  }

  keep_rows <- rep(TRUE, nrow(data))

  for (j in cols_idx) {
    x <- data[[j]]
    x_no_na <- x[!is.na(x)]

    if (length(x_no_na) < 4) {
      next
    }

    q1 <- stats::quantile(x_no_na, 0.25, names = FALSE)
    q3 <- stats::quantile(x_no_na, 0.75, names = FALSE)
    iqr <- q3 - q1

    lower <- q1 - k * iqr
    upper <- q3 + k * iqr

    outlier <- (x < lower) | (x > upper)
    outlier[is.na(outlier)] <- FALSE

    keep_rows <- keep_rows & !outlier
  }

  data[keep_rows, , drop = FALSE]
}


