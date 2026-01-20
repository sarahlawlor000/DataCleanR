#' Print a cleaned_data object
#'
#' This is the print method for objects returned by cleaned_data().
#' It prints a summary of what happened during cleaning,
#' then prints the cleaned dataset.
#'
#' @param x a cleaned_data object
#' @param ... Further arguments passed to or from other methods.

#'
#' @return The cleaned dataset
#' @examples
#' df <- data.frame(x = c(1,2,3,100), y = c(10,11,12,13))
#' cleaned <- clean_data(df)
#' cleaned  # triggers print.cleaned_data()
#'
#' @export
print.cleaned_data <- function(x, ...){
  cat("Cleaned dataset\n")

  rows_before <- attr(x, "rows_before")
  rows_after <- attr(x, "rows_after")
  k_used <- attr(x, "outlier_k")

  if(!is.null(rows_before) && !is.null(rows_after)){
    cat("Rows before: ", rows_before, "\n")
    cat("Rows after: ", rows_after, "\n")
    cat("Rows removed: ", rows_before - rows_after, "\n")


  }

#----------------------------------------------------
  # Aadi
  na_before <- attr(x, "na_before")
  na_after <- attr(x, "na_after")
  na_added <- attr(x, "na_added")

  if(!is.null(na_before) && !is.null(na_after) && !is.null(na_added)){
    cat("\nMissing values standardised (Aadi):\n")
    cat("NA before: ", na_before, "\n")
    cat("NA after: ", na_after, "\n")
    cat("New NA added: ", na_added, "\n")
  }

#----------------------------------------------------
  # Hanna
  inspection <- attr(x, "inspection")
  inspected  <- attr(x, "inspected")

  if (isTRUE(inspected) && !is.null(inspection)) {
    cat("\nData diagnostics (Hanna):\n")

    if (!is.null(inspection$na_summary)) {
      total_na <- sum(inspection$na_summary$n_na)
      cat("Total missing values detected: ", total_na, "\n")
    }

    if (!is.null(inspection$outlier_summary)) {
      total_outliers <- sum(inspection$outlier_summary$n_outliers)
      cat("Potential outliers detected: ", total_outliers, "\n")
    }

    if (!is.null(inspection$duplicate_rows)) {
      cat("Duplicated rows detected: ", inspection$duplicate_rows, "\n")
    }
  }
  #----------------------------------------------------

  print.data.frame(x)
  invisible(x)
}


