#' Print a cleaned_data object
#'
#' This is the print method for objects returned by cleaned_data().
#' It prints a summary of what happened during cleaning,
#' then prints the cleaned dataset.
#'
#' @param x a cleaned_data object
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
    cat("Rows removed: ", rows_before-rows_after)

  }

#----------------------------------------------------
  # Aadi and hannas codes:

#----------------------------------------------------

  print.data.frame(x)
  invisible(x)
}


