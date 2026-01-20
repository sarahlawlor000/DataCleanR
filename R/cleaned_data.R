#' Clean a dataset using the DataCleanR pipeline
#'
#' Runs the main cleaning steps in this package and returns the cleaned dataset.
#'
#' @param data A data frame to clean.
#' @param cols Optional column names to check for outliers.
#'   If NULL, all numeric columns are checked.
#' @param k IQR multiplier for outlier removal.
#'
#' @return A cleaned data frame with class "cleaned_data".
#'
#' @examples
#' df <- data.frame(x = c(1,2,3,100), y = c(10,11,12,13))
#' cleaned <- clean_data(df)
#' cleaned
#'
#' @export
clean_data <- function(data, cols = NULL, k = 1.5) {

  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.", call. = FALSE)
  }

  # keeps track of what changed
  rows_before <- nrow(data)

  # remove outliers
  cleaned <- remove_outliers(data, cols = cols, k = k)

#-----------------------------------------------
  # standardise missing values: Aadi
  # cleaned <- standardise_missing(cleaned)
#-----------------------------------------------

#----------------------------------------------
  # inspect: Hanna
  # inspect_data(cleaned)
#----------------------------------------------

  rows_after <- nrow(cleaned)

  # this is information for the method to print later
  attr(cleaned, "rows_before") <- rows_before
  attr(cleaned, "rows_after") <- rows_after
  attr(cleaned, "outlier_k") <- k

  class(cleaned) <- c("cleaned_data", "data.frame")
  cleaned
}



