#' Create a cleanData object
#'
#' Wrap a data.frame with metadata about cleaning operations.
#'
#' @param data A data.frame (or tibble).
#' @param notes A named list containing metadata (e.g. counts of replaced values).
#'
#' @return An object of class \code{"cleanData"}.
#' @export
as_cleanData <- function(data, notes = list()) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.list(notes)) {
    stop("`notes` must be a list.", call. = FALSE)
  }

  structure(
    list(data = data, notes = notes),
    class = "cleanData"
  )
}

