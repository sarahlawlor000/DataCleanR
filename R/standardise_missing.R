#' Standardise missing-value encodings to NA
#'
#' Many datasets use different strings to represent missingness (e.g. "N/A",
#' "not available", "", "null"). This function converts common missing-like
#' strings to proper \code{NA} while preserving column types where possible.
#' Numeric \code{NaN} values are also converted to \code{NA}.
#'
#' @param data A data.frame (or tibble).
#' @param tokens Character vector of missing-like strings to convert to \code{NA}.
#'   Matching is applied to character and factor columns.
#' @param ignore_case Logical; if TRUE, matching ignores case.
#' @param trim_ws Logical; if TRUE, trims leading/trailing whitespace before matching.
#' @param convert_nan Logical; if TRUE, converts \code{NaN} in numeric columns to \code{NA}.
#'
#' @return A data.frame of the same class as \code{data} (where possible), with
#'   missing-like values converted to \code{NA}.
#'
#' @examples
#' df <- data.frame(
#'   a = c("ok", "N/A", "  not available  ", ""),
#'   b = c(1, NaN, 3, 4),
#'   c = factor(c("yes", "NA", "no", "null")),
#'   stringsAsFactors = FALSE
#' )
#'
#' cleaned <- standardise_missing(df)
#' cleaned$a
#' cleaned$b
#' cleaned$c
#'
#' @export
standardise_missing <- function(
    data,
    tokens = c("NA", "N/A", "na", "n/a", "null", "NULL", "none", "None",
               "not available", "Not Available", "missing", "", " "),
    ignore_case = TRUE,
    trim_ws = TRUE,
    convert_nan = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!is.character(tokens)) {
    stop("`tokens` must be a character vector.", call. = FALSE)
  }

  out <- data

  # Helper to normalise strings for matching
  normalise <- function(x) {
    if (trim_ws) x <- trimws(x)
    if (ignore_case) x <- tolower(x)
    x
  }

  # Pre-normalise tokens for matching
  tokens_norm <- unique(normalise(tokens))

  for (nm in names(out)) {
    col <- out[[nm]]

    # Convert NaN -> NA for numeric-like columns
    if (convert_nan && (is.numeric(col) || is.complex(col))) {
      if (any(is.nan(col), na.rm = TRUE)) {
        col[is.nan(col)] <- NA
      }
      out[[nm]] <- col
      next
    }

    # Handle character columns: match tokens -> NA
    if (is.character(col)) {
      col_norm <- normalise(col)
      to_na <- !is.na(col_norm) & (col_norm %in% tokens_norm)
      if (any(to_na)) col[to_na] <- NA
      out[[nm]] <- col
      next
    }

    # Handle factor columns: convert to character, replace, convert back
    if (is.factor(col)) {
      col_chr <- as.character(col)
      col_norm <- normalise(col_chr)
      to_na <- !is.na(col_norm) & (col_norm %in% tokens_norm)
      if (any(to_na)) col_chr[to_na] <- NA
      out[[nm]] <- factor(col_chr, levels = unique(col_chr[!is.na(col_chr)]))
      next
    }

    # Leave other column types unchanged (logical, Date, list)
    out[[nm]] <- col
  }

  out
}
