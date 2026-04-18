#' Compute Intraclass Correlation Coefficient (ICC) for Selected Columns
#'
#' @description
#' Computes Intraclass Correlation Coefficients (ICC) for a specified set
#' of numeric variables within a data frame using \code{psych::ICC()}.
#'
#' This function standardizes column selection and basic validation while
#' delegating all statistical estimation to \code{psych::ICC()}.
#'
#' @param data A data frame containing the ratings or measurements.
#' @param items A character vector specifying column names for which
#'   ICC should be calculated (e.g., raters or repeated measurements).
#' @param check_numeric Logical. If \code{TRUE} (default), ensures all
#'   selected columns are numeric.
#' @param lmer Logical. Passed to \code{psych::ICC()}. Default is FALSE.
#' @param ... Additional arguments passed to \code{psych::ICC()}.
#'
#' @return An object returned by \code{psych::ICC()}, typically a list
#'   containing a results table with ICC estimates, F statistics,
#'   confidence intervals, and model details.
#'
#' @details
#' The function does not choose or filter specific ICC models.
#' All available ICC types (e.g., ICC1, ICC2, ICC3, single and average)
#' are returned exactly as produced by \code{psych::ICC()}.
#'
#' Users are responsible for:
#' \itemize{
#'   \item Choosing the appropriate ICC model for their study design.
#'   \item Ensuring assumptions (e.g., continuous ratings, independence).
#'   \item Handling missing data appropriately.
#' }
#' @seealso
#' \code{\link[psych]{ICC}}
#'
#' @importFrom psych ICC
#' @export
compute_ICC <- function(data,
                        items,
                        check_numeric = TRUE,
                        lmer = FALSE,
                        ...) {

  # ---- Validate data ----
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  if (missing(items) || length(items) < 2) {
    stop("`items` must contain at least two column names.")
  }

  if (!all(items %in% names(data))) {
    missing_items <- items[!items %in% names(data)]
    stop(
      paste("The following items were not found in `data`:",
            paste(missing_items, collapse = ", "))
    )
  }

  # ---- Subset data ----
  sub_data <- data[, items, drop = FALSE]

  # ---- Numeric check ----
  if (check_numeric) {
    non_numeric <- names(sub_data)[!sapply(sub_data, is.numeric)]
    if (length(non_numeric) > 0) {
      stop(
        paste("The following items are not numeric:",
              paste(non_numeric, collapse = ", "))
      )
    }
  }

  # ---- Compute ICC using psych ----
  psych::ICC(sub_data, lmer = lmer, ...)
}
