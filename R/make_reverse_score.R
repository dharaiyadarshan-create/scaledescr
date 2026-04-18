#' Create Reverse Scores as New Columns
#'
#' @param data A data frame.
#' @param vars A character vector of column names to reverse.
#' @param min_val The minimum possible score of the scale.
#' @param max_val The maximum possible score of the scale.
#' @param suffix A character string to append to the new column names. Defaults to "_rev".
#'
#' @return A data frame containing the original data plus the new reversed columns.
#' @export
make_reverse_score <- function(data, vars, min_val, max_val, suffix = "_rev") {

  # Warn if multiple variables are supplied
  if (length(vars) > 1) {
    warning("Multiple variables selected for reverse scoring. Ensure that min_val and max_val are identical for all variables.")
  }

  # Calculate the reversal constant
  rev_constant <- min_val + max_val

  # Generate the new column names (e.g., "Q1_rev")
  new_cols <- paste0(vars, suffix)

  # Calculate reverse scores and assign them directly to the new columns
  data[new_cols] <- rev_constant - data[vars]

  return(data)
}
