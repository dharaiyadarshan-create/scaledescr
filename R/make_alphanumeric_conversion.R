#' Convert Between Alphabetical and Numeric Responses
#'
#' @description
#' Converts specified columns between alphabetical (text) and numeric values.
#' Can either overwrite existing columns or create new ones.
#'
#' @param data A data frame.
#' @param column_vars Character vector of column names to convert.
#' @param from_values Vector of values to replace (character or numeric).
#' @param to_values Vector of replacement values (must be same length as from_values).
#' @param new_names Optional character vector of new column names.
#'   If NULL (default), original columns are overwritten.
#' @param case_sensitive Logical. Only relevant when converting character values.
#'   Default is FALSE.
#'
#' @details
#' Factors are automatically converted to character before matching.
#' Unmatched values become NA with a warning.
#'
#' @return A data frame with converted values.
#' @export
make_alphanumeric_conversion <- function(data,
                                         column_vars,
                                         from_values,
                                         to_values,
                                         new_names = NULL,
                                         case_sensitive = FALSE) {

  # ---- Basic checks ----
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  if (!all(column_vars %in% names(data))) {
    missing <- setdiff(column_vars, names(data))
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }

  if (length(from_values) != length(to_values)) {
    stop("from_values and to_values must be the same length")
  }

  if (!is.null(new_names) && length(new_names) != length(column_vars)) {
    stop("new_names must be same length as column_vars")
  }

  target_cols <- if (is.null(new_names)) column_vars else new_names

  # ---- Create mapping ----
  if (is.character(from_values) && !case_sensitive) {
    mapping <- stats::setNames(to_values, tolower(from_values))
  } else {
    mapping <- stats::setNames(to_values, from_values)
  }

  # ---- Apply conversion ----
  for (i in seq_along(column_vars)) {

    old_col <- column_vars[i]
    new_col <- target_cols[i]

    col_data <- data[[old_col]]

    # Convert factors to character
    if (is.factor(col_data)) {
      col_data <- as.character(col_data)
    }

    # Handle character matching
    if (is.character(from_values)) {

      if (!case_sensitive) {
        match_col <- tolower(as.character(col_data))
      } else {
        match_col <- as.character(col_data)
      }

      converted <- unname(mapping[match_col])

    } else {
      # Numeric matching
      converted <- unname(mapping[as.character(col_data)])
    }

    # ---- Unmatched warning ----
    original_na <- sum(is.na(col_data))
    new_na <- sum(is.na(converted))

    if (new_na > original_na) {
      unmatched <- setdiff(unique(col_data), from_values)
      unmatched <- unmatched[!is.na(unmatched)]
      if (length(unmatched) > 0) {
        warning("Column '", old_col, "' has unmatched values (becomes NA): ",
                paste(utils::head(unmatched, 5), collapse = ", "),
                if (length(unmatched) > 5) "..." else "")
      }
    }

    data[[new_col]] <- converted
  }

  return(data)
}
