#' Create Descriptive Table for Multiple Scale Columns
#'
#' Computes descriptive statistics for one or more scale total columns.
#' Accepts either a numeric vector (single column) or a data frame with column names.
#'
#' @param x Optional numeric vector of total scores (single column, old-style).
#' @param data Optional data frame containing one or more scale columns.
#' @param columns Optional character vector of column names in `data` (new-style).
#' @param scale_names Optional character vector of names for each scale. Defaults to column names.
#' @param type "summary" for base::summary(), NULL (default) uses psych::describe().
#'
#' @return A data frame with one row per column, containing descriptive statistics.
#' @export
make_scale_description_table <- function(x = NULL, data = NULL, columns = NULL,
                                         scale_names = NULL, type = NULL) {

  # ---- Determine input ----
  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("data must be a data.frame")
    if (is.null(columns)) stop("columns must be provided when using data")
    missing_cols <- setdiff(columns, names(data))
    if (length(missing_cols) > 0) stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
    x_list <- lapply(columns, function(col) data[[col]])
  } else if (!is.null(x)) {
    # single numeric vector
    if (!is.numeric(x)) stop("x must be numeric")
    x_list <- list(x)
    if (is.null(scale_names)) scale_names <- "Scale1"
  } else {
    stop("Either x or data must be provided")
  }

  # ---- Scale names ----
  if (!is.null(data)) {
    if (is.null(scale_names)) scale_names <- columns
    if (length(scale_names) != length(columns)) stop("scale_names length must match columns length")
  }

  # ---- Compute statistics for each column ----
  results <- lapply(seq_along(x_list), function(i) {
    x_col <- x_list[[i]]
    scale_name <- scale_names[i]

    if (is.null(type)) {
      descr <- psych::describe(x_col)
      data.frame(
        Scale = scale_name,
        N = descr$n,
        Mean = round(descr$mean, 2),
        SD = round(descr$sd, 2),
        Median = round(descr$median, 2),
        Range = paste0(round(descr$min, 2), "-", round(descr$max, 2)),
        Skewness = round(descr$skew, 2),
        Kurtosis = round(descr$kurtosis, 2),
        stringsAsFactors = FALSE
      )
    } else if (type == "summary") {
      s <- summary(x_col)
      data.frame(
        Scale = scale_name,
        N = sum(!is.na(x_col)),
        `1st Qu.` = round(s[["1st Qu."]], 2),
        Median = round(s[["Median"]], 2),
        Mean = round(s[["Mean"]], 2),
        `3rd Qu.` = round(s[["3rd Qu."]], 2),
        Range = paste0(round(s[["Min."]], 2), "-", round(s[["Max."]], 2)),
        stringsAsFactors = FALSE
      )
    } else {
      stop("type must be NULL or 'summary'")
    }
  })

  # ---- Combine into one table ----
  do.call(rbind, results)
}
