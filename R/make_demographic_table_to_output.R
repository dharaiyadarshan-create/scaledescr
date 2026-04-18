#' Create and Export Demographic Summary Table
#'
#' @description
#' Generates a demographic summary table using `make_demographic_table()`
#' and exports the resulting table to Word, Excel, or CSV format.
#'
#' This is a convenience wrapper that combines analysis and output
#' in a single step for users who want immediate file export.
#'
#' @param data A data frame containing the dataset.
#' @param vars Character vector of variable names to include in the
#'   demographic summary.
#' @param continuous_vars Optional character vector specifying which
#'   variables should be treated as continuous.
#'   If `NULL`, numeric variables are automatically detected.
#' @param file_name Name of the output file WITHOUT extension
#'   (e.g., "demographics").
#' @param format Output format. Must be one of:
#'   \itemize{
#'     \item `"word"`  - exports to a Word document (.docx)
#'     \item `"excel"` - exports to an Excel file (.xlsx)
#'     \item `"csv"`   - exports to a CSV file (.csv)
#'   }
#' @param path Optional character string specifying the directory where the
#'   file should be saved. If `NULL` (default), the file is written to a
#'   temporary directory. Provide a full directory path or use getwd()
#'   during the function call.
#'
#' @details
#' The demographic table includes:
#' \itemize{
#'   \item Categorical variables reported as n (%)
#'   \item Continuous variables reported as Mean (SD)
#' }
#'
#' Word export uses `flextable` and `officer` for formatting.
#' Excel export uses `openxlsx`.
#' CSV export uses base R `write.csv()`.
#'
#' @return Invisibly returns the full file path to the generated output file.
#'
#' @export
make_demographic_table_to_output <- function(data,
                                             vars,
                                             continuous_vars = NULL,
                                             file_name = "demographic_table",
                                             format = c("word", "excel", "csv"),
                                             path = NULL) {

  format <- match.arg(format)

  # 1. Determine output directory
  if (is.null(path)) {
    out_dir <- tempdir()
  } else {
    out_dir <- path
  }

  if (!dir.exists(out_dir)) stop("Directory does not exist: ", out_dir, call. = FALSE)

  # 2. Construct full file path FIRST
  ext <- switch(format,
                "word" = "docx",
                "excel" = "xlsx",
                "csv" = "csv")

  filepath <- file.path(out_dir, paste0(file_name, ".", ext))

  # 3. Generate demographic table and convert to dataframe
  gtsummary_table <- make_demographic_table(
    data = data,
    vars = vars,
    continuous_vars = continuous_vars
  )

  table_data <- as.data.frame(gtsummary_table)

  # 4. Export the dataframe using the constructed 'filepath'
  if (format == "word") {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Package 'flextable' is required for Word export. Please install it.")
    }
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Package 'officer' is required for Word export. Please install it.")
    }

    flex_tbl <- flextable::flextable(table_data)
    flex_tbl <- flextable::autofit(flex_tbl)
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, flex_tbl)
    print(doc, target = filepath)
  }

  if (format == "excel") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for Excel export. Please install it.")
    }
    openxlsx::write.xlsx(table_data, file = filepath)
  }

  if (format == "csv") {
    utils::write.csv(table_data, file = filepath, row.names = FALSE)
  }

  message("File created: ", filepath)
  invisible(filepath)
}
