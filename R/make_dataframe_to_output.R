#' Export a data frame to CSV, Word (.docx),Excel (.xlsx) or pdf format.
#'
#' If no output path is specified, the file is written to a temporary directory
#' using `tempdir()`.
#' For reproducible workflows, users are encouraged to explicitly specify an output location, either the current working directory
#' or a full file path.
#'
#' - CSV files are written using `utils::write.csv()`
#' - Word files (`docx`) are created using the `officer` package
#' - Excel (`xlsx`) files are created using the `openxlsx` package
#' - PDF files are created using the 'flextable' package
#'
#' @param data A data frame to export.
#' @param filename Optional base file name (without extension). Defaults to object name.
#' (without path and without extension).
#' If not provided, the name of the input object is used.
#' @param format Character string specifying the output format. One of
#'   `"csv"`, `"word"`,`"excel"` or `"pdf"`. Default is `"csv"`.
#' @param path Optional character string specifying the directory where the
#'   file should be saved. If `NULL` (default), the file is written to a
#'   temporary directory. Use `path = getwd()` to save the file in the current
#'   working directory, or provide a full directory path.
#' @return Invisibly returns the full file path to the generated output file.
#' @note Exporting to PDF requires the 'webshot2' package to be installed.
#' @export
#' @importFrom utils write.csv
#' @importFrom officer read_docx body_add_par body_add_table
#' @importFrom openxlsx write.xlsx
#' @importFrom flextable flextable gen_grob font autofit
#' @importFrom grDevices pdf dev.off
#' @importFrom grid viewport pushViewport popViewport grid.draw
#'
#' @examples
#' \dontrun{
#'
#' # Example dataset available in base R
#' data_df <- head(mtcars)
#'
#' #--------------------------------------------------
#' # 1. Simplest use: export to CSV in temp directory
#' #--------------------------------------------------
#' make_dataframe_to_output(data_df)
#'
#' #--------------------------------------------------
#' # 2. Specify filename
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = data_df,
#'   filename = "mtcars_sample"
#' )
#'
#' #--------------------------------------------------
#' # 3. Export as Word document
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = data_df,
#'   filename = "mtcars_word_table",
#'   format = "word"
#' )
#'
#' #--------------------------------------------------
#' # 4. Export as Excel file
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = data_df,
#'   filename = "mtcars_excel_table",
#'   format = "excel"
#' )
#'
#' #--------------------------------------------------
#' # 5. Save to current working directory
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = data_df,
#'   filename = "mtcars_current_folder",
#'   format = "csv",
#'   path = "getwd()"
#' )
#'
#' #--------------------------------------------------
#' # 6. Save Excel file to current working directory
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = data_df,
#'   filename = "mtcars_excel_current",
#'   format = "excel",
#'   path = "getwd()"
#' )
#'
#' #--------------------------------------------------
#' # 7. Export another base dataset (iris)
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = head(iris),
#'   filename = "iris_sample",
#'   format = "word"
#' )
#'
#' #--------------------------------------------------
#' # 8. Using a custom folder path
#' #--------------------------------------------------
#' make_dataframe_to_output(
#'   data = head(airquality),
#'   filename = "airquality_data",
#'   format = "excel",
#'   path = "D:/output_folder"
#' )
#'
#' }


make_dataframe_to_output <- function( data,
                                      filename = NULL,
                                      format = "csv",
                                      path = NULL)
{ if (!is.data.frame(data)) stop("Input must be a data frame", call. = FALSE)

  # Validate format
  format <- tolower(format)
  if (!format %in% c("csv", "word", "excel","pdf")) {
    stop("Format must be one of 'csv', 'word', or 'excel', or 'pdf'.", call. = FALSE)
  }

  # Default filename
  if (is.null(filename)) {
    filename <- make.names(deparse(substitute(data)))
  }
  filename <- make.names(filename)  # ensure safe filename

  # Determine output directory
  if (is.null(path)) {
    out_dir <- tempdir()
  }
  else if (identical(path, "getwd()"))
  {    out_dir <- getwd()
  }
  else {
    out_dir <- path
  }
  if (!dir.exists(out_dir)) stop("Directory does not exist: ", out_dir, call. = FALSE)

  # Construct full file path
  ext <- switch(format,
                "excel" = "xlsx",
                "word"  = "docx",
                "pdf"   = "pdf",
                "csv")
  filepath <- file.path(out_dir, paste0(filename, ".", ext))

  # Export based on format
  if (format == "csv") {
    utils::write.csv(data, filepath, row.names = FALSE, fileEncoding = "UTF-8")
  }
  else if (format == "word") {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Please install the 'officer' package to export Word documents.", call. = FALSE)
    }
    colnames(data) <- make.names(colnames(data))
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, filename, style = "heading 1")
    doc <- officer::body_add_table(doc, value = data)
    print(doc, target = filepath)
  }
  else if (format == "excel") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Please install the 'openxlsx' package to export Excel files.", call. = FALSE)
    }
    # Write Excel
    openxlsx::write.xlsx(data, file = filepath, rowNames = FALSE)
  }
  else if (format == "pdf") {

    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Please install the 'flextable' package for PDF export.", call. = FALSE)
    }

    # ---- Branch condition ----
    is_citation_df <- all(c("Citation_Text", "BibTeX") %in% names(data))

    # ---- Warning early ----
    if (is_citation_df) {
      warning(
        "PDF export for citation tables may render poorly due to long text fields.
      Consider using format = 'docx' and converting to PDF for better results.",
        call. = FALSE
      )
    }

    # ---- Build flextable differently based on data ----
    if (is_citation_df) {
      ft <- flextable::flextable(data)
      ft <- flextable::font(ft, fontname = "sans", part = "all")
      ft <- flextable::fontsize(ft, size = 7, part = "all")   # smaller text
      ft <- flextable::autofit(ft)

      # safer grob (avoid layout stress)
      table_grob <- flextable::gen_grob(ft)

      use_viewport <- FALSE   # 🚨 disable viewport for this case

    } else {
      ft <- flextable::flextable(data)
      ft <- flextable::font(ft, fontname = "sans", part = "all")
      ft <- flextable::autofit(ft)

      table_grob <- flextable::gen_grob(ft, fit = "width", scaling = "min")

      use_viewport <- TRUE
    }

    # ---- Open PDF ----
    grDevices::pdf(file = filepath, width = 8.27, height = 11.69)
    grid::grid.newpage()

    # ---- Conditional drawing ----
    if (use_viewport) {
      margin_window <- grid::viewport(
        width = 0.9,
        height = 0.9,
        just = "top",
        y = 0.98
      )
      grid::pushViewport(margin_window)
      grid::grid.draw(table_grob)
      grid::popViewport()
    } else {
      # safer path (no layout conflict)
      grid::grid.draw(table_grob)
    }

    grDevices::dev.off()

  }

  message("File created: ", filepath)
  invisible(filepath)}
