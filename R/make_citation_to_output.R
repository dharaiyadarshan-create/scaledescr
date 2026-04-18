#' Export Session Package Citations to Multiple Formats
#'
#' This function identifies all currently loaded external packages in the current R session,
#' along with base R, and extracts their citation information. It generates both
#' plain text and BibTeX formats and exports them using the package's internal
#' export utility.
#'
#' @param filename A character string specifying the name of the output file.
#'   Defaults to "Session_Citations".
#' @param format A character string specifying the output format:
#'   "csv", "word", "excel", or "pdf". Defaults to "csv".
#' @param path A character string specifying the directory path for the output.
#'   If NULL (default), it uses a temporary directory. Use "getwd()" for the
#'   current working directory.
#' @param packages A character vector of package names to extract citations for.
#'   If NULL (default), citations are extracted for all currently loaded packages
#'   in the session, including base R. Example: \code{packages = c("lavaan", "psych")}.
#'
#' @return The file path of the created document (invisibly).
#'
#' @examples
#' \dontrun{
#' # Export citations for currently loaded packages to a docs
#' make_citation_to_output(filename = "My_References", format = "word")
#'
#' # Export citations for selected packages to a docs in current working directory
#' make_citation_to_output( packages = c("base","psych","ggplot2"),
#'                          filename = "Selected_References",
#'                          format = "word",
#'                          path = getwd())
#'
#'
#' # Export to Excel in the current working directory
#' make_citation_to_output(format = "excel", path = "getwd()")
#'
#' }
#'
#' @importFrom utils sessionInfo citation capture.output packageVersion
#' @export
make_citation_to_output <- function(filename = "Session_Citations",
                                    format = "csv",
                                    path = NULL,
                                    packages = NULL) {
  # 1. Identify packages
  if (is.null(packages)) {
    pkgs <- c("base", names(sessionInfo()$otherPkgs))
  } else {
    pkgs <- packages
  }

  # 2. Extract Citation Data
  citation_list <- lapply(pkgs, function(p) {

    text_cit <- tryCatch({
      cit <- citation(p)
      paste(format(cit, style = "text"), collapse = " ")
    }, error = function(e) NA_character_)

    bib_cit <- tryCatch({
      cit <- citation(p)
      paste(utils::capture.output(print(utils::toBibtex(cit))), collapse = "\n")
    }, error = function(e) NA_character_)

    ver <- tryCatch(
      as.character(utils::packageVersion(p)),
      error = function(e) NA_character_
    )

    data.frame(
      Package       = p,
      Version       = ver,
      Citation_Text = text_cit,
      BibTeX        = bib_cit,
      stringsAsFactors = FALSE
    )
  })

  citation_df <- do.call(rbind, citation_list)

  # 3. Export
  scaledescr::make_dataframe_to_output(
    data = citation_df,
    filename = filename,
    format = format,
    path = path
  )
}
