#' Create a demographics summary table
#'
#' @param data A data frame
#' @param vars demographic variables to include in the table
#' @param continuous_vars Optional subset of vars to be treated as continuous
#'
#' @return A gtsummary table
#' @export
#' @examples
#' df <- data.frame(
#'   age = c("25", "30 years", "35", " 40 ", "22.5", "28+", NA, ""),
#'   sex = c("M", "F", "m", "f", " M ", "F", "m", NA),
#'   education = c("HS", "BA", "MA", "ma", "Hs", "Ma", "Ba Bed", "Msc bed ")
#' )
#'
#' # Generate a demographic summary table (assign to object to avoid printing)
#' demo_table <- make_demographic_table(df, vars = c("age", "sex", "education"))
#' demo_table # optionally inspect the table
#'
#' @importFrom dplyr select all_of
#' @importFrom dplyr %>%
#' @importFrom gtsummary tbl_summary all_categorical all_continuous
#' @importFrom purrr map_lgl map_chr
#' @importFrom rlang enquos as_name
#' @importFrom stringr str_trim str_to_title
make_demographic_table <- function(data, vars, continuous_vars = NULL) {

  # --- Check vars exist ---
  if (!all(vars %in% names(data))) {
    stop("Some variables in `vars` are not present in `data`.",
         call. = FALSE)
  }

  # --- Check continuous_vars only if provided ---
  if (!is.null(continuous_vars)) {
    if (!all(continuous_vars %in% vars)) {
      stop("All continuous_vars must also be included in vars.",
           call. = FALSE)
    }
  }

  data_selected <- data |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ {res <- stringr::str_to_title(stringr::str_trim(.))
        ifelse(res == "", NA_character_, res)
        }
      )
    )

  # --- Detect or assign continuous variables ---
  if (is.null(continuous_vars)) {
    continuous_names <- names(data_selected)[
      purrr::map_lgl(data_selected, is.numeric)
    ]
  } else {
    # User-specified continuous variables
    continuous_names <- continuous_vars

    # Coerce even if alphabets are present
    data_selected <- data_selected |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(continuous_names),
          ~ suppressWarnings(
            as.numeric(gsub("[^0-9.+-]", "", as.character(.)))
          )
        )
      )
  }

  # --- Define type list safely ---
  type_list <- NULL
  if (length(continuous_names) > 0) {
    type_list <- list(dplyr::all_of(continuous_names) ~ "continuous")
  }

  gtsummary::tbl_summary(
    data_selected,
    statistic = list(
      gtsummary::all_categorical() ~ "{n} ({p}%)",
      gtsummary::all_continuous() ~ "{mean} ({sd})"
    ),
    type = type_list,
    missing = "ifany"
  )
}
