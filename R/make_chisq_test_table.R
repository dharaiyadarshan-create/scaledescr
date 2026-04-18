#' Create a one-row summary table for a chi-square test
#'
#' This function formats the result of a pre-computed \code{chisq.test()}
#' into a single-row data frame. It supports goodness-of-fit,
#' independence, and homogeneity chi-square tests and includes an appropriate
#' effect size with a qualitative interpretation.
#'
#' The function does not perform the chi-square test itself and does not
#' introduce new statistical methods. All test statistics are extracted
#' directly from the supplied \code{chisq.test()} object.
#'
#' @param chisq_object An object or list of objects of class \code{"htest"} produced by
#'   \code{stats::chisq.test()}.
#' @param test_type Character string specifying the type of chi-square test.
#'   One of \code{"goodness-of-fit"}, \code{"independence"}, or
#'   \code{"homogeneity"}.  for multiple object, if separate test_type are not provided than given test type will be uesd for all chi square test./
#' @param digits Integer indicating the number of decimal places to round to.
#'
#' @return A single-row data frame with the following columns:
#' \itemize{
#'   \item \code{test}: Type of chi-square test
#'   \item \code{chi_square}: Chi-square statistic
#'   \item \code{df}: Degrees of freedom
#'   \item \code{p_value}: p-value
#'   \item \code{N}: Total sample size
#'   \item \code{effect_size}: Effect size (Cohen’s w or Cramér’s V)
#'   \item \code{effect_type}: Type of effect size reported
#'   \item \code{effect_interpretation}: Qualitative interpretation of effect size
#' }
#'
#' @details
#' For goodness-of-fit tests, Cohen’s w is reported. For tests of independence
#' and homogeneity, Cramér’s V is reported. Effect size interpretations follow
#' conventional benchmarks (0.10 = small, 0.30 = medium, 0.50 = large).
#'
#' @examples
#' # Goodness-of-fit example
#' observed <- c(40, 30, 50)
#' chisq_gof <- chisq.test(observed)
#'
#' make_chisq_test_table(
#'   chisq_object = chisq_gof,
#'   test_type = "goodness-of-fit"
#' )
#'
#' # Independence test example
#' tbl <- matrix(c(20, 30, 10, 40), nrow = 2)
#' chisq_ind <- chisq.test(tbl)
#'
#' make_chisq_test_table(
#'   chisq_object = chisq_ind,
#'   test_type = "independence"
#' )
#'
#' @export
make_chisq_test_table <- function(
    chisq_object,
    test_type = c("independence", "goodness-of-fit", "homogeneity"),
    digits = 3
) {

  # 1. Allow single htest or list of htest
  if (inherits(chisq_object, "htest")) {
    chisq_object <- list(chisq_object)
  }

  # 2. Validate the input
  if (!all(sapply(chisq_object, inherits, "htest"))) {
    stop("All inputs must be a chisq.test() result object.")
  }

  # 3. Handle test_type argument logic
  valid_types <- c("independence", "goodness-of-fit", "homogeneity")

  # If user didn't change the default or provided nothing
  if (missing(test_type) || identical(test_type, valid_types)) {
    test_type <- "independence"
  }

  # 4. Recycle test_type if it's only length 1 to match number of tests
  if (length(test_type) == 1) {
    test_type <- rep(test_type, length(chisq_object))
  }

  # 5. Check and Standardize types (This replaces match.arg for vector support)
  test_type <- sapply(test_type, function(x) {
    if (x %in% valid_types) {
      return(x)
    } else {
      warning(paste0("'", x, "' is not a standard type. Defaulting to 'independence'."))
      return("independence")
    }
  })

  # 6. Process each test
  results <- lapply(seq_along(chisq_object), function(i) {
    obj <- chisq_object[[i]]
    type <- test_type[i]

    chi_sq <- unname(obj$statistic)
    df <- unname(obj$parameter)
    p_val <- obj$p.value
    N <- sum(obj$observed)

    # Effect Size Logic
    if (type == "goodness-of-fit") {
      effect_type <- "Cohen_w"
      effect_size <- sqrt(chi_sq / N)
    } else {
      effect_type <- "Cramers_V"
      dims <- dim(obj$observed)
      # k is min(rows, cols) - 1. For vectors, dims is NULL, so k=1.
      k <- if (is.null(dims)) 1 else min(dims) - 1
      k <- ifelse(k < 1, 1, k)
      effect_size <- sqrt(chi_sq / (N * k))
    }

    # Interpretation (Cohen, 1988)
    #
    effect_label <- ifelse(effect_size < 0.10, "negligible",
                           ifelse(effect_size < 0.30, "small",
                                  ifelse(effect_size < 0.50, "medium", "large")))

    # Format p-value
    p_formatted <- ifelse(p_val < 10^-digits,
                          paste0("< ", formatC(10^-digits, format = "f", digits = digits)),
                          formatC(p_val, format = "f", digits = digits))

    data.frame(
      test = type,
      chi_square = round(chi_sq, digits),
      df = df,
      p_value = p_formatted,
      N = N,
      effect_size = round(effect_size, digits),
      effect_type = effect_type,
      effect_interpretation = effect_label,
      stringsAsFactors = FALSE
    )
  })

  # 7. Bind into one table
  df_final <- do.call(rbind, results)
  rownames(df_final) <- NULL
  return(df_final)
}
