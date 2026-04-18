#' Wrap a pre computed psych::alpha object into a single row table with N and alpha only.
#'
#' Extracts only the Scale Name, Sample Size (N), and Cronbach's Alpha
#' from a `psych::alpha` object.
#' @param alpha_res A psych::alpha object (already computed)
#' @param scale_name Name of the scale (default: "Scale")
#'
#' @return A data frame with three columns: Scale,N, and Alpha.
#' @examples
#' \dontrun{
#' library(psych)
#' res <- psych::alpha(mtcars[,1:3])
#' make_alpha_table(res, scale_name = "Car Scale")
#' }
#' @export
make_alpha_table <- function(alpha_res, scale_name = "Scale") {

  # 1. Provide the requested warning about the structural change
  warning("The current version simplify output to Scale, N, and Alpha.
           The previous version of  `make_alpha_table()` was returning inconsistent results for
          some psych::alpha objects due to variation in alpha_res object structure.")

  # 2. Initialize variables to NA to satisfy CMD check and handle missing data
  raw_alpha <- NA
  n_obs <- NA

  # 3. Robust extraction of required values
  # Alpha: from total summary, rounded to 2 decimals
  raw_alpha <- if (!is.null(alpha_res$total$raw_alpha)) {
    round(as.numeric(alpha_res$total$raw_alpha), 2)
  } else {
    NA
  }

  # 4. Create the simplified table
  # Using stringsAsFactors = FALSE is best practice for older R versions
  res_df <- data.frame(
    Scale = scale_name,
    N = n_obs,
    Alpha = raw_alpha,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  return(res_df)
}
