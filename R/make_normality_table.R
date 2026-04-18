#' Make Normality Table
#'
#' Computes Shapiro-Wilk and Kolmogorov-Smirnov normality tests for specified variables.
#'
#' @param data A data frame.
#' @param vars Character vector of variable names. If NULL, all columns are used.
#' @importFrom stats shapiro.test ks.test
#' @return A data frame with W/D statistics and p-values for both tests.
#'
#' @export
make_normality_table <- function(data, vars = NULL) {

  if (is.null(vars)) vars <- names(data)
  df <- data[, vars, drop = FALSE]

  result <- do.call(rbind, lapply(vars, function(v) {
    x <- na.omit(df[[v]])

    sw <- shapiro.test(x)
    ks <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))


    format_p <- function(p) ifelse(p < .001, "< .001", as.character(round(p, 3)))

    data.frame(
      Variable  = v,
      SW_W      = round(sw$statistic, 3),
      SW_p = format_p(sw$p.value),
      KS_D      = round(ks$statistic, 3),
      KS_p = format_p(ks$p.value),
      stringsAsFactors = FALSE
    )
  }))

  rownames(result) <- NULL
  message("Note: p > .05 indicates data does not significantly deviate from normality. Shapiro-Wilk is generally recommended for small to moderate samples; interpret both tests alongside visual inspection (Q-Q plot, histogram).")
  return(result)
}
