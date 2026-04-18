#' Create a one-row summary table of a paired t-test
#'
#' This function performs a paired t-test between two numeric variables in a data frame
#' and returns a one-row summary table including means, mean difference, t-value, degrees of freedom,
#' p-value, and confidence interval.
#'
#' @param data A data frame containing the two numeric variables.
#' @param var1 Character string. Name of the first variable (observation 1) in `data`.
#' @param var2 Character string. Name of the second variable (observation 2) in `data`.
#' @param var_name Optional character string. Custom name for the variable to display in the table. Default is `var1 vs var2`.
#' @param alternative Character string specifying the alternative hypothesis. One of `"two.sided"`, `"less"`, or `"greater"`. Default is `"two.sided"`.
#' @param conf.level Confidence level for the interval. Default is 0.95.
#'
#' @return A one-row data frame with columns:
#' \itemize{
#'   \item `Variable` - variable name
#'   \item `Mean_obs1` - mean of observation 1
#'   \item `Mean_obs2` - mean of observation 2
#'   \item `Mean_diff` - mean difference (obs1 - obs2)
#'   \item `t_value` - t statistic
#'   \item `df` - degrees of freedom
#'   \item `p_value` - p-value
#'   \item `CI_lower` - lower bound of confidence interval
#'   \item `CI_upper` - upper bound of confidence interval
#' }
#'
#' @export
#' @examples
#' # example data
#' df <- data.frame(
#'   before = c(10, 12, 14, 15, 11),
#'   after  = c(11, 13, 13, 16, 12)
#' )
#'
#' # Run the paired t-test summary
#' make_paired_t_test_table(df, var1 = "before", var2 = "after")

#' @importFrom stats t.test na.omit
make_paired_t_test_table <- function(data, var1, var2, var_name = NULL,
                                     alternative = "two.sided", conf.level = 0.95) {
  # extract variables
  x <- data[[var1]]
  y <- data[[var2]]

  # keep complete pairs only
  paired_data <- na.omit(data.frame(x, y))

  # paired t-test
  tt <- t.test(paired_data$x,
    paired_data$y,
    paired = TRUE,
    alternative = alternative,
    conf.level = conf.level
  )

  # means
  mean_x <- mean(paired_data$x)
  mean_y <- mean(paired_data$y)

  # variable label
  if (is.null(var_name)) {
    var_name <- paste(var1, "vs", var2)
  }

  # result table
  data.frame(
    Variable      = var_name,
    Mean_obs1     = round(mean_x, 2),
    Mean_obs2     = round(mean_y, 2),
    Mean_diff     = round(as.numeric(tt$estimate), 2),
    t_value       = round(as.numeric(tt$statistic), 2),
    df            = as.numeric(tt$parameter),
    p_value       = round(tt$p.value, 3),
    CI_lower      = round(tt$conf.int[1], 2),
    CI_upper      = round(tt$conf.int[2], 2)
  )
}
