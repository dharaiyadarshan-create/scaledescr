#' Create a one-row summary table for an independent-samples t-test
#'
#' This function performs an independent-samples t-test (Welch's t-test by default)
#' between two groups defined by a binary grouping variable and returns a
#' single-row data frame. The output includes group names,
#' sample sizes, mean difference, test statistics, p-value, and effect size
#' (Cohen's d) with a qualitative interpretation.
#'
#' The function is intended for streamlined reporting and does not introduce
#' new statistical methods. All computations rely on \code{stats::t.test()}.
#'
#' @param data A data frame containing the outcome and grouping variables.
#' @param outcome Character string specifying the numeric outcome variable.
#' @param group Character string specifying the grouping variable. Must have exactly two levels.
#'
#' @return A single-row data frame with the following columns:
#' \itemize{
#'   \item \code{test}: Name of the statistical test
#'   \item \code{group1}, \code{group2}: Group labels
#'   \item \code{mean_diff}: Mean difference between groups (group1 - group2)
#'   \item \code{t_value}: t statistic
#'   \item \code{df}: Degrees of freedom
#'   \item \code{p_value}: p-value
#'   \item \code{n_group1}, \code{n_group2}: Sample sizes per group
#'   \item \code{cohens_d}: Cohen's d effect size
#'   \item \code{interpretation}: Qualitative interpretation of effect size
#' }
#'
#' @details
#' Welch’s t-test is used by default, which does not assume equal variances.
#' Cohen’s d is computed using the pooled standard deviation for comparability
#' with conventional benchmarks. Group ordering follows the factor level order
#' of the grouping variable.
#'
#' @examples
#' set.seed(123)
#'
#' data_t <- data.frame(
#'   group = rep(c("CBT", "Psychodynamic"), each = 30),
#'   score = c(
#'     rnorm(30, mean = 18, sd = 4),
#'     rnorm(30, mean = 21, sd = 4)
#'   )
#' )
#'
#' make_independent_t_test_table(
#'   data = data_t,
#'   outcome = "score",
#'   group = "group"
#' )
#' @importFrom stats sd
#' @export
make_independent_t_test_table <- function(data, outcome, group) {
  # Extract variables
  y <- data[[outcome]]
  g <- data[[group]]

  if (length(unique(g)) != 2) {
    stop("Grouping variable must have exactly two levels")
  }

  # Run Welch t-test
  t_obj <- t.test(y ~ g)

  # Group stats
  grp <- split(y, g)
  m1 <- mean(grp[[1]], na.rm = TRUE)
  m2 <- mean(grp[[2]], na.rm = TRUE)
  s1 <- sd(grp[[1]], na.rm = TRUE)
  s2 <- sd(grp[[2]], na.rm = TRUE)
  n1 <- length(grp[[1]])
  n2 <- length(grp[[2]])

  # Pooled SD
  sd_pooled <- sqrt(
    ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) /
      (n1 + n2 - 2)
  )

  # Cohen's d
  d <- (m1 - m2) / sd_pooled

  interpretation <- cut(
    abs(d),
    breaks = c(-Inf, 0.2, 0.5, 0.8, Inf),
    labels = c("Negligible", "Small", "Medium", "Large")
  )

  data.frame(
    test = "Independent t-test",
    group1 = names(grp)[1],
    group2 = names(grp)[2],
    mean_diff = round(m1 - m2, 3),
    t_value = round(unname(t_obj$statistic), 3),
    df = round(unname(t_obj$parameter), 2),
    p_value = t_obj$p.value,
    n_group1 = n1,
    n_group2 = n2,
    cohens_d = round(d, 3),
    interpretation = as.character(interpretation),
    stringsAsFactors = FALSE
  )
}
