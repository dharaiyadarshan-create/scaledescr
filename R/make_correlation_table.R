#' Make Correlation Table
#'
#' Computes a correlation matrix with inline significance stars.
#'
#' @param data A data frame.
#' @param vars Character vector of variable names. If NULL, all columns are used.
#' @param method Correlation method: "pearson" (default), "spearman", or "kendall".
#' @importFrom stats cor.test
#' @return A character data frame with correlations and significance stars.
#' @export
make_correlation_table <- function(data, vars = NULL, method = "pearson") {

  if (is.null(vars)) vars <- names(data)
  df <- data[, vars, drop = FALSE]

  n <- ncol(df)
  cor_mat <- matrix(NA_character_, nrow = n, ncol = n)
  rownames(cor_mat) <- vars
  colnames(cor_mat) <- vars

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        cor_mat[i, j] <- "1.00"
      } else {
        test <- cor.test(df[[i]], df[[j]], method = method, exact = FALSE)
        r <- sprintf("%.2f", test$estimate)
        p <- test$p.value
        stars <- ifelse(p < .001, "***",
                        ifelse(p < .01,  "**",
                               ifelse(p < .05,  "*", "")))
        cor_mat[i, j] <- paste0(r, stars)
      }
    }
  }

  result <- as.data.frame(cor_mat, stringsAsFactors = FALSE)
  attr(result, "method") <- method
  attr(result, "note")   <- "* p<.05  ** p<.01  *** p<.001"
  message("Note: * p<.05  ** p<.01  *** p<.001  (method: ", method, ")")
  return(result)
}
