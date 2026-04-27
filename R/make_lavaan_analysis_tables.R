#' Comprehensive Lavaan Regression Analysis
#'
#' @description Fits a lavaan model and returns a list containing the regression
#' coefficients table and the global model fit indices.
#'
#' @param data A dataframe.
#' @param dvs A character vector of one or more Dependent Variables.
#' @param ivs A character vector of one or more Independent Variables.
#' @param standardize Logical. If TRUE, standardizes data before analysis.
#' @importFrom lavaan sem parameterEstimates standardizedSolution fitMeasures lavInspect
#' @return A list with two dataframes: 'Regression' and 'Fit'.
#' @export
make_lavaan_analysis_tables <- function(data, dvs, ivs, standardize = FALSE) {

  # 1. PREP & FIT
  data_sub <- data[, c(dvs, ivs)]
  if(standardize) {
    data_sub <- as.data.frame(lapply(data_sub, scale))
  }

  formula_string <- paste(paste(dvs, collapse = " + "), "~", paste(ivs, collapse = " + "))
  fit <- lavaan::sem(formula_string, data = data_sub, fixed.x = FALSE, missing = "fiml")

  # 2. BUILD REGRESSION DATAFRAME
  est <- lavaan::parameterEstimates(fit)
  std <- lavaan::standardizedSolution(fit)
  reg_df <- est[est$op == "~", c("lhs", "rhs", "est", "se", "z", "pvalue")]
  reg_df$Beta <- std$est.std[std$op == "~"]

  colnames(reg_df) <- c("Outcome", "Predictor", "B", "SE", "z", "p", "Beta")
  reg_df$p <- ifelse(reg_df$p < .001, "<.001", sprintf("%.3f", reg_df$p))
  num_cols <- c("B", "SE", "z", "Beta")
  reg_df[num_cols] <- lapply(reg_df[num_cols], round, 3)
  rownames(reg_df) <- NULL

  # 3. BUILD FIT DATAFRAME
  indices <- lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
  fit_df <- data.frame(
    Index = c("CFI", "TLI", "RMSEA", "SRMR"),
    Value = round(as.numeric(indices), 3),
    Threshold = c("> 0.90", "> 0.90", "< 0.08", "< 0.08")
  )

  # 4. EXTRACT R-SQUARED
  r2_vals <- round(lavaan::lavInspect(fit, "rsquare"), 3)

  # --- CONSOLE PRINTING ---
  cat("\n==============================\n")
  cat("  LAVAAN REGRESSION RESULTS   \n")
  cat("==============================\n")
  print(reg_df)

  cat("\n--- Model R-Squared ---\n")
  print(r2_vals)

  cat("\n--- Global Model Fit ---\n")
  print(fit_df)

  # --- ACCESS NOTE ---
  cat("\n------------------------------------------------------------\n")
  cat("NOTE: To access these tables later, save the output to a variable\n")
  cat("Example: res <- make_lavaan_analysis_tables(...)\n")
  cat("Then use:\n")
  cat(" - res$Regression  : For the coefficients table\n")
  cat(" - res$Fit         : For the model fit indices\n")
  cat(" - res$R2          : For the R-squared values\n")
  cat("------------------------------------------------------------\n")

  # Return everything in a clean list
  return(invisible(list(Regression = reg_df, Fit = fit_df, R2 = r2_vals)))
}
