#' Comprehensive Lavaan Mediation Analysis with Optional Covariances
#'
#' @description Fits a lavaan mediation model with one or more mediators and
#' optionally allows covariances between IVs, mediators, and DVs.
#' Returns direct effects, indirect effects, total effects, correlations, and fit indices.
#'
#' @param data A dataframe.
#' @param dvs A character vector of one or more Dependent Variables.
#' @param ivs A character vector of one or more Independent Variables.
#' @param mediators A character vector of one or more Mediator Variables.
#' @param bootstrap Integer. Number of bootstrap samples. Default 1000.
#' @param standardize Logical. If TRUE, standardizes data before analysis.
#' @param correlate_ivs Logical. If TRUE, adds covariances between IVs.
#' @param correlate_mediators Logical. If TRUE, adds covariances between mediators.
#' @param correlate_dvs Logical. If TRUE, adds covariances between DVs.
#' @importFrom lavaan sem parameterEstimates standardizedSolution fitMeasures lavInspect
#' @importFrom utils combn
#' @return Invisibly returns a list: Direct, Indirect, Total, Correlations, Fit, R2
#' @export
make_lavaan_mediation_tables <- function(data, dvs, ivs, mediators,
                                         bootstrap = 1000,
                                         standardize = FALSE,
                                         correlate_ivs = FALSE,
                                         correlate_mediators = FALSE,
                                         correlate_dvs = FALSE) {

  # 1. PREP
  all_vars <- unique(c(dvs, ivs, mediators))
  data_sub <- data[, all_vars]
  if (standardize) {
    data_sub <- as.data.frame(lapply(data_sub, scale))
  }

  # 2. BUILD MODEL STRING
  model_lines <- c()
  indirect_labels <- c()
  total_labels <- c()
  label_tracker <- list()

  # a-paths: IV -> Mediator
  for (m in mediators) {
    for (x in ivs) {
      a_label <- paste0("a_", m, "_", x)
      model_lines <- c(model_lines, paste0(m, " ~ ", a_label, "*", x))
      label_tracker[[paste0(m, "_", x)]] <- a_label
    }
  }

  # b-paths and c-paths: Mediator -> DV, IV -> DV
  for (y in dvs) {
    for (m in mediators) {
      b_label <- paste0("b_", y, "_", m)
      model_lines <- c(model_lines, paste0(y, " ~ ", b_label, "*", m))
    }
    for (x in ivs) {
      c_label <- paste0("c_", y, "_", x)
      model_lines <- c(model_lines, paste0(y, " ~ ", c_label, "*", x))

      # Indirect effects
      for (m in mediators) {
        a_label <- label_tracker[[paste0(m, "_", x)]]
        b_label <- paste0("b_", y, "_", m)
        ind_label <- paste0("ind_", x, "_", m, "_", y)
        indirect_labels <- c(indirect_labels, ind_label)
        model_lines <- c(model_lines,
                         paste0(ind_label, " := ", a_label, " * ", b_label))
      }

      # Total effects
      tot_label <- paste0("tot_", x, "_", y)
      ind_sum_parts <- paste(
        sapply(mediators, function(m)
          paste0("a_", m, "_", x, " * ", "b_", y, "_", m)),
        collapse = " + "
      )
      model_lines <- c(model_lines,
                       paste0(tot_label, " := c_", y, "_", x, " + ", ind_sum_parts))
      total_labels <- c(total_labels, tot_label)
    }
  }

  # 3. COVARIANCE LINES
  cov_pairs <- function(vars) {
    if (length(vars) < 2) return(c())
    pairs <- combn(vars, 2, simplify = FALSE)
    sapply(pairs, function(p) paste0(p[1], " ~~ ", p[2]))
  }

  if (correlate_ivs        && length(ivs)       > 1) model_lines <- c(model_lines, cov_pairs(ivs))
  if (correlate_mediators  && length(mediators)  > 1) model_lines <- c(model_lines, cov_pairs(mediators))
  if (correlate_dvs        && length(dvs)        > 1) model_lines <- c(model_lines, cov_pairs(dvs))

  model_string <- paste(unique(model_lines), collapse = "\n")

  # 4. FIT MODEL
  fit <- lavaan::sem(model_string, data = data_sub,
                     se = "bootstrap", bootstrap = bootstrap)

  # 5. EXTRACT
  pe  <- lavaan::parameterEstimates(fit, boot.ci.type = "perc")
  std <- lavaan::standardizedSolution(fit)

  # Helper
  build_table <- function(op_filter = NULL, label_filter = NULL) {
    if (!is.null(op_filter)) {
      rows     <- pe[pe$op == op_filter, ]
      std_rows <- std[std$op == op_filter, ]
    } else {
      rows     <- pe[pe$label %in% label_filter, ]
      std_rows <- std[std$label %in% label_filter, ]
    }
    if (nrow(rows) == 0) return(NULL)
    df <- data.frame(
      Outcome   = rows$lhs,
      Predictor = rows$rhs,
      B         = round(rows$est, 3),
      SE        = round(rows$se, 3),
      z         = round(rows$z, 3),
      p         = ifelse(rows$pvalue < .001, "<.001", sprintf("%.3f", rows$pvalue)),
      CI_lower  = round(rows$ci.lower, 3),
      CI_upper  = round(rows$ci.upper, 3),
      stringsAsFactors = FALSE
    )
    if (nrow(std_rows) == nrow(df)) df$Beta <- round(std_rows$est.std, 3)
    rownames(df) <- NULL
    df
  }

  # Direct effects
  direct_df <- build_table(op_filter = "~")

  # Indirect effects
  indirect_df <- build_table(label_filter = indirect_labels)
  if (!is.null(indirect_df)) {
    indirect_df$Path      <- gsub("ind_", "", indirect_df$Outcome)
    indirect_df$Outcome   <- NULL
    indirect_df$Predictor <- NULL
  }

  # Total effects
  total_df <- build_table(label_filter = total_labels)

  # 6. CORRELATIONS TABLE (from ~~ rows, excluding variances i.e. lhs != rhs)
  cov_rows     <- pe[pe$op == "~~" & pe$lhs != pe$rhs, ]
  cov_std_rows <- std[std$op == "~~" & std$lhs != std$rhs, ]

  if (nrow(cov_rows) > 0) {
    corr_df <- data.frame(
      Var1     = cov_rows$lhs,
      Var2     = cov_rows$rhs,
      r        = round(cov_std_rows$est.std, 3),
      B_cov    = round(cov_rows$est, 3),
      SE       = round(cov_rows$se, 3),
      p        = ifelse(cov_rows$pvalue < .001, "<.001", sprintf("%.3f", cov_rows$pvalue)),
      CI_lower = round(cov_rows$ci.lower, 3),
      CI_upper = round(cov_rows$ci.upper, 3),
      stringsAsFactors = FALSE
    )
    rownames(corr_df) <- NULL
  } else {
    corr_df <- NULL
  }

  # Fit indices
  indices <- lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
  fit_df <- data.frame(
    Index     = c("CFI", "TLI", "RMSEA", "SRMR"),
    Value     = round(as.numeric(indices), 3),
    Threshold = c("> 0.90", "> 0.90", "< 0.08", "< 0.08"),
    stringsAsFactors = FALSE
  )

  # R-squared
  r2_vals <- round(lavaan::lavInspect(fit, "rsquare"), 3)

  # 7. CONSOLE PRINT
  cat("\n==========================================\n")
  cat("       LAVAAN MEDIATION RESULTS          \n")
  cat("==========================================\n")

  cat("\n--- Direct Effects (all ~ paths) ---\n")
  print(direct_df)

  cat("\n--- Indirect Effects (bootstrapped CIs) ---\n")
  if (!is.null(indirect_df)) print(indirect_df) else cat("None computed.\n")

  cat("\n--- Total Effects ---\n")
  if (!is.null(total_df)) print(total_df) else cat("None computed.\n")

  cat("\n--- Covariances / Correlations ---\n")
  if (!is.null(corr_df)) print(corr_df) else cat("No covariances requested.\n")

  cat("\n--- Model R-Squared ---\n")
  print(r2_vals)

  cat("\n--- Global Model Fit ---\n")
  print(fit_df)

  cat("\n------------------------------------------------------------\n")
  cat("NOTE: Save output to access tables:\n")
  cat("  res <- make_lavaan_mediation_tables(...)\n")
  cat("  res$Direct       : Direct effects\n")
  cat("  res$Indirect     : Indirect effects with bootstrap CIs\n")
  cat("  res$Total        : Total effects\n")
  cat("  res$Correlations : Covariances/correlations between variables\n")
  cat("  res$Fit          : Model fit indices\n")
  cat("  res$R2           : R-squared values\n")
  cat("------------------------------------------------------------\n")

  return(invisible(list(
    Direct       = direct_df,
    Indirect     = indirect_df,
    Total        = total_df,
    Correlations = corr_df,
    Fit          = fit_df,
    R2           = r2_vals
  )))
}
