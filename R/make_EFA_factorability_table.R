#' Check EFA Factorability and Return Summary Table
#'
#' This function calculates the overall Kaiser-Meyer-Olkin (KMO) MSA,
#' Bartlett's Test of Sphericity, and the Determinant of the correlation matrix.
#'
#' @param data A data frame containing the variables.
#' @param items A character vector of column names to be included in the analysis.
#' @param scale_name Character. A label for the scale (e.g., "GLPFS"). Defaults to "Overall".
#'
#' @return A data frame with one row containing the scale-level factorability diagnostics.
#'
#' @importFrom psych KMO cortest.bartlett
#' @export
make_EFA_factorability_table <- function(data, items, scale_name = "Overall") {

  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")

  # Ensure items exist in the data
  missing_items <- items[!items %in% colnames(data)]
  if (length(missing_items) > 0) {
    stop(paste("The following items were not found in the data:", paste(missing_items, collapse = ", ")))
  }

  # Subset data
  df_subset <- data[, items, drop = FALSE]

  # Correlation Matrix and Determinant
  cor_mat <- stats::cor(df_subset, use = "pairwise.complete.obs")


  # MODIFIED: Determinant formatting
  # Using 5 decimal places instead of scientific notation for readability
  det_val <- det(cor_mat)

  # KMO and Bartlett
  kmo_results <- psych::KMO(df_subset)
  bt_results <- psych::cortest.bartlett(df_subset)

  # Format P-Value Tier
  p_val <- bt_results$p.value
  p_tier <- ifelse(p_val < .001, "< .001",
                   ifelse(p_val < .01, "< .01",
                          ifelse(p_val < .05, "< .05",
                                 round(p_val, 3))))

  # Formatting results
  results <- data.frame(
    Scale          = scale_name,
    n_items        = length(items),
    KMO_Overall    = round(kmo_results$MSA, 3),
    Determinant    = format(det_val, scientific = TRUE, digits = 3),
    Bartlett_Chisq = round(bt_results$chisq, 2),
    df             = bt_results$df,
    p_value        = p_tier,
    stringsAsFactors = FALSE
  )

  # Check for Singularity/Multicollinearity
  if (det_val < 0.00001) {
    message("WARNING: Determinant is very low. Check for highly redundant items (r > 0.9).")
  }

  # Professional Warning
  message("Note: KMO and Bartlett's test only confirm factorability.")
  message("A high KMO_Overall can hide individual items with low MSAi.")
  message("Please inspect factor loadings and individual MSAi values for confirmation.")

  return(results)
}
