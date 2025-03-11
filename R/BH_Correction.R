#' @title Apply Benjamini-Hochberg Correction
#' @description Adjusts p-values using the BH method.
#' @param data A data frame containing a column of p-values.
#' @param pval_col The name of the column containing p-values (default `"p.value.fisher"`).
#' @return The input data frame with an added column `bh_adjusted_p`.
#' @export
apply_bh_correction <- function(data, pval_col = "p.value.fisher") {
  data %>%
    mutate(bh_adjusted_p = p.adjust(!!sym(pval_col), method = "BH"))
}