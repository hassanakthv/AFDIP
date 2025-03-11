
#' Calculate Fisher's Combined p-value
#'
#' This function takes a data frame containing p-values and computes Fisher's combined p-value
#' for each unique gene using the `sumlog()` function from the `metap` package.
#'
#' @param data A data frame containing at least two columns: "Gene.names" and a column with p-values.
#' @param pval_col A string indicating the name of the column containing p-values. Default is `"p.value"`.
#'
#' @return A data frame with `Gene.names`, Fisher's combined p-value, and the number of peptides per gene.
#' @export
#'
#' @examples
#' df <- data.frame(Gene.names = rep(c("Gene1", "Gene2"), each = 3),
#'                  p.value = c(0.01, 0.02, 0.05, 0.001, 0.02, 0.03))
#' calculate_fisher_pvalues(df, "p.value")
#'
calculate_fisher_pvalues <- function(data, pval_col = "p.value") {
  data %>%
    group_by(Gene.names) %>%
    summarise(
      p.value.fisher = ifelse(n() == 1, first(!!sym(pval_col)), sumlog(!!sym(pval_col))$p),
      n = n()
    ) %>%
    ungroup()
}

