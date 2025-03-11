#' @title Compute Mean Shift for Each Protein
#' @description Calculates the mean `Mean_Shift` for each unique protein.
#' @param peptide_data A data frame with `Gene.names` and `Mean_Shift` columns.
#' @return A data frame with each `Gene.names` and its mean `Mean_Shift`.
#' @export
compute_mean_shift <- function(peptide_data) {
  peptide_data %>%
    group_by(Gene.names) %>%
    summarise(Mean_Mean_shift = mean(Mean_Shift, na.rm = TRUE)) %>%
    ungroup()
}