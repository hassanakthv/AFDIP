merge_fisher_and_shifts <- function(fisher_data, shift_data) {
  fisher_data %>%
    left_join(shift_data, by = "Gene.names")
}
