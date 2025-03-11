generate_volcano_plot <- function(data, x_col = "Mean_Mean_shift", y_col = "p.value.fisher",
                                  log_threshold = 0.05, shift_cutoff = 0.5, output_file = "volcano_plot.png") {
  
  data <- data %>%
    mutate(neg_log10_p = -log10(!!sym(y_col)),
           significant = abs(!!sym(x_col)) > shift_cutoff & neg_log10_p > -log10(log_threshold))
  
  volcano_plot <- ggplot(data, aes(x = !!sym(x_col), y = neg_log10_p)) +
    geom_point(aes(color = significant), alpha = 0.6) +
    geom_hline(yintercept = -log10(log_threshold), linetype = "dashed", color = "red") +
    geom_vline(xintercept = c(-shift_cutoff, shift_cutoff), linetype = "dashed", color = "red") +
    scale_color_manual(values = c("black", "red")) +
    labs(x = "Mean CoG Shift (Protein)", y = "-log10(p-value)", title = "Volcano Plot - Protein Level MTX") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey90")) +
    geom_text_repel(data = subset(data, significant == TRUE),
                    aes(label = Gene.names),
                    size = 3, box.padding = 0.5, point.padding = 0.1, force = 1, max.overlaps = Inf)
  
  print(volcano_plot)
  ggsave(output_file, volcano_plot, width = 6, height = 5, dpi = 300)
}