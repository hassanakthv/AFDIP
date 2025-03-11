## Ratio and p value calculation for non-log data
rp_calc_nonlog <- function(x, rep = 3) {
  n <- seq(1, ncol(x), rep)
  mx <- as.matrix(x)
  
  res <- lapply(1:nrow(mx), function(j) {
    row_vals <- mx[j, ]
    medians <- sapply(n, function(i) median(row_vals[i:(i + rep - 1)]))
    
    ratios <- sapply(n, function(i) {
      median(row_vals[i:(i + rep - 1)]) / median(row_vals[1:rep])
    })
    
    p_values <- sapply(n, function(i) {
      t.test(row_vals[1:rep], row_vals[i:(i + rep - 1)], paired = FALSE)$p.value
    })
    
    c(row_vals, medians, ratios, p_values)
  })
  
  df_result <- as.data.frame(do.call(rbind, res))
  colnames(df_result) <- c(colnames(x), paste0(colnames(x)[n], "_median"), paste0(colnames(x)[n], "_ratio"), paste0(colnames(x)[n], "_p.value"))
  
  return(df_result)
}
