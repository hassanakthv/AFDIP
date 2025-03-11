
## Standardize function
standardize <- function(z) {
  rowmed <- apply(z, 1, median, na.rm = TRUE)
  rowmad <- apply(z, 1, mad, na.rm = TRUE)  # Median absolute deviation
  return(sweep(sweep(z, 1, rowmed, "-"), 1, rowmad, "/"))
}
