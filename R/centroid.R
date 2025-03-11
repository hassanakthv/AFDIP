
## Centroid function
Centroid <- function(x, func) {
  t_func <- tidy(func)
  poly_coeffs <- as.numeric(t_func[, 2])
  poly_function <- function(x) sum(poly_coeffs * x^(0:(length(poly_coeffs) - 1)))
  poly_function_x <- function(x) x * poly_function(x)
  
  A <- integrate(poly_function, min(x), max(x))$value
  x_centroid <- integrate(poly_function_x, min(x), max(x))$value / A
  return(x_centroid)
}