## Generating name of the samples
Name_Generator <- function(xlist, rep = 3) {
  unlist(lapply(xlist, function(x) paste0(x, "_Rep", 1:rep)))
}
