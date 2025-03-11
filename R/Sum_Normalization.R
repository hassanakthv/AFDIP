
## Normalization based on the total intensity of each column
Sum_Normalization <- function(x, dff_){
  
  
  for (col in 1:ncol(x)){
    
    dff <- x[,col]/sum(x[,col], na.rm = T)
    dff_ <- bind_cols(dff_, dff)
    
  }
  
  return(dff_)
}

