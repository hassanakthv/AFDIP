## Normalization of samples based on the best fitted model
Normalize_Based_On_Model <- function(res, degree = 3) {
  ss <- colSums(res[, 12:59])
  batch_fit <- seq(1, 48, 8)
  Adj <- numeric(48)
  
  for (i in batch_fit) {
    x <- 1:8
    y <- ss[i:(i + 7)]
    fitmodel <- lm(y ~ poly(x, degree))
    Adj[i:(i + 7)] <- predict(fitmodel, newdata = data.frame(x = x))
  }
  
  Adjj <- Adj / ss
  res_norm <- res[, 12:59] * Adjj
  res_norm <- cbind(res[, 1:11], res_norm)
  
  return(res_norm)
}

Compute_Shift_Analysis <- function(res_norm) {
  xx <- 1:48
  result <- data.frame()
  rep_res <- data.frame()
  
  for (i in 1:nrow(res_norm)) {
    test <- as.numeric(res_norm[i, 12:59])
    tmt_i <- seq(1, 48, 16)
    
    tt_df <- do.call(rbind, lapply(tmt_i, function(j) {
      tmt_end <- j + 15
      tt_test <- data.frame(
        Hour = rep(1:8, 2),
        Sample = rep(c("Control", "Treated"), each = 8),
        Intensity = test[j:tmt_end]
      )
      
      Control_Ratio <- tt_test %>% filter(Sample == "Control") %>% pull(Intensity) / max(tt_test %>% filter(Sample == "Control") %>% pull(Intensity))
      Treat_Ratio <- tt_test %>% filter(Sample == "Treated") %>% pull(Intensity) / max(tt_test %>% filter(Sample == "Treated") %>% pull(Intensity))
      
      data.frame(
        Hour = rep(1:8, 2),
        Sample = rep(c("Control", "Treated"), each = 8),
        Ratio = c(Control_Ratio, Treat_Ratio),
        Rep = j
      )
    }))
    
    model_results <- lapply(unique(tt_df$Sample), function(ns) {
      model <- try(lm(formula = Ratio ~ poly(Hour, 4, raw = TRUE), data = filter(tt_df, Sample == ns)), silent = TRUE)
      if (!inherits(model, "try-error")) {
        return(data.frame(
          Sample = ns,
          T_centroid = Centroid(x = 1:8, func = model),
          R2 = rSquared(model, filter(tt_df, Sample == ns)$Ratio)
        ))
      } else {
        return(data.frame(Sample = ns, T_centroid = 0, R2 = 0))
      }
    })
    
    model_df <- do.call(rbind, model_results)
    names(model_df) <- paste0(names(model_df), "__", model_df$Sample)
    model_df$Sample <- NULL
    
    result <- rbind(result, cbind(res_norm[i, 1:11], model_df))
  }
  
  return(result)
}
