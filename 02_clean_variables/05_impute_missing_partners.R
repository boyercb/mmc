if (run_imputations) {
  el_imputed <- 
    mice(
      data = el_imputed,
      method = "cart",
      m = 1,
      seed = 97645
    )
  
  el_imputed <- complete(el_imputed)
  
  write.csv(el_imputed, get_data("el_imputed.csv"), row.names = FALSE, na = "")
  
} else {
  
  el_imputed <- read.csv(get_data("el_imputed.csv"), stringsAsFactors = FALSE)
  
}