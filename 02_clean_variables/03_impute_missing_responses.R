
# new strategy impute men's data first 
# then impute women's data using completed men's data
if (run_imputations) {
  elm_imputed <- 
    mice(
      data = subset(elm, inrelationship == 1),
      method = "cart",
      m = 1,
      seed = 97645
    )
  
  elm_imputed <- complete(elm_imputed)
  
  write.csv(elm_imputed, get_data("elm_imputed.csv"), row.names = FALSE, na = "")
  
  elw_imputed <- 
    mice(
      data = subset(elw, currently_w_man == 1),
      method = "cart",
      m = 1,
      seed = 97645
    )
  
  elw_imputed <- complete(elw_imputed)
  
  write.csv(elw_imputed, get_data("elw_imputed.csv"), row.names = FALSE, na = "")
  
  bl_imputed <- 
    mice(
      data = bl,
      method = "cart",
      m = 1,
      seed = 97645
    )
  
  bl_imputed <- complete(bl_imputed)
  
  write.csv(bl_imputed, get_data("bl_imputed.csv"), row.names = FALSE, na = "")
  
} else {
  elm_imputed <- read.csv(get_data("elm_imputed.csv"), stringsAsFactors = FALSE)
  elw_imputed <- read.csv(get_data("elw_imputed.csv"), stringsAsFactors = FALSE)
  bl_imputed <- read.csv(get_data("bl_imputed.csv"), stringsAsFactors = FALSE)
}




