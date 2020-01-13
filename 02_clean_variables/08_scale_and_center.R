# create centered versions of pre-treatment variables
el[, paste0(names(el)[names(el) %in% invariant_covariates], "_c")] <- 
  scale(el[, names(el) %in% invariant_covariates], scale = FALSE)

el_imputed[, paste0(names(el_imputed)[names(el_imputed) %in% invariant_covariates], "_c")] <- 
  scale(el_imputed[, names(el_imputed) %in% invariant_covariates], scale = FALSE)

pooled_el[, paste0(names(pooled_el)[names(pooled_el) %in% invariant_covariates], "_c")] <- 
  scale(pooled_el[, names(pooled_el) %in% invariant_covariates], scale = FALSE)


# TO ADD: standardize indices to control mean and sd?