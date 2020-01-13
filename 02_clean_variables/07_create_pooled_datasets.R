pooled_outcome_list <- 
  c(
    "n", 
    outcomes,
    invariant_covariates
  )

pooled_el <-
  el_imputed %>%
  group_by(block_id, treatment, block_rand) %>%
  add_count() %>%
  summarise_at(pooled_outcome_list, mean, na.rm = TRUE)