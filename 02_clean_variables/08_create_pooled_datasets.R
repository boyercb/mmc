pooled_outcome_list <- 
  c(
    "n", 
    violence_outcomes,
    pre_violence_outcomes,
    pre_violence_outcomes_c,
    violence_subitems,
    violence_freq_subitems,
    pre_violence_subitems
  )

pooled_el <-
  el %>%
  group_by(block_m, treatment, manual_block) %>%
  add_count() %>%
  summarise_at(pooled_outcome_list, mean, na.rm = TRUE)