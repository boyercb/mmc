el_imputed %>% 
  select(
    treatment,
    complier_m,
    starts_with("mmc_"),
    
  ) %>%
  select_if(is.numeric) %>%
  group_by(treatment) %>%
  summarise_all(mean, na.rm = T) %>% 
  gather(key, value, -treatment) %>%
  spread(treatment, value, sep = "_") %>%
  mutate(
    key = case_when()
  )


# score,
# steps,
# 