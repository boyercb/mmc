complier_covariates <- c(
  invariant_covariates
)

covariate_means <- 
  el_imputed %>%
  filter(complier_m == 1) %>%
  select(treatment, complier_covariates) %>%
  gather(covariate, value, -treatment) %>%
  group_by(covariate, treatment) %>%
  summarise(mean = mean(value)) %>%
  spread(treatment, mean, sep = "_")

complier_balance_models <-
  map(complier_covariates,
      function (x) {
        main_estimator(
          outcome = x,
          data = filter(el_imputed, complier_m == 1),
          cluster = "block_id",
          se_type = "wild",
          sims = sims
        )
      })

complier_balance_results <- lapply(complier_balance_models, get, x = "robust")

complier_balance_results <- data.frame(
  covariate = complier_covariates,
  diff = sapply(complier_balance_results, "[", 2, 1),
  p.value = sapply(complier_balance_results, "[", 2, 4),
  stringsAsFactors = FALSE
)

complier_balance_results <- left_join(covariate_means, complier_balance_results, by = "covariate")

complier_balance_results$covariate <- paste0("\\texttt{", gsub("_", "\\\\_", complier_balance_results$covariate), "}")
sink("08_memo/tables/complier_balance_results")
kable(
  x = complier_balance_results,
  format = "latex",
  col.names = c("Covariate", "MMC", "Control", "(1) - (2)", "$p$-value"),
  digits = 2,
  escape = FALSE,
  align = "lcccc",
  booktabs = TRUE
) %>% 
  kable_styling(latex_options = "HOLD_position") %>%
  add_header_above(c(" ", "(1)", "(2)", "Diff", " "),line = F)
sink()