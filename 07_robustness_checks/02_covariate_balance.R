covariate_means <- 
  el_imputed %>%
  select(treatment, invariant_covariates) %>%
  gather(covariate, value, -treatment) %>%
  group_by(covariate, treatment) %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>%
  spread(treatment, mean, sep = "_")

balance_models <-
  map(invariant_covariates,
      function (x) {
        main_estimator(
          outcome = x,
          data = el_imputed,
          cluster = "block_id",
          se_type = "wild",
          sims = sims
        )
      })

balance_results <- lapply(balance_models, get, x = "robust")

balance_results <- data.frame(
  covariate = invariant_covariates,
  diff = sapply(balance_results, "[", 2, 1),
  p.value = sapply(balance_results, "[", 2, 4),
  stringsAsFactors = FALSE
)

balance_results <- left_join(covariate_means, balance_results, by = "covariate")

balance_results$covariate <- paste0("\\texttt{", gsub("_", "\\\\_", balance_results$covariate), "}")
sink("08_memo/tables/balance_results.tex")
kable(
  x = balance_results,
  format = "latex",
  col.names = c("Covariate", "MMC", "Control", "(1) - (2)", "$p$-value"),
  digits = 2,
  escape = FALSE,
  align = "lcccc",
  longtable = TRUE,
  booktabs = TRUE
) %>% 
  kable_styling(latex_options = c("HOLD_position", "repeat_header")) %>%
  add_header_above(c(" ", "(1)", "(2)", "Diff", " "),line = F)
sink()