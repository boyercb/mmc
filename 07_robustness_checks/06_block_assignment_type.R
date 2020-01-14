# Randomized blocks only (N = 10) -----------------------------------------


primary_outcomes <- rep(violence_outcomes, each = 2)
blocks <- rep(c(16, 10), length(violence_outcomes))

block_rand_models <-
  map2(primary_outcomes,
       blocks,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = get_covariates(x, selected_covariates),
           data = if (y == 16) el_imputed else filter(el_imputed, block_rand == 1),
           cluster = "block_id", 
           se_type = "wild",
           sims = sims
         )
       })

block_rand_X_models <-
  map(violence_outcomes,
       function (x) {
         main_estimator(
           outcome = x,
           covariates = c("block_rand", get_covariates(x, selected_covariates)),
           data = el_imputed,
           cluster = "block_id", 
           se_type = "wild",
           sims = sims
         )
       })


# Make tables -------------------------------------------------------------

sink("08_memo/tables/block_rand.tex")
texreg(
  lapply(block_rand_models, get, x = "fit"),
  custom.model.names = c("IPV",
                         "IPV",
                         "Physical/Sexual",
                         "Physical/Sexual",
                         "Emotional",
                         "Emotional"),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(block_rand_models, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(block_rand_models, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", rep("Yes", 6), "}"),
    "Clusters" = blocks
  ),  
  reorder.gof = c(1, 2, 4, 3),
  custom.gof.names = c("Adj. R$^2$", "Observations"),
  omit.coef = "_[c]",
  digits = 3,
  include.ci = FALSE,
  table = FALSE,
  include.rsquared = FALSE,
  include.rmse = FALSE,
  booktabs = TRUE,
  use.packages = FALSE,
  custom.note = "\\parbox{\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile 
       messaging program on pre-registered primary outcomes using adjusted regression 
       specification based on the Lin 2013 estimator with wild cluster bootstrap 
       standard errors in parentheses. Columns 1 and 2 are a composite index of 
       acts of intimate partner violence. Columns 3 and 4 are a composite index of acts
       of physical violence. Columns 5 and 6 are a composite index of acts of sexual violence.
       All indices were constructed using the first factor from IRT models of subitems. 
       Bootstrapped standard errors estimated using 10,000 replicates. \\\\ %stars.}"
) %>% print()
sink()


sink("08_memo/tables/block_rand_X.tex")
texreg(
  lapply(block_rand_X_models, get, x = "fit"),
  custom.model.names = c("IPV",
                         "Physical/Sexual",
                         "Emotional"),
  custom.coef.names = c("Constant", "MMC", "Min-max randomized", "MMC $\\times$ Min-max randomized"),
  override.se = lapply(lapply(block_rand_X_models, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(block_rand_X_models, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 3, 4, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", rep("Yes", 3), "}"),
    "Clusters" = c(16, 16, 16)
  ),  
  reorder.gof = c(1, 2, 4, 3),
  custom.gof.names = c("Adj. R$^2$", "Observations"),
  omit.coef = "_[c]",
  digits = 3,
  include.ci = FALSE,
  table = FALSE,
  include.rsquared = FALSE,
  include.rmse = FALSE,
  booktabs = TRUE,
  use.packages = FALSE,
  custom.note = "\\parbox{\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Estimates of treatment effect heterogeneity for the Modern Man mobile 
       messaging program based on randomization type. Estimates are from adjusted regression 
       specification based on the Lin 2013 estimator with wild cluster bootstrap 
       standard errors in parentheses. Column 1 is a composite index of 
       acts of intimate partner violence. Column 2 is a composite index of acts
       of physical violence. Column 3 is a composite index of acts of sexual violence.
       All indices were constructed using the first factor from IRT models of subitems. 
       Bootstrapped standard errors estimated using 10,000 replicates. \\\\ %stars.}"
) %>% print()
sink()


