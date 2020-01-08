# All blocks (N = 16) -----------------------------------------------------

primary_outcomes <- rep(violence_outcomes, each = 2)
primary_covariates <- rep(c("Yes", "No"), length(violence_outcomes))

main_models <-
  map2(primary_outcomes,
       primary_covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = if (y == "Yes") get_covariates(x, selected_covariates),
           data = el_imputed,
           cluster = "block_id", 
           se_type = "wild",
           sims = sims
         )
       })


# Make tables -------------------------------------------------------------

sink("08_memo/tables/ipv.tex")
texreg(
  lapply(main_models, get, x = "fit"),
  custom.model.names = c("IPV", "IPV", "Physical/Sexual", "Physical/Sexual", "Emotional", "Emotional"),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(main_models, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(main_models, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", primary_covariates, "}"),
    "Clusters" = c(16, 16, 16, 16, 16, 16)
  ),  
  reorder.gof = c(1, 2, 4, 3),
  custom.gof.names = c("Adj. R$^2$", "Observations"),
  omit.coef = "_[mw]",
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


