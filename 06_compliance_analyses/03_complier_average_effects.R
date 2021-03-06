# All blocks (N = 16) -----------------------------------------------------

main_complier_models <-
  map2(primary_outcomes,
       primary_covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = if (y == "Yes") get_covariates(x, selected_covariates),
           data = filter(el_imputed, complier_m == 1),
           cluster = "block_id", 
           se_type = "wild",
           studentized = TRUE,
           sims = sims
         )
       })


# Make tables -------------------------------------------------------------

sink("08_memo/tables/compliers_ipv.tex")
texreg(
  lapply(main_complier_models, get, x = "fit"),
  custom.model.names = c("IPV", "IPV", "Physical/Sexual", "Physical/Sexual", "Emotional", "Emotional"),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(main_complier_models, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(main_complier_models, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Bootstrap $p$-value" = specd(sapply(lapply(main_complier_models, get, x = "bs"), get, x = "boot.p"), 3),
    "Covariates" = paste0("\\textrm{", primary_covariates, "}"),
    "Clusters" = c(16, 16, 16, 16, 16, 16)
  ),  
  reorder.gof = c(1, 2, 3, 5, 4),
  custom.gof.names = c("Adj. R$^2$", "Observations"),
  omit.coef = "_c",
  digits = 3,
  include.ci = FALSE,
  table = FALSE,
  include.rsquared = FALSE,
  include.rmse = FALSE,
  booktabs = TRUE, 
  use.packages = FALSE,
  custom.note = "\\parbox{\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Estimates of the complier average treatment effects of Modern Man mobile 
       messaging program on pre-registered primary outcomes using adjusted regression 
       specification based on the Lin 2013 estimator with CR2 cluster-robust 
       standard errors in parentheses. Columns 1 and 2 are a composite index of 
       acts of intimate partner violence. Columns 3 and 4 are a composite index of acts
       of physical violence. Columns 5 and 6 are a composite index of acts of sexual violence.
       All indices were constructed using the first factor from IRT models of subitems. 
       Bootstrap $p$-value estimated using 10,000 replicates of wild cluster bootstrap-$t$. \\\\ %stars.}"
) %>% print()
sink()


