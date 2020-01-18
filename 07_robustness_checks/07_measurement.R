# Randomized blocks only (N = 10) -----------------------------------------

demand_effects_outcomes <- rep(demand_outcomes, each = 2)
demand_effects_covariates <- rep(c("Yes", "No"), length(demand_outcomes))

demand_effects_models <-
  map2(demand_effects_outcomes,
       demand_effects_covariates,
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

sink("08_memo/tables/demand.tex")
texreg(
  lapply(demand_effects_models, get, x = "fit"),
  custom.model.names = c("\\shortstack{Donated to\\\\charity}",
                         "\\shortstack{Donated to\\\\charity}",
                         "\\shortstack{Lend to\\\\neighbor}",
                         "\\shortstack{Lend to\\\\neighbor}",
                         "\\shortstack{Donated to\\\\charity}",
                         "\\shortstack{Donated to\\\\charity}",
                         "\\shortstack{Lend to\\\\neighbor}",
                         "\\shortstack{Lend to\\\\neighbor}"),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(demand_effects_models, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(demand_effects_models, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Bootstrap $p$-value" = specd(sapply(lapply(demand_effects_models, get, x = "bs"), get, x = "boot.p"), 3),
    "Respondent" = c("Man", "Man", "Man", "Man", "Woman", "Woman", "Woman", "Woman"),
    "Covariates" = paste0("\\textrm{", demand_effects_covariates, "}"),
    "Clusters" = c(16, 16, 16, 16, 16, 16, 16, 16)
  ),  
  reorder.gof = c(1, 2, 3, 4, 6, 5),
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
       messaging program on experimenter demand effects questions using adjusted regression 
       specification based on the Lin 2013 estimator with CR2 cluster-robust
       standard errors in parentheses. Columns 1 and 2 and 5 and 6 are an indicator of how often the
       respondent reports donating to charity/an international NGO. Columns 3 and 4 and 7 and 8 are 
       an indicator of how often the respondent reports lending money or help to a neighbor.
       Bootstrap $p$-value estimated using 10,000 replicates of wild cluster bootstrap-$t$. \\\\ %stars.}"
) %>% print()
sink()


