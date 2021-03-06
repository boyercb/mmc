# All blocks (N = 16) -----------------------------------------------------

primary_outcomes <- rep(violence_outcomes, each = 2)
primary_covariates <- rep(c("Yes", "No"), length(violence_outcomes))


main_pooled_models <-
  map2(primary_outcomes,
       primary_covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = if (y == "Yes") get_covariates(x, selected_pooled_covariates),
           data = pooled_el,
           se_type = "wild",
           studentized = TRUE,
           sims = sims
         )
       })


pooled_p1 <- plot_treatment_effects(
  fit = main_models[[2]]$fit,
  outcome = "ipv_control_2pl_irt_index_w",
  data = pooled_el,
  type = "cluster",
  color = "block_rand",
  color_name = "Manually assigned",
  color_values = c("black", "red"),
  color_labels = c("No", "Yes"),
  title = "",
  ylabel = "IPV Index"
)

pdf("08_memo/figures/ipv_pooled.pdf", width = 9, height = 6)
pooled_p1 %>% print()
dev.off()


# Make tables -------------------------------------------------------------

sink("08_memo/tables/ipv_pooled.tex")
texreg(
  lapply(main_pooled_models, get, x = "fit"),
  custom.model.names = c("IPV",
                         "IPV",
                         "Physical/Sexual",
                         "Physical/Sexual",
                         "Emotional",
                         "Emotional"),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(main_pooled_models, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(main_pooled_models, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Bootstrap $p$-value" = specd(sapply(lapply(main_pooled_models, get, x = "bs"), get, x = "boot.p"), 3),
    "Covariates" = paste0("\\textrm{", primary_covariates, "}"),
    "Clusters" = c(16, 16, 16, 16, 16, 16)
  ),  
  reorder.gof = c(1, 2, 3, 5, 4),
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
       messaging program on pre-registered primary outcomes with data pooled at the block 
       level and using adjusted regression specification based on the Lin 2013 estimator with 
       CR2 cluster-robust standard errors in parentheses. Columns 1 and 2 are a composite 
       index of acts of intimate partner violence. Columns 3 and 4 are a composite index of acts
       of physical violence. Columns 5 and 6 are a composite index of acts of sexual violence.
       All indices were constructed using the first factor from IRT models of subitems. 
       Bootstrap $p$-value estimated using 10,000 replicates of wild cluster bootstrap-$t$. \\\\ %stars.}"
) %>% print()
sink()