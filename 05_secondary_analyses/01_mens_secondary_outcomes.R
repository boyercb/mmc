# All blocks (N = 16) -----------------------------------------------------

mens_secondary_outcomes <- rep(mens_outcomes, each = 2)
mens_secondary_covariates <- rep(c("Yes", "No"), length(mens_outcomes))

secondary_mens_models <-
  map2(mens_secondary_outcomes,
       mens_secondary_covariates,
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

# Create results lists ----------------------------------------------------

attitudes_results_m <- list(
  secondary_mens_models[[1]],
  secondary_mens_models[[2]],
  secondary_mens_models[[5]],
  secondary_mens_models[[6]]
)

dm_etc_results_m <- list(
  secondary_mens_models[[7]],
  secondary_mens_models[[8]],
  secondary_mens_models[[11]],
  secondary_mens_models[[12]]
)

sex_results_m <- list(
  secondary_mens_models[[3]],
  secondary_mens_models[[4]],
  secondary_mens_models[[27]],
  secondary_mens_models[[28]]
)

comm_results_m <- list(
  secondary_mens_models[[13]],
  secondary_mens_models[[14]],
  secondary_mens_models[[9]],
  secondary_mens_models[[10]],
  secondary_mens_models[[15]],
  secondary_mens_models[[16]],
  secondary_mens_models[[17]],
  secondary_mens_models[[18]]
)

conflict_results_m <- list(
  secondary_mens_models[[19]],
  secondary_mens_models[[20]],
  secondary_mens_models[[21]],
  secondary_mens_models[[22]],
  secondary_mens_models[[23]],
  secondary_mens_models[[24]],
  secondary_mens_models[[25]],
  secondary_mens_models[[26]]
)


# Make tables -------------------------------------------------------------

sink("08_memo/tables/attitudes_results_m.tex")
texreg(
  lapply(attitudes_results_m, get, x = "fit"),
  custom.model.names = c(
    "Justifies Violence",
    "Justifies Violence",
    "\\shortstack{Perceptions of \\\\ Violence Norms}",
    "\\shortstack{Perceptions of \\\\ Violence Norms}"
  ),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(attitudes_results_m, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(attitudes_results_m, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("Yes", "No", "Yes", "No"), "}"),
    "Clusters" = c(16, 16, 16, 16)
  ),
  reorder.gof = c(1, 2, 4, 3),
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
       \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile
       messaging program on secondary men's outcomes using adjusted regression
       specification based on the Lin 2013 estimator with wild cluster bootstrap
       standard errors in parentheses. Columns 1 and 2 are a composite index of
       whether man's attitudes justify use of violence against women. Columns 3 and 4
       are a composite index of men's perceptions about whether their community justifies
       the use of violence. All indices were constructed as sums of subitems coded in
       same substantive direction. Bootstrapped standard errors estimated using 10,000 replicates. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/dm_etc_results_m.tex")
texreg(
  lapply(dm_etc_results_m, get, x = "fit"),
  custom.model.names = c(
    "Decision-making",
    "Decision-making",
    "Man feels supported",
    "Man feels supported"
  ),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(dm_etc_results_m, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(dm_etc_results_m, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("Yes", "No", "Yes", "No"), "}"),
    "Clusters" = c(16, 16, 16, 16)
  ),
  reorder.gof = c(1, 2, 4, 3),
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
       \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile
       messaging program on secondary men's outcomes using adjusted regression
       specification based on the Lin 2013 estimator with wild cluster bootstrap
       standard errors in parentheses. Columns 1 and 2 are a composite index of
       women's involvement in decision-making. Columns 3 and 4 are a composite index of 
       whether men feel supported by their partner. All indices were constructed as sums of 
       subitems coded in same substantive direction. Bootstrapped standard errors estimated 
       using 10,000 replicates. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/sex_results_m.tex")
texreg(
  lapply(sex_results_m, get, x = "fit"),
  custom.model.names = c(
    "\\shortstack{Neg. response \\\\ to no consent}",
    "\\shortstack{Neg. response \\\\ to no consent}",
    "Values woman's pleasure",
    "Values woman's pleasure"
  ),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(sex_results_m, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(sex_results_m, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("Yes", "No", "Yes", "No"), "}"),
    "Clusters" = c(16, 16, 16, 16)
  ),
  reorder.gof = c(1, 2, 4, 3),
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
       \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile
       messaging program on secondary men's outcomes using adjusted regression
       specification based on the Lin 2013 estimator with wild cluster bootstrap
       standard errors in parentheses. Columns 1 and 2 are a composite index of
       whether the man justifies negative responses to woman's refusal of sex. Columns 3 and 4
       are a composite index of men's value of woman's pleasure during sex. All indices were
       constructed as sums of subitems coded insame substantive direction. 
       Bootstrapped standard errors estimated using 10,000 replicates. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/comm_results_m.tex")
texreg(
  lapply(comm_results_m, get, x = "fit"),
  custom.model.names = c(
    "\\shortstack{Discuss \\\\ relation- \\\\ ship}",
    "\\shortstack{Discuss \\\\ relation- \\\\ ship}",
    "\\shortstack{Good \\\\ talk}",
    "\\shortstack{Good \\\\ talk}",
    "\\shortstack{Freq. \\\\ good \\\\ expressions}",
    "\\shortstack{Freq. \\\\ good \\\\ expressions}",
    "\\shortstack{Enjoy \\\\ mutual \\\\ acts}",
    "\\shortstack{Enjoy \\\\ mutual \\\\ acts}"
  ),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(comm_results_m, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(comm_results_m, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"), "}"),
    "Clusters" = c(16, 16, 16, 16, 16, 16, 16, 16)
  ),
  reorder.gof = c(1, 2, 4, 3),
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
       \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile
       messaging program on secondary men's outcomes using adjusted regression
       specification based on the Lin 2013 estimator with wild cluster bootstrap
       standard errors in parentheses. Columns 1 and 2 are a composite index of
       how often the couple discusses relationship and household practicalities. 
       Columns 3 and 4 are a composite index of couple's shared discussion about each other.
       Columns 5 and 6 are a composite index of frequency of good expressions between couple.
       Columns 7 and 8 are a composite index of the man's enjoyment of mutual activities with 
       his female partner. All indices were constructed as sums of subitems coded in
       same substantive direction. Bootstrapped standard errors estimated using 10,000 
       replicates. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/conflict_results_m.tex")
texreg(
  lapply(conflict_results_m, get, x = "fit"),
  custom.model.names = c(
    "\\shortstack{Resolve \\\\ conflicts}",
    "\\shortstack{Resolve \\\\ conflicts}",
    "\\shortstack{Freq. \\\\ argue}",
    "\\shortstack{Freq. \\\\ argue}",
    "\\shortstack{Woman's \\\\ resolution \\\\ skills}",
    "\\shortstack{Woman's \\\\ resolution \\\\ skills}",
    "\\shortstack{Man's \\\\ emotional \\\\ reg.}",
    "\\shortstack{Man's \\\\ emotional \\\\ reg.}"
  ),
  custom.coef.names = c("Constant", "MMC"),
  override.se = lapply(lapply(conflict_results_m, get, x = "robust"), "[", , 2),
  override.pvalues = lapply(lapply(conflict_results_m, get, x = "robust"), "[", , 4),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"), "}"),
    "Clusters" = c(16, 16, 16, 16, 16, 16, 16, 16)
  ),
  reorder.gof = c(1, 2, 4, 3),
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
       \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile
       messaging program on secondary men's outcomes using adjusted regression
       specification based on the Lin 2013 estimator with wild cluster bootstrap
       standard errors in parentheses. Columns 1 and 2 are a composite index of
       the man's report of the couple's ability to resolve conflict. Columns 3 and 4
       are a composite index of the man's report of how frequently he and his partner argue. 
       Columns 5 and 6 are a composite index of the man's report of his partner's positive conflict 
       resolution skills. Columns 7 and 8 are a composite index of the man's ability to emotionally regulate
       during conflict. All indices were constructed as sums of subitems coded in same 
       substantive direction. Bootstrapped standard errors estimated using 10,000 replicates. \\\\ %stars.}"
) %>% print()
sink()


