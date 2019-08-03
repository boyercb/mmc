# All blocks (N = 16) -----------------------------------------------------

outcomes <- rep(violence_outcomes, each = 2)
covariates <- rep(pre_violence_outcomes_c, each = 2)
covariates[c(TRUE, FALSE)] <- NA

main_models <-
  map2(outcomes,
       covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = y,
           data = el,
           clusters = "block_m"
         )
       })


# Only randomized blocks (N = 10) -----------------------------------------

main_models_ra <-
  map2(outcomes,
       covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = y,
           data = filter(el, manual_block == 0),
           clusters = "block_m"
         )
       })


# Create results lists ----------------------------------------------------

ipv_results <-
  list(
    main_models[[1]], 
    main_models[[2]], 
    main_models_ra[[1]],
    main_models_ra[[2]]
  )

ipv_freq_results <-
  list(
    main_models[[3]], 
    main_models[[4]], 
    main_models_ra[[3]],
    main_models_ra[[4]]
  )

physical_results <-
  list(
    main_models[[5]], 
    main_models[[6]], 
    main_models_ra[[5]],
    main_models_ra[[6]]
  )

physical_freq_results <-
  list(
    main_models[[7]], 
    main_models[[8]], 
    main_models_ra[[7]],
    main_models_ra[[8]]
  )

sexual_results <-
  list(
    main_models[[9]], 
    main_models[[10]], 
    main_models_ra[[9]],
    main_models_ra[[10]]
  )

sexual_freq_results <-
  list(
    main_models[[11]], 
    main_models[[12]], 
    main_models_ra[[11]],
    main_models_ra[[12]]
  )


# Make tables -------------------------------------------------------------

sink("08_memo/tables/ipv.tex")
texreg(
  ipv_results,
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c("Constant", "MMC"),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("No", "Yes", "No", "Yes"), "}"),
    "Clusters" = c(16, 16, 10, 10)
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
  custom.note = "\\parbox{.5\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Columns 1 and 2 estimate the effect on the full sample while Columns 3 
       and 4 estimate effects among the subset of clusters that were randomly allocated.
       Columns 1 and 3 use the unadjusted estimator while Columns 2 and 4 condition on 
       pre-treatment violence outcomes using the adjusted estimator. Cluster-robust 
       standard errors for all estimates are reported in parentheses. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/ipv_freq.tex")
texreg(
  ipv_freq_results,
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c("Constant", "MMC"),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("No", "Yes", "No", "Yes"), "}"),
    "Clusters" = c(16, 16, 10, 10)
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
  custom.note = "\\parbox{.5\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Columns 1 and 2 estimate the effect on the full sample while Columns 3 
       and 4 estimate effects among the subset of clusters that were randomly allocated.
       Columns 1 and 3 use the unadjusted estimator while Columns 2 and 4 condition on 
       pre-treatment violence outcomes using the adjusted estimator. Cluster-robust 
       standard errors for all estimates are reported in parentheses. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/physical.tex")
texreg(
  physical_results,
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c("Constant", "MMC"),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("No", "Yes", "No", "Yes"), "}"),
    "Clusters" = c(16, 16, 10, 10)
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
  custom.note = "\\parbox{.5\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Columns 1 and 2 estimate the effect on the full sample while Columns 3 
       and 4 estimate effects among the subset of clusters that were randomly allocated.
       Columns 1 and 3 use the unadjusted estimator while Columns 2 and 4 condition on 
       pre-treatment violence outcomes using the adjusted estimator. Cluster-robust 
       standard errors for all estimates are reported in parentheses. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/physical_freq.tex")
texreg(
  physical_freq_results,
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c("Constant", "MMC"),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("No", "Yes", "No", "Yes"), "}"),
    "Clusters" = c(16, 16, 10, 10)
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
  custom.note = "\\parbox{.5\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Columns 1 and 2 estimate the effect on the full sample while Columns 3 
       and 4 estimate effects among the subset of clusters that were randomly allocated.
       Columns 1 and 3 use the unadjusted estimator while Columns 2 and 4 condition on 
       pre-treatment violence outcomes using the adjusted estimator. Cluster-robust 
       standard errors for all estimates are reported in parentheses. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/sexual.tex")
texreg(
  sexual_results,
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c("Constant", "MMC"),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("No", "Yes", "No", "Yes"), "}"),
    "Clusters" = c(16, 16, 10, 10)
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
  custom.note = "\\parbox{.5\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Columns 1 and 2 estimate the effect on the full sample while Columns 3 
       and 4 estimate effects among the subset of clusters that were randomly allocated.
       Columns 1 and 3 use the unadjusted estimator while Columns 2 and 4 condition on 
       pre-treatment violence outcomes using the adjusted estimator. Cluster-robust 
       standard errors for all estimates are reported in parentheses. \\\\ %stars.}"
) %>% print()
sink()

sink("08_memo/tables/sexual_freq.tex")
texreg(
  sexual_freq_results,
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c("Constant", "MMC"),
  reorder.coef = c(2, 1),
  custom.gof.rows = list(
    "Covariates" = paste0("\\textrm{", c("No", "Yes", "No", "Yes"), "}"),
    "Clusters" = c(16, 16, 10, 10)
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
  custom.note = "\\parbox{.5\\linewidth}{\\vspace{2pt} 
       \\textit{Notes:} Columns 1 and 2 estimate the effect on the full sample while Columns 3 
       and 4 estimate effects among the subset of clusters that were randomly allocated.
       Columns 1 and 3 use the unadjusted estimator while Columns 2 and 4 condition on 
       pre-treatment violence outcomes using the adjusted estimator. Cluster-robust 
       standard errors for all estimates are reported in parentheses. \\\\ %stars.}"
) %>% print()
sink()
