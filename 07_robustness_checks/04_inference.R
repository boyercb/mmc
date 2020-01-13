# All blocks (N = 16) -----------------------------------------------------

se_outcomes <- rep(violence_outcomes, each = 9)
se_types <-
  rep(
    c(
      "wild-rademacher",
      "HC0",
      "HC1",
      "HC2",
      "HC3",
      "xy",
      "wild-mammen",
      "wild-norm",
      "wild-webb"
    ),
    length(violence_outcomes)
  )

inference_models <-
  map2(se_outcomes,
       se_types,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = get_covariates(x, selected_covariates),
           data = el_imputed,
           cluster = "block_id", 
           se_type = y,
           sims = sims
         )
       })

inference_vcovs <- lapply(inference_models, get, x = "robust")

inference_table <-
  tibble(
    outcome = se_outcomes,
    se_type = se_types,
    std.error = sapply(inference_vcovs, "[", 2, 2),
    p.value = sapply(inference_vcovs, "[", 2, 4)
  )

inference_table <-
  inference_table %>%
  gather(key, value, -outcome, -se_type) %>%
  spread(outcome, value) %>%
  arrange(se_type, desc(key)) %>%
  mutate_if(is.numeric, function(x) as.character(specd(x, 3))) %>%
  mutate(
    ipv_control_2pl_irt_index_w =
      ifelse(
        key == "p.value",
        paste0("[", ipv_control_2pl_irt_index_w , "]"),
        ipv_control_2pl_irt_index_w
      ),
    phys_sex_violence_2pl_index_w =
      ifelse(
        key == "p.value",
        paste0("[", phys_sex_violence_2pl_index_w , "]"),
        phys_sex_violence_2pl_index_w
      ),
    emo_violence_2pl_index_w =
      ifelse(
        key == "p.value",
        paste0("[", emo_violence_2pl_index_w , "]"),
        emo_violence_2pl_index_w
      ),
    se_type =
      ifelse(
        key == "p.value",
        "",
        se_type
      )
  ) %>%
  select(
    se_type,
    ipv_control_2pl_irt_index_w,
    phys_sex_violence_2pl_index_w,
    emo_violence_2pl_index_w
  )


# Make tables -------------------------------------------------------------

sink("08_memo/tables/inference.tex")
kable(
  x = inference_table,
  format = "latex",
  col.names = c("Estimator", "IPV", "Physical/Sexual", "Emotional"),
  escape = FALSE,
  align = "lccc",
  booktabs = TRUE,
  linesep = c("", "\\addlinespace")
) %>% 
  kable_styling(latex_options = "HOLD_position") %>%
  add_header_above(c(" ", "(1)", "(2)", "(3)"), line = F)
sink()

# kable
# texreg(
#   lapply(main_models, get, x = "fit"),
#   custom.model.names = c("IPV",
#                          "IPV",
#                          "Physical/Sexual",
#                          "Physical/Sexual",
#                          "Emotional",
#                          "Emotional"),
#   custom.coef.names = c("Constant", "MMC"),
#   override.se = lapply(lapply(main_models, get, x = "robust"), "[", , 2),
#   override.pvalues = lapply(lapply(main_models, get, x = "robust"), "[", , 4),
#   reorder.coef = c(2, 1),
#   custom.gof.rows = list(
#     "Covariates" = paste0("\\textrm{", primary_covariates, "}"),
#     "Clusters" = c(16, 16, 16, 16, 16, 16)
#   ),  
#   reorder.gof = c(1, 2, 4, 3),
#   custom.gof.names = c("Adj. R$^2$", "Observations"),
#   omit.coef = "_[c]",
#   digits = 3,
#   include.ci = FALSE,
#   table = FALSE,
#   include.rsquared = FALSE,
#   include.rmse = FALSE,
#   booktabs = TRUE,
#   use.packages = FALSE,
#   custom.note = "\\parbox{\\linewidth}{\\vspace{2pt} 
#        \\textit{Notes:} Estimates of the intent-to-treat effects of Modern Man mobile 
#        messaging program on pre-registered primary outcomes using adjusted regression 
#        specification based on the Lin 2013 estimator with wild cluster bootstrap 
#        standard errors in parentheses. Columns 1 and 2 are a composite index of 
#        acts of intimate partner violence. Columns 3 and 4 are a composite index of acts
#        of physical violence. Columns 5 and 6 are a composite index of acts of sexual violence.
#        All indices were constructed using the first factor from IRT models of subitems. 
#        Bootstrapped standard errors estimated using 10,000 replicates. \\\\ %stars.}"
# ) %>% print()
# sink()


