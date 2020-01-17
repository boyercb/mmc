# variance estimators -----------------------------------------------------

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


# randomization inference -------------------------------------------------

ri_models <-
  map(violence_outcomes,
      function (x) {
        ri(
          outcome = x,
          covariates = get_covariates(x, selected_covariates),
          data = el_imputed,
          cluster = "block_id",
          sims = sims
        )
      })




# compile results ---------------------------------------------------------

inference_vcovs <- lapply(inference_models, get, x = "robust")
ri_pvals <- sapply(ri_models, get, x = "ri_pval")

inference_table <-
  tibble(
    outcome = c(se_outcomes, violence_outcomes),
    se_type = c(se_types, rep("ri", 3)),
    std.error = c(sapply(inference_vcovs, "[", 2, 2), rep(NA, 3)),
    p.value = c(sapply(inference_vcovs, "[", 2, 4), ri_pvals)
  )

se_order <- c(
  "HC0",
  "HC1",
  "HC2",
  "HC3",
  "wild-rademacher",
  "wild-mammen",
  "wild-norm",
  "wild-webb",
  "xy",
  "ri"
)

inference_table <-
  inference_table %>%
  gather(key, value, -outcome, -se_type) %>%
  spread(outcome, value) %>%
  arrange(se_type, desc(key)) %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), "-", as.character(specd(x, 3)))) %>%
  arrange(match(se_type, se_order)) %>%
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
      ),
    se_type = case_when(
      se_type == "HC0" ~ "CR0",
      se_type == "HC1" ~ "CR1",
      se_type == "HC2" ~ "CR2",
      se_type == "HC3" ~ "CR3",
      se_type == "wild-rademacher" ~ "wild-cluster bootstrap-se (Rademacher)",
      se_type == "wild-mammen" ~ "wild-cluster bootstrap-se (Mammen)",
      se_type == "wild-norm" ~ "wild-cluster bootstrap-se (Norm)",
      se_type == "wild-webb" ~ "wild-cluster bootstrap-se (Webb)",
      se_type == "xy" ~ "cluster pairs bootstrap-se",
      se_type == "ri" ~ "randomization inference $\\beta$",
      se_type == "ri" ~ "randomization inference $t$"
    )
  ) %>%
  select(
    se_type,
    ipv_control_2pl_irt_index_w,
    phys_sex_violence_2pl_index_w,
    emo_violence_2pl_index_w
  )


# make tables -------------------------------------------------------------

options(knitr.kable.NA = '')

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
  add_header_above(c(" ", "(1)", "(2)", "(3)"), line = F) %>%
  footnote("Comparison of inference from sampling and randomization-based 
       uncertainty estimates for pre-registered primary outcomes. Estimated standard errors
       are shown with $p$-value from two-sided hypothesis of no effect shown in parentheses. 
       CR0 - CR3 are cluster and heteroskedasticity robust variance estimators as defined in
       The wild cluster bootstrap estimates use the algorithm from Cameron, Gelbach, & Miller (2008) 
       with multipliers drawn from either the Rademacher, Mammen, Normal, or Webb distributions.", 
       escape = F, threeparttable = T )
sink()


