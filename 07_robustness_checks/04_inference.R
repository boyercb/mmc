# variance estimators -----------------------------------------------------

se_outcomes <- 
  rep(violence_outcomes, each = 14)

se_types <-
  rep(
    c(
      "HC0",
      "HC1",
      "HC2",
      "HC3",
      "xy",
      "wild-rademacher",
      "wild-mammen",
      "wild-norm",
      "wild-webb",
      "xy",
      "wild-rademacher",
      "wild-mammen",
      "wild-norm",
      "wild-webb"
    ),
    length(violence_outcomes)
  )

se_studentized <-
  rep(c(rep("No", 9), rep("Yes", 5)), length(violence_outcomes))

inference_models <-
  pmap(
    list(x = se_outcomes,
         y = se_types,
         z = se_studentized
    ),
    function (x, y, z) {
      main_estimator(
        outcome = x,
        covariates = get_covariates(x, selected_covariates),
        data = el_imputed,
        cluster = "block_id",
        se_type = y,
        studentized = if (z == "Yes") TRUE else FALSE,
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
inference_bs <- lapply(inference_models, get, x = "bs")
inference_bs <- inference_bs[sapply(inference_bs, function (x) !is.null(x))]
ri_pvals <- sapply(ri_models, get, x = "ri_pval")

inference_table <-
  tibble(
    outcome = c(se_outcomes, violence_outcomes),
    se_type = c(se_types, rep("ri", length(violence_outcomes))),
    se_studentized = c(se_studentized, rep("No", length(violence_outcomes))),
    std.error = c(
      sapply(inference_vcovs[1:9], "[", 2, 2), 
      rep(NA, 5),
      sapply(inference_vcovs[15:23], "[", 2, 2), 
      rep(NA, 5),
      sapply(inference_vcovs[29:37], "[", 2, 2), 
      rep(NA, 5),
      rep(NA, length(violence_outcomes))
    ),
    p.value = c(
      sapply(inference_vcovs[1:9], "[", 2, 4),
      sapply(inference_bs[1:5], get, x = "boot.p"),
      sapply(inference_vcovs[15:23], "[", 2, 4),
      sapply(inference_bs[6:10], get, x = "boot.p"),
      sapply(inference_vcovs[29:37], "[", 2, 4),
      sapply(inference_bs[11:15], get, x = "boot.p"),
      ri_pvals
    )
  )

se_order <- c(
  "HC0_No",
  "HC1_No",
  "HC2_No",
  "HC3_No",
  "wild-rademacher_No",
  "wild-mammen_No",
  "wild-norm_No",
  "wild-webb_No",
  "xy_No",
  "wild-rademacher_Yes",
  "wild-mammen_Yes",
  "wild-norm_Yes",
  "wild-webb_Yes",
  "xy_Yes",
  "ri_No"
)

inference_table <-
  inference_table %>%
  gather(key, value, -outcome, -se_type, -se_studentized) %>%
  spread(outcome, value) %>%
  arrange(se_type, se_studentized, desc(key)) %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), "-", as.character(specd(x, 3)))) %>%
  unite("se_type", se_type, se_studentized, remove = TRUE) %>%
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
      se_type == "HC0_No" ~ "CR0",
      se_type == "HC1_No" ~ "CR1",
      se_type == "HC2_No" ~ "CR2",
      se_type == "HC3_No" ~ "CR3",
      se_type == "wild-rademacher_No" ~ "wild-cluster bootstrap-se (Rademacher)",
      se_type == "wild-mammen_No" ~ "wild-cluster bootstrap-se (Mammen)",
      se_type == "wild-norm_No" ~ "wild-cluster bootstrap-se (Norm)",
      se_type == "wild-webb_No" ~ "wild-cluster bootstrap-se (Webb)",
      se_type == "xy_No" ~ "cluster pairs bootstrap-se",
      se_type == "wild-rademacher_Yes" ~ "wild-cluster bootstrap-$t$ (Rademacher)",
      se_type == "wild-mammen_Yes" ~ "wild-cluster bootstrap-$t$ (Mammen)",
      se_type == "wild-norm_Yes" ~ "wild-cluster bootstrap-$t$ (Norm)",
      se_type == "wild-webb_Yes" ~ "wild-cluster bootstrap-$t$ (Webb)",
      se_type == "xy_Yes" ~ "cluster pairs bootstrap-$t$",
      se_type == "ri_No" ~ "randomization inference $\\beta$"
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
tab <-
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
  footnote(
    "Comparison of inference from sampling and randomization-based
       uncertainty estimates for pre-registered primary outcomes. Estimated standard errors
       are shown with $p$-value from two-sided hypothesis of no effect shown in parentheses.
       CR0 - CR3 are cluster and heteroskedasticity robust variance estimators as defined in
       The wild cluster bootstrap estimates use the algorithm from Cameron, Gelbach, and Miller (2008)
       with multipliers drawn from either the Rademacher, Mammen, Normal, or Webb distributions.",
    escape = F,
    threeparttable = T
  )  
  print(tab)
sink()


