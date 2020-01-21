# T-test of missingness ---------------------------------------------------

# First, we will perform a two-tailed unequal-variances t-test of the 
# hypothesis that treatment does not affect the attrition rate. Per the 
# Green lab SOP (https://github.com/acoppock/Green-Lab-SOP), we will 
# implement the test as a permutation test that compares the observed 
# t–statistic with its empirical distribution under thousands of repeated 
# random reassignments of treatment.

get_attrit_T <-
  function(data,
           attrited,
           treatment,
           subset,
           scramble_Z = FALSE) {
    if (scramble_Z) {
      data[, treatment] <- rerandomize(data, treatment)
    }
    
    fit <- t.test(
      reformulate(treatment, attrited),
      data = data,
      subset = subset,
      alternative = "two.sided",
      var.equal = FALSE
    )
    
    fit$statistic
  }

observed_T <-
  get_attrit_T(bll, "attrited", "Z", !bll$death_or_prison)


null_T <-
  replicate(n = sims,
            get_attrit_T(bll, "attrited", "Z", !bll$death_or_prison, scramble_Z = TRUE))
  
attrition_T_test_pval <- mean(abs(null_T) >= abs(observed_T))

# F-test of missingness on Z * cov ----------------------------------------

# Second, using a linear regression of an attrition indicator on treatment,
# baseline covariates, and treatment-covariate interactions, we will perform a
# heteroskedasticity-robust F-test of the hypothesis that all the interaction
# coefficients are zero.

get_attrit_F <- function(data, attrited, treatment, covariates, 
                         subset, se_type, scramble_Z = FALSE) {
  if (scramble_Z) {
    data[,treatment] <- rerandomize(data, treatment)
  }
  
  full_formula <-
    reformulate(termlabels = c(treatment, covariates, paste0(treatment, ":", covariates)),
                response = attrited)
  
  restricted_formula <- 
    reformulate(termlabels = c(treatment, covariates),
                response = attrited)
  
  if (se_type == "classical") {
    full_fit <-
      lm(full_formula,
         data = data,
         subset = subset)
    
    restricted_fit <-
      lm(restricted_formula,
         data = data,
         subset = subset)
    
    with(anova(full_fit, restricted_fit), F) %>% na.omit()
  } else {
    hypothesis <- paste0(paste0("Z:", covariates), " = 0")
    
    with(car::linearHypothesis(
      lm(full_formula, data = data, subset = subset),
      hypothesis.matrix = hypothesis,
      white.adjust = tolower(se_type),
      singular.ok = TRUE
    ), F) %>% na.omit()
  }
}

observed_F <-
  get_attrit_F(
    data = bll,
    attrited = "attrited",
    treatment = "Z",
    covariates = individual_covariates[1:100],
    subset = !bll$death_or_prison,
    se_type =  "HC2"
  )

if (run_attrition_analysis) {
  null_F <-
    replicate(
      n = sims,
      get_attrit_F(
        data = bll,
        attrited = "attrited",
        treatment = "Z",
        covariates = individual_covariates[1:100],
        subset = !bll$death_or_prison,
        se_type =  "HC2",
        scramble_Z = TRUE
      )
    )
  
  write_rds(null_F, get_path("results/null_F.rds"))
} else {
  null_F <- read_rds(get_path("results/null_F.rds"))
}

attrition_F_test_pval <- mean(abs(null_F) >= abs(observed_F))


# Attrition test plot -----------------------------------------------------

plot_dat <- rbind(
  data.frame(observed_stat = observed_T, 
             null_stat = null_T, 
             pval = attrition_T_test_pval,
             test = "T-Test of Null:\nTreatment assignment unrelated to missingness",
             row.names = NULL),
  data.frame(observed_stat = observed_F[[1]], 
             null_stat = null_F, 
             pval = attrition_F_test_pval,
             test = "F-Test of Null:\nMissingness unrelated to potential outcomes",
             row.names = NULL)
)

if(run_attrition_plots){
  pdf("07_midline_manuscript/figures/attrition_plot.pdf",width = 6,height = 3)
  p <- ggplot(data = plot_dat, aes(x = null_stat,  fill = test)) +
    scale_fill_manual(values = c("darkgreen", "darkblue")) +
    geom_histogram(show.legend = FALSE, alpha = .8) +
    geom_vline(aes(xintercept = observed_stat), size = .5) +
    geom_label(aes(y = 1, 
                   x = observed_stat, 
                   label = paste0("p-value: ",round(pval,3))), 
               fill = "white", label.size = NA) +
    facet_wrap(~ test, scales = "free") +
    xlab("Distribution of test-statistic under sharp null.") +
    coord_cartesian(clip = "off") +
    mmc_theme()
  print(p)
  dev.off()
}



