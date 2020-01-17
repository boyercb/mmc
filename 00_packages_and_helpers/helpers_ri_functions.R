# helper functions --------------------------------------------------------

# function to generate permutted treatment assignments for use with 
# randomization inference functions
rerandomize <- function(data, sims) {
  declaration <- with(data,
                      declare_ra(
                        clusters = block_id,
                        prob = 0.5
                      ))
  
  permutation_matrix <- replicate(sims, conduct_ra(declaration))
  
  return(permutation_matrix)
}

# function to calculate p-value based on simulated null distribution and
# the direction of the null hypothesis
get_p <- function(observed_effect, null_distribution, hypothesis = "two") {
  if (hypothesis == "two") {
    mean(abs(null_distribution) >= abs(observed_effect))
  } else if (hypothesis %in% c("upper", "positive")) {
    mean(null_distribution >= observed_effect)
  } else if (hypothesis %in% c("lower", "negative")) {
    mean(null_distribution <= observed_effect)
  } else {
    stop("You have not supplied a valid hypothesis direction. Please choose:\n two; 'upper' (alt: 'positive'); or 'lower' (alt: 'negative').")
  }
}

# helper functions to get treatment effect estimates from model objectw
get_estimate <- function(model, treatment = "treatment"){
  model %>% 
    tidy() %>% 
    filter(term == treatment) %>% 
    pull(estimate)
}

# quick version 
get_estimate_quick <- function(model, treatment = "treatment"){
  coef_mat <- coef(model)
  coef_mat[, treatment_variable]
}

get_F <- function(model, term) {
  model$proj_fstatistic[1] %>% as.numeric()
}

get_ci <- function(observed_effect, null_distribution) {
  observed_effect + quantile(null_distribution, probs = c(0.025, 0.975))
}

get_interaction_terms <- function(model, terms) {
  model %>% 
    tidy() %>% 
    filter(term %in% terms) %>% 
    pull(estimate)
}


# randomization inference functions ---------------------------------------

ri <-
  function(data,
           outcome,
           treatment = "treatment",
           covariates = NULL,
           cluster = "block_id",
           se_type = "HC2",
           print = outcome,
           extract = get_estimate,
           estimator = main_estimator,
           term = treatment,
           hypothesis = "two",
           sims) {
    
    cat(paste0("   outcome: ", print, "\n"))
    cat(paste0("   simulating ", sims, " assignments under the sharp null...\n"))
    
    model <- estimator(
      outcome = outcome,
      covariates = covariates,
      treatment = treatment,
      cluster = cluster,
      se_type = se_type,
      data = data
    )
    
    observed_effect <- get_estimate(
      model = model$fit, 
      treatment = term
    )
    
    permutted_Z <- rerandomize(data, sims)
    
    pb <- txtProgressBar(min = 0, max = sims, style = 3)
    i <- 1
    
    null_distribution <- 
      apply(permutted_Z,
            2,
            function(permutted_Z) {
              i <<- i + 1
              
              if (outcome == "treatment") {
                data[, outcome] <- permutted_Z
              } else {
                data[, treatment] <- permutted_Z
              }
              
              est_obj <-
                estimator(
                  outcome = outcome,
                  covariates = covariates,
                  treatment = treatment,
                  cluster = cluster,
                  se_type = se_type,
                  data = data
                )
              
              fit <- est_obj$fit
              
              setTxtProgressBar(pb, i)
              
              extract(fit, term)
            })
    
    cat("\n")
    close(pb)
    
    ri_pval <- get_p(
      observed_effect = observed_effect,
      null_distribution = null_distribution,
      hypothesis = hypothesis
    )
    
    return_fit <- 
      list(model = model, 
           ri_pval = ri_pval,
           hypothesis = hypothesis,
           null_distribution = null_distribution,
           sims = sims, 
           treatment = treatment,
           outcome = outcome)
    
    class(return_fit) <- "ri_fit"
    
    return(return_fit)
  }

# ri_interaction <-
#   function(data,
#            outcome,
#            sharp_null,
#            subgroup,
#            treatment = "Z",
#            covariates = NULL,
#            fixed_effects = "block",
#            clusters = "ward",
#            weights = "ipw",
#            se_type = "none",
#            print = outcome,
#            extract = get_estimate,
#            term = treatment,
#            sims) {
#     
#     cat(paste0("   outcome: ", print, "\n"))
#     cat(paste0("   subgroups: ", subgroup, "\n"))
#     cat(paste0("   simulating ", sims, " assignments under the sharp null...\n"))
#     
#     if (!is.null(covariates)) {
#       res_covs <- str_subset(covariates, subgroup, negate = TRUE)
#     } else {
#       res_covs <- NULL
#     }
#     
#     Z_sims <- rerandomize(data, sims)
#     
#     pb <- txtProgressBar(min = 0, max = sims, style = 3)
#     i <- 1
#     
#     reps <- apply(
#       Z_sims, 
#       2, 
#       function(Z_sim) {
#         i <<- i + 1
#         
#         if (outcome == "Z") {
#           data[, outcome] <- Z_sim
#         } else {
#           data[, treatment] <- Z_sim
#         }
#         
#         fit_0 <-
#           main_estimator(
#             outcome = outcome,
#             covariates = res_covs,
#             treatment = treatment,
#             subgroup = paste0(subgroup, " == 0"),
#             clusters = clusters,
#             fixed_effects = fixed_effects,
#             weights = weights,
#             se_type = se_type,
#             data = data,
#             ci = FALSE,
#             return_vcov = FALSE, 
#             try_cholesky = TRUE
#           )
#         
#         fit_1 <-
#           main_estimator(
#             outcome = outcome,
#             covariates = res_covs,
#             treatment = treatment,
#             subgroup = paste0(subgroup, " == 1"),
#             clusters = clusters,
#             fixed_effects = fixed_effects,
#             weights = weights,
#             se_type = se_type,
#             data = data,
#             ci = FALSE,
#             return_vcov = FALSE, 
#             try_cholesky = TRUE
#           )
#         
#         Y_0 <- data[, outcome] - sharp_null * Z_sim 
#         Y_1 <- data[, outcome] + sharp_null * (1 - Z_sim)
#         data[, "Y_sim"] <- Y_0 * (1 - Z_sim) + Y_1 * Z_sim
#         
#         fit_full <- 
#           main_estimator(
#             outcome = "Y_sim",
#             covariates = c(subgroup, covariates),
#             treatment = treatment,
#             clusters = clusters,
#             fixed_effects = fixed_effects,
#             weights = weights,
#             se_type = se_type,
#             data = data,
#             ci = FALSE,
#             return_vcov = FALSE, 
#             try_cholesky = TRUE
#           )
#         
#         setTxtProgressBar(pb, i)
#         
#         c(
#           extract(fit_0, term),
#           extract(fit_1, term),
#           extract(fit_full, paste0(term, ":", subgroup))
#         )
#       })
#     
#     cat("\n")
#     close(pb)
#     return(reps)
#   }


