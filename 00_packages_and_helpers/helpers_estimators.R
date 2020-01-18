# Function to grab covariates ---------------------------------------------

get_covariates <- function(outcome_name, covariate_frame){
  if(!outcome_name %in% covariate_frame$outcome) {
    print(paste0("No covariates found for ",outcome_name,".\nDid you use the right lasso dataset?"))
    return(NULL)}
  covariate_frame %>% filter(outcome == outcome_name) %>% select(term) %>% 
    unlist()
}


# Estimating functions ----------------------------------------------------

main_estimator <-
  function (outcome,
            covariates = NULL,
            treatment = "treatment",
            data,
            cluster = NULL,
            se_type = "HC2",
            studentized = FALSE,
            sims) {
    
    if (is.null(covariates)) {
      f <- reformulate(treatment, outcome)
    } else {
      f <- reformulate(
        termlabels = c(treatment, covariates, paste0(treatment, ":", covariates)), 
        response = outcome
      )
    }
    
    fit <- lm(
      formula = f,
      data = data,
    )
    
    if (!is.null(cluster)) {
      fc <- reformulate(cluster)
    } else {
      fc <- NULL
    }
    
    if (se_type %in% c("xy", "residual") | grepl("^wild", se_type)) {
      stopifnot(!is.null(sims))
      if (studentized) {
        if (is.null(cluster)) {
          vcov <- vcovHC(
            fit,
            type = "HC2"
          )
        } else {
          vcov <- vcovCL(
            fit,
            cluster = fc,
            type = "HC2"
          )
        }
        bs <- boott(
          fit,
          cluster = fc,
          R = sims,
          type = se_type,
          impose.null = treatment
        )
      } else { 
        vcov <- vcovBS(
          fit,
          cluster = fc,
          R = sims,
          type = se_type,
          start = TRUE
        )
      }
    } else if (grepl("^HC[0-3]", se_type)) {
      if (is.null(cluster)) {
        vcov <- vcovHC(
          fit,
          type = se_type
        )
      } else {
        vcov <- vcovCL(
          fit,
          cluster = fc,
          type = se_type
        )
      }
    } else if (grepl("^classic", se_type)) {
      vcov <- vcov(fit)
    } else {
      stop(paste0("Error: standard error type ", se_type, " not found."))
    }
    
    robust <- coeftest(fit, vcov = vcov)
    
    bs <- if (studentized) bs else NULL
    
    est_obj <- list(
      fit = fit,
      robust = robust,
      bs = bs
    )
    
    class(est_obj) <- "main_estimator"
    
    return(est_obj)
  }

# examples
if (FALSE) {
  main_estimator(
    outcome = "ipv_control_2pl_irt_index_w",
    covariates = get_covariates("ipv_control_2pl_irt_index_w", selected_covariates),
    data = el_imputed,
    cluster = "block_id", 
    se_type = "wild",
    sims = sims
  )
  
  main_estimator(
    outcome = "ipv_control_2pl_irt_index_w",
    covariates = get_covariates("ipv_control_2pl_irt_index_w", selected_covariates),
    data = el_imputed,
    cluster = "block_id", 
    se_type = "wild",
    studentized = T,
    sims = sims
  )
  
  main_estimator(
    outcome = "ipv_control_2pl_irt_index_w",
    covariates = get_covariates("ipv_control_2pl_irt_index_w", selected_covariates),
    data = el_imputed,
    cluster = "block_id", 
    se_type = "wild-webb",
    studentized = T,
    sims = sims
  )
}
