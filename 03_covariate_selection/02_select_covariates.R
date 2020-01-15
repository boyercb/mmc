# Select covariates using lasso -------------------------------------------

if (run_lasso) {
	
	selected_covariates <-
		outcomes %>%
		map(function (x) {
			select_covariates(
				outcome_name = x,
				covariates = paste0(invariant_covariates, "_c"), # use centered versions 
				data = el_imputed,
				sims = lasso_sims,
				N_folds = lasso_folds
			)
		}) %>% bind_rows()
	
	write.csv(
	  selected_covariates,
		"03_covariate_selection/selected_covariates.csv",
		na = ""
	)
	
	selected_pooled_covariates <-
	  outcomes %>%
	  map(function (x) {
	    select_covariates(
	      outcome_name = x,
	      covariates = paste0(invariant_covariates, "_c"), # use centered versions 
	      data = pooled_el,
	      sims = lasso_sims,
	      N_folds = lasso_folds
	    )
	  }) %>% bind_rows()
	
	write.csv(
	  selected_pooled_covariates,
	  "03_covariate_selection/selected_pooled_covariates.csv",
	  na = ""
	)
	
	
} else {
  selected_covariates <-
		read.csv(
			"03_covariate_selection/selected_covariates.csv",
			stringsAsFactors = FALSE
		)
  
  selected_pooled_covariates <-
    read.csv(
      "03_covariate_selection/selected_pooled_covariates.csv",
      stringsAsFactors = FALSE
    )
	
}
