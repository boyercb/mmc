# Rerandomize treatment at any level --------------------------------------

rerandomize <- function(data, treatment_variable = "Z"){
	if(treatment_variable == "Z"){
		with(data, randomizr::block_and_cluster_ra(blocks = blocks, clusters = cup_id, prob = .5))
	} else if(treatment_variable == "no_bl_violence_qs"){
		sample(data$no_bl_violence_qs)
	} else {
		stop(paste0("No randomization method defined for variable ", treatment_variable))
	}
}


if(FALSE){
	# Examples
	rerandomize(mll)
	rerandomize(mlw)
	mutate(mlw, Z_sim = rerandomize(mlw)) %>% with(.,table(blocks, Z_sim))
	mutate(mll, Z_sim = rerandomize(mll)) %>% with(.,table(blocks, Z_sim))
}

# Get distribution of effects under null ----------------------------------


get_estimate <- function(model, treatment_variable = "Z"){
	model %>% 
		tidy() %>% 
		filter(term == treatment_variable) %>% 
		pull(estimate)
}

get_estimate_quick <- function(model, treatment_variable = "Z"){
	coef_mat <- coef(model)
	coef_mat[, treatment_variable]
}

get_estimate_ipv <- function(model, treatment_variable = "Z"){
	if (class(model) == "bife") {
		coef_mat <- model %>% summary %>% get("coefmat_beta", .)
		colnames(coef_mat) <- c("estimate", "std.error", "statistic", "p-value")
		coef_mat <- as_tibble(coef_mat, rownames = "term")
		
		coef_mat %>%
			filter(term == treatment_variable) %>% 
			pull(estimate)
	} else {
		model %>%
			tidy() %>%
			filter(term == treatment_variable) %>%
			pull(estimate)
	}
}


get_effects_under_null <- function(outcome, covariates = NULL, 
																	 subgroup = NULL, treatment_variable = "Z",
																	 extract_function = get_estimate, weights = NULL,
																	 data, sims = 500, estimator = main_estimator,
																	 term = treatment_variable){
	if(!is.null(subgroup)){
		data <- subset(data, eval(parse(text = subgroup)))
	}
	i <- 1
	replicate(n = sims, expr = {
		print(paste0(outcome, " sim: ", i))
		i <<- i + 1
		data[,treatment_variable] <- rerandomize(data = data,
																						 treatment_variable = treatment_variable)
		estimator(outcome = outcome,
							covariates = covariates, 
							treatment_variable = treatment_variable,
							level = "RI", 
							weights = weights,
							data = data) %>% 
			extract_function(treatment_variable = term)
	})
}

if(FALSE){
	get_effects_under_null(
		outcome = "control_index",
		data = mlw, sims = 3
	)
	get_effects_under_null(
		outcome = "dm_ladder",
		data = mll, sims = 3
	)
	get_effects_under_null(
		outcome = "dm_ladder", covariates = c("woman"),
		data = mll, sims = 3, term = "Z:woman"
	)
}

# Get p-value from a null distribution ------------------------------------

get_pval <- function(observed_effect, null_distribution, hypothesis = "two"){
	if(hypothesis == "two"){
		mean(abs(null_distribution) >= abs(observed_effect))
	} else if(hypothesis %in% c("upper", "positive")){
		mean(null_distribution >= observed_effect)
	} else if(hypothesis %in% c("lower", "negative")){
		mean(null_distribution <= observed_effect)
	} else{
		stop("You have not supplied a valid hypothesis direction. Please choose:\n two; 'upper' (alt: 'positive'); or 'lower' (alt: 'negative').")
	}
}

# Wrapper function --------------------------------------------------------

get_estimate_and_pval <- function(
	outcome, covariates = NULL, level = "couple",
	subgroup = NULL, treatment_variable = "Z",
	data, sims = 500, estimator = main_estimator,
	hypothesis = "two", weights = NULL, term = treatment_variable){
	
	print(paste0("Estimating observed effects on ",outcome))
	model <- estimator(outcome = outcome,
										 covariates = covariates, 
										 treatment_variable = treatment_variable,
										 subgroup = subgroup,
										 level = level, 
										 weights = weights,
										 data = data)
	
	observed_effect <- get_estimate(model = model, 
																	treatment_variable = term)
	print(paste0("Estimating effects on ",outcome," under the sharp null."))
	null_distribution <- 
		get_effects_under_null(outcome = outcome,
													 covariates = covariates, 
													 treatment_variable = treatment_variable,
													 term = term,
													 subgroup = subgroup,
													 weights = weights,
													 data = data, 
													 sims = sims)
	
	ri_pval <- get_pval(observed_effect = observed_effect,
											null_distribution = null_distribution,
										  hypothesis = hypothesis)
	
	ctrl_mean <- data[data[treatment_variable] == 0, outcome] %>% mean()
	ctrl_sd <- data[data[treatment_variable] == 0, outcome] %>% sd()
	
	return_fit <- 
		list(model = model, 
				 ri_pval = ri_pval,
				 hypothesis = hypothesis,
				 null_distribution = null_distribution,
				 sims = sims, 
				 treatment_variable = treatment_variable,
				 ctrl_mean = ctrl_mean,
				 ctrl_sd = ctrl_sd,
				 outcome = outcome)
	
	class(return_fit) <- "ri_fit"
	
	return(return_fit)
	
}

# Examples
if(FALSE){
	test <- get_estimate_and_pval(outcome = "control_index",
																covariates = get_covariates("control_index",
																														selected_covariates_couple), 
																level = "couple",
																data = mlw,
																sims = 5)
	
	get_estimate_and_pval(outcome = "dm_ladder",
												covariates = NULL, 
												level = "individual",
												data = mll,
												sims = 5)
	
	get_estimate_and_pval(outcome = "dm_ladder",
												covariates = c("woman"), 
												level = "individual",
												term = "Z:woman",
												data = mll,
												sims = 5)
}

# Multiple comparisons functions ------------------------------------------

# This function calculates the family-wise error rate for each 
# family of tests grouped by sim_ID

get_alpha_per_family <- function(simulations, alpha_per_test){
	simulations %>%
		mutate(test_rejection = p.value <= alpha_per_test) %>%
		group_by(sim_ID) %>%
		summarize(family_rejection = any(test_rejection,na.rm = TRUE)) %>%
		ungroup() %>%
		with(., mean(family_rejection))
}

# This function computes the testwise alpha that gets the familywise alpha
# as close as possible to some target, searching over a one-dimensional
# grid of candidate alphas
get_alpha <- function(simulations, 
											alpha_per_tests = seq(.001,.10,.0001),
											target_alpha = .05){
	alpha_per_familys <- sapply(alpha_per_tests, 
															get_alpha_per_family, 
															simulations = simulations)
	dist_from_goal <- (alpha_per_familys - target_alpha)^2
	new_alpha_per_test <- max(alpha_per_tests[which.min(dist_from_goal)])
	return(new_alpha_per_test)
}


# Wrapper for running mutliple models per sim -----------------------------

get_multiple_estimates_and_pvals <-  function(
	outcomes, covariates = NULL, level = "couple",
	subgroup = NULL, treatment_variable = "Z",
	data, sims = 500, estimator = main_estimator,
	hypothesis = "two"){

	if (length(outcomes) == 1) {
		stop("Use get_estimates_and_pval for a single outcome!")
	}
	
	names(outcomes) <- outcomes
	
	models <- map(outcomes, function(x) {
		print(paste0("Estimating observed effects on ", x))
		estimator(
			outcome = x,
			covariates = covariates,
			treatment_variable = treatment_variable,
			subgroup = subgroup,
			level = level,
			data = data
		) %>% tidy()
	}) %>% bind_rows()
	
	observed_effects <- 
		models %>% 
		filter(term == treatment_variable) %>% 
		pull(estimate)
	
	i <- 1
	
	null_distributions <- 
		replicate(n = sims, expr = {
			print(paste0("sim: ", i))
			i <<- i + 1
			data[, treatment_variable] <- rerandomize(data = data,
																								treatment_variable = treatment_variable)
			sapply(outcomes, function(x) {
				estimator(
					outcome = x,
					covariates = covariates,
					treatment_variable = treatment_variable,
					level = "RI",
					data = data
				) %>%
					get_estimate(treatment_variable = treatment_variable)
			}) 
		}, simplify = TRUE) %>% t() %>% as_tibble()
	

	ri_pvals <-
		map2(null_distributions, observed_effects, function (y, x) {
			get_pval(observed_effect = x,
							 null_distribution = y)
		}) %>% bind_rows()
	
	return_fit <- 
		list(models = models, 
				 ri_pvals = ri_pvals, 
				 hypothesis = hypothesis,
				 sims = sims, 
				 treatment_variable = treatment_variable,
				 outcomes = outcomes)
	
	class(return_fit) <- "ri_fits"
	
	return(return_fit)
}


if (FALSE) {
	library(tictoc)
	tic()
	get_multiple_estimates_and_pvals(
		outcome = c("pair_13_m", "education_m", "age_m"),
		covariates = NULL,
		level = "RI",
		data = blw,
		sims = 1000
	)
	toc()
	
	tic()
	map(c("pair_13_m", "education_m", "age_m"), function(x) {
		get_estimate_and_pval(
			outcome = x,
			covariates = NULL,
			level = "RI",
			data = blw,
			sims = 1000
		)
	})
	toc()
}




