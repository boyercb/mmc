# Core functions for the lasso analysis -----------------------------------

# This function performs k-fold cross-validation, calculating the 
# tuning parameter (lambda) with the lowest average (across folds)
# test error. 

lasso_cv <- function(outcome_name, covariates, data, N_folds = 30, ...){
	# glmnet only takes matrices
	Y <- as.matrix(data[ ,outcome_name])
	X <- as.matrix(data[ ,covariates])
	
	cv.glmnet(x = X, y = Y,
						# Use MSE minimization for CV
						type.measure = "deviance",
						# Number of folds
						nfolds = N_folds,
						# Alpha = 1 sets glmnet() to use lasso penalty
						alpha = 1,
						...)
}

# This function takes a set of cross-validated lasso models and returns the
# non-zero coefficients from the model that uses a lambda that minimizes the 
# mean cross-validated error

tidy_lasso_covariates <- function(lasso_fit, lambda = "lambda.min"){
	coefs <- coef(lasso_fit, s = lambda)
	coefs <- ifelse(is.list(coefs), 
									as.list(coefs),
									list(coefs))
	coefs %>% 
		do.call(what = cbind) %>% 
		rowMeans() %>% 
		data.frame() %>% 
		rownames_to_column() %>% 
		select(rowname, ".") %>% 
		rename(term = rowname, estimate = ".") %>% 
		filter(estimate > 0) %>% 
		filter(term != "(Intercept)")
}

# This function is a wrapper for lasso_cv and tidy_lasso_covariates that 
# performs cross-validated lasso sims times, averages the lambdas that 
# minimize CV error across the sims runs, and returns the covariates selected 
# using that average lambda

select_covariates <- function(outcome_name, covariates, data, N_folds = 30, sims = 10,...){
	print(paste0("Selecting covariates for ", outcome_name))
	# Do k-fold CV sims times, returning sims CV models
	lambdas <- lapply(X = 1:sims, 
										FUN = function(i) lasso_cv(outcome_name = outcome_name, 
																							 covariates = covariates, 
																							 data = data,
																							 N_folds = N_folds,
																							 ... = ...))
	# Get lambda that minimizes mean CV error for each
	min_lambdas <- sapply(lambdas, function(x) x$lambda.min)
	# Use the first model (doesn't matter which one) with 
	# average lambda to get covariates with optimal lambda
	tidy_lasso_covariates(
		lasso_fit = lambdas[[1]], 
		lambda = mean(min_lambdas)) %>% 
		mutate(outcome = outcome_name) %>% 
		select(outcome, term, estimate)
}


















