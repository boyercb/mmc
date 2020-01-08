# Need to build this table out with a bit more functionality

make_table <- function(fits, model_names = NULL, add_rows = NULL,...){
	if(class(fits) == "ri_fit"){
		fits <- list(fits)
	} 
	
	if(class(fits) == "list"){
		(sapply(fits, class) == "ri_fit") %>% 
			all() %>% 
			stopifnot()
	}
	
	ctrl_means <- sapply(fits, get, x = "ctrl_mean")
	ctrl_sds <- sapply(fits, get, x = "ctrl_sd")
	
	ri_pvals <- sapply(fits, get, x = "ri_pval")
	ri_hyps <- sapply(fits, get, x = "hypothesis")

	treatment_variables <- sapply(fits, get, x = "treatment_variable")
	models <- lapply(fits, get, x = "model")
	
	pval_lists <- lapply(models, 
											 function(x) tidy(x) %>% select(p.value,term))
	
	
	for(i in 1:length(fits)){
		# replace pval on treatment with RI pvalues
		pval_lists[[i]]$p.value[pval_lists[[i]]$term == treatment_variables[i]] <- ri_pvals[i]
	}
	pval_lists <- lapply(pval_lists, get, x = "p.value")
	
	
	if(is.null(model_names)){
		model_names <- sapply(models, get, x = "outcome")
	} 
	
	if(length(models) == 1){
		models <- models[[1]]
	}

	add_rows <- c(
		list("Control Mean" = ctrl_means),
		list("Control SD" = ctrl_sds),
		list("RI p-value" = ri_pvals),
		list("Hypothesis" = hypotheses(ri_hyps)),
		add_rows)
	
	suppressWarnings(texreg(l = models,
				 omit.coef = "_c", 
				 stars = c(0.01, 0.05, 0.1), 
				 digits = 3,
				 include.ci = FALSE,
				 custom.model.names = model_names,
				 override.pvalues = pval_lists,
				 table = FALSE,
				 include.rsquared = FALSE,
				 include.rmse = FALSE,
				 custom.gof.rows = add_rows,
				 ... = ...)) %>%  print()
}

# Examples
if(FALSE){
	test1 <- get_estimate_and_pval(outcome = "control_index",
																 covariates = get_covariates("control_index",
																 														selected_covariates_couple), 
																 level = "couple",
																 data = mlw,
																 sims = 5)
	
	make_table(fits = test1)
	
	test2 <- lapply(X = primary_outcomes, FUN = get_estimate_and_pval,
									covariates = NULL, level = "couple", data = mlw, 
									sims = 2)
	make_table(test2, 
						 custom.note = "%stars stuff",
						 custom.coef.names = "B1")
}


# Function for making ticks in tables -------------------------------------

ticks <- function(x){
	if(length(x) == 1){
		if(x > 1) x <- rep(1, x)
	}
	ifelse(x == 1, "\\checkmark","\\times")
} 

# Examples
if(FALSE){
	# Make alternating ticks and crosses
	ticks(c(0,1,0,1))
	# Make four ticks
	ticks(4)
	# Make 1 tick
	ticks(1)
	# Make 1 cross
	ticks(0)
}

# Function for listing hypotheses in tables -------------------------------

hypotheses <- function(x) {
	case_when(
		x %in% c("upper", "positive") ~ "+",
		x %in% c("lower", "negative") ~ "-",
		x %in% c("two") ~ "+/-"
	)
}

# #xamples
if (FALSE) {
	hypotheses("upper")
	hypotheses(c("upper", "lower", "two"))
}

# Function for making baseline table --------------------------------------

make_balance_table <- function(fits, data, treatment, ...) {
	treatment <- sym(treatment)
	
	if (class(fits) != "ri_fits") {
		stop("This function is made for use with -get_multiple_estimates_and_pvals- function.")
	}
	
	models <- fits$models
	
	variables <- fits$outcomes
	
	baseline_means <-
		data %>%
		select(variables, !! treatment) %>%
		group_by(!! treatment) %>%
		summarise_all(mean, na.rm = TRUE) %>%
		gather(variable, value, variables) %>%
		spread(!! treatment, value, sep = "_")
	
	model_results <-
		data.frame(
			variable = variables,
			estimate = models$estimate,
			p.value = t(fits$ri_pvals),
			stringsAsFactors = FALSE
		)
	
	balance_table <-
		baseline_means %>%
		left_join(model_results, by = "variable") %>%
		arrange(p.value)
	
	kable(
		x = balance_table,
		format = "latex",
		digits = 3,
		col.names = c("Covariate", "Control", 
									"Treatment", "Difference", "P-value"),
		align = c("l", "c", "c", "c", "c"),
		booktabs = TRUE,
		longtable = TRUE,
		...
	)
}

# Function for quick-checking which tables already added ------------------

get_tables_not_in_tex <- function(){
	text <- read.table(file = "07_midline_manuscript/B1.tex")
	text <- text %>% unlist() %>% as.character() %>% paste(collapse = " ")
	
	tables <- list.files("07_midline_manuscript/tables/")
	tables <- gsub(".tex","",tables)
	
	tables_in <- sapply(tables, grepl, x = text, ignore.case = TRUE)
	
	tables[!tables_in]
}
get_plots_not_in_tex <- function(){
	text <- read.table(file = "07_midline_manuscript/B1.tex")
	text <- text %>% unlist() %>% as.character() %>% paste(collapse = " ")
	
	figs <- list.files("07_midline_manuscript/figures/")
	figs <- gsub(".pdf","",figs)
	
	figs_in <- sapply(figs, grepl, x = text, ignore.case = TRUE)
	
	figs[!figs_in]
}

if(FALSE){
	get_tables_not_in_tex()
	get_plots_not_in_tex()
}



