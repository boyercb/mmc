rm(list = ls())


# Define global analysis parameters ---------------------------------------

sims <- 10000

run_imputations <- TRUE 


# Packages and helper functions -------------------------------------------

source("00_packages_and_helpers/01_load_packages.R")

source("00_packages_and_helpers/02_load_helpers.R")


# Load data ---------------------------------------------------------------

source("01_load_data/01_load_baseline.R")

#source("01_load_data/02_load_randomization.R")

source("01_load_data/03_load_intervention.R")

source("01_load_data/04_load_endline.R")


# Data cleaning and preparation -------------------------------------------

source("02_clean_variables/01_make_replacements.R")

source("02_clean_variables/02_confirm_sample.R")

source("02_clean_variables/03_impute_missing_responses.R")

source("02_clean_variables/04_merge_datasets.R")

#source("02_clean_variables/05_impute_missing_partners.R")

source("02_clean_variables/06_create_variable_lists.R")

source("02_clean_variables/07_create_pooled_datasets.R")

source("02_clean_variables/08_scale_and_center.R")

source("02_clean_variables/09_subset_to_final_sample.R")



# Lasso covariate selection -----------------------------------------------

run_lasso <- FALSE
lasso_sims <- 10
lasso_folds <- 30

# Create lasso functions
source("03_covariate_selection/01_lasso_functions.R")

# Use cross-validated lasso to select covariates
source("03_covariate_selection/02_select_covariates.R")


# Main analyses -----------------------------------------------------------

source("04_main_analyses/01_primary_outcomes.R")
 
source("04_main_analyses/02_subitems.R")
 
source("04_main_analyses/03_plots.R")


# Secondary analyses ------------------------------------------------------

source("05_secondary_analyses/01_mens_secondary_outcomes.R")

source("05_secondary_analyses/02_womens_secondary_outcomes.R")


# Compliance analyses -----------------------------------------------------

source("06_compliance_analyses/01_complier_summary.R")

source("06_compliance_analyses/02_complier_covariate_balance.R")

source("06_compliance_analyses/03_complier_average_effects.R")


# Robustness checks -------------------------------------------------------

source("07_robustness_checks/01_outcome_summaries.R")

source("07_robustness_checks/02_covariate_balance.R")

source("07_robustness_checks/03_attrition.R")

source("07_robustness_checks/04_inference.R")

source("07_robustness_checks/05_pooled_outcomes.R")

source("07_robustness_checks/06_block_assignment_type.R")

source("07_robustness_checks/07_measurement.R")

source("07_robustness_checks/08_missing_data_models.R")

source("07_robustness_checks/09_bayes.R")


# compliance
# standard errors
# random vs. non-random blocks
# bayes
# balance
# pooled analysis
# demand effects
# complete case
