rm(list = ls())

# Packages and helper functions -------------------------------------------

source("00_packages_and_helpers/01_load_packages.R")

source("00_packages_and_helpers/02_load_helpers.R")


# Load data ---------------------------------------------------------------

source("01_load_data/01_load_baseline.R")

source("01_load_data/02_load_randomization.R")

source("01_load_data/03_load_intervention.R")

source("01_load_data/04_load_endline.R")


# Data cleaning and preparation -------------------------------------------

source("02_clean_variables/01_make_replacements.R")

source("02_clean_variables/02_confirm_sample.R")

source("02_clean_variables/03_merge_datasets.R")

source("02_clean_variables/04_build_outcomes.R")

source("02_clean_variables/05_create_variable_lists.R")

source("02_clean_variables/06_create_pooled_datasets.R")


# Main analyses -----------------------------------------------------------

source("04_main_analyses/01_primary_outcomes.R")

source("04_main_analyses/02_pooled_outcomes.R")

source("04_main_analyses/03_subitems.R")

source("04_main_analyses/04_plots.R")



