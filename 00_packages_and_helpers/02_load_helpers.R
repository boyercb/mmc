# Load helper functions ---------------------------------------------------

get_data <- function(path) {
  paste0("../../Data/", path)
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))


source("00_packages_and_helpers/helpers_bootstrap.R")
source("00_packages_and_helpers/helpers_estimators.R")
source("00_packages_and_helpers/helpers_codebook.R")
source("00_packages_and_helpers/helpers_ri_functions.R")
source("00_packages_and_helpers/helpers_plot_functions.R")
