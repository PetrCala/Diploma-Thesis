##################### ENVIRONMENT PREPARATION ########################

# Clean the environment - DO NOT CHANGE THIS
rm(list = ls()) 
options(scipen=999) # No scientific notation
set.seed(123) # Results reproduction, stochastic functions to deterministic for caching

# Static 
source_file <- "source_master_thesis_cala.R" # Main source file
user_param_file <- "user_parameters.yaml" # File with user parameters

# Working directory - change only if the script is being ran interactively
if(interactive()) {
  if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi')
  if (! getwd() == dirname(getActiveDocumentContext()$path)){
    setwd(dirname(getActiveDocumentContext()$path)) # Set WD to the current file location
    print(paste0('Setting the working directory to: ', getwd()))
  }
}

# Required packages
packages <- c(
  "AER", # Applied econometrics with R
  "BMS", # bayesian model averaging
  "DescTools", # Descriptive statistics and data analysis
  "bayesm", # bayesian modeling and inference
  "cachem", # Cache system - creating and deleting cache files
  "car", # Variance Inflation Factor
  "corrplot", # Graphical display of correlation matrices
  "data.table", # Fast data manipulation and aggregation
  "devtools", # Loading local packages
  "ddpcr", # Analysis of Droplet Digital PCR (ddPCR) data
  "dplyr", # Data manipulation and data wrangling
  "fdrtool", # Elliott et al. (2022)
  "foreign", # Reading and writing data stored by other statistical software
  "gdata", # Elliott et al. (2022)
  "grDevices", # Elliott et al. (2022)
  "ggplot2", # Creating graphics and data visualizations
  "ggtext", # ggplot axis text without warnings
  "haven", # Importing and exporting data from SAS, SPSS, and Stata
  "lmtest", # Hypothesis testing and diagnostics for linear regression models
  "memoise", # Cache system - speeding up deterministic function calls
  "meta", # Meta-analysis package
  "metafor", # Conducting meta-analyses
  "multcomp", # Simultaneous inference for general linear hypotheses
  "multiwayvcov", # Computing clustered covariance matrix estimators
  "NlcOptim", # Elliott et al. (2022) - CoxShi
  "plm", # Random Effects, Between Effects
  "plotly", # Interactive plots
  "puniform", # Computing the density, distribution function, and quantile function of the uniform distribution
  "purrr", # Smart tables, study size
  "pracma", # MAIVE Estimator, Elliott et al. (2022)
  "rddensity", # Elliott et al. (2022)
  "readr", # Reading data into R from various file formats
  "readxl", # Reading Excel files
  "sandwich", # Computing robust covariance matrix estimators, MAIVE estimator
  "shiny", # Andrew & Kasy (2019) Selection model
  "spatstat", # Elliott et al. (2022)
  "stats", # Statistical analysis and modeling
  "testthat", # Unit testing for R
  "tidyverse", # A collection of R packages designed for data science, including ggplot2, dplyr, tidyr, readr, purrr, and tibble
  "varhandle", # MAIVE estimator
  "xfun", # Proper ggplot label display
  "xtable", # Creating tables in LaTeX or HTML
  "yaml" # User parameters
)

##### PREPARATION #####

# Verify the existence of the two main source files exist
if (!file.exists(source_file)){
  stop(paste("Please make sure to place the source file", source_file, "in the working directory first."))
} else {
  "Source file located."
}
if (!file.exists(user_param_file)){
  stop(paste("Please make sure to place the user parameter file",user_param_file,"in the working directory first."))
} else {
  "User parameter file located."
}

# Load the source script
source(source_file)

# Load packages
loadPackages(packages)

# Load user parameters and unlist for easier fetching
user_params <- yaml::read_yaml(user_param_file) 
run_this <- user_params$run_this # Which parts of the scipt to run
adj_params <- user_params$adjustable_parameters # Various parameters
data_files <- user_params$data_files # Data files (only files names)
script_files <- user_params$script_files # Script files (only file names)
folder_paths <- user_params$folder_paths # Paths to various folders

# Validate folder existence
validateFolderExistence(folder_paths$cache_folder)
validateFolderExistence(folder_paths$data_folder, require_existence = T) # No overwriting
validateFolderExistence(folder_paths$graphics_folder)
validateFolderExistence(folder_paths$export_folder)
validateFolderExistence(folder_paths$ext_package_folder, require_existence = T) # No overwriting

# Load external packages
loadExternalPackages(folder_paths$ext_package_folder)

# Define paths of all source files and save them in a vector for validation
data_files_full_paths <- paste0(folder_paths$data_folder, as.vector(unlist(data_files))) # Vector of "./data/file_name"
script_files_full_paths <- paste0(folder_paths$scripts_folder, as.vector(unlist(script_files))) # Vector of "./scripts/file_name"
all_source_files <- c(
  data_files_full_paths,
  script_files_full_paths
)

# In development, create the .csv data files from a source .xlsx/.xlsm file
if (user_params$development_on) {
  dev_params <- user_params$development_params
  xlsx_folder <- dev_params$xlsx_data_folder # Folder
  xlsx_file <- dev_params$xlsx_data_name # File
  xlsx_path <- paste0(xlsx_folder, xlsx_file) # Full path
  sheet_names <- dev_params$xlsx_sheet_names
  csv_suffix <- dev_params$csv_suffix
  # Read multiple sheets from the master data set and write them as CSV files (overwriting existing files)
  readExcelAndWriteCsv(xlsx_path, sheet_names, csv_suffix, data_folder_path = folder_paths$data_folder)
}

# Validate all the necessary files
validateFiles(all_source_files)

######################### DATA PREPROCESSING #########################

# Read all the source .csv files
data <- readDataCustom(paste0(folder_paths$data_folder, data_files$master_data_set_source))
var_list <- readDataCustom(paste0(folder_paths$data_folder, data_files$var_list_source))

# Validate the input variable list
validateInputVarList(var_list)

# Preprocess data - validate dimensions, column names, etc.
data <- runCachedFunction(
  preprocessData, user_params,
  verbose_function = preprocessDataVerbose,
  data, var_list
)

# Get the raw but preprocessed data summary statistics (without winsorization, missing value handling)
if (run_this$variable_summary_stats){
  variable_sum_stats_list <- runCachedFunction(
    getVariableSummaryStats, user_params,
    verbose_function = getVariableSummaryStatsVerbose,
    data, var_list
  )
  variable_sum_stats <- variable_sum_stats_list[[1]] # Also missing data information
  if (user_params$export_results){
    exportTable(variable_sum_stats, user_params, "variable_summary_stats")
  }
}

# Handle missing variables
data <- runCachedFunction(
  handleMissingData, user_params,
  verbose_function = handleMissingDataVerbose,
  data, var_list, allowed_missing_ratio = adj_params$allowed_missing_ratio
)

# Validate the data types, correct values, etc. VERY restrictive. No missing values allowed until explicitly set.
data <- runCachedFunction(
  validateData, user_params,
  verbose_function = validateDataVerbose,
  data, var_list
)

# Rename source columns to fit the script expected colnames
renamed_list <- runCachedFunction(
  renameUserColumns, user_params,
  verbose_function = renameUserColumnsVerbose,
  data, var_list,
  user_params$required_cols,
  precision_type = adj_params$data_precision_type
)
data <- renamed_list[[1]] # Renamed column names
var_list <- renamed_list[[2]] # Renamed var_name vector
funnel_data <- data # A copy of the data frame before winsoriaztion - used in funnel plot

# Winsorize the data
data <- runCachedFunction(
  winsorizeData, user_params,
  verbose_function = winsorizeDataVerbose,
  data,
  win_level = adj_params$data_winsorization_level,
  winsorize_precision = adj_params$winsorize_precision
)

# Subset data using the conditions specified in the customizable section
subset_conditions <- getMultipleParams(adj_params, "data_subset_condition_") # Extract all the data subset conditions
data <- runCachedFunction(
  applyDataSubsetConditions, user_params, 
  verbose_function = applyDataSubsetConditionsVerbose,
  data, subset_conditions
)

##################### DEBUG CENTER ########################

test_data <- data[1,]
i <- 2
while (i * 10 < nrow(data)){
  new_data <- data[i,]
  test_data <- rbind(test_data, new_data)
  i <- i + 1
}

test_data <- data # No subsetting

stem_param <- c(
  10^(-4), # Tolerance - set level of sufficiently small stem to determine convergence
  10^3 # max_N_count - set maximum number of iteration before termination
)

weighted_mean_squared_orig <- function(beta, se, sigma){
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  for (i in 2:N){
    Y1 <- sum(WB[1:i,1:i]) - sum(weights_beta[1:i]^2)
    Y2 <- sum(W[1:i,1:i]) - sum(weights[1:i]^2)
    Y[i] <- Y1/Y2
  }
  return(Y)
}


compute_submat_sums <- function(mat) {
  # calculate cumulative sums over rows and columns
  mat_cumsum <- apply(mat, 2, cumsum) # Cumulative sum over columns
  
  indices <- 1:nrow(mat_cumsum)
  rows <- split(mat_cumsum, row(mat_cumsum))  # split the matrix by row
  
  # Iterate over rows
  result <- mapply(function(r, i) {
    sum(r[1:i])
  }, r = rows, i = indices)
  
  return(as.vector(result))
}


weighted_mean_squared_new <- function(beta, se, sigma){
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  W_csum <- compute_submat_sums(W)
  WB_csum <- compute_submat_sums(WB)
  
  # Create cumulative sum of weights_beta squared - works fine
  weights_beta_csum <- cumsum(weights_beta^2)
  weights_csum <- cumsum(weights^2)
  
  # Preallocate vector
  Y1 <- numeric(N - 1)
  Y2 <- numeric(N - 1)
  
  # Perform the operation
  Y1 <- WB_csum - weights_beta_csum
  Y2 <- W_csum - weights_csum
  Y <- Y1 / Y2
  Y[1] <- 0
  
  return(Y)
}


source("./scripts/stem_method_master_thesis_cala.R")

# Time the run of the new function
time_df <- data.frame()

# Run the command 10 times and store elapsed times
for (i in 1:10) {
  result <- system.time(
    stem1 <- stem(test_data$effect, test_data$se, stem_param)$estimates
  )
  
  # Append elapsed time to the data frame
  time_df <- rbind(time_df, data.frame(Time = result["elapsed"]))
}
print(time_df)
print(paste("Mean elapsed time:",round(mean(time_df$Time), 2)))

# Results:

#Out:
#  > print(stem2)
#estimate       se sd of total heterogeneity n_stem n_iteration multiple % info used
#[1,]      7.2 1.231869                  2.133449      3           2        0 0.002256697
#
#> print(time_df)
#Time
#elapsed   3.07
#elapsed1  3.09
#elapsed2  2.98
#elapsed3  2.60
#elapsed4  2.77
#elapsed5  2.78
#elapsed6  3.17
#elapsed7  2.60
#elapsed8  2.64
#elapsed9  3.16
#elapsed10 2.62
#elapsed11 2.97
#elapsed12 3.45
#elapsed13 2.23
#elapsed14 3.07
#elapsed15 2.83
#elapsed16 2.75
#elapsed17 2.82
#elapsed18 2.63
#elapsed19 2.65
#> print(paste("Mean elapsed time:",round(mean(time_df$Time), 2)))
#[1] "Mean elapsed time: 2.84"

