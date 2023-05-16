#' |--------------------------|
#' Script name: main_master_thesis_cala.R
#' 
#' The main script for running the analysis for my Master Thesis on the topic
#' of 'Ability bias in returns to schooling: how large it is and why it matters?'
#' 
#' For detailed explanation, see the README file distributed with this script.
#' 
#' Author: Petr Čala
#' Year created: 2023
#' GitHub: github.com/PetrCala/
#' |--------------------------|

##################### ENVIRONMENT PREPARATION ########################

# Clean the environment - DO NOT CHANGE THIS
rm(list = ls()) 
options(scipen=999) # No scientific notation
set.seed(123) # Results reproduction, stochastic functions to deterministic for caching

# Static 
source_file <- "source_master_thesis_cala.R" # Main source file
user_param_file <- "user_parameters.yaml" # File with user parameters

# Working directory - change only if the script is being ran as the master script (not imported)
if (length(commandArgs()) == 0) {
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
  "car", # Variance Inflation Factor
  "DescTools", # Descriptive statistics and data analysis
  "LowRankQP", # Solving convex quadratic optimization problems
  "bayesm", # bayesian modeling and inference
  "cachem", # Cache system - creating and deleting cache files
  "corrplot", # Graphical display of correlation matrices
  "data.table", # Fast data manipulation and aggregation
  "ddpcr", # Analysis of Droplet Digital PCR (ddPCR) data
  "dplyr", # Data manipulation and data wrangling
  "fdrtool", # Elliott et al. (2022)
  "foreign", # Reading and writing data stored by other statistical software
  "gdata", # Elliott et al. (2022)
  "grDevices", # Elliott et al. (2022)
  "ggplot2", # Creating graphics and data visualizations
  "haven", # Importing and exporting data from SAS, SPSS, and Stata
  "lmtest", # Hypothesis testing and diagnostics for linear regression models
  "memoise", # Cache system - speeding up deterministic function calls
  "meta", # Meta-analysis package
  "metafor", # Conducting meta-analyses
  "multcomp", # Simultaneous inference for general linear hypotheses
  "multiwayvcov", # Computing clustered covariance matrix estimators
  "NlcOptim", # Elliott et al. (2022) - CoxShi
  "plm", # Random Effects, Between Effects
  "puniform", # Computing the density, distribution function, and quantile function of the uniform distribution
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

# Create temporary folders if they do not exist yet
validateFolderExistence(user_params$cache_path)
validateFolderExistence(user_params$export_path)

# Add the name-customizable files to the source file vector
source_files <- c(
  user_params$master_data_set_source,
  user_params$var_list_source,
  user_params$user_param_file,
  user_params$stem_source,
  user_params$selection_model_source,
  user_params$endo_kink_source,
  user_params$maive_source
)

# Validate .xlsx -> .csv conversion in development
if (user_params$development_on) {
  # Read multiple sheets from the master data set and write them as CSV files (overwriting existing files if necessary)
  master_data_set_xlsx_path <- "../Data/data_set_master_thesis_cala.xlsm"
  sheet_names <- c("data_set", "var_list") # Sheet names to read
  readExcelAndWriteCsv(master_data_set_xlsx_path, sheet_names)
}

# Validate all the necessary files
validateFiles(source_files)

######################### DATA PREPROCESSING #########################

# Read all the source .csv files
data <- readDataCustom(user_params$master_data_set_source)
var_list <- readDataCustom(user_params$var_list_source)

# Validate the input variable list
validateInputVarList(var_list)

# Preprocess data - validate dimensions, column names, etc.
data <- runCachedFunction(
  preprocessData, user_params,
  data, var_list
)

# Get the raw but preprocessed data summary statistics (without winsorization, missing value handling)
if (run_this$variable_summary_stats){
  variable_sum_stats <- runCachedFunction(
    getVariableSummaryStats, user_params,
    data, var_list
  )
  if (user_params$export_results){
    exportTable(variable_sum_stats, user_params, "variable_summary_stats")
  }
}


# Handle missing variables
data <- runCachedFunction(
  handleMissingData, user_params,
  data, var_list, allowed_missing_ratio = adj_params$allowed_missing_ratio
)

# Winsorize the data
data <- runCachedFunction(
  winsorizeData, user_params,
  data,
  win_level = adj_params$data_winsorization_level,
  precision_type = adj_params$data_precision_type
)

# Validate the data types, correct values, etc. VERY restrictive. No missing values allowed until explicitly set.
runCachedFunction(
  validateData, user_params,
  data, var_list
)

# Subset data using the conditions specified in the customizable section
subset_conditions <- getMultipleParams(adj_params, "data_subset_condition_") # Extract all the data subset conditions
data <- runCachedFunction(
  applyDataSubsetConditions, user_params, 
  data, subset_conditions
)

######################### DATA EXPLORATION #########################

###### EFFECT SUMMARY STATISTICS ######
if (run_this$effect_summary_stats){
  effect_sum_stats <- runCachedFunction(
    getEffectSummaryStats, user_params,
    data, var_list,
    conf.level = adj_params$effect_summary_stats_conf_level,
    formal_output = adj_params$formal_output_on
  )
  if (user_params$export_results){
    exportTable(effect_sum_stats, user_params, "effect_summary_stats")
  }
}

###### BOX PLOT ######
if (run_this$box_plot){
  # Parameters
  factor_names <- getMultipleParams(adj_params, "box_plot_group_by_factor_")
  
  # Run box plots for all these factors iteratively
  for (factor_name in factor_names){
    # Handle factors with large number of boxes - automatically split them into multiple plots
    if (factor_name %in% c("study_name", "study_id")){
      getLargeBoxPlot(data, max_studies = 60,
                      factor_by = factor_name,
                      verbose = adj_params$box_plot_verbose,
                      effect_name = adj_params$effect_name)
    } else {
      getBoxPlot(data, factor_by = factor_name,
                      verbose = adj_params$box_plot_verbose,
                      effect_name = adj_params$effect_name)
    }
  }
}

###### FUNNEL PLOT ######
if (run_this$funnel_plot){
  # Funnel with all data
  getFunnelPlot(data, adj_params$funnel_effect_proximity,
                adj_params$funnel_maximum_precision,
                use_study_medians = F,
                adj_params$funnel_verbose)
  # Funnel with medians only
  getFunnelPlot(data, adj_params$funnel_effect_proximity,
                adj_params$funnel_maximum_precision,
                use_study_medians = T,
                adj_params$funnel_verbose)
}

###### HISTOGRAM OF T-STATISTICS ######
if (run_this$t_stat_histogram){
  getTstatHist(data, 
               lower_cutoff = adj_params$t_hist_lower_cutoff,
               upper_cutoff = adj_params$t_hist_upper_cutoff)
}

######################### LINEAR TESTS ######################### 

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

if (run_this$linear_tests){
  linear_tests_results <- runCachedFunction(
    getLinearTests, user_params,
    data
  )
  if (user_params$export_results){
    exportTable(linear_tests_results, user_params, "linear_tests")
  }
}

######################### NON-LINEAR TESTS ######################### 

if (run_this$nonlinear_tests){
  # Parameters
  global_non_lin_res <- T # Set to false if tests should be ran separately
  # Estimation
  if (!global_non_lin_res) {
    ###### PUBLICATION BIAS - WAAP (Ioannidis et al., 2017) ######
    waap_results<- getWaapResults(data, pub_bias_present = F, verbose_coefs = T)
    
    ###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######
    top10_results <- getTop10Results(data, pub_bias_present = F, verbose_coefs= T)
    
    ###### PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019) #####
    stem_results <- getStemResults(data, pub_bias_present = F, verbose_coefs= T)
    
    ###### PUBLICATION BIAS - FAT-PET hierarchical in R ######
    hier_results <- getHierResults(data, pub_bias_present = T, verbose_coefs= T)
    
    ###### PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019) ######
    selection_results <- getSelectionResults(data, 
                                             cutoffs = c(1.960), symmetric = F, modelmu = "normal",
                                             pub_bias_present = T, verbose_coefs = T)
    
    ###### PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020) ######
    endo_kink_results <- getEndoKinkResults(data, pub_bias_present = T, verbose_coefs = T)
  } else {
    # Get all results at once without assigning the output to any variables - unparametrizable
    nonlinear_tests_results <- runCachedFunction(
      getNonlinearTests, user_params, 
      data
    )
    if (user_params$export_results){
      exportTable(nonlinear_tests_results, user_params, "nonlinear_tests")
    }
  }
}

### Apply p-uniform* method using sample means

######################### RELAXING THE EXOGENEITY ASSUMPTION ######################### 
if (run_this$exo_tests){
  # Parameters
  global_exo_tests <- T # Set to false if tests should be ran separately
  # Estimation
  if (!global_exo_tests) {
    ###### PUBLICATION BIAS - FAT-PET with IV ######
    iv_results <- getIVResults(data,
                               effect_present = T, pub_bias_present = T, verbose_coefs = T)
    
    p_uni_results <- getPUniResults(data, method = "ML",
                                    effect_present=T, pub_bias_present=T, verbose_coefs=T)
    
  } else{
    exo_tests_results <- runCachedFunction(
      getExoTests, user_params,
      data
    )
    if (user_params$export_results){
      exportTable(exo_tests_results, user_params, "exo_tests")
    }
  }
}

######################### P-HACKING TESTS #########################

if (run_this$p_hacking_tests){
  ###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######
  caliper_results <- runCachedFunction(
    getCaliperResults, user_params,
    data,
    thresholds = adj_params$caliper_thresholds,
    widths = adj_params$caliper_widths,
    verbose = T
  )
  
  ###### PUBLICATION BIAS - p-hacking test (Elliott et al., 2022) ######
  elliott_results <- runCachedFunction(
    getElliottResults, user_params,
    data,
    data_subsets = adj_params$elliott_data_subsets,
    p_min = adj_params$elliott_p_min,
    p_max = adj_params$elliott_p_max,
    d_point = adj_params$elliott_d_point,
    CS_bins = adj_params$elliott_CS_bins,
    verbose = adj_params$elliott_verbose
  )
  
  ###### MAIVE Estimator (Irsova et al., 2023) ######
  maive_results <- runCachedFunction(
    getMaiveResults, user_params,
    data,
    method=adj_params$maive_method,
    weight=adj_params$maive_weight,
    instrument=adj_params$maive_instrument,
    studylevel=adj_params$maive_studylevel,
    verbose=adj_params$maive_verbose
  )
  if (user_params$export_results){
     exportTable(caliper_results, user_params, "p_hacking_tests_caliper")
     exportTable(elliott_results, user_params, "p_hacking_tests_elliott")
     exportTable(maive_results, user_params, "p_hacking_tests_maive")
  }
}

######################### MODEL AVERAGING #########################

###### HETEROGENEITY - Bayesian Model Averaging in R ######
if (run_this$bma){
  if (adj_params$automatic_bma){
    # Get the optimal BMA formula automatically
    bma_formula <- runCachedFunction(
      findOptimalBMAFormula, user_params,
      data, var_list,
      verbose = adj_params$bma_verbose
    )
  } else {
    # From the variable information instead
    bma_formula <- runCachedFunction(
      getBMAFormula, user_params,
      var_list, input_data
    )
  }
  # Run the Variance Inflation Test
  vif_coefs <- runVifTest(bma_formula, data,
                          print_all_coefs = adj_params$bma_verbose, verbose = F)
  # BMA estimation
  bma_vars <- all.vars(bma_formula) # Only variables - for data subsettings
  bma_data <- runCachedFunction(
    getBMAData, user_params,
    data, var_list, bma_vars
  )
  bma_model <- runCachedFunction(
    runBMA, user_params,
    bma_data,
    burn=adj_params$bma_burn,
    iter=adj_params$bma_iter,
    g=adj_params$bma_g, # UIP, BRIC, HQ
    mprior=adj_params$bma_mprior, # uniform, random
    nmodel=adj_params$bma_nmodel,
    mcmc=adj_params$bma_mcmc
  )
  # Print out the results
  bma_coefs <- runCachedFunction(
    extractBMAResults, user_params,
    bma_model, bma_data,
    print_results = adj_params$bma_print_results
  )
}

###### HETEROGENEITY - Frequentist model averaging code for R (Hansen) ######

if (run_this$fma){
  if (!exists("bma_data") || !exists("bma_model")){
    stop("You must create these two objects first - bma_data, bma_model. Refer to the 'bma' section.")
  }
  # Estimation
  fma_coefs <- runCachedFunction(
    runFMA, user_params,
    bma_data, bma_model,
    verbose = adj_params$fma_verbose
  )
}

###### MODEL AVERAGING RESULTS PRESENTATION ######

# Print out the results of model averaging into a nice table - only if BMA and FMA output exists
if (adj_params$ma_results_table & (all(exists("bma_coefs"), exists("fma_coefs")))){
  ma_res_table <- runCachedFunction(
    getMATable, user_params,
    bma_coefs, fma_coefs, var_list,
    verbose = T
  )
  if (user_params$export_results){
     exportTable(ma_res_table, user_params, "ma")
   }
}

# Model averaging variable description table
if (run_this$ma_variables_description_table){
  # Get the table with new BMA data (including all reference groups and other excluded BMA variables)
  desc_table_data <- runCachedFunction(
    getBMAData, user_params,
    data, var_list,
    var_list,
    from_vector = F,
    include_reference_groups = T
  )
  ma_var_desc_table <- runCachedFunction( # Runs with winsorized data
    getMAVariablesDescriptionTable, user_params,
    desc_table_data, var_list,
    verbose = adj_params$ma_variables_description_table_verbose # Use View(...) for best viewing experience
  )
  if (user_params$export_results){
     exportTable(ma_var_desc_table, user_params, "ma_variables_description_table")
   }
}

######################### BEST-PRACTICE ESTIMATE #########################

if (run_this$bpe){
  if (!exists("bma_data") || !exists("bma_model") || !exists("bma_formula")){
    stop("You must create these three objects first - bma_data, bma_model, bma_formula. Refer to the 'bma' section.")
  }
  # Parameters
  bpe_study_ids <- getMultipleParams(adj_params, "bpe_studies")
  # BPE estimation
  bpe_res <- runCachedFunction(
    generateBPEResultTable, user_params,
    bpe_study_ids,
    data, var_list, bma_model, bma_formula, bma_data,
    use_ci = adj_params$bpe_use_ci,
    verbose_output = TRUE
  )
  # Economic significance table
  bpe_est <- bpe_res[1,1] # BPE estimate of the first row - usually Author's BPE
  bpe_econ_sig <- runCachedFunction(
    getEconomicSignificance, user_params,
    bpe_est, var_list, bma_data, bma_model,
    display_large_pip_only = adj_params$bpe_econ_sig_large_pip_only,
    verbose_output = TRUE
  )
  # Export
  if (user_params$export_results){
     exportTable(bpe_res, user_params, "bpe_res")
     exportTable(bpe_econ_sig, user_params, "bpe_econ_sig")
  }
}
 