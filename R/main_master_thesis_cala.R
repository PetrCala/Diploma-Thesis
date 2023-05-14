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
  "corrplot", # Graphical display of correlation matrices
  "data.table", # Fast data manipulation and aggregation
  "ddpcr", # Analysis of Droplet Digital PCR (ddPCR) data
  "dplyr", # Data manipulation and data wrangling
  "fdrtool", # Eliott et al. (2022)
  "foreign", # Reading and writing data stored by other statistical software
  "gdata", # Eliott et al. (2022)
  "grDevices", # Eliott et al. (2022)
  "ggplot2", # Creating graphics and data visualizations
  "haven", # Importing and exporting data from SAS, SPSS, and Stata
  "lmtest", # Hypothesis testing and diagnostics for linear regression models
  "meta", # Meta-analysis package
  "metafor", # Conducting meta-analyses
  "multcomp", # Simultaneous inference for general linear hypotheses
  "multiwayvcov", # Computing clustered covariance matrix estimators
  "NlcOptim", # Eliott et al. (2022) - CoxShi
  "plm", # Random Effects, Between Effects
  "puniform", # Computing the density, distribution function, and quantile function of the uniform distribution
  "pracma", # MAIVE Estimator, Eliott et al. (2022)
  "rddensity", # Eliott et al. (2022)
  "readr", # Reading data into R from various file formats
  "readxl", # Reading Excel files
  "sandwich", # Computing robust covariance matrix estimators, MAIVE estimator
  "shiny", # Andrew & Kasy (2019) Selection model
  "spatstat", # Eliott et al. (2022)
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
data <- preprocessData(data, var_list)

# Get the raw but preprocessed data summary statistics (without winsorization, missing value handling)
if (run_this$variable_summary_stats){
  getVariableSummaryStats(data, var_list)
}

# Handle missing variables
data <- handleMissingData(data, var_list, allowed_missing_ratio = adj_params$allowed_missing_ratio)

# Winsorize the data
data <- winsorizeData(data,
                      win_level = adj_params$data_winsorization_level,
                      precision_type = adj_params$data_precision_type)

# Validate the data types, correct values, etc. VERY restrictive. No missing values allowed until explicitly set.
validateData(data, var_list)

# Subset data using the conditions specified in the customizable section
subset_conditions <- getMultipleParams(adj_params, "data_subset_condition_") # Extract all the data subset conditions
data <- applyDataSubsetConditions(data, subset_conditions)

######################### DATA EXPLORATION #########################

###### EFFECT SUMMARY STATISTICS ######
if (run_this$effect_summary_stats){
  getEffectSummaryStats(data, var_list,
                        conf.level = adj_params$effect_summary_stats_conf_level,
                        formal_output = adj_params$formal_output_on)
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
  getLinearTests(data)
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
    getNonlinearTests(data)
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
    getExoTests(data)
  }
}

######################### P-HACKING TESTS #########################

if (run_this$p_hacking_tests){
  ###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######
  caliper_results <- getCaliperResults(data,
                                       thresholds = adj_params$caliper_thresholds,
                                       widths = adj_params$caliper_widths,
                                       verbose = T)
  
  ###### PUBLICATION BIAS - p-hacking test (Eliott et al., 2022) ######
  eliott_results <- getEliottResults(data,
                                     data_subsets = adj_params$eliott_data_subsets,
                                     p_min = adj_params$eliott_p_min,
                                     p_max = adj_params$eliott_p_max,
                                     d_point = adj_params$eliott_d_point,
                                     CS_bins = adj_params$eliott_CS_bins,
                                     verbose = adj_params$eliott_verbose)
  
  ###### MAIVE Estimator (Irsova et al., 2023) ######
  maive_results <- getMaiveResults(data,
                                   method=adj_params$maive_method,
                                   weight=adj_params$maive_weight,
                                   instrument=adj_params$maive_instrument,
                                   studylevel=adj_params$maive_studylevel,
                                   verbose=adj_params$maive_verbose)
}

######################### MODEL AVERAGING #########################

###### HETEROGENEITY - Bayesian Model Averaging in R ######
if (run_this$bma){
  if (adj_params$automatic_bma){
    # Get the optimal BMA formula automatically
    bma_formula <- findOptimalBMAFormula(data, var_list, verbose = adj_params$bma_verbose)
  } else {
    # From the variable information instead
    bma_formula <- getBMAFormula(var_list, input_data)
  }
  # Run the Variance Inflation Test
  vif_coefs <- runVifTest(bma_formula, data, print_all_coefs = adj_params$bma_verbose)
  # BMA estimation
  bma_vars <- all.vars(bma_formula) # Only variables - for data subsettings
  bma_data <- getBMAData(data, var_list, bma_vars)
  bma_model <- runBMA(
    bma_data,
    burn=adj_params$bma_burn,
    iter=adj_params$bma_iter,
    g=adj_params$bma_g, # UIP, BRIC, HQ
    mprior=adj_params$bma_mprior, # uniform, random
    nmodel=adj_params$bma_nmodel,
    mcmc=adj_params$bma_mcmc
  )
  # Print out the results
  bma_coefs <- extractBMAResults(bma_model, bma_data, print_results = adj_params$bma_print_results)
}

###### HETEROGENEITY - Frequentist model averaging code for R (Hansen) ######

if (run_this$fma){
  if (!exists("bma_data") || !exists("bma_model")){
    stop("You must create these two objects first - bma_data, bma_model. Refer to the 'bma' section.")
  }
  # Estimation
  fma_coefs <- runFMA(bma_data, bma_model, verbose = adj_params$fma_verbose)
}

###### MODEL AVERAGING RESULTS PRESENTATION ######

# Model averaging variable description table
if (run_this$ma_variables_description_table){
  # Get the table with new BMA data (including all reference groups and other excluded BMA variables)
  desc_table_data <- getBMAData(data, var_list,
                                var_list,
                                from_vector = F,
                                include_reference_groups = T)
  ma_var_desc_table <- getMAVariablesDescriptionTable(desc_table_data, var_list, # Runs with winsorized data
        verbose = adj_params$ma_variables_description_table_verbose) # Use View(...) for best viewing experience
  # Copy the table to a clipboard
  if (adj_params$ma_variables_description_table_clip){
    writeClipboard(capture.output(print(ma_var_desc_table, row.names=F))) 
  }
}

# Print out the results of model averaging into a nice table - only if BMA and FMA output exists
if (adj_params$ma_results_table & (all(exists("bma_coefs"), exists("fma_coefs")))){
  ma_res_table <- getMATable(bma_coefs, fma_coefs, var_list, verbose = T)
}

######################### BEST-PRACTICE ESTIMATE #########################

if (run_this$best_practice_estimate){
  if (!exists("bma_data") || !exists("bma_model") || !exists("bma_formula")){
    stop("You must create these two objects first - bma_data, bma_model, bma_formula. Refer to the 'bma' section.")
  }
  # Parameters
  bpe_study_ids <- getMultipleParams(adj_params, "bpe_studies")
  # BPE estimation
  bpe_res <- generateBPEResultTable(bpe_study_ids,
                                    data, var_list, bma_model, bma_formula, bma_data,
                                    use_ci = adj_params$bpe_use_ci,
                                    verbose_output = TRUE)
  # Economic significance table
  bpe_est <- bpe_res[1,1] # BPE estimate of the first row - usually Author's BPE
  bpe_econ_sig <- getEconomicSignificance(bpe_est, var_list, bma_data, bma_model,
                                          display_large_pip_only = adj_params$bpe_econ_sig_large_pip_only,
                                          verbose_output = TRUE)
}