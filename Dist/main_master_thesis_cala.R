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

######################### DATA EXPLORATION #########################

###### EFFECT SUMMARY STATISTICS ######
if (run_this$effect_summary_stats){
  effect_sum_stats_list <- runCachedFunction(
    getEffectSummaryStats, user_params,
    verbose_function = getEffectSummaryStatsVerbose,
    data, var_list,
    conf.level = adj_params$effect_summary_stats_conf_level,
    formal_output = adj_params$formal_output_on
  )
  effect_sum_stats <- effect_sum_stats_list[[1]] # Also missing variable info
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
    # Main plot
    box_plot_list <- runCachedFunction(
      getLargeBoxPlot, user_params,
      verbose_function = getBoxPlotVerbose,
      data,
      max_boxes = adj_params$box_plot_max_boxes,
      verbose_on = adj_params$box_plot_verbose,
      export_html = user_params$export_html_graphs,
      output_folder = folder_paths$graphics_folder,
      factor_by = factor_name,
      effect_name = adj_params$effect_name,
      theme = user_params$theme,
      verbose = F # Internal function parameter - no doubling
    )
  }
}

###### FUNNEL PLOT ######
if (run_this$funnel_plot){
  run_cached_funnel <- function(use_medians, graph_name){
    return ( 
      runCachedFunction(
        getFunnelPlot, user_params,
        verbose_function = nullVerboseFunction,
        funnel_data,
        effect_proximity = adj_params$funnel_effect_proximity,
        maximum_precision = adj_params$funnel_maximum_precision,
        use_study_medians = use_medians,
        theme = user_params$theme,
        verbose = adj_params$funnel_verbose,
        export_html = user_params$export_html_graphs,
        output_path = graph_name
      )
    )
  }
  # Funnel with all data
  funnel_all_path <- paste0(folder_paths$graphics_folder, "funnel.html")
  funnel_all <- run_cached_funnel(use_medians = FALSE, graph_name = funnel_all_path)
  # Funnel with medians only
  funnel_medians_path <- paste0(folder_paths$graphics_folder, "funnel_medians.html")
  funnel_medians <- run_cached_funnel(use_medians = TRUE, graph_name = funnel_medians_path)
}

###### HISTOGRAM OF T-STATISTICS ######
if (run_this$t_stat_histogram){
  t_hist_path <- paste0(folder_paths$graphics_folder, "t_hist.html")
  t_hist_plot <- runCachedFunction( # Plot only if input changes
    getTstatHist, user_params,
    verbose_function = nullVerboseFunction,
    data,
    lower_cutoff = adj_params$t_hist_lower_cutoff,
    upper_cutoff = adj_params$t_hist_upper_cutoff,
    theme = user_params$theme,
    verbose = TRUE, # Print into console
    export_html = user_params$export_html_graphs,
    output_path = t_hist_path
  )
}

######################### LINEAR TESTS ######################### 

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

if (run_this$linear_tests){
  linear_tests_results <- runCachedFunction(
    getLinearTests, user_params,
    verbose_function = getLinearTestsVerbose,
    data
  )
  if (user_params$export_results){
    exportTable(linear_tests_results, user_params, "linear_tests")
  }
}

######################### NON-LINEAR TESTS ######################### 

if (run_this$nonlinear_tests){
  # Extract source script paths
  stem_script_path <- paste0(folder_paths$scripts_folder, script_files$stem_source)
  selection_script_path <- paste0(folder_paths$scripts_folder, script_files$selection_model_source)
  endo_script_path <- paste0(folder_paths$scripts_folder, script_files$endo_kink_source)
  nonlinear_script_paths <- list(stem=stem_script_path,
                                 selection=selection_script_path,
                                 endo=endo_script_path)
  # Parameters
  global_non_lin_res <- T # Set to false if tests should be ran separately
  # Estimation
  if (!global_non_lin_res) {
    ###### PUBLICATION BIAS - WAAP (Ioannidis et al., 2017) ######
    waap_results<- getWaapResults(data, pub_bias_present = F, verbose_coefs = T)
    
    ###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######
    top10_results <- getTop10Results(data, pub_bias_present = F, verbose_coefs= T)
    
    ###### PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019) #####
    stem_results <- getStemResults(data,
                                   script_path = stem_script_path,
                                   pub_bias_present = F, verbose_coefs= T)
    
    ###### PUBLICATION BIAS - FAT-PET hierarchical in R ######
    hier_results <- getHierResults(data, pub_bias_present = T, verbose_coefs= T)
    
    ###### PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019) ######
    selection_results <- getSelectionResults(data, 
                                             script_path = selection_script_path,
                                             cutoffs = c(1.960), symmetric = F, modelmu = "normal",
                                             pub_bias_present = T, verbose_coefs = T)
    
    ###### PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020) ######
    endo_kink_results <- getEndoKinkResults(data,
                                            script_path = endo_script_path,
                                            pub_bias_present = T, verbose_coefs = T)
  } else {
    # Get all results at once without assigning the output to any variables - unparametrizable
    nonlinear_tests_results <- runCachedFunction(
      getNonlinearTests, user_params, 
      verbose_function = getNonlinearTestsVerbose,
      data, script_paths = nonlinear_script_paths
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
    
    p_uni_results <- getPUniResults(data, method = adj_params$puni_method,
                                    effect_present=T, pub_bias_present=T, verbose_coefs=T)
    
  } else{
    exo_tests_results <- runCachedFunction(
      getExoTests, user_params,
      verbose_function = getExoTestsVerbose,
      data,
      puni_method = adj_params$puni_method
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
    verbose_function = getCaliperResultsVerbose,
    data,
    thresholds = adj_params$caliper_thresholds,
    widths = adj_params$caliper_widths,
    verbose = adj_params$caliper_verbose
  )
  
  ###### PUBLICATION BIAS - p-hacking test (Elliott et al., 2022) ######
  elliott_results <- runCachedFunction(
    getElliottResults, user_params,
    verbose_function = getElliottResultsVerbose,
    data,
    script_path = paste0(folder_paths$scripts_folder, script_files$elliott_source),
    temp_data_path = folder_paths$data_folder, # Store temp files here
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
    verbose_function = getMaiveResultsVerbose,
    data,
    script_path = paste0(folder_paths$scripts_folder, script_files$maive_source),
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
    bma_formula_list <- runCachedFunction(
      findOptimalBMAFormula, user_params,
      verbose_function = findOptimalBMAFormulaVerbose,
      data, var_list,
      verbose = adj_params$bma_verbose
    )
    bma_formula <- bma_formula_list[[1]] # Also verbose information
  } else {
    # From the variable information instead
    bma_formula <- runCachedFunction(
      getBMAFormula, user_params,
      verbose_function = nullVerboseFunction, # No verbose output
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
    verbose_function = nullVerboseFunction, # No verbose output
    data, var_list, bma_vars
  )
  bma_model <- runCachedFunction(
    runBMA, user_params,
    verbose_function = runBMAVerbose,
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
    verbose_function = extractBMAResultsVerbose,
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
    verbose_function = runFMAVerbose,
    bma_data, bma_model,
    verbose = adj_params$fma_verbose
  )
}

###### MODEL AVERAGING RESULTS PRESENTATION ######

# Print out the results of model averaging into a nice table - only if BMA and FMA output exists
if (adj_params$ma_results_table & (all(exists("bma_coefs"), exists("fma_coefs")))){
  ma_res_table <- runCachedFunction(
    getMATable, user_params,
    verbose_function = getMATableVerbose,
    bma_coefs, fma_coefs, var_list
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
    verbose_function = nullVerboseFunction, # No verbose output
    data, var_list,
    var_list,
    from_vector = F,
    include_reference_groups = T
  )
  ma_var_desc_table <- runCachedFunction( # Runs with winsorized data
    getMAVariablesDescriptionTable, user_params,
    verbose_function = getMAVariablesDescriptionTableVerbose,
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
    verbose_function = generateBPEResultTableVerbose,
    bpe_study_ids,
    data, var_list, bma_model, bma_formula, bma_data,
    use_ci = adj_params$bpe_use_ci,
    study_info_verbose = adj_params$bpe_study_info,
    verbose_output = adj_params$bpe_result_table_verbose
  )
  # Economic significance table
  bpe_est <- bpe_res[1,1] # BPE estimate of the first row - usually Author's BPE
  bpe_econ_sig <- runCachedFunction(
    getEconomicSignificance, user_params,
    verbose_function = getEconomicSignificanceVerbose,
    bpe_est, var_list, bma_data, bma_model,
    display_large_pip_only = adj_params$bpe_econ_sig_large_pip_only,
    verbose_output = adj_params$bpe_econ_sig_verbose
  )
  # Export
  if (user_params$export_results){
     exportTable(bpe_res, user_params, "bpe_res")
     exportTable(bpe_econ_sig, user_params, "bpe_econ_sig")
  }
}
 