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

# Required packages - NA <=> fetch latest version (see 'loadPackages')
packages <- list(
  "AER" = NA, # Applied econometrics with R
  "BMS" = NA, # bayesian model averaging
  "DescTools" = NA, # Descriptive statistics and data analysis
  "bayesm" = NA, # bayesian modeling and inference
  "cachem" = NA, # Cache system - creating and deleting cache files
  "car" = NA, # Variance Inflation Factor
  "corrplot" = NA, # Graphical display of correlation matrices
  "data.table" = NA, # Fast data manipulation and aggregation
  "devtools" = NA, # Loading local packages
  "ddpcr" = NA, # Analysis of Droplet Digital PCR (ddPCR) data
  "fdrtool" = NA, # Elliott et al. (2022)
  "foreign" = NA, # Reading and writing data stored by other statistical software
  "gdata" = NA, # Elliott et al. (2022)
  "grDevices" = NA, # Elliott et al. (2022)
  "ggtext" = NA, # ggplot axis text without warnings
  "haven" = NA, # Importing and exporting data from SAS, SPSS, and Stata
  "ivmodel" = NA, # Instrumental variable confidence interval (Anderson-Rubin)
  "lmtest" = NA, # Hypothesis testing and diagnostics for linear regression models
  "memoise" = NA, # Cache system - speeding up deterministic function calls
  "meta" = NA, # Meta-analysis package
  "metafor" = NA, # Conducting meta-analyses
  "multcomp" = NA, # Simultaneous inference for general linear hypotheses
  "multiwayvcov" = NA, # Computing clustered covariance matrix estimators
  "NlcOptim" = NA, # Elliott et al. (2022) - CoxShi
  "plm" = NA, # Random Effects, Between Effects
  "plotly" = NA, # Interactive plots
  "png" = NA, # PNG plots
  "puniform" = '0.2.2', # Computing the density, distribution function, and quantile function of the uniform distribution
  "pracma" = NA, # MAIVE Estimator, Elliott et al. (2022)
  "RColorBrewer" = NA, # Plot colors
  "rddensity" = NA, # Elliott et al. (2022)
  "readxl" = NA, # Reading Excel files
  "RoBMA" = NA, # Robust BMA, Bartos et al. (2021)
  "sandwich" = NA, # Computing robust covariance matrix estimators, MAIVE estimator
  "shiny" = NA, # Andrew & Kasy (2019) Selection model
  "spatstat" = NA, # Elliott et al. (2022)
  "stats" = NA, # Statistical analysis and modeling
  "testthat" = NA, # Unit testing for R
  "tidyverse" = NA, # A collection of R packages designed for data science, including ggplot2, dplyr, tidyr, readr, purrr, and tibble
  "varhandle" = NA, # MAIVE estimator
  "xfun" = NA, # Proper ggplot label display
  "xtable" = NA, # Creating tables in LaTeX or HTML
  "yaml" = NA # User parameters
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

# Load devtools for package loading
if (!require('devtools')) install.packages('devtools'); library('devtools')

# Load packages
loadPackages(packages)

# Load user parameters and unlist for easier fetching
user_params <- yaml::read_yaml(user_param_file) 
source_file_params <- user_params$source_file_params # Parameters of the source data file
run_this <- user_params$run_this # Which parts of the script to run
adj_params <- user_params$adjustable_parameters # Various parameters
data_files <- user_params$data_files # Data files (only files names)
script_files <- user_params$script_files # Script files (only file names)
folder_paths <- user_params$folder_paths # Paths to various folders
export_options <- user_params$export_options # Various export options

# Validate folder existence
modifiable_folders <- c(
  folder_paths$cache_folder,
  folder_paths$temp_data_folder,
  folder_paths$graphic_results_folder,
  folder_paths$numeric_results_folder,
  folder_paths$all_results_folder
)
unmodifiable_folders <- c(
  source_file_params$source_data_folder,
  folder_paths$ext_package_folder
)
invisible(sapply(modifiable_folders, validateFolderExistence))
invisible(sapply(unmodifiable_folders, validateFolderExistence, require_existence = TRUE)) # No overwriting

# Clean result and data folders
folders_to_clean_forcefully <- c(
  folder_paths$numeric_results_folder
)
folders_to_clean_old_files_only <- c(
  folder_paths$graphic_results_folder,
  folder_paths$temp_data_folder
)
invisible(sapply(folders_to_clean_forcefully, cleanFolder, force = T)) # Clean all files
invisible(sapply(folders_to_clean_old_files_only, cleanFolder)) # Clean only files older than 1 hour

# Load external packages
loadExternalPackages(folder_paths$ext_package_folder)

# Define paths of all source files and save them in a vector for validation
data_files_full_paths <- paste0(folder_paths$data_folder, as.vector(unlist(data_files))) # Vector of "./data/file_name"
script_files_full_paths <- paste0(folder_paths$scripts_folder, as.vector(unlist(script_files))) # Vector of "./scripts/file_name"
all_source_files <- c(
  data_files_full_paths,
  script_files_full_paths
)

# Create the .csv data files from a source .xlsx/.xlsm file
csv_suffix <- source_file_params$csv_suffix # File suffix
source_data_path <- paste0(
  source_file_params$source_data_folder, # Folder
  source_file_params$file_name,   # File
  source_file_params$file_suffix  # Suffix
)
data_sheet_name <- source_file_params$data_sheet_name # Name of sheet with data
var_list_sheet_name <- source_file_params$var_list_sheet_name # Name of sheet with variable info
source_sheets <- c(
  data_sheet_name,
  var_list_sheet_name
)

# Read multiple sheets from the master data set and write them as CSV files (overwriting existing files)
readExcelAndWriteCsv(source_data_path, source_sheets, csv_suffix, temp_data_folder_path = folder_paths$temp_data_folder)

# Validate all the necessary files
validateFiles(all_source_files)

# Save the console output to a log file in the results folder
log_file_path <- paste0(folder_paths$all_results_folder, export_options$export_log_file)
quiet(sink()) # Empty the sink
sink(log_file_path, append = FALSE, split = TRUE) # Capture console output

######################### DATA PREPROCESSING #########################

# Construct the source file paths
data_path <- paste0(folder_paths$temp_data_folder, data_sheet_name, '_', csv_suffix, '.csv')
var_list_path <- paste0(folder_paths$temp_data_folder, var_list_sheet_name, '_', csv_suffix, '.csv')

# Read all the source .csv files
data_separators <- identifyCsvSeparators(data_path) # Read on the larger file for higher likelihood of identification
data <- readDataCustom(data_path, separators = data_separators)
var_list <- readDataCustom(var_list_path, separators = data_separators)

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
  if (export_options$export_results){
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
subset_conditions <- getMultipleParams(adj_params, "data_subset_condition_", T) # Extract all the data subset conditions
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
  if (export_options$export_results){
    exportTable(effect_sum_stats, user_params, "effect_summary_stats")
  }
}

###### PRIMA FACIE GRAPHS ######
if (run_this$prima_facie_graphs){
  print("Generating the prima facie graphs...")
  prima_facie_graphs <- runCachedFunction(
    getPrimaFacieGraphs, user_params,
    verbose_function = nullVerboseFunction,
    data, var_list,
    prima_factors = adj_params$prima_factors,
    prima_type = adj_params$prima_type,
    prima_hide_outliers = adj_params$prima_hide_outliers,
    prima_bins = adj_params$prima_bins,
    theme = export_options$theme,
    export_graphics = export_options$export_graphics,
    graphic_results_folder_path = folder_paths$graphic_results_folder,
    prima_scale = adj_params$prima_scale,
    prima_legend_font_size = adj_params$prima_legend_font_size
  )
}

###### BOX PLOT ######
if (run_this$box_plot){
  # Parameters
  factor_names <- getMultipleParams(adj_params, "box_plot_group_by_factor_")
  factor_names <- factor_names[!is.na(factor_names)] # Only non-NA factors
  # Run box plots for all these factors iteratively
  for (factor_name in factor_names){
    # Main plot
    box_plot_list <- runCachedFunction(
      getLargeBoxPlot, user_params,
      verbose_function = getBoxPlotVerbose,
      data,
      max_boxes = adj_params$box_plot_max_boxes,
      verbose_on = adj_params$box_plot_verbose,
      export_graphics = export_options$export_graphics,
      graph_scale = adj_params$box_plot_graph_scale,
      output_folder = folder_paths$graphic_results_folder,
      factor_by = factor_name,
      effect_name = adj_params$effect_name,
      theme = export_options$theme,
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
        precision_to_log = adj_params$funnel_precision_to_log,
        effect_proximity = adj_params$funnel_effect_proximity,
        maximum_precision = adj_params$funnel_maximum_precision,
        use_study_medians = use_medians,
        add_zero = adj_params$funnel_add_zero,
        theme = export_options$theme,
        verbose = adj_params$funnel_verbose,
        export_graphics = export_options$export_graphics,
        output_path = graph_name,
        graph_scale = adj_params$funnel_graph_scale
      )
    )
  }
  # Funnel with all data
  funnel_all_path <- paste0(folder_paths$graphic_results_folder, "funnel.png")
  funnel_all <- run_cached_funnel(use_medians = FALSE, graph_name = funnel_all_path)
  # Funnel with medians only
  funnel_medians_path <- paste0(folder_paths$graphic_results_folder, "funnel_medians.png")
  funnel_medians <- run_cached_funnel(use_medians = TRUE, graph_name = funnel_medians_path)
}

###### HISTOGRAM OF T-STATISTICS ######

if (run_this$t_stat_histogram){
  t_hist_path <- paste0(folder_paths$graphic_results_folder, "t_hist.png")
  t_hist_plot <- runCachedFunction( # Plot only if input changes
    getTstatHist, user_params,
    verbose_function = nullVerboseFunction,
    data,
    lower_cutoff = adj_params$t_hist_lower_cutoff,
    upper_cutoff = adj_params$t_hist_upper_cutoff,
    highlight_mean = adj_params$t_hist_highlight_mean,
    add_density = adj_params$t_hist_add_density,
    t_stats = adj_params$t_hist_t_stats,
    theme = export_options$theme,
    verbose = TRUE, # Print into console
    export_graphics = export_options$export_graphics,
    output_path = t_hist_path,
    graph_scale = adj_params$t_hist_graph_scale
  )
}

######################### LINEAR TESTS ######################### 

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

if (run_this$linear_tests){
  linear_tests_results <- runCachedFunction(
    getLinearTests, user_params,
    verbose_function = getLinearTestsVerbose,
    data,
    add_significance_marks = adj_params$linear_add_significance_marks
  )
  if (export_options$export_results){
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
  selection_params <- getMultipleParams(adj_params, "non_linear_param_selection_",T,T)
  # Estimation
  nonlinear_tests_results <- runCachedFunction(
    getNonlinearTests, user_params, 
    verbose_function = getNonlinearTestsVerbose,
    data, script_paths = nonlinear_script_paths,
    add_significance_marks = adj_params$non_linear_add_significance_marks,
    selection_params = selection_params,
    theme = export_options$theme,
    export_graphics = export_options$export_graphics,
    export_path = folder_paths$graphic_results_folder,
    graph_scale = adj_params$non_linear_stem_graph_scale,
    stem_legend_pos = adj_params$non_linear_stem_legend_position
  )
  if (export_options$export_results){
    exportTable(nonlinear_tests_results, user_params, "nonlinear_tests")
  }
}

### Apply p-uniform* method using sample means

######################### RELAXING THE EXOGENEITY ASSUMPTION ######################### 
if (run_this$exo_tests){
  # Parameters
  puni_params <- getMultipleParams(adj_params, "puni_param_",T,T)
  # Estimation
  exo_tests_results_list <- runCachedFunction(
    getExoTests, user_params,
    verbose_function = getExoTestsVerbose,
    data,
    puni_params = puni_params,
    iv_instrument = adj_params$iv_instrument,
    add_significance_marks = adj_params$exo_add_significance_marks
  )
  exo_tests_results <- exo_tests_results_list[[1]]
  if (export_options$export_results){
    exportTable(exo_tests_results, user_params, "exo_tests")
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
    verbose = adj_params$caliper_verbose,
    add_significance_marks = adj_params$caliper_add_significance_marks
  )
  
  ###### PUBLICATION BIAS - p-hacking test (Elliott et al., 2022) ######
  elliott_results <- runCachedFunction(
    getElliottResults, user_params,
    verbose_function = getElliottResultsVerbose,
    data,
    script_path = paste0(folder_paths$scripts_folder, script_files$elliott_source),
    temp_data_path = paste0(folder_paths$temp_data_folder),
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
    verbose=adj_params$maive_verbose,
    add_significance_marks = adj_params$maive_add_significance_marks
  )
  if (export_options$export_results){
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
    input_vars <- var_list$var_name[var_list$bma]
    bma_formula <- runCachedFunction(
      getBMAFormula, user_params,
      verbose_function = nullVerboseFunction, # No verbose output
      input_vars, data
    )
    # Run the Variance Inflation Test
    vif_coefs <- runVifTest(bma_formula, data,
                            print_all_coefs = adj_params$bma_verbose, verbose = T)
  }
  # BMA estimation
  bma_vars <- all.vars(bma_formula) # Only variables - for data subsettings
  bma_data <- runCachedFunction(
    getBMAData, user_params,
    verbose_function = nullVerboseFunction, # No verbose output
    data, var_list, bma_vars,
    scale_data = adj_params$bma_scale_data
  )
  raw_bma_params <- getMultipleParams(adj_params, "bma_param_",T,T)
  bma_params <- handleBMAParams(raw_bma_params) # Split into lists, each for a single model
  # Run a BMA estimation for each parameter list - iterate last to first, where index == 1 is main model
  bma_models <- list() # All BMA model objects stored here
  bma_all_coefs <- list() # All BMA coefficients stored here
  for (i in length(bma_params):1){
    model_params <- bma_params[[i]]
    # Estimate the model
    print(paste("Running Bayesian Model Averaging with",model_params$g, "g-prior and",model_params$mprior, "model prior..."))
    bma_model <- runCachedFunction(
      runBMA, user_params,
      verbose_function = nullVerboseFunction,
      bma_data,
      bma_params = model_params
    )
    # Extract the coefficients and plot graphs
    bma_coefs <- runCachedFunction(
      extractBMAResults, user_params,
      verbose_function = extractBMAResultsVerbose,
      bma_model, bma_data, var_list,
      print_results = adj_params$bma_print_results,
      adjustable_theme = adj_params$bma_adjustable_theme,
      theme = export_options$theme,
      export_graphics = export_options$export_graphics,
      export_path = folder_paths$graphic_results_folder,
      graph_scale = adj_params$bma_graph_scale
    )
    # Save the results
    bma_models[[i]] <- bma_model
    bma_all_coefs[[i]] <- bma_coefs
  }
  # Add a BMA comparison graph
  graphBMAComparison(
    bma_models, var_list,
    theme = export_options$theme,
    verbose = T,
    export_graphics = export_options$export_graphics,
    export_path = folder_paths$graphic_results_folder,
    graph_scale = adj_params$bma_comparison_graph_scale
  )
  # Store the bma data in the temporary data folder
  if (export_options$export_bma_data){
    bma_data_path <- paste0(folder_paths$temp_data_folder, 'bma_data', '_', csv_suffix, '.csv')
    write_csv(bma_data, bma_data_path)
  }
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
    bma_data, bma_model, var_list,
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
  if (export_options$export_results){
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
    scale_data = F, # Display original values
    from_vector = F,
    include_reference_groups = T
  )
  ma_var_desc_table <- runCachedFunction( # Runs with winsorized data
    getMAVariablesDescriptionTable, user_params,
    verbose_function = getMAVariablesDescriptionTableVerbose,
    desc_table_data, var_list,
    verbose = adj_params$ma_variables_description_table_verbose # Use View(...) for best viewing experience
  )
  if (export_options$export_results){
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
  if ("all" %in% bpe_study_ids){
    print("Running the best practice estimate for all studies. This may take some time...")
  }
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
  bpe_df <- bpe_res$bpe_df
  # Economic significance table
  bpe_est <- bpe_df[1,1] # BPE estimate of the first row - usually Author's BPE
  bpe_econ_sig <- runCachedFunction(
    getEconomicSignificance, user_params,
    verbose_function = getEconomicSignificanceVerbose,
    bpe_est, var_list, bma_data, bma_model,
    display_large_pip_only = adj_params$bpe_econ_sig_large_pip_only,
    verbose_output = adj_params$bpe_econ_sig_verbose
  )
  # Export
  if (export_options$export_results){
    bpe_res_name <- ifelse("all" %in% bpe_study_ids, "bpe_res_all_studies", "bpe_res")
    exportTable(bpe_df, user_params, bpe_res_name)
    exportTable(bpe_econ_sig, user_params, "bpe_econ_sig")
  }
}

###### BPE GRAPHS ######
if (run_this$bpe_graphs){
  if (!exists("bpe_df")){
    stop("You must run BPE first before you construct the summary statistic tables.")
  }
  bpe_graphs <- runCachedFunction(
    graphBPE, user_params,
    verbose_function = nullVerboseFunction,
    bpe_df, data, var_list,
    bpe_factors = adj_params$bpe_factors,
    graph_type = adj_params$bpe_graphs_type,
    theme = export_options$theme,
    export_graphics = export_options$export_graphics,
    graphic_results_folder_path = folder_paths$graphic_results_folder,
    bpe_graphs_scale = adj_params$bpe_graphs_scale
  )
}

###### BPE SUMMARY STATISTICS ######
if (run_this$bpe_summary_stats){
  if (!exists("bpe_df")){
    stop("You must run BPE first before you construct the summary statistic tables.")
  }
  bpe_sum_stats <- runCachedFunction(
    getBPESummaryStats, user_params,
    verbose_function = getBPESummaryStatsVerbose,
    bpe_df, data, var_list,
    bpe_factors = adj_params$bpe_factors,
    conf.level = adj_params$bpe_summary_stats_conf_level
  )
  if (export_options$export_results){
    exportTable(bpe_sum_stats, user_params, "bpe_summary_stats")
  }
}

######################### ROBUST BAYESIAN MODEL AVERAGING #########################

# Source:  https://github.com/FBartos/RoBMA
if (run_this$robma){
  robma_params <- getMultipleParams(adj_params, "robma_param_",T,T)
  print("Running Robust BMA. This may take some time...")
  robma_res <- runCachedFunction(
    getRoBMA, user_params,
    verbose_function = getRoBMAVerbose,
    data,
    verbose = adj_params$robma_verbose,
    add_significance_marks = adj_params$robma_add_significance_marks,
    robma_params
  )
  # Export
  if (export_options$export_results){
     exportTable(robma_res$Components, user_params, "robma_components")
     exportTable(robma_res$Estimates, user_params, "robma_estimates")
  }
}

### EXPORT ###

# Zip the results
if (export_options$export_results){
  # Create the file
  zipFolders(
    zip_name = export_options$export_zip_name,
    dest_folder = folder_paths$all_results_folder,
    folder_paths$temp_data_folder,
    folder_paths$graphic_results_folder,
    folder_paths$numeric_results_folder,
    log_file_path
  )
}

# Close the connection to the file
sink()