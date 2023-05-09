#' Master script for my Diploma Thesis
#'  
#' Topic - Ability bias in the returns to schooling: How large it is and why it matters 
#' Author - Bc. Petr Čala
#' Year of defense - 2024
#' Supervisor - doc. PhDr. Zuzana Havránková Ph.D. 
#' 
#' PREREQUISITES:
#'  1. Make sure that your working directory contains the following files:
#'      -<NAME_OF_YOUR_DATA_FRAME>.csv (modifiable below)
#'      -elliot_master_thesis_cala.R
#'      -endo_kink_master_thesis_cala.R
#'      -main_master_thesis_cala.R
#'      -maive_master_thesis_cala.R
#'      -pretty_output_master_thesis_cala.R
#'      -selection_model_master_thesis_cala.R
#'      -source_master_thesis_cala.R
#'      -stem_method_master_thesis_cala.R
#'      -<NAME_OF_YOUR_VARIABLE_INFORMATION_FILE>.csv (modifiable below)
#'  2. Try to eliminate as many missing values in your data frame as you can.
#'    The script will automatically use interpolation for missing data, so that model averaging
#'    can run, but in case of many missing values, the results may be unstable.
#'  3. The data frame should contain these columns (named exactly as listed below):
#'    study_name - Name of the study, such as Einstein et al. (1935).
#'    study_id - ID of the study. Should be numeric and unique for each study.
#'    effect -  The main effect/estimate values. Ideally it should be  a transformed effect, such as
#'      the partial correlation coefficient.
#'    se - standard error of the effect
#'    t_stat - t-statistic of the main effect. Can be calculated as a ratio of the effect
#'      and its standard error.
#'    n_obs - Number of observations associated with this estimate.
#'    study_size - Size of the study that the estimate comes from. In Excel, this can be easily
#'      computed as "=COUNTIF(<COL>:<COL>,<CELL>)", where <COL> is the column with study names or
#'      study id's, and <CELL> is the cell in that column on the same row you want to calculate the
#'      study size on. Example: =COUNTIF(B:B,B2). This calculates the study size of the study located
#'      in cell B2, assuming that the column B contains the study information.
#'    reg_df - Degrees of freedom associated with this estimate.
#'  4. In the file <NAME_OF_YOUR_VARIABLE_INFORMATION_FILE>.csv, input the list of variables you are using in your data frame,
#'    along with these parameters:
#'    var_name - Name of the variable exactly as it appears in the data frame columns. Must not include
#'      spaces and various special characters. Underscores are allowed.
#'    var_name_verbose - A descriptive form of the variable name. Needs not to limit to any subset of characters.
#'    data_type - Type of the data this variable holds. Can be only one type. Can be one of:
#'      int - Integer. Any integer.
#'      category - Categorical variable. Any string.
#'      float - Float. Any number.
#'      dummy - Dummy. Either 0 or 1.
#'      perc - Percentage. Any value between 0 and 1, inclusive.
#'    group_category - Group of the variable. Group similar together, otherwise make a new group.
#'      Examples - dummies, gender, urban vs. rural, short-run vs. long-run
#'    na_handling - Specify how missing values should be handled for the variable. Can be one of:
#'      stop - Do not allow missing values. Throw an error in case there is a missing value.
#'      mean - Interpolate with the mean of the existing data.
#'      median - Interpolate with the median of the existing data.
#'      allow - Allow missing values. Use only for variables which whose values will be filled in automatically
#'          during preprocessing, meaning for which you can guarantee no missing values.
#'    variable_summary - Boolean. If TRUE, this variable will appear in the summary statistics table.
#'    effect_sum_stats - Boolean. If TRUE, this variable will appear in the effect summary statistics table.
#'    equal - Float. If set to any value, the effect summary statistics table will print out the statistics
#'      for the main effect of the data when subsetted to this variable equal to the specified value.
#'      If set to any value, can not set the "gtlt" column value.
#'    gtlt - One of "MED", float. Similar to "equal", but if set to MED, will print out the statistics
#'      for the effect of the data when subsetted to values above/below the median value of this variable.
#'      If set to float, the subsetting breakpoint will be that value instead.
#'    bma - Boolean. If TRUE, this variable will be used in the Bayesian model averaging. Do NOT set all
#'      values of one variable group to TRUE. This would create a dummy trap.
#'    to_log_for_bma - Boolean. If TRUE, this variable will be converted to logarithm during the 
#'      Bayesian model averaging.
#'    bpe - If set to any value, this value will be used when evaluating the best practice estimate.
#' 
#' HOW TO RUN:
#'  1. The script should be ran all at once, which should make for the most
#'    user-friendly experience. In order to achieve this, you can customize
#'    which parts of the code should be ran during the global call.
#'    This is achieved by splitting the script into two parts:
#'      - Customizable part: Here you define which parts of the script you want
#'          to run and with which parameters
#'      - Technical part: The actual code, which should run without any problems,
#'          and all at once, if you specify the parameters correctly.
#'  2. Go to the customizable part, and set which parts of the code you want to run.
#'    T stand for TRUE, F for FALSE. If the name of the part is set to T, that
#'    part will run. If it is set to F, it will not.
#'  3. Adjust the parameters with which to run the script. Find the 
#'    'adjustable_parameters' vector, and inside, feel free to adjust the various
#'    parameters as you see fit.
#'  4. Run the code ALL AT ONCE, and see the results in the console, and in the
#'    'Plots' section.

######################################################################
#####################    CUSTOMIZABLE PART    #######################
######################################################################

# Clean the environment - DO NOT CHANGE THIS
#rm(list = ls()) 

# Customizable data file names
master_data_set_source <- "data_set_master_thesis_cala.csv" # Master data frame
var_list_source <- "var_list_master_thesis_cala.csv" # Variable information file

#' WHAT PARTS OF THE SCRIPT TO RUN
#' T - RUN THIS PART
#' F - DO NOT RUN THIS PART
#' Note:
#'  Do NOT change the variable names, or the name of the vector
run_this <- c(
  "variable_summary_stats" = F,
  "effect_summary_stats" = F,
  "box_plot" = F,
  "funnel_plot" = F,
  "t_stat_histogram" = F,
  "linear_tests" = F,
  "nonlinear_tests" = F,
  "exo_tests" = F,
  "p_hacking_tests" = F,
  "bma" = F,
  "fma" = F, # Should be ran together with BMA
  "best_practice_estimate" = T # Should be ran together with BMA
)

#' ADJUSTABLE PARAMETERS
#' Adjust the parameters by modifying the numbers, or boolean values
#' Note:
#'  Do NOT change the variable names (apart from when adding new Box plot factors),
#'    the names of vectors, or value types (character, integer, vector...)
adjustable_parameters <- c(
  # Effect name
  "effect_name" = "years of schooling on wage", # A verbose name of what the effect represents
  # Data subsetting conditions
  # Note - if you do not with to use any conditions, set the conditions to NA
  # Example usage -  "data_subset_condition_1" = "column_name1 > <some_value>"
  "data_subset_condition_1" = NA,
  #"data_subset_condition_2" = "ability_direct == 1",
  # "data_subset_condition_X" = X, # Add more conditions in this manner - up to 20
  # Data winsorization characteristics
  "data_winsorization_level" = 0.01, # Between 0 and 1 (excluding)
  "data_precision_type" = "DoF", # Precision measure - one of "1/SE", "DoF" - latter is sqrt(DoF)
  # Handle missing data
  "allowed_missing_ratio" = 0.7, # Allow ratio*100(%) missing observations for each variable
  # Effect summary statistics confidence level
  "effect_summary_stats_conf_level" = 0.95, # Between 0 and 1 (excluding)
  # Box plot parameters
  "box_plot_group_by_factor_1" = "study_name", # Group by study name
  "box_plot_group_by_factor_2" = "country", # Group by country
  # "box_plot_group_by_factor_X" = X, # Add more factors in this manner - up to 20
  "box_plot_verbose" = T, # Get information about the plots being printed
  # Funnel plot parameters
  "funnel_plot_effect_proximity" = 0.15, # Effect axis cutoff point (perc) on either side of mean
  "funnel_plot_maximum_precision" = 0.2, # Precision axis maximum value cutoff point (perc)
  "funnel_plot_verbose" = T, # If T, print cut outlier information
  # T-statistic histogram parameters
  "t_hist_lower_cutoff" = -120, # Lower cutoff point for t-statistics
  "t_hist_upper_cutoff" = 120, # Upper cutoff point for t-statistics
  # Caliper test parameters
  "caliper_thresholds" = c(0, 1.96, 2.58), # Caliper thresholds - keep as vector
  "caliper_widths" = c(0.05, 0.1, 0.2), # Caliper widths - keep as vector
  # Eliott test parameters
  "eliott_data_subsets" = c("All data"), # Data subsets to run the tests on
  "eliott_p_min" = 0,
  "eliott_p_max" = 0.1,
  "eliott_d_point" = 0.1,
  "eliott_cs_bins" = 15,
  "eliott_verbose" = T,
  # Bayesian Model Averaging parameters
  "automatic_bma" = T, # If TRUE, automatically generate a formula for BMA with all VIF < 10
  "bma_burn" = 1e4, # Burn-ins (def 1e5)
  "bma_iter" = 3e4, # Draws (def 3e5)
  "bma_g" = "HQ", # g-Prior
  "bma_mprior" = "random", # Model Prior
  "bma_nmodel" = 20000, # Number of models (def 50000)
  "bma_mcmc" = "bd", # Markov Chain Monte Carlo
  "bma_print_results" = "fast", # Print results - one of c("none", "fast", "verbose", "all")
  # Best practice estimate parameters - for econ. significance, estimate of first study in vector is used
  "bpe_studies" = c( # Vector of study indexes for which to run the BPE. For author's BPE, use 0.
    0,
    1,
    2
  ),
  "bpe_use_ci" = TRUE, # If TRUE, display confidence intervals in BPE output. If FALSE, display SEs instead.
  "bpe_econ_sig_large_pip_only" = TRUE # If TRUE, display econ. significance for variables with PIP >= 0.5
)

######################################################################
#####################      TECHNICAL PART      #######################
######################################################################

##################### ENVIRONMENT PREPARATION ########################
# Static 
development_on <- T # Turn off when distributing the code
options(scipen=999) # No scientific notation

technical_parameters <- c(
  # Handle missing data
  "allow_missing_vars" = F # UNSAFE!! Allow missing variables in the data - Do NOT turn on
)

# Working directory - change only if the script is being ran as the master script (not imported)
if (length(commandArgs()) == 0) {
  if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi')
  if (! getwd() == dirname(getActiveDocumentContext()$path)){
    setwd(dirname(getActiveDocumentContext()$path)) # Set WD to the current file location
    print(paste0('Setting the working directory to: ', getwd()))
  }
}

# Source files - unmodifiable
stem_source <- "stem_method_master_thesis_cala.R" # STEM method (Furukawa, 2019) - fixed package handling
selection_model_source <- "selection_model_master_thesis_cala.R" # Selection model (Andrew & Kasy, 2019)
endo_kink_source <- "endo_kink_master_thesis_cala.R" # Endogenous Kink model (Bom & Rachinger, 2019)
maive_source <- "maive_master_thesis_cala.R" # MAIVE Estimator (Irsova et al., 2023)
source_files <- c(
  master_data_set_source,
  var_list_source,
  stem_source,
  selection_model_source,
  endo_kink_source,
  maive_source
)

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
  "xtable" # Creating tables in LaTeX or HTML
)

##### PREPARATION #####
# Load the source script
if (!file.exists("source_master_thesis_cala.R")){
  print('Please make sure to put the source file \"source_master_thesis_cala\" in
        your working directory.')
} else{
  source("source_master_thesis_cala.R")
  print("Source file loaded.")
}

# Load packages
loadPackages(packages)

if (development_on) {
  # Read multiple sheets from the master data set and write them as CSV files (overwriting existing files if necessary)
  master_data_set_xlsx_path <- "../Data/data_set_master_thesis_cala.xlsm"
  sheet_names <- c("data_set", "var_list") # Sheet names to read
  readExcelAndWriteCsv(master_data_set_xlsx_path, sheet_names)
}

# Validate all the necessary files
validateFiles(source_files)

######################### DATA PREPROCESSING #########################

# Read all the source .csv files
data <- readDataCustom(master_data_set_source)
var_list <- readDataCustom(var_list_source)

# Validate the input variable list
validateInputVarList(var_list)

# Preprocess data - validate dimensions, column names, etc.
data <- preprocessData(data, var_list)

# Get the raw but preprocessed data summary statistics (without winsorization, missing value handling)
if (run_this["variable_summary_stats"]){
  getVariableSummaryStats(data, var_list)
}

# Handle missing variables
allow_missing_vars <- as.logical(technical_parameters["allow_missing_vars"]) # Try to always keep FALSE
if (!allow_missing_vars){
  allowed_missing_ratio <- as.numeric(adjustable_parameters["allowed_missing_ratio"])
  data <- handleMissingData(data, var_list, allowed_missing_ratio = allowed_missing_ratio)
}

# Winsorize the data
data_win_level <- as.numeric(adjustable_parameters["data_winsorization_level"])
data_precision_type <- as.character(adjustable_parameters["data_precision_type"])
data <- winsorizeData(data, win_level = data_win_level, precision_type = data_precision_type)

# Validate the data types, correct values, etc. VERY restrictive. No missing values allowed until explicitly set.
validateData(data, var_list, ignore_missing = allow_missing_vars)

# Subset data using the conditions specified in the customizable section
subset_conditions <- getMultipleParams(adjustable_parameters, "data_subset_condition_", "character") # Extract all the data subset conditions
data <- applyDataSubsetConditions(data, subset_conditions)

######################### DATA EXPLORATION #########################

###### EFFECT SUMMARY STATISTICS ######
if (run_this["effect_summary_stats"]){
  effect_sum_stats_conf_level <- as.numeric(adjustable_parameters["effect_summary_stats_conf_level"])
  getEffectSummaryStats(data, var_list, effect_sum_stats_conf_level)
}

###### BOX PLOT ######
if (run_this["box_plot"]){
  # Adjustable parameters
  effect_name <- as.character(adjustable_parameters["effect_name"]) # Inside this scope for safety
  factor_names <- getBoxPlotFactors(adj_pars_source = adjustable_parameters, pattern = "box_plot_group_by_factor_")
  box_plot_verbose <- as.logical(adjustable_parameters["box_plot_verbose"])
  
  # Run box plots for all these factors iteratively
  for (factor_name in factor_names){
    getBoxPlot(data, factor_by = factor_name, verbose = box_plot_verbose, effect_name = effect_name)
  }
}

###### FUNNEL PLOT ######
if (run_this["funnel_plot"]){
  # Adjustable parameters
  funnel_effect_proximity <- as.numeric(adjustable_parameters["funnel_plot_effect_proximity"])
  funnel_maximum_precision <- as.numeric(adjustable_parameters["funnel_plot_maximum_precision"])
  funnel_verbose <- as.logical(adjustable_parameters["funnel_plot_verbose"])
  
  # Plot the funnel plot
  getFunnelPlot(data, funnel_effect_proximity, funnel_maximum_precision, use_study_medians = F, funnel_verbose)
  getFunnelPlot(data, funnel_effect_proximity, funnel_maximum_precision, use_study_medians = T, funnel_verbose)
  
}

###### HISTOGRAM OF T-STATISTICS ######
if (run_this["t_stat_histogram"]){
  lower_cutoff <- as.numeric(adjustable_parameters["t_hist_lower_cutoff"])
  upper_cutoff <- as.numeric(adjustable_parameters["t_hist_upper_cutoff"])
  getTstatHist(data, lower_cutoff, upper_cutoff)
}

######################### LINEAR TESTS ######################### 

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

if (run_this["linear_tests"]){
  getLinearTests(data)
}

######################### NON-LINEAR TESTS ######################### 

if (run_this["nonlinear_tests"]){
  global_non_lin_res <- T # Set to false if tests should be ran separately
  
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
          cutoffs = c(1.960), symmetric = F, modelmu = "normal", # Adjustable
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
if (run_this["exo_tests"]){
  global_exo_tests <- T # Set to false if tests should be ran separately
  
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

if (run_this["p_hacking_tests"]){
  ###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######
  caliper_thresholds <- getMultipleParams(adjustable_parameters, "caliper_thresholds", "numeric")
  caliper_widths <- getMultipleParams(adjustable_parameters, "caliper_widths", "numeric")
  caliper_results <- getCaliperResults(data,
          thresholds = caliper_thresholds, widths = caliper_widths, verbose = T)
   
  ###### PUBLICATION BIAS - p-hacking test (Eliott et al., 2022) ######
  eliott_data_subsets <- getMultipleParams(adjustable_parameters, "eliott_data_subsets", "character")
  eliott_p_min <- as.numeric(adjustable_parameters["eliott_p_min"])
  eliott_p_max <- as.numeric(adjustable_parameters["eliott_p_max"])
  eliott_d_point <- as.numeric(adjustable_parameters["eliott_d_point"])
  eliott_cs_bins <- as.numeric(adjustable_parameters["eliott_cs_bins"])
  eliott_verbose <- as.logical(adjustable_parameters["eliott_verbose"])
  eliott_results <- getEliottResults(data, eliott_data_subsets,
      eliott_p_min, eliott_p_max, eliott_d_point, eliott_cs_bins, eliott_verbose)
   
  ###### MAIVE Estimator (Irsova et al., 2023) ######
  maive_results <- getMaiveResults(data,
          method=3, weight=0, instrument=1, studylevel=0, verbose=T)
}

######################### MODEL AVERAGING #########################

###### HETEROGENEITY - Bayesian Model Averaging in R ######
if (run_this["bma"]){
  automatic_bma <- as.logical(adjustable_parameters["automatic_bma"])
  # Extract adjustable parameters
  bma_burn <- as.numeric(adjustable_parameters["bma_burn"])
  bma_iter <- as.numeric(adjustable_parameters["bma_iter"])
  bma_g <- as.character(adjustable_parameters["bma_g"])
  bma_mprior <- as.character(adjustable_parameters["bma_mprior"])
  bma_nmodel <- as.numeric(adjustable_parameters["bma_nmodel"])
  bma_mcmc <- as.character(adjustable_parameters["bma_mcmc"])
  bma_print_results <- as.character(adjustable_parameters["bma_print_results"])
  if (automatic_bma){
    # Get the optimal BMA formula automatically
    bma_formula <- findOptimalBMAFormula(data, var_list, verbose = T)
    # Get the vector of variables used in the automatic BMA
    bma_bool <- getBMAExcelBool(var_list, bma_formula, verbose=F)
  } else {
    # From the variable information instead
    bma_formula <- getBMAFormula(var_list)
  }
  # Run the Variance Inflation Test
  vif_coefs <- runVifTest(bma_formula, data, print_all_coefs = T)
  # BMA estimation
  bma_vars <- all.vars(bma_formula) # Only variables - for data subsettings
  bma_data <- getBMAData(data, var_list, bma_vars)
  bma_model <- runBMA(
       bma_data,
       burn=bma_burn,
       iter=bma_iter,
       g=bma_g, # UIP, BRIC, HQ
       mprior=bma_mprior, # uniform, random
       nmodel=bma_nmodel,
       mcmc=bma_mcmc
  )
  # Print out the results
  bma_coefs <- extractBMAResults(bma_model, bma_data, print_results = bma_print_results)
}

###### HETEROGENEITY - Frequentist model averaging code for R (Hansen) ######
  
if (run_this["fma"]){
  if (!exists("bma_data") || !exists("bma_model")){
    stop("You must create these two objects first - bma_data, bma_model. Refer to the 'bma' section.")
  }
  # Actual estimation
  fma_coefs <- runFMA(bma_data, bma_model, verbose = T)
}


######################### BEST-PRACTICE ESTIMATE #########################

if (run_this["best_practice_estimate"]){
  if (!exists("bma_data") || !exists("bma_model") || !exists("bma_formula")){
    stop("You must create these two objects first - bma_data, bma_model, bma_formula. Refer to the 'bma' section.")
  }
  bpe_study_ids <- getMultipleParams(adjustable_parameters, "bpe_studies", "numeric")
  bpe_use_ci <- as.logical(adjustable_parameters["bpe_use_ci"])
  bpe_econ_sig_large_pip_only <- as.logical(adjustable_parameters["bpe_econ_sig_large_pip_only"])
  # BPE estimation
  bpe_res <- generateBPEResultTable(bpe_study_ids,
                    data, var_list, bma_model, bma_formula, bma_data,
                    use_ci = bpe_use_ci, verbose_output = TRUE)
  # Economic significance table
  bpe_est <- bpe_res[1,1] # BPE estimate of the first row - usually Author's BPE
  bpe_econ_sig <- getEconomicSignificance(bpe_est, var_list, bma_data, bma_model,
                          display_large_pip_only = TRUE, verbose_output = TRUE)
}
