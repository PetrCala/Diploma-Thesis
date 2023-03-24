#' Master script for my Diploma Thesis
#'  
#' Topic - Ability bias in the returns to schooling: How large it is and why it matters 
#' Author - Bc. Petr Čala
#' Year of defense - 2024
#' Supervisor - doc. PhDr. Zuzana Havránková Ph.D. 
#' 
#' HOW TO RUN:
#'  1. Make sure that your working directory contains the following files:
#'    - diploma_thesis_main.R
#'    - diploma_thesis_source.R
#'    - stem_method_ext.R
#'  2. The script should be ran all at once, which should make for the most
#'    user-friendly experience. In order to achieve this, you can customize
#'    which parts of the code should be ran during the global call.
#'    This is achieved by splitting the script into two parts:
#'      - Customizable part: Here you define which parts of the script you want
#'          to run and with which parameters
#'      - Technical part: The actual code, which should run without any problems,
#'          and all at once, if you specify the parameters correctly.
#'  3. Go to the customizable part, and set which parts of the code you want to run.
#'    T stand for TRUE, F for FALSE. If the name of the part is set to T, that
#'    part will run. If it is set to F, it will not.
#'  4. Adjust the parameters with which to run the script. Find the 
#'    'adjustable_parameters' vector, and inside, feel free to adjust the various
#'    parameters as you see fit.
#'  5. Run the code ALL AT ONCE, and see the results in the console, and in the
#'    'Plots' section.

######################################################################
#####################    CUSTOMIZABLE PART    #######################
######################################################################

# Clean the environment - DO NOT CHANGE THIS
rm(list = ls()) 

#' WHAT PARTS OF THE SCRIPT TO RUN
#' T - RUN THIS PART
#' F - DO NOT RUN THIS PART
#' Note:
#'  Do NOT change the variable names, or the name of the vector
run_this <- c(
  "variable_summary_stats" = F,
  "pcc_summary_stats" = F,
  "box_plot" = F,
  "funnel_plot" = F,
  "t_stat_histogram" = F,
  "linear_tests" = F,
  "nonlinear_tests" = F,
  "exo_tests" = F,
  "p_hacking_tests" = T,
  "bma" = F,
  "fma" = F,
  "best_practice_estimate" = F
)

#' ADJUSTABLE PARAMETERS
#' Adjust the parameters by modifying the numbers, or boolean values
#' Note:
#'  Do NOT change the variable names (apart from when adding new Box plot factors),
#'    or the name of the vector
adjustable_parameters <- c(
  # Data winsorization level
  "data_winsorization_level" = 0.01, # Between 0 and 1 (excluding)
  # PCC summary statistics confidence level
  "pcc_summary_stats_conf_level" = 0.95, # Between 0 and 1 (excluding)
  # Box plot parameters
  "box_plot_group_by_factor_1" = "study_name", # Group by study name
  "box_plot_group_by_factor_2" = "country", # Group by country
  # "box_plot_group_by_factor_X" = X, # Add more factors in this manner - up to 20
  "box_plot_verbose" = T, # Get information about the plots being printed
  # Funnel plot parameters
  "funnel_plot_pcc_cutoff" = 0.8, # PCC axis cutoff point
  "funnel_plot_precision_cutoff" = 0.2, # Precision axis cutoff point
  "funnel_plot_verbose" = T, # If T, print cut outlier information
  # T-statistic histogram parameters
  "t_hist_lower_cutoff" = -150, # Lower cutoff point for t-statistics
  "t_hist_upper_cutoff" = 150, # Upper cutoff point for t-statistics
  # Subset data to one study only
  "subset_this_study_only" = NA # Use index, such as 1,2,3,... Default NA means no subsetting.
)

######################################################################
#####################      TECHNICAL PART      #######################
######################################################################

##################### ENVIRONMENT PREPARATION ########################
# Static 
development_on <- T # Turn off when distributing the code
options(scipen=999) # No scientific notation

# Source files
master_data_set_source <- "data_set_master_thesis_cala.csv" # Master data frame
var_list_source <- "var_list_master_thesis_cala.csv" # Variable info source
stem_source <- "stem_method_custom.R" # STEM method (Furukawa, 2019) - fixed package handling
selection_model_source <- "selection_model_custom.R" # Selection model (Andrew & Kasy, 2019)
endo_kink_source <- "endo_kink_custom.R" # Endogenous Kink model (Bom & Rachinger, 2019)
maive_source <- "maive_custom.R" # MAIVE Estimator (Irsova et al., 2023)

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
  "DescTools", # Descriptive statistics and data analysis
  "LowRankQP", # Solving convex quadratic optimization problems
  "bayesm", # bayesian modeling and inference
  "corrplot", # Graphical display of correlation matrices
  "data.table", # Fast data manipulation and aggregation
  "ddpcr", # Analysis of Droplet Digital PCR (ddPCR) data
  "dplyr", # Data manipulation and data wrangling
  "foreign", # Reading and writing data stored by other statistical software
  "ggplot2", # Creating graphics and data visualizations
  "haven", # Importing and exporting data from SAS, SPSS, and Stata
  "lmtest", # Hypothesis testing and diagnostics for linear regression models
  "meta", # Meta-analysis package
  "metafor", # Conducting meta-analyses
  "multcomp", # Simultaneous inference for general linear hypotheses
  "multiwayvcov", # Computing clustered covariance matrix estimators
  "puniform", # Computing the density, distribution function, and quantile function of the uniform distribution
  "pracma", # MAIVE Estimator
  "readr", # Reading data into R from various file formats
  "readxl", # Reading Excel files
  "sandwich", # Computing robust covariance matrix estimators, MAIVE estimator
  "shiny", # Andrew & Kasy (2019) Selection model
  "stats", # Statistical analysis and modeling
  "testthat", # Unit testing for R
  "tidyverse", # A collection of R packages designed for data science, including ggplot2, dplyr, tidyr, readr, purrr, and tibble
  "varhandle", # MAIVE estimator
  "xtable" # Creating tables in LaTeX or HTML
)

##### PREPARATION #####
# Load the source script
if (!file.exists("diploma_thesis_source.R")){
  print('Please make sure to put the source file \"diploma_thesis_source\" in
        your working directory.')
} else{
  source("diploma_thesis_source.R")
  print("Source file loaded.")
}

# Load packages
loadPackages(packages, load_quietly = development_on)

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
data_source <- readDataCustom(master_data_set_source)
var_list <- readDataCustom(var_list_source)

# Validate, preprocess, and winsorize data
validateInputVarList(var_list)
data_win_level <- as.numeric(adjustable_parameters["data_winsorization_level"])
data <- preprocessData(data_source, var_list, win_level = data_win_level)

# Subset data to only one study for testing (does nothing by default)
one_study_subset <- adjustable_parameters["subset_this_study_only"]
data <- limitDataToOneStudy(data, one_study_subset) # Do nothing if subset == NA


######################### DATA EXPLORATION #########################

###### SUMMARY STATISTICS ######
if (run_this["variable_summary_stats"]){
  getVariableSummaryStats(data, var_list)
}

if (run_this["pcc_summary_stats"]){
  pcc_sum_stats_conf_level <- as.numeric(adjustable_parameters["pcc_summary_stats_conf_level"])
  getPCCSummaryStats(data, var_list, pcc_sum_stats_conf_level)
}

###### BOX PLOT ######
if (run_this["box_plot"]){
  # Automatically extract all specified factor names
  factor_names <- getBoxPlotFactors(adj_pars_source = adjustable_parameters, pattern = "box_plot_group_by_factor_")
  box_plot_verbose <- as.logical(adjustable_parameters["box_plot_verbose"])
  
  # Run box plots for all these factors iteratively
  for (factor_name in factor_names){
    getBoxPlot(data, factor_by = factor_name, verbose = run_verbose)
  }
}

###### FUNNEL PLOT ######

if (run_this["funnel_plot"]){
  custom_pcc_cutoff <- as.numeric(adjustable_parameters["funnel_plot_pcc_cutoff"])
  custom_precision_cutoff <- as.numeric(adjustable_parameters["funnel_plot_precision_cutoff"])
  custom_verbose <- as.logical(adjustable_parameters["funnel_plot_verbose"])
  getFunnelPlot(data, custom_pcc_cutoff, custom_precision_cutoff, custom_verbose)
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


######################### RELAXING THE EXOGENEITY ASSUMPTION ######################### 
if (run_this["exo_tests"]){
  global_exo_tests <- T # Set to false if tests should be ran separately
  
  if (!global_exo_tests) {
    
    ###### PUBLICATION BIAS - FAT-PET with IV ######
    iv_results <- getIVResults(data,
            effect_present = T, pub_bias_present = T, verbose_coefs = T)
    
    ###### PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) ######
    p_uni_results <- getPUniResults(data, method = "ML",
            effect_present=T, pub_bias_present=T, verbose_coefs=T)
    
  } else{
    getExoTests(data)
  }
}

######################### P-HACKING TESTS #########################

if (run_this["p_hacking_tests"]){
  
  ###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######
  caliper_results <- getCaliper(data)
   
  ###### PUBLICATION BIAS - p-hacking test (Eliott et al., 2022) ######
  eliott_results <- getEliott(data)
   
  ###### MAIVE Estimator (Irsova et al., 2023) ######
  maive_results <- getMaiveResults(data,
          method=3, weight=0, instrument=1, studylevel=0, verbose=T)
}

