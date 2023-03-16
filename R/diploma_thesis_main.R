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
  "summary_stats" = T,
  "box_plot" = T,
  "funnel_plot" = T,
  "linear_tests" = T,
  "nonlinear_tests" = T,
  "exo_tests" = F,
  "caliper" = F,
  "bma" = F,
  "fma" = F,
  "bp" = F
)

#' ADJUSTABLE PARAMETERS
#' Adjust the parameters by modifying the numbers, or boolean values
#' Note:
#'  Do NOT change the variable names, or the name of the vector
adjustable_parameters <- c(
  # Box plot parameters
  "box_plot_pcc_cutoff" = 0.8, # pcc axis cutoff point
  "box_plot_precision_cutoff" = 0.2, # precision axis cutoff point
  "box_plot_verbose" = T # If T, print out outlier information
  # Other parameters
)

######################################################################
#####################      TECHNICAL PART      #######################
######################################################################

##################### ENVIRONMENT PREPARATION ########################

# Source files
master_data_set_source <- "data_set_master_thesis_cala.csv" # Master data frame
stem_source <- "stem_method_ext.R" # STEM method (Furukawa, 2019) - fixed package handling
source_files <- c(
  master_data_set_source,
  stem_source

)

# Required packages
packages <- c("readr", "tidyverse", "ggplot2", "readxl", "stats", "DescTools", "sandwich", "lmtest", "multiwayvcov",
              "metafor", "bayesm", "puniform", "haven", "meta", "AER", "BMS", "corrplot", "foreign", "xtable",
              "LowRankQP", "foreign", "multcomp", "data.table", "dplyr", "ddpcr")

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
loadPackages(packages)

# Copy the master data frame from ./Data into the WD (DELETE IN PRODUCTION)
master_data_set_xlsx_path = "../Data/data_set_master_thesis_cala.xlsx" # Master data set in folder Data
copyMasterDF(xlsx_path = master_data_set_xlsx_path, csv_path = master_data_set_source)

# Validate all the necessary files
validateFiles(source_files)

######################### DATA PREPROCESSING #########################

# Read the data set into the environment
data_raw <- read_csv(
              master_data_set_source,
              locale = locale(decimal_mark=".",
                              grouping_mark=",",
                              tz="UTC"),
              show_col_types = FALSE) # Quiet warnings
# data_raw <- read_xlsx(master_data_set_xlsx_path, sheet = 'main')

# Data transformation
data <- copy(data_raw) # Make a deep copy
data <- preprocessData(data) # Validate, preprocess, and winsorize data
                            

######################### DATA EXPLORATION #########################

###### SUMMARY STATISTICS ######

if (run_this["summary_stats"]){
  summary_stats_desc <- loadSummaryStats()
  getSummaryStats(data, summary_stats_desc)
}

if (run_this["box_plot"]){
  getBoxPlot(data)
}

###### FUNNEL PLOT ######

if (run_this["funnel_plot"]){
  custom_pcc_cutoff <- adjustable_parameters["box_plot_pcc_cutoff"]
  custom_precision_cutoff <- adjustable_parameters["box_plot_precision_cutoff"]
  custom_verbose <- adjustable_parameters["box_plot_verbose"]
  getFunnelPlot(data, custom_pcc_cutoff, custom_precision_cutoff, custom_verbose)
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
    waap_results<- getWaapResults(data)
    
    ###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######
    top10_results <- getTop10Results(data)
    
    ###### PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019) #####
    stem_results <- getStemResults(data)
    
    ###### PUBLICATION BIAS - FAT-PET hierarchical in R ######
    hier_results <- getHierResults(data)
    
    ###### PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019) ######
    
    # Source:
    # https://maxkasy.github.io/home/metastudy/
    # We used data found on the sheet "selection_model" in the appended data set (winsorization 1%)
    
    
    ###### PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020) ######
    # Stata code appended below
  } else {
    # Get all results at once without assigning the output to any variables
    getNonlinearResults(data)
  }
}


