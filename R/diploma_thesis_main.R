##################### ENVIRONMENT PREPARATION ########################

# Sys.setlocale("LC_ALL", "en_US.UTF-8") # Set the correct locale

#Clean the environment
rm(list = ls()) 

##### STATIC #####

# What to run (allows for easier testing of the script with "Run script")
run_this <- c(
  "summary_stats" = T,
  "funnel_plot" = F,
  "linear_tests" = T,
  "nonlinear_tests" = T,
  "exo_tests" = F,
  "caliper" = F,
  "bma" = F,
  "fma" = F,
  "bp" = F
)

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
              "LowRankQP", "foreign", "multcomp", "data.table", "dplyr")

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

###### FUNNEL PLOT ######

if (run_this["funnel_plot"]){
  # Filter out the outliers
  filter_pcc_w <- getOutliers(data, pcc_cutoff=0.8, precision_cutoff=0.1, verbose=T)
  filter_pcc_w <- getOutliers(data, pcc_cutoff=1, precision_cutoff=1, verbose=T) # Allow all
  
  # Single out the data for the funnel plot
  funnel_data <- data[filter_pcc_w, c('pcc_w', 'se_precision_w')] # Only PCC, Precision
  funnel_data[] <- lapply(funnel_data, as.numeric) # To numeric
  
  # Plot the plot
  funnel_win <- ggplot(data = funnel_data, aes(x = pcc_w, y = se_precision_w)) + 
    geom_point(color = "#0d4ed1") + 
    labs(title = NULL, x = "Partial correlation coefficient", y = "Precision of the estimate (1/SE)") +
    main_theme()
    
  suppressWarnings(print(funnel_win)) # Print out the funnel plot
}


######################### LINEAR TESTS ######################### 

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

if (run_this["linear_tests"]){
  getLinearTests(data)
}

######################### NON-LINEAR TESTS ######################### 

if (run_this["nonlinear_tests"]){
  global_non_lin_res <- F # Set to false if tests should be ran separately
  
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



