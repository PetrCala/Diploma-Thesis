#' |--------------------------|
#' Script name: script_runner_master_thesis_cala.R
#' 
#' The script runner for running the analysis for my Master Thesis on the topic
#' of 'Ability bias in returns to schooling: how large it is and why it matters?'
#' 
#' For detailed explanation, see the README file distributed with this script.
#' 
#' Author: Petr Čala
#' Year created: 2023
#' GitHub: github.com/PetrCala/
#' |--------------------------|
 
# Clean the environment
rm(list = ls()) 

# Load packages
library('yaml') # In-built package for handling parameters
if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi') # Working directory

# Define the initial parameters
user_params <- list(
  # Development information
  development_on = TRUE, # Keep FALSE at all times
  
  # Customizable data file names
  master_data_set_source = "data_set_master_thesis_cala.csv", # Master data frame
  var_list_source = "var_list_master_thesis_cala.csv", # Variable information file
  stem_source = "stem_method_master_thesis_cala.R", # STEM method (Furukawa, 2019) - fixed package handling
  selection_model_source = "selection_model_master_thesis_cala.R", # Selection model (Andrew & Kasy, 2019)
  endo_kink_source = "endo_kink_master_thesis_cala.R", # Endogenous Kink model (Bom & Rachinger, 2019)
  maive_source = "maive_master_thesis_cala.R", # MAIVE Estimator (Irsova et al., 2023)
  
  # RUN THESE PARTS OF THE MAIN SCRIPT
  run_this = list(
    "variable_summary_stats" = F,
    "effect_summary_stats" = F,
    "box_plot" = F,
    "funnel_plot" = F,
    "t_stat_histogram" = F,
    "linear_tests" = F,
    "nonlinear_tests" = F,
    "exo_tests" = F,
    "p_hacking_tests" = F,
    "bma" = T,
    "fma" = T, # Executable only after running BMA
    "ma_variables_description_table" = T, # Executable only after running BMA
    "best_practice_estimate" = T # Executable only after running BMA
  ),
  
  # USER PARAMETERS
  # Adjust the parameters by modifying the numbers, or boolean values
  # Note:
  #  Do NOT change the variable names (apart from when adding new Box plot factors),
  #    the names of vectors, or value types (character, integer, vector...)
  adjustable_parameters = list(
    # Effect name
    "effect_name" = "years of schooling on wage", # A verbose name of what the effect represents
    # Formal output
    "formal_output_on" = TRUE, # If TRUE, return tables in a form presentable in text
    # Data subsetting conditions
    # Note - if you do not with to use any conditions, set ANY condition to NA. The data will not subset.
    # Example usage -  "data_subset_condition_1" = "column_name1 > <some_value>"
    "data_subset_condition_1" = NA,
    "data_subset_condition_2" = "ability_uncontrolled == 1",
    # "data_subset_condition_X" = X, # Add more conditions in this manner - up to 20
    # Data winsorization characteristics
    "data_winsorization_level" = 0.01, # Between 0 and 1 (excluding)
    "data_precision_type" = "DoF", # Precision measure - one of "1/SE", "DoF" - latter is sqrt(DoF)
    # Handle missing data - only in development
    "allowed_missing_ratio" = 0.7, # Allow ratio*100(%) missing observations for each variable
    # Effect summary statistics confidence level
    "effect_summary_stats_conf_level" = 0.95, # Between 0 and 1 (excluding)
    # Box plot parameters
    "box_plot_group_by_factor_1" = "study_name", # Group by study name
    "box_plot_group_by_factor_2" = "country", # Group by country
    # "box_plot_group_by_factor_X" = X, # Add more factors in this manner - up to 20
    "box_plot_verbose" = TRUE, # Get information about the plots being printed
    # Funnel plot parameters
    "funnel_effect_proximity" = 0.15, # Effect axis cutoff point (perc) on either side of mean
    "funnel_maximum_precision" = 0.2, # Precision axis maximum value cutoff point (perc)
    "funnel_verbose" = TRUE, # If T, print cut outlier information
    # T-statistic histogram parameters
    "t_hist_lower_cutoff" = -120, # Lower cutoff point for t-statistics
    "t_hist_upper_cutoff" = 120, # Upper cutoff point for t-statistics
    # Caliper test parameters
    "caliper_thresholds" = c(1.645, 1.96, 2.58), # Caliper thresholds - keep as vector
    "caliper_widths" = c(0.05, 0.1, 0.2), # Caliper widths - keep as vector
    # Eliott test parameters
    "eliott_data_subsets" = c("All data"), # Data subsets to run the tests on
    "eliott_p_min" = 0,
    "eliott_p_max" = 0.1,
    "eliott_d_point" = 0.1,
    "eliott_CS_bins" = 15,
    "eliott_verbose" = TRUE,
    # MAIVE parameters - for explanation, see MAIVE instructions (Irsova et al., 2023)
    "maive_method" = 3, # 3 = PET-PEESE
    "maive_weight" = 0, # 0 = no weights
    "maive_instrument" = 1, # 1 = Yes (instrument SEs)
    "maive_studylevel" = 2, # 2 = cluster-robust SEs
    "maive_verbose" = TRUE,
    # Bayesian Model Averaging parameters
    "automatic_bma" = TRUE, # If TRUE, automatically generate a formula for BMA with all VIF < 10
    "bma_verbose" = FALSE, # If TRUE, print suggested formulas, VIF, etc.
    "bma_burn" = 1e4, # Burn-ins (def 1e5)
    "bma_iter" = 3e4, # Draws (def 3e5)
    "bma_g" = "HQ", # g-Prior
    "bma_mprior" = "random", # Model Prior
    "bma_nmodel" = 20000, # Number of models (def 50000)
    "bma_mcmc" = "bd", # Markov Chain Monte Carlo
    "bma_print_results" = "none", # Print raw results - one of c("none", "fast", "verbose", "all")
    # Frequentist Model Averaging parameters
    "fma_verbose" = FALSE, # If TRUE, print out the raw results of FMA into the console
    # Model averaging parameters
    "ma_variables_description_table_verbose" = TRUE, # If TRUE, print out the BMA variable desc table into console
    "ma_variables_description_table_clip" = FALSE, # If TRUE, copy the table to a clipboard
    "ma_results_table" = TRUE, # If TRUE, print out results of model averaging into a pretty table
    # Best practice estimate parameters - for econ. significance, estimate of first study in vector is used
    "bpe_studies" = c( # Vector of study indexes for which to run the BPE. For author's BPE, use 0.
      0, # Author
      2, # Bartlolj et al. (2013) - Most years of schooling
      112, # Staiger et al. (1997) - Most citations
      7 # Webbink (2004) - Random, unpublished, uncited work
    ),
    "bpe_use_ci" = TRUE, # If TRUE, display confidence intervals in BPE output. If FALSE, display SEs instead.
    "bpe_econ_sig_large_pip_only" = TRUE # If TRUE, display econ. significance for variables with PIP >= 0.5
  )
)

# Working directory
if (! getwd() == dirname(getActiveDocumentContext()$path)){
  setwd(dirname(getActiveDocumentContext()$path)) # Set WD to the current file location
  print(paste0('Setting the working directory to: ', getwd()))
}

# Static
user_param_file <- 'user_parameters.yaml'

# Save the user parameters into the working directory
yaml::write_yaml(user_params, user_param_file)

# Run the main code
source("main_master_thesis_cala.R")