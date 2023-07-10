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
if (!require('yaml')) install.packages('yaml'); library('yaml')                   # yaml - handle params
if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi') # Working directory
if (!require('ddpcr')) install.packages('ddpcr'); library('ddpcr')                # Quiet output

# Define the initial parameters
user_params <- list(
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
    "bpe" = T, # Executable only after running BMA
    "robma" = F # Computationally expensive
  ),
  
  # CUSTOMIZABLE SOURCE FILE PATH
  source_file_params = list(
    source_data_folder = "./data/source/", # Folder with the source data frame
    file_name = "data_set_master_thesis_cala", # Name of the data frame
    file_suffix = '.xlsx',
    data_sheet_name = "data_set",
    var_list_sheet_name = "var_list",
	csv_suffix = "master_thesis_cala" # Suffix of the .csv files
  ),

  # CUSTOMIZABLE COLUMN NAMES - set value to NA if not present in your data set
  required_cols = list(
    obs_id = "obs_n", # Observation id
    study_id = "study_id", # Study id
    study_name = "study_name", # Study name
    effect = "effect", # Main effect
    se = "se", # Standard error
    t_stat = "t_stat", # T-statistic (optional)
    n_obs = "n_obs", # Number of observations associated with the estimate
    study_size = "study_size", # Number of estimates reported per study (optional)
    reg_df = "reg_df", # Degrees of Freedom in the regression (optional)
    precision = NA # A measure of precision (optional) - handle during winsorization
  ),
  
  # USER PARAMETERS
  # Adjust the parameters by modifying the numbers, or boolean values
  # Note:
  #  Do NOT change the variable names, the names of vectors,
  #  or value types (character, integer, vector...) (apart from when specified explicitly)
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
    "data_precision_type" = "1/SE", # Precision measure - one of "1/SE", "DoF" - latter is sqrt(DoF)
    "winsorize_precision" = TRUE, # If TRUE, winsorize precision (for different precision types)
    #   Note: The precision will be used only in case you do not provide a column with precision yourself
    # Handle missing data - only in development
    "allowed_missing_ratio" = 0.7, # Allow ratio*100(%) missing observations for each variable
    # Effect summary statistics confidence level
    "effect_summary_stats_conf_level" = 0.95, # Between 0 and 1 (excluding)
    # Box plot parameters
    "box_plot_group_by_factor_1" = "study_name", # Group by study name
    "box_plot_group_by_factor_2" = "country", # Group by country
    # "box_plot_group_by_factor_X" = X, # Add more factors in this manner - up to 20
    "box_plot_max_boxes" = 60, # Maximum number of boxes to display per single plot - more plots otherwise
    "box_plot_graph_scale" = 3, # Numeric, scale the graph by this number
    "box_plot_verbose" = TRUE, # Get information about the plots being printed
    # Funnel plot parameters
    "funnel_precision_to_log" = FALSE, # If T, use log of precision as y axis (default: precision)
    "funnel_effect_proximity" = 1, # Effect axis cutoff point (perc) on either side of mean
    "funnel_maximum_precision" = 1, # Precision axis maximum value cutoff point (perc)
    "funnel_add_zero" = TRUE, # If T, always add a zero tick into the plot
    "funnel_graph_scale" = 2.5, # Numeric, scale the graph by this number
    "funnel_verbose" = TRUE, # If T, print cut outlier information
    # T-statistic histogram parameters
    "t_hist_lower_cutoff" = -120, # Lower cutoff point for t-statistics
    "t_hist_upper_cutoff" = 120, # Upper cutoff point for t-statistics
    "t_hist_graph_scale" = 6, # Numeric, scale the graph by this number
    # Nonlinear parameters - only selection model parametrizable
    "non_linear_stem_graph_scale" = 5, # Numeric, scale the graph by this number
    "non_linear_stem_legend_position" = "topright", # Position of the STEM plot legend
    "non_linear_param_selection_cutoffs" = c(1.960),
    "non_linear_param_selection_symmetric" = F,
    "non_linear_param_selection_modelmu" = "t", # Can be one of "normal", "t"
    # P-uniform paramteres
    "puni_param_side" = "right", # puni_star side argument
    "puni_param_method" = "ML", # Method used for p-uniform calculation - one of "ML", "P"
    "puni_param_alpha" = 0.05, # puni_star alpha argument
    "puni_param_control" = list(max.iter=1000,tol=0.1,reps=10000, int=c(-5,25), est.ci = c(-10,10), verbose=TRUE), # puni_star controls
    # Caliper test parameters
    "caliper_thresholds" = c(1.645, 1.96, 2.58), # Caliper thresholds - keep as vector
    "caliper_widths" = c(0.05, 0.1, 0.2), # Caliper widths - keep as vector
    "caliper_verbose" = TRUE,
    # Elliott test parameters
    "elliott_data_subsets" = c("All data"), # Data subsets to run the tests on
    "elliott_p_min" = 0,
    "elliott_p_max" = 0.1,
    "elliott_d_point" = 0.1,
    "elliott_CS_bins" = 15,
    "elliott_verbose" = TRUE,
    # MAIVE parameters - for explanation, see MAIVE instructions (Irsova et al., 2023)
    "maive_method" = 3, # 3 = PET-PEESE
    "maive_weight" = 0, # 0 = no weights
    "maive_instrument" = 1, # 1 = Yes (instrument SEs)
    "maive_studylevel" = 0, # 0 = No study-level correlation
    "maive_verbose" = TRUE,
    # Bayesian Model Averaging parameters
    "automatic_bma" = FALSE, # If TRUE, automatically generate a formula for BMA with all VIF < 10
    "bma_scale_data" = FALSE, # If TRUE, standardize all variables onto the same scale automatically - UNSTABLE
    "bma_adjustable_theme" = TRUE, # If TRUE, modify the colors of bma plots to fit the theme
    "bma_verbose" = TRUE, # If TRUE, print suggested formulas, VIF, etc.
    "bma_graph_scale" = 2, # Numeric, scale the corrplot graph by this amount
    "bma_print_results" = "none", # Print raw results - one of c("none", "fast", "verbose", "all")
    "bma_param_burn" = 1e4, # Burn-ins (def 1e5)
    "bma_param_iter" = 3e4, # Draws (def 3e5)
    "bma_param_g" = "UIP", # g-Prior
    "bma_param_mprior" = "dilut", # Model Prior
    "bma_param_nmodel" = 20000, # Number of models (def 50000)
    "bma_param_mcmc" = "bd", # Markov Chain Monte Carlo
    # Frequentist Model Averaging parameters
    "fma_verbose" = FALSE, # If TRUE, print out the raw results of FMA into the console
    # Model averaging parameters
    "ma_results_table" = TRUE, # If TRUE, print out results of model averaging into a pretty table
    "ma_variables_description_table_verbose" = FALSE, # If TRUE, print out the BMA variable desc table into console
    "ma_variables_description_table_clip" = FALSE, # If TRUE, copy the table to a clipboard
    # Best practice estimate parameters - for econ. significance, estimate of first study in vector is used
    "bpe_studies" = c( 
      # Vector of study indexes for which to run the BPE. For author's BPE, use 0. For all studies, use "all".
       0, # Author
       2, # Bartlolj et al. (2013) - Most years of schooling
       112, # Staiger et al. (1997) - Most citations
       7 # Webbink (2004) - Random, unpublished, uncited work
    ),
    "bpe_use_ci" = TRUE, # If TRUE, display confidence intervals in BPE output. If FALSE, display SEs instead.
    "bpe_study_info" = TRUE, # If TRUE, print out information about individual studies being estimated
    "bpe_result_table_verbose" = TRUE, # If TRUE, print out the table into the console along with the functional BPE form
    "bpe_econ_sig_large_pip_only" = TRUE, # If TRUE, display econ. significance for variables with PIP >= 0.5
    "bpe_econ_sig_verbose" = TRUE, # If TRUE, print out the economic significance table into the console
    # Robust Bayesian Model Averaging parameters - do not pass nested lists, priors etc.
    "robma_verbose" = TRUE,
    "robma_param_priors_bias" = NULL,
    "robma_param_parallel" = TRUE
  ),
  
  # FOLDER PATHS
  folder_paths = list(
    cache_folder = './_cache/', # Store cache files here
    temp_data_folder = './data/temp/', # A folder for temporary data fiels storage
    numeric_results_folder = './results/numeric/', # Store results here
    ext_package_folder = './pckg/', # Store external packages here
    graphic_results_folder = './results/graphic/', # Store graphical output here
    scripts_folder = './scripts/', # Store R scripts here
    all_results_folder = "./results/" # Store the zip files with all results here
  ),
  
  # SCRIPT FILE NAMES
  script_files = list(
    endo_kink_source = "endo_kink_master_thesis_cala.R", # Endogenous Kink model (Bom & Rachinger, 2019)
    elliott_source = "elliott_master_thesis_cala.R", # Elliott p-hacking test (Elliott et al., 2022)
    maive_source = "maive_master_thesis_cala.R", # MAIVE Estimator (Irsova et al., 2023)
    selection_model_source = "selection_model_master_thesis_cala.R", # Selection model (Andrew & Kasy, 2019)
    stem_source = "stem_method_master_thesis_cala.R" # STEM method (Furukawa, 2019) - fixed package handling
  ),
  
  # EXPORT OPTIONS
  export_results = TRUE, # Export all results if they differ from the existing ones - no extra time
  export_methods = list( # Verbose names of all allowed export methods
    "variable_summary_stats" = "Variable summary stats",
    "effect_summary_stats" = "Effect summary stats",
    "linear_tests" = "Linear tests",
    "nonlinear_tests" = "Nonlinear tests",
    "exo_tests" = "Tests relaxing the exogeneity assumption",
    "p_hacking_tests_caliper" = "Caliper tests",
    "p_hacking_tests_elliott" = "Elliott tests",
    "p_hacking_tests_maive" = "MAIVE",
    "ma" = "Model averaging",
    "ma_variables_description_table" = "Model averaging description table",
    "bpe_res" = "Best practice estimate",
    "bpe_econ_sig" = "Economic significance",
    "robma_components" = "RoBMA components",
    "robma_estimates" = "RoBMA estimates"
  ),
  export_log_file_path = "main_results.txt", # Console log as a text file
  export_zip_name = paste0("results_all_", format(Sys.Date(), "%m-%d-%y")), # Zip file with all results
  export_graphics = TRUE, # If TRUE, save the graphs into the graphics folder as HTML files
  export_bma_data = FALSE, # If TRUE, export the data used for BMA into the temporary data folder
  theme = "blue", # One of "blue", "yellow", "green", "red", "purple"
  
  # CACHE HANDLING
  # I recommend you use caches only after you are certain the functions run correctly
  use_cache = TRUE, # Store intermediate output in a cache in the /_cache/ folder.
  cache_age = 3600 # In seconds - an hour
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

# Time the script run
# start_time <- Sys.time()
# source("main_master_thesis_cala.R")
# end_time <- Sys.time()
# elapsed_time <- end_time - start_time
# print(elapsed_time)
 
# Run the main file
source("main_master_thesis_cala.R")
