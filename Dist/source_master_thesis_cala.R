#' |--------------------------|
#' Script name: source_master_thesis_cala.R
#' 
#' The source script for running the analysis for my Master Thesis on the topic
#' of 'Ability bias in returns to schooling: how large it is and why it matters?'
#' 
#' For detailed explanation, see the README file distributed with this script.
#' 
#' Author: Petr Čala
#' Year created: 2023
#' GitHub: github.com/PetrCala/
#' |--------------------------|
 
##################### ENVIRONMENT PREPARATION ########################

#' Capture the output of an expression:
#' - The function captures and returns all output (e.g., messages, errors, and print statements) 
#'   that is produced when evaluating the provided expression.
#' - Used after calling cached functions for printing verbose output that would otherwise
#'    get silenced.
#' 
#' @param expr [expression] The expression to evaluate
#' @return [character] A character vector containing the lines of output produced by the expression
captureOutput <- function(expr) {
  con <- textConnection("captured", "w", local = TRUE)
  sink(con)
  on.exit({
    sink()
    close(con)
  })
  force(expr)
  captured
}

#' Create a folder in the working directory if it does not exist yet
#' 
#' @param folder_name [character] Name of the folder. Specify in the format
#' "./<name_of_the_folder>/
#' @param require_existence [logical] Only check the existence of the folder.
#'  Raise an error in case the folder does not exist.
validateFolderExistence <- function(folder_name, require_existence = FALSE){
  if (!file.exists(folder_name)){
    if (require_existence){
      stop(paste("The folder", folder_name, "must exist in the working directory."))
    }
    dir.create(folder_name, recursive = TRUE)
  }
}

#' Function to read multiple sheets from an Excel file and write them as CSV files
#' Used in development mode for .csv file creation from a source .xlsx file.
#' @param xlsx_path Path to the Excel file
#' @param sheet_names A vector of sheet names to read
#' @param csv_suffix Suffix of the created .csv files. Defaults to "master_thesis_cala".
#' @param new_csv_path Data folder path. Defaults to './data/'.
#' @return A list of data frames
readExcelAndWriteCsv <- function(xlsx_path, sheet_names, csv_suffix = "master_thesis_cala",
                                 data_folder_path = './_data/') {
  # Validate input
  stopifnot(
    is.character(xlsx_path),
    is.character(csv_suffix)
  )
  # Validate source file existence explicitly
  if (!file.exists(xlsx_path)){
    stop(paste("The file", xlsx_path, "does not exist."))
  }
  # Read each sheet and write it as a CSV file in the working directory
  quiet(
    dfs <- lapply(sheet_names, function(sheet_name) {
      csv_path <- paste0(sheet_name, "_", csv_suffix, ".csv")
      new_data_path <- paste0(data_folder_path, csv_path) # Store in data folder
      # Read the source file
      df_xlsx <- read_excel(xlsx_path, sheet = sheet_name)
      # Remove .
      df_xlsx[df_xlsx == '.'] <- NA
      # Overwrite the CSV file
      hardRemoveFile(new_data_path)
      write_csv(df_xlsx, new_data_path)
      return(df_xlsx)
    })
  )
  print('Read all data from the source file successfully.')
  # invisible(dfs) # Return if need be
}

#' Read_csv with parameters to avoid redundancy
#' 
#' @param source_path [str] - Path to the .csv file
readDataCustom <- function(source_path){
  data_out <- read_csv(
    source_path,
    locale = locale(decimal_mark=".",
                    grouping_mark=",",
                    tz="UTC"),
    show_col_types = FALSE) # Quiet warnings
  invisible(data_out)
}


#' Input a vector of file names, that should be located in the folder
#' of the main script, and validate that all are indeed present.
#' Print out a status message after the validation.
#' 
#' @param files[vector] A vector of strings.
validateFiles <- function(files){
  for (file in files){
    if (!file.exists(file)){
      stop(paste0(file, ' does not exist or could not be located.
                  Please make sure to include it in the working directory.'))
    }
  }
  print("All necessary files located successfully.")
}

#' Extract multiple parameters from a vector dictionary
#' 
#' Input the adjustable parameters list and the name of the parameter to extract
#' the values for. Extract all the values of that parameters and return the vector of the values.
#' In case the list itself should be extracted, set "extract_list" to TRUE.
#' 
#' @param adj_params [list] The list with adjustable parameters.
#' @param prefix [character] The prefix for which to extract the values for. Note that the 
#'  parameters names MUST START with this prefix.
#' @param extract_list [logical] If TRUE, extract the whole list instead. Deafults to FALSE.
#' @param drop_prefix [logical] If TRUE, extract the list without the prefix. Defaults to FALSE.
#' @return Vector of values.
getMultipleParams <- function(adj_params, prefix, extract_list = FALSE, drop_prefix = FALSE){
  # Validate input
  stopifnot(
    is.list(adj_params),
    is.character(prefix),
    is.logical(extract_list)
  )
  # Extract the list
  param_list <- adj_params[grep(paste0("^", prefix), names(adj_params))]
  if (extract_list){
    if (drop_prefix){
      param_list <- setNames(param_list, sub(paste0("^", prefix), "", names(param_list))) # No prefixes
    }
    return(param_list)
  }
  value_vector <- as.vector(unlist(param_list)) # Values instead
  return(value_vector)
}


#' Apply data subsetting conditions to the input data frame
#'
#' This function applies all the specified subset conditions to the input data frame. Users can
#' add any number of conditions to subset the data, and these conditions will be applied
#' to the data frame before the script continues.
#'
#' @param data [data.frame] The input data frame on which to apply the data subsetting conditions.
#' @param conditions [vector] A vector of data subset conditions.
#' @return A data frame with the specified data subsetting conditions applied.
applyDataSubsetConditions <- function(data, conditions) {
  # Validate input
  stopifnot(
    is.data.frame(data),
    is.list(conditions)
  )
  # Do not subset if any conditions are NA
  if (any(is.na(conditions))){
    return(data)
  }
  # Subset the data given the conditions
  if (length(conditions) > 0) {
    for (condition in conditions) {
      # Evaluate each condition and apply it to the data frame
      data <- data[eval(parse(text = paste0("data$", condition))),]
    }
  }
  return(data)
}

#' Verbose output for the applyDataSubsetConsitions function
applyDataSubsetConditionsVerbose <- function(...){
}



####################### PACKAGE HANDLING ########################

#' Create a custom error object
#'
#' This function creates a custom error object with a given message. The custom
#' error object can be used in tryCatch expressions to handle specific errors.
#'
#' @param message [character] The error message.
#'
#' @return A custom error object with the specified message and class "custom_error".
customError <- function(message) {
  structure(list(message = message), class = "custom_error")
}

#' Quietly execute an expression
#'
#' This function suppresses package startup messages, warnings, and messages
#' while executing an expression. It is useful for keeping the console output
#' clean when loading or installing packages.
#'
#' @param expr [expression] The expression to be executed quietly.
#'
#' @return The result of the executed expression without any messages, warnings, or package startup messages.
quietPackages <- function(expr) {
  suppressPackageStartupMessages(suppressWarnings(suppressMessages(expr)))
}

#' Load and install a list of R packages
#'
#' This function checks if the specified packages are installed, installs any missing
#' packages, and then loads all of them. If an error occurs during the installation
#' or loading process, the function stops execution and displays an error message.
#'
#' @param package_list [character] A character vector of package names.
#'
#' @return A message indicating that all packages were loaded successfully or an error message if the process fails.
loadPackages <- function(package_list) {
  # Install packages not yet installed
  installed_packages <- package_list %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("Installing package ", package_list[!installed_packages], "...", sep = ""))
    tryCatch(
      {
        quietPackages(
          install.packages(package_list[!installed_packages])
        )
      },
      error = function(e) {
        message("Package installation failed. Exiting the function...")
        stop(customError("Package installation failed"))
      }
    )
  }
  # Package loading
  print("Attempting to load the packages...")
  tryCatch(
    {
      quietPackages(
        invisible(lapply(package_list, library, character.only = TRUE))
      )
    },
    error = function(e) {
      message("Package loading failed. Exiting the function...")
      stop(customError("Package loading failed"))
    }
  )
  print("All packages loaded successfully")
}

#' @title Load External Packages
#' @description This function is used to load all external packages located in a specified folder.
#' It iterates over the package folders and attempts to load each package using devtools::load_all function.
#' If any error occurs during the loading process, it exits the function and throws a custom error message.
#'
#' @param pckg_folder [character] This parameter should be a string representing the path to the directory 
#' containing the packages that are to be loaded. The path can be either absolute or relative.
#'
#' @return The function doesn't explicitly return a value. It's used for the side effect of loading packages 
#' into the R environment.
#'
#' @examples
#' loadExternalPackages("path/to/your/packages")
#'
#' @seealso
#' \code{\link[devtools]{load_all}}
#'
#' @export
loadExternalPackages <- function(pckg_folder){
  package_list <- list.files(pckg_folder, full.names = TRUE)
  package_names <- list.files(pckg_folder, full.names = FALSE) # Package names only
  # Iterate over the package folders
  installed_packages <- package_names %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("Installing an external package ", package_names[!installed_packages], "...", sep = ""))
    tryCatch(
      {
        quietPackages(
          install.packages(package_list[!installed_packages], repos = NULL, type = "source")
        )
      },
      error = function(e) {
        message("External package installation failed. Exiting the function...")
        stop(customError("External package installation failed"))
      }
    )
  }
  # Package loading
  print("Attempting to load the external packages...")
  tryCatch(
    {
      quietPackages(
        invisible(lapply(package_names, library, character.only = TRUE))
      )
    },
    error = function(e) {
      message("External package loading failed. Exiting the function...")
      stop(customError("External package loading failed"))
    }
  )
  print("All external packages loaded successfully")
}



######################### DATA PREPROCESSING #########################


#' Check that the input variable list specifications are all correct
#'
#' @param input_var_list [data.frame] The input variable list
validateInputVarList <- function(input_var_list){
  # Validate input
  stopifnot(
    is.data.frame(input_var_list)
  )
  # Allowed characters for column names
  valid_col_pattern <- "^[a-zA-Z0-9._]+$"
  # Check if all column names match the pattern
  valid_column_names <- sapply(input_var_list$var_name, function(x) grepl(valid_col_pattern, x))
  # Validate column names
  if (!all(valid_column_names)){
    special_char_cols <- input_var_list$var_name[!valid_column_names]
    message("These variable names contain special characters. Please modify these names so that there are no such characters.")
    message(special_char_cols)
    stop("Invalid column names")
  }
  # Validate that there are no two same column names
  if (any(duplicated(input_var_list$var_name))){
    duplicated_col <- input_Var_list$var_name[which(duplicated(input_var_lsit$var_name))]
    message("Duplicate column values are not allowed.")
    message("Modify the names of these columns:")
    message(duplicated_col)
    stop("Duplicate columns.")
  }
  # Validate that data type stays consistent within each group
  for (i in 1:max(input_var_list$group_category)){
    data_slice <- input_var_list$data_type[input_var_list$group_category == i]
    arbitrary_type <- data_slice[1] # Should be equal for all
    validity_test <- all(data_slice == arbitrary_type)
    if (!validity_test){
      stop(paste("Incorrect data type for group", i))
    }
  }
  # Validate that specifications are present for all variables where sum stats are required
  data_to_summarize <- input_var_list[input_var_list$effect_sum_stats == TRUE, ]
  for (i in 1:nrow(data_to_summarize)){
    temp_row <- data_to_summarize[i,]
    # Only one of the two specifications is used
    validity_test <- xor(!is.na(temp_row$equal),!is.na(temp_row$gtlt))
    if (!validity_test){
      problematic_var <- temp_row$var_name
      stop(paste("Missing effect summary specifications for", problematic_var))
    }
  }
    
  # Check data values
  dummy_data_to_check <- data_to_summarize[data_to_summarize$data_type == 'dummy',]
  dummy_data_allowed_values <- c(0, 1)
  for (i in 1:nrow(dummy_data_to_check)){
    temp_row <- dummy_data_to_check[i,]
    validity_test <- temp_row$equal %in% dummy_data_allowed_values
    if (!validity_test){
      problematic_var <- temp_row$var_name
      message("Dummy variables must have the effect summary stats checked against 1/0 only.")
      stop(paste("Problematic variable:",problematic_var))
    }
  }
  
  perc_data_to_check <- data_to_summarize[data_to_summarize$data_type == 'perc',]
  for (i in 1:nrow(perc_data_to_check)){
    temp_row <- perc_data_to_check[i,]
    validity_test <- c(
      temp_row$gtlt < 1,
      temp_row$gtlt > 0
    )
    if (all(is.na(validity_test))){ # NAs
      next
    }
    if (!all(validity_test)){
      problematic_var <- temp_row$var_name
      message("Percentage variables must have the effect summary stats GLTL value between 0 and 1.")
      stop(paste("Problematic variable:",problematic_var))
    } 
  }
  print("Variable information data validated.")
}

#' Preprocess the raw excel data:
#' - Adjust the source data dimensions
#' - Transform ALL columns into the correct data type.
#' 
#' Check column validity, add winsorized statistics (Effect, SE, t-stat)
#' @param input_data [data.frame] Main data frame
#' @param input_var_list [data.frame] Data frame with variable descriptions.
#' @return [data.frame] The preprocessed data
preprocessData <- function(input_data, input_var_list){
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list)
  )
  # Remove redundant columns
  expected_col_n <- nrow(input_var_list)
  while(ncol(input_data) > expected_col_n){
    input_data <- input_data[,-ncol(input_data)]
  }
  
  # Variable name validity check
  varnames <- colnames(input_data)
  expected_varnames <- input_var_list$var_name
  if(!all(varnames == expected_varnames)){ # A strong, restrictive assumption
    missing_from_var_list <- varnames[!varnames %in% expected_varnames]
    missing_from_data <- expected_varnames[!expected_varnames %in% varnames]
    message("These variables are not a part of the variable list var_name column.")
    message(paste(missing_from_var_list, "\n"))
    message("These variables are not a part of the main data frame columns.")
    message(paste(missing_from_data, "\n"))
    stop("Mismatching variable names")
  }
  
  # Remove redundant rows
  while(is.na(input_data[nrow(input_data), "study_name"])) {
    input_data <- input_data[-nrow(input_data),]
  }
  
  # Preprocess and enforce correct data types
  for (col_name in varnames) {
    col_data_type <- input_var_list$data_type[input_var_list$var_name == col_name]
    if (col_data_type == "int" || col_data_type == "dummy") {
      input_data[[col_name]] <- as.integer(input_data[[col_name]])
    } else if (col_data_type == "float" || col_data_type == "perc") {
      input_data[[col_name]] <- as.numeric(input_data[[col_name]])
    } else if (col_data_type == "category") {
      input_data[[col_name]] <- as.character(input_data[[col_name]])
    }
  }
  # Print out the output information and return the processed data frame
  preprocessDataVerbose()
  return(input_data)
}

#' Verbose output for the preprocessData function
preprocessDataVerbose <- function(...){
  print("Data preprocessing complete.")
}

#' Handle Missing Data in a Data Frame
#'
#' This function handles missing values in a data frame based on the specified handling methods.
#' It iterates through the columns specified in the input_var_list and interpolates the missing values
#' based on the corresponding handling method (stop, mean, or median). If the ratio of missing values
#' in a column is more than the desired value (default 80%), it throws an error notifying the user to
#' consider modifying or deleting the variable.
#'
#' @param input_data [data.frame] A data frame containing the data with missing values.
#' @param input_var_list [data.frame] A data frame with two columns: 'var_name' containing the names of the columns in input_data
#'        and 'na_handling' containing the handling method for the corresponding column (stop, mean, or median).
#' @param allowed_missing_ratio [float] A ratio indicating how many variables can be missing for the function to allow 
#'  interpolation. Anything above and the function will stop and warn the user. Defaults to 0.8 (80%).
#' @return A data frame with the missing values handled based on the specified methods.
#' @examples
#' # Create a sample data frame with missing values
#' sample_data <- data.frame(a = c(1, 2, 3, NA, 5), b = c(2, NA, 4, 6, NA))
#' 
#' # Create an input_var_list for handling missing values
#' input_vars <- data.frame(var_name = c("a", "b"), na_handling = c("mean", "median"))
#' 
#' # Handle missing values in the sample_data based on the input_vars
#' handled_data <- handleMissingData(sample_data, input_vars, allowed_missing_ratio = 0.5)
handleMissingData <- function(input_data, input_var_list, allowed_missing_ratio = 0.8){
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    allowed_missing_ratio >= 0,
    allowed_missing_ratio <= 1
  )
  # Get the NA values handling information
  var_names <- input_var_list$var_name
  na_handling <- input_var_list$na_handling
  
  # Iterate through columns
  for (i in seq_along(var_names)) {
    column_name <- var_names[i]
    handling_method <- na_handling[i]
    
    # Check if the column exists in the data frame
    if (!column_name %in% colnames(input_data)) {
      stop(paste("The column", column_name, "does not exist in the input_data."))
    }
    
    # Do not interpolate or validate missing values for this variable
    if (handling_method == "allow"){
      next
    }
    
    column_data <- input_data[[column_name]]
    na_count <- sum(is.na(column_data))
    total_count <- length(column_data)
    
    # Check if the ratio of missing values is more than the allowed ratio
    if (na_count / total_count > allowed_missing_ratio) {
      missing_ratio_verbose <- paste0(as.character(allowed_missing_ratio * 100), "%")
      stop(paste("The column", column_name, "has more than,", missing_ratio_verbose, "missing values. Consider modifying or deleting the variable."))
    }
    
    # Handle missing values based on the handling method
    if (handling_method == "stop") {
      if (any(is.na(input_data[[column_name]]))){
        stop(paste("No missing values allowed for", column_name))
      }
    } else if (handling_method == "mean") {
      input_data[[column_name]][is.na(column_data)] <- mean(column_data, na.rm = TRUE)
    } else if (handling_method == "median") {
      input_data[[column_name]][is.na(column_data)] <- median(column_data, na.rm = TRUE)
    } else {
      stop(paste("Invalid handling method for column", column_name, ": ", handling_method))
    }
  }
  # Print out the output information and return the processed data frame
  handleMissingDataVerbose()
  return(input_data)
}

#' Verbose output for the handleMissingData function
handleMissingDataVerbose <- function(...){
  print("Missing data handled successfully.")
}


#' A very restrictive function for validating the correct data types
#' 
#' Check that all values across all columns have the correct data type, check that all dummy groups
#' are correctly specified across all observations (only one 1, otherwise 0), and 
#' stop and print out the error message, if it is not so.
#' 
#' Allows for skipping NA values, if your data set is incomplete.
#' 
#' @param input_data [data.frame] The data frame to be validated
#' @param input_var_list [data.frame] The data frame that specifies the data type of each variable
#' @param ignore_missing [bool] If TRUE, allow missing values in the data frame. Deafults to FALSE.
validateData <- function(input_data, input_var_list, ignore_missing = F){
  ### Static
  ref_prone_types <- c("dummy", "perc") # Data types that always must have a single reference variable
  ref_forbidden_categories <- c("bma", "to_log_for_bma", "bpe") # A reference variable can't have these TRUE
  ### End of static
  
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.logical(ignore_missing)
  )
  
  # Do not pass if any values are NA. 
  if (!ignore_missing){
    if(any(is.na(input_data))){
      stop("This script does not allow for ANY missing values. Make sure you have called the data preprocessing function.")
    }
  }
  # Verify that all studies are unique
  runs <- rle(as.character(input_data$study_name)) # Study names in chunks
  if (length(runs$values) != length(unique(runs$values))) {
    stop("Each study must appear only once in the data and in one continuous chunk.")
  }
  ## Column names validation
  valid_col_pattern <- "^[a-zA-Z0-9._]+$"
  # Check if all column names match the pattern
  valid_column_names <- sapply(colnames(input_data), function(x) grepl(valid_col_pattern, x))
  if (!all(valid_column_names)){
    special_char_cols <- colnames(input_data)[!valid_column_names]
    message("These columns contain special characters. Please modify the columns so that there are no such characters.")
    message(special_char_cols)
    stop("Invalid column names")
  }
  # Validate that there are no two same column names
  if (any(duplicated(colnames(input_data)))){
    duplicated_col <- colnames(input_data)[which(duplicated(colnames(input_data)))]
    message("Duplicate column values are not allowed.")
    message("Modify the names of these columns:")
    message(duplicated_col)
    stop("Duplicate columns.")
  }
  # Validate that no columns are static
  constant_columns <- apply(input_data, 2, function(col){length(unique(col)) == 1}) # Boolean vector with names
  if (any(constant_columns)){
    message("There are constant columns in your data. Make sure to remove these first.")
    message("These columns have constant values:")
    message(paste(colnames(input_data)[constant_columns]), sep = "\n")
    stop("Constant columns.")
  }
  ### Correlation validation
  cor_matrix <- cor(data[sapply(data, is.numeric)]) # Corr table of numeric columns
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA # Lower triangle to NA
  cor_matrix[cor_matrix != 1 & cor_matrix != -1] <- NA # Non -1/1 values to NA
  indices <- which(!is.na(cor_matrix), arr.ind = TRUE) # Only -1/1 values
  # Extract corresponding variable names
  correlated_vars <- data.frame(
    variable_1 = rownames(cor_matrix)[indices[, 1]],
    variable_2 = colnames(cor_matrix)[indices[, 2]]
  )
  if (length(indices) > 0 ){ # Corr %in% c(-1,1) present
    # Filter out pairs from the same dummy group
    problematic_pairs <- correlated_vars %>%
      rowwise() %>%
      filter({
        type1 <- input_var_list$data_type[input_var_list$var_name == variable_1]
        type2 <- input_var_list$data_type[input_var_list$var_name == variable_2]
        group1 <- input_var_list$group_category[input_var_list$var_name == variable_1]
        group2 <- input_var_list$group_category[input_var_list$var_name == variable_2]
        !all(type1 %in% c("dummy","perc"), type2 %in% c("dummy","perc"), type1 == type2, group1 == group2)
      })
    if (nrow(problematic_pairs) > 0) {
      message(
        "There are variables with perfect correlation in your data (1 or -1). Make sure to treat these variables.\n",
        "These variables have perfect correlation:\n",
        paste(capture.output(print(problematic_pairs)), collapse = "\n")
      )
      stop("Perfect correlation in data.")
    }
  }
  ### Dummy group validation
  # Names of dummy variable columns
  dummy_group_vars <- as.vector(unlist(input_var_list[input_var_list$data_type == "dummy", "var_name"]))
  # Group numbers for these columns (e.g. 11,11,12,12,12,13,13,...)
  dummy_group_nums <- as.vector(unlist(input_var_list[input_var_list$data_type == "dummy", "group_category"]))
  # Split the data frame based on the dummy variable groups
  unique_group_nums <- unique(dummy_group_nums)
  splitted_data_frames <- list()
  for (group_num in unique_group_nums){
    group_col_names <- dummy_group_vars[dummy_group_nums == group_num] # Get col names of the same category
    splitted_data_frames[[as.character(group_num)]] <- input_data[,group_col_names, drop = FALSE] # To list
  }
  # Validate all groups
  for (i in 1:length(splitted_data_frames)){
    df_to_test <- splitted_data_frames[[i]] # Explicit for indexing clarity
    # Skip empty groups in development - should not be used
    if(ignore_missing & all(is.na(df_to_test))){
      next # Empty group
    }
    if(!any(apply(df_to_test,1,sum) == 1)){ # Check that for all rows of all groups, only one value is 1, other are 0
        stop("Invalid dummy group configuration: One and only one dummy variable must be 1 in each group for each row")
    }
  }
  ### Validate all reference groups
  # Get ids of groups that must have one reference variable
  ref_prone_groups <- as.vector(unlist(input_var_list[input_var_list$data_type %in% ref_prone_types, "group_category"]))
  ref_prone_groups <- unique(ref_prone_groups)
  # Get reference variable presence info for these groups and verify that all have exactly one such variable
  for (group_id in ref_prone_groups){
    ## Validate that exactly one variable is marked as reference
    ref_info <- input_var_list$bma_reference_var[input_var_list$group_category == group_id]
    if (sum(ref_info) != 1){
      unreferenced_vars <- input_var_list$var_name[input_var_list$group_category == group_id] # Unreferenced var names
      message("This group of variables is missing a reference variable:")
      message(paste(unreferenced_vars,"\n"))
      stop("You must define one and only one reference variable for each BMA group.")
    }
    ## Validate that that variable is not marked for any other BMA/BPE categories
    reference_var <- input_var_list$var_name[input_var_list$group_category == group_id &
                                             input_var_list$bma_reference_var == TRUE]
    # Get the parameters the reference variable has set for other BMA/BPE categories
    forbidden_values <- input_var_list[input_var_list$var_name == reference_var, ref_forbidden_categories]
    if (any(forbidden_values == TRUE)){
      stop(paste("The variable", reference_var, "is a reference variable and should have all values of BMA/BPE information set to FALSE."))
    }
  }
  
  # Validate that all perc variables sum up to 1
  perc_groups <- unique(as.vector(unlist(input_var_list[input_var_list$data_type == "perc", "group_category"])))
  for (group in perc_groups){
    group_var_names <- as.vector(unlist(input_var_list[input_var_list$group_category == group, "var_name"]))
    data_to_validate <- input_data[,group_var_names] # Only columns of the relevant percentage group
    row_sums <- rowSums(data_to_validate)
    # Check if all sums are equal to 1
    rows_equal_to_1 <- abs(row_sums - 1) < .Machine$double.eps^0.5 + 0.001 # Marginal error allowed
    if (!all(rows_equal_to_1)){
      problematic_rows <- which(!rows_equal_to_1)
      message("All percentage groups must some up to 1.")
      message("These variables do not fulfill that:")
      message(paste(group_var_names, "\n"))
      message("at rows:")
      message(paste(head(problematic_rows), "\n"))
      if (length(problematic_rows) > 6){
        message(paste("and",length(problematic_rows) - 6,"other rows."))
      }
      message("One of the following values is generated during interpolation, and one is the actually faulty data point:")
      message(paste(as.character(unique(row_sums)), "\n"))
      stop("Incorrect percentage group values")
    }
  }
  
  ### Data type validation
  for (row in 1:nrow(input_var_list)) {
    var_name <- as.character(input_var_list[row, "var_name"])
    data_type <- as.character(input_var_list[row, "data_type"])
    
    if (ignore_missing) {
      non_missing_rows <- !is.na(input_data[[var_name]]) & input_data[[var_name]] != "."
    } else {
      non_missing_rows <- TRUE
    }
    # Check that all values in all columns have the expected value
    if (data_type == "int") {
      if (!all(sapply(as.vector(unlist(input_data[non_missing_rows, var_name])), is.integer))) {
        stop(paste("Invalid data type for variable:", var_name, "Expected integer values"))
      }
    } else if (data_type == "category") {
      if (!all(sapply(as.vector(unlist(input_data[non_missing_rows, var_name])), is.character))) {
        stop(paste("Invalid data type for variable:", var_name, "Expected categorical (string) values"))
      }
    } else if (data_type == "float") {
      if (!all(sapply(as.vector(unlist(input_data[non_missing_rows, var_name])), is.numeric))) {
        stop(paste("Invalid data type for variable:", var_name, "Expected float (numeric) values"))
      }
    } else if (data_type == "dummy") {
      if (!all(as.vector(unlist(input_data[non_missing_rows, var_name])) %in% c(0, 1))) {
        stop(paste("Invalid data type for variable:", var_name, "Expected dummy (0 or 1) values"))
      }
    }
  }
  # Print out the verbose information
  validateDataVerbose()
  return(data)
}


#' Verbose output for the validateData function
validateDataVerbose <- function(...){
  print("All values across all columns of the main data frame are of the correct type.")
}

#' A list of columns that should always appear in the data frame (modified)
#' 
#' Return a list of column names that should always appear in the data frame,
#' after renaming to script-accepted names. As values to the list names,
#' put booleans that indicate whether that column must appear in the source data
#' frame (TRUE), or can be added during preprocessing (FALSE).
#' 
#' @param names_only [logical] If TRUE, return names only.
getDefaultColumns <- function(names_only=TRUE){
  def_cols <- list(
    "obs_id" = TRUE,
    "study_id" = TRUE,
    "study_name" = TRUE,
    "effect" = TRUE,
    "se" = TRUE,
    "t_stat" = FALSE,
    "n_obs" = TRUE,
    "study_size" = FALSE,
    "reg_df" = FALSE,
    "precision" = FALSE
  )
  if (names_only){
    return(as.vector(names(def_cols)))
  }
  return(def_cols)
}

#' Specify to what should be initialized default columns that the user
#' fails to provide in their dataset. Return the values of the new column.
#' Should be called from within the renameUserColumns function.
#' 
#' Currently handles these four columns:
#'  - t_stat - Calculated as effect / standard error
#'  - study_size - Calculated automaticcaly using the purrr package as the
#'    number of estimates reported by each study. Uses "study_id" column for
#'    calculation.
#'  - reg_df - Sets to the number of observations.
#'  - precision - Calculated using an external parameter precision_type.
#' @param col_name [character] Name of the column to handle.
#' @return function
getMissingDefaultColumnsHandling <- function(col_name){
  # Specify the columns that can be omitted by the user
  possible_missing_columns <- c("t_stat", "study_size", "reg_df", "precision")
  if (!col_name %in% possible_missing_columns){
    stop("Incorrect name of the missing column to handle.")
  }
  # Validate that this column is not required
  default_columns <- getDefaultColumns(names_only = FALSE)
  val <- default_columns[[col_name]]
  # Non-changeable value. Modify the default column setting if necessary.
  if (val == TRUE){
    message(paste(val,"is a non-changeable value."))
    stop("This value must always be provided by the user.")
  }
  # Return the function for this column
  if (col_name == "t_stat"){
    # Handle t-stat
    f <- function(input_data,...){ input_data$effect / input_data$se }
  } else if (col_name == "study_size"){
    # Handle study size
    f <- function(input_data,...){
      freq_table <- table(input_data$study_id) # Number of occurances of each study id
      # Study size - for each row of the data frame
      new_col <-  purrr::map_int(input_data$study_id, ~freq_table[as.character(.x)])
      return(new_col)
    }
  } else if (col_name == "reg_df"){
    # Handle DoF
    f <- function(input_data,...){ input_data$n_obs }
  } else if (col_name == "precision"){
    # Handle precision - takes in an additional paramter
    f <- function(input_data, precision_type){
      if (precision_type == "1/SE"){
        return ( 1/input_data$se )
      } else if (precision_type == "DoF"){
          return ( sqrt(input_data$reg_df) )
      } else {
        stop("Invalid type of precision")
      }
    }
  } else {
    stop("Incorrect column name.")
  }
  # Return the function call
  return(f)
}

#' Modify the user-defined column names so that they fit the script readable names
#' 
#' If any columns are set to NA by the user, they will be automatically initialized
#' to the values defined in the getMissingDefaultHandling function. For the detailed
#' list of these columns, see the function getMissingDefaultColumnsHandling().
#' 
#' Return two of the modified data frames. The variable list data frames is handled
#' too for unified variable names.
#' 
#' @param input_data [data.frame] Main data column.
#' @param input_var_list [data.frame] Data frame with variable information.
#' @param required_col_list [list] List with required/default column information.
#' @param precision_type [character] Type of precision.
#'    This parameter can be one of the following:
#'    - 1/SE - Inverse of the standard error is used.
#'    - DoF - Square root of the degrees of freedom is used.
#'    Deafults to "DoF".
#' @return List of two data frames - main data frame, variable data frame.
renameUserColumns <- function(input_data, input_var_list, required_cols_list, precision_type){
  # Default column names for the script recognition
  def_cols_list <- getDefaultColumns(names_only = FALSE) # Whole list
  def_cols <- names(def_cols_list) # Names of columns
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is(required_cols_list, "list"),
    is.character(precision_type),
    precision_type %in% c("1/SE", "DoF")
  )
  # Extract the expected colnames (by script)
  expected_colnames <- names(required_cols_list)
  if (!all(expected_colnames == def_cols)){
    message("You have modified the names of the source columns in the required_cols list.")
    message("Please rename the columns back to:")
    message(def_cols)
    stop("Incorrect source column names.")
  }
  # Iterate over all columns and validate their presence
  for (col_name in expected_colnames){
    required <- def_cols_list[[col_name]] # Boolean
    user_col_name <- required_cols_list[[col_name]]
    # Column required, but name in user data not specified (set to NA)
    if (is.logical(user_col_name)){
      if (required & !is.na(user_col_name)){
        # Required, but set to NA (not present in data set)
        message(paste("The column", col_name, "must be present in your data."))
        message("Make sure its expected name in required_cols is correctly specified.")
        stop("Required column not present in source data.")
      }
      # Missing columns in data
      if (is.na(user_col_name)){
        # Get the function call for the new column
        new_col_function <- getMissingDefaultColumnsHandling(col_name)
        # Get the values of the column
        new_col_values <- new_col_function(input_data,
                                           precision_type = precision_type)
        input_data[[col_name]] <- new_col_values # Initialize the column with these values
        next
      }
    } else if (! user_col_name %in% colnames(input_data)){
    # Check that the user-defined name appears in the data - for non-character only
      message(paste("The column name", user_col_name, "does not appear in your data."))
      stop("Incorrect user column specification.")
    }
    # Rename the column in source data from user's name to script recognized
    names(input_data)[names(input_data) == user_col_name] <- col_name
    # Rename the variable in variable list using dplyr - kinda RRRR approach
    if (user_col_name %in% input_var_list$var_name){
      input_var_list <- input_var_list %>%
        mutate(var_name = replace(var_name, input_var_list$var_name == user_col_name, col_name))
    }
  }
  # Print verbose output and return new data frame
  renameUserColumnsVerbose()
  return(list(input_data, input_var_list))
}

#' Verbose output for the renameUserColumns function
renameUserColumnsVerbose <- function(...){
  print("Several data frame columns renamed to fit the script expected form.")
}

#' Winsorize input data
#'
#' This function applies Winsorization to the data to minimize the effect of outliers on the statistical analysis.
#' Winsorization is a process of limiting extreme values by replacing them with the next lowest or
#' highest value that is within a certain percentile range.
#'
#' @param input_data [data.frame] A data frame containing the main effect (effect),
#' standard error of the effect (se), and t-statistic (t_stat) columns.
#' @param win_level [float] A numeric value between 0 and 1, exclusive, specifying the percentage of
#' extreme values to be replaced with the nearest non-extreme value.
#' @param winsorize_precision [logical] If TRUE, winsorize precision. Used for different types of
#' precision. Defaults to TRUE.
#' @return A data frame with the same columns as input_data, where the effect, standard error of the effect,
#' and t-statistic columns have been Winsorized.
winsorizeData <- function(input_data, win_level, winsorize_precision = TRUE){
  # Validate input
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.numeric(win_level),
    win_level > 0,
    win_level < 1,
    all(required_cols %in% colnames(input_data)),
    !(any(is.na(input_data$effect))), # No missing effect values
    !(any(is.na(input_data$se))) # No missing SE values
    )
  # Get the winsorization interval
  win_int <-  c(win_level, 1-win_level) # e.g. c(0.01, 0.99)
  # Get the winsorization function
  win_fun <- function(input_vector, win_interval = win_int){
    Winsorize(x = input_vector, minval = NULL, maxval = NULL, probs = win_interval)
  }
  # Winsorize the necessary columns
  input_data$effect <- win_fun(input_data$effect)
  input_data$se <- win_fun(input_data$se)
  input_data$t_stat <- win_fun(input_data$t_stat)
  if (winsorize_precision){
    input_data$precision <- win_fun(input_data$precision)
  }
  # Add a column indicating t statistic significance
  input_data$significant_t <- c(rep(0,nrow(input_data)))
  input_data$significant_t[(input_data$t_stat > 1.96) | (input_data$t_stat < -1.96)] <- 1
  # Return 
  winsorizeDataVerbose()
  return(input_data)
}

#' Verbose output for the winsorizeData function
winsorizeDataVerbose <- function(...){
  print("Data winsorized successfully.")
}


#' An auxiliary method to limit the data frame to one study only. Use only for testing
#' 
#' @param input_data [data.frame] The main data frame
#' @param input_study_id [int] Study ID of the study to subset to
#' @return data.frame The subsetted data frame
limitDataToOneStudy <- function(input_data, input_study_id){
  stopifnot(
    is.data.frame(input_data)
  )
  # Validate the input
  study_id <- tryCatch(
    {
      as.numeric(input_study_id) # Dict value is a character by default
    },
    warning = function(e){
      message("Invalid index for subsetting data. Use an integer.")
      return(input_data)
    }
  )
  if(is.na(study_id)){ # NAs pass through the as.numeric function, but are not detecable before (jeez)
    # Do nothing
    return(input_data)
  }
  # Subset to one study
  stopifnot(study_id %in% input_data$study_id)
  study_data <- input_data[input_data$study_id == study_id,]
  stopifnot(nrow(study_data) > 0) # Check valid output
  # Extract info and return data
  study_name <- as.character(study_data[1,]$study_name)
  print(paste0('Subsetting the dataset to ', study_name))
  invisible(study_data)
}

######################### DATA EXPLORATION #########################

#' Compute summary statistics for selected variables in a data frame
#'
#' This function computes summary statistics for selected variables in a data frame,
#' including mean, median, minimum, maximum, standard deviation, and percentage of missing observations.
#' If a variable contains missing or non-numeric data, the corresponding summary statistics will be omitted.
#'
#' @param input_data [data.frame] The input data frame.
#' @param input_var_list [data.frame] A data frame with information about the variables to be summarized.
#' It should have columns "var_name", "data_type", and "variable_summary".
#' @param names_verbose [bool] If True, print out the descriptive variable names. If F,
#' print out the data frame column names. Defaults to TRUE.
#'
#' @return [data.frame] A data frame containing summary statistics for selected variables.
getVariableSummaryStats <- function(input_data, input_var_list, names_verbose = T){
  # List of the statistics to compute
  variable_stat_names <- c("Var Name", "Var Class", "Mean", "Median",
                            "Min", "Max", "SD", "Obs", "Missing obs")
  # Variables to preprocess
  desired_vars <- input_var_list[input_var_list$variable_summary == TRUE,]$var_name # Vector
  # Initialize output data frame
  df <- data.frame(matrix(nrow = length(desired_vars), ncol = length(variable_stat_names)))
  colnames(df) <- variable_stat_names
  
  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  for (var_name in desired_vars){
    var_data <- as.vector(unlist(subset(input_data, select = var_name))) # Roundabout way, because types
    var_specs <- input_var_list[input_var_list$var_name == var_name,] # Specifications for this variable
    var_class <- var_specs$data_type
    var_name_display <- ifelse(names_verbose, var_specs$var_name_verbose, var_name) # Variable display name
    row_idx <- match(var_name, desired_vars) # Append data to this row
    # Missing all data 
    if (!any(is.numeric(var_data), na.rm=TRUE) || all(is.na(var_data))){
      missing_data_vars <- append(missing_data_vars, var_name)
      df[row_idx, ] <- c(var_name, var_class, rep(NA, length(variable_stat_names) - 2))
      next
    }
    # Calculate the statistics
    var_mean <- round(mean(var_data, na.rm = TRUE), 3)
    var_median <- round(median(var_data, na.rm = TRUE), 3)
    var_sd <- round(sd(var_data, na.rm = TRUE), 3)
    var_min <- round(min(var_data, na.rm = TRUE), 3)
    var_max <- round(max(var_data, na.rm = TRUE), 3)
    var_obs <- sum(!is.na(var_data) & var_data != 0)
    var_missing <- round((sum(is.na(var_data)) / length(var_data)) * 100, 1)
    var_missing_verbose <- paste0(as.character(var_missing),"%")
    # Aggregate and append to the main DF
    row_data <- c(
      var_name_display,
      var_class,
      var_mean,
      var_median,
      var_min,
      var_max,
      var_sd,
      var_obs,
      var_missing_verbose
    )
    df[row_idx, ] <- row_data
  }
  # Print and return output data frame - for cacheing missing variable information
  out_list <- list(df, missing_data_vars)
  getVariableSummaryStatsVerbose(out_list)
  return(out_list)
}

#' Verbose output for the getVariableSummaryStats function
getVariableSummaryStatsVerbose <- function(out_list,...){
  # Validate input
  stopifnot(
    is(out_list, "list"),
    length(out_list) == 2
  )
  # Delist
  df <- out_list[[1]]
  missing_data_vars <- out_list[[2]]
  # Verbose output
  cat("Variable summary statistics:\n")
  print(df)
  cat("\n")
  if (length(missing_data_vars) > 0){
    print(paste0("Missing data for: ", length(missing_data_vars), " variables."))
    cat("\n")
  }
}

#' getMAVariablesDescriptionTable
#' 
#' This function computes and returns a descriptive statistics table for a list of
#' specified variables in a given data set. It outputs a data frame that includes the verbose name,
#' description, mean, and standard deviation of each variable. If the verbose argument is set to TRUE, 
#' t will print the table.
#' 
#' @param bma_data [data.frame] The BMA data frame. All column names should
#' be present in the 'var_name' column of the 'input_var_list' data frame.
#' @param input_var_list [data.frame] A data frame that lists the variables to describe.
#' @param verbose [logical] A boolean flag that controls whether to print the output data frame.
#' Default is TRUE.
#' 
#' @return [data.frame] A data frame that contains the following columns: 'Variable', 
#' Description', 'Mean', 'SD'. 'Variable' corresponds to 'var_name_verbose' from the 'input_var_list', 
#' Description' corresponds to 'var_name_description' from the 'input_var_list',
#' 'Mean' and 'SD' are the mean and standard deviation of the variable in 'bma_data'.
#' 
#' @examples
#' \dontrun{
#'   getMAVariablesDescriptionTable(bma_data, input_var_list, verbose = TRUE)
#' }
#'
#' @export
getMAVariablesDescriptionTable <- function(bma_data, input_var_list, verbose = T){
  # Input validation
  stopifnot(
    is.data.frame(bma_data),
    is.data.frame(input_var_list),
    all(colnames(bma_data) %in% input_var_list$var_name)
  )
  # Initialize the empty data frame
  desc_df <- data.frame(
    "variable" = character(0),
    "description" = character(0),
    "mean" = numeric(0),
    "sd" = numeric(0)
  )
  # Loop through all the available variables
  for (input_var in colnames(bma_data)){
    # Characters
    var_verbose <- input_var_list$var_name_verbose[input_var_list$var_name == input_var]
    var_desc <- input_var_list$var_name_description[input_var_list$var_name == input_var]
    var_mean <- mean(as.numeric(unlist(bma_data[,input_var])))
    var_sd <- sd(as.numeric(unlist(bma_data[,input_var])))
    # Temporary row
    temp_df <- data.frame(
      "variable" = as.character(var_verbose),
      "description" = as.character(var_desc),
      "mean" = round(as.numeric(var_mean),3),
      "sd" = round(as.numeric(var_sd),3)
    )
    # Join together
    desc_df <- rbind(desc_df, temp_df)
  }
  # Rename columns
  colnames(desc_df) <- c("Variable", "Description", "Mean", "SD")
  # Return the output
  if (verbose){
    getMAVariablesDescriptionTable(res, verbose = verbose)
  }
  return(desc_df)
}

#' Verbose output for the getMAVariablesDescriptionTable function
getMAVariablesDescriptionTableVerbose <- function(res,...){
  args <- list(...)
  verbose_on <- args$verbose
  # Print verbose output
  if (verbose_on){
    print("Model averaging variables description table:")
    print(res)
    cat("\n\n")
  }
}

#' The function getEffectSummaryStats() calculates the summary statistics for variables in a given data frame input_data
#'    using the percentage of correct classification (Effect) effect and sample size study_size columns,
#'    and returns a data frame with the results. The function takes as input input_var_list,
#'    a data frame that contains metadata about the variables in input_data and which variables to calculate
#'    summary statistics for. The summary statistics calculated are the mean, median, weighted mean,
#'    minimum, maximum, standard deviation, and number of observations. For the weighted mean,
#'    the inverse squared sample size is used as weights. The confidence level for the weighted mean
#'    confidence interval can be set using the conf.level parameter, which defaults to 0.95.
#'    If any input data is missing or non-numeric, it is ignored, and the variable is not included in the output.
#'    
#' @param input_data [data.frame] Main data frame.
#' @param input_var_list [data.frame] Data frame with variable information.
#' @param conf.level [numeric] Confidence level for the confidence intervals. Defaults to 0.95 (95%).
#' @param formal_output [logical] If TRUE, return the table in a form that can be used in LaTeX. Defaults to FALSE.    
#' 
#' The function returns a list containing a data frame containing the following columns:
#' -Var Name: The name of the variable.
#' -Var Class: The data type of the variable.
#' -Mean: The arithmetic mean of the effect for the variable.
#' -Median: The median of the effect for the variable.
#' -Weighted Mean: The weighted mean of the effect for the variable, using the inverse squared sample size as weights.
#' -WM CI lower: The lower bound of the confidence interval for the weighted mean.
#' -WM CI upper: The upper bound of the confidence interval for the weighted mean.
#' -Min: The minimum effect value for the variable.
#' -Max: The maximum effect value for the variable.
#' -SD: The standard deviation of the effect for the variable.
#' -Obs: The number of observations for the variable.
#' If a variable has missing or non-numeric data, it will not be included in the output.
#' If no variables are included in the output, the function returns an empty data frame.
getEffectSummaryStats <- function (input_data, input_var_list, conf.level = 0.95, formal_output = FALSE) {
  # Parameter checking
  stopifnot(all(c(conf.level > 0, conf.level < 1)))
  
  # Constants
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE) # Z value for conf. int. calculation
  effect_data <- with(input_data, as.vector(effect))
  study_size_data <- with(input_data, as.vector(study_size))
  
  # Output columns
  effect_stat_names <- c("Var Name", "Var Class", "Mean", "CI lower", "CI upper", "Weighted Mean",
                     "WM CI lower", "WM CI upper", "Median", "Min", "Max", "SD", "Obs")
  
  # Variables to preprocess
  desired_vars <- input_var_list[input_var_list$effect_sum_stats == TRUE,]$var_name # Vector
  
  # Initialize output data frame
  df <- data.frame(col1 = character(),
                   col2 = character(),
                   col3 = numeric(),
                   col4 = numeric(),
                   col5 = numeric(),
                   col6 = numeric(),
                   col7 = numeric(),
                   col8 = numeric(),
                   col9 = numeric(),
                   col10 = numeric(),
                   col11 = numeric(),
                   col12 = numeric(),
                   col13 = numeric(),
                   stringsAsFactors = F
                   )
  stopifnot(ncol(df) == length(effect_stat_names))
  
  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  for (var_name in desired_vars){
    # Get data for this var
    var_data <- as.vector(unlist(subset(input_data, select = var_name))) # Roundabout way, because types
    var_specs <- input_var_list[input_var_list$var_name == var_name,] # Specifications for this variable
    var_class <- var_specs$data_type
    var_name_verbose <- var_specs$var_name_verbose
    row_idx <- match(var_name, desired_vars) # Append data to this row
    
    # Missing all data 
    if (any(
      !any(is.numeric(var_data), na.rm=TRUE), # No numerics
      all(is.na(var_data)), # All NAs
      nrow(var_data) == 0, # Empty data
      all(var_data %in% c(0,NA)) # Only 0s or NAs
      )){
      missing_data_vars <- append(missing_data_vars, var_name)
      next
    }
    
    # Get the specifications and subset the data accordingly
    equal_val <- var_specs$equal
    gtlt_val <- var_specs$gtlt
    stopifnot(xor(is.na(equal_val),is.na(gtlt_val))) # Additional validity check - should never occur
    # The specification is EQUAL
    if (!is.na(equal_val)){
      effect_data_equal <- effect_data[var_data == equal_val]
      study_size_data_equal <- study_size_data[var_data == equal_val] # For W. mean - wonky, but straightforward
      cutoff <- equal_val  # For verbose output
    } else { # The specification is gtlt
      if (gtlt_val %in% c("mean", "median")){
        cutoff <- ifelse(gtlt_val == 'mean', mean(var_data, na.rm=T), median(var_data, na.rm=T))
        effect_data_gt <- effect_data[var_data >= cutoff]
        effect_data_lt <- effect_data[var_data < cutoff]
        study_size_data_gt <- study_size_data[var_data >= cutoff]
        study_size_data_lt <- study_size_data[var_data < cutoff]
      } else if (!is.na(gtlt_val)){
        cutoff <- gtlt_val # For verbose output
        effect_data_gt <- effect_data[var_data >= gtlt_val]
        effect_data_lt <- effect_data[var_data < gtlt_val]
        study_size_data_gt <- study_size_data[var_data >= gtlt_val]
        study_size_data_lt <- study_size_data[var_data < gtlt_val]
      } else {
        stop("Value error")
      }
    }
    
    # A function for statistics calculation
    getNewDataRow <- function(input_var_name, input_class_name, input_effect_data, input_study_size_data){
      input_effect_data <- na.omit(input_effect_data)
      input_study_size_data <- na.omit(input_study_size_data)
      # Summary stats computation
      var_mean <- round(mean(input_effect_data), 3)
      var_sd <- round(sd(input_effect_data), 3)
      var_ci_lower <- round(var_mean - var_sd*z, 3)
      var_ci_upper <- round(var_mean + var_sd*z, 3)
      var_weighted_mean <- round(weighted.mean(input_effect_data, w = input_study_size_data^2),3)
      var_ci_lower_w <- round(var_weighted_mean - var_sd*z, 3)
      var_ci_upper_w <- round(var_weighted_mean + var_sd*z, 3)
      var_median <- round(median(input_effect_data), 3)
      var_min <- round(min(input_effect_data), 3)
      var_max <- round(max(input_effect_data), 3)
      var_obs <- length(input_effect_data)
      
      new_row <- data.frame(
        col1 = input_var_name,
        col2 = input_class_name,
        col3 = var_mean,
        col4 = var_ci_lower,
        col5 = var_ci_upper,
        col6 = var_weighted_mean,
        col7 = var_ci_lower_w,
        col8 = var_ci_upper_w,
        col9 = var_median,
        col10 = var_min,
        col11 = var_max,
        col12 = var_sd,
        col13 = var_obs
      )
      return (new_row)
    }
    # EQUAL data
    if (!is.na(equal_val)){
      equal_cutoff <- ifelse(cutoff == 1, "", paste0(" = ", round(cutoff, 3))) # None if equal to 1
      new_varname_equal <- paste0(var_name_verbose, equal_cutoff)
      new_row <- getNewDataRow(new_varname_equal, var_class, effect_data_equal, study_size_data_equal)
      df <- rbind(df, new_row)
    } else { # GTLT data
      new_varname_gt <- paste0(var_name_verbose, " >= ", round(as.numeric(cutoff), 3))
      new_varname_lt <- paste0(var_name_verbose, " < ", round(as.numeric(cutoff),3))
      new_row_gt <- getNewDataRow(new_varname_gt, var_class, effect_data_gt, study_size_data_gt)
      new_row_lt <- getNewDataRow(new_varname_lt, var_class, effect_data_lt, study_size_data_lt)
      df <- rbind(df, new_row_gt)
      df <- rbind(df, new_row_lt)
    }
  }
  # Add a row on top of the data frame with all observations
  first_row <- getNewDataRow("All Data", "any", effect_data, study_size_data)
  df <- rbind(first_row, df)
  # Put the final output together
  colnames(df) <- effect_stat_names
  # Format into a more presentable form
  if (formal_output){
    cols_to_drop <- c("Var Class", "Median", "Min", "Max", "SD")
    df <- df[,!names(df) %in% cols_to_drop]
  }
  # Print out verbose output and return a list with data and missing vars info
  out_list <- list(df, missing_data_vars)
  getEffectSummaryStatsVerbose(out_list)
  # Return data frame only
  return(out_list)
}

#' Verbose output for the getEffectSummaryStatsVerbose function
getEffectSummaryStatsVerbose <- function(out_list, ...){
  # Validate input
  stopifnot(
    is(out_list, "list"),
    length(out_list) == 2
  )
  # Delist
  df <- out_list[[1]]
  missing_data_vars <- out_list[[2]]
  # Verbose output
  cat("Summary statistics:\n")
  print(df)
  cat("\n")
  if (length(missing_data_vars) > 0){
    print(paste0("Missing data for ", length(missing_data_vars), " variables:"))
    print(missing_data_vars)
    cat("\n")
  }
}


#' Input the main data frame, specify a factor to group by, and create a box plot.
#' This plot is automatically printed out into the Plots window if verbose == TRUE.
#' 
#' @note This function is meant to be called from within the getLargeBoxPlot()
#'  function. If possible, do not use as a standalone function.
#' 
#' @param input_data [data.frame] Input data
#' @param factor_by [str] Factor to group by. Can be one of the following:
#'  - 'country'
#'  - 'study_level'
#'  Defaults to 'country'
#' @param theme [character] Theme to use. Defaults to "blue".
#' @param effect_name [character] Verbose explanation of the effect.
#' @param verbose [bool] If T, print out the information about the plot being printed.
#'  Defaults to F.
getBoxPlot <- function(input_data, factor_by = 'country', effect_name = 'effect', theme = "blue", verbose = F){
  # Check column and input validity
  required_cols <- getDefaultColumns()
  stopifnot(
    all(required_cols %in% colnames(input_data)),
    is.data.frame(input_data),
    is.character(factor_by),
    is.character(effect_name),
    is.logical(verbose)
  )
  # Plot variable preparation
  factor_levels <- rev(sort(unique(input_data[[factor_by]]))) # Dark magic - tells plot how to group y-axis
  factor_by_verbose <- gsub("_", " ", factor_by) # More legible y-axis label
  # Get the theme to use
  current_theme <- getTheme(theme)
  plot_colors <- switch(theme,
     blue = list("#005CAB","#e6f3ff","#0d4ed1"),
     yellow = list("#AB9800","#FFF5CC","#D1B00D"),
     green = list("#009B0F","#CCF3D1","#0DD146"),
     red = list("#AB0000","#FFCCCC","#D10D0D"),
    stop("Invalid theme type")
  )
  plot_outlier_color <- plot_colors[[1]]
  plot_fill <- plot_colors[[2]]
  plot_color <- plot_colors[[3]]
  vline_color <- ifelse(theme %in% c("blue", "green"), "#D10D0D", "#0d4ed1") # Make v-line contrast with the theme
  # Construct the plot - use !!sym(factor_by) to cast some more dark magic - makes plot recognize function input
  # Also double flip the axis - this makes ggplotly draw the boxes in the correct way. Really, what are these spells.
   box_plot <- ggplot(data = input_data, aes(y = effect, x=factor(!!sym(factor_by), levels = factor_levels))) +
       geom_boxplot(outlier.colour = plot_outlier_color, outlier.shape = 21, outlier.fill = plot_outlier_color,
                    fill=plot_fill, color = plot_color) +
       geom_hline(aes(yintercept = mean(effect)), color = vline_color, linewidth = 0.85) + 
       coord_flip() + # The dark speech of Mordor, let it be heard around every corner
       labs(title = NULL,y=paste("Effect of", tolower(effect_name)), x = "Grouped by " %>% paste0(factor_by_verbose)) +
       current_theme
  
  # Print the plot into the console
  if (verbose){
    print(paste0("Printing a box plot for the factor: ", factor_by))
    suppressWarnings(print(box_plot))
    cat("\n\n")
  }
  # Return the box plot
  return(box_plot)
}

#' Plot multiple box plots for datasets that have too large a number of studies
#' 
#' Input the data, specify how many studies should be displayed on a single box plot,
#' and plot a box plot for the subsets of data. If the data has fewer studies than
#' the maximum allowed amount, plot a single plot.
#' 
#' @note Function is meant for caching, and as the main way of creating box plots.
#'  Use mainly this function, not the getBoxPlot().
#' 
#' @param input_data [data.frame] Main data frame.
#' @param max_boxes [numeric] Maximum boxes to display in a single plot.
#' Defaults to 60.
#' @param verbose_on [logical] If TRUE, print out box plot information and box plots. Defaults to TRUE.
#' @param export_graphics [logical] If TRUE, export the plot to a png object into the graphics folder.
#'  Defaults to F.
#' @param graph_scale [numeric] Scale of the graph. Defaults to 3.
#' @param output_folder [character] Path to a folder where the plots should be stored. Defaults to NA.
#' @inheritDotParams getBoxPlot Parameters that should be used in the getBoxPlot function
#' call.
getLargeBoxPlot <- function(input_data, max_boxes = 60, verbose_on = T,
                            export_graphics = T, graph_scale = 3, output_folder = NA, ...){
  # Check that the number of studies is traceable, validate input
  stopifnot(
    is.data.frame(input_data),
    is.numeric(graph_scale),
    "study_id" %in% colnames(input_data)
  )
  # Get the factor information
  args <- list(...)
  factor_by <- args$factor_by
  stopifnot(factor_by %in% colnames(input_data)) # Validation
  # Split the data into subsets - works well with one split too
  all_factors <- as.vector(unlist(data[factor_by]))
  unique_factors <- sort(unique(all_factors))
  n_factors <- length(unique_factors)
  datasets <- list()
  remaining_factors <- unique_factors
  splits <- 0
  while (length(remaining_factors) > 0){
    # Use an upper bound for slicing - either max boxes, or length of remaining factors
    upper_bound <- ifelse(length(remaining_factors) > max_boxes, max_boxes, length(remaining_factors))
    split_factors <- remaining_factors[1:upper_bound] # Slice of factors to use
    temp_df <- input_data[all_factors %in% split_factors,] # Data subset
    datasets[[splits + 1]] <- temp_df
    remaining_factors <- remaining_factors[!remaining_factors %in% split_factors] # Drop used factors
    splits <- splits + 1
  }
  # Get the list of objects to return
  out_list <- list(box_plots = list(), factor_by = factor_by) # factor_by from the dots args
  # Print a box plot for each subset of data
  for (dataset in datasets){
    box_plot <- getBoxPlot(dataset, ...)
    out_list$box_plots <- c(out_list$box_plots, list(box_plot)) # All to the first index
  }
  # Print the output
  if (verbose_on){
    getBoxPlotVerbose(out_list, verbose_on = verbose_on)
    # Print graphs externally - this allows them to not get reprinted during cached runs
    for (bp in out_list$box_plots){
      suppressWarnings(print(bp))
    }
  }
  # Export to a html object
  if (export_graphics){
    if (is.na(output_folder)){
      stop("You must specify an output folder path.")
    }
    bp_idx <- 1
    use_indexing <- length(out_list$box_plots) > 1
    for (bp in out_list$box_plots){
      bp_counter <- ifelse(use_indexing,
                           paste0("_",bp_idx), # _1, _2, ...
                           "") # No indexing for single plots
      out_path <- paste0(output_folder, "box_plot_",factor_by,bp_counter,".png")
      hardRemoveFile(out_path) # Remove if exists
      ggsave(filename = out_path, plot = bp,
             width = 800*graph_scale, height = 1100*graph_scale, units = "px")
      bp_idx <- bp_idx + 1
    }
  }
  # Return the list
  return(out_list)
}


#' Verbose output for the getBoxPlot function
getBoxPlotVerbose <- function(out_list, ...){
  args <- list(...)
  verbose_on <- args$verbose_on
  # Validate input
  stopifnot(
    is(out_list, "list"),
    length(out_list) == 2
  )
  # Extract function output
  box_plots <- out_list[[1]]
  factor_by <- out_list[[2]]
  # Print out the output
  if (verbose_on){
    bp_idx <- 1
    bp_count <- length(box_plots)
    for (box_plot in box_plots){
      bp_counter <- paste0(bp_idx,"/",bp_count," ")
      bp_verbose <- ifelse(bp_count > 1, bp_counter, "") # No info if single plot
      print(paste0("Printing a box plot ",bp_verbose,"for the factor: ", factor_by))
      cat("\n")
      bp_idx <- bp_idx + 1
    }
  }
}


#' Identify outliers in the data, return the filter which can be used
#'  to get the data without these outliers.
#'  
#' How it works:
#'  In order for an observation to be identified as an outlier, both specifications
#'  must be FALSE. These specifications are:
#'  -'effect_proximity' - specifies, how far from the mean the observations are allow
#'    to appear for them to still not be considered outliers. As an example, for
#'    effect_proximity = 0.2, all observations that are 20% of observations away on
#'    either side will be marked as outliers by this specification.
#'  - 'maximum_precision' - specifies the maximum precision where the observations can
#'    appear for them not to be marked as outliers. Anything above things point will
#'    be considered an outlier by this specification.
#'  Keep in mind that both these specifications must be violated in order for the point
#'    to be marked as outlier.
#' 
#' @param input_data Data to check
#' @param effect_proximity A float indicating how many percentage points (this value times 100)
#'  away from the mean on either side can an observation appear.
#' @param maximum_precision A flot indicating the maximum precision of the sample in
#'  percentage (this value times 100) can the observations observe not to be considered outliers.
#' @param verbose If true, print out information about the outliers
#' @return [list] Filter for the data without outliers
getOutliers <- function(input_data, effect_proximity = 0.2, maximum_precision = 0.2, verbose=T) {
  # Check column validity
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.numeric(effect_proximity),
    is.numeric(maximum_precision),
    is.logical(verbose),
    all(required_cols %in% colnames(input_data)),
    effect_proximity <= 1,
    effect_proximity >= 0,
    maximum_precision <= 1,
    maximum_precision >= 0
  )
  
  # Get source values
  obs <- input_data$obs_id
  effect <- input_data$effect
  precision <- input_data$precision
  
  # Maximum values and effect mean value
  max_effect <- max(effect)
  max_precision <- max(precision)
  effect_mean <- mean(effect)
  
  # Calculate the cutoff bounds for both effect and precision as percentages of max value
  effect_cutoff <- max_effect * effect_proximity
  precision_cutoff <- max_precision * maximum_precision
  
  # Create filters
  effect_filter_lbound <- effect < effect_mean - effect_cutoff # Below lower bound
  effect_filter_ubound <- effect > effect_mean + effect_cutoff # Above upper bounds
  effect_filter <- effect_filter_lbound | effect_filter_ubound # Out of the proximity bound
  precision_filter <- precision >= precision_cutoff
  outlier_filter <- effect_filter & precision_filter
    
  # Filter suspicious observations
  outliers <- obs[outlier_filter]
  if ((length(outliers)>0) & (verbose)) {
    # Get the list of studies with outliers
    suspicious_studies <- c()
    for (outlier in outliers) {
      study <- as.character(input_data[outlier, 'study_name'])
      if (!study %in% suspicious_studies) {
        suspicious_studies <- c(suspicious_studies, study) # Add to the vector
      }
    }
    
    # Print out the information
    print("Funnel plot outlier information:")
    print(paste('Outliers found:', length(outliers)), sep=' ')
    print('Data rows:')
    print(outliers)
    print('Suspicious studies:')
    print(suspicious_studies)
    cat('\n\n')
  }
  
  # Return the negated filter
  return(!outlier_filter)
  
}

#' Generate ticks for a funnel plot
#'
#' This function takes a vector of three numbers as input, which represent the lower bound,
#' upper bound, and mean value. It generates a sorted vector of tick values between the lower
#' and upper bounds, where ticks are spaced at intervals of 10, excluding ticks that are closer
#' than 2 to either bound. The input mean value is also included in the output vector. Additionally,
#' the function generates a vector of colors ("black" or "red") with the same length as the output
#' vector, where the "red" color corresponds to the position of the mean value.
#'
#' @param input_vec [numeric(3)] A numeric vector of length 3, containing the lower bound, upper bound, and mean value.
#' @param theme [character] Theme to use for the ticks
#' @return A list with two elements: "output_vec", a sorted numeric vector containing the generated tick values and the mean value,
#'         and "x_axis_tick_text", a character vector of the same length as "output_vec", 
#'         with "red" indicating the position of the mean value and "black" for all other positions.
generateFunnelTicks <- function(input_vec, theme = "blue"){
  lower_bound <- input_vec[1]
  upper_bound <- input_vec[2]
  mean_value <- input_vec[3]
  
  ticks <- c(lower_bound, upper_bound)
  current_tick <- ceiling(lower_bound / 10) * 10 # Closest number divisible by 10 higher than lower bound
  
  while (current_tick < upper_bound) {
    if (abs(current_tick - lower_bound) >= 2 && abs(current_tick - upper_bound) >= 2) {
      ticks <- c(ticks, round(current_tick, 2))
    }
    current_tick <- current_tick + 10
  }
  
  # Add the mean value and sort the vector
  funnel_ticks <- sort(c(ticks, mean_value))
  
  # Create the color vector
  x_axis_tick_text <- rep("black", length(funnel_ticks))
  mean_index <- which(funnel_ticks == mean_value)
  x_axis_tick_text[mean_index] <- ifelse(theme %in% c("blue", "green"), "red", "blue")
  
  # Round all ticks to 2 decimal spaces
  funnel_ticks <- round(funnel_ticks, 2)
  
  return(list("funnel_ticks" = funnel_ticks, "x_axis_tick_text" = x_axis_tick_text))
}


#' Input the main data frame, several specifications, and create a funnel plot
#' 
#' @Note: In accordance with Stanley (2005), we decide to use the square root of the degrees of freedom
#'  isntead of 1/SE as a measure of precision, to account for study size.
#'  
#' @param input_data [data.frame] Main data frame. Must contain cols 'effect', 'precision'
#' @param effect_proximity [float] Cutoff point for the effect. See getOutliers() for more.
#' @param maximum_precision [float] Cutoff point for precision. See getOutliers() for more.
#' @param use_study_medians [bool] If TRUE, plot medians of studies instead of all observations.
#'  Defaults to FALSE.
#' @param theme [character] Theme to use. Defaults to "blue".
#' @param verbose [bool] If T, print out outlier information. Defaults to T.
#' @param export_graphics [bool] If TRUE, export the plot to png object into the graphics folder.
#'  Defaults to FALSE.
#' @param graph_scale [numeric] Scale for the output graph. Defaults to 3.
#' @param output_path [character] Full path to where the plot should be stored. Defaults to NA.
getFunnelPlot <- function(input_data, effect_proximity=0.2, maximum_precision=0.2,
                          use_study_medians = F, theme = "blue", verbose = T,
                          export_graphics = F, output_path = NA, graph_scale = 3){
  # Check input validity
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.numeric(effect_proximity),
    is.numeric(maximum_precision),
    is.logical(verbose),
    is.logical(export_graphics),
    is.numeric(graph_scale),
    all(required_cols %in% colnames(input_data)),
    effect_proximity <= 1,
    effect_proximity >= 0,
    maximum_precision <= 1,
    maximum_precision >= 0
  )
  
  # Filter out the outliers
  filter_effect <- getOutliers(input_data, effect_proximity=effect_proximity, maximum_precision=maximum_precision, verbose=verbose)
  
  # Create the data frame for the funnel plot
  funnel_data <- input_data[filter_effect, c('study_id', 'effect', 'precision')] # Only Effect, Precision
  funnel_data[] <- lapply(funnel_data, as.numeric) # To numeric
  
  # Plot study medians instead
  if (use_study_medians){
    funnel_data <- funnel_data %>%
      group_by(study_id) %>% 
      summarize(median_effect = median(effect),
              median_precision = precision[which.min(abs(effect - median_effect))])
    colnames(funnel_data) <- c("study_id", "effect", "precision")
  }
  
  # Get visual bounds and tick colorshttp://127.0.0.1:45381/graphics/plot_zoom_png?width=1200&height=900
  funnel_x_lbound <- min(funnel_data$effect)
  funnel_x_ubound <- max(funnel_data$effect)
  mean_x_tick <- mean(funnel_data$effect)
  # Generate and extract the info
  base_funnel_ticks <- c(funnel_x_lbound, funnel_x_ubound, mean_x_tick) # c(lbound, ubound, mean)
  funnel_visual_info <- generateFunnelTicks(base_funnel_ticks, theme = theme)
  funnel_ticks <- funnel_visual_info$funnel_ticks
  funnel_tick_text <- funnel_visual_info$x_axis_tick_text
  # Get the theme to use
  current_theme <- getTheme(theme, x_axis_tick_text = funnel_tick_text)
  point_color <- switch(theme,
    blue = "#1261ff",
    yellow =  "#FFD700",
    green =  "#00FF00",
    red =  "#FF0000",
    stop("Invalid theme type")
  )
  vline_color <- ifelse(theme %in% c("blue", "green"), "#D10D0D", "#0d4ed1") # Make v-line contrast with the theme
  
  # Plot the plot
  x_title <- ifelse(use_study_medians, "study median values", "all observations")
  quiet(
    funnel_win <- ggplot(data = funnel_data, aes(x = effect, y = precision)) + 
      geom_point(color = point_color) + 
      geom_vline(aes(xintercept = mean(effect)), color = vline_color, linewidth = 0.5) + 
      labs(title = NULL, x = paste("Estimate of the effect -",x_title), y = "Precision of the effect") +
      scale_x_continuous(breaks = funnel_ticks) +
      current_theme
  )
    
  # Print out the plot
  if (verbose){
    suppressWarnings(print(funnel_win))
  }
  # Export to a png object
  if (export_graphics){
    if (is.na(output_path)){
      stop("You must specify an output path.")
    }
    hardRemoveFile(output_path)
    ggsave(filename = output_path, plot = funnel_win,
           width = 800*graph_scale, height = 736*graph_scale, units = "px")
  }
  # Return the R plot object
  return(funnel_win)
}

#' Generate ticks for a histogram plot
#'
#' This function takes a vector of five numbers as input, which represent the lower bound,
#' upper bound, mean value, lower t-statictic, and upper t-statistic. It generates a sorted vector of
#' tick values between the lower and upper bounds, where ticks are spaced at intervals of 50, excluding ticks
#' that are closer than 2 to either bound or the t-stats. The input mean value, as well as the two t-statistics
#' are included in the output vector. Additionally, the function generates a vector of
#' colors ("black", "red", or "darkorange") with the same length as the output vector, where the
#' "red" color corresponds to the positions of the t-statistics values and "darkorange" corresponds
#' to the position of the mean value.
#'
#' @param input_vec [numeric(3)] A numeric vector of length 5, containing the lower bound, upper bound, mean value, and the t-stats.
#' @param theme [character] Type of theme to use
#' @return A list with two elements: "output_vec", a sorted numeric vector containing the generated tick values, the mean value,
#'         and the t-statistics values, and "x_axis_tick_text", a character vector of the same length as "output_vec",
#'         with "red" indicating the positions of the t-statistics values, "darkoran
generateHistTicks <- function(input_vec, theme = "blue") {
  lower_bound <- input_vec[1]
  upper_bound <- input_vec[2]
  mean_value <- input_vec[3]
  t_stat_low <- input_vec[4]
  t_stat_high <- input_vec[5]
  
  # Exclude lower or upper bound if it's closer than 2 to any of the t-statistics values
  ticks <- c()
  if (abs(lower_bound - t_stat_low) >= 2 && abs(lower_bound - t_stat_high) >= 2) {
    ticks <- c(ticks, lower_bound)
  }
  if (abs(upper_bound - t_stat_low) >= 2 && abs(upper_bound - t_stat_high) >= 2) {
    ticks <- c(ticks, upper_bound)
  }
  
  ticks <- c(ticks, t_stat_low, t_stat_high) # Base vector
  
  #' For two numeric values, find the highest possible number (step) from a predefined list
  #' that splits the range between these two values into at least 3 roughly equal segments.
  #' Example: For numbers 0 and 60, such step could be 25 -> 0, 25, 50, 60
  findStepLength <- function(a,b){
    # Calculate the range
    range_ <- b - a - ((b-a)/25) # Range minus a small fraction
    
    ticks <- c(50, 25, 10, 5, 2, 1, 0.5, 0.25, 0.1, 0.01, 0.001)
    for (tick in ticks){
      ticks_inside_range <- ceiling(range_ / tick) - 1
      if (ticks_inside_range >= 2){ # At least 2 ticks in the range
        return(tick)
      }
    }
  }
  step_length <- findStepLength(lower_bound, upper_bound)
  
  # Start with the closest number divisible by the step higher than lower bound
  current_tick <- ceiling(lower_bound / step_length) * step_length
  
  while (current_tick < upper_bound) {
    # If not too close to bounds/t-stats, add the tick to tick list
    if (abs(current_tick - lower_bound) >= 2 && 
        abs(current_tick - upper_bound) >= 2 &&
        abs(current_tick - t_stat_low) >= 2 &&
        abs(current_tick - t_stat_high) >= 2 &&
        abs(current_tick - mean_value) >= 2
        ) {
      ticks <- c(ticks, round(current_tick, 2))
    }
    current_tick <- current_tick + step_length
  }
  
  # Add the mean value and sort the vector
  hist_ticks <- sort(c(ticks, mean_value))
  
  # Create the color vector
  x_axis_tick_text <- rep("black", length(hist_ticks))
  mean_index <- which(hist_ticks == mean_value)
  t_stat_low_index <- which(hist_ticks == t_stat_low)
  t_stat_high_index <- which(hist_ticks == t_stat_high)
  
  # Get colors for the ticks
  x_axis_tick_text[mean_index] <- ifelse(theme %in% c("blue", "green"), "darkorange", "darkgreen")
  x_axis_tick_text[t_stat_low_index] <- ifelse(theme %in% c("blue", "green"), "red", "blue")
  x_axis_tick_text[t_stat_high_index] <- ifelse(theme %in% c("blue", "green"), "red", "blue")
  
  # Round the tick values to 2 decimal points
  hist_ticks <- round(hist_ticks, 2)
  
  return(list("hist_ticks" = hist_ticks, "x_axis_tick_text" = x_axis_tick_text))
}


#' Generate a histogram of the T-statistic values for the given input data, with the 
#'  option to specify lower and upper cutoffs for filtering outliers.
#' 
#' @param input_data A data frame containing the T-statistic values to be plotted.
#' @param lower_cutoff An optional numeric value specifying the lower cutoff for filtering outliers. Default is -150.
#' @param upper_cutoff An optional numeric value specifying the upper cutoff for filtering outliers. Default is 150.
#' @param lower_tstat A numeric value specifying which t statistic should be highlighted in the plot
#' @param upper_tstat Similar to lower_tstat
#' @param theme Theme to use. Defaults to "blue".
#' @param verbose If TRUE, print out the plot. Defaults to TRUE.
#' @param export_graphics If TRUE, export the plot to a png object into the graphics folder. Defaults to TRUE.
#' @param output_path Full path to where the plot should be stored. Defaults to NA.
#' @param graph_scale Numeric, scale the graph by this number. Defaults to 6.
#' @return A histogram plot of the T-statistic values with density overlay and mean, as well as vertical
#'  lines indicating the critical values of a two-tailed T-test with a significance level of 0.05.
getTstatHist <- function(input_data, lower_cutoff = -120, upper_cutoff = 120,
                         lower_tstat = -1.96, upper_tstat = 1.96, theme = "blue", verbose = T,
                         export_graphics = T, output_path = NA, graph_scale = 6){
  # Specify a cutoff filter
  t_hist_filter <- (input_data$t_stat > lower_cutoff & input_data$t_stat < upper_cutoff) #removing the outliers from the graph
  hist_data <- input_data[t_hist_filter,]
  
  # Get lower bound
  lbound_choices <- c(lower_cutoff, min(hist_data$t_stat)) # Either lowest t-stat, or cutoff point
  hist_lbound <- lbound_choices[which.max(lbound_choices)] # Choose the higher one
  # Get upper bound
  ubound_choices <- c(upper_cutoff, max(hist_data$t_stat)) # Either highest t-stat, or cutoff point
  hist_ubound <- ubound_choices[which.min(ubound_choices)] # Choose the lower one
  # Put all the visual information input together
  hist_mean <- mean(hist_data$t_stat)
  base_hist_ticks <- c(hist_lbound, hist_ubound, hist_mean, lower_tstat, upper_tstat)
  # Generate and extract variable visual information
  hist_visual_info <- generateHistTicks(base_hist_ticks, theme = theme)
  hist_ticks <- hist_visual_info$hist_ticks
  hist_ticks_text <- hist_visual_info$x_axis_tick_text
  # Get the theme to use
  current_theme <- getTheme(theme, x_axis_tick_text = hist_ticks_text)
  fill_color <- switch(theme,
    blue = "#1261ff",
    yellow =  "#FFD700",
    green =  "#00FF00",
    red =  "#FF0000",
    stop("Invalid theme type")
  )
  mean_line_color <- ifelse(theme %in% c("blue", "green"), "darkorange", "darkgreen")
  tstat_line_color <- ifelse(theme %in% c("blue", "green"), "#D10D0D", "#0d4ed1") # Make v-line contrast with the theme
  # Construct the histogram
  quiet(
    t_hist_plot <- ggplot(data = hist_data, aes(x = t_stat, y = after_stat(density))) +
      geom_histogram(color = "black", fill = fill_color, bins = 80) +
      geom_vline(aes(xintercept = mean(t_stat)), color = mean_line_color, linetype = "dashed", linewidth = 0.7) + 
      geom_vline(aes(xintercept = lower_tstat), color = tstat_line_color, linewidth = 0.5) +
      geom_vline(aes(xintercept = upper_tstat), color = tstat_line_color, linewidth = 0.5) +
      labs(x = "T-statistic", y = "Density") +
      scale_x_continuous(breaks = hist_ticks) + 
      current_theme
  )
  # Print out the plot
  if (verbose){
    suppressWarnings(print(t_hist_plot))
  }
  # Export to a html object
  if (export_graphics){
    if (is.na(output_path)){
      stop("You must specify an output path.")
    }
    hardRemoveFile(output_path)
    ggsave(filename = output_path, plot = t_hist_plot,
           width = 403*graph_scale, height = 371*graph_scale, units = "px")
  }
  # Return R object
  return(t_hist_plot)
}

######################### LINEAR TESTS ######################### 

#' Extract the four coefficients from linear test in the order
#' - Intercept, Intercept SE, Slope, Slope SE
#' 
#' @param coeftest_object Coeftest object from the linear test
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @return [vector] - Vector of len 4, with the coefficients
extractLinearCoefs <- function(coeftest_object, verbose_coefs=T){
  # Check validity of the coeftest object
  stopifnot(
    nrow(coeftest_object) == 2,
    ncol(coeftest_object) == 4,
    colnames(coeftest_object)[1] == "Estimate",
    colnames(coeftest_object)[2] == "Std. Error"
  )
  
  # Extract coefficients
  pub_bias_coef <- round(coeftest_object[2,"Estimate"], 3)
  pub_bias_se <- round(coeftest_object[2,"Std. Error"], 3)
  effect_coef <- round(coeftest_object[1,"Estimate"], 3)
  effect_se <- round(coeftest_object[1,"Std. Error"], 3)
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs){
    pub_bias_se <- paste0("(", pub_bias_se, ")")
    effect_se <- paste0("(", effect_se, ")")
  }
  # Group and return quietly
  lin_coefs <- c(pub_bias_coef, pub_bias_se, effect_coef, effect_se)
  invisible(lin_coefs)
}

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

#' Run all the linear tests on data, and return a matrix of results.
#' These tests are ran: OLS, FE, RE, Weighted OLS (by study size),
#'  Weighted OLS (by precision).
#' 
#' @param data [data.frame] Input data
getLinearTests <- function(data) {
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data)))
  # OLS
  ols <- lm(formula = effect ~ se, data = data)
  ols_res <- coeftest(ols, vcov = vcovHC(ols, type = "HC0", cluster = c(data$study_id)))
  ols_coefs <- extractLinearCoefs(ols_res)
  # Between effects
  be <- plm(effect ~ se, model = "between", index = "study_id", data = data)
  be_res <- coeftest(be, vcov = vcov(be, type = "fixed", cluster = c(data$study_id)))
  be_coefs <- extractLinearCoefs(be_res)
  # Random Effects
  re <- plm(effect ~ se, model = "random", index = "study_id", data = data)
  re_res <- coeftest(re, vcov = vcov(re, type = "fixed", cluster = c(data$study_id)))
  re_coefs <- extractLinearCoefs(re_res)
  # Weighted by number of observations per study
  ols_w_study <- lm(formula = effect ~ se, data = data, weight = (data$study_size*data$study_size))
  ols_w_study_res <- coeftest(ols_w_study, vcov = vcovHC(ols_w_study, type = "HC0", cluster = c(data$study_id)))
  ols_w_study_coefs <- extractLinearCoefs(ols_w_study_res)
  # Weighted by precision
  ols_w_precision <- lm(formula = effect ~ se, data = data, weight = c(data$precision*data$precision))
  ols_w_precision_res <- coeftest(ols_w_precision, vcov = vcovHC(ols_w_precision, type = "HC0", cluster = c(data$study_id)))
  ols_w_precision_coefs <- extractLinearCoefs(ols_w_precision_res)
  # Combine the results into a data frame
  results <- data.frame(
    OLS = ols_coefs,
    BE = be_coefs,
    RE = re_coefs,
    OLS_weighted_study = ols_w_study_coefs,
    OLS_weighted_precision = ols_w_precision_coefs
  )
  rownames(results) <- c("Publication Bias", "(Standard Error)", "Effect Beyond Bias", "(Constant)")
  colnames(results) <- c("OLS", "Between Effects", "Random Effects", "Study weighted OLS", "Precision weighted OLS")
  # Print the results into the console and return
  getLinearTestsVerbose(results)
  return(results) 
}

#' Verbose output for the getLinearTests function
getLinearTestsVerbose <- function(res, ...){
  print("Results of the linear tests, clustered by study:")
  print(res)
  cat("\n\n")
}

######################### NON-LINEAR TESTS ######################### 


#' Extract the four coefficients from linear test in the order
#' - Intercept, Intercept SE
#' Assume a very simplitic form of the non-linear objects, where the coefficients
#' are the the first two positions of the object.
#' 
#' @param nonlinear_object Non-linear object from the linear test
#' @param pub_bias_present [bool] If T, the method returns publication bias coefs too.
#'  Deafults to F.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @return [vector] - Vector of len 4, with the coefficients
extractNonlinearCoefs <- function(nonlinear_object, pub_bias_present = F, verbose_coefs=T, ...){
  # Extract coefficients
  effect_coef <- round(as.numeric(nonlinear_object[1,1]), 3)
  effect_se <- round(as.numeric(nonlinear_object[1,2]), 3)
  if (pub_bias_present){
    pub_coef <- round(as.numeric(nonlinear_object[2,1]), 3)
    pub_se <- round(as.numeric(nonlinear_object[2,2]), 3)
  }
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs){
    effect_se <- paste0("(", effect_se, ")")
    if (pub_bias_present){
      pub_se <- paste0("(", pub_se, ")")
    }
  }
  # Group and return quietly
  if (pub_bias_present){
    nonlin_coefs <- c(pub_coef, pub_se, effect_coef, effect_se) # First two for pub bias
  } else {
    nonlin_coefs <- c("", "", effect_coef, effect_se)
  }
  invisible(nonlin_coefs)
}

###### PUBLICATION BIAS - WAAP (Ioannidis et al., 2017) ######

getWaapResults <- function(data, ...){
  WLS_FE_avg <- sum(data$effect/data$se)/sum(1/data$se)
  WAAP_bound <- abs(WLS_FE_avg)/2.8
  WAAP_reg <- lm(formula = effect ~ -precision, data = data[data$se<WAAP_bound,])
  WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(data$study_id)))
  WAAP_coefs <- extractNonlinearCoefs(WAAP_reg_cluster, ...)
  invisible(WAAP_coefs)
}

###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######


getTop10Results <- function(data, ...){
  T10_bound <- quantile(data$precision, probs = 0.9) #Setting the 90th quantile bound
  T10_reg <- lm(formula = effect ~ -precision, data = data[data$precision>T10_bound,]) #Regression using the filtered data
  T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(data$study_id)))
  T10_coefs <- extractNonlinearCoefs(T10_reg_cluster, ...)
  invisible(T10_coefs)
}


###### PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019) #####

#' Compute STEM-based method coefficients from input data
#'
#' This function computes coefficients using the STEM-based method from the \code{stem}
#' package (available at \url{https://github.com/Chishio318/stem-based_method}). The input data
#' should include the necessary columns for the STEM method, and the output will be a numeric
#' vector containing the estimated coefficients.
#'
#' @param data A data frame containing the necessary columns for the STEM-based method
#' @param script_path Full path to the source script.
#' @param print_plot If TRUE, print out the STEM plot.
#' @param export_graphics If TRUE, export the STEM plot.
#' @param export_path Path to the export folder. Deafults to ./graphics.
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated coefficients for the STEM-based method
#' in the usual format.
#' 
#' @import stem_method_master_thesis_cala.R
getStemResults <- function(data, script_path, print_plot = T, export_graphics = T, export_path = "./graphics", ...){
  source(script_path) #github.com/Chishio318/stem-based_method
  
  stem_param <- c(
    10^(-4), # Tolerance - set level of sufficiently small stem to determine convergence
    10^3 # max_N_count - set maximum number of iteration before termination
  )
  
  # Estimation
  est_stem <- stem(data$effect, data$se, stem_param)$estimates # Actual esimation
  # Stem plot
  funnel_stem_call <- quote(
    stem_funnel(data$effect, data$se, est_stem)
  )
  # Print and export the plot 
  if (print_plot){
    eval(funnel_stem_call)
  }
  if (export_graphics){
    stopifnot(is.character(export_path), length(export_path) > 0)
    validateFolderExistence(export_path)
    stem_path <- paste0(export_path, "/stem.png")
    hardRemoveFile(stem_path)
    png(stem_path)
    eval(funnel_stem_call)
    dev.off()
  }
  # Save results
  stem_coefs <- extractNonlinearCoefs(est_stem, ...)
  return(stem_coefs)
}


###### PUBLICATION BIAS - FAT-PET hierarchical in R ######

#' Compute hierarchical linear model coefficients from input data
#'
#' This function computes hierarchical linear model coefficients from input data using
#' the \code{rhierLinearModel} function from the \code{bayesm} package. It first organizes
#' the data by study and creates a list of regression data for each study. It then runs the
#' hierarchical linear model using default settings and extracts the estimated coefficients.
#' 
#' @param data A data frame containing the necessary columns for the hierarchical linear model
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated coefficients for the hierarchical linear
#' model in the usual format.
#' 
#' @import bayesm
getHierResults <- function(data, ...){
  study_levels_h <- levels(as.factor(data$study_name))
  nreg_h <- length(study_levels_h)
  regdata_h <- NULL
  for (i in 1:nreg_h) {
    filter <- data$study_name==study_levels_h[i] #T/F vector identifying if the observation is from the i-th study
    y <- data$effect[filter] #Effects from the i-th study
    X <- cbind(1,
               data$se[filter])
    regdata_h[[i]] <- list(y=y, X=X)
  }
  Data_h <- list(regdata=regdata_h)
  Mcmc_h <- list(R=6000)
  
  # Run the model silently
  quiet(
    out_h <- bayesm::rhierLinearModel(
      Data=Data_h,
      Mcmc=Mcmc_h),
  )
  
  # Save results
  quiet(
    hier_raw_coefs <- summary(out_h$Deltadraw)
  )
  hier_coefs <- extractNonlinearCoefs(hier_raw_coefs, ...)
  invisible(hier_coefs)
}

###### PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019) ######

#' Estimate the Selection Model and extract the coefficients for Effect and its SE
#'  - Source: https://maxkasy.github.io/home/metastudy/
#' 
#' This function computes selection model coefficients from input data using
#' the \code{metastudies_estimation} function from the \code{selection_model_master_thesis_cala.R}
#' package. It extracts the estimated effect and publication bias, as well as their
#' standard errors, and returns them as a vector..
#'
#' @param input_data A data frame containing the necessary columns for the selection model
#' @param script_path Full path to the source script.
#' @param cutoffs A numeric vector of cutoff values for computing the selection model
#' coefficients. The default is \code{c(1.960)}, corresponding to a 95% confidence interval.
#' @param symmetric A logical value indicating whether to use the symmetric or asymmetric
#' selection model. The default is \code{FALSE}, indicating the asymmetric model.
#' @param modelmu A character string indicating the type of model to use for the mean
#' effect estimate. The default is \code{"normal"}, corresponding to a normal distribution.
#' Another option is \code{"t"}, corresponding to a t-distribution.
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated effect and publication bias, as well
#' as their standard errors, in the usual format.
#' 
#' @import selection_model_master_thesis_cala.R
getSelectionResults <- function(data, script_path, cutoffs = c(1.960),
                                symmetric = F, modelmu="normal", ...){
  # Read the source script
  source(script_path) 
  # Validate input
  stopifnot(all(cutoffs %in% c(1.645, 1.960, 2.576))) # Cutoffs
  stopifnot(modelmu %in% c("normal", "t")) # Model
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data)))
  # Extract winsorized estimates, standard errors
  sel_X <- data$effect # Effect - Winsorized
  sel_sigma <- data$se # SE - Winsorized
  # Handle argument
  all_params <- list(
    X = sel_X,
    sigma = sel_sigma,
    cutoffs = cutoffs,
    symmetric = symmetric,
    model = modelmu
  )
  estimates <- do.call(
    metastudies_estimation,
    all_params
  )
  # Extract coefficients
  estimates_psi <- estimates$Psihat
  estimates_se <- estimates$SE
  estimates_vec <- c(estimates_psi[1], # Effect
                     estimates_se[1],  # Effect SE
                     estimates_psi[2], # Pub Bias
                     estimates_se[2]   # Pub Bias SE
                     )
  estimates_mat <- matrix(estimates_vec, nrow=2, ncol=2, byrow=TRUE)
  
  # Extract the coefficients and return as a vector
  sel_coefs <- extractNonlinearCoefs(estimates_mat, ...)
  return(sel_coefs)
}
  
###### PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020) ######

#' Estimate the Endogenous Kink model and extract the effect/pub_bias coefficients
#'  - Source: https://osf.io/f6nzb/

#'  @param data [data.frame] The main data frame on which to run the estimation on.
#'    Must contain the columns - "effect", and "se"
#'  @param script_path [character] Path to the source script
#'  @inheritDotParams Parameters for the extractNonlinearCoefs function.
#'  
#'  @return endo_kink_coefs [vector] The four desired coefficients, which are:
#'    - Pub bias estimate
#'    - Pub bias standard error
#'    - Mean effect estimate
#'    - Mean effect standard error
#'
#' @import endo_kink_master_thesis_cala.R
#'
#'  Note - The runEndoKink method returns the coefficients in order mean_effect-pub_bias,
#'    this way is just for easier printing into the console, so be mindful of that.
getEndoKinkResults <- function(data, script_path, ...){
  # Read the source file
  source(script_path)
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data))) 
  # Extract winsorized estimates, standard errors
  data <- data[,c("effect", "se")]
  # Run the model estimation and get the four coefficients
  estimates_vec <- runEndoKink(data, verbose = F)
  # Handle output and return verbose coefs
  estimates_mat <- matrix(estimates_vec, nrow=2, ncol=2, byrow=TRUE)
  endo_kink_coefs <- extractNonlinearCoefs(estimates_mat, ...)
  return(endo_kink_coefs)
}
  
###### NON-LINEAR MODELS RESULTS ######

#' Get Non-Linear Tests
#'
#' This function takes in a data frame and returns the results of several non-linear regression methods
#' clustered by study. It first validates that the necessary columns are present in the input data frame.
#' Then, it calls the functions getWaapResults(), getTop10Results(), getStemResults(), getHierResults(),
#' getSelectionResults(), and getEndoKinkResults() to get the coefficients for each method. Finally,
#' it combines the results into a data frame, prints the results to the console, and returns the data
#' frame silently.
#'
#' @param data The main data frame, onto which all the non-linear methods are then called.
#' @param script_paths List of paths to all source scripts.
#' @param export_graphics If TRUE, export various graphs into the graphics folder.
#' @param export_path Path to the export folder. Defaults to ./graphics.
#' @return A data frame containing the results of the non-linear tests, clustered by study.
getNonlinearTests <- function(input_data, script_paths, selection_params = NULL,
                              export_graphics = T, export_path = './graphics') {
  # Validate the input
  
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    all(required_cols %in% names(input_data)),
    is(script_paths, "list"),
    all(c("stem", "selection", "endo") %in% names(script_paths))
  )
  # Get script_paths
  stem_script_path <- script_paths$stem
  selection_script_path <- script_paths$selection
  endo_script_path <- script_paths$endo
  # Get parameters
  all_selection_params <- c(
    list(
      data = input_data,
      script_path = selection_script_path
    ),
    selection_params,
    list(pub_bias_present = T, verbose_coefs = T)
  )
  all_stem_params <- c(
    list(
      input_data,
      stem_script_path,
      print_plot = T,
      export_graphics = export_graphics,
      export_path = export_path,
      pub_bias_present = F,
      verbose_coefs = T
    )
  )
  # Get coefficients
  waap_res <- getWaapResults(input_data, pub_bias_present = F, verbose_coefs = T)
  top10_res <- getTop10Results(input_data, pub_bias_present = F, verbose_coefs = T)
  stem_res <- do.call(getStemResults, all_stem_params)
  hier_res <- getHierResults(input_data, pub_bias_present = T, verbose_coefs = T)
  sel_res <- do.call(getSelectionResults, all_selection_params)
  endo_kink_res <- getEndoKinkResults(input_data, endo_script_path, pub_bias_present = T, verbose_coefs = T)
  
  # Combine the results into a data frame
  results <- data.frame(
    waap_df = waap_res,
    top10_df = top10_res,
    stem_df = stem_res,
    hier_df = hier_res,
    sel_df = sel_res,
    endo_kink_df = endo_kink_res)
  
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)")
  colnames(results) <- c("WAAP", "Top10", "Stem", "Hierarch", "Selection", "Endogenous Kink")
  # Print the results into the console and return
  getNonlinearTestsVerbose(results)
  return(results) 
}

#' Verbose output for the getNonlinearTests function
getNonlinearTestsVerbose <- function(res, ...){
  print("Results of the non-linear tests, clustered by study:")
  print(res)
  cat("\n\n")
}

######################### RELAXING THE EXOGENEITY ASSUMPTION ######################### 

#' Extract the four coefficients from an exo test in the order
#' - Pub bias, Pub bias SE, Effect, Effect SE
#' Input a 2 by 2 matrix, where in the first row, you have the effect coefficients,
#'  and in the second row, the pub bias coefficients.
#' 
#' @param exo_object [matrix] Object from the exo tests, should be matrix (M(2,2))
#' @param effect_present [bool] If T, the method returns effect coefs. Defaults to T
#' @param pub_bias_present [bool] If T, the method returns publication bias coefs too.
#'  Deafults to T.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @return [vector] The four desired coefficients, which are, in order:
#'    - Pub bias estimate
#'    - Pub bias standard error
#'    - Mean effect estimate
#'    - Mean effect standard error
extractExoCoefs <- function(exo_object, effect_present = T, pub_bias_present = T, verbose_coefs=T){
  # Validate input
  stopifnot(
    is.logical(effect_present),
    is.logical(pub_bias_present),
    is.logical(verbose_coefs),
    effect_present || pub_bias_present # At least one
  )
  # Extract coefficients
  effect_coef <- ifelse(effect_present,
                        round(as.numeric(exo_object[1,1]), 3),
                        "")
  effect_se <- ifelse(effect_present,
                        round(as.numeric(exo_object[1,2]), 3),
                        "")
  pub_coef <- ifelse(pub_bias_present,
                        round(as.numeric(exo_object[2,1]), 3),
                        "")
  pub_se <- ifelse(pub_bias_present,
                        round(as.numeric(exo_object[2,2]), 3),
                        "")
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs){
    if (effect_present){
      effect_se <- paste0("(", effect_se, ")")
    }
    if (pub_bias_present){
      pub_se <- paste0("(", pub_se, ")")
    }
  }
  # Group and return quietly
  exo_coefs <- c(pub_coef, pub_se, effect_coef, effect_se)
  invisible(exo_coefs)
}


#' Identify the best instrument(s) from a set of instruments based on IV regression diagnostics.
#'
#' This function takes in a data frame, a list of potential instruments, and a vector of verbose names for each instrument. 
#' The function then runs IV regressions using each of the potential instruments, and returns the instrument(s)
#' with the best performance based on four different diagnostics: R-squared, weak instruments test, Wu-Hausman test,
#' and Sargan test. If multiple instruments are tied for the best performance, all of them will be returned.
#' The function also prints the identified best instrument(s).
#'
#' @param input_data [data.frame] A data frame containing the effect (effect), its standard error (se), study ids, and source
#' data for the instrument(s) (specified as separate columns). It must have the columns "effect", "se", "study_id", and "n_obs".
#' @param instruments [list] A list of potential instruments. Each element of the list should be a vector of numeric values.
#'  Ideally specify as 1/data$n_obs, etc.
#' @param instruments_verbose [vector] A vector of verbose names (strings) for each instrument. It must have the same length
#'  as the number of potential instruments.
#' @return a character vector containing the best instrument(s) identified by the function.
#' @examples
#' data("instrument_data")
#' instruments <- list(instrument_data$instrument1, instrument_data$instrument2)
#' instruments_verbose <- c("Instrument 1", "Instrument 2")
#' findBestInstrument(instrument_data, instruments, instruments_verbose)
findBestInstrument <- function(input_data, instruments, instruments_verbose){
  # Validity checks
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.list(instruments),
    is.vector(instruments_verbose),
    all(required_cols %in% colnames(input_data))
  )
  # Initialize an empty data frame - each row will be one instrument
  results <- data.frame(r_squared = numeric(length(instruments)),
                        weak_instruments = numeric(length(instruments)),
                        wu_hausman = numeric(length(instruments)),
                        sargan = numeric(length(instruments)))
  # Run the IV regressions and get diagnostics from each of them
  for (i in seq_along(instruments)) {
    instrument <- instruments[i][[1]] # Unlist
    stopifnot(is.numeric(instrument)) # Amend previous line if this keeps failing - should be only numeric
    instrument_verbose <- instruments_verbose[i]
    input_data$instr_temp <- instrument # Add a column with the instrument values
    iv_formula <- as.formula("effect ~ se | instr_temp")
    model <- ivreg(formula = iv_formula, data = input_data)
    model_summary <- summary(model, vcov = vcovHC(model, cluster = c(input_data$study_id)), diagnostics=T)
    # Extract relevant statistics
    results[i,"r_squared"] <- model_summary$r.squared
    results[i,"weak_instruments"] <- model_summary$diagnostics["Weak instruments", "p-value"]
    results[i,"wu_hausman"] <- model_summary$diagnostics["Wu-Hausman", "p-value"]
    results[i,"sargan"] <- model_summary$diagnostics["Sargan", "p-value"]
  }
  rownames(results) <- instruments_verbose
  # Find the row index with the best performing instrument
  # R-sq
  best_r_squared_idx <- ifelse(any(is.na(results$r_squared)), 
                               NA,
                               which.max(results$r_squared)) 
  # Weak instr
  best_weak_instruments_idx <- ifelse(any(is.na(results$weak_instruments)),
                              NA,
                              which.min(results$weak_instruments)) 
  # Wu Hausman
  best_wu_hausman_idx <- ifelse(any(is.na(results$wu_hausman)),
                                NA,
                                which.min(results$wu_hausman)) 
  # Sargan
  best_sargan_idx <- ifelse(any(is.na(results$sargan)),
                            NA,
                            which.min(results$sargan))
  # Get indexes into a table
  best_instruments_idx <- c(best_r_squared_idx, best_weak_instruments_idx, best_wu_hausman_idx, best_sargan_idx)
  freqs <- table(best_instruments_idx[!is.na(best_instruments_idx)]) # Remove NAs
  stopifnot(length(freqs) > 0) # All NAs
  # Get the most frequent index
  max_freq <- max(freqs)
  max_values <- sapply(names(freqs[freqs == max_freq]), as.numeric) # Numeric index of best performing instrument (or instruments)
  # Get the best instrument(s)
  best_instruments <- rownames(results[max_values,])
  # Return results - verbose
  if (length(best_instruments > 1)){
    print(paste0("Identified multiple best instruments:"))
    print(best_instruments)
  } else {
    print(paste0("Identified ", best_instruments, " as the best instrument."))
  }
  return(best_instruments)
}

#' getIVResults function
#'
#' This function takes in data and finds the best instrument for the IV regression of effect against se.
#' It then runs the IV regression and extracts the coefficients. The strength of the function is found
#' in being able to identify the best instrument automatically. The list of instruments is unmodifiable as of now.
#' 
#' The four instruments from which the function chooses are:
#' - 1/sqrt(data$n_obs)
#' - 1/data$n_obs
#' - 1/data$n_obs^2
#' - log(data$n_obs)
#'
#' @param data a data frame containing the data for the IV regression
#' @inheritDotParams ... additional arguments to be passed to extractExoCoefs
#'
#' @return a numeric vector containing the extracted coefficients from the IV regression
#'
#' @details The function defines a list of instruments to use, and finds the best instrument
#' by running a function called findBestInstrument. If multiple best instruments are identified,
#' the function arbitrarily chooses the first one. The function then runs the IV regression and
#' extracts the coefficients using extractExoCoefs.
#'
#' @examples
#' data <- data.frame(effect = rnorm(10), se = rnorm(10), n_obs = rep(10, 10), study_id = rep(1:10, each = 1))
#' getIVResults(data)
getIVResults <- function(data, ...){
  # Define the instruments to use
  instruments <- list(1/sqrt(data$n_obs), 1/data$n_obs, 1/data$n_obs^2, log(data$n_obs))
  instruments_verbose <- c('1/sqrt(n_obs)', '1/n_obs', '1/n_obs^2', 'log(n_obs)')
  # Find out the best instrument
  best_instrument <- findBestInstrument(data, instruments, instruments_verbose)
  # If more best instruments are identified
  if (length(best_instrument) > 1){ 
    best_instrument <- best_instrument[1] # Choose the first one arbitrarily
    print(paste("Choosing", best_instrument, "arbitrarily as an instrument for the regression."))
  }
  stopifnot(
    best_instrument %in% instruments_verbose,
    length(best_instrument) == 1 # Should be redundant
    )
  # Get instrument values instead of name
  best_instrument_values <- instruments[match(best_instrument, instruments_verbose)][[1]]
  # Run the regression
  data$instr_temp <- best_instrument_values
  iv_formula <- as.formula("effect ~ se | instr_temp")
  model <- ivreg(formula = iv_formula, data = data)
  model_summary <- summary(model, vcov = vcovHC(model, cluster = c(data$study_id)), diagnostics=T)
  # Get the coefficients
  all_coefs <- model_summary$coefficients
  IV_coefs_vec <- c(
    all_coefs["(Intercept)","Estimate"], # Effect
    all_coefs["(Intercept)", "Std. Error"], # Effect SE
    all_coefs["se", "Estimate"], # Pub Bias
    all_coefs["se", "Std. Error"] # Pub Bias SE
    ) 
  iv_coefs_mat <- matrix(IV_coefs_vec, nrow=2, ncol=2, byrow=TRUE)
  # Extract the coefficients and return as a vector
  iv_coefs_out <- extractExoCoefs(iv_coefs_mat, ...) 
  return(iv_coefs_out)
}

###### PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) ######

#' getMedians - Calculates the vector of medians for effect
#' Input the data frame, and the name of the column to calculate a vector of medians for,
#'  grouped by the study levels.
#' 
#' @param input_data [data.frame] Main data frame.
#' @param mec_col [str] Name of the column to compute the medians for. Must be in colnames
#'  of the input data frame.
#' @return [vector] Vector of medians by levels of the data frame studies.
getMedians <- function(input_data, med_col){
  # Validation
  stopifnot(all(c(med_col, 'study_name') %in% colnames(input_data)))
  # Preparation
  med_vec <- c()
  study_levels <- levels(as.factor(input_data$study_name)) # Names of studies as levels
  # Calculation
  for (study in study_levels) {
    col_data_numeric <- as.numeric(unlist(input_data[input_data$study_name == study,med_col]))
    med <- median(col_data_numeric)
    med_vec <- append(med_vec, med)
  }
  stopifnot(length(med_vec) == length(study_levels)) # Calculated median for all studies
  return(med_vec)
}

#' getPUniResults - Calculates publication bias test results using the p-uniform method
#'
#' This function calculates publication bias test results using the p-uniform method.
#' It takes in a data frame of the effects with their corresponding standard errors and either uses
#' the Maximum Likelihood (ML) or Moments (P) method to estimate the publication bias.
#'
#' @param data [data.frame] A data frame containing the effects with their corresponding standard errors.
#' @inheritDotParams Parameters to pass to the main 'puni_star' call
#'
#' @return A vector containing the following four elements:
#' \describe{
#' \item{Test Statistic for the P-uniform publication bias test}{A character string indicatingthe L test
#'  statistic for the P-uniform publication bias test.}
#' \item{P-value for the L test statistic}{A character string indicating the P-value for the L test statistic.}
#' \item{Effect Beyond Bias}{A numeric value indicating the effect beyond bias estimate.}
#' \item{Effect Standard Error}{A character string indicating the standard error of the effect beyond bias estimate.}
#' }
getPUniResults <- function(data, ...){
  # Validation
  stopifnot(
    is.data.frame(data)
  )
  # Calculate medians for all studies
  med_yi <- getMedians(data, "effect")
  med_ni <- getMedians(data, "study_size")
  med_ses <- getMedians(data, "se")
  med_sample_sizes <- getMedians(data, "n_obs")
  med_sdi <- med_ses * sqrt(med_sample_sizes) # SD = SE * sqrt(sample_size)
  # Get parameters
  all_params <- c(
    list(
      yi = med_yi,
      vi = med_sdi^2, # Squared sd
      ni = med_ni
    ),
    list(...)
  )
  #Estimation
  quiet(
    est_main <- do.call(
      puni_star,
      all_params
    )
  )
  # Extract and save coefficients - using a custom format for this method
  est_se <- (est_main$ci.ub - est_main$est) / 1.96 # Standard error of the estmiate
  est_effect_verbose <- round(est_main$est, 3) # Effect Beyond Bias
  est_se_verbose <- paste0("(", round(est_se, 3), ")") # Effect Standard Error
  est_pub_test_verbose <- paste0("L = ", round(est_main$L.0, 3)) # Test statistic of p-uni publication bias test
  est_pub_p_val_verbose <- paste0("(p = ", round(est_main$pval.0, 3), ")") # P-value for the L test statistic
  # Return as a vector
  p_uni_coefs_out <- c(
    est_pub_test_verbose,
    est_pub_p_val_verbose,
    est_effect_verbose,
    est_se_verbose
  )
  return(p_uni_coefs_out)
}

#' getExoTests
#'
#' Performs two tests for publication bias and exogeneity in instrumental variable (IV) analyses using clustered data.
#'
#' @param input_data [data.frame] A data frame containing the necessary columns: "effect", "se", "study_id", "study_size", and "precision".
#' @param puni_method [character] Method to be used for p-uniform calculation. One of "ML", "P". Defaults to "ML".
#' @param puni_params [list] Aruments to be used in p-uniform.
#' @return A data frame with the results of the three tests for publication bias and exogeneity in IV analyses using clustered data.
#'
#' @details This function first validates that the necessary columns are present in the input data frame.
#' If the validation is successful, it performs three tests for publication bias and exogeneity in instrumental variable (IV)
#' analyses using clustered data: the IV test, and the p-Uniform test. The results of the two tests are combined
#' into a data frame, with row names corresponding to the tests and column names corresponding to the test type.
#' The results are then printed into the console and returned invisibly.
getExoTests <- function(input_data, puni_params) {
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    all(required_cols %in% names(input_data))
  )
  # Get arguments
  all_puni_params <- c(
    list(
      data = input_data
    ),
    puni_params
  )
  # Get coefficients
  iv_res <- getIVResults(input_data, effect_present = T, pub_bias_present = T, verbose_coefs = T)
  p_uni_res <- do.call(getPUniResults, all_puni_params)
  # Combine the results into a data frame
  results <- data.frame(
    iv_df = iv_res,
    p_uni_df = p_uni_res)
  # Label names
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)")
  colnames(results) <- c("IV", "p-Uniform")
  # Print the results into the console and return
  getExoTestsVerbose(results)
  return(results) 
}

#' Verbose output for the getExoTests function
getExoTestsVerbose <- function(res, ...){
  print("Results of the tests relaxing exogeneity, clustered by study:")
  print(res)
  cat("\n\n")
}

######################### P-HACKING TESTS #########################

###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######

#' Run a Caliper Test
#'
#' This function performs a Caliper test on a data set to detect selective reporting of statistically significant results.
#'
#' @param input_data [data.frame] A data.frame containing the data set to be tested. The data.frame must have at least two columns named
#'  "t_stat" and "study_id", and these columns must be numeric.
#' @param threshold [numeric] The t-statistic threshold used to define statistically significant results. Default is 1.96.
#' @param width [numeric] The width of the Caliper interval used to define the sub-sample of observations used in the test. Default is 0.05.
#' @return A numeric vector with four elements: the estimate of the proportion of results reported, the standard error of the estimate,
#' the number of observations with t-statistics above the threshold, and the number of observations with t-statistics below the threshold.
runCaliperTest <- function(input_data, threshold = 1.96, width = 0.05){
  # Validate input
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.numeric(threshold),
    is.numeric(width),
    all(required_cols %in% colnames(input_data))
  )
  # Add a column indicating which observations have t-stats above (below) threshold
  if (threshold >= 0){ # Explicit because ifelse does not work, due to some dark spells probably
    significant_obs <- input_data$t_stat > threshold
  } else {
    significant_obs <- input_data$t_stat < threshold
  }
  input_data$significant_t <- ifelse(significant_obs, 1, 0) # Col of 0/1
  # Initialize variables for output storage
  caliper_output <- list()
  # Run the test
  lower_bound <- input_data$t_stat > ( threshold - width ) # Bool vector
  upper_bound <- input_data$t_stat < ( threshold + width ) # Bool vector
  subsetted_data <- input_data[lower_bound & upper_bound,] # Only desired rows
  if (nrow(subsetted_data) == 0){
    return(c(0,0,0,0)) # No observations in the interval
  }
  cal_res <- lm(formula = significant_t ~ t_stat - 1, data = subsetted_data)
  cal_res_coefs <- coeftest(cal_res, vcov = vcovHC(cal_res, type = "const", cluster = c(input_data$study_id)))
  cal_est <- cal_res_coefs["t_stat", "Estimate"] # Estimate
  cal_se <- cal_res_coefs["t_stat", "Std. Error"] # Standard Error
  cal_above <- nrow(subsetted_data[subsetted_data$t_stat > threshold, ]) # N. obs above the threshold
  cal_below <- nrow(subsetted_data[subsetted_data$t_stat < threshold, ]) # N. obs below the threshold
  # Return the output
  res <- c(
    round(cal_est, 3),
    round(cal_se, 3),
    cal_above,
    cal_below)
  invisible(res)
}

#' Run Caliper tests across all thresholds and widths and store the output in a data frame
#' 
#' @param input_data [data.frame] A data frame containing the input data.
#' @param thresholds [vector] A numeric vector containing the thresholds at which the caliper tests are to be run.
#'                   Defaults to c(0, 1.96, 2.58).
#' @param widths [vector] A numeric vector containing the caliper widths at which the tests are to be run. 
#'               Defaults to c(0.05, 0.1, 0.2).
#' @param verbose [bool] A logical value indicating whether the results should be printed to the console. 
#'                Defaults to TRUE.
#' 
#' @return A data frame with dimensions nrow = length(widths) * 3 and ncol = length(thresholds),
#'         where the rows are named with the caliper width and its estimate, standard error, and n1/n2 ratio,
#'         and the columns are named with the corresponding thresholds.
getCaliperResults <- function(input_data, thresholds = c(0, 1.96, 2.58), widths = c(0.05, 0.1, 0.2), verbose = T){
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.vector(thresholds),
    is.vector(widths),
    is.numeric(thresholds),
    is.numeric(widths)
  )
  # Initialize the output data frame
  num_thresholds <- length(thresholds)
  num_widths <- length(widths)
  result_df <- data.frame(matrix(ncol = num_thresholds, nrow = num_widths * 3))
  colnames(result_df) <- paste0("Threshold ", thresholds)
  rownames_vec <- c()
  for (width in widths){
    rows <- c(
      paste0("Caliper width ", width, " - Estimate"),
      paste0("Caliper width ", width, " - SE"),
      paste0("Caliper width ", width, " - n1/n2")
    )
    rownames_vec <- append(rownames_vec, rows)
  }
  rownames(result_df) <- rownames_vec
  # Run caliper tests for all thresholds and widths
  for (i in 1:num_thresholds){
    for (j in 1:num_widths){
      caliper_res <- runCaliperTest(input_data, threshold = thresholds[i], width = widths[j])
      result_df[j*3-2, i] <- round(caliper_res[1], 5) # Estimate
      result_df[j*3-1, i] <- paste0("(", round(caliper_res[2], 5), ")") # Standard Error
      result_df[j*3, i] <- paste0(caliper_res[3], "/", caliper_res[4]) # n1/n2
    }
  }
  # Verbose output
  if (verbose){
    getCaliperResultsVerbose(result_df, verbose = verbose)
  }
  # Return the data frame
  return(result_df)
}

#' Verbose output for the getCaliperResults function
getCaliperResultsVerbose <- function(res, ...){
  args <- list(...)
  verbose_on <- args$verbose
  # Verbose output
  if (verbose_on){
    print("Results of the Caliper tests:")
    print(res)
    cat("\n\n")
  }
}

###### PUBLICATION BIAS - p-hacking test (Elliott et al., 2022) ######

#' getElliottResults - Calculate Elliott's five tests and other statistics for a given dataset
#'  - Source: https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA18583
#'
#' @param input_data A data frame containing at least the "t_stat" column.
#' @param script_path Full path to the source script.
#' @param temp_data_path Store temporary output here.
#' @param data_subsets A character vector with the names of the subsets of data to test. By default, only "All data" is tested.
#' @param p_min The minimum p-value threshold for the tests. Default is 0.
#' @param p_max The maximum p-value threshold for the tests. Default is 1.
#' @param d_point The discontinuity cutoff point for the discontinuity test. Default is 0.15.
#' @param CS_bins The number of bins for the Cox-Shi test. Default is 10.
#' @param verbose A logical indicating whether to print the results to console. Default is TRUE.
#'
#' @return A data frame with the results of the Elliott tests and other statistics.
getElliottResults <- function(input_data, script_path, temp_data_path, data_subsets = c("All data"), 
      p_min = 0, p_max = 1, d_point = 0.15, CS_bins = 10, verbose = T){
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is.character(script_path),
    is.character(temp_data_path),
    is.vector(data_subsets),
    is.numeric(p_min),
    is.numeric(p_max),
    is.numeric(d_point),
    is.numeric(CS_bins),
    is.logical(verbose),
    all("t_stat" %in% colnames(input_data))
  )
  # Static values, not necessary to adjust (probably)
  id <- 1 # No dependence
  h <- 0.01
  lcm_norm <- 8
  # Create the data frame with the appropriate dimensions and labels
  # Rownames
  threshold1_verbose <- paste0("Observations in [",p_min,", ",p_max,"]")
  threshold2_verbose <- paste0("Observations <= ", d_point)
  data_rownames <- c(
    "Binomial:",
    "s Test:",
    "Discontinuity:",
    "CS1:",
    "CS2B:",
    "LCM:",
    threshold1_verbose,
    threshold2_verbose
  )
  # Colnames
  data_colnames <- data_subsets
  # DF
  elliott_df <- data.frame(matrix(NA, nrow = length(data_rownames), ncol = length(data_colnames)))
  rownames(elliott_df) <- data_rownames
  colnames(elliott_df) <- data_colnames
  # Load the source script
  source(script_path)
  # Load the file with CDFs (if it does not exist, create one)
  validateFolderExistence(temp_data_path) # Validate cache folder existence
  elliott_source_file <- paste0(temp_data_path, "elliott_data_temp.csv")
  # On the first run, create a cached file of CDFs (large in memory)
  if (!file.exists(elliott_source_file)){
    print(paste0("Creating a temporary file in the",temp_data_path,"folder for the Elliott et al. (2022) method..."))
    cdfs <- getCDFs() # Generate the file from scratch (takes time)
    write.table(cdfs, elliott_source_file, col.names = "cdfs", row.names = F)
  }
  cdfs <- read.csv(elliott_source_file, col.names = "cdfs") # Read the cached file
  cdfs <- as.numeric(cdfs[,1]) # To a numeric vector
  # Run the estimation for all data subsets
  for (data_col in data_colnames){
    # Get the data subset
    data <- input_data # Adjust later if desired
    
    # Convert t-statistics to p-values
    t_stat <- data$t_stat
    df <- ifelse("reg_df" %in% colnames(data), data$reg_df, data$n_obs) # Nobs if DoF not available
    P <- 2 * pt(abs(t_stat), df = df, lower.tail=FALSE) # p-values
     
    # Tests (each test returns the corresponding p-value)
    Bin_test <- Binomial(P, p_min, p_max, "c")
    Discontinuity <- Discontinuity_test(P,d_point, h)
    LCM_sup <- LCM(P, p_min,p_max, lcm_norm, cdfs)
    CS_1 <- CoxShi(P,id, p_min, p_max, CS_bins, 1, 0) #Test for 1-non-increasingness
    CS_2B <- CoxShi(P,id, p_min, p_max, CS_bins, 2, 1) #Test for 2-monotonicity and bounds
    FM <- Fisher(P, p_min, p_max)
    
    # Save the results
    elliott_res <- c(Bin_test, Discontinuity, LCM_sup, CS_1, CS_2B, FM)
    elliott_res <- sapply(elliott_res, function(x){round(x,3)})
    
    # Thresholds
    n_obs_between <- length(P[P>=p_min&P<=p_max])
    n_obs_below <- length(P[P<=d_point&P>=0])
  
    # Fill in the data frame with values from elliott_res
    elliott_df["Binomial:", data_col] <- elliott_res[1]
    elliott_df["Discontinuity:", data_col] <- elliott_res[2]
    elliott_df["LCM:", data_col] <- elliott_res[3]
    elliott_df["CS1:", data_col] <- elliott_res[4]
    elliott_df["CS2B:", data_col] <- elliott_res[5]
    elliott_df["s Test:", data_col] <- elliott_res[6]
    elliott_df[threshold1_verbose, data_col] <- n_obs_between # In between min, max
    elliott_df[threshold2_verbose, data_col] <- n_obs_below # Below disc cutoff
  }
  
  # Verbose output
  if (verbose){
    getElliottResultsVerbose(elliott_df, verbose = verbose)
  }
  # Return the data frame
  return(elliott_df)
}

#' Verbose output for the getElliottResults function
getElliottResultsVerbose <- function(res, ...){
  args <- list(...)
  verbose_on <- args$verbose
  # Print out the output
  if (verbose_on){
    print(paste0("Results of the Elliott tests:"))
    print(res)
    cat("\n\n")
  }
}


###### MAIVE Estimator (Irsova et al., 2023) ######

#' Run the MAIVE estimation using a modified source script
#'  - Source: http://meta-analysis.cz/maive/
#'
#' @param method [int] Method. Options - PET:1, PEESE:2, PET-PEESE:3, EK:4 (default 3)
#' @param script_path [character] Full to the source script.
#' @param weight [int] Weighting. Options - no weight: 0 ; weights: 1, adjusted weights: 2 (default 0)
#' @param instrument [int] Instrumenting. Options - 0;1(default 1)
#' @param studylevel[int] Correlation at study level. Options -  none: 0 (default), fixed effects: 1, cluster: 2
#'  (default 0)
#' @param verbose [bool] Print out the results into the console in a nice format.
#' @inheritDotParams Parameters for the extractExoCoefs function.
#' 
#' @import maive_master_thesis_cala.R
getMaiveResults <- function(input_data, script_path, method = 3, weight = 0, instrument = 1, studylevel = 2, verbose = T, ...){
  # Read the source file
  source(script_path)
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(
    all(required_cols %in% names(input_data)),
    is.character(script_path),
    method %in% c(1,2,3,4),
    weight %in% c(0,1,2),
    instrument %in% c(0,1),
    studylevel %in% c(0,1,2),
    is.logical(verbose)
  )
  # Subset data and rename columns
  input_data <- input_data[,c("effect", "se", "n_obs", "study_id")]
  colnames(input_data) <- c('bs', 'sebs', 'Ns', 'studyid')
  # Run the estimation
  MAIVE <- maive(dat=input_data,method=method,weight=weight,instrument=instrument,studylevel=studylevel)
  # Extract (and print) the output
  object<-c("MAIVE coefficient","MAIVE standard error","F-test of first step in IV",
            "Hausman-type test (use with caution)","Critical Value of Chi2(1)")
  maive_coefs_all<-c(MAIVE$beta,MAIVE$SE,MAIVE$`F-test`,MAIVE$Hausman,MAIVE$Chi2)
  MAIVEresults<-data.frame(object,maive_coefs_all)
  colnames(MAIVEresults) <- c("Object", "Coefficient")
  # Verbose output
  if (verbose){
    getMaiveResultsVerbose(MAIVEresults, verbose = verbose)
  }
  # Return the data frame
  return(MAIVEresults)
}

#' Verbose output for the getMaiveResults function
getMaiveResultsVerbose <- function(res, ...){
  args <- list(...)
  verbose_on <- args$verbose
  # Print out the output
  if (verbose_on){
    print(paste0("Results of the MAIVE estimator:"))
    print(res)
    cat("\n\n")
  }
}

######################### MODEL AVERAGING #########################

###### HETEROGENEITY - Bayesian Model Averaging in R ######
    
#' This function searches for an optimal Bayesian Model Averaging (BMA) formula by removing the variables
#' with the highest Variance Inflation Factor (VIF) until the VIF coefficients of the remaining variables
#' are below 10 or the maximum number of groups to remove is reached.
#'
#' @param input_data A data frame containing the input data.
#' @param input_var_list A data frame containing the variable names, a boolean indicating whether the variable
#' is a potential variable for the model, and a grouping category for each variable.
#' @param max_groups_to_remove An integer indicating the maximum number of variable groups to remove.
#' @param return_variable_vector_instead A logical value indicating whether the function should return
#' a vector of remaining variables instead of the BMA formula.
#' @param verbose A logical value indicating whether the function should print the progress and the suggested BMA formula.
#'
#' @return If return_variable_vector_instead is TRUE, the function returns a character vector of the remaining variables.
#' Otherwise, it returns a formula object of the suggested BMA formula. These are returned as a list along with
#' three other performance indicators (used in verbose output and cacheing).
findOptimalBMAFormula <- function(input_data, input_var_list, max_groups_to_remove = 30,
                                    return_variable_vector_instead = F, verbose = T) {
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.numeric(max_groups_to_remove),
    is.logical(return_variable_vector_instead),
    is.logical(verbose),
    all(c("bma", "var_name", "group_category") %in% colnames(input_var_list))
  )
  # Subset input data to only columns defined in variable list
  input_data <- input_data[,colnames(input_data) %in% input_var_list$var_name]
  # Remove any variables for which all values in data are the same
  non_const_cols <- apply(input_data, 2, function(col){length(unique(col)) > 1})
  input_data <- input_data[,non_const_cols]
  # Extract the information from source data
  bma_potential_vars_bool <- input_var_list$bma & non_const_cols # BMA OK and non constant
  potential_vars <- input_var_list$var_name[bma_potential_vars_bool]
  var_grouping <- input_var_list$group_category[bma_potential_vars_bool]
  
  # Pop the effect from var grouping and potential vars (not used in the iteration)
  var_grouping <- var_grouping[!potential_vars == "effect"]
  potential_vars <- potential_vars[!potential_vars == "effect"]
  
  # Get initial BMA formula and VIF coefficients
  bma_formula <- getBMAFormula(potential_vars, input_data)
  bma_lm <- lm(bma_formula, data = input_data)
  vif_coefs <- car::vif(bma_lm)
  if (length(var_grouping) != length(vif_coefs)){ # 1 less variable in VIF coefs
   stop("The lengths of the variable vectors do not match")
  }

  removed_groups <- 0
  removed_groups_verbose <- c()
  while (any(vif_coefs > 10) && max_groups_to_remove > 0) {
    # Get the group with the highest VIF coefficient
    highest_vif_coef_name <- names(which.max(vif_coefs)) # Name of the coefficient with highest VIF
    highest_vif_coef_idx <- which(potential_vars == highest_vif_coef_name) # Index of the highest VIF coef
    highest_vif_group <- var_grouping[highest_vif_coef_idx] # Index of group to remove
    # Get new potential vars, new grouping
    vars_to_remove <- potential_vars[var_grouping == highest_vif_group]
    potential_vars <- potential_vars[!potential_vars %in% vars_to_remove]
    var_grouping <- var_grouping[!var_grouping %in% highest_vif_group]
    # Get modified BMA formula and VIF coefficients
    bma_formula <- getBMAFormula(potential_vars, input_data)
    bma_lm <- lm(bma_formula, data = input_data)
    vif_coefs <- car::vif(bma_lm)
    if (length(var_grouping) != length(vif_coefs)){
      stop("The lengths of the variable vectors do not match")
    }
    # Decrease the maximum number of groups to remove
    max_groups_to_remove <- max_groups_to_remove - 1
    removed_groups <- removed_groups + 1
    removed_groups_verbose <- append(removed_groups_verbose, vars_to_remove)
  }
  # Print out the information about the procedure outcome
  if (max_groups_to_remove == 0) {
    stop("Maximum number of groups to remove reached. Optimal BMA formula not found.")
  }
  # Get main object to return - explicit because ifelse() does not work for some RRRRRReason
  if(return_variable_vector_instead){
    res_object <- potencial_vars
  } else {
    res_object <- bma_formula
  }
  # All information to return (for cacheing)
  out_list <- list(res_object, removed_groups, removed_groups_verbose, bma_formula)
  # Verbose output
  if (verbose) {
    findOptimalBMAFormulaVerbose(
      out_list, # Object plus four verbose indicators
      verbose = verbose
    )
  }
  # Return the outcome
  return(out_list)
}

#' Verbose output for the findOptimalBMAFormula function
findOptimalBMAFormulaVerbose <- function(out_list, ...){
  args <- list(...)
  verbose_on <- args$verbose
  # Validate input
  stopifnot(
    is(out_list, "list"),
    length(out_list) == 4 # Via the main function
  )
  # Extract function output
  removed_groups <- out_list[[2]]
  removed_groups_verbose <- out_list[[3]]
  bma_formula <- out_list[[4]]
  if (verbose_on){
    print(paste("Removed", removed_groups, "groups with VIF > 10."))
    print("The removed groups contained these variables:")
    print(removed_groups_verbose)
    print("The suggested BMA formula is:")
    print(bma_formula)
  }
}

#' Creates a formula for Bayesian model averaging
#'
#' This function creates a formula for Bayesian model averaging based on the variables in \code{var_vector}.
#' The formula includes the variables "effect" and "se", as well as any other variables specified in \code{var_vector}.
#'
#' @param input_var [vector] A vector of variables that should be used to construct the formula. Must include
#' "effect" and "se".
#' @param input_data [data.frame] A data frame on which the formula will later be used. Skip adding any variables
#'  where all values of this data frame are 0 for the variable.
#' @param get_var_vector_instead [bool] If TRUE, return a vector with variable names instead, with effect and se
#'  at the first two positions of the vector. Used for a simple rearrangement. Defaults to FALSE.
#' @return A formula object (to be used) Bayesian model averaging
#' 
#' @note To get the vector itself from the formula, you can use the in-built "all.vars()" method instead.
getBMAFormula <- function(input_var, input_data, get_var_vector_instead = F){
  # Separate the effect and SE from the remaining variables
  bool_wo_effect <- input_var != "effect" # Pop effect
  bool_wo_se <- input_var != "se" # Pop se
  remaining_vars <- input_var[bool_wo_effect & bool_wo_se] # Remaining variables
  # Remove any variables for which all values in data are the same
  zero_vars <- input_data %>% select_if(~ length(unique(.)) == 1) %>% names
  remaining_vars <- remaining_vars[!remaining_vars %in% zero_vars]
  # Get the formula
  if (get_var_vector_instead){
    var_vector <- c("effect", "se", remaining_vars)
    return(var_vector)
  }
  remaining_vars_verbose <- paste(remaining_vars, sep="", collapse = " + ")
  all_vars_verbose <- paste0("effect ~ se + ", remaining_vars_verbose)
  bma_formula <- as.formula(all_vars_verbose)
  return(bma_formula)
}

#' Function to test the Variance Inflation Factor of a Linear Regression Model
#'
#' @details runVifTest is a function that tests the Variance Inflation Factor (VIF) of a linear regression model.
#' It takes three arguments: input_, and print_all_coefs. The function tests whether the input_ is either a vector
#' or a formula. If it is a vector of variables, it transforms it into a formula. Then, it calculates the VIF coefficients
#' using the vif function from the car package. If print_all_coefs is set to TRUE, the function prints all the VIF
#' coefficients. If any of the VIF coefficients is larger than 10, the function prints a message indicating the
#' variables with a high VIF. Otherwise, it prints a message indicating that all variables have a VIF lower than 10.
#' Finally, the function returns the VIF coefficients as a numeric vector.
#' 
#' @note If you input the formula, all data for these variables must be a vector with at least some variation.
#' Otherwise the function will return an error.
#' 
#' @param input_var [vector | formula] One of - vector of variable names, formula. If it is a vector, the function
#' transforms the input into a formula.
#' @param input_data [data.frame] Data to run the test on.
#' @param print_all_coefs [bool] A logical value indicating whether to print all the VIF coefficients into
#'  the console
#' @param verbose [bool] If TRUE, print out the information about the output. Defaults to TRUE.
#'
#' @return [vector] A numeric vector with the VIF coefficients.
runVifTest <- function(input_var, input_data, print_all_coefs = F, verbose = T){
  # Validate input
  stopifnot(
    any(
      is_formula(input_var),
      is.vector(input_var)
    ),
    is.data.frame(input_data)
  )
  # Get the BMA formula - explicit because RRRRRR
  if (is.vector(input_var)){
    BMA_formula <- getBMAFormula(input_var) # Automatically validates that all vectors are non-0
  } else{
    if (nrow(input_data %>% select_if(~ length(unique(.)) > 1)) < nrow(input_data)){
      stop("All data must have at least some variation.")
    }
    BMA_formula <- input_var # Formula is valid
  }
  # Run the test
  BMA_reg_test <- lm(formula = BMA_formula, data = input_data)
  # Unhandled exception - fails in case of too few observations vs. too many variables
  vif_coefs <- car::vif(BMA_reg_test) #VIF coefficients
  if (verbose){
    if (print_all_coefs){
      print("These are all the Variance Inflation Coefficients for this formula:")
      print(vif_coefs)
    }
    if (any(vif_coefs > 10)){
      coefs_above_10_vif <- names(vif_coefs)[vif_coefs > 10]
      print("These variables have a Variance Inflation Coefficient larger than 10:")
      print(coefs_above_10_vif)
    } else {
      print("All BMA variables have a Variance Inflation Factor lower than 10. All good to go.")
    }
  }
  return(vif_coefs)
  }


#' Get the data for Bayesian Model Averaging
#' 
#' @details An explicit function to subset the main data frame onto only those columns that are used
#' during the BMA estimation. The function is explicit for the simple reason that one of the 
#' plots in the extractBMAResults requires the data object, so this function allows for that
#' object to exist outside the scope of the runBMA function, where it would be otherwise hidden.
#'
#' @param input_data [data.frame] A data from containing the BMA data (and more)
#' @param input_var_list [data.frame] A data frame containing the variable information.
#' @param variable_info [data.frame | vector] Either a data frame containing the variable information,
#'  or a vector of variables. In the latter case, the "from_vector" variable must be set to T.
#' @param from_vector [logical] If True, the "variable_info" must be specified as a vector, otherwise
#'  as a data frame. Defaults to FALSE.
#' @param include_reference_groups [logical] If TRUE, add the reference groups to the data. Be very
#' careful, as this may create a dummy trap. Used when creating the descriptive table of all potential
#' BMA variables. Usable only when from_vector == FALSE. Defaults to FALSE.
#' @note When transforming/subsetting the data, there is a need to convert the data into a
#' data.frame object, otherwise the plot functions will not recognize the data types correctly
#' later on. The "bms" function works well even with a tibble, but the plots do not. RRRRRRR
getBMAData <- function(input_data, input_var_list, variable_info, from_vector = T,
                       include_reference_groups = F){
  # Input validation
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    any(
      is.data.frame(variable_info),
      is.vector(variable_info)
    ),
    is.logical(from_vector)
  )
  # Subset the data
  if (from_vector & !is.vector(variable_info)){
    stop("You must provide a vector if you wish to extract the variable information form a vector.")
  }
  if (!from_vector & !is.data.frame(variable_info)){
    stop("You must provide a data frame if you wish to extract the variable information form a data frame.")
  }
  if (is.data.frame(variable_info)){ # Input data frame
    desired_vars_bool <- variable_info$bma
    if (include_reference_groups){
      ref_bool <- variable_info$bma_reference_var
      desired_vars_bool <- desired_vars_bool | ref_bool # Add reference variables
    }
    desired_vars <- variable_info$var_name[desired_vars_bool]
  } else { # Input vector
   desired_vars <- variable_info
  }
  bma_data <- input_data[desired_vars] # Only desired variables
  bma_data <- as.data.frame(bma_data) # To a data.frame object, because RRRR
  
  # Convert all specified columns to logs - sub-optimal approach
  for (column in colnames(bma_data)){
    row_idx <- match(column, input_var_list$var_name)
    to_log <- as.logical(input_var_list[row_idx, "to_log_for_bma"])
    if (to_log){
      bma_data[,column] <- log(bma_data[,column])
      bma_data[is.infinite(bma_data[,column]),column] <- 0 # Replace infinite values with 0
    }
  }
  return(bma_data)
}

#' Run a Bayesian model averaging estimation
#' 
#' @details Input the BMA data, the variable information data frame
#' and inhereted parameters, which are all the parameters you want to use for the actual
#' estimation inside the 'bms' function. Validate correct input, run the estimation, and
#' return the BMA model without printing any results.
#' 
#' @param bma_data [data.frame] The data for BMA. "effect" must be in the first column.
#' @param bma_params [list] Parameters to be used inside the "bms" function. These are:
#' burn, iter, g, mprior, nmodel, mcmc
#' For more info see the "bms" function documentation.
#' @return The bma model
runBMA <- function(bma_data, bma_params){
  # Input validation
  stopifnot(
    is.data.frame(bma_data),
    !any(is.na(bma_data)), # No missing obs
    all(sapply(bma_data,is.numeric)), # Only numeric obs
    colnames(bma_data[,1]) == "effect"
  )
  # Get parameters
  all_bma_params <- c(
    list(
      bma_data
    ),
    bma_params
  )
  # Actual estimation with inhereted parameters
  runBMAVerbose()
  quiet(
    bma_model <- do.call(bms, all_bma_params)
  )
  return(bma_model)
}

#' Verbose output for the runBMA function
runBMAVerbose <- function(...){
  print("Running the Bayesian Model Averaging...")
}


#' Extract results from a Bayesian Model Averaging (BMA) regression
#' 
#' @details extractBMAResults is a function that extracts results from a Bayesian Model Averaging (BMA) regression model.
#' The function takes three arguments: bma_model, bma_data, and print_results. bma_model is an object of class bma
#' containing the BMA regression model. bma_data is a data frame containing the data used to fit the BMA model. print_results
#' is a character value indicating the level of result printing desired. The possible values for print_results are "none", "fast",
#' "verbose", and "all".
#'
#' The function first validates the input to ensure that the class of bma_model is "bma", bma_data is a data frame, and
#' print_results is a character value that matches one of the four valid options.
#'
#' The function then extracts the coefficients from bma_model using the coef function. The output is a numeric vector containing
#' the BMA coefficients.
#'
#' The function then prints out coefficient and model statistics based on the value of print_results. If print_results is set to
#' "verbose" or "all", the function prints the coefficients and summary information of bma_model, as well as the top model. If
#' print_results is set to "fast", the function prints only the BMA coefficients. If print_results is set to "all", the function
#' also prints the main BMA plots, which may take some time to generate.
#'
#' Finally, the function plots the correlation matrix of bma_data using corrplot.mixed, and returns the BMA coefficients as a
#' numeric vector.
#' 
#' @param bma_model [bma] An object of class bma containing the BMA regression model.
#' @param bma_data [data.frame] A data frame containing the data used to fit the BMA model.
#' @param print_results [character] A character value indicating the level of result printing desired.
#'  Can be one of:
#'  * none - print nothing
#'  * fast - print only those results that do not take time to print
#'  * verbose - print all the fast results, plus extra information about the model
#'  * all - print all results, plots included (takes a long time)
#' @param export_graphics [logical] If TRUE, export the graphs into the graphics folder. Defaults to TRUE.
#' @param export_path [character] Path to the export folder. Defaults to ./graphics.
#' @param graph_sclae [numeric] Scale the corrplot graph by this number. Defaults to 1.
#'
#' @return A numeric vector containing only the BMA coefficients.
extractBMAResults <- function(bma_model, bma_data, print_results = "fast",
                              export_graphics = T, export_path = "./graphics", graph_scale = 1){
  # Validate the input
  stopifnot(
    class(bma_model) == "bma",
    is.data.frame(bma_data),
    is.character(print_results),
    print_results %in% c("none", "fast", "verbose", "all", "table"),
    is.logical(export_graphics),
    is.character(export_path),
    is.numeric(graph_scale)
  )
  # Extract the coefficients
  bma_coefs <- coef(bma_model,order.by.pip= F, exact=T, include.constant=T)
  # Print out coefficient and model statistics
  if (!print_results == "none"){
    print("Results of the Bayesian Model Averaging:")
  }
  if (print_results %in% c("verbose","all")){
    print(bma_model) # Coefficients, summary information
    print(bma_model$topmod[1]) # Topmod
  } else if (print_results == "fast"){
    print(bma_coefs)
  }
  # Create plots for printing/export
  if (any(print_results == "all", export_graphics == TRUE)){
    # Main plot
    main_plot_call <- quote(
      image(bma_model, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE,
          do.axis=TRUE, xlab = "", main = "") # Takes time
    )
    # Model distribution
    bma_dist_call <- quote(
      plot(bma_model)
    )
    # Corrplot
    bma_col<- colorRampPalette(c("red", "white", "blue"))
    bma_matrix <- cor(bma_data)
    corrplot_mixed_call <- quote(
      corrplot.mixed(bma_matrix, lower = "number", upper = "circle",
                     lower.col=bma_col(200), upper.col=bma_col(200),tl.pos = c("lt"),
                     diag = c("u"), tl.col="black", tl.srt=70, tl.cex=0.55,
                     number.cex = 0.5,cl.cex=0.8, cl.ratio=0.1) 
    )
  }
  # Print out plots (takes time)
  if (print_results == "all"){
    print("Printing out Bayesian Model Averaging plots. This may take some time...")
    eval(main_plot_call)
    eval(bma_dist_call)
    eval(corrplot_mixed_call)
  }
  # Return coefficients only
  if (!print_results == "none"){
    cat("\n\n")
  }
  if (export_graphics){
    # Paths
    validateFolderExistence(export_path)
    main_path <- paste0(export_path, "/bma_main.png")
    dist_path <- paste0(export_path, "/bma_dist.png")
    corrplot_path <- paste0(export_path, "/bma_corrplot.png")
    # Remove existing plots if they exist
    for (path in list(main_path, dist_path, corrplot_path)){
      hardRemoveFile(path)
    }
    # Main plot
    png(main_path, width=1400, height=1341, res=150)
    eval(main_plot_call)
    dev.off()
    # Model distribution
    png(dist_path, width = 528, height = 506)
    eval(bma_dist_call)
    dev.off()
    # Corrplot
    png(corrplot_path,
        width = 700*graph_scale, height = 669*graph_scale, units = "px",
        res = 90*graph_scale) # pointsize for text size
    eval(corrplot_mixed_call)
    dev.off()
  }
  return(bma_coefs)
}

#' Verbose output for the extractBMAResults function
extractBMAResultsVerbose <- function(...){
  # Todo
}

#' A helper function (not used in the main analysis) that allows the user to generate
#' a boolean vector for the excel variable info sheet - namely the "bma" column.
#' 
#' Serves as a way to extract the list of variables that the optimal BMA formula chooses.
#' 
#' @param input_var_list [data.frame] The data frame with variable information.
#' @param bma_formula [formula] Formula of the BMA model.
#' @param verbose [bool] If TRUE, print out the resulting boolean vector into the console.
#' Defaults to TRUE.
#' @return Boolean vector.
getBMAExcelBool <- function(input_var_list, bma_formula, verbose = T){
  # Input validation
  stopifnot(
    is.data.frame(input_var_list),
    is_formula(bma_formula),
    is.logical(verbose)
  )
  # Get the excel "bma" boolean
  bma_vars <- all.vars(bma_formula)
  bma_bool <- input_var_list$var_name %in% bma_vars
  if (verbose){
    print(bma_bool)
  }
  invisible(bma_bool)
}

###### HETEROGENEITY - Frequentist model averaging code for R (Hansen) ######

#' Copy of the lowRankQP function from the LowRankQP package which is no longer available
#' Source: https://cran.r-project.org/package=LowRankQP
lowRankQPCopy <- function(Vmat,dvec,Amat,bvec,uvec,method="PFCF",verbose=FALSE,niter=200) {
   # Some Type Checking
   typeError <- FALSE
   if ( nrow(Vmat)!=length(dvec) )
   {
        print("ERROR: nrow(Vmat)!=length(dvec)")
        typeError <- TRUE
   }
   if ( nrow(Vmat)!=ncol(Amat) )
   {
        print("ERROR: nrow(Vmat)!=ncol(Amat)")
        typeError <- TRUE
   }
   if ( nrow(Vmat)!=length(uvec) )
   {
        print("ERROR: nrow(Vmat)!=length(uvec)")
        typeError <- TRUE
   }
   if ( nrow(Amat)!=length(bvec) )
   {
        print("ERROR: nrow(Amat)!=length(bvec)")
        typeError <- TRUE
   }
   if (typeError) stop("ERROR: check input dimensions.")

   n <- nrow(Vmat)
   m <- ncol(Vmat)
   p <- nrow(Amat)
   
   alpha <- as.array(matrix( 0.0, n, 1 ))
   beta  <- as.array(matrix( 0.0, p, 1 ))
   xi    <- as.array(matrix( 0.0, n, 1 ))
   zeta  <- as.array(matrix( 0.0, n, 1 ))

   # Create numerical version of method for C call.

   if (method=="LU")   methodNum <- 1
   if (method=="CHOL") methodNum <- 2
   if (method=="SMW")  methodNum <- 3
   if (method=="PFCF") methodNum <- 4

   res <- .C("LowRankQP", n, m, p, as.integer(methodNum), as.integer(verbose),
         as.integer(niter), Vmat, dvec, t(Amat), bvec, uvec, alpha, beta, xi, 
         zeta, PACKAGE="LowRankQP")

   alpha <- res[[12]]
   beta  <- res[[13]]
   xi    <- res[[14]]
   zeta  <- res[[15]]

   list(alpha=alpha, beta=beta, xi=xi, zeta=zeta)
}

#' runFMA - Run Frequentist Model Averaging
#'
#' This function takes a Bayesian model averaging object, a data frame, and a list of variables
#' used in the Bayesian model averaging, and runs the frequentist model averaging.
#' It extracts and prints the final model coefficients and their standard errors.
#' It also returns the results as a data frame. Optionally, it can print the results.
#'
#' @param bma_data [data.frame] A data frame containing the data used to fit the BMA model.
#' @param bma_model [bma] A Bayesian model averaging object obtained from the BMA package.
#' @param input_var_list [data.frame] A data frame containing a list of variables used in the Bayesian model averaging.
#' @param verbose [logical] A logical value. If TRUE, the function prints the results.
#'
#' @return The function returns a data frame with the final model coefficients and their standard errors.
#'
#' @details
#' The function first validates the input arguments. Then it extracts the variables used
#' in the Bayesian model averaging and verifies that they exist in the input data.
#' Afterward, the frequentist model averaging is performed on the ordered and preprocessed data.
#' Finally, the function extracts and prints the results as a data frame.
#'
#' This function uses the lowRankQPCopy function, a copy of the LowRankQP function from the
#' LowRankQP package, which is not available anymore.
runFMA <- function(bma_data, bma_model, verbose = T){
  # Validate input
  stopifnot(
    is.data.frame(bma_data),
    class(bma_model) == "bma",
    names(bma_data[,1]) == "effect" # Restrictive, but extra safe
  )
  # Estimation from here
  print("Running the Frequentist Model Averaging...")
  # Main data - bma_data without the first column (effect)
  x.data <- bma_data[,-1]
  # Reorder the columns according to the bma_model coefficients
  bma_c <- coef(bma_model,order.by.pip= T, exact=T, include.constant=T) #loading the matrix sorted by PIP
  FMA_order <- c(0)
  for (i in 1:nrow(bma_c)-1){
    FMA_order[i] <- bma_c[i,5]
  }
  x.data <- x.data[,c(FMA_order)] # Order the data
  const_<-c(1) # Vector of ones
  x.data <-cbind(const_,x.data) # Plus the data set
  x <- sapply(1:ncol(x.data),function(i){x.data[,i]/max(x.data[,i])})
  scale.vector <- as.matrix(sapply(1:ncol(x.data),function(i){max(x.data[,i])}))
  Y <- as.matrix(bma_data[,1]) # The effect
  # Further groundwork
  output.colnames <- colnames(x.data)
  full.fit <- lm(Y~x-1)
  beta.full <- as.matrix(coef(full.fit))
  M <- k <- ncol(x)
  n <- nrow(x)
  beta <- matrix(0,k,M)
  e <- matrix(0,n,M)
  K_vector <- matrix(c(1:M))
  var.matrix <- matrix(0,k,M)
  bias.sq <- matrix(0,k,M)
  
  # Calculations
  for(i in 1:M)
  {
    X <- as.matrix(x[,1:i])
    ortho <- eigen(t(X)%*%X)
    Q <- ortho$vectors ; lambda <- ortho$values
    x.tilda <- X%*%Q%*%(diag(lambda^-0.5,i,i))
    beta.star <- t(x.tilda)%*%Y
    beta.hat <- Q%*%diag(lambda^-0.5,i,i)%*%beta.star
    beta[1:i,i] <- beta.hat
    e[,i] <- Y-x.tilda%*%as.matrix(beta.star)
    bias.sq[,i] <- (beta[,i]-beta.full)^2
    var.matrix.star <- diag(as.numeric(((t(e[,i])%*%e[,i])/(n-i))),i,i)
    var.matrix.hat <- var.matrix.star%*%(Q%*%diag(lambda^-1,i,i)%*%t(Q))
    var.matrix[1:i,i] <- diag(var.matrix.hat)
    var.matrix[,i] <- var.matrix[,i]+ bias.sq[,i]
  }
  
  e_k <- e[,M]
  sigma_hat <- as.numeric((t(e_k)%*%e_k)/(n-M))
  G <- t(e)%*%e
  a <- ((sigma_hat)^2)*K_vector
  A <- matrix(1,1,M)
  b <- matrix(1,1,1)
  u <- matrix(1,M,1)
  quiet(
    optim <- LowRankQP::LowRankQP(Vmat=G,dvec=a,Amat=A,bvec=b,uvec=u,method="LU",verbose=FALSE)
    
  )
  weights <- as.matrix(optim$alpha)
  beta.scaled <- beta%*%weights
  final.beta <- beta.scaled/scale.vector
  std.scaled <- sqrt(var.matrix)%*%weights
  final.std <- std.scaled/scale.vector
  results.reduced <- as.matrix(cbind(final.beta,final.std))
  rownames(results.reduced) <- output.colnames; colnames(results.reduced) <- c("Coefficient","SE")
  MMA.fls <- round(results.reduced,4)
  MMA.fls <- data.frame(MMA.fls)
  t <- as.data.frame(MMA.fls$Coefficient/MMA.fls$SE)
  t[MMA.fls$Coefficient == 0,] <- 0 #added by the author
  MMA.fls$pv <-round((1-apply(as.data.frame(apply(t,1,abs)), 1, pnorm))*2,3)
  MMA.fls$pv[MMA.fls$pv == 1] <- 0 #added by the author
  MMA.fls$names <- rownames(MMA.fls)
  names <- c(colnames(bma_data))
  names <- c(names,"const_")
  MMA.fls <- MMA.fls[match(names, MMA.fls$names),]
  MMA.fls$names <- NULL
  
  # Extract results
  fma_res <- MMA.fls[-1,]
  if (verbose){
    runFMAVerbose(fma_res, verbose = verbose)
  }
  return(fma_res)
}

#' Verbose output for the runFMA function
runFMAVerbose <- function(res,...){
  args <- list(...)
  verbose_on <- args$verbose
  # Print verbose output
  if (verbose_on){
    print("Results of the Frequentist Model Averaging:")
    print(res)
    cat("\n\n")
  }
}

#' Get model averaging results
getMATable <- function(bma_coefs, fma_coefs, input_var_list){
  # Validate the input
  stopifnot(
    !all(is.na(bma_coefs)), # No missing results allowed
    !all(is.na(fma_coefs)),
    all(rownames(bma_coefs)[-nrow(bma_coefs)] == rownames(fma_coefs)[-nrow(fma_coefs)]) # All names same but intercept
  )
  # Initiate the data frame
  bma_df <- data.frame(bma_coefs) # BMA coefficients to DF
  bma_df <- bma_df[,c("Post.Mean","Post.SD","PIP")]
  fma_df <- data.frame(fma_coefs)
  res_df <- cbind(bma_df, fma_df)
  # Round the results
  res_df <- round(res_df, 3)
  # Change column names
  colnames(res_df) <- c("BMA P.Mean", "BMA SD", "BMA PIP", "FMA Coef", "FMA SE", "FMA p-val")
  # Change row names
  match_positions <- match(rownames(res_df), input_var_list$var_name) # Positions of row names in var info DF
  verbose_rownames <- input_var_list$var_name_verbose[match_positions] # Verbose name of variables
  if (sum(is.na(verbose_rownames)) > 1){
    stop("Unspecified verbose variable names in the model averaging result table.")
  } else if (sum(is.na(verbose_rownames)) == 1){
    # Intercept unspecified
    verbose_rownames[is.na(verbose_rownames)] <- "Intercept"
  }
  rownames(res_df) <- verbose_rownames
  # Move intercept to the top
  res_df <- rbind(res_df[nrow(res_df),], res_df[-nrow(res_df),]) # Last row to first
  # Return the result
  getMATableVerbose(res_df)
  return(res_df)
}

#' Verbose output for the getMATable function
getMATableVerbose <- function(res,...){
  print("Results of Model Averaging:")
  print(res)
  cat("\n\n")
}
######################### BEST-PRACTICE ESTIMATE #########################


#' Fetch the BMA coefficient value from the list of BMA coefficients
#' 
#' @param coef_name [character] Name of the coefficient whose value to fetch.
#' @param bma_coefs All BMA coefficients. A data frame with 5 columns.
#' @param value_type [character] Name of the coefficient to extract. Can be
#'  One of the following - "PIP", "Post Mean", "Post SD", "Cond.Pos.Sign", "Idx"
#' @return [numeric] The value of the coefficient
getBMACoefValue <- function(coef_name, bma_coefs, value_type = "Post Mean"){
  stopifnot(
    value_type %in% c("PIP", "Post Mean", "Post SD", "Cond.Pos.Sign", "Idx")
  )
  idx <- match(coef_name, rownames(bma_coefs))
  val <- bma_coefs[idx,value_type]
  return(val)
}

#' Generate the formula for evaluation of the best practice estimate. Can either be
#' used for obtaining of the BPE estimate, or the BPE standard error.
#'
#' @param input_data [data.frame] Main data frame.
#' @param input_var_list [data.frame] Data frame with variable information.
#' @param bma_model Main model on which to evaluate the BPE on.
#' @param bma_formula Formula used to generate the BMA model
#' @param bma_data [data.frame] Data frame used to generate the BMA model
#' @param study_id [numeric] ID of the study for which to run the BPE for. If equal
#'  to 0, the variable list BPE information is used (author's BPE). Defaults to 0.
#' @param include_intercept [logical] If TRUE, include intercept in the BPE.
#'   Defaults to TRUE.
#' @param get_se [logical] If TRUE, return the formula for SE evaluation instead (for
#'   explanation see the getBPE function). Defaults to FALSE.
#' @return [character] The formula as a string.
constructBPEFormula <- function(input_data, input_var_list, bma_data, bma_coefs,
                                study_id = 0, include_intercept = TRUE, get_se = FALSE) {
  # Check input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.data.frame(bma_data),
    is.numeric(study_id),
    is.logical(include_intercept),
    nrow(input_data)==nrow(bma_data) # Input data used for indexing BMA data
  )
  # Define static variables
  allowed_characters <- c('mean', 'median', 'min', 'max')
  bma_vars <- rownames(bma_coefs)
  # Check if all bma_vars except (Intercept) are present in source data
  stopifnot(
    all(bma_vars[bma_vars != "(Intercept)"] %in% input_var_list$var_name),
    all(bma_vars[bma_vars != "(Intercept)"] %in% colnames(bma_data))
  )
  # Initialize the bpe_est_string with (Intercept), or its value in case of BPE est
  bpe_string_base <- ifelse(get_se, "(Intercept)", getBMACoefValue("(Intercept)", bma_coefs)) # Value for estimate
  bpe_est_string <- ifelse(include_intercept, bpe_string_base, "") # Empty for no intercept
  # Iterate over the bma_vars and add the corresponding coefficients from input_var_list
  for (bma_var in bma_vars) {
    if (!bma_var %in% c("(Intercept)","se")){
      # Use a study
      if (study_id != 0){
        coef <- median(bma_data[input_data$study_id==study_id,bma_var])
      } else {
      # Use author's BPE - variable list information
        coef <- input_var_list$bpe[input_var_list$var_name == bma_var] # Automatically coerced to character - RRRRRR
      }
      # Handle unassigned variables
      if (coef == "stop"){
        stop("Make sure to assign values to all variables that appear in the BMA model.")
      }
      # Handle numeric coefficients
      quiet(
        numeric_var <- !is.na(as.numeric(coef)) # Recognize numeric values based on lack of error - not ideal
      )
      if(numeric_var){
        coef <- as.numeric(coef) # To numeric
        if (coef == 0){ # Do not add to the formula
          next
        }
        coef <- round(coef, 3)
      } else { # char
      # Handle character coefficients
        stopifnot(
          is.character(coef),
          coef %in% allowed_characters
          )
        func <- get(coef) # Get the function to evaluate the value with - mean, median,...
        coef <- func(bma_data[[bma_var]], na.rm=TRUE) # Evaluate on BMA data column of this variable
        coef <- as.character(round(coef, 3)) # Back to character
      }
      # Handle output different than static numbers
      output_var_name <- ifelse(get_se, bma_var, getBMACoefValue(bma_var, bma_coefs)) # Var name for SE, value for EST
      bpe_est_string <- paste0(bpe_est_string, " + ", coef, "*", output_var_name)
    }
  }
  # Append =0 to finish the formula in case of SE
  if (get_se){
    bpe_est_string <- paste(bpe_est_string, "= 0")
  }
  return(bpe_est_string)
}


#' Get the Best practice estimate
#' 
#' Input all the necessary data and BMA outcome information, specify for which study
#' you want to run the estimate for, whether to include the intercept, and whether to
#' get a verbose output, and generate the best practice estimate. In case of no
#' verbose output, a vector of two coefficients is returned - BPE mean, and BPE SE.
#' In case of verbose output, three coefficients are returned - BPE mean, and 95% CI bounds.
#' 
#' @param input_data [data.frame] Main data frame.
#' @param input_var_list [data.frame] Data frame with variable information.
#' @param bma_model Main model on which to evaluate the BPE on.
#' @param bma_formula Formula used to generate the BMA model
#' @param bma_data [data.frame] Data frame used to generate the BMA model
#' @param study_id [numeric] ID of the study to run the BPE on. If set to 0,
#'  run the author's BPE (using the variable information DF). Defaults to 0.
#' @param include_intercept [logical] If TRUE, include intercept in the equation.
#' Defaults to TRUE.
#' @param verbose_output [logical] If TRUE, print out the output information into the console.
#' Defaults to TRUE.
getBPE <- function(input_data, input_var_list, bma_model, bma_formula, bma_data,
                   study_id = 0, include_intercept = TRUE, study_info_verbose = TRUE, verbose_output = TRUE){
  # Check input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.data.frame(bma_data),
    is.numeric(study_id),
    is.logical(include_intercept),
    is.logical(verbose_output)
  )
  # Run information
  if (study_info_verbose){
    if (study_id == 0){
      print("Running the author's best practice estimate...")
    } else {
      study_name <- input_data$study_name[input_data$study_id == study_id][1]
      print(paste("Running the best practice estimate for",study_name))
    }
  }
  # Input preprocessing
  bma_coefs <- coef(bma_model,order.by.pip= F, exact=T, include.constant=T) # Extract the coefficients
  bma_vars <- rownames(bma_coefs) # Variables used in the BMA
  
  # Get the BPE estimate
  # Get formula as a string - ((intercept) + coefs * values)
  bpe_formula_est <- constructBPEFormula(input_data, input_var_list, bma_data, bma_coefs,
                                     study_id, include_intercept, get_se = FALSE)
  bpe_est <- eval(parse(text = bpe_formula_est)) # Evaluate the formula
  
  # Get the BPE Standard error
  # Get formula as a string ((intercept) + coefs * variable_names = 0)
  bpe_formula_se <- constructBPEFormula(input_data, input_var_list, bma_data, bma_coefs,
                                     study_id, include_intercept, get_se = TRUE)
  bpe_ols <- lm(formula = bma_formula, data = bma_data) # Constructing an OLS model
  bpe_glht <- glht(bpe_ols, linfct = c(bpe_formula_se), # GLHT
                   vcov = vcovHC(bpe_ols, type = "HC0", cluster = c(input_data$study_id)))
  bpe_se <- as.numeric(summary(bpe_glht)$test$sigma) # Extract output
  
  # Extract the results and return
  res <- c(bpe_est, bpe_se) # Result vector - c(BPE Estimate, BPE Standard Error)
  res <- round(res, 3) # Round up
  # Return the output
  if (verbose_output){ # Not callable automatically - use BPEResultTable instead
    print(paste("BPE Estimate:", res[1]))
    print(paste("BPE Standard Error:", res[2]))
    cat("\n\n")
  }
  return(res)
}

#' Generate a table with best multiple best practice estimate results
#' 
#' Input the main data, specify for which studies the BPE should be ran, and 
#' run the estimation using these specifications. Return a pretty table where 
#' all results are presented neatly as estimates and their 95% confidence bounds.
#' Alternatively, they can be presented as estimates and their standard errors.
#' 
#' @param study_ids [numeric|vector] A vector with indexes of studies for which the 
#' estimation shall be ran. Can be set to "all", in which case all studies will be evaluated.
#' @param input_data [data.frame] Main data frame.
#' @param input_var_list [data.frame] Data frame with variable information.
#' @param bma_model Main model on which to evaluate the BPE on.
#' @param bma_formula Formula used to generate the BMA model
#' @param bma_data [data.frame] Data frame used to generate the BMA model
#' @param use_ci [logical] If TRUE, use confidence intervals in the output. If FALSE,
#' use standard errors instead. Defaults to TRUE.
#' @param verbose_output [logical] If TRUE, print out the result table into the console.
generateBPEResultTable <- function(study_ids, input_data, input_var_list, bma_model, bma_formula, bma_data,
                                   use_ci = TRUE, study_info_verbose = TRUE, verbose_output = TRUE){
  # Initialize the data frame
  if (use_ci) {
    res_df <- data.frame("estimate" = numeric(0), "ci_95_lower" = numeric(0), "ci_95_higher" = numeric(0))
  } else {
    res_df <- data.frame("estimate" = numeric(0), "standard_error" = numeric(0))
  }
  # Set study ids to all ids if required
  if (all(study_ids == "all")){
    study_ids <- seq(from = 0, to = max(input_data$study_id), by = 1)
  }
  # Loop through study ids
  for (study_id in study_ids) {
    study_name <- ifelse(study_id == 0,
                         "Author",
                         as.character(input_data$study_name[input_data$study_id == study_id][1]))
    # BPE estimation
    bpe_result <- getBPE(input_data, input_var_list, bma_model, bma_formula, bma_data, study_id,
                         include_intercept = TRUE,
                         study_info_verbose = study_info_verbose, # Information about study names
                         verbose_output = FALSE) # Individual study outcomes into console - keep FALSE
    # Extract the results
    est <- bpe_result[1] # BPE Estimate
    se <- bpe_result[2] # BPE Standard error
    # Obtain the data frame values, save them in a temporary data frame
    if (use_ci) {
      ci_lbound <- est - 1.96 * se
      ci_ubound <- est + 1.96 * se
      temp_df <- data.frame("estimate" = round(est, 3),
                            "ci_95_lower" = round(ci_lbound, 3),
                            "ci_95_higher" = round(ci_ubound, 3))
    } else {
      temp_df <- data.frame("estimate" = round(est, 3),
                            "standard_error" = round(se, 3))
    }
    # Join together
    row.names(temp_df) <- study_name
    res_df <- rbind(res_df, temp_df)
  }
  # Return the output
  if (verbose_output) {
    generateBPEResultTableVerbose(res_df, verbose_output = verbose_output)
  }
  return(res_df)
}


#' Verbose output for the generateBPEResultTable function
generateBPEResultTableVerbose <- function(res,...){
  args <- list(...)
  verbose_on <- args$verbose_output
  # Print verbose output
  if (verbose_on){
    print("Best practice estimate results:")
    print(res)
    cat("\n\n")
  }
}

#' getEconomicSignificance
#'
#' This function calculates and returns the economic significance of variables included in a Bayesian Model Averaging (BMA) model.
#' It computes the effects of standard deviation change and maximum change in variable values on the model output.
#'
#' @param bpe_est [numeric] The estimate of the model's dependent variable. It should be a single numeric value.
#' @param input_var_list [data.frame] A data frame containing the list of input variables used in the BMA model.
#' This data frame should have at least one column, 'var_name', containing the names of the variables.
#' If the verbose_output argument is set to TRUE, it should also have a 'var_name_verbose' column containing the verbose
#' (extended) names of the variables.
#' @param bma_data [data.frame] A data frame containing the BMA data, including all variables listed in the input_var_list.
#' @param bma_model [object] The BMA model object from which the coefficients will be extracted. This object should be
#' of a type that can be processed by the coef() function.
#' @param display_large_pip_only [logical] An optional argument specifying whether the function should only consider
#' variables with a posterior inclusion probability (PIP) of at least 0.5. Defaults to FALSE.
#' @param verbose_output [logical] An optional argument specifying whether the function should display verbose output,
#' including the verbose names of variables and percentage values. Defaults to TRUE.
#' @return [data.frame] A data frame where each row represents a variable from the BMA model, and columns contain the following values:
#' - Effect on Sigma (1ΔSD): Effect of a one standard deviation change in the variable value.
#' - % of best (1ΔSD): The percentage of the best possible estimate represented by the effect of a one standard deviation change.
#' - Effect on Sigma (ΔMax): Effect of a change from the minimum to the maximum value of the variable.
#' - % of best(ΔMax): The percentage of the best possible estimate represented by the effect of a change from the minimum to the maximum value.
getEconomicSignificance <- function(bpe_est, input_var_list, bma_data, bma_model,
                                    display_large_pip_only = FALSE, verbose_output = TRUE){
  # Input preprocessing
  stopifnot(
    is.numeric(bpe_est),
    is.data.frame(input_var_list),
    is.data.frame(bma_data),
    is.logical(display_large_pip_only),
    is.logical(verbose_output),
    length(bpe_est) == 1
  )
  bma_coefs <- coef(bma_model,order.by.pip= F, exact=T, include.constant=T) # Extract the coefficients
  bma_vars <- rownames(bma_coefs) # Variables used in the BMA - vector of characters/names
  # Remove "(intercept)" from the list of BMA variables for this function
  intercept_index <- which(bma_vars == "(Intercept)")
  bma_vars <- bma_vars[-intercept_index]
  # Check that there are no undefined variables in the BMA vector now
  stopifnot(all(bma_vars %in% input_var_list$var_name))
  # Initialize the empty data frame
  res_df <- data.frame("effect_sd_change" = numeric(0), "perc_of_best_sd_change" = numeric(0),
                       "effect_max_change" = numeric(0), "perc_of_best_max_change" = numeric(0))
  # Iterate over BMA vars, add the results for each variable seaprately
  for (bma_var in bma_vars) {
    # Get numeric values for the variable
    coef_value <- getBMACoefValue(bma_var, bma_coefs, value_type = "Post Mean")
    coef_pip <- getBMACoefValue(bma_var, bma_coefs, value_type = "PIP")
    bma_data_values <- as.numeric(unlist(bma_data[bma_var])) # A numeric vectors with data for this variable
    max_ <- max(bma_data_values)
    min_ <- min(bma_data_values)
    stdev_ <- sd(bma_data_values)
    # Skip for variables with PIP below 0.5
    if ((display_large_pip_only) & (coef_pip < 0.5)){
      next
    }
    # Calculate the four main measures
    effect_sd_change <- coef_value * stdev_
    perc_sd_change <- effect_sd_change / bpe_est
    effect_max_change <- coef_value * (max_ - min_)
    perc_max_change <- effect_max_change / bpe_est
    # Get percentages in verbose
    perc_sd_change_verbose <- paste0(as.character(round(perc_sd_change * 100, 2)),"%")
    perc_max_change_verbose <- paste0(as.character(round(perc_max_change * 100, 2)),"%")
    # Create a temporary data frame with the results
    temp_df <- data.frame("effect_sd_change" = round(effect_sd_change, 3),
                          "%_of_best_sd_change" = perc_sd_change_verbose,
                          "effect_max_change" = round(effect_max_change, 3),
                          "%_of_best_max_change" = perc_max_change_verbose)
    # Join together
    bma_var_verbose <- input_var_list$var_name_verbose[input_var_list$var_name == bma_var]
    row.names(temp_df) <- bma_var_verbose
    colnames(temp_df) <- c("Effect on Sigma (1*ΔSD)", "% of best (1*ΔSD)",
                           "Effect on Sigma (ΔMax)", "% of best(ΔMax)")
    res_df <- rbind(res_df, temp_df)
  }
  # Return the output
  if (verbose_output) {
    getEconomicSignificanceVerbose(res_df, verbose_output = verbose_output)
  }
  return(res_df)
}

#' Verbose output for the getEconomicSignificance function
getEconomicSignificanceVerbose <- function(res,...){
  args <- list(...)
  verbose_on <- args$verbose_output
  # Print verbose output
  if (verbose_on){
    print(paste0("Economic significance of variables:"))
    print(res)
    cat("\n\n")
  }
}

######################### ROBUST BAYESIAN MODEL AVERAGING #########################

getRoBMA <- function(input_data, verbose, ...){
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is.logical(verbose)
  )
  # Handle arguments
  fixed_params <- list(
      y = input_data$effect,
      se = input_data$se,
      study_names = input_data$study_name,
      priors_effect  = prior(
        distribution = "cauchy",
        parameters = list(location = 0, scale = 1/sqrt(2)),
        truncation = list(0, Inf)),
      priors_heterogeneity = prior(
        distribution = "invgamma",
        parameters = list(shape = 1, scale = 0.15))
  )
  all_params <- c(fixed_params, ...) # Vector of lists for the do.call
  # Estimation
  robma_est <- do.call(
    RoBMA, # Function
    all_params # Parameters
  )
  robma_out <- list(
    summary(robma_est)$components,
    summary(robma_est)$estimates
  )
  names(robma_out) <- c("Components","Estimates")
  # Return the output
  if (verbose) {
    getRoBMAVerbose(robma_out, verbose = verbose)
  }
  return(robma_out)
}

#' Verbose output for the getRoBMA function
getRoBMAVerbose <- function(res,...){
  args <- list(...)
  verbose_on <- args$verbose
  robma_components <- res$Components
  robma_estimates <- res$Estimates
  # Print verbose output
  if (verbose_on){
    print("Robust Bayesian Model Averaging results:")
    cat("\n")
    print(robma_components)
    cat("\n")
    print(robma_estimates)
    cat("\n\n")
  }
}


######################### CACHE HANDLING #########################

#' Cache a function using the memoise package if so desired
#' 
#' Input a function and memoise it based on disk cache if caching is on.
#' If not, return the function as is instead.
#' 
#' @param f [function] The function to be memoised.
#' @param is_cache_on [logical] Indicates whether cache should be used.
#' @param cache_path [character] Path to the folder where cache should be stored.
#'  Defaults to './_cache/'.
#' @param cache_age [numeric] In seconds, how long the cache should exist after creation.
#'  They get deleted with every script run. Defaults to 3600 (1 hour).
#' @return Function. Memoised or not, based on the is_cache_on parameter.
cacheIfNeeded <- function(f, is_cache_on, cache_path = './_cache/', cache_age = 3600) {
  # Validate input
  stopifnot(
    is.function(f),
    is.logical(is_cache_on),
    is.character(cache_path),
    is.numeric(cache_age)
  )
  # Main
  if (is_cache_on) {
    # Get the disk cache
    disk_cache <- cachem::cache_disk(dir = cache_path, max_size = 1e9, max_age = cache_age)
    return(memoise(f, cache = disk_cache))
  } else {
    return(f)
  }
}

#' Run a cached function by using the function call from cacheIfNeeded.
#' 
#' Input the function call from cacheIfNeeded, specify the user parameters list,
#' and input the verbose input (as a function) that should get printed from
#' the cached function call. This is because if a function call results are found
#' in a cache, the function does not get called, so nothing gets logged into the 
#' console.
#' 
#' @param f [function] Cached (or bare) function that should be called.
#' @param user_params [list] A list with user parameters.
#' @param verbose_function [function] A function with the verbose output of the 
#'  function f.
#' @inheritDotParams The parameters with which the function should be called.
#' @return The returned object from the function call.
runCachedFunction <- function(f, user_params, verbose_function, ...){
  # Validate input
  stopifnot(
    is.function(f),
    is.list(user_params),
    is.function(verbose_function),
    !all(c("is_cache_on", "cache_path") %in% names(user_params))
  )
  # Define the function to call based on cache information
  f <- cacheIfNeeded(f, user_params$use_cache, user_params$folder_paths$cache_folder, user_params$cache_age)
  # Capture verbose output to print in case it gets silenced
  verbose_output <- captureOutput(
    # Call the function with parameters
    res <- f(...)
  )
  # If the function runs cached, call verbose output explicitly
  if (length(verbose_output) == 0){ # Always "character" class
    verbose_function(res, ...) # Call with original function parameters
  } else {
    cat(verbose_output, sep="\n") # Print actual output
  }
  # Return the result
  return(res) 
}

#' A null function for no verbose output
nullVerboseFunction <- function(res,... ){NULL}

######################### EXPORT AND CACHES #########################

#' writeIfNotIdentical
#' 
#' This function compares the contents of an R object in the environment with an existing file.
#' If the contents are different, or if the file does not exist, it overwrites or creates
#' the file with the content of the object. If 'force_overwrite' is TRUE, the function will
#' overwrite the file regardless of the comparison result.
#'
#' @param object_name [data.frame] The name of the R object in the environment to be
#' compared with the file content and possibly written to the file.
#' @param file_name [character] The name (and path) of the file to be compared with the
#' R object and possibly overwritten.
#' @param use_rownames [logical] If TRUE, write rownames.
#' @param force_overwrite [logical] A flag that forces the function to overwrite the file
#' regardless of the comparison result. Default is FALSE.
#' @return [logical] Returns TRUE if the contents of the object and the file were identical
#' and no write operation was needed. Returns FALSE if the file was created or overwritten.
#' 
writeIfNotIdentical <- function(object_name, file_name, use_rownames, force_overwrite = FALSE){
  # A temp function for code efficiency
  overwrite <- function(x = object_name, file = file_name, row.names = use_rownames){
    hardRemoveFile(file) # Remove if exists
    write.csv(x, file, row.names = row.names, fileEncoding = "UTF-8")
  }
  # Force overwrite
  if (force_overwrite){
      overwrite()
      return(FALSE)
  }
  # Check if file exists
  if (!file.exists(file_name)) {
    overwrite()
    return(FALSE)
  }
  # Read the existing CSV file
  content <- read.csv(file_name, stringsAsFactors = FALSE)
  # Handle the rownames column
  if ("X" %in% colnames(content)){
    content <- content[, -(colnames(content) == "X")] # Discard row names
    content <- as.data.frame(content) # Avoid type change
  }
  # Check for mismatching shapes
  if (nrow(content) != nrow(object_name) | ncol(content)!=ncol(object_name)){
    overwrite()
    return(FALSE)
  }
  # Replace NAs with 0 to allow for direct object comparison
  content[is.na(content)] <- 0
  object_name[is.na(object_name)] <- 0
  # Check for identical contents
  if (!all(content == object_name)) {
    overwrite()
    return(FALSE)
  }
  return(TRUE)
}


#' exportTable
#' 
#' This function exports a given results table as a CSV file into a specified directory.
#' The directory is determined based on the 'export_path' item in the 'user_params' list,
#' and the filename is constructed from the 'method_name' argument. If the directory does not exist,
#' the function will create it. The function also prints a message to inform the user about the location of the exported file.
#' The file will not export if the same file already exists under the specified path.
#' 
#' @param results_table [data.frame] The data frame to be exported as a CSV file.
#' @param user_params [list] A list of user parameters. It should contain an 'numeric_results' item,
#' which specifies the directory to export the file, and an 'export_methods' item, which is a
#' named list where the names correspond to valid 'method_name' values and the values are used for verbose output.
#' @param method_name [character] A character string that specifies the export method. 
#' It should be present in the names of 'user_params$export_methods' and is used to construct the filename
#' of the exported CSV file.
#' 
#' @return No explicit return value. The function writes a CSV file to the file system.
#' 
#' @examples
#' \dontrun{
#'   exportTable(results_table, user_params, method_name)
#' }
#' @export
exportTable <- function(results_table, user_params, method_name){
  # Validate input
  stopifnot(
    is.data.frame(results_table),
    is.list(user_params),
    is.character(method_name),
    method_name %in% names(user_params$export_methods) # Only recognized exports
  )
  # Define the export paths
  numeric_results_folder <- user_params$folder_paths$numeric_results_folder # Export folder
  validateFolderExistence(numeric_results_folder) # Create the export folder if not present in the working directory
  results_path <- paste0(numeric_results_folder, method_name, ".csv") # numeric_results_folder/export_file.csv
  
  # Check whether the row names are sequential numbers (1,2,3,...) - do not use rows if they are
  row_names <- rownames(results_table)
  n <- nrow(results_table)
  use_rownames <- !all(1:n == row_names) # Rows are sequential integers
  # Export the table if it does not exist
  verbose_info <- user_params$export_methods[[method_name]] # Verbose name for the message
  identical_file_exists <- writeIfNotIdentical(results_table, results_path, use_rownames)
  if (!identical_file_exists){
    print(paste("Writing the", tolower(verbose_info), "results into", results_path))
    cat("\n")
  }
}

#' Remove a file from the system
hardRemoveFile <- function(file_path){
  if (file.exists(file_path)){
    quiet(system(paste("rm",file_path)))
  }
}

#' Zip multiple folders into a single zip file.
#'
#' @param zip_name [character] The name of the zip file to be created.
#' @param dest_folder [character] The absolute path of the destination folder where the zip file will be created.
#' @param ... [character] Variable length argument, absolute paths of folders to be zipped.
#' 
#' @return A success message indicating the location of the created zip file.
#'
#' @examples
#' \dontrun{
#' zip_folders(zip_name = "my_folders", dest_folder = "/path/to/dest", "/path/to/folder1", "/path/to/folder2")
#' }
#' 
#' @export
zipFolders <- function(zip_name, dest_folder, ...){
  # Get arguments and paths
  folder_names <- as.vector(unlist(list(...))) # Folders to be zipped, character vector
  zip_file_path <- file.path(paste0(dest_folder, zip_name, ".zip"))
  if(!dir.exists(dest_folder)){
    dir.create(dest_folder, recursive = TRUE)
  }
  # Handle the .zip file creation
  hardRemoveFile(zip_file_path) # Remove if exists
  print("Writing the results into a zip file...")
  tryCatch({
    utils::zip(zip_file_path, files=folder_names, extras = "-r") # Create the zip file
  }, error = function(e){
    print("An error occured when creating the zip file:")
    print(e$message)
    return()
  }
  )
  # Return a message
  zipFoldersVerbose(zip_file_path)
  return()
}

#' Verbose function for the zipFolders function
zipFoldersVerbose <- function(zip_file_path){
  print(paste0("Successfully zipped results into: ", zip_file_path))
}

######################### GRAPHICS #########################

#' Specify the type of theme to use and return the theme
#' 
#' Available choices - main, yellow, green, red
getTheme <- function(theme_type, x_axis_tick_text = "black"){
  # Validate the theme type
  available_themes <- c("blue", "yellow", "green", "red")
  if (!theme_type %in% available_themes){ # Loaded from source
    message(paste(theme_type, "is not a valid theme."))
    message("You must choose one of the following themes:")
    message(available_themes)
    stop("Invalid theme")
  }
  # Get specific colors
  theme_color <- switch(theme_type,
    blue = "#DCEEF3",
    yellow = "#FFFFD1",
    green = "#D1FFD1",
    red = "#FFD1D1",
    stop("Invalid theme type.")
  )
  # Construct and return the theme
  theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
        axis.text.x = ggtext::element_markdown(color = x_axis_tick_text),
        axis.text.y = ggtext::element_markdown(color = "black"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = theme_color),
        plot.background = element_rect(fill = theme_color))
}

#' Export the graph into an HTML file using the plotly package
#' 
#' @param graph_object The object generated by ggplot
#' @param export_path Full path to where the object should be stored.
exportHtmlGraph <- function(graph_object, export_path){
  plotly_img <- ggplotly(graph_object)
  htmlwidgets::saveWidget(plotly_img, export_path)
}