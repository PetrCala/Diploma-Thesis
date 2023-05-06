##################### ENVIRONMENT PREPARATION ########################

#' Function to read multiple sheets from an Excel file and write them as CSV files
#' USE ONLY IN DEVELOPMENT
#' @param xlsx_path Path to the Excel file
#' @param sheet_names A vector of sheet names to read
#' @return A list of data frames
readExcelAndWriteCsv <- function(xlsx_path, sheet_names) {
  # Read each sheet and write it as a CSV file in the working directory
  quiet(
    dfs <- lapply(sheet_names, function(sheet_name) {
      csv_path <- paste0(sheet_name, "_master_thesis_cala.csv")
      # Read the source file
      df_xlsx <- read_excel(xlsx_path, sheet = sheet_name)
      # Remove .
      df_xlsx[df_xlsx == '.'] <- NA
      # Overwrite the CSV file
      write_csv(df_xlsx, csv_path)
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
#' Input the adjustable parameters dictionary, the name of the parameter to extract
#' the values for, and the type these values should be. Extract all the values of that
#' parameters and return the vector of the values
#' 
#' @details The reason for having this function is that if the user inputs multiple
#' values into a vector dictionary in R, the language assigns each value its unique
#' key, as it can not store a nested element. RRRRRRRR
#' 
#' @param adj_params [vector] A vector dictionary of adjustable parameters.
#' @param desired_param [character] The name of the parameter for which to extract
#' the values for
#' @param param_type [character] Type of the character. Can only be one of the two:
#' numeric, character
#' @return Vector of values.
getMultipleParams <- function(adj_params, desired_param, param_type){
  # Validate input
  stopifnot(
    is.vector(adj_params),
    is.character(desired_param),
    is.character(param_type),
    param_type %in% c("numeric", "character")
  )
  res <- c()
  # Assume only one value
  one_val <- adj_params[desired_param]
  if (!is.na(one_val)){ # Only one value
    res <- append(res, one_val)
  } else {
    # More values
    keep_going <- T
    i <- 1
    while (keep_going){
      new_key <- paste0(desired_param,as.character(i))
      new_param <- adj_params[new_key]
      if (!is.na(new_param)){
        res <- append(res, new_param)
        i <- i + 1
      } else {
        keep_going <- F
      }
    }
  }
  # No values for this key - a single NA - a wonky case
  if (length(res) == 1){
    if (is.na(res)){
      return(NA)
    }
  }
  # Correct types
  if (param_type == "character"){
    res <- as.character(res)
  } else if (param_type == "numeric"){
    res <- as.numeric(res)
  } else {
    stop("This variable type is unaccepted/unhandled for vectors of multiple values.")
  }
  return(res)
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
    is.vector(conditions)
  )
  # Subset the data given the conditions
  if (!is.na(conditions)) {
    for (condition in conditions) {
      if (!is.na(condition)){
        # Evaluate each condition and apply it to the data frame
        data <- data[eval(parse(text = paste0("data$", condition))),]
      }
    }
  }
  return(data)
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


######################### DATA PREPROCESSING #########################


#' Check that the input variable list specifications are all correct
#'
#' @param input_var_list [data.frame] The input variable list
validateInputVarList <- function(input_var_list){
  # Validate that data type stays consistent within each group
  for (i in 1:max(input_var_list$group_category)){
    data_slice <- input_var_list$data_type[input_var_list$group_category == i]
    arbitrary_type <- data_slice[1] # Should be equal for all
    validity_test <- all(data_slice == arbitrary_type)
    stopifnot(validity_test)
  }
  
  # Validate that specifications are present for all variables where sum stats are required
  data_to_summarize <- input_var_list[input_var_list$effect_sum_stats == TRUE, ]
  for (i in 1:nrow(data_to_summarize)){
    temp_row <- data_to_summarize[i,]
    # Only one of the two specifications is used
    validity_test <- xor(!is.na(temp_row$equal),!is.na(temp_row$gtlt))
    stopifnot(validity_test)
  }
    
  # Check data values
  dummy_data_to_check <- data_to_summarize[data_to_summarize$data_type == 'dummy',]
  dummy_data_allowed_values <- c(0, 1)
  for (i in 1:nrow(dummy_data_to_check)){
    temp_row <- dummy_data_to_check[i,]
    validity_test <- temp_row$equal %in% dummy_data_allowed_values
    stopifnot(all(validity_test))
  }
  
  perc_data_to_check <- data_to_summarize[data_to_summarize$data_type == 'perc',]
  for (i in 1:nrow(perc_data_to_check)){
    temp_row <- perc_data_to_check[i,]
    validity_test <- c(
      temp_row$gtlt < 1,
      temp_row$gtlt > 0
    )
    stopifnot(all(validity_test))
  }
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
    print("These variables from the source data names do not match the expected names:")
    print(varnames[!(varnames == expected_varnames)])
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
  print("All data preprocessed successfully.")
  return(input_data)
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
  
  print("Missing data handled successfully.")
  return(input_data)
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
#' @param precision_type [character] Type of precision to use. Can be one of the following:
#'  1/SE - Inverse of the standard error is used.
#'  DoF - Square root of the degrees of freedom is used.
#'  Deafults to "DoF".
#' @return A data frame with the same columns as input_data, where the effect, standard error of the effect,
#' and t-statistic columns have been Winsorized.
winsorizeData <- function(input_data, win_level, precision_type = "DoF"){
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is.numeric(win_level),
    is.character(precision_type),
    win_level > 0,
    win_level < 1,
    all(c('effect', 'se') %in% colnames(input_data)),
    !(any(is.na(input_data$effect))), # No missing effect values
    !(any(is.na(input_data$se))), # No missing SE values
    precision_type %in% c("1/SE", "DoF")
    )
  # Get the winsorization interval
  win_int <-  c(win_level, 1-win_level) # e.g. c(0.01, 0.99)
  # Create a t-stat column if it does not exist in the data
  if (!("t_stat") %in% colnames(input_data)){
    input_data$t_stat <- input_data$effect / input_data$se
  }
  # Statistic preprocessing
  input_data$effect_w <- Winsorize(x = input_data$effect, minval = NULL, maxval = NULL, probs = win_int)
  input_data$se_w <- Winsorize(x = input_data$se, minval = NULL, maxval = NULL, probs = win_int)
  if (precision_type == "1/SE"){
    input_data$se_precision_w <- 1/input_data$se_w
  } else if (precision_type == "DoF"){
    input_data$se_precision_w <- sqrt(input_data$reg_df)
  } else {
    stop("Invalid type of precision")
  }
  input_data$t_w <- Winsorize(x = input_data$t_stat, minval = NULL, maxval = NULL, probs = win_int)
  input_data$significant_w <- c(rep(0,nrow(input_data)))
  input_data$significant_w[(input_data$t_w > 1.96) | (input_data$t_w < -1.96)] <- 1
  # Return quietly
  print("Data winsorized successfully.")
  invisible(input_data)
}


#' A very restrictive function for validating the correct data types
#' 
#' Check that all values across all columns have the correct data type, check that all dummy groups
#' are correctly specified across all observations (only one 1, otherwise 0), and 
#' stop and print out the error message, if it is not so.
#' 
#' Allows for skipping NA values, if your data set is incomplete. Otherwise
#' 
#' @param input_data [data.frame] The data frame to be validated
#' @param input_var_list [data.frame] The data frame that specifies the data type of each variable
#' @param ignore_missing [bool] If TRUE, allow missing values in the data frame. Deafults to FALSE.
validateData <- function(input_data, input_var_list, ignore_missing = F){
  # Columns that the data frame must contain
  expected_columns <- c("study_name", "study_id", "effect", "se", "t_stat", "n_obs", "study_size", "reg_df")
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.logical(ignore_missing),
    all(expected_columns %in% colnames(input_data))
  )
  
  # Do not pass if any values are NA. 
  if (!ignore_missing){
    if(any(is.na(input_data))){
      stop("This script does not allow for ANY missing values. Make sure you have called the data preprocessing function.")
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
  for (i in length(splitted_data_frames)){
    df_to_test <- splitted_data_frames[[i]] # Explicit for indexing clarity
    # Skip empty groups in development - should not be used
    if(ignore_missing & all(is.na(df_to_test))){
      next # Empty group
    }
    if(!any(apply(df_to_test,1,sum) == 1)){ # Check that for all rows of all groups, only one value is 1, other are 0
        stop("Invalid dummy group configuration: One and only one dummy variable must be 1 in each group for each row")
    }
  }
  # Validate that all perc variables sum up to 1
  # TO-DO
  
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
  print("All values across all columns of the main data frame are of the correct type.")
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
                            "Min", "Max", "SD", "Missing obs")
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
      var_missing_verbose
    )
    df[row_idx, ] <- row_data
  }
  # Print and return output data frame
  cat("Variable summary statistics:\n")
  print(df)
  cat("\n")
  if (length(missing_data_vars) > 0){
    print(paste0("Missing data for: ", length(missing_data_vars), " variables."))
    cat("\n")
  }
  invisible(df)
}


#' The function getEffectSummaryStats() calculates the summary statistics for variables in a given data frame input_data
#'    using the percentage of correct classification (Effect) effect_w and sample size study_size columns,
#'    and returns a data frame with the results. The function takes as input input_var_list,
#'    a data frame that contains metadata about the variables in input_data and which variables to calculate
#'    summary statistics for. The summary statistics calculated are the mean, median, weighted mean,
#'    minimum, maximum, standard deviation, and number of observations. For the weighted mean,
#'    the inverse squared sample size is used as weights. The confidence level for the weighted mean
#'    confidence interval can be set using the conf.level parameter, which defaults to 0.95.
#'    If any input data is missing or non-numeric, it is ignored, and the variable is not included in the output.
#' 
#' The function returns a data frame with the following columns:
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
getEffectSummaryStats <- function (input_data, input_var_list, conf.level = 0.95) {
  # Parameter checking
  stopifnot(all(c(conf.level > 0, conf.level < 1)))
  
  # Constants
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE) # Z value for conf. int. calculation
  effect_data <- with(input_data, as.vector(effect_w))
  study_size_data <- with(input_data, as.vector(study_size))
  
  # Output columns
  effect_stat_names <- c("Var Name", "Var Class", "Mean", "Median", "Weighted Mean",
                     "WM CI lower", "WM CI upper", "Min", "Max", "SD", "Obs")
  
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
      var_median <- round(median(input_effect_data), 3)
      var_weighted_mean <- round(weighted.mean(input_effect_data, w = input_study_size_data^2),3)
      var_sd <- round(sd(input_effect_data), 3)
      var_ci_lower <- round(var_weighted_mean - var_sd*z, 3)
      var_ci_upper <- round(var_weighted_mean + var_sd*z, 3)
      var_min <- round(min(input_effect_data), 3)
      var_max <- round(max(input_effect_data), 3)
      var_obs <- length(input_effect_data)
      
      new_row <- data.frame(
        col1 = input_var_name,
        col2 = input_class_name,
        col3 = var_mean,
        col4 = var_median,
        col5 = var_weighted_mean,
        col6 = var_ci_lower,
        col7 = var_ci_upper,
        col8 = var_min,
        col9 = var_max,
        col10 = var_sd,
        col11 = var_obs
      )
      return (new_row)
    }
    
    # EQUAL data
    if (!is.na(equal_val)){
      new_varname_equal <- paste0(var_name_verbose, " = ", round(as.numeric(cutoff,3)))
      new_row <- getNewDataRow(new_varname_equal, var_class, effect_data_equal, study_size_data_equal)
      df <- rbind(df, new_row)
    } else { # GTLT data
      new_varname_gt <- paste0(var_name_verbose, " >= ", round(as.numeric(cutoff,3)))
      new_varname_lt <- paste0(var_name_verbose, " < ", round(as.numeric(cutoff,3)))
      new_row_gt <- getNewDataRow(new_varname_gt, var_class, effect_data_gt, study_size_data_gt)
      new_row_lt <- getNewDataRow(new_varname_lt, var_class, effect_data_lt, study_size_data_lt)
      df <- rbind(df, new_row_gt)
      df <- rbind(df, new_row_lt)
    }
  }
  # Put the final output together
  colnames(df) <- effect_stat_names
  cat("Summary statistics:\n")
  print(df)
  cat("\n")
  if (length(missing_data_vars) > 0){
    print(paste0("Missing data for ", length(missing_data_vars), " variables:"))
    print(missing_data_vars)
    cat("\n")
  }
  invisible(df)
}


#' A quick search function to extract all specified factors for the box plot
#' 
#' @param adj_pars_source [vector] Source vector with adjustable parameters.
#' @param pattern [str] Pattern to search for inside the paramteres vector.
#' @return factor_names [vector] A vector with specified factor names.
getBoxPlotFactors <- function(adj_pars_source, pattern){
  factor_names <- c()
  i <- 1
  while (i < 20) {
    factor_name <- as.character(adj_pars_source[paste0(pattern,i)])
    if (is.na(factor_name)){ # No more factors specified
      break
    }
    factor_names <- append(factor_names, factor_name)
    i <- i + 1
  }
  invisible(factor_names)
}

#' Input the main data frame, specify a factor to group by, and create a box plot.
#' This plot is automatically printed out into the Plots window.
#' 
#' @param input_data [data.frame] Input data
#' @param factor_by [str] Factor to group by. Can be one of the following:
#'  - 'country'
#'  - 'study_level'
#'  Defaults to 'country'
#' @param verbose [bool] If T, print out the information about the plot being printed.
#'  Defaults to T.
#' @param effect_name [character] Verbose explanation of the effect.
getBoxPlot <- function(input_data, factor_by = 'country', verbose=T, effect_name = 'effect'){
  # Check column and input validity
  expected_cols <- c('effect_w', factor_by)
  stopifnot(
    all(expected_cols %in% colnames(input_data)),
    is.data.frame(input_data),
    is.character(factor_by),
    is.logical(verbose),
    is.character(effect_name)
  )
  
  # Plot variable preparation
  factor_levels <- rev(sort(unique(input_data[[factor_by]]))) # Dark magic - tells plot how to group y-axis
  factor_by_verbose <- gsub("_", " ", factor_by) # More legible y-axis label
  
  # Construct the plot - use !!sym(factor_by) to cast some more dark magic - makes plot recognize function input
  box_plot <- ggplot(data = input_data, aes(x = effect_w, y=factor(!!sym(factor_by), levels = factor_levels))) +
      geom_boxplot(outlier.colour = "#005CAB", outlier.shape = 21, outlier.fill = "#005CAB", fill="#e6f3ff", color = "#0d4ed1") +
      geom_vline(aes(xintercept = mean(effect_w)), color = "red", linewidth = 0.85) + 
      labs(title = NULL,x=paste("Effect of", tolower(effect_name)), y = "Grouped by " %>% paste0(factor_by_verbose)) +
      main_theme()
  
  # Plot the plot
  print(paste0('Printing a box plot for the factor: ', factor_by_verbose))
  suppressWarnings(print(box_plot))
  cat('\n')
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
  expected_cols <- c('effect_w', 'se_precision_w')
  stopifnot(
    is.data.frame(input_data),
    is.numeric(effect_proximity),
    is.numeric(maximum_precision),
    is.logical(verbose),
    all(expected_cols %in% colnames(input_data)),
    effect_proximity <= 1,
    effect_proximity >= 0,
    maximum_precision <= 1,
    maximum_precision >= 0
  )
  
  # Get source values
  obs <- input_data$obs_n
  effect <- input_data$effect_w
  precision <- input_data$se_precision_w
  
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
#' @return A list with two elements: "output_vec", a sorted numeric vector containing the generated tick values and the mean value,
#'         and "x_axis_tick_text", a character vector of the same length as "output_vec", 
#'         with "red" indicating the position of the mean value and "black" for all other positions.
generateFunnelTicks <- function(input_vec){
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
  x_axis_tick_text[mean_index] <- "red"
  
  # Round all ticks to 2 decimal spaces
  funnel_ticks <- round(funnel_ticks, 2)
  
  return(list("funnel_ticks" = funnel_ticks, "x_axis_tick_text" = x_axis_tick_text))
}


#' Input the main data frame, several specifications, and create a funnel plot
#' 
#' @Note: In accordance with Stanley (2005), we decide to use the square root of the degrees of freedom
#'  isntead of 1/SE as a measure of precision, to account for study size.
#'  
#' @param input_data [data.frame] Main data frame. Must contain cols 'effect_w', 'se_precision_w'
#' @param effect_proximity [float] Cutoff point for the effect. See getOutliers() for more.
#' @param maximum_precision [float] Cutoff point for precision. See getOutliers() for more.
#' @param use_study_medians [bool] If TRUE, plot medians of studies instead of all observations.
#'  Defaults to FALSE.
#' @param verbose [bool] If T, print out outlier information. Defaults to T.
getFunnelPlot <- function(input_data, effect_proximity=0.2, maximum_precision=0.2,
                          use_study_medians = F, verbose = T){
  # Check input validity
  expected_cols <- c('effect_w', 'se_precision_w')
  stopifnot(
    is.data.frame(input_data),
    is.numeric(effect_proximity),
    is.numeric(maximum_precision),
    is.logical(verbose),
    all(expected_cols %in% colnames(input_data)),
    effect_proximity <= 1,
    effect_proximity >= 0,
    maximum_precision <= 1,
    maximum_precision >= 0
  )
  
  # Filter out the outliers
  filter_effect_w <- getOutliers(input_data, effect_proximity=effect_proximity, maximum_precision=maximum_precision, verbose=verbose)
  
  # Create the data frame for the funnel plot
  funnel_data <- data[filter_effect_w, c('study_id', 'effect_w', 'se_precision_w')] # Only Effect, Precision
  funnel_data[] <- lapply(funnel_data, as.numeric) # To numeric
  
  # Plot study medians instead
  if (use_study_medians){
    funnel_data <- funnel_data %>%
      group_by(study_id) %>% 
      summarize(median_effect_w = median(effect_w),
              median_se_precision_w = se_precision_w[which.min(abs(effect_w - median_effect_w))])
    colnames(funnel_data) <- c("study_id", "effect_w", "se_precision_w")
  }
  
  # Get visual bounds and tick colors
  funnel_x_lbound <- min(funnel_data$effect_w)
  funnel_x_ubound <- max(funnel_data$effect_w)
  mean_x_tick <- mean(funnel_data$effect_w)
  # Generate and extract the info
  base_funnel_ticks <- c(funnel_x_lbound, funnel_x_ubound, mean_x_tick) # c(lbound, ubound, mean)
  funnel_visual_info <- generateFunnelTicks(base_funnel_ticks)
  funnel_ticks <- funnel_visual_info$funnel_ticks
  funnel_tick_text <- funnel_visual_info$x_axis_tick_text
  
  # Plot the plot
  x_title <- ifelse(use_study_medians, "study median values", "all observations")
  quiet(
    funnel_win <- ggplot(data = funnel_data, aes(x = effect_w, y = se_precision_w)) + 
      geom_point(color = "#0d4ed1") + 
      geom_vline(aes(xintercept = mean(effect_w)), color = "red", linewidth = 0.5) + 
      labs(title = NULL, x = paste("Estimate of the effect -",x_title), y = "Precision of the effect") +
      scale_x_continuous(breaks = funnel_ticks) +
      main_theme(
          x_axis_tick_text = funnel_tick_text
      )
  )
    
  suppressWarnings(print(funnel_win)) # Print out the funnel plot
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
#' @return A list with two elements: "output_vec", a sorted numeric vector containing the generated tick values, the mean value,
#'         and the t-statistics values, and "x_axis_tick_text", a character vector of the same length as "output_vec",
#'         with "red" indicating the positions of the t-statistics values, "darkoran
generateHistTicks <- function(input_vec) {
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
  
  ticks <- c(ticks, t_stat_low, t_stat_high)
  current_tick <- ceiling(lower_bound / 50) * 50 # Closest number divisible by 50 higher than lower bound
  
  while (current_tick < upper_bound) {
    if (abs(current_tick - lower_bound) >= 2 && 
        abs(current_tick - upper_bound) >= 2 &&
        abs(current_tick - t_stat_low) >= 2 &&
        abs(current_tick - t_stat_high) >= 2
        ) {
      ticks <- c(ticks, round(current_tick, 2))
    }
    current_tick <- current_tick + 50
  }
  
  # Add the mean value and sort the vector
  hist_ticks <- sort(c(ticks, mean_value))
  
  # Create the color vector
  x_axis_tick_text <- rep("black", length(hist_ticks))
  mean_index <- which(hist_ticks == mean_value)
  t_stat_low_index <- which(hist_ticks == t_stat_low)
  t_stat_high_index <- which(hist_ticks == t_stat_high)
  
  x_axis_tick_text[mean_index] <- "darkorange"
  x_axis_tick_text[t_stat_low_index] <- "red"
  x_axis_tick_text[t_stat_high_index] <- "red"
  
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
#' @param higher_tstat Similar to lower_tstat
#' @return A histogram plot of the T-statistic values with density overlay and mean, as well as vertical
#'  lines indicating the critical values of a two-tailed T-test with a significance level of 0.05.
getTstatHist <- function(input_data, lower_cutoff = -120, upper_cutoff = 120,
                         lower_tstat = -1.96, upper_tstat = 1.96){
  # Specify a cutoff filter
  t_hist_filter <- (data$t_w > lower_cutoff & data$t_w < upper_cutoff) #removing the outliers from the graph
  hist_data <- input_data[t_hist_filter,]
  
  # Get lower bound
  lbound_choices <- c(lower_cutoff, min(hist_data$t_w)) # Either lowest t-stat, or cutoff point
  hist_lbound <- lbound_choices[which.max(lbound_choices)] # Choose the higher one
  # Get upper bound
  ubound_choices <- c(upper_cutoff, max(hist_data$t_w)) # Either highest t-stat, or cutoff point
  hist_ubound <- ubound_choices[which.min(ubound_choices)] # Choose the lower one
  # Put all the visual information input together
  hist_mean <- mean(hist_data$t_w)
  base_hist_ticks <- c(hist_lbound, hist_ubound, hist_mean, lower_tstat, upper_tstat)
  # Generate and extract variable visual information
  hist_visual_info <- generateHistTicks(base_hist_ticks)
  hist_ticks <- hist_visual_info$hist_ticks
  hist_ticks_text <- hist_visual_info$x_axis_tick_text
  
  # Construct the histogram
  quiet(
    t_hist_plot <- ggplot(data = hist_data, aes(x = t_w, y = after_stat(density))) +
      geom_histogram(color = "black", fill = "#1261ff", bins = "80") +
      geom_vline(aes(xintercept = mean(t_w)), color = "dark orange", linetype = "dashed", linewidth = 0.7) + 
      geom_vline(aes(xintercept = lower_tstat), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = upper_tstat), color = "red", linewidth = 0.5) +
      labs(x = "T-statistic", y = "Density") +
      scale_x_continuous(breaks = hist_ticks) + 
      main_theme(
        x_axis_tick_text = hist_ticks_text)
  )
  # Print out the plot
  suppressWarnings(print(t_hist_plot))
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
  pub_bias_coef <- round(coeftest_object[2,"Estimate"], 5)
  pub_bias_se <- round(coeftest_object[2,"Std. Error"], 5)
  effect_coef <- round(coeftest_object[1,"Estimate"], 5)
  effect_se <- round(coeftest_object[1,"Std. Error"], 5)
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
  required_cols <- c("effect_w", "se_w", "study_id", "study_size", "se_precision_w")
  stopifnot(all(required_cols %in% names(data)))
  # OLS
  ols <- lm(formula = effect_w ~ se_w, data = data)
  ols_res <- coeftest(ols, vcov = vcovHC(ols, type = "HC0", cluster = c(data$study_id)))
  ols_coefs <- extractLinearCoefs(ols_res)
  # Between effects
  be <- plm(effect_w ~ se_w, model = "between", index = "study_id", data = data)
  be_res <- coeftest(be, vcov = vcov(be, type = "fixed", cluster = c(data$study_id)))
  be_coefs <- extractLinearCoefs(be_res)
  # Random Effects
  re <- plm(effect_w ~ se_w, model = "random", index = "study_id", data = data)
  re_res <- coeftest(re, vcov = vcov(re, type = "fixed", cluster = c(data$study_id)))
  re_coefs <- extractLinearCoefs(re_res)
  # Weighted by number of observations per study
  ols_w_study <- lm(formula = effect_w ~ se_w, data = data, weight = (data$study_size*data$study_size))
  ols_w_study_res <- coeftest(ols_w_study, vcov = vcovHC(ols_w_study, type = "HC0", cluster = c(data$study_id)))
  ols_w_study_coefs <- extractLinearCoefs(ols_w_study_res)
  # Weighted by precision
  ols_w_precision <- lm(formula = effect_w ~ se_w, data = data, weight = c(data$se_precision_w*data$se_precision_w))
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
  # Print the results into the console
  print("Results of the linear tests, clustered by study:")
  print(results)
  cat("\n\n")
  # Return silently
  invisible(results) 
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
extractNonlinearCoefs <- function(nonlinear_object, pub_bias_present = F, verbose_coefs=T){
  # Extract coefficients
  effect_coef <- round(as.numeric(nonlinear_object[1,1]), 5)
  effect_se <- round(as.numeric(nonlinear_object[1,2]), 5)
  if (pub_bias_present){
    pub_coef <- round(as.numeric(nonlinear_object[2,1]), 5)
    pub_se <- round(as.numeric(nonlinear_object[2,2]), 5)
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
  WLS_FE_avg <- sum(data$effect_w/data$se_w)/sum(1/data$se_w)
  WAAP_bound <- abs(WLS_FE_avg)/2.8
  WAAP_reg <- lm(formula = effect_w ~ -se_precision_w, data = data[data$se_w<WAAP_bound,])
  WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(data$study_id)))
  WAAP_coefs <- extractNonlinearCoefs(WAAP_reg_cluster, ...)
  invisible(WAAP_coefs)
}

###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######


getTop10Results <- function(data, ...){
  T10_bound <- quantile(data$se_precision_w, probs = 0.9) #Setting the 90th quantile bound
  T10_reg <- lm(formula = effect_w ~ -se_precision_w, data = data[data$se_precision_w>T10_bound,]) #Regression using the filtered data
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
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated coefficients for the STEM-based method
#' in the usual format.
#' 
#' @import stem_method_master_thesis_cala.R
getStemResults <- function(data, ...){
  source("stem_method_master_thesis_cala.R") #github.com/Chishio318/stem-based_method
  
  stem_param <- c(
    10^(-4), # Tolerance - set level of sufficiently small stem to determine convergence
    10^3 # max_N_count - set maximum number of iteration before termination
  )
  
  est_stem <- stem(data$effect_w, data$se_w, stem_param)$estimates # Actual esimation
  
  # Save results
  stem_coefs <- extractNonlinearCoefs(est_stem, ...)
  invisible(stem_coefs)
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
    y <- data$effect_w[filter] #Effects from the i-th study
    X <- cbind(1,
               data$se_w[filter])
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
getSelectionResults <- function(data, cutoffs = c(1.960),
                                symmetric = F, modelmu="normal", ...){
  # Read the source script
  source("selection_model_master_thesis_cala.R") 
  # Validate input
  stopifnot(all(cutoffs %in% c(1.645, 1.960, 2.576))) # Cutoffs
  stopifnot(modelmu %in% c("normal", "t")) # Model
  # Validate that the necessary columns are present
  required_cols <- c("effect_w", "se_w")
  stopifnot(all(required_cols %in% names(data)))
  # Extract winsorized estimates, standard errors
  sel_X <- data$effect_w # Effect - Winsorized
  sel_sigma <- data$se_w # SE - Winsorized
  
  # Estimation
  estimates <- metastudies_estimation(sel_X, sel_sigma, cutoffs, symmetric, model = modelmu)
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
#'    Must contain the columns - "effect_w", and "se_w"
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
getEndoKinkResults <- function(data, ...){
  # Read the source file
  source("endo_kink_master_thesis_cala.R")
  # Validate that the necessary columns are present
  required_cols <- c("effect_w", "se_w")
  stopifnot(all(required_cols %in% names(data))) 
  # Extract winsorized estimates, standard errors
  data <- data[,required_cols]
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
#' @return A data frame containing the results of the non-linear tests, clustered by study.
getNonlinearTests <- function(input_data) {
  # Validate the input
  required_cols <- c("effect_w", "se_w", "study_id", "study_size", "se_precision_w")
  stopifnot(
    is.data.frame(input_data),
    all(required_cols %in% names(input_data))
  )
  # Get coefficients
  waap_res <- getWaapResults(input_data, pub_bias_present = F, verbose_coefs = T)
  top10_res <- getTop10Results(input_data, pub_bias_present = F, verbose_coefs = T)
  stem_res <- getStemResults(input_data, pub_bias_present = F, verbose_coefs = T)
  hier_res <- getHierResults(input_data, pub_bias_present = T, verbose_coefs = T)
  sel_res <- getSelectionResults(input_data, pub_bias_present = T, verbose_coefs = T)
  endo_kink_res <- getEndoKinkResults(input_data, pub_bias_present = T, verbose_coefs = T)
  
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
  # Print the results into the console
  print("Results of the non-linear tests, clustered by study:")
  print(results)
  cat("\n\n")
  # Return silently
  invisible(results) 
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
                        round(as.numeric(exo_object[1,1]), 5),
                        "")
  effect_se <- ifelse(effect_present,
                        round(as.numeric(exo_object[1,2]), 5),
                        "")
  pub_coef <- ifelse(pub_bias_present,
                        round(as.numeric(exo_object[2,1]), 5),
                        "")
  pub_se <- ifelse(pub_bias_present,
                        round(as.numeric(exo_object[2,2]), 5),
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
#' @param input_data [data.frame] A data frame containing the effect (effect_w), its standard error (se_w), study ids, and source
#' data for the instrument(s) (specified as separate columns). It must have the columns "effect_w", "se_w", "study_id", and "n_obs".
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
  stopifnot(
    is.data.frame(input_data),
    is.list(instruments),
    is.vector(instruments_verbose),
    all(c("effect_w", "se_w", "study_id", "n_obs") %in% colnames(input_data))
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
    model <- ivreg(formula = effect_w ~ se_w | instrument, data = input_data)
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
#' This function takes in data and finds the best instrument for the IV regression of effect_w against se_w.
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
#' data <- data.frame(effect_w = rnorm(10), se_w = rnorm(10), n_obs = rep(10, 10), study_id = rep(1:10, each = 1))
#' getIVResults(data)
getIVResults <- function(data, ...){
  # Define the instruments to use
  instruments <- list(1/sqrt(data$n_obs), 1/data$n_obs, 1/data$n_obs^2, log(data$n_obs))
  instruments_verbose <- c('1/sqrt(data$n_obs)', '1/data$n_obs', '1/data$n_obs^2', 'log(data$n_obs)')
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
  best_instrument <- instruments[match(best_instrument, instruments_verbose)]
  # Run the regression
  instrument <- best_instrument[[1]] # Unlist
  stopifnot(is.numeric(instrument)) # Amend previous line if this keeps failing - should be only numeric
  model <- ivreg(formula = effect_w ~ se_w | instrument, data = data)
  model_summary <- summary(model, vcov = vcovHC(model, cluster = c(data$study_id)), diagnostics=T)
  # Get the coefficients
  all_coefs <- model_summary$coefficients
  IV_coefs_vec <- c(
    all_coefs["(Intercept)","Estimate"], # Effect
    all_coefs["(Intercept)", "Std. Error"], # Effect SE
    all_coefs["se_w", "Estimate"], # Pub Bias
    all_coefs["se_w", "Std. Error"] # Pub Bias SE
    ) 
  iv_coefs_mat <- matrix(IV_coefs_vec, nrow=2, ncol=2, byrow=TRUE)
  # Extract the coefficients and return as a vector
  iv_coefs_out <- extractExoCoefs(iv_coefs_mat, ...) 
  return(iv_coefs_out)
}

###### PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) ######

#' getMedians - Calculates the vector of medians for effect_w
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
#' @param method [str] A character string indicating which method to use to estimate publication bias.
#' "ML" for Maximum Likelihood and "P" for Moments. Default is "ML".
#'
#' @return A vector containing the following four elements:
#' \describe{
#' \item{Test Statistic for the P-uniform publication bias test}{A character string indicatingthe L test
#'  statistic for the P-uniform publication bias test.}
#' \item{P-value for the L test statistic}{A character string indicating the P-value for the L test statistic.}
#' \item{Effect Beyond Bias}{A numeric value indicating the effect beyond bias estimate.}
#' \item{Effect Standard Error}{A character string indicating the standard error of the effect beyond bias estimate.}
#' }
getPUniResults <- function(data, method="ML",...){
  # Validation
  stopifnot(
    is.data.frame(data),
    is.character(method),
    method %in% c("ML", "P") # Max likelihood, Moments
  )
  # Calculate medians for all studies
  med_t <- getMedians(data, 't_w') # T-stat vector of medians
  med_nobs <- getMedians(data, 'n_obs') # N-obs vector of medians
  #Estimation
  if (method == "ML"){
    #Maximum likelihood
    quiet(
      est_main <- puni_star(tobs = med_t, ni = med_nobs, side = "right", method = "ML", alpha = 0.05,
                             control=list(stval.tau=0.5, max.iter=1000,tol=0.1,reps=10000, int=c(0,2), verbose=TRUE))
    )
  } else if (method == "P"){
    # Moments method estimation
    quiet(
      est_main <- puni_star(tobs = med_t, ni = med_nobs, side = "right", method = "P",
                          alpha = 0.05,control=list(stval.tau = 0.5, max.iter=1000, tol=0.05, reps=10000, int=c(-1,1), verbose=TRUE))
    )
  } else {
    stop("Broken validity checks") # Should not happen
  }
  est_se <- (est_main$ci.ub - est_main$ci.lb) / (2*1.96) # Standard error of the estmiate
  # Extract and save coefficients - using a custom format for this method
  est_effect_verbose <- round(est_main$est, 5) # Effect Beyond Bias
  est_se_verbose <- paste0("(", round(est_se, 5), ")") # Effect Standard Error
  est_pub_test_verbose <- paste0("L = ", round(est_main$L.0, 5)) # Test statistic of p-uni publication bias test
  est_pub_p_val_verbose <- paste0("(p = ", round(est_main$pval.0, 5), ")") # P-value for the L test statistic
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
#' @param input_data [data.frame] A data frame containing the necessary columns: "effect_w", "se_w", "study_id", "study_size", and "se_precision_w".
#' @return A data frame with the results of the three tests for publication bias and exogeneity in IV analyses using clustered data.
#'
#' @details This function first validates that the necessary columns are present in the input data frame.
#' If the validation is successful, it performs three tests for publication bias and exogeneity in instrumental variable (IV)
#' analyses using clustered data: the IV test, and the p-Uniform test. The results of the two tests are combined
#' into a data frame, with row names corresponding to the tests and column names corresponding to the test type.
#' The results are then printed into the console and returned invisibly.
getExoTests <- function(input_data) {
  # Validate that the necessary columns are present
  required_cols <- c("effect_w", "se_w", "study_id", "study_size", "se_precision_w")
  stopifnot(
    is.data.frame(input_data),
    all(required_cols %in% names(input_data))
  )
  # Get coefficients
  iv_res <- getIVResults(input_data, effect_present = T, pub_bias_present = T, verbose_coefs = T)
  p_uni_res <- getPUniResults(input_data, effect_present = T, pub_bias_present = T, verbose_coefs = T)
  # Combine the results into a data frame
  results <- data.frame(
    iv_df = iv_res,
    p_uni_df = p_uni_res)
  # Label names
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)")
  colnames(results) <- c("IV", "p-Uniform")
  # Print the results into the console
  print("Results of the tests relaxing exogeneity, clustered by study:")
  print(results)
  cat("\n\n")
  # Return silently
  invisible(results) 
}

######################### P-HACKING TESTS #########################

###### PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008) ######

#' Run a Caliper Test
#'
#' This function performs a Caliper test on a data set to detect selective reporting of statistically significant results.
#'
#' @param input_data [data.frame] A data.frame containing the data set to be tested. The data.frame must have at least two columns named
#'  "t_w" and "study_id", and these columns must be numeric.
#' @param threshold [numeric] The t-statistic threshold used to define statistically significant results. Default is 1.96.
#' @param width [numeric] The width of the Caliper interval used to define the sub-sample of observations used in the test. Default is 0.05.
#' @return A numeric vector with four elements: the estimate of the proportion of results reported, the standard error of the estimate,
#' the number of observations with t-statistics above the threshold, and the number of observations with t-statistics below the threshold.
runCaliperTest <- function(input_data, threshold = 1.96, width = 0.05){
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is.numeric(threshold),
    is.numeric(width),
    all(c("t_w", "study_id") %in% colnames(input_data))
  )
  # Add a column indicating which observations have t-stats above (below) threshold
  if (threshold >= 0){ # Explicit because ifelse does not work, due to some dark spells probably
    significant_obs <- input_data$t_w > threshold
  } else {
    significant_obs <- input_data$t_w < threshold
  }
  input_data$significant_w <- ifelse(significant_obs, 1, 0) # Col of 0/1
  # Initialize variables for output storage
  caliper_output <- list()
  # Run the test
  lower_bound <- input_data$t_w > ( threshold - width ) # Bool vector
  upper_bound <- input_data$t_w < ( threshold + width ) # Bool vector
  subsetted_data <- input_data[lower_bound & upper_bound,] # Only desired rows
  if (nrow(subsetted_data) == 0){
    return(c(0,0,0,0)) # No observations in the interval
  }
  cal_res <- lm(formula = significant_w ~ t_w - 1, data = subsetted_data)
  cal_res_coefs <- coeftest(cal_res, vcov = vcovHC(cal_res, type = "const", cluster = c(input_data$study_id)))
  cal_est <- cal_res_coefs["t_w", "Estimate"] # Estimate
  cal_se <- cal_res_coefs["t_w", "Std. Error"] # Standard Error
  cal_above <- nrow(subsetted_data[subsetted_data$t_w > threshold, ]) # N. obs above the threshold
  cal_below <- nrow(subsetted_data[subsetted_data$t_w < threshold, ]) # N. obs below the threshold
  # Return the output
  res <- c(
    round(cal_est, 5),
    round(cal_se, 5),
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
  if (verbose){
    print("Results of the Caliper tests:")
    print(result_df)
    cat("\n\n")
  }
  # Return the data frame
  invisible(result_df)
}

###### PUBLICATION BIAS - p-hacking test (Eliott et al., 2022) ######

#' getEliottResults - Calculate Elliot's five tests and other statistics for a given dataset
#'  - Source: https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA18583
#'
#' @param input_data A data frame containing at least the "t_w" and "reg_df" columns.
#' @param data_subsets A character vector with the names of the subsets of data to test. By default, only "All data" is tested.
#' @param p_min The minimum p-value threshold for the tests. Default is 0.
#' @param p_max The maximum p-value threshold for the tests. Default is 1.
#' @param d_point The discontinuity cutoff point for the discontinuity test. Default is 0.15.
#' @param CS_bins The number of bins for the Cox-Shi test. Default is 10.
#' @param verbose A logical indicating whether to print the results to console. Default is TRUE.
#'
#' @return A data frame with the results of the Elliot tests and other statistics.
getEliottResults <- function(input_data, data_subsets = c("All data"), 
      p_min = 0, p_max = 1, d_point = 0.15, CS_bins = 10, verbose = T){
  # Validate input
  stopifnot(
    is.data.frame(input_data),
    is.vector(data_subsets),
    is.numeric(p_min),
    is.numeric(p_max),
    is.numeric(d_point),
    is.numeric(CS_bins),
    is.logical(verbose),
    all(c("t_w", "reg_df") %in% colnames(input_data))
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
  elliot_df <- data.frame(matrix(NA, nrow = length(data_rownames), ncol = length(data_colnames)))
  rownames(elliot_df) <- data_rownames
  colnames(elliot_df) <- data_colnames
  # Load the source script
  source("elliot_master_thesis_cala.R")
  # Load the file with CDFs (if it does not exist, create one)
  elliot_source_file <- "elliot_data_master_thesis_cala.csv"
  if (file.exists(elliot_source_file)){
    cdfs <- read.csv(elliot_source_file, col.names = "cdfs") # Read if exists
  } else {
    cdfs <- getCDFs(create_csv = T) # Generate the file from scratch (takes time)
  }
  cdfs <- as.numeric(cdfs[,1]) # To a numeric vector
  # Run the estimation for all data subsets
  for (data_col in data_colnames){
    # Get the data subset
    data <- input_data # Adjust later if desired
    
    # Convert t-statistics to p-values
    t_w <- data$t_w
    df <- data$reg_df
    P <- 2 * pt(abs(t_w), df = df, lower.tail=FALSE) # p-values
     
    # Tests (each test returns the corresponding p-value)
    Bin_test <- Binomial(P, p_min, p_max, "c")
    Discontinuity <- Discontinuity_test(P,d_point, h)
    LCM_sup <- LCM(P, p_min,p_max, lcm_norm, cdfs)
    CS_1 <- CoxShi(P,id, p_min, p_max, CS_bins, 1, 0) #Test for 1-monotonicity
    CS_2B <- CoxShi(P,id, p_min, p_max, CS_bins, 2, 1) #Test for 2-monotonicity and bounds
    FM <- Fisher(P, p_min, p_max)
    
    # Save the results
    elliot_res <- c(Bin_test, Discontinuity, LCM_sup, CS_1, CS_2B, FM)
    elliot_res <- sapply(elliot_res, function(x){round(x,3)})
    
    # Thresholds
    n_obs_between <- length(P[P>=p_min&P<=p_max])
    n_obs_below <- length(P[P<=d_point&P>=0])
  
    # Fill in the data frame with values from elliot_res
    elliot_df["Binomial:", data_col] <- elliot_res[1]
    elliot_df["Discontinuity:", data_col] <- elliot_res[2]
    elliot_df["LCM:", data_col] <- elliot_res[3]
    elliot_df["CS1:", data_col] <- elliot_res[4]
    elliot_df["CS2B:", data_col] <- elliot_res[5]
    elliot_df["s Test:", data_col] <- elliot_res[6]
    elliot_df[threshold1_verbose, data_col] <- n_obs_between # In between min, max
    elliot_df[threshold2_verbose, data_col] <- n_obs_below # Below disc cutoff
  }
  
  if (verbose){
    print(paste0("Results of the Elliot tests:"))
    print(elliot_df)
    cat("\n\n")
  }
  return(elliot_df)
}

###### MAIVE Estimator (Irsova et al., 2023) ######

#' Run the MAIVE estimation using a modified source script
#'  - Source: http://meta-analysis.cz/maive/
#'
#' @param method [int] Method. Options - PET:1, PEESE:2, PET-PEESE:3, EK:4 (default 3)
#' @param weight [int] Weighting. Options - no weight: 0 ; weights: 1, adjusted weights: 2 (default 0)
#' @param instrument [int] Instrumenting. Options - 0;1(default 1)
#' @param studylevel[int] Correlation at study level. Options -  none: 0 (default), fixed effects: 1, cluster: 2
#'  (default 0)
#' @param verbose [bool] Print out the results into the console in a nice format.
#' @inheritDotParams Parameters for the extractExoCoefs function.
#' 
#' @import maive_master_thesis_cala.R
getMaiveResults <- function(data, method = 3, weight = 0, instrument = 1, studylevel = 0, verbose = T, ...){
  # Read the source file
  source("maive_master_thesis_cala.R")
  # Validate that the necessary columns are present
  required_cols <- c("effect_w", "se_w", "study_size", "study_id")
  stopifnot(
    all(required_cols %in% names(data)),
    method %in% c(1,2,3,4),
    weight %in% c(0,1,2),
    instrument %in% c(0,1),
    studylevel %in% c(0,1,2),
    is.logical(verbose)
  )
  # Subset data and rename columns
  data <- data[,required_cols]
  colnames(data) <- c('bs', 'sebs', 'Ns', 'studyid')
  # Run the estimation
  MAIVE <- maive(dat=data,method=method,weight=weight,instrument=instrument,studylevel=studylevel)
  # Extract (and print) the output
  if (verbose){
    object<-c("MAIVE coefficient","MAIVE standard error","F-test of first step in IV",
              "Hausman-type test (use with caution)","Critical Value of Chi2(1)")
    maive_coefs_all<-c(MAIVE$beta,MAIVE$SE,MAIVE$`F-test`,MAIVE$Hausman,MAIVE$Chi2)
    MAIVEresults<-data.frame(object,maive_coefs_all)
    colnames(MAIVEresults) <- c("Object", "Coefficient")
    print("Results using the MAIVE estimator:")
    print(MAIVEresults)
    cat("\n\n")
  }
  # Extract the main two coefficients
  maive_coefs <- c(
    as.numeric(MAIVE$beta), # MAIVE Coefficient
    as.numeric(MAIVE$SE) # MAIVE Standard Error
    ) 
  return(maive_coefs) # Return as a simple vector
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
#' Otherwise, it returns a formula object of the suggested BMA formula.
findOptimalBMAFormula <- function(input_data, input_var_list, max_groups_to_remove = 30,
                                    return_variable_vector_instead = F, verbose = T) {
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.numeric(max_groups_to_remove),
    is.logical(return_variable_vector_instead),
    is.logical(verbose),
    all(c("bma_potential_var", "var_name", "group_category") %in% colnames(input_var_list))
  )
  # Extract the information from source data
  bma_potential_vars_bool <- input_var_list$bma_potential_var
  potential_vars <- input_var_list$var_name[bma_potential_vars_bool]
  var_grouping <- input_var_list$group_category[bma_potential_vars_bool]
  
  # Pop the effect from var grouping (not used in the iteration)
  var_grouping <- var_grouping[!potential_vars == "effect_w"]
  
  # Get initial BMA formula and VIF coefficients
  bma_formula <- getBMAFormula(potential_vars)
  bma_lm <- lm(bma_formula, data = input_data)
  vif_coefs <- car::vif(bma_lm)
  if (length(var_grouping) != length(vif_coefs)){
    print("The lengths of the variable vectors do not match")
  }

  removed_groups <- 0
  removed_groups_verbose <- c()
  while (any(vif_coefs > 10) && max_groups_to_remove > 0) {
    # Get the group with the highest VIF coefficient
    highest_vif_coef <- which.max(vif_coefs)
    highest_vif_group <- var_grouping[highest_vif_coef]
    # Get new potential vars, new grouping
    vars_to_remove <- potential_vars[var_grouping == highest_vif_group]
    potential_vars <- potential_vars[!potential_vars %in% vars_to_remove]
    var_grouping <- var_grouping[!var_grouping %in% highest_vif_group]
    # Get modified BMA formula and VIF coefficients
    bma_formula <- getBMAFormula(potential_vars)
    bma_lm <- lm(bma_formula, data = input_data)
    vif_coefs <- car::vif(bma_lm)
    if (length(var_grouping) != length(vif_coefs)){
      print("The lengths of the variable vectors do not match")
    }
    # Decrease the maximum number of groups to remove
    max_groups_to_remove <- max_groups_to_remove - 1
    removed_groups <- removed_groups + 1
    removed_groups_verbose <- append(removed_groups_verbose, vars_to_remove)
  }
  # Print out the information about the procedure outcome
  if (max_groups_to_remove == 0) {
    message("Maximum number of groups to remove reached. Returning variables found so far.")
  }
  if (verbose) {
    print(paste("Removed", removed_groups, "groups with VIF > 10."))
    print("The removed groups contained these variables:")
    print(removed_groups_verbose)
    print("The suggested BMA formula is:")
    print(bma_formula)
  }
  # Return the outcome
  if (return_variable_vector_instead){
    return(potencial_vars)
  }
  return(bma_formula)
}

#' Creates a formula for Bayesian model averaging
#'
#' This function creates a formula for Bayesian model averaging based on the variables in \code{var_vector}.
#' The formula includes the variables "effect_w" and "se_w", as well as any other variables specified in \code{var_vector}.
#'
#' @param input_var [vector] A vector of variables that should be used to construct the formula. Must include
#' "effect_w" and "se_w".
#' @param get_var_vector_instead [bool] If TRUE, return a vector with variable names instead, with effect and se
#'  at the first two positions of the vector. Used for a simple rearrangement. Defaults to FALSE.
#' @return A formula object (to be used) Bayesian model averaging
#' 
#' @note To get the vector itself from the formula, you can use the in-built "all.vars()" method instead.
getBMAFormula <- function(input_var, get_var_vector_instead = F){
  # Validate the input
  stopifnot(
    all(c("effect_w","se_w") %in% input_var)
  )
  # Separate the effect and SE from the remaining variables
  bool_wo_effect <- input_var != "effect_w" # Pop effect
  bool_wo_se <- input_var != "se_w" # Pop se
  remaining_vars <- input_var[bool_wo_effect & bool_wo_se] # Remaining variables
  # Get the formula
  if (get_var_vector_instead){
    var_vector <- c("effect_w", "se_w", remaining_vars)
    return(var_vector)
  }
  remaining_vars_verbose <- paste(remaining_vars, sep="", collapse = " + ")
  all_vars_verbose <- paste0("effect_w ~ se_w + ", remaining_vars_verbose)
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
#' @param input_var [vector | formula] One of - vector of variable names, formula. If it is a vector, the function
#' transforms the input into a formula.
#' @param input_data [data.frame] Data to run the test on.
#' @param print_all_coefs [bool] A logical value indicating whether to print all the VIF coefficients into
#'  the console
#'
#' @return [vector] A numeric vector with the VIF coefficients.
runVifTest <- function(input_var, input_data, print_all_coefs = F){
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
    BMA_formula <- getBMAFormula(input_var)
  } else{
    BMA_formula <- input_var
  }
  BMA_reg_test <- lm(formula = BMA_formula, data = input_data)
  # Unhandled exception - fails in case of too few observations vs. too many variables
  vif_coefs <- car::vif(BMA_reg_test) #VIF coefficients
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
#' @note When transforming/subsetting the data, there is a need to convert the data into a
#' data.frame object, otherwise the plot functions will not recognize the data types correctly
#' later on. The "bms" function works well even with a tibble, but the plots do not. RRRRRRR
getBMAData <- function(input_data, input_var_list, variable_info, from_vector = T){
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
#' @param bma_data [data.frame] The data for BMA. "effect_w" must be in the first column.
#' @inheritDotParams Parameters to be used inside the "bms" function. These are:
#' burn, iter, g, mprior, nmodel, mcmc
#' For more info see the "bms" function documentation.
#' @return The bma model
runBMA <- function(bma_data, ...){
  # Input validation
  stopifnot(
    is.data.frame(bma_data),
    !any(is.na(bma_data)), # No missing obs
    all(sapply(bma_data,is.numeric)), # Only numeric obs
    colnames(bma_data[,1]) == "effect_w"
  )
  # Actual estimation with inhereted parameters
  print("Running the Bayesian Model Averaging...")
  quiet(
    bma_model <- bms(bma_data, ...)
  )
  return(bma_model)
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
#'
#' @return A numeric vector containing only the BMA coefficients.
extractBMAResults <- function(bma_model, bma_data, print_results = "fast"){
  # Validate the input
  stopifnot(
    class(bma_model) == "bma",
    is.data.frame(bma_data),
    is.character(print_results),
    print_results %in% c("none", "fast", "verbose", "all")
  )
  # Extract the coefficients
  bma_coefs <- coef(bma_model,order.by.pip= F, exact=T, include.constant=T)
  # Print out coefficient and model statistics
  print("Results of the Bayesian Model Averaging:")
  if (print_results %in% c("verbose","all")){
    print(bma_model) # Coefficients, summary information
    print(bma_model$topmod[1]) # Topmod
  } else if (print_results == "fast"){
    print(bma_coefs)
  }
  # Print out plots (takes time)
  if (print_results == "all"){
    # Main BMA plots
    print("Printing out Bayesian Model Averaging plots. This may take some time...")
    image(bma_model, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE,
          do.axis=TRUE, xlab = "", main = "") # Takes time
    plot(bma_model)
    # Correlation matrix
    bma_col<- colorRampPalette(c("red", "white", "blue"))
    bma_matrix <- cor(bma_data)
    corrplot.mixed(bma_matrix, lower = "number", upper = "circle",
                   lower.col=bma_col(200), upper.col=bma_col(200),tl.pos = c("lt"),
                   diag = c("u"), tl.col="black", tl.srt=70, tl.cex=0.55,
                   number.cex = 0.5,cl.cex=0.8, cl.ratio=0.1) 
  }
  # Return coefficients only
  if (!print_results == "none"){
    cat("\n\n")
  }
  return(bma_coefs)
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
#' This function uses the LowRankQP package for optimization.
runFMA <- function(bma_data, bma_model, verbose = T){
  # Validate input
  stopifnot(
    is.data.frame(bma_data),
    class(bma_model) == "bma",
    names(bma_data[,1]) == "effect_w" # Restrictive, but extra safe
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
    optim <- LowRankQP(Vmat=G,dvec=a,Amat=A,bvec=b,uvec=u,method="LU",verbose=FALSE)
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
    print("Results of the Frequentist Model Averaging:")
    print(fma_res)
    cat("\n\n")
  }
  invisible(fma_res)
}



######################### GRAPHICS #########################

#' Custom ggplot theme
main_theme <- function(x_axis_tick_text = "black"){
  theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = x_axis_tick_text), axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
        plot.background = element_rect(fill = "#DCEEF3"))
}

