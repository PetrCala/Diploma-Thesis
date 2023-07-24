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

#' Clean a folder of all files by destroying and recreating it.
#' If force == F, only old files will get deleted.=
cleanFolder <- function(folder_name, force = F, time_threshold = 1){
  current_time <- Sys.time()
  files <- list.files(path = folder_name)
  files <- file.path(folder_name, files)  # use file.path() to ensure the correct path
  # Remove all files
  for (file in files){
    # Always remove files if force == T, otherwise depending on time
    if(force){
      delete_file <- TRUE
    } else {
      file_mod_time <- file.info(file)$mtime  # get file modification time
      time_diff <- difftime(current_time, file_mod_time, units = "hours")  # calculate time difference in hours
      delete_file <- time_diff > time_threshold
    }
    if (delete_file){
      tryCatch({
        system(paste("rm", file), ignore.stdout = TRUE, ignore.stderr = TRUE)
      }, warning = function(wrn){
        cat("Warning:\n")
        print(wrn)
      }, error = function(err){
        cat("Error:\n")
        print(err)
      })
    }
  }
}

#' Function to read multiple sheets from an Excel file and write them as CSV files
#' Used in development mode for .csv file creation from a source .xlsx file.
#' @param xlsx_path Path to the Excel file
#' @param source_sheets A vector of sheet names to read
#' @param csv_suffix Suffix of the created .csv files. Defaults to "master_thesis_cala".
#' @param temp_data_folder_path Folder path with the temporary data. Defaults to './data/temp/'.
#' @return A list of data frames
readExcelAndWriteCsv <- function(xlsx_path, source_sheets, csv_suffix = "master_thesis_cala",
                                 temp_data_folder_path = './data/temp/') {
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
    dfs <- lapply(source_sheets, function(sheet_name) {
      file_csv_path <- paste0(sheet_name, "_", csv_suffix, ".csv")
      new_data_path <- paste0(temp_data_folder_path, file_csv_path) # Store in data folder under temp folder
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


#' identifyCsvSeparators
#' 
#' A rather simple function for infering separator and decimal marks
#' from a csv file.
identifyCsvSeparators <- function(source_path){
  if (!file.exists(source_path)){
    stop(paste("The", source_path, "file not found."))
  }
  # Read the first few lines of the data frame
  first_few_lines <- readLines(source_path, n = 20)
  # Identify header line (assuming it is the first non-empty line)
  header_line <- first_few_lines[which(nchar(trimws(first_few_lines)) > 0)][1]
  remaining_lines <- first_few_lines[first_few_lines != header_line]
  # Identify the first line after header that contains digits
  first_data_line <- ""
  for(line in remaining_lines) {
    if(all(grepl("\\D", strsplit(line, "")[[1]])) | nchar(trimws(line)) == 0) next
    else {
      first_data_line <- line
      break
    }
    stop("No rows with numeric values identified in the data. Error in reading data.")
  }
  # Infer decimal mark and grouping mark
  decimal_mark <- ifelse(grepl("\\.", first_data_line), ".", ",")
  grouping_mark <- ifelse(grepl(",", first_data_line), ",", ".")
  out_list <- list(decimal_mark = decimal_mark, grouping_mark = grouping_mark)
  return(out_list)
}

#' readDataCustom function
#'
#' This function reads data from a given source path, infers the decimal mark and grouping mark,
#' and checks if the data is read correctly. It specifically designed for files where data 
#' begins after several non-data lines. It assumes the first non-empty line as the header line.
#' The function then identifies the first line containing numeric values after the header line
#' to infer the decimal and grouping marks. Finally, it reads the data, verifies its integrity,
#' and returns it.
#'
#' @param source_path [character] A string that is the path of the file to be read.
#'
#' @return Returns a data frame if the data is read successfully, otherwise it stops 
#'         execution with an error message. 
#'
#' @examples
#' \dontrun{
#'   # To read a file, just pass the path of the file
#'   data <- readDataCustom("/path/to/your/data.txt")
#'   print(data)
#' }
#'
#' @seealso
#' \code{\link[utils]{read.delim}}, \code{\link[utils]{readLines}}
#'
#' @export
readDataCustom <- function(source_path, separators = NA){
  # Validate the file existence and infer the separators
  if (!file.exists(source_path)){
    stop(paste("The", source_path, "file not found."))
  }
  if (all(is.na(separators))){
    separators <- identifyCsvSeparators(source_path)
  }
  decimal_mark <- separators$decimal_mark
  grouping_mark <- separators$grouping_mark
  # Read data
  data_out <- read_delim(
    source_path,
    locale = locale(decimal_mark = decimal_mark,
                    grouping_mark = grouping_mark,
                    tz = "UTC"),
    show_col_types = FALSE # Quiet warnings
  )
  # Check if data is read correctly
  if (is.data.frame(data_out) && length(dim(data_out)) == 2) {
    print(paste("Data loaded successfully from the following source:", source_path))
  } else {
    stop("Error in reading data.")
  }
  # Return the data
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

#' Validate that data types are identical within each group
validateConsistentDataTypes <- function(input_var_list){
  stopifnot(
    all(
      c("group_category", "data_type") %in% colnames(input_var_list)
    )
  )
  prob_data <- input_var_list %>% 
    group_by(group_category) %>%
    summarise(n_distinct_data_type = n_distinct(data_type)) %>%
    filter(n_distinct_data_type > 1)
  if(nrow(prob_data) > 0){
    prob_groups <- prob_data$group_category
    prob_groups_verbose <- var_list$var_name_verbose[match(prob_groups, var_list$group_category)]
    message(paste(
    "All variables of the same group must have the same data type.",
    "These variables have mismatching data types.",
    paste(prob_groups_verbose, sep = '\n'),
    sep = "\n"
    ))
    stop("Mismatching data types within variable groups.")
  }
}

#' Validate that there are no dummy groups with only one variable (dummies must have at least 2)
validateDummySpecifications <- function(input_var_list){
  stopifnot(
    all(
      c("group_category", "data_type") %in% colnames(input_var_list)
    )
  )
  prob_data <- input_var_list %>%
    group_by(group_category) %>%
    filter(sum(data_type == "dummy") == 1)
  if (nrow(prob_data) > 0){
    prob_groups <- prob_data$group_category
    prob_groups_verbose <- var_list$var_name_verbose[match(prob_groups, var_list$group_category)]
    message(paste(
      "All dummy variable groups must have at least 2 distinct variable columns associated with them.",
      "For single columns, use 'int' type instead of 'dummy'.",
      "These variables are such single dummy columns:",
      paste(prob_groups_verbose, sep = '\n'),
      sep = '\n'
    ))
    stop("Single dummy columns identified.")
  }
}

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
  # Check different specifications
  validateConsistentDataTypes(input_var_list) # Consistent data types
  validateDummySpecifications(input_var_list) # No single dummy columns
  # Check effect summary statistics specifications
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
  # Validate that dummy columns contain only 1s and 0s
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
  # Check data values
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
  # Check if all columns of the first vector are in the second one and vice versa
  if(!all(varnames %in% expected_varnames) || !all(expected_varnames %in% varnames)){
    missing_from_var_list <- varnames[!varnames %in% expected_varnames]
    missing_from_data <- expected_varnames[!expected_varnames %in% varnames]
    message(
      paste(
        "Mismatching variable names. \n",
        "These variables are not a part of the variable list: ", 
        paste(missing_from_var_list, collapse = ", "), "\n",
        "These variables are not a part of the main data frame columns: ",
        paste(missing_from_data, collapse = ", "), "\n"
      )
    )
    stop("Mismatching variable names")
  }
  # Check for correct ordering
  if(!identical(varnames, expected_varnames)){
    problematic_indexes <- which(varnames != expected_varnames)
    message(
      paste(
        "The order of some columns in the data frame and the expected variable list is different. \n",
        paste("Problematic indexes and their column names: \n"),
        paste(
          problematic_indexes, 
          ": Data frame has '", varnames[problematic_indexes], 
          "' but expected variable list has '", 
          expected_varnames[problematic_indexes], "'.", 
          collapse = "\n"
        )
      )
    )
    stop("Ordering of some columns is not matching")
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
    } else if (handling_method == "foo") {
      column_type <- input_var_list$data_type[i]
      calculate_foo <- function(type, col_data) {
        interpolation_method <- switch(
          type,
          "float" = median(col_data, na.rm = TRUE),
          "int" = median(col_data, na.rm = TRUE),
          "dummy" = median(col_data, na.rm = TRUE),
          "perc" = mean(col_data, na.rm = TRUE),
          "category" = "missing",
          # return a default value or warning when the type does not match any cases
          {warning("Invalid type."); NULL}
        )
        return(interpolation_method)
      }
      input_data[[column_name]][is.na(column_data)] <- calculate_foo(column_type, column_data)
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
    message(paste(
      "These columns contain special characters. Please modify the columns so that there are no such characters.",
      paste(as.character(special_char_cols), collapse = "\n"),
      sep = "\n"
    ))
    stop("Invalid column names")
  }
  # Validate that there are no two same column names
  if (any(duplicated(colnames(input_data)))){
    duplicated_col <- colnames(input_data)[which(duplicated(colnames(input_data)))]
    message(paste(
      "Duplicate column values are not allowed.",
      "Modify the names of these columns:",
      paste(as.character(duplicated_col), collapse = "\n"),
      sep = "\n"
    ))
    stop("Duplicate columns.")
  }
  # Validate that no columns are static
  constant_columns <- apply(input_data, 2, function(col){length(unique(col)) == 1}) # Boolean vector with names
  if (any(constant_columns)){
    message(paste(
      "There are constant columns in your data. Make sure to remove these first.",
      "These columns have constant values:",
      paste(as.character(colnames(input_data)[constant_columns]), collapse = "\n"),
      sep= "\n"
    ))
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
      message(paste(
        "There are variables with perfect correlation in your data (1 or -1). Make sure to treat these variables.",
        "These variables have perfect correlation:",
        paste(capture.output(print(problematic_pairs)), collapse = "\n"),
        sep = "\n"
      ))
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
      problematic_rows <- which(!(rows_equal_to_1) & abs(row_sums - 1) > 0.005) # Eliminate marginal errors from interpolation
      if (length(problematic_rows) > 6){
        other_rows <- paste("and",length(problematic_rows) - 6,"other rows.")
      } else {
        other_rows <- ""
      }
      message(paste(
        "All percentage groups must sum up to 1.",
        "These variables do not fulfill that:",
        paste(group_var_names, collapse="\n"),
        "at rows:",
        paste(head(problematic_rows), collapse="\n"),
        other_rows,
        "These are all unique row sums for this variable category. One or more, but perhaps not all of them are the faulty rows.",
        paste(as.character(unique(row_sums)), collapse="\n"), 
        sep="\n"
      ))
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
    getMAVariablesDescriptionTableVerbose(desc_df, verbose = verbose)
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
      var_weighted_mean <- round(weighted.mean(input_effect_data, w = 1/input_study_size_data),3)
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

#' @title generateGroupColumn
#' 
#' @description
#' Using a subsetted dataframe var_data, generate a vector (referred to as the "group column")
#' that fully describes the grouping of the variables. One could think of this function as
#' an inverse function to "dummify", only the var_data can be of many types. A variables information
#' data frame is also necessary for the procedure.
#' 
#' @details
#' Make sure the var_data object only contains the variables you wish to generate the group column for.
#' Also, make sure these have the same data type and group category in the variable information df.
#' 
#' @param var_data [data.frame] A data frame with variables to generate the group column for.
#' @param input_var_list [data.frame] Variable information data frame.
#' 
#' @return group_col [vector] A vector that fully specifies the grouping of the var_data variables.
generateGroupColumn <- function(var_data, input_var_list){
  # Validate input
  stopifnot(
    is.data.frame(var_data),
    is.data.frame(input_var_list)
  )
  n_groups <- ncol(var_data)
  vars_to_use <- as.vector(colnames(var_data))
  # Get the verbose variable names
  vars_to_use_verbose <- as.vector(input_var_list$var_name_verbose[match(vars_to_use, input_var_list$var_name)])
  # Get the group_type
  group_type_vec <- as.vector(input_var_list$data_type[match(vars_to_use, input_var_list$var_name)])
  group_type <- unique(group_type_vec)
  if (length(group_type) != 1){
    stop("Incorrectly specified group type. Must be consistent for the variables to use.")
  }
  # Unlist if there is only one column
  if (n_groups == 1){
    stopifnot(ncol(var_data) == 1)
    var_data <- as.vector(unlist(var_data))
  }
  # Handle case of multiple columns
  if (group_type == "dummy" || n_groups > 1){
    if (n_groups < 2){
      stop("A dummy may never have only one column. Check the data validation function.")
    }
    if(any(!sapply(var_data, is.numeric))){
      message(paste("Only numeric values are allowed in case of multiple-valued variables.",
              "Invalid variables:",
              paste(vars_to_use, sep = '\n'),
              sep = '\n'))
      stop("Invalid variable values in the BPE graph function.")
    }
    # Vector of names of columns with highest value in each row
    colnames(var_data) <- vars_to_use_verbose # Might be broken with special characters?
    group_col <- apply(var_data, 1, function(x) names(x)[which.max(x)])
  } else if (all(var_data %in% c(0,1))){
  # Single 0/1 column
    group_col <- paste(vars_to_use_verbose, "=", as.vector(var_data))
  } else if (is.numeric(var_data)){
  # Numeric, non-0/1 column
    var_data <- as.vector(var_data) # A single column
    above_below_vec <- ifelse(var_data >= median(var_data), ">=", "<") # Above/below median
    group_col <- paste(vars_to_use_verbose, above_below_vec, median(var_data))
  } else if (is.character(var_data)){
    col_values <- as.integer(factor(var_data))
    group_col <- paste0(vars_to_use_verbose, ": ", col_values)
  } else {
    message(paste(
      "Your data is incorrectly specified. Check the values of variables:",
      paste(vars_to_use, sep = "\n"),
      sep = "\n"
      ))
    stop("Incorrect variable values in the BPE graphing function when constructing an auxiliary group column.")
  }
  return(group_col)
}


#' @param input_data [data.frame] A data frame containing the main data.
#' @param input_var_list [data.frame] A data frame with variable information.
#' @param prima_factors [numeric] A vector of numeric values specifying the variable groups to factor by.
#' If NULL, the function will stop and return an error message. Defaults to NULL.
#' @param prima_type [character] One of "density", "histogram", "automatic". Graph the graph either as densities, or
#'  using histograms. If set to automatic, the script will automatically determine the optimal type. Defaults to "automatic".
#' @param prima_hide_outliers [logical] If TRUE, outliers (equal to the outermost histogram bins) will be
#'  hidden in the graph. Defaults to TRUE.
#' @param prima_bins [numeric] Number of bins to use in the histogram. Defaults to 80.
#' @param theme [character] A string specifying the color theme for the plots. Defaults to "blue".
#' @param export_graphics [logical] A boolean value indicating whether or not to export the graphs as .png files.
#' Defaults to TRUE.
#' @param graphic_results_folder_path [character] A string representing the path to the folder
#' where the graphics should be saved. If export_graphics is TRUE and this parameter is NA,
#' the function will stop and return an error message. Defaults to NA.
#' @param prima_scale [numeric] A number specifying the scale for the exported .png graphics.
#' It affects both the width and the height of the graphics. Default is 6.
#' @param prima_legend_font_size [numeric] A number to represent the font size in the prima
#' facie graphs. Defaults to 18.
#'
#' @return [list] A list of ggplot objects representing the prima facie graphs.
#'
#' @examples
#' \dontrun{
#' # Assuming appropriate input data is available
#' prima_facie_graphs = getPrimaFacieGraphs(data, var_list, bpe_factors = c(1,2,3), theme = "blue",
#' export_graphics = TRUE, graphic_results_folder_path = "path/to/folder", prima_scale = 5)
#' }
#'
#' @seealso \code{\link{ggplot2}}
#' 
#' @export
getPrimaFacieGraphs <- function(input_data, input_var_list, prima_factors = NULL, prima_type = "density",
                                prima_hide_outliers = T, prima_bins = 80, theme = "blue",
                                export_graphics = T, graphic_results_folder_path = NA,
                                prima_scale = 3, prima_legend_font_size = 18){
  # Input validation
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.character(prima_type),
    is.logical(prima_hide_outliers),
    is.numeric(prima_bins),
    is.character(theme),
    is.logical(export_graphics),
    is.numeric(prima_scale),
    nrow(input_data) > 0, # At least some data
    prima_bins > 0
  )
  if (length(prima_factors) < 1){
    stop("You must specify at least one factor to group the BPE plots by.")
  }
  if (all(is.na(prima_factors))){
    stop("Please specify at least one non-NA BPE graph grouping factor.")
  }
  if (!all(is.numeric(prima_factors))){
    stop("All BPE graph factors must be numeric values spectifying the variable groups to factor by.")
  }
  if (!prima_type %in% c("density", "histogram", "automatic")){
    stop(paste("Please choose a BPE graph type from one of the following: \"density\", \"histogram\", \"automatic\""))
  }
  # Get the theme to use
  current_theme <- getTheme(theme) +
    getTopRightLegend(text_size = prima_legend_font_size)
  # Get the information about graphs to use
  clean_data <- input_data
  prima_names <- paste0("prima_facie_", seq(length(prima_factors))) # prima_facie_1, prima_facie_2,...
  prima_names_counter <- 1
  prima_graphs <- list()  
  for (prima_factor in prima_factors){
    prima_df <- copy(clean_data) # Each iteration with a clean dataset - sorting shuffles the data otherwise
    vars_to_use <- as.vector(input_var_list$var_name[input_var_list$group_category == prima_factor])
    # Construct an auxiliary group column
    var_data <- prima_df[,vars_to_use]
    group_col <- generateGroupColumn(var_data, input_var_list)
    # Assign the group column to the data
    if (length(group_col) != nrow(prima_df)){
      stop("Incorrect length of the generated group column in prima graphing.")
    }
    prima_df$group <- group_col
    # Hide outliers if desired
    bin_width <- diff(range(prima_df$effect)) / prima_bins
    if (prima_hide_outliers){
      x_min <- min(prima_df$effect) + bin_width
      x_max <- max(prima_df$effect) - bin_width
      prima_df <- prima_df[prima_df$effect < x_max & prima_df$effect > x_min,] # Outliers out
    }
    # Determine the optimal graph type
    if (prima_type == "automatic"){
      graph_type <- ifelse(length(unique(prima_df$group)) > 3, "density", "histogram")
    } else {
      graph_type <- prima_type
    }
    # Get custom colors for the current group of variables
    prima_palette <- getColors(theme, "prima_facie_graphs", submethod = graph_type)
    # Construct the graph
    if (graph_type == "histogram"){
      prima_graph <- ggplot(data = prima_df, aes(x = effect, y = after_stat(density), fill = group)) +
        geom_histogram(binwidth = bin_width, bins = bin_width) +
        scale_fill_brewer(palette = prima_palette)
    } else if (graph_type == "density"){
      prima_graph <- ggplot(data = prima_df, aes(x = effect, y = after_stat(density), color = group)) +
          geom_density(aes(x = effect), alpha = 0.2, linewidth = 1) +
          scale_color_brewer(palette = prima_palette)
    } else {
      stop("Incorrect graph specification")
    }
    # Add labs and theme
    prima_graph <- prima_graph + 
      labs(x = "Effect", y = "Density") + 
      current_theme
    # Drop the auxiliary group column
    prima_df$group <- NULL
    # Save the graph and its name
    current_prima_name <- prima_names[prima_names_counter]
    prima_graphs[[current_prima_name]] <- prima_graph
    prima_names_counter <- prima_names_counter + 1
  }
  # Print the output - do not reprint during cache runs
  for (prima_graph in prima_graphs){
    suppressWarnings(print(prima_graph))
  }
  # Export the graphs
  if (export_graphics){
    if (is.na(graphic_results_folder_path)){
      stop("You must specify a path to the graphic results folder.")
    }
    for (name in prima_names){
      # Check the path to the graph
      full_graph_path <- paste0(graphic_results_folder_path, name,'.png')
      hardRemoveFile(full_graph_path)
      # Fetch the graph object from the graphs list object and graph the object
      graph_object <- prima_graphs[[name]]
      suppressWarnings(
        ggsave(filename = full_graph_path, plot = graph_object,
               width = 800*prima_scale, height = 666*prima_scale, units = "px")
      )
    }
  }
  # Return the graphs
  return(prima_graphs)
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
    is.character(effect_name),
    is.logical(verbose)
  )
  if (!factor_by %in% colnames(input_data)){
    message(paste(factor_by, "is an invalid factor. Can not plot the box plot."))
    stop("Invalid box plot factor.")
  }
  # Plot variable preparation
  factor_levels <- rev(sort(unique(input_data[[factor_by]]))) # Dark magic - tells plot how to group y-axis
  factor_by_verbose <- gsub("_", " ", factor_by) # More legible y-axis label
  # Get the theme to use
  current_theme <- getTheme(theme)
  plot_colors <- getColors(theme, "box_plot")
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
       coord_flip() + # The dark speech of Mordor, let it be heard around every corner of Middle RRRth
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
#' @param add_zero [logical] If TRUE, always add 0 to the ticks
#' @param theme [character] Theme to use for the ticks
#' @return A list with two elements: "output_vec", a sorted numeric vector containing the generated tick values and the mean value,
#'         and "x_axis_tick_text", a character vector of the same length as "output_vec", 
#'         with "red" indicating the position of the mean value and "black" for all other positions.
generateFunnelTicks <- function(input_vec, add_zero = T, theme = "blue"){
  lower_bound <- input_vec[1]
  upper_bound <- input_vec[2]
  mean_value <- input_vec[3]
  
  ticks <- c(lower_bound, upper_bound) # Base ticks
  if (add_zero &&
      !0 %in% ticks &&
      0 > lower_bound &&
      0 < upper_bound) {
    ticks <- sort(c(ticks, 0))
  }
  current_tick <- ceiling(lower_bound / 10) * 10 # Closest number divisible by 10 higher than lower bound
  
  while (current_tick < upper_bound) {
    if (abs(current_tick - lower_bound) >= 2 &&
        abs(current_tick - upper_bound) >= 2 &&
        !current_tick %in% ticks) {
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
#' @param precision_to_log [logical] If TRUE, use log of precision. Defaults to FALSE.
#' @param effect_proximity [float] Cutoff point for the effect. See getOutliers() for more.
#' @param maximum_precision [float] Cutoff point for precision. See getOutliers() for more.
#' @param use_study_medians [bool] If TRUE, plot medians of studies instead of all observations.
#'  Defaults to FALSE.
#' @param add_zero [bool] If T, always add zero to the graph. Defaults to T.
#' @param theme [character] Theme to use. Defaults to "blue".
#' @param verbose [bool] If T, print out outlier information. Defaults to T.
#' @param export_graphics [bool] If TRUE, export the plot to png object into the graphics folder.
#'  Defaults to FALSE.
#' @param graph_scale [numeric] Scale for the output graph. Defaults to 3.
#' @param output_path [character] Full path to where the plot should be stored. Defaults to NA.
getFunnelPlot <- function(input_data, precision_to_log = F, effect_proximity=0.2, maximum_precision=0.2,
                          use_study_medians = F, add_zero = T, theme = "blue", verbose = T,
                          export_graphics = F, output_path = NA, graph_scale = 3){
  # Check input validity
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.logical(precision_to_log),
    is.numeric(effect_proximity),
    is.numeric(maximum_precision),
    is.logical(add_zero),
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
  
  # Get visual bounds and tick colors
  funnel_x_lbound <- min(funnel_data$effect)
  funnel_x_ubound <- max(funnel_data$effect)
  mean_x_tick <- mean(funnel_data$effect)
  # Generate and extract the info
  base_funnel_ticks <- c(funnel_x_lbound, funnel_x_ubound, mean_x_tick) # c(lbound, ubound, mean)
  funnel_visual_info <- generateFunnelTicks(base_funnel_ticks, add_zero = add_zero, theme = theme)
  funnel_ticks <- funnel_visual_info$funnel_ticks
  funnel_tick_text <- funnel_visual_info$x_axis_tick_text
  # Get the theme to use
  current_theme <- getTheme(theme, x_axis_tick_text = funnel_tick_text)
  point_color <- getColors(theme, "funnel_plot")
  vline_color <- ifelse(theme %in% c("blue", "green"), "#D10D0D", "#0d4ed1") # Make v-line contrast with the theme
  
  # Precision to log if necessary
  if (precision_to_log){
    funnel_data$precision <- log(funnel_data$precision)
  }
  
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
#' This function takes a list of multiple subsets, which represent the lower bound,
#' upper bound, information about whether to highlight the mean value, as well as t-statistics to highlight.
#' Using these, generate and return a list that contains the values of ticks to use, as well as their colors.
#'
#' @param input_list [list] A list containing the information about the t-stat bounds, desired t-stat values to
#'  highlight, and whether to highlight mean.
#' A numeric vector of length 5, containing the lower bound, upper bound, mean value, and the t-stats.
#' @param theme [character] Type of theme to use
#' @return A list with two elements: "output_vec", a sorted numeric vector containing the generated tick values, the mean value,
#'         and the t-statistics values, and "x_axis_tick_text", a character vector of the same length as "output_vec",
#'         with "red" indicating the positions of the t-statistics values, "darkoran
generateHistTicks <- function(input_list, theme = "blue") {
  # Validate input
  expected_input_names <- c("bounds", "mean", "t_stats")
  stopifnot(
    is(input_list, "list"),
    is.character(theme),
    all(expected_input_names %in% names(input_list)),
    length(input_list$bounds) == 2,
    length(input_list$mean) == 1
  )
  # Unlist the input list
  lower_bound <- input_list$bounds$lower_bound
  upper_bound <- input_list$bounds$upper_bound
  mean_info <- input_list$mean
  t_stats <- input_list$t_stats
  # Replace t-stats with arbitrary value if NA
  if (all(is.na(t_stats))){
    t_stats <- lower_bound
  }
  # Exclude lower or upper bound if they are closer than 2 to any of the t-statistics values
  ticks <- c()
  if (all(abs(lower_bound - t_stats) >= 2)){
    ticks <- c(ticks, lower_bound)
  }
  if (all(abs(upper_bound - t_stats) >= 2)){
    ticks <- c(ticks, upper_bound)
  }
  # Get the base vector with bounds and t_stats
  ticks <- c(ticks, t_stats)
  if (!is.na(mean_info)){
    ticks <- c(ticks, mean_info)
  }
  ticks <- unique(ticks) # Remove duplicit values
  base_ticks <- ticks # Save for for loop, unmodifiable
  
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
    # If not too close to bounds/t-stats/mean, add the tick to tick list
    if (all(abs(current_tick - base_ticks) >= 2)) {
      ticks <- c(ticks, round(current_tick, 2))
    }
    current_tick <- current_tick + step_length
  }
  
  # Sort the vector
  hist_ticks <- sort(ticks)
  
  # Create the color vector
  x_axis_tick_text <- rep("black", length(hist_ticks))
  t_stat_indexes <- as.vector(which(hist_ticks %in% t_stats))
  x_axis_tick_text[t_stat_indexes] <- ifelse(theme %in% c("blue", "green"), "red", "blue")
  if (!is.na(mean_info)){
    mean_index <- which(hist_ticks == mean_info)
    x_axis_tick_text[mean_index] <- ifelse(theme %in% c("blue", "green"), "darkorange", "darkgreen")
  }
  
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
#' @param highlight_mean Boolean, if TRUE, highlight the mean t-statistic of the data in the plot.
#' @param add_density Boolean, if TRUE, add a density line into the plot.
#' @param t_stats A vector with t-statistic values that should be highlighted in the plot.
#' @param theme Theme to use. Defaults to "blue".
#' @param verbose If TRUE, print out the plot. Defaults to TRUE.
#' @param export_graphics If TRUE, export the plot to a png object into the graphics folder. Defaults to TRUE.
#' @param output_path Full path to where the plot should be stored. Defaults to NA.
#' @param graph_scale Numeric, scale the graph by this number. Defaults to 6.
#' @return A histogram plot of the T-statistic values with density overlay and mean, as well as vertical
#'  lines indicating the critical values of a two-tailed T-test with a significance level of 0.05.
getTstatHist <- function(input_data, lower_cutoff = -120, upper_cutoff = 120, highlight_mean = T,
                         add_density = T, t_stats = c(-1.96, 1.96), theme = "blue", verbose = T,
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
  hist_mean <- ifelse(highlight_mean, mean(hist_data$t_stat), NA) # None if not to be highlighted
  base_hist_ticks <- list(
    bounds = list(
      lower_bound = hist_lbound,
      upper_bound = hist_ubound
    ),
    mean = hist_mean,
    t_stats = t_stats
  )
  # Generate and extract variable visual information
  hist_visual_info <- generateHistTicks(base_hist_ticks, theme = theme)
  hist_ticks <- hist_visual_info$hist_ticks
  hist_ticks_text <- hist_visual_info$x_axis_tick_text
  # Get the theme to use
  current_theme <- getTheme(theme, x_axis_tick_text = hist_ticks_text) # Tick text automatically theme adjusted
  fill_color <- getColors(theme, "t_stat_histogram", submethod = "main")
  # Get line colors
  mean_line_color <- ifelse(theme %in% c("blue", "green"), "darkorange", "darkgreen")
  tstat_line_color <- ifelse(theme %in% c("blue", "green"), "#D10D0D", "#0d4ed1") # Make v-line contrast with the theme
  # Construct the histogram
  quiet(
    t_hist_plot <- ggplot(data = hist_data, aes(x = t_stat, y = after_stat(density))) +
      geom_histogram(color = "black", fill = fill_color, bins = 80) +
      lapply(t_stats, function(v) geom_vline(aes(xintercept=v), color = tstat_line_color, linewidth = 0.5)) + 
      labs(x = "T-statistic", y = "Density") +
      scale_x_continuous(breaks = hist_ticks) + 
      current_theme 
  )
  # Add a mean line if desired
  if (!is.na(hist_mean)){
    t_hist_plot <- t_hist_plot +
            geom_vline(aes(xintercept = hist_mean), color = mean_line_color, linetype = "dashed", linewidth = 0.7)
  }
  # Add a density line if desired
  if (add_density){
    density_line_color <- getColors(theme, "t_stat_histogram", submethod = "density")
    t_hist_plot <- t_hist_plot + 
      geom_density(aes(x = t_stat), alpha = 0.2, color = density_line_color, linewidth = 1)
  }
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

#' Add significance marks (asterisks) to a coefficient. Input the coefficient and its
#' standard error and return that coefficient with the asterisks. Character is always returned,
#' although the input must be numeric.
#' 
#' @param coef [numeric] Coefficient.
#' @param se [numeric] Its standard error.
#' 
#' @return [character] The coefficient with asterisks. Returned as character.
add_asterisks <- function(coef, se) { # Switch does not really work here as far as I know
  stopifnot(
    is.numeric(coef),
    is.numeric(se)
  )
  # NA values or 0 values
  if (any(
    is.na(coef),
    is.na(se),
    coef == 0,
    se == 0
    )){
    return(coef)
  }
  tvalue <- coef / se
  if (tvalue > 2.58) {
    asterisks <- "***"
  } else if (tvalue > 1.96) {
    asterisks <- "**"
  } else if (tvalue > 1.645) {
    asterisks <- "*"
  } else {
    asterisks <- ""
  }
  new_value <- paste0(as.character(coef), asterisks)
  return(new_value)
}

#' Extract the four coefficients from linear test in the order
#' - Intercept, Intercept SE, Slope, Slope SE
#' 
#' @param coeftest_object Coeftest object from the linear test
#' @param nobs_total [numeric] Number of observations used to estimate the model. Usually the number
#'  of rows in the main data frame.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @return [vector] - Vector of len 4, with the coefficients
extractLinearCoefs <- function(coeftest_object, nobs_total, add_significance_marks = T, verbose_coefs=T){
  # Check validity of the coeftest object
  stopifnot(
    is.numeric(nobs_total),
    is.logical(add_significance_marks),
    is.logical(verbose_coefs),
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
  if (add_significance_marks){
    pub_bias_coef <- add_asterisks(pub_bias_coef, pub_bias_se)
    effect_coef <- add_asterisks(effect_coef, effect_se)
  }
  # Wrap the standard errors in parenthesis for cleaner presentation
  if (verbose_coefs){
    pub_bias_se <- paste0("(", pub_bias_se, ")")
    effect_se <- paste0("(", effect_se, ")")
  }
  # Group and return quietly
  lin_coefs <- c(pub_bias_coef, pub_bias_se, effect_coef, effect_se, nobs_total)
  invisible(lin_coefs)
}

###### PUBLICATION BIAS - FAT-PET (Stanley, 2005) ######

#' Run all the linear tests on data, and return a matrix of results.
#' These tests are ran: OLS, FE, BE, RE, Weighted OLS (by study size),
#'  Weighted OLS (by precision). You may also choose to add significance
#'  level asterisks to the final output.
#' 
#' @param data [data.frame] Input data
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
getLinearTests <- function(data, add_significance_marks = T) {
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(all(required_cols %in% names(data)))
  # Save the data length as information in the output
  total_obs <- nrow(data)
  ### OLS
  ols <- lm(formula = effect ~ se, data = data)
  ols_res <- coeftest(ols, vcov = vcovHC(ols, type = "HC0", cluster = c(data$study_id)))
  ols_coefs <- extractLinearCoefs(ols_res, total_obs, add_significance_marks)
  ### Fixed effects
  fe <- plm(effect ~ se, model = "within", index = "study_id", data = data)
  fe_res <- coeftest(fe, vcov = vcov(fe, type = "fixed", cluster = c(data$study_id)))
  # Calculate the intercept and extract the coefficients
  fe_intercept <- within_intercept(fe, vcov = function(fe) {vcov(fe, type = "fixed", cluster = c(data$study_id))})
  fe_effect_coef <- fe_intercept[[1]] # Coefficient
  fe_effect_se <- attr(fe_intercept, "se") # SE
  # Bind together the values into the existing coeftest object
  names(fe_res) <- c("Estimate", "Str. Error")
  new_row <- c(fe_effect_coef, fe_effect_se)
  new_fe_res <- rbind(new_row, fe_res)
  # Extract the coefficients
  fe_coefs <- extractLinearCoefs(new_fe_res, total_obs, add_significance_marks)
  ### Between effects
  be <- plm(effect ~ se, model = "between", index = "study_id", data = data)
  be_res <- coeftest(be, vcov = vcov(be, type = "fixed", cluster = c(data$study_id)))
  be_coefs <- extractLinearCoefs(be_res, total_obs, add_significance_marks)
  ### Random Effects
  re <- plm(effect ~ se, model = "random", index = "study_id", data = data)
  re_res <- coeftest(re, vcov = vcov(re, type = "fixed", cluster = c(data$study_id)))
  re_coefs <- extractLinearCoefs(re_res, total_obs, add_significance_marks)
  ### Weighted by number of observations per study
  ols_w_study <- lm(formula = effect ~ se, data = data, weight = (data$study_size*data$study_size))
  ols_w_study_res <- coeftest(ols_w_study, vcov = vcovHC(ols_w_study, type = "HC0", cluster = c(data$study_id)))
  ols_w_study_coefs <- extractLinearCoefs(ols_w_study_res, total_obs, add_significance_marks)
  ### Weighted by precision
  ols_w_precision <- lm(formula = effect ~ se, data = data, weight = c(data$precision*data$precision))
  ols_w_precision_res <- coeftest(ols_w_precision, vcov = vcovHC(ols_w_precision, type = "HC0", cluster = c(data$study_id)))
  ols_w_precision_coefs <- extractLinearCoefs(ols_w_precision_res, total_obs, add_significance_marks)
  # Combine the results into a data frame
  results <- data.frame(
    OLS = ols_coefs,
    FE = fe_coefs,
    BE = be_coefs,
    RE = re_coefs,
    OLS_weighted_study = ols_w_study_coefs,
    OLS_weighted_precision = ols_w_precision_coefs
  )
  rownames(results) <- c("Publication Bias", "(Standard Error)", "Effect Beyond Bias", "(Constant)", "Total observations")
  colnames(results) <- c("OLS", "Fixed Effects", "Between Effects", "Random Effects", "Study weighted OLS", "Precision weighted OLS")
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
#' @param nobs_total [numeric] Number of observations used to estimate the model. Usually the number
#'  of rows in the main data frame.
#' @param nobs_model [numeric] Number of observations that are associated with the particular model.
#'  Optional, defaults to "".
#' @param nonlinear_object Non-linear object from the linear test
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @param pub_bias_present [bool] If T, the method returns publication bias coefs too.
#'  Deafults to F.
#' @param verbose_coefs [bool] If F, return coefs as numeric. If F, return
#'  standard errors as strings wrapped in parentheses. Defaults to T.
#' @return [vector] - Vector of len 6, with the pub bias coefs, effect coefs, and two nobs information coefs.
extractNonlinearCoefs <- function(nonlinear_object, nobs_total, nobs_model = "",
                                  add_significance_marks = T, pub_bias_present = F, verbose_coefs=T, ...){
  # Input validation
  stopifnot(
    is.logical(add_significance_marks),
    is.logical(pub_bias_present),
    is.logical(verbose_coefs)
  )
  # Extract coefficients
  effect_coef <- round(as.numeric(nonlinear_object[1,1]), 3)
  effect_se <- round(as.numeric(nonlinear_object[1,2]), 3)
  if (add_significance_marks){
    effect_coef <- add_asterisks(effect_coef, effect_se)
  }
  if (pub_bias_present){
    pub_coef <- round(as.numeric(nonlinear_object[2,1]), 3)
    pub_se <- round(as.numeric(nonlinear_object[2,2]), 3)
    if (add_significance_marks){
      pub_coef <- add_asterisks(pub_coef, pub_se)
    }
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
    nonlin_coefs <- c(pub_coef, pub_se, effect_coef, effect_se, nobs_total, nobs_model) # First two for pub bias
  } else {
    nonlin_coefs <- c("", "", effect_coef, effect_se, nobs_total, nobs_model)
  }
  invisible(nonlin_coefs)
}

###### PUBLICATION BIAS - WAAP (Ioannidis et al., 2017) ######

getWaapResults <- function(data, ...){
  WLS_FE_avg <- sum(data$effect/data$se)/sum(1/data$se)
  WAAP_bound <- abs(WLS_FE_avg)/2.8
  WAAP_data <- data[data$se<WAAP_bound,] # Only adequatedly powered
  WAAP_reg <- lm(formula = effect ~ -precision, data = WAAP_data)
  WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(data$study_id)))
  nobs_total <- nrow(data)
  nobs_model <- nrow(WAAP_data)
  WAAP_coefs <- extractNonlinearCoefs(WAAP_reg_cluster, nobs_total, nobs_model, ...)
  invisible(WAAP_coefs)
}

###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######


getTop10Results <- function(data, ...){
  T10_bound <- quantile(data$precision, probs = 0.9) #Setting the 90th quantile bound
  T10_data <- data[data$precision>T10_bound,] # Only Top10 percent of obs
  T10_reg <- lm(formula = effect ~ -precision, data = T10_data) #Regression using the filtered data
  T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(data$study_id)))
  nobs_total <- nrow(data)
  nobs_model <- nrow(T10_data)
  T10_coefs <- extractNonlinearCoefs(T10_reg_cluster, nobs_total, nobs_model, ...)
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
#' @param theme Theme for the graphics. Defaults to "blue".
#' @param export_graphics If TRUE, export the STEM plot.
#' @param export_path Path to the export folder. Deafults to ./results/graphic.
#' @param graph_scale Numeric, scale the graph by this number. Defaults to 5.
#' @param ... Additional arguments to be passed to the \code{extractNonlinearCoefs} function
#' for formatting the output.
#'
#' @return A numeric vector containing the estimated coefficients for the STEM-based method
#' in the usual format.
#' 
#' @import stem_method_master_thesis_cala.R
getStemResults <- function(data, script_path, print_plot = T, theme = "blue", 
                           export_graphics = T, export_path = "./results/graphic",
                           graph_scale = 5, legend_pos = "topleft", ...){
  source(script_path) #github.com/Chishio318/stem-based_method
  
  stem_param <- c(
    10^(-4), # Tolerance - set level of sufficiently small stem to determine convergence
    10^3 # max_N_count - set maximum number of iteration before termination
  )
  
  # Estimation
  est_stem <- stem(data$effect, data$se, stem_param)$estimates # Actual esimation
  # Stem plot
  funnel_stem_call <- bquote(
    stem_funnel(data$effect, data$se, est_stem, theme = .(theme), legend_pos = .(legend_pos))
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
    png(stem_path, width = 403*graph_scale, height = 371*graph_scale, res = 250)
    eval(funnel_stem_call)
    dev.off()
  }
  # Save results
  nobs_total <- nrow(data)
  stem_coefs <- extractNonlinearCoefs(est_stem, nobs_total, ...)
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
  nobs_total <- nrow(data)
  hier_coefs <- extractNonlinearCoefs(hier_raw_coefs, nobs_total, ...)
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
  nobs_total <- nrow(data)
  sel_coefs <- extractNonlinearCoefs(estimates_mat, nobs_total, ...)
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
  nobs_total <- nrow(data)
  endo_kink_coefs <- extractNonlinearCoefs(estimates_mat, nobs_total, ...)
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
#' frame silently. You may also choose to add significance level asterisks into the final output.
#'
#' @param data The main data frame, onto which all the non-linear methods are then called.
#' @param script_paths List of paths to all source scripts.
#' @param add_significance_marks If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @param theme Theme for the graphics. Defaults to "blue".
#' @param export_graphics If TRUE, export various graphs into the graphics folder.
#' @param export_path Path to the export folder. Defaults to ./results/graphic.
#' @param graph_scale Numeric, scale the graph by this number. Defaults to 5.
#' @return A data frame containing the results of the non-linear tests, clustered by study.
getNonlinearTests <- function(input_data, script_paths, selection_params = NULL,
                              add_significance_marks = T, theme = "blue",
                              export_graphics = T, export_path = './results/graphic',
                              graph_scale = 5, stem_legend_pos = "topleft") {
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
    list(
      add_significance_marks = add_significance_marks, 
      pub_bias_present = T,
      verbose_coefs = T
    )
  )
  all_stem_params <- c(
    list(
      input_data,
      stem_script_path,
      print_plot = T,
      theme = theme,
      export_graphics = export_graphics,
      export_path = export_path,
      graph_scale = graph_scale,
      legend_pos = stem_legend_pos,
      add_significance_marks = add_significance_marks,
      pub_bias_present = F,
      verbose_coefs = T
    )
  )
  # Get coefficients
  waap_res <- getWaapResults(input_data, add_significance_marks = add_significance_marks, pub_bias_present = F, verbose_coefs = T)
  top10_res <- getTop10Results(input_data, add_significance_marks = add_significance_marks, pub_bias_present = F, verbose_coefs = T)
  stem_res <- do.call(getStemResults, all_stem_params)
  hier_res <- getHierResults(input_data, add_significance_marks = add_significance_marks, pub_bias_present = T, verbose_coefs = T)
  sel_res <- do.call(getSelectionResults, all_selection_params)
  endo_kink_res <- getEndoKinkResults(input_data, endo_script_path, add_significance_marks = add_significance_marks, pub_bias_present = T, verbose_coefs = T)
  
  # Combine the results into a data frame
  results <- data.frame(
    waap_df = waap_res,
    top10_df = top10_res,
    stem_df = stem_res,
    hier_df = hier_res,
    sel_df = sel_res,
    endo_kink_df = endo_kink_res)
  
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)", "Total observations", "Model observations")
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
#' @param nobs_total [numeric] Number of observations used to estimate the model. Usually the number
#'  of rows in the main data frame.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
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
extractExoCoefs <- function(exo_object, total_obs, add_significance_marks = T,
                            effect_present = T, pub_bias_present = T, verbose_coefs=T){
  # Validate input
  stopifnot(
    is.numeric(total_obs),
    is.logical(add_significance_marks),
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
  # Add significance marks
  if (add_significance_marks){
    if (effect_present){
      effect_coef <- add_asterisks(effect_coef, effect_se)
    }
    if (pub_bias_present){
      pub_coef <- add_asterisks(pub_coef, pub_se)
    }
  }
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
  exo_coefs <- c(pub_coef, pub_se, effect_coef, effect_se, total_obs)
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
#' @param iv_instrument [character] Instrument to choose in the IV regression. If set to "automatic", determine the best
#' instrument automatically.
#' @inheritDotParams ... additional arguments to be passed to extractExoCoefs
#'
#' @return A list with a numeric vector containing the extracted coefficients from the IV regression
#' and the name of the instrument used.
#'
#' @details The function defines a list of instruments to use, and finds the best instrument
#' by running a function called findBestInstrument. If multiple best instruments are identified,
#' the function arbitrarily chooses the first one. The function then runs the IV regression and
#' extracts the coefficients using extractExoCoefs.
#'
#' @examples
#' data <- data.frame(effect = rnorm(10), se = rnorm(10), n_obs = rep(10, 10), study_id = rep(1:10, each = 1))
#' getIVResults(data)
getIVResults <- function(data, iv_instrument = "automatic", ...){
  # Determine the best instrument automatically
  if (iv_instrument == "automatic"){
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
  } else {
    if (!grepl("n_obs", iv_instrument)){
      stop("The chosen IV instrument must contain the column n_obs.")
    }
    best_instrument <- iv_instrument # Character
    best_instrument_values <- eval(parse(text = gsub("n_obs", "data$n_obs", best_instrument))) # Actual values
  }
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
  total_obs <- nrow(data)
  iv_coefs_out <- extractExoCoefs(iv_coefs_mat, total_obs, ...) 
  return(list(iv_coefs_out, best_instrument))
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
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
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
getPUniResults <- function(data, add_significance_marks = T, ...){
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
  # Add significance marks
  if (add_significance_marks && !is.na(est_effect_verbose)){
    est_effect_verbose <- add_asterisks(est_effect_verbose, est_se)
  }
  # Return as a vector
  total_obs <- nrow(data)
  p_uni_coefs_out <- c(
    est_pub_test_verbose,
    est_pub_p_val_verbose,
    est_effect_verbose,
    est_se_verbose,
    total_obs
  )
  return(p_uni_coefs_out)
}

#' getExoTests
#'
#' Performs two tests for publication bias and exogeneity in instrumental variable (IV) analyses using clustered data.
#'
#' @param input_data [data.frame] A data frame containing the necessary columns: "effect", "se", "study_id", "study_size", and "precision".
#' @param puni_params [list] Aruments to be used in p-uniform.
#' @param iv_instrument [character] Instrument to choose in the IV regression. If set to "automatic", determine the best
#' instrument automatically.
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#'
#' @details This function first validates that the necessary columns are present in the input data frame.
#' If the validation is successful, it performs three tests for publication bias and exogeneity in instrumental variable (IV)
#' analyses using clustered data: the IV test, and the p-Uniform test. The results of the two tests are combined
#' into a data frame, with row names corresponding to the tests and column names corresponding to the test type.
#' The results are then printed into the console and returned invisibly.
getExoTests <- function(input_data, puni_params, iv_instrument = "automatic", add_significance_marks = T) {
  # Validate that the necessary columns are present
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.character(iv_instrument),
    all(required_cols %in% names(input_data))
  )
  # Get arguments
  all_puni_params <- c(
    list(
      data = input_data,
      add_significance_marks = add_significance_marks
    ),
    puni_params
  )
  # Get coefficients
  iv_res_list <- getIVResults(input_data, iv_instrument = iv_instrument, add_significance_marks = add_significance_marks,
                              effect_present = T, pub_bias_present = T, verbose_coefs = T)
  iv_res <- iv_res_list[[1]] # Coefficients
  iv_best_instrument <- iv_res_list[[2]]
  p_uni_res <- do.call(getPUniResults, all_puni_params)
  # Combine the results into a data frame
  results <- data.frame(
    iv_df = iv_res,
    p_uni_df = p_uni_res)
  # Label names
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)", "Total observations")
  colnames(results) <- c("IV", "p-Uniform")
  # Print the results into the console and return
  out_list <- list(results, iv_best_instrument)
  getExoTestsVerbose(out_list)
  return(out_list) 
}

#' Verbose output for the getExoTests function
getExoTestsVerbose <- function(result_list, ...){
  # Unlist
  res <- result_list[[1]]
  best_instrument <- result_list[[2]]
  # Print out info
  print(paste("Instrument used in the IV regression:", best_instrument))
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
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @return A numeric vector with four elements: the estimate of the proportion of results reported, the standard error of the estimate,
#' the number of observations with t-statistics above the threshold, and the number of observations with t-statistics below the threshold.
runCaliperTest <- function(input_data, threshold = 1.96, width = 0.05, add_significance_marks = T){
  # Validate input
  required_cols <- getDefaultColumns()
  stopifnot(
    is.data.frame(input_data),
    is.numeric(threshold),
    is.numeric(width),
    is.logical(add_significance_marks),
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
  cal_est <- round(cal_res_coefs["t_stat", "Estimate"], 3) # Estimate
  cal_se <- round(cal_res_coefs["t_stat", "Std. Error"], 3) # Standard Error
  cal_above <- nrow(subsetted_data[subsetted_data$t_stat > threshold, ]) # N. obs above the threshold
  cal_below <- nrow(subsetted_data[subsetted_data$t_stat < threshold, ]) # N. obs below the threshold
  # Add significance marks if desired
  if (add_significance_marks){
    cal_est <- add_asterisks(cal_est, cal_se)
  }
  # Return the output
  res <- c(
    cal_est,
    cal_se,
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
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' 
#' @return A data frame with dimensions nrow = length(widths) * 3 and ncol = length(thresholds),
#'         where the rows are named with the caliper width and its estimate, standard error, and n1/n2 ratio,
#'         and the columns are named with the corresponding thresholds.
getCaliperResults <- function(input_data, thresholds = c(0, 1.96, 2.58), widths = c(0.05, 0.1, 0.2),
                              verbose = T, add_significance_marks = T){
  # Validate the input
  stopifnot(
    is.data.frame(input_data),
    is.vector(thresholds),
    is.vector(widths),
    is.numeric(thresholds),
    is.numeric(widths),
    is.logical(verbose),
    is.logical(add_significance_marks)
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
      caliper_res <- runCaliperTest(input_data, threshold = thresholds[i], width = widths[j],
                                    add_significance_marks = add_significance_marks)
      result_df[j*3-2, i] <- caliper_res[1] # Estimate
      result_df[j*3-1, i] <- paste0("(", caliper_res[2], ")") # Standard Error
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
#' @param add_significance_marks [logical] If TRUE, calculate significance levels and mark these in the tables.
#'  Defaults to T.
#' @inheritDotParams Parameters for the extractExoCoefs function.
#' 
#' @import maive_master_thesis_cala.R
getMaiveResults <- function(input_data, script_path,
                            method = 3, weight = 0, instrument = 1, studylevel = 2,
                            verbose = T, add_significance_marks = T, ...){
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
    is.logical(verbose),
    is.logical(add_significance_marks)
  )
  # Subset data and rename columns
  input_data <- input_data[,c("effect", "se", "n_obs", "study_id")]
  colnames(input_data) <- c('bs', 'sebs', 'Ns', 'studyid')
  # Run the estimation
  MAIVE <- maive(dat=input_data,method=method,weight=weight,instrument=instrument,studylevel=studylevel)
  # Add significance marks if desired
  if (add_significance_marks){
    MAIVE$beta <- add_asterisks(MAIVE$beta, MAIVE$SE)
  }
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
    
#' @title Handle BMA setup parameters for multiple models
#' 
#' @description
#' Split the BMA parameter list into sub-lists, where each of these has values of
#'  length 1 and represents a single BMA model setup. The first model should always
#'  appear in index one of parameters with multiple values. For parameters with
#'  only one value, this value will be used for all models (sub-lists). In case
#'  there are not either 1 or n values specified for each parameter (where n is
#'  the number of models the user wishes to use), the code will throw an error.
#'  
#' @note
#' The function always returns a list, even if all parameters have a single value.
#'  This is so that the bma model main for loop can iterate over the results.
#'  
#' @param bma_params [list] A list with the "bma_param_" parameters from the 
#'  user parameter file.
#'
#' @return A list of lists, where each sub-list corresponds to a single BMA
#'  model setup.
handleBMAParams <- function(bma_params){
  adj_bma_params <- list() # Store results here
  param_counts <- unique(sapply(bma_params, length)) # Values per parameter
  if (length(param_counts) == 1){
    adj_bma_params[[1]] <- bma_params
  } else if (length(param_counts) == 2){
    model_count <- param_counts[!param_counts == 1]
    # Iterate over models parameter values - evaluate main model first with index 1
    for (i in 1:model_count){ 
      # Extract parameters of the current iteration model
      single_model_params <- lapply(bma_params, function(x){x[1]})
      adj_bma_params[[i]] <- single_model_params
      # Remove the last value for parameters with multiple values
      bma_params <- lapply(bma_params, function(x){
        if (length(x) > 1){
          return(x[-1]) # All but first element
        }
        return(x)
      })
    }
  } else {
    stop("You must provide one or n values for each BMA parameter. n can be any number, but all parameters must have 1 or n values.")
  }
  return(adj_bma_params)
}
 
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
  out_list <- list(res_object, vif_coefs, removed_groups, removed_groups_verbose, bma_formula)
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
    length(out_list) == 5 # Via the main function
  )
  # Extract function output
  vif_coefs <- out_list[[2]]
  removed_groups <- out_list[[3]]
  removed_groups_verbose <- out_list[[4]]
  bma_formula <- out_list[[5]]
  if (verbose_on){
    print("These are all the Variance Inflation Coefficients for this formula:")
    print(vif_coefs)
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
  # Check that there are no NAs in the model
  if (any(is.na(coef(BMA_reg_test)))){
    problematic_vars <- names(coef(BMA_reg_test))[which(is.na(coef(BMA_reg_test)))]
    message(paste(
      "There are some aliased coefficients in one of the suggested BMA model configurations.",
      "Check colinearity in the data, remove the correlated variables, or try changing the model.",
      "These are the problematic variables for the model:",
      paste(problematic_vars, collapse = ", "),
      "Note that the problem may lie elsewhere too, so removing these variables may not necessarily help.",
      sep='\n'
    ))
    stop("Aliased coefficients in a suggested BMA model.")
  }
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
#' @param scale_data [logical] If TRUE, scale the data onto the same scale. Defaults to T.
#' @param from_vector [logical] If True, the "variable_info" must be specified as a vector, otherwise
#'  as a data frame. Defaults to FALSE.
#' @param include_reference_groups [logical] If TRUE, add the reference groups to the data. Be very
#' careful, as this may create a dummy trap. Used when creating the descriptive table of all potential
#' BMA variables. Usable only when from_vector == FALSE. Defaults to FALSE.
#' @note When transforming/subsetting the data, there is a need to convert the data into a
#' data.frame object, otherwise the plot functions will not recognize the data types correctly
#' later on. The "bms" function works well even with a tibble, but the plots do not. RRRRRRR
getBMAData <- function(input_data, input_var_list, variable_info, scale_data = T, from_vector = T,
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
  
  # Standardize non-binary data onto similar and directly comparable scale
  if (scale_data){
    # Store source column names
    source_colnames <- colnames(bma_data)
    # Check whether the variables are binary or not
    is_binary <- function(x) {
      length(unique(x)) == 2
    }
    binary_cols <- sapply(bma_data, is_binary) # Boolean where TRUE if col is binary
    bma_data[, !binary_cols] <- lapply(bma_data[, !binary_cols], function(x){as.numeric(scale(x))}) # Scale non-binary cols
    # Restore column names
    colnames(bma_data) <- source_colnames
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
  dev.off() # Reset the graphics device
  # Actual estimation with inhereted parameters
  quiet(
    bma_model <- do.call(bms, all_bma_params)
  )
  return(bma_model)
}

#' Verbose output for the runBMA function
runBMAVerbose <- function(...){
}

#' Rename the BMA model names to their verbose form using the variable information
#' data frame. Input these two objects (BMA model and variable list DF) and return
#' the modified BMA model.
renameBMAModel <- function(bma_model, input_var_list){
  # Validate input
  stopifnot(
    class(bma_model) == "bma",
    is.data.frame(input_var_list)
  )
  # Rename the model names
  bma_names <- bma_model$reg.names
  idx <- match(bma_names, input_var_list$var_name)
  bma_names[!is.na(idx)] <- input_var_list$var_name_verbose[na.omit(idx)]
  bma_names[is.na(idx)] <- "Intercept"
  bma_model$reg.names <- bma_names
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
#' @param input_var_list [data.frame] A data frame with the variable information.
#' @param print_results [character] A character value indicating the level of result printing desired.
#'  Can be one of:
#'  * none - print nothing
#'  * fast - print only those results that do not take time to print
#'  * verbose - print all the fast results, plus extra information about the model
#'  * all - print all results, plots included (takes a long time)
#' @param adjustable_theme [logical] If TRUE, modify the plot colors to fit the theme. Defaults to FALSE.
#' @param theme [character] Theme for the two plots.
#' @param export_graphics [logical] If TRUE, export the graphs into the graphics folder. Defaults to TRUE.
#' @param export_path [character] Path to the export folder. Defaults to ./results/graphic.
#' @param graph_scale [numeric] Scale the corrplot graph by this number. Defaults to 1.
#'
#' @return A numeric vector containing only the BMA coefficients.
extractBMAResults <- function(bma_model, bma_data, input_var_list, print_results = "fast", adjustable_theme = F,
                              theme = "blue", export_graphics = T, export_path = "./results/graphic", graph_scale = 1){
  # Validate the input
  stopifnot(
    class(bma_model) == "bma",
    is.data.frame(bma_data),
    is.data.frame(input_var_list),
    is.character(print_results),
    is.logical(adjustable_theme),
    is.character(theme),
    print_results %in% c("none", "fast", "verbose", "all", "table"),
    is.logical(export_graphics),
    is.character(export_path),
    is.numeric(graph_scale)
  )
  # Rename the variables to verbose form
  bma_model <- renameBMAModel(bma_model, input_var_list)
  # Get verbose names for the bma correlation matirx too
  effect_verbose <- input_var_list$var_name_verbose[match("effect",input_var_list$var_name)]
  bma_matrix_names <- c(effect_verbose, bma_model$reg.names)
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
    # Get the plot theme
    if (adjustable_theme){
      color_spectrum <- getColors(theme, "bma")
    } else {
      color_spectrum <- c("red", "white", "blue") # Default
    }
    # Main plot
    main_plot_call <- bquote( # Evaluate the color spectrum directly because RRRR
      image(bma_model, col = .(color_spectrum), yprop2pip=FALSE,order.by.pip=TRUE,
          do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") # Takes time
    )
    # Model distribution
    dist_color_spectrum <- color_spectrum[color_spectrum != "white"] # Pop white
    bma_dist_call <- bquote(
      base::plot(bma_model, col = .(dist_color_spectrum))
    )
    # Corrplot
    bma_matrix <- cor(bma_data)
    dimnames(bma_matrix) <- lapply(dimnames(bma_matrix),function(x){bma_matrix_names}) # Rename
    bma_col<- colorRampPalette(color_spectrum) # Color palette
    corrplot_mixed_call <- quote( # Simple eval, works for some reason
      corrplot.mixed(bma_matrix, lower = "number", upper = "circle",
                     lower.col=bma_col(200), upper.col=bma_col(200),tl.pos = c("lt"),
                     diag = c("u"), tl.col="black", tl.srt=70, tl.cex=0.55,
                     number.cex = 0.5,cl.cex=0.8, cl.ratio=0.1) 
    )
  }
  # Print out plots (takes time)
  if (print_results == "all"){
    print("Printing out Bayesian Model Averaging plots. This may take some time...")
    eval(main_plot_call, envir = environment())
    eval(bma_dist_call, envir = environment())
    eval(corrplot_mixed_call, envir = environment())
  }
  # Return coefficients only
  if (!print_results == "none"){
    cat("\n\n")
  }
  if (export_graphics){
    # Get the model flagging information
    gprior <- bma_model$gprior.info$gtype
    mprior <- bma_model$mprior.info$origargs$mpmode
    # Paths
    validateFolderExistence(export_path)
    main_path <- paste0(export_path, "/bma_", gprior, "_", mprior, "_results.png")
    dist_path <- paste0(export_path, "/bma_", gprior, "_", mprior, "_dist.png")
    corrplot_path <- paste0(export_path, "/bma_", gprior, "_", mprior, "_corrplot.png")
    # Remove existing plots if they exist
    for (path in list(main_path, dist_path, corrplot_path)){
      hardRemoveFile(path)
    }
    # Main plot
    png(main_path, width=933*graph_scale, height=894*graph_scale, units = "px",
        res=70*graph_scale)
    eval(main_plot_call, envir = environment())
    dev.off()
    # Model distribution
    png(dist_path, width = 528*graph_scale, height = 506*graph_scale, units = "px",
        res = 90*graph_scale)
    eval(bma_dist_call, envir = environment())
    dev.off()
    # Corrplot
    png(corrplot_path,
        width = 700*graph_scale, height = 669*graph_scale, units = "px",
        res = 90*graph_scale) # pointsize for text size
    eval(corrplot_mixed_call, envir = environment())
    dev.off()
  }
  return(bma_coefs)
}

#' Verbose output for the extractBMAResults function
extractBMAResultsVerbose <- function(...){
  # Todo
}

graphBMAComparison <- function(bma_models, input_var_list, theme = "blue", verbose = T, export_graphics = T,
                               export_path = "./results/graphic", graph_scale = 2){
  # Rename the BMA models to verbose
  bma_models <- lapply(bma_models, renameBMAModel, input_var_list = input_var_list)
  # Loop through the BMA objects and construct the partial call strings
  bma_model_calls <- list()
  for (i in 1:length(bma_models)){
    bma_model <- bma_models[[i]]
    # Get the model flagging information
    gprior <- bma_model$gprior.info$gtype
    mprior <- bma_model$mprior.info$origargs$mpmode
    prior_info_verbose <- paste(gprior,"and",mprior)
    # Construct and save the partial call
    model_call <- sprintf('"%s"=bma_models[[%s]]', prior_info_verbose, i)
    bma_model_calls <- append(bma_model_calls, model_call)
  }
  # Construct the final call string
  call_str <- paste0("BMS::plotComp(", paste(bma_model_calls, collapse = ", "), ", add.grid=F, cex.xaxis=0.7)")
  graph_call <- parse(text = call_str)
  # Create an environment to evaluate the expression under
  # Plot the plot
  if (verbose){
    eval(graph_call, envir=environment())
  }
  # Export the plot
  if (export_graphics){
    # Export the results
    validateFolderExistence(export_path)
    main_path <- paste0(export_path, "/bma_comparison",".png")
    hardRemoveFile(main_path) # Remove the graph if it exists
    # Save the plot
    png(main_path, width=520*graph_scale, height=478*graph_scale, units = "px",
        res = 90*graph_scale)
    eval(graph_call, envir=environment())
    dev.off()
  }
  return(NULL)
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
runFMA <- function(bma_data, bma_model, input_var_list, verbose = T){
  # Validate input
  stopifnot(
    is.data.frame(bma_data),
    is.data.frame(input_var_list),
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
  # Rename the columns to their verbose form
  fma_names <- rownames(fma_res)
  idx <- match(fma_names, input_var_list$var_name)
  fma_names[!is.na(idx)] <- input_var_list$var_name_verbose[na.omit(idx)]
  fma_names[is.na(idx)] <- "Intercept"
  rownames(fma_res) <- fma_names
  # Print results
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
#'   explanation see the runBPE function). Defaults to FALSE.
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
        coef <- median(bma_data[input_data$study_id==study_id,bma_var]) # Actual data from the study - use median in case of varying data
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
        stopifnot(is.character(coef)) # Should never occur (non-numeric values autoamtically read as characters)
        if (!coef %in% allowed_characters){
          # Invalid bpe specification for this varaiable
          message(paste0(
            "Invalid BPE specification for the variable '", bma_var, "'. \n",
            "Current specification: '", coef,"'.\n",
            "Must be one of the following: ", 
            paste(allowed_characters, collapse = ", "), "."
          )
          )
          stop("Invalid BPE specification.")
        }
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

#' Get data for the Best practice estimate
#' 
#' Replace the raw data in the main data frame with the data used in BMA so that the
#' BMA coefficients are directly reproducible in the BPE form construction
getBPEData <- function(input_data, bma_data){
  stopifnot(
    all(sapply(bma_data, is.numeric))
  )
  # Keep source colnames
  source_colnames <- colnames(input_data)
  # Replace the columns of the input data with columns of bma data
  cols_to_replace <- colnames(input_data) %in% colnames(bma_data)
  input_data[,cols_to_replace] <- lapply(bma_data, function(x){as.numeric(x)})
  # Restore column names
  colnames(input_data) <- source_colnames
  return(input_data)
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
runBPE <- function(input_data, input_var_list, bma_model, bma_formula, bma_data,
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
  # Prone to errors, so use tryCatch instead
  bpe_se <- tryCatch({
    as.numeric(summary(bpe_glht)$test$sigma) # Extract output
  }, error = function(e){
    NA # Return NA instead
  })
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
  if ("all" %in% study_ids){
    study_ids <- seq(from = 0, to = max(input_data$study_id), by = 1)
    study_info_verbose <- F # Silence individual study message
  }
  # Loop through study ids
  for (study_id in study_ids) {
    study_name <- ifelse(study_id == 0,
                         "Author",
                         as.character(input_data$study_name[input_data$study_id == study_id][1]))
    # BPE estimation
    bpe_result <- runBPE(input_data, input_var_list, bma_model, bma_formula, bma_data, study_id,
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
  # Replace NAs with median SE
  if (all(is.na(res_df$ci_95_lower)) || all(is.na(res_df$ci_95_lower))){
    stop("All best-practice estimates yielded missing standard errors. Stopping the code.")
  }
  temp_df$ci_95_lower[is.na(temp_df$ci_95_lower)] <- median(temp_df$ci_95_lower, na.rm=T)
  temp_df$ci_95_higher[is.na(temp_df$ci_95_higher)] <- median(temp_df$ci_95_higher, na.rm=T)
  # Get an arbitrary formula to print out in case of verbose output
  bma_coefs <- coef(bma_model,order.by.pip= F, exact=T, include.constant=T) 
  bpe_formula <- constructBPEFormula(input_data, input_var_list, bma_data, bma_coefs,
                                     study_ids[1], include_intercept = TRUE, get_se = TRUE)
  # Return the output
  res <- list(bpe_df = res_df, bpe_formula = bpe_formula)
  if (verbose_output) {
    generateBPEResultTableVerbose(res, verbose_output = verbose_output)
  }
  return(res)
}


#' Verbose output for the generateBPEResultTable function
generateBPEResultTableVerbose <- function(res,...){
  args <- list(...)
  verbose_on <- args$verbose_output
  # Print verbose output
  if (verbose_on){
    cat('\n')
    print("The best practice formula used:")
    print(res$bpe_formula)
    cat('\n')
    print("Best practice estimate results:")
    print(res$bpe_df)
    cat("\n\n")
  }
}

#' This function checks if all available studies were used in the BPE run.
#' 
#' @param input_data [data.frame] A dataframe that includes a column named 'study_name' which contains
#' the names of all available studies.
#' @param bpe_df [data.frame] The BPE result dataframe where the row names should include the
#' study names from 'input_data' and an 'Author' row. These are the studies used in the BPE run.
#' @return [logical] Returns TRUE if all studies were used, and FALSE otherwise.
checkIfAllBPEStudiesUsed <- function(input_data, bpe_df){
  stopifnot(
    is.data.frame(input_data),
    is.data.frame(bpe_df)
  )
  all_studies <- unique(input_data$study_name)
  used_studies <- rownames(bpe_df)
  if (!"Author" %in% used_studies){
    return(FALSE)
  }
  # Pop the author
  author_idx <- which(used_studies == "Author")
  used_studies <- used_studies[-author_idx]
  if (!all(all_studies %in% used_studies)){
    return(FALSE)
  }
  return(TRUE)
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

#' Sort a data frame based on the estimates and grouping so that the estimates get
#'  sorted within group, but the grouping order of the whole dataset stays the same.
#'  In other words, shift rows within groups until estimates within all groups are sorted from lowest to highest.
#'  
#' @details The group column can either contain integers or characters. The values can not be NA. During the
#' algorithm run, all characters are converted into integer factors, and these are then used for sorting.
#' As for the logic, the algorithm uses two auxiliary vectors and a for loop to iterate over all rows of the
#' data frame. In each iteration, it identifies the correct final index of the current row, and assigns that
#' final index to one of the auxiliary vectors called miracle_index. This vector is finally used to sort
#' the data into the desired form.
#' 
#' @param df [data.frame] The data frame to be sorted. Must contain two numeric columns - estimate and group.
#' 
#' @return The sorted data frame.
miracleBPESorting <- function(df){
  # Validate input
  stopifnot(
    is.data.frame(df),
    all(c("estimate", "group") %in% colnames(df)),
    is.numeric(df$estimate),
    !any(is.na(df$group))
  )
  group_verbose_names <- copy(df$group) # Save the verbose names
  df$row_names <- rownames(df) # Rownames to col
  df$group <- as.integer(factor(df$group)) # Group, factored
  original_group <- copy(df$group) # Save the original grouping (factored)
  # stopifnot(all)
  # Create a new column that stores the sorted rank within each group
  df <- df %>%
    group_by(group) %>%
    mutate(rank_within_group = row_number(estimate)) %>%
    ungroup()
  # Sort the data frame using the new column
  df <- df %>%
    arrange(group, rank_within_group)
  df$original_group <- original_group
  # Initialize two objects to store information for the algorithm
  miracle_index <- rep(NA, nrow(df))
  group_scale_vector <- rep(1, length(unique(df$original_group))) # A vector for storing placement index values
  # The miracle algorithm
  for(i in 1:nrow(df)){
    current_group <- df$original_group[i] # Group for this iteration 
    current_rank_within_group <- group_scale_vector[current_group] # Within group rank of the row to fetch
    desired_row_index <- which(df$group == current_group & df$rank_within_group == current_rank_within_group) # Index of the row to fetch
    miracle_index[desired_row_index] <- i # Assign the index to the next expected value (lowest of current group)
    group_scale_vector[current_group] <- current_rank_within_group + 1 # Increase the expected within group rank for next iteration
  }
  # Sort the data frame to its final order
  df$miracle_index <- miracle_index
  df <- df %>%
    arrange(miracle_index)
  # Drop redundant columns
  df$miracle_index <- NULL
  df$original_group <- NULL
  df$rank_within_group <- NULL
  df$group <- group_verbose_names # Back to verbose
  df <- column_to_rownames(df, var = "row_names") # Restore rownames
  # Validate correct sorting
  for (group_id in unique(df$group)){
    group_data <- as.vector(unlist(df[df$group == group_id,"estimate"]))
    group_is_sorted <- all(group_data == sort(group_data))
    if (!group_is_sorted){
      message(paste("The miracle algorithm broke down. Check sorting of group",group_id))
      stop("The miracle is over.")
    }
  }
  return(df)
}

#' An auxiliary method for adding a "group" column to the bpe data object. Used for 
#' graphing BPE graphs, and for generating the BPE summary statistics table. The output
#' of the function is the same dataframe as the inputted "bpe_df" object, only with an
#' extra "group" column.
addGroupColToBPEData <- function(bpe_df, vars_to_use, input_data, input_var_list){
  # Validate input
  stopifnot(
    is.data.frame(bpe_df),
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    all(colnames(bpe_df) == c("estimate", "ci_95_lower", "ci_95_higher"))
  )
  bpe_studies <- rownames(bpe_df)
  # Define a custom function that will allow the grouping to be done using mode (most frequent value)
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  # Group the main data to the same length as the BPE df
  var_data <- input_data %>%
    subset(study_name %in% bpe_studies) %>%
    select(c("study_name", all_of(vars_to_use))) %>%
    group_by(study_name) %>%
    summarise(across(everything(), getmode)) %>%
    select(-c("study_name")) # Drop the study_name col
  # Validate correct grouping
  expected_studies <- ifelse("Author" %in% bpe_studies, length(bpe_studies) - 1, length(bpe_studies))
  if (nrow(var_data) != expected_studies){
    stop("Incorrect grouping of variables in construction of the auxiliary group BPE graph column.")
  }
  # Generate the group column using a custom function
  group_col <- generateGroupColumn(var_data, input_var_list)
  # Put back the author's bpe
  if ("Author" %in% bpe_studies){
    author_idx <- which(bpe_studies == "Author")
    group_col <- append(group_col, "Author", after = author_idx - 1)
  }
  # Assign the group column to the data
  if (length(group_col) != nrow(bpe_df)){
    stop("Incorrect length of the generated group column in BPE graphing.")
  }
  bpe_df$group <- group_col
  return(bpe_df)
}



#' @title Graph Best Practice Estimates (BPEs)
#'
#' @description Create and export graphs of best practice estimates. These are grouped
#' by factors, and for every factor, a new plot is created. All of them are then plotted
#' and exported in a single list. If desired, these plots are also automatically saved
#' in the graphics folder.
#' 
#' @details For different types of factors, different grouping is employed. For dummies, a
#' graph is created for each variable (value) of that dummy. For categoric variables, it is
#' created for each category. For single numberic factors, median of that column is used to split
#' the data into two factors, and these are then graphed. Always, the author's best practice
#' estimate is highlighted in the graph.
#' Furthermore, the function employs a custom sorting algorithm to allow BPEs of different factors
#' to stand out from one another in the graph despite the lack of a usable x axis (in the current
#' form of the function, simple integers are used for x-axis indexing).
#' 
#' @param bpe_df [data.frame] A data frame containing the best practice estimates and their CIs.
#' @param input_data [data.frame] A data frame containing the main data.
#' @param input_var_list [data.frame] A data frame with variable information.
#' @param bpe_factors [numeric] A vector of numeric values specifying the variable groups to factor by.
#' If NULL, the function will stop and return an error message. Defaults to NULL.
#' @param graph_type [character] One of "density", "miracle". Graph the graph either as densities, or
#'  using the miracle sorting algorithm. Defaults to "density".
#' @param theme [character] A string specifying the color theme for the plots. Defaults to "blue".
#' @param export_graphics [logical] A boolean value indicating whether or not to export the graphs as .png files.
#' Defaults to TRUE.
#' @param graphic_results_folder_path [character] A string representing the path to the folder
#' where the graphics should be saved. If export_graphics is TRUE and this parameter is NA,
#' the function will stop and return an error message. Defaults to NA.
#' @param bpe_graphs_scale [numeric] A number specifying the scale for the exported .png graphics.
#' It affects both the width and the height of the graphics. Default is 6.
#'
#' @return [list] A list of ggplot objects representing the BPE graphs.
#'
#' @examples
#' \dontrun{
#' # Assuming appropriate input data is available
#' bpe_graphs = graphBPE(bpe_df, input_data, input_var_list, bpe_factors = c(1,2,3), theme = "blue",
#' export_graphics = TRUE, graphic_results_folder_path = "path/to/folder", bpe_graphs_scale = 5)
#' }
#'
#' @seealso \code{\link{ggplot2}}
#' 
#' @export
graphBPE <- function(bpe_df, input_data, input_var_list, bpe_factors = NULL, graph_type = "density", theme = "blue",
                     export_graphics = T, graphic_results_folder_path = NA, bpe_graphs_scale = 6
                     ){
  # Input validation
  stopifnot(
    is.data.frame(bpe_df),
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.character(graph_type),
    is.logical(export_graphics),
    is.numeric(bpe_graphs_scale),
    nrow(bpe_df) > 0 # At least some data
  )
  if (length(bpe_factors) < 1){
    stop("You must specify at least one factor to group the BPE plots by.")
  }
  if (all(is.na(bpe_factors))){
    stop("Please specify at least one non-NA BPE grouping factor.")
  }
  if (!all(is.numeric(bpe_factors))){
    stop("All BPE factors must be numeric values spectifying the variable groups to factor by.")
  }
  if (!graph_type %in% c("density", "miracle")){
    stop(paste("Please choose a BPE graph type from one of the following: \"density\", \"miracle\""))
  }
  # Shuffle the author BPE somewhere into the middle of the estimates
  author_row_bool <- rownames(bpe_df) == "Author"
  if (sum(author_row_bool) == 1){
    author_row <- bpe_df[author_row_bool,]
    author_rank <- match(author_row$estimate, sort(bpe_df$estimate)) # Rank among studies
    bpe_df <- bpe_df[!author_row_bool,] # Drop the author index row
    bpe_df <- rbind(bpe_df[1:(author_rank-1), ], author_row, bpe_df[author_rank:nrow(bpe_df), ]) # Insert into a new index
  }
  # Get the theme to use
  current_theme <- getTheme(theme) +
    getTopRightLegend(text_size = 16)
  mean_line_color <- ifelse(theme %in% c("blue", "green"), "darkorange", "darkgreen")
  tstat_line_color <- ifelse(theme %in% c("blue", "green"), "#D10D0D", "#0d4ed1") # Make v-line contrast with the theme
  # Get the information about graphs to use
  clean_bpe_df <- bpe_df
  bpe_names <- paste0("bpe_", seq(length(bpe_factors))) # bpe_1, bpe_2,...
  bpe_names_counter <- 1
  bpe_graphs <- list()  
  for (bpe_factor in bpe_factors){
    bpe_df <- copy(clean_bpe_df) # Each iteration with a clean dataset - sorting shuffles the data otherwise
    vars_to_use <- as.vector(input_var_list$var_name[input_var_list$group_category == bpe_factor])
    # Construct and add an auxiliary group column
    bpe_df <- addGroupColToBPEData(bpe_df, vars_to_use, input_data, input_var_list)
    # Get custom colors for the current group of variables
    bpe_palette <- getColors(theme, "bpe", submethod = graph_type)
    # Construct the graph
    if (graph_type == "miracle"){
      # Sort the data using the miracle sort algorithm so that estimates are sorted within groups
      bpe_df <- miracleBPESorting(bpe_df)
      bpe_graph <- ggplot(data = bpe_df, aes(x = seq(1, nrow(bpe_df)), y = estimate, color = group)) +
        geom_point() +
        geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = F) +
        labs(x = "Study id", y = "Best-practice estimate")
    } else if (graph_type == "density"){
      bpe_graph <- ggplot(data = subset(bpe_df, rownames(bpe_df) != "Author"),
                          aes(x = estimate, y = after_stat(density), color = group)) +
        geom_density(aes(x = estimate), alpha = 0.2, linewidth = 1) +
        labs(x = "Best-practice estimate", y = "Density")
    } else {
      stop("Incorrect graph specification")
    }
    # Add the theme
    bpe_graph <- bpe_graph +
      scale_color_brewer(palette = bpe_palette) +
      current_theme
    # Plot the plot
    suppressWarnings(print(bpe_graph))
    # Drop the auxiliary group column
    bpe_df$group <- NULL
    # Save the graph and its name
    current_bpe_name <- bpe_names[bpe_names_counter]
    bpe_graphs[[current_bpe_name]] <- bpe_graph
    bpe_names_counter <- bpe_names_counter + 1
  }
  # Export the graphs
  if (export_graphics){
    if (is.na(graphic_results_folder_path)){
      stop("You must specify a path to the graphic results folder.")
    }
    for (name in bpe_names){
      # Check the path to the graph
      full_graph_path <- paste0(graphic_results_folder_path, name,'.png')
      hardRemoveFile(full_graph_path)
      # Fetch the graph object from the graphs list object and graph the object
      graph_object <- bpe_graphs[[name]]
      suppressWarnings(
        ggsave(filename = full_graph_path, plot = graph_object,
               width = 800*bpe_graphs_scale, height = 666*bpe_graphs_scale, units = "px")
      )
    }
  }
  # Return the graphs
  return(bpe_graphs)
}

###### BPE SUMMARY STATISTICS ######

#' @title getBPESummaryStats
#' 
#' @description
#' Generate a table containing various summary statistics for those Best-practice estimates across
#' literature that are listed in the "bpe_df" object. Grouping factors are necessary to specify for which
#' variables these statistics should be printed, as well as for what confidence level. A data set of
#' the summary statistics is returned.
#' 
#' @details
#' The function employs several auxiliary functions, including an automatic splitting of estimates into groups
#' based values of a specified variable (see generateGroupColumn() and addGroupColumnToBPEData()).
#' 
#' 
#' @note
#' This function works very similarly in principle to the `getEffectSummaryStats()` function.
#'  Feel free to check that function's documentation as well.
#' 
#' @param bpe_df [data.frame] Data frame with the Best-practice estimates. Must contain the columns
#'  "estimate", "ci_95_lower', and "ci_95_higher"
#' @param input_data [data.frame] Main data frame.
#' @param input_var_list [data.frame] Variable information data frame.
#' @param bpe_factors [numeric] A vector of numeric values specifying the variable groups to factor by.
#' If NULL, the function will stop and return an error message. Defaults to NULL.
#' @param conf.level [numeric] Confidence level for the confidence interval. Must be between 0 and 1.
#' Defaults to 0.95
#' 
#' @return [data.frame] A data frame with the summary statistics
#' 
#' @export
getBPESummaryStats <- function (bpe_df, input_data, input_var_list, bpe_factors = NULL, conf.level = 0.95) {
  # Validate input
  stopifnot(
    is.data.frame(bpe_df),
    is.data.frame(input_data),
    is.data.frame(input_var_list),
    is.numeric(conf.level),
    conf.level > 0,
    conf.level < 1,
    all(colnames(bpe_df) == c("estimate", "ci_95_lower", "ci_95_higher"))
  )
  if (length(bpe_factors) < 1){
    stop("You must specify at least one factor to group the BPE plots by.")
  }
  if (all(is.na(bpe_factors))){
    stop("Please specify at least one non-NA BPE grouping factor.")
  }
  if (!all(is.numeric(bpe_factors))){
    stop("All BPE factors must be numeric values spectifying the variable groups to factor by.")
  }
  if (nrow(bpe_df) < 2){
    stop("There are not enough estimates to compute summary statistics. Please specify at least two studies to run the BPE for.")
  }
  # Constants
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE) # Z value for conf. int. calculation
  # Output columns
  bpe_stat_names <- c("Data subset", "Mean", "CI lower", "CI upper", "Median", "Min", "Max", "SD", "Studies")
  # Initialize output data frame
  df <- data.frame(col1 = character(),
                   col2 = numeric(),
                   col3 = numeric(),
                   col4 = numeric(),
                   col5 = numeric(),
                   col6 = numeric(),
                   col7 = numeric(),
                   col8 = numeric(),
                   col9 = numeric(),
                   stringsAsFactors = F
                   )
  stopifnot(ncol(df) == length(bpe_stat_names))
  # Add an auxiliary function for constructing new rows
  getNewDataRow <- function(bpe_df_subset, var_name, z){
    # Summary stats computation
    var_mean <- round(mean(bpe_df_subset$estimate), 3)
    var_sd <- round(sd(bpe_df_subset$estimate), 3)
    var_ci_lower <- round(var_mean - var_sd*z, 3)
    var_ci_upper <- round(var_mean + var_sd*z, 3)
    var_median <- round(median(bpe_df_subset$estimate), 3)
    var_min <- round(min(bpe_df_subset$estimate), 3)
    var_max <- round(max(bpe_df_subset$estimate), 3)
    var_obs <- nrow(bpe_df_subset)
    # Initialize and append a new row
    new_row <- data.frame(
      col1 = var_name,
      col2 = var_mean,
      col3 = var_ci_lower,
      col4 = var_ci_upper,
      col5 = var_median,
      col6 = var_min,
      col7 = var_max,
      col8 = var_sd,
      col9 = var_obs
    )
    return(new_row)
  }
  
  # Iterate over the desired factors and append new rows for each of them
  bpe_df <- bpe_df[rownames(bpe_df) != "Author",] # Do not use author, as the method is preocupied with literature instead
  clean_bpe_df <- bpe_df
  bpe_studies <- rownames(bpe_df)
  for (bpe_factor in bpe_factors){
    bpe_df <- copy(clean_bpe_df) # Each iteration with a clean dataset - sorting shuffles the data otherwise
    vars_to_use <- as.vector(input_var_list$var_name[input_var_list$group_category == bpe_factor])
    # Construct and add an auxiliary group column
    bpe_df <- addGroupColToBPEData(bpe_df, vars_to_use, input_data, input_var_list)
    # Add rows for each unique group of the group column
    for (var_name in unique(bpe_df$group)){
      bpe_df_sub <- bpe_df[bpe_df$group == var_name, ]
      new_row <- getNewDataRow(bpe_df_sub, var_name, z)
      df <- rbind(df, new_row)
    }
  }
  # Add a row on top of the data frame with all observations
  first_row <- getNewDataRow(bpe_df, "All data", z)
  df <- rbind(first_row, df)
  # Put the final output together
  colnames(df) <- bpe_stat_names
  # Print out verbose output and return the data frame
  getBPESummaryStatsVerbose(df)
  # Return data frame only
  return(df)
}

#' Verbose output for the getBPESummaryStatsVerbose function
getBPESummaryStatsVerbose <- function(res, ...){
  # Verbose output
  cat("Best-practice estimate summary statistics:\n")
  print(res)
  cat("\n")
}

######################### ROBUST BAYESIAN MODEL AVERAGING #########################

#' Run the main RoBMA method using the main data frame and return the results.
#' Automatically add significance marks if desired. Is able to inherit parameters
#' that are then fed into the RoBMA() function call. 
getRoBMA <- function(input_data, verbose, add_significance_marks = T, ...){
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
  rownames(robma_out$Estimates) <- c("Effect", "Standard Error")
  # Handle data types for easier object handling
  robma_out$Estimates <- as.data.frame(robma_out$Estimates)
  # Add significance marks - a bit wonky
  if (add_significance_marks){
    estimates_with_asterisks <- sapply(robma_out$Estimates, function(col){
      est <- round(col[1], 3)
      se <- round(col[2], 3)
      add_asterisks(est, se) # Return the asterisk'ed estimate
    })
    robma_out$Estimates["Effect",] <- as.vector(estimates_with_asterisks)
    robma_out$Estimates["Standard Error", ] <- round(as.numeric(robma_out$Estimates["Standard Error", ]), 3)
  }
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
    print("Model-averaged estimates:")
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
  # Save the parameters for cleaner code
  use_cache <- user_params$cache_handling$use_cache
  cache_folder <- user_params$folder_paths$cache_folder
  cache_age <- user_params$cache_handling$cache_age
  # Define the function to call based on cache information
  f <- cacheIfNeeded(f, use_cache, cache_folder, cache_age)
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
#' It should be present in the names of 'user_params$export_options$export_methods' and is used to construct the filename
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
    is.character(method_name)
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
  verbose_info <- user_params$export_options$export_methods[[method_name]] # Verbose name for the message
  verbose_info <- ifelse(is.null(verbose_info), method_name, verbose_info) # Use non-verbose info if verbose not available
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
  # Get today's date
  today <- Sys.Date()
  zip_date <- format(today, "%m-%d-%y")
  # Get arguments and paths
  folder_names <- as.vector(unlist(list(...))) # Folders to be zipped, character vector
  zip_file_path <- file.path(paste0(dest_folder, zip_name, "_", zip_date, ".zip"))
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

#' Get a list of themes that the script recognizes
getAvailableThemes <- function(){
  c("blue", "yellow", "green", "red", "purple")
}

#' Validate that a theme is available for use
validateTheme <- function(theme){
  available_themes <- getAvailableThemes()
  if (!is.character(theme)){
    message(paste(
      paste(theme,"is not a valid theme."),
      "Please choose one of the following themes:",
      paste(available_themes, collapse = ", "),
      sep = "\n"
    ))
  }
  if (!theme %in% available_themes){ # Loaded from source
    message(paste(theme, "is not a valid theme."))
    message("You must choose one of the following themes:")
    message(available_themes)
    stop("Invalid theme")
  }
}

#' Specify the type of theme to use and return the theme
#' 
#' Available choices - main, yellow, green, red
getTheme <- function(theme, x_axis_tick_text = "black"){
  # Validate the theme
  validateTheme(theme)
  # Get specific colors
  theme_color <- switch(theme,
    blue = "#DCEEF3",
    yellow = "#FFFFD1",
    green = "#D1FFD1",
    red = "#FFD1D1",
    purple = "#E6D1FF",
    stop("Invalid theme type.")
  )
  # Construct and return the theme
  theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
      axis.text.x = ggtext::element_markdown(color = x_axis_tick_text),
      axis.text.y = ggtext::element_markdown(color = "black"),
      panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = theme_color),
      plot.background = element_rect(fill = theme_color))
}

#' Return a theme object that moves the legend to the top right corner of the graph
getTopRightLegend <- function(text_size = 18){
    theme(
      legend.title = element_blank(), # No legend title
      legend.position = c(.98, .98), # Legend at the top right
      legend.justification = c("right", "top"), # To make sure the legend doesn't go outside the plotting area
      legend.direction = "vertical", # Stack legend text vertically
      legend.box = "vertical", # Set legend box direction
      legend.text = element_text(size = text_size), # Larger legend text
      legend.background = element_rect(colour = "grey", fill = "white") # Grey border around the legend
    )
}

getColors <- function(theme, method, submethod = NA, ...){
  validateTheme(theme)
  colors <- switch(method,
    prima_facie_graphs = switch(submethod,
      #\dontrun $display.brewer.all()
      histogram =  switch(theme, # Get a scale name instead
        blue = "Paired",
        yellow =  "YlOrRd",
        green =  "Set2",
        red =  "Reds",
        purple = "Purples",
        stop(paste("Invalid theme type", theme))
      ),
      density = switch(theme,
        blue = "Paired",
        yellow = "RdYlBu",
        green = "Paired",
        red = "RdBu",
        purple = "Purples",
        stop(paste("Invalid theme type", theme))
      ),
      stop(paste("Invalid submethod type:", submethod))
    ),
    box_plot = switch(theme,
      blue = list("#005CAB","#e6f3ff","#0d4ed1"),
      yellow = list("#AB9800","#FFF5CC","#D1B00D"),
      green = list("#009B0F","#CCF3D1","#0DD146"),
      red = list("#AB0000","#FFCCCC","#D10D0D"),
      purple = list("#6A0DAB","#EAD6F5","#900DAB"),
      stop(paste("Invalid theme type", theme))
    ),
    funnel_plot = switch(theme,
      blue = "#1261ff",
      yellow =  "#D1B00D",
      green =  "#00FF00",
      red =  "#FF0000",
      purple = "#800080",
      stop(paste("Invalid theme type", theme))
    ),
    t_stat_histogram = switch(submethod,
      main = switch(theme,
        blue = "#1261ff",
        yellow =  "#D1B00D",
        green =  "#00FF00",
        red =  "#FF0000",
        purple = "#800080",
        stop(paste("Invalid theme type", theme))
      ),
      density = switch(theme,
        blue = "#013091",
        yellow = "#b89b00",
        green = "#008000",
        red = "#b30000", 
        purple = "#660066",
        stop(paste("Invalid theme type", theme))
      ),
      stop(paste("Invalid submethod for t-statistic histogram:", submethod))
    ),
    bma = switch(theme,
      blue = c("#005CAB", "white", "#844ec7"),
      yellow = c("#AB9800", "white", "#009B0F"),
      green = c("#009B0F", "white", "#AB0000"),
      red = c("#AB0000", "white", "#6A0DAB"),
      purple = c("#6A0DAB", "white", "#005CAB"),
      stop(paste("Invalid theme type", theme))
    ),
    bpe = switch(submethod,
      #\dontrun $display.brewer.all()
      miracle =  switch(theme,
        blue = "Paired",
        yellow =  "YlOrRd",
        green =  "Set2",
        red =  "Reds",
        purple = "Purples",
        stop(paste("Invalid theme type", theme))
      ),
      density = switch(theme,
        blue = "Paired",
        yellow = "RdYlBu",
        green = "Paired",
        red = "RdBu",
        purple = "Purples",
        stop(paste("Invalid theme type", theme))
      ),
      stop(paste("Invalid submethod type:", submethod))
    ),
    stop(paste("Invalid method:", method))
  )
  return(colors)
}

#' Export the graph into an HTML file using the plotly package
#' 
#' @param graph_object The object generated by ggplot
#' @param export_path Full path to where the object should be stored.
exportHtmlGraph <- function(graph_object, export_path){
  plotly_img <- ggplotly(graph_object)
  htmlwidgets::saveWidget(plotly_img, export_path)
}