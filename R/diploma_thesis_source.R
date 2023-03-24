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


####################### PACKAGE HANDLING ########################

#' Package loading function
#' 
#' Insert a vector/list of package names, install all missing ones,
#'  load all into workspace, and clean the environment
loadPackages <- function(package_list, load_quietly = F){
  # Install packages not yet installed
  installed_packages <- package_list %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("Installing package ", package_list[!installed_packages],"...", sep = ""))
    install.packages(package_list[!installed_packages])
  }
  # Package loading
  if (load_quietly){
    invisible(lapply(package_list, suppressWarnings(suppressMessages(library)), character.only = TRUE))
  } else {
    invisible(lapply(package_list, library, character.only = TRUE))
  }
  print('All packages loaded successfully')
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
  data_to_summarize <- input_var_list[input_var_list$pcc_sum_stats == TRUE, ]
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
#' 
#' 
#' Check column validity, add winsorized statistics (PCC, SE, t-stat)
#' @param win_int [float] Interval for winsirization. If 0.01, winsorize at 1%.
#'    Defaults to 0.01.
#' @return [data.frame] The preprocessed data
preprocessData <- function(input_data, input_var_list, win_level = 0.01){
  # Remove redundant columns
  expected_col_n <- nrow(var_list)
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
  
  # Winsorize the data using a custom function
  input_data <- winsorizeData(input_data, win_level)
  
  return(input_data)
}

winsorizeData <- function(input_data, win_level){
  # Validate input
  stopifnot(
    win_level > 0,
    win_level < 1,
    all(c('pcc', 'se_pcc') %in% colnames(input_data)),
    !(any(is.na(input_data$pcc))), # No missing PCC values
    !(any(is.na(input_data$se_pcc))) # No missing SE values
    )
  # Get the winsorization interval
  win_int <-  c(win_level, 1-win_level) # e.g. c(0.01, 0.99)
  # Create a t-stat column if it does not exist in the data
  if (!("t_stat") %in% colnames(input_data)){
    input_data$t_stat <- input_data$pcc / input_data$se_pcc
  }
  # Statistic preprocessing
  input_data$pcc_w <- Winsorize(x = input_data$pcc, minval = NULL, maxval = NULL, probs = win_int)
  input_data$se_pcc_w <- Winsorize(x = input_data$se_pcc, minval = NULL, maxval = NULL, probs = win_int)
  input_data$se_precision_w <- 1/input_data$se_pcc_w
  input_data$t_w <- Winsorize(x = input_data$t_stat, minval = NULL, maxval = NULL, probs = win_int)
  input_data$significant_w <- c(rep(0,nrow(input_data)))
  input_data$significant_w[(input_data$t_w > 1.96) | (input_data$t_w < -1.96)] <- 1
  # Return quietly
  invisible(input_data)
}

limitDataToOneStudy <- function(input_data, input_study_id){
  if (is.na(input_study_id)){
    return(input_data) # Do nothing
  } else if (is.logical(input_study_id)){
    stop("Invalid index for subsetting data. Use an integer.") # Boolean
  }
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
#' including mean, median, minimum, maximum, and standard deviation.
#' If a variable contains missing or non-numeric data, the corresponding summary statistics will be omitted.
#'
#' @param input_data [data.frame] The input data frame.
#' @param input_var_list [data.frame] A data frame with information about the variables to be summarized.
#' It should have columns "var_name", "data_type", and "variable_summary".
#'
#' @return [data.frame] A data frame containing summary statistics for selected variables.
getVariableSummaryStats <- function(input_data, input_var_list){
  # List of the statistics to compute
  variable_stat_names <- c("Var Name", "Var Class", "Mean", "Median",
                            "Min", "Max", "SD")
  # Variables to preprocess
  desired_vars <- input_var_list[input_var_list$variable_summary == TRUE,]$var_name # Vector
  # Initialize output data frame
  df <- data.frame(matrix(nrow = length(desired_vars), ncol = length(variable_stat_names)))
  colnames(df) <- variable_stat_names
  
  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  for (var_name in desired_vars){
    var_data <- as.vector(unlist(subset(input_data, select = var_name))) # Roundabout way, because types
    var_class <- input_var_list[input_var_list$var_name == var_name,]$data_type
    row_idx <- match(var_name, desired_vars) # Append data to this row
    # Missing all data 
    if (!any(is.numeric(var_data), na.rm=TRUE)){
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
    # Aggregate and append to the main DF
    row_data <- c(
      var_name,
      var_class,
      var_mean,
      var_median,
      var_min,
      var_max,
      var_sd
    )
    df[row_idx, ] <- row_data
  }
  # Print and return output data frame
  cat("Variable summary statistics:\n")
  print(df)
  cat("\n")
  print(paste0("Missing data for: ", length(missing_data_vars), " variables."))
  cat("\n")
  invisible(df)
}


#' The function getPCCSummaryStats() calculates the summary statistics for variables in a given data frame input_data
#'    using the percentage of correct classification (PCC) pcc_w and sample size study_size columns,
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
#' -Mean: The arithmetic mean of the PCC for the variable.
#' -Median: The median of the PCC for the variable.
#' -Weighted Mean: The weighted mean of the PCC for the variable, using the inverse squared sample size as weights.
#' -WM CI lower: The lower bound of the confidence interval for the weighted mean.
#' -WM CI upper: The upper bound of the confidence interval for the weighted mean.
#' -Min: The minimum PCC value for the variable.
#' -Max: The maximum PCC value for the variable.
#' -SD: The standard deviation of the PCC for the variable.
#' -Obs: The number of observations for the variable.
#' If a variable has missing or non-numeric data, it will not be included in the output.
#' If no variables are included in the output, the function returns an empty data frame.
getPCCSummaryStats <- function (input_data, input_var_list, conf.level = 0.95) {
  # Parameter checking
  stopifnot(all(c(conf.level > 0, conf.level < 1)))
  
  # Constants
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE) # Z value for conf. int. calculation
  pcc_data <- with(input_data, as.vector(pcc_w))
  study_size_data <- with(input_data, as.vector(study_size))
  
  # Output columns
  pcc_stat_names <- c("Var Name", "Var Class", "Mean", "Median", "Weighted Mean",
                     "WM CI lower", "WM CI upper", "Min", "Max", "SD", "Obs")
  
  # Variables to preprocess
  desired_vars <- input_var_list[input_var_list$pcc_sum_stats == TRUE,]$var_name # Vector
  
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
  stopifnot(ncol(df) == length(pcc_stat_names))
  
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
    if (!any(is.numeric(var_data), na.rm=TRUE)){
      missing_data_vars <- append(missing_data_vars, var_name)
      next
    }
    
    # Get the specifications and subset the data accordingly
    equal_val <- var_specs$equal
    gtlt_val <- var_specs$gtlt
    stopifnot(xor(is.na(equal_val),is.na(gtlt_val))) # Additional validity check - should never occur
    # The specification is EQUAL
    if (!is.na(equal_val)){
      pcc_data_equal <- pcc_data[var_data == equal_val]
      study_size_data_equal <- study_size_data[var_data == equal_val] # For W. mean - wonky, but straightforward
      cutoff <- equal_val  # For verbose output
    } else { # The specification is gtlt
      if (gtlt_val %in% c("MEAN", "MED")){
        cutoff <- ifelse(gtlt_val == 'MEAN', mean(var_data, na.rm=T), median(var_data, na.rm=T))
        pcc_data_gt <- pcc_data[var_data >= cutoff]
        pcc_data_lt <- pcc_data[var_data < cutoff]
        study_size_data_gt <- study_size_data[var_data >= cutoff]
        study_size_data_lt <- study_size_data[var_data < cutoff]
      } else if (!is.na(gtlt_val)){
        cutoff <- gtlt_val # For verbose output
        pcc_data_gt <- pcc_data[var_data >= gtlt_val]
        pcc_data_lt <- pcc_data[var_data < gtlt_val]
        study_size_data_gt <- study_size_data[var_data >= gtlt_val]
        study_size_data_lt <- study_size_data[var_data < gtlt_val]
      } else {
        stop("Value error")
      }
    }
    
    # A function for statistics calculation
    getNewDataRow <- function(input_var_name, input_class_name, input_pcc_data, input_study_size_data){
      input_pcc_data <- na.omit(input_pcc_data)
      input_study_size_data <- na.omit(input_study_size_data)
      # Summary stats computation
      var_mean <- round(mean(input_pcc_data), 3)
      var_median <- round(median(input_pcc_data), 3)
      var_weighted_mean <- round(weighted.mean(input_pcc_data, w = input_study_size_data^2),3)
      var_sd <- round(sd(input_pcc_data), 3)
      var_ci_lower <- round(var_weighted_mean - var_sd*z, 3)
      var_ci_upper <- round(var_weighted_mean + var_sd*z, 3)
      var_min <- round(min(input_pcc_data), 3)
      var_max <- round(max(input_pcc_data), 3)
      var_obs <- length(input_pcc_data)
      
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
      new_row <- getNewDataRow(new_varname_equal, var_class, pcc_data_equal, study_size_data_equal)
      df <- rbind(df, new_row)
    } else { # GTLT data
      new_varname_gt <- paste0(var_name_verbose, " >= ", round(as.numeric(cutoff,3)))
      new_varname_lt <- paste0(var_name_verbose, " < ", round(as.numeric(cutoff,3)))
      new_row_gt <- getNewDataRow(new_varname_gt, var_class, pcc_data_gt, study_size_data_gt)
      new_row_lt <- getNewDataRow(new_varname_lt, var_class, pcc_data_lt, study_size_data_lt)
      df <- rbind(df, new_row_gt)
      df <- rbind(df, new_row_lt)
    }
  }
  # Put the final output together
  colnames(df) <- pcc_stat_names
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
#' @param verbose [bool] - If T, print out the information about the plot being printed.
#'  Defaults to T.
getBoxPlot <- function(input_data, factor_by = 'country', verbose=T){
  # Check column validity
  expected_cols <- c('pcc_w', factor_by)
  stopifnot(all(expected_cols %in% colnames(input_data)))
  
  # Plot variable preparation
  factor_levels <- rev(sort(unique(input_data[[factor_by]]))) # Dark magic - tells plot how to group y-axis
  factor_by_verbose <- gsub("_", " ", factor_by) # More legible y-axis label
  
  # Construct the plot - use !!sym(factor_by) to cast some more dark magic - makes plot recognize function input
  box_plot <- ggplot(data = input_data, aes(x = pcc_w, y=factor(!!sym(factor_by), levels = factor_levels))) +
      geom_boxplot(outlier.colour = "#005CAB", outlier.shape = 21, outlier.fill = "#005CAB", fill="#e6f3ff", color = "#0d4ed1") +
      geom_vline(aes(xintercept = mean(pcc_w)), color = "red", linewidth = 0.85) + 
      labs(title = NULL,x="Estimate of the PCC between years of schooling and wage", y = "Grouped by " %>% paste0(factor_by_verbose)) +
      main_theme()
  
  # Plot the plot
  print(paste0('Printing a box plot for the factor: ', factor_by_verbose))
  suppressWarnings(print(box_plot))
  cat('\n')
}


#' Identify outliers in the data, return the filter which can be used
#'  to get the data without these outliers.
#' 
#' @param input_data Data to check
#' @param pcc_cutoff Outlier cutoff point for the PCC
#' @param precision_cutoff Outlier cutoff point for the SE precision
#' @param verbose If true, print out information about the outliers
#' @return [list] Filter for the data without outliers
getOutliers <- function(input_data, pcc_cutoff = 0.2, precision_cutoff = 0.2, verbose=T) {
  # Check column validity
  expected_cols <- c('pcc_w', 'se_precision_w')
  stopifnot(all(expected_cols %in% colnames(input_data)))
  
  # Get source values
  obs <- input_data$obs_n
  pcc <- input_data$pcc_w
  precision <- input_data$se_precision_w
  
  # Maximum values
  max_pcc <- max(pcc)
  max_precision <- max(precision)
  
  # Percentage of the maximum value - [0.2, 0.8, 0.7, ...]
  pcc_perc <- pcc/max_pcc
  precision_perc <- precision/max_precision
  
  # Create filters
  pcc_filter <- pcc_perc >= pcc_cutoff
  precision_filter <- precision_perc >= precision_cutoff
  outlier_filter <- pcc_filter & precision_filter
    
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

#' Input the main data frame, several specifications, and create a funnel plot
#' 
#' @param input_data [data.frame] Main data frame. Must contain cols 'pcc_w', 'se_precision_w'
#' @param pcc_cutoff [float] Cutoff point for PCC.
#' @param precision_cutoff [float] Cutoff point for precision.
#' @param verbose [bool] If T, print out outlier information. Defaults to T.
getFunnelPlot <- function(input_data, pcc_cutoff=0.2, precision_cutoff=0.2, verbose = T){
  # Check column validity
  expected_cols <- c('pcc_w', 'se_precision_w')
  stopifnot(all(expected_cols %in% colnames(input_data)))
  
  # Filter out the outliers
  filter_pcc_w <- getOutliers(input_data, pcc_cutoff=pcc_cutoff, precision_cutoff=precision_cutoff, verbose=verbose)
  
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

#' Generate a histogram of the T-statistic values for the given input data, with the 
#'  option to specify lower and upper cutoffs for filtering outliers.
#' 
#' @param input_data A data frame containing the T-statistic values to be plotted.
#' @param lower_cutoff An optional numeric value specifying the lower cutoff for filtering outliers. Default is -150.
#' @param upper_cutoff An optional numeric value specifying the upper cutoff for filtering outliers. Default is 150.
#' @return A histogram plot of the T-statistic values with density overlay and mean, as well as vertical
#'  lines indicating the critical values of a two-tailed T-test with a significance level of 0.05.
getTstatHist <- function(input_data, lower_cutoff = -150, upper_cutoff = 150){
  # Specify a cutoff filter
  t_hist_filter <- (data$t_w > lower_cutoff & data$t_w < upper_cutoff) #removing the outliers from the graph
  # Construct the histogram
  quiet(
    t_hist_plot <- ggplot(data = input_data[t_hist_filter,], aes(x = t_w[t_hist_filter], y = after_stat(density))) +
      geom_histogram(color = "black", fill = "#1261ff", bins = "80") +
      geom_vline(aes(xintercept = mean(t_w)), color = "dark orange", linetype = "dashed", linewidth = 0.7) + 
      geom_vline(aes(xintercept = -1.96), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = 1.96), color = "red", linewidth = 0.5) +
      labs(x = "T-statistic", y = "Density") +
      scale_x_continuous(breaks = c(-1.96, 1.96, round(mean(data$t_w),2), 50, 100, 130),
                         limits = c(-10, 130)) + 
      main_theme(
        x_axis_tick_text = c("red", "red", "darkorange", "black", "black", "black"))
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
  required_cols <- c("pcc_w", "se_pcc_w", "study_id", "study_size", "se_precision_w")
  stopifnot(all(required_cols %in% names(data)))
  # OLS
  ols <- lm(formula = pcc_w ~ se_pcc_w, data = data)
  ols_res <- coeftest(ols, vcov = vcovHC(ols, type = "HC0", cluster = c(data$study_id)))
  ols_coefs <- extractLinearCoefs(ols_res)
  # FE
  fe <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = data, method = "FE")
  fe_res <- coeftest(fe, vcov = vcov(fe, type = "fixed", cluster = c(data$study_id)))
  fe_coefs <- extractLinearCoefs(fe_res)
  # RE
  re <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = data, method = "REML")
  re_res <- coeftest(re, vcov = vcov(re, type = "fixed", cluster = c(data$study_id)))
  re_coefs <- extractLinearCoefs(re_res)
  # Weighted by number of observations per study
  ols_w_study <- lm(formula = pcc_w ~ se_pcc_w, data = data, weight = (data$study_size*data$study_size))
  ols_w_study_res <- coeftest(ols_w_study, vcov = vcovHC(ols_w_study, type = "HC0", cluster = c(data$study_id)))
  ols_w_study_coefs <- extractLinearCoefs(ols_w_study_res)
  # Weighted by precision
  ols_w_precision <- lm(formula = pcc_w ~ se_pcc_w, data = data, weight = c(data$se_precision_w*data$se_precision_w))
  ols_w_precision_res <- coeftest(ols_w_precision, vcov = vcovHC(ols_w_precision, type = "HC0", cluster = c(data$study_id)))
  ols_w_precision_coefs <- extractLinearCoefs(ols_w_precision_res)
  # Combine the results into a data frame
  results <- data.frame(
    OLS = ols_coefs,
    FE = fe_coefs,
    RE = re_coefs,
    OLS_weighted_study = ols_w_study_coefs,
    OLS_weighted_precision = ols_w_precision_coefs
  )
  rownames(results) <- c("Publication Bias", "(Standard Error)", "Effect Beyond Bias", "(Constant)")
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
  WLS_FE_avg <- sum(data$pcc_w/data$se_pcc_w)/sum(1/data$se_pcc_w)
  WAAP_bound <- abs(WLS_FE_avg)/2.8
  WAAP_reg <- lm(formula = pcc_w ~ -se_precision_w, data = data[data$se_pcc_w<WAAP_bound,])
  WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(data$study_id)))
  WAAP_coefs <- extractNonlinearCoefs(WAAP_reg_cluster, ...)
  invisible(WAAP_coefs)
}

###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######


getTop10Results <- function(data, ...){
  T10_bound <- quantile(data$se_precision_w, probs = 0.9) #Setting the 90th quantile bound
  T10_reg <- lm(formula = pcc_w ~ -se_precision_w, data = data[data$se_precision_w>T10_bound,]) #Regression using the filtered data
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
#' @import stem_method_custom.R
getStemResults <- function(data, ...){
  source("stem_method_custom.R") #github.com/Chishio318/stem-based_method
  
  est_stem <- stem(data$pcc_w, data$se_pcc_w, param)$estimates # Actual esimation
  
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
    y <- data$pcc_w[filter] #PCCs from the i-th study
    X <- cbind(1,
               data$se_pcc_w[filter])
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

#' Estimate the Selection Model and extract the coefficients for PCC and its SE
#'  - Source: https://maxkasy.github.io/home/metastudy/
#' 
#' This function computes selection model coefficients from input data using
#' the \code{metastudies_estimation} function from the \code{selection_model_custom.R}
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
#' @import selection_model_custom.R
getSelectionResults <- function(data, cutoffs = c(1.960),
                                symmetric = F, modelmu="normal", ...){
  # Read the source script
  source("selection_model_custom.R") 
  # Validate input
  stopifnot(all(cutoffs %in% c(1.645, 1.960, 2.576))) # Cutoffs
  stopifnot(modelmu %in% c("normal", "t")) # Model
  # Validate that the necessary columns are present
  required_cols <- c("pcc_w", "se_pcc_w")
  stopifnot(all(required_cols %in% names(data)))
  # Extract winsorized estimates, standard errors
  sel_X <- data$pcc_w # PCC - Winsorized
  sel_sigma <- data$se_pcc_w # SE - Winsorized
  
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
#'    Must contain the columns - "pcc_w", and "se_pcc_w"
#'  @inheritDotParams Parameters for the extractNonlinearCoefs function.
#'  
#'  @return endo_kink_coefs [vector] The four desired coefficients, which are:
#'    - Pub bias estimate
#'    - Pub bias standard error
#'    - Mean effect estimate
#'    - Mean effect standard error
#'
#' @import endo_kink_custom.R
#'
#'  Note - The runEndoKink method returns the coefficients in order mean_effect-pub_bias,
#'    this way is just for easier printing into the console, so be mindful of that.
getEndoKinkResults <- function(data, ...){
  # Read the source file
  source("endo_kink_custom.R")
  # Validate that the necessary columns are present
  required_cols <- c("pcc_w", "se_pcc_w")
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
  required_cols <- c("pcc_w", "se_pcc_w", "study_id", "study_size", "se_precision_w")
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
#' @param input_data [data.frame] A data frame containing the effect (pcc_w), its standard error (se_pcc_w), study ids, and source
#' data for the instrument(s) (specified as separate columns). It must have the columns "pcc_w", "se_pcc_w", "study_id", and "n_obs".
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
    all(c("pcc_w", "se_pcc_w", "study_id", "n_obs") %in% colnames(input_data))
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
    model <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = input_data)
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
  print(paste0("Identified ", best_instruments, " as the best instrument."))
  return(best_instruments)
}

#' getIVResults function
#'
#' This function takes in data and finds the best instrument for the IV regression of pcc_w against se_pcc_w.
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
#' data <- data.frame(pcc_w = rnorm(10), se_pcc_w = rnorm(10), n_obs = rep(10, 10), study_id = rep(1:10, each = 1))
#' getIVResults(data)
getIVResults <- function(data, ...){
  # Define the instruments to use
  instruments <- list(1/sqrt(data$n_obs), 1/data$n_obs, 1/data$n_obs^2, log(data$n_obs))
  instruments_verbose <- c('1/sqrt(data$n_obs)', '1/data$n_obs', '1/data$n_obs^2', 'log(data$n_obs)')
  # Find out the best instrument
  best_instrument <- findBestInstrument(data, instruments, instruments_verbose)
  # If more best instruments are identified
  if (length(best_instrument) > 1){ 
    best_instrument <- best_instrument[0] # Choose the first one arbitrarily
    print(paste0("More best instruments identified. Running the regression for ", best_instrument))
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
  model <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = data)
  model_summary <- summary(model, vcov = vcovHC(model, cluster = c(data$study_id)), diagnostics=T)
  # Get the coefficients
  all_coefs <- model_summary$coefficients
  IV_coefs_vec <- c(
    all_coefs["(Intercept)","Estimate"], # Effect
    all_coefs["(Intercept)", "Std. Error"], # Effect SE
    all_coefs["se_pcc_w", "Estimate"], # Pub Bias
    all_coefs["se_pcc_w", "Std. Error"] # Pub Bias SE
    ) 
  iv_coefs_mat <- matrix(IV_coefs_vec, nrow=2, ncol=2, byrow=TRUE)
  # Extract the coefficients and return as a vector
  iv_coefs_out <- extractExoCoefs(iv_coefs_mat, ...) 
  return(iv_coefs_out)
}

###### PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) ######

#' getMedians - Calculates the vector of medians for pcc_w
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
  med_pcc_w <- getMedians(data, 'pcc_w') # PCC vector of medians
  med_se_pcc_w <- getMedians(data, 'se_pcc_w') # SE vector of medians
  #Estimation
  if (method == "ML"){
    #Maximum likelihood
    quiet(
      est_ml <- puni_star(yi = med_pcc_w, vi = med_se_pcc_w^2, side = "right", method = "ML", alpha = 0.05,
                             control=list( max.iter=1000,tol=0.1,reps=10000, int=c(0,2), verbose=TRUE))
    )
  } else if (method == "P"){
    # Moments method estimation
    quiet(
      est_ml <- puni_star(yi = med_pcc_w, vi = med_se_pcc_w^2, side = "right", method = "P",
                          alpha = 0.05,control=list(max.iter=1000, tol=0.05, reps=10000, int=c(-1,1), verbose=TRUE))
    )
  } else {
    stop("Broken validity checks") # Should not happen
  }
  est_se <- (est_ml$ci.ub - est_ml$ci.lb) / (2*1.96) # Standard error of the estmiate
  # Extract and save coefficients - using a custom format for this method
  est_effect_verbose <- round(est_ml$est, 5) # Effect Beyond Bias
  est_se_verbose <- paste0("(", round(est_se, 5), ")") # Effect Standard Error
  est_pub_test_verbose <- paste0("L = ", round(est_ml$L.0, 5)) # Test statistic of p-uni publication bias test
  est_pub_p_val_verbose <- paste0("(p = ", round(est_ml$pval.0, 5), ")") # P-value for the L test statistic
  # Return as a vector
  p_uni_coefs_out <- c(
    est_pub_test_verbose,
    est_pub_p_val_verbose,
    est_effect_verbose,
    est_se_verbose
  )
  return(p_uni_coefs_out)
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
#' @import maive_custom.R
getMaiveResults <- function(data, method = 3, weight = 0, instrument = 1, studylevel = 0, verbose = T, ...){
  # Read the source file
  source("maive_custom.R")
  # Validate that the necessary columns are present
  required_cols <- c("pcc_w", "se_pcc_w", "study_size", "study_id")
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
              "Hausman-type test (to be used with caution)","Critical Value of Chi2(1)")
    maive_coefs_all<-c(MAIVE$beta,MAIVE$SE,MAIVE$`F-test`,MAIVE$Hausman,MAIVE$Chi2)
    MAIVEresults<-data.frame(object,maive_coefs_all)
    print(MAIVEresults)
  }
  # Extract for getExoTests
  maive_coefs_vec <- c(
    "", # Effect
    "", # Effect SE
    as.numeric(MAIVE$beta), # Pub Bias
    as.numeric(MAIVE$SE) # Pub Bias SE
    ) 
  maive_coefs_mat <- matrix(maive_coefs_vec, nrow=2, ncol=2, byrow=TRUE)
  # Extract the coefficients and return as a vector
  maive_coefs_out <- extractExoCoefs(maive_coefs_mat, ...)
  return(maive_coefs_out)
}

getExoTests <- function(input_data) {
  # Validate that the necessary columns are present
  required_cols <- c("pcc_w", "se_pcc_w", "study_id", "study_size", "se_precision_w")
  stopifnot(
    is.data.frame(input_data),
    all(required_cols %in% names(input_data))
  )
  # Get coefficients
  iv_res <- getIVResults(input_data, effect_present = T, pub_bias_present = T, verbose_coefs = T)
  p_uni_res <- getPUniResults(input_data, effect_present = T, pub_bias_present = T, verbose_coefs = T)
  maive_res <- getMaiveResults(input_data, verbose=F, effect_present = F, pub_bias_present = T, verbose_coefs = T)
  # Combine the results into a data frame
  results <- data.frame(
    iv_df = iv_res,
    p_uni_df = p_uni_res,
    maive_df = maive_res)
  # Label names
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)")
  colnames(results) <- c("IV", "p-Uniform", "MAIVE")
  # Print the results into the console
  print("Results of the tests relaxing exogeneity (and MAIVE), clustered by study:")
  print(results)
  cat("\n\n")
  # Return silently
  invisible(results) 
}

######################### GRAPHICS #########################

#' Custom ggplot theme
main_theme <- function(x_axis_tick_text = "black"){
  theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = x_axis_tick_text), axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
        plot.background = element_rect(fill = "#DCEEF3"))
}

