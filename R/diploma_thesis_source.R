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
  invisible(lapply(package_list, library, character.only = TRUE))
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
    validity_test <- any(c( # At least one of the following is not empty
      !is.na(temp_row$equal),
      !is.na(temp_row$gtlt)
      ))
    stopifnot(validity_test)
  }
    
  # Check data values
  float_data_to_check <- data_to_summarize[data_to_summarize$data_type %in% c('float','int'),]
  float_data_allowed_values <- c('MED', 'MEAN')
  for (i in 1:nrow(float_data_to_check)){
    temp_row <- float_data_to_check[i,]
    validity_test <- temp_row$gtlt %in% float_data_allowed_values
    stopifnot(all(validity_test))
  }
  
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
  
  # Other useful columns
  input_data$study_size <- ave(input_data$study_id, input_data$study_id, FUN = length) # Size of each study
  
  
  return(input_data)
}

winsorizeData <- function(input_data, win_level){
  # Validate input
  stopifnot(0 < win_level && win_level < 1)
  
  # Get the winsorization interval
  win_int <-  c(win_level, 1-win_level) # e.g. c(0.01, 0.99)
  
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

######################### DATA EXPLORATION #########################

getVariableSummaryStats <- function(input_data, input_var_list){
  # List of the statistics to compute
  variable_stat_names <- c("Variable Name", "Mean", "Median",
                           "SD", "Min", "Max")
  
  # Variables to preprocess
  desired_vars <- input_var_list[input_var_list$variable_summary == TRUE,]$var_name # Vector
  
  # Initialize output data frame
  df <- data.frame(matrix(nrow = length(desired_vars), ncol = length(variable_stat_names)))
  colnames(df) <- variable_stat_names
  
  # Iterate over all desired variables and append summary statistics to the main DF
  missing_data_vars <- c()
  for (var_name in desired_vars){
    var_data <- as.vector(unlist(subset(input_data, select = var_name))) # Roundabout way, because types
    row_idx <- match(var_name, desired_vars) # Append data to this row
    
    # Missing all data 
    if (!any(is.numeric(var_data), na.rm=TRUE)){
      missing_data_vars <- append(missing_data_vars, var_name)
      df[row_idx, ] <- c(var_name, rep(NA, length(variable_stat_names) - 1))
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
      var_mean,
      var_median,
      var_sd,
      var_min,
      var_max
    )
    df[row_idx, ] <- row_data
  }
  
  # Print and return output data frame
  cat("Variable summary statistics:\n")
  print(df)
  cat("\n")
  invisible(df)
}


#' Input a data frame, the variable list, and return a data frame of 
#'  these summary statistics.
#' 
#' @param input_data [data.frame] - The input data frame
#' @param input_var_list [data.frame] The variable list data frame.
#' @param conf.level [float] - Confidence level for the confidence interval.
#'    Defaults to 0.95
#' @param weight [bool] - If True, return weighted mean instead of a usual mean.
#'    Defaults to FALSE.
#' @return [data.frame] - A data frame with summary statistic for all the specified
#'      variables. These summary statistics are "mean", "SD", "lower CI bound",
#'      "upper CI bound", "number of observations"
getPCCSummaryStats <- function (input_data, input_var_list, conf.level = 0.95, weight = FALSE) {
  # Parameter checking
  stopifnot(all(c(conf.level > 0, conf.level < 1)))
  
  # Output columns
  pcc_stat_names <- c("Variable", "Mean", "Median", "Weighted Mean",
                     "SD", "CI lower", "CI upper", "Min", "Max", "Number of Outliers", "Obs")
  
  # Z value for confidence interval calculation 
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE) 
  
  # Initialize output data frame
  df <- data.frame(matrix(nrow = length(sum_stats), ncol = length(stats_list)))
  colnames(df) <- stats_list
  
  # Loop over each summary statistic 
  #for (i in names(sum_stats)) {
    #value <- sum_stats[i]
    
    ## Filter input data based on variable value
    #if (startsWith(value, "gt")) {
      #filter <- input_data[, i] > as.numeric(substr(value, 3))
      #disp_name <- paste(i, ">", substr(value, 3))
    #} else if (startsWith(value, "lt")) {
      #filter <- input_data[, i] < as.numeric(substr(value, 3))
      #disp_name <- paste(i, "<", substr(value, 3))
    #} else {
      #filter <- input_data[, i] == as.numeric(value)
      #disp_name <- i
    #}
    
    ## Calculate PCC_w summary statistics for various levels of the specified variables
    #obs <- sum(filter)
    #xbar <- if (weight) {
      #weighted.mean(input_data[filter, "pcc_w"], w = input_data[filter, "study_size"]^2)
    #} else {
      #mean(input_data[filter, "pcc_w"])
    #}
    #sdx <- sd(input_data[filter, "pcc_w"])
    #conf_l <- xbar - z * sdx
    #conf_u <- xbar + z * sdx
    
    ## Add results to output data frame
    #df[i, ] <- c(disp_name, round(xbar, 3), round(sdx, 3), round(conf_l, 3), round(conf_u, 3), obs)
  #}
  
  # Print and return output data frame
  cat("Summary statistics:\n")
  print(df)
  cat("\n")
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
  pcc_filter <- pcc_perc > pcc_cutoff
  precision_filter <- precision_perc > precision_cutoff
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
  validity_checks <- list(
    nrow(coeftest_object) == 2,
    ncol(coeftest_object) == 4,
    colnames(coeftest_object)[1] == "Estimate",
    colnames(coeftest_object)[2] == "Std. Error"
  )
  stopifnot(all(validity_checks))
  
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
  if(!all(required_cols %in% names(data))) {
    stop("Input data frame is missing necessary columns")
  }
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

getStemResults <- function(data, ...){
  source("stem_method_ext.R") #github.com/Chishio318/stem-based_method
  
  est_stem <- stem(data$pcc_w, data$se_pcc_w, param)$estimates # Actual esimation
  
  # Save results
  stem_coefs <- extractNonlinearCoefs(est_stem, ...)
  invisible(stem_coefs)
}


###### PUBLICATION BIAS - FAT-PET hierarchical in R ######

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

###### NON-LINEAR MODELS RESULTS ######

getNonlinearResults <- function(data) {
  
  # Validate that the necessary columns are present
  required_cols <- c("pcc_w", "se_pcc_w", "study_id", "study_size", "se_precision_w")
  if(!all(required_cols %in% names(data))) {
    stop("Input data frame is missing necessary columns")
  }
  # Get coefficients
  waap_res <- getWaapResults(data, pub_bias_present = F, verbose_coefs = T)
  top10_res <- getTop10Results(data, pub_bias_present = F, verbose_coefs = T)
  stem_res <- getStemResults(data, pub_bias_present = F, verbose_coefs = T)
  hier_res <- getHierResults(data, pub_bias_present = T, verbose_coefs = T)
  
  # Combine the results into a data frame
  results <- data.frame(
    waap_df = waap_res,
    top10_df = top10_res,
    stem_df = stem_res,
    hier_df = hier_res)
  
  rownames(results) <- c("Publication Bias", "(PB SE)", "Effect Beyond Bias", "(EBB SE)")
  colnames(results) <- c("WAAP", "Top10", "Stem", "Hierarch")
  # Print the results into the console
  print("Results of the non-linear tests, clustered by study:")
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
