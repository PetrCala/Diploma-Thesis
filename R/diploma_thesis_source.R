##################### ENVIRONMENT PREPARATION ########################

#' DELETE IN PRODUCTION
#' 
#' Copy the existing excel data frame into a basic .csv file in
#'  the working directory
#'  
#' :args:
#'  xlsx_path [str] - Path to the main, already existing excel file.
#'  csv_path [str]- Path to where the new .csv file should be created.
copyMasterDF <- function(xlsx_path, csv_path){
  data_raw <- read_xlsx(xlsx_path, sheet = 'main')
  
  # Remove .
  data_raw[data_raw == '.'] <- NA
  
  write_csv(data_raw,
            csv_path)
  print(".csv data source file created in the working directory.")
}


#' Input a vector of file names, that should be located in the folder
#' of the main script, and validate that all are indeed present.
#' Print out a status message after the validation.
#' 
#' :args:
#'  files[vector] - A vector of strings.
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
loadPackages <- function(package_list){
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

#' Preprocess the raw excel data
#' 
#' Check column validity, add winsorized statistics (PCC, SE, t-stat)
#' :args:
#'  win_int [float] - Interval for winsirization. If 0.01, winsorize at 1%.
#'    Defaults to 0.01.
#' :return:
#'  [data.frame] - The preprocessed data
preprocessData <- function(input_data, win_level = 0.01){
  # Remove redundant rows
  while(is.na(input_data[nrow(input_data), "source"])) {
    input_data <- input_data[-nrow(input_data),]
  }
  
  # Column validity check
  expected_cols <- c('pcc', 'se_pcc', 't_stat')
  if (!all(expected_cols %in% colnames(input_data))) {
    print('There are missing columns in the data.')
    return(NA)
  }
  
  # Conver double to numeric
  #input_data <- input_data %>% mutate_all(~ ifelse(is.double(.), as.numeric(.), .))
  
  # Get the winsorization interval
  if (!(0 < win_level & win_level <1)){
    stop('Incorrect winsorization level. Choose a float between 0 and 1.')
  }
  win_int = c(win_level, 1-win_level) # c(0.01, 0.99)
  
  # Statistic preprocessing
  input_data$pcc_w <- Winsorize(x = input_data$pcc, minval = NULL, maxval = NULL, probs = win_int)
  input_data$se_pcc_w <- Winsorize(x = input_data$se_pcc, minval = NULL, maxval = NULL, probs = win_int)
  input_data$se_precision_w <- 1/input_data$se_pcc_w
  input_data$t_w <- Winsorize(x = input_data$t_stat, minval = NULL, maxval = NULL, probs = win_int)
  input_data$significant_w <- c(rep(0,nrow(input_data)))
  input_data$significant_w[(input_data$t_w > 1.96) | (input_data$t_w < -1.96)] <- 1
  
  return(input_data)
}

######################### DATA EXPLORATION #########################

#' Load the identifiers of various summary stats that should be used
#' during the data exploration.
loadSummaryStats <- function(){
  return(c(
    "schooling_years" = 1,
    "schooling_levels" = 1,
    "data_type_micro" = 1,
    "data_type_survey" = 1,
    "data_type_national_register" = 1,
    "data_cross_section" = 1,
    "data_panel" = 1,
    "gender_male1" = "gt0.5",
    "gender_male2" = "lt0.5",
    "sector_urban" = 1,
    "sector_rural" = 1
  ))
}


#' Input a data frame, and a vector of variables and their values for which to
#' load the summary statistics, and return a data frame of these summary statistics.
#' 
#' :args:
#'  data [data.frame] - The input data frame
#'  sum_stats [vector] - A vector in the form of a dictionary, such as
#'    c("var_name1" = "value1",
#'    "var_name2" = "value2")
#'    The values can also be prepended with "lt" (lower than) or
#'    "gt" (greater than), such as "var1" = "gt0.5".
#'  conf.level [float] - Confidence level for the confidence interval.
#'    Defaults to 0.95
#'  weight [bool] - If True, return weighted mean instead of a usual mean.
#'    Defaults to FALSE.
#'  
#'  :return:
#'    [data.frame] - A data frame with summary statistic for all the specified
#'      variables. These summary statistics are "mean", "SD", "lower CI bound",
#'      "upper CI bound", "number of observations"
getSummaryStats <- function (input_data, sum_stats, conf.level = 0.95, weight = FALSE) {
  stats_list <- c("Variable", "Mean", "SD",
                    "CI lower", "CI upper", "Obs") # Columns of the output df
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE) # Z value 
  df_rows <- length(sum_stats)
  df_cols <- length(stats_list)
  df_matrix <- matrix(nrow=df_rows, ncol=df_cols) #Temporary matrix
  
  for (i in names(sum_stats)) {
    name <- i
    value <- sum_stats[name]
    last_char <- substr(name, nchar(name), nchar(name))
    if (last_char %in% (c(1,2,3,4,5))) {
      name <- substring(name, 1, nchar(i)-1)
    }
    
    # Get display names
    if (grepl("lt", value, fixed=TRUE)) {
      value <- substr(value, 3, nchar(value))
      filter <- input_data[,name]<as.numeric(value)
      disp_name <- paste(name, "<", as.character(value))
    } else if (grepl("gt", value, fixed=TRUE)) {
      value <- substr(value, 3, nchar(value))
      filter <- input_data[,name]>as.numeric(value)
      disp_name <- paste(name, ">", as.character(value))
    } else {
      filter <- input_data[,name]==as.numeric(value)
      disp_name <- name
    }
    
    # Mean
    if (weight != TRUE) {
      xbar <- mean(input_data$pcc_w[filter]) #Simple mean
    } else {
      xbar <- weighted.mean(input_data$pcc_w[filter], #Weighted mean
                            w = c(input_data$study_size*input_data$study_size)[filter])
    }
    sdx <- sd(input_data$pcc_w[filter])   #SD
    conf_l <- xbar - z * sdx        #Confidence interval lower bound
    conf_u <- xbar + z * sdx        #Confidence interval upper bound
    obs <- sum(filter)              #Number of observations
    # Resulting vector
    out <- c(disp_name, round(xbar, 3), round(sdx, 3),
             round(conf_l, 3), round(conf_u, 3), obs)
    row_idx <- match(i, names(sum_stats))
    df_matrix[row_idx,] <- out
  }
  df <- data.frame(df_matrix) # Main output df
  colnames(df) <- stats_list # Rename columns
  return(df)
}


#' Identify outliers in the data, return the filter which can be used
#'  to get the data without these outliers.
#' 
#' :args:
#'  input_data - Data to check
#'  pcc_cutoff - Outlier cutoff point for the PCC
#'  precision_cutoff - Outlier cutoff point for the SE precision
#'  verbose - If true, print out information about the outliers
#'  
#' :return:
#'  [list] - Filter for the data without outliers
getOutliers <- function(input_data, pcc_cutoff = 0.2, precision_cutoff = 0.2, verbose=T) {
  # Check column validity
  expected_cols <- c('pcc_w', 'se_precision_w')
  if (!all(expected_cols %in% colnames(input_data))) {
    stop('Missing columns in the data set when trying to identify outliers.')
  }
  
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
  if ((!length(outliers) == 0) & (verbose)) {
    # Get the list of studies with outliers
    suspicious_studies <- c()
    for (outlier in outliers) {
      study <- as.character(input_data[outlier, 'source'])
      if (!study %in% suspicious_studies) {
        suspicious_studies <- c(suspicious_studies, study) # Add to the vector
      }
    }
    
    # Print out the information
    print(paste('Outliers found:', length(outliers)), sep=' ')
    print('Data rows:')
    print(outliers)
    print('Suspicious studies:')
    print(suspicious_studies)
  }
  
  # Return the negated filter
  return(!outlier_filter)
  
}

######################### GRAPHICS #########################

#' Custom ggplot theme
main_theme <- function(){
  theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
        plot.background = element_rect(fill = "#DCEEF3"))
}
