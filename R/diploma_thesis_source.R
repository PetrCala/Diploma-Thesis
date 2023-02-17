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

#' Identify outliers in the data, return the filter which can be used
#'  to get the data without these outliers.
#' 
#' :args:
#'  input_data - Data to check
#'  pcc_cutoff - Outlier cutoff point for the PCC
#'  precision_cutoff - Outlier cutoff point for the SE precision
#'  
#' :return:
#'  [list] - Filter for the data without outliers
getOutliers <- function(input_data, pcc_cutoff = 0.2, precision_cutoff = 0.2) {
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
  if (!length(outliers) == 0) {
    print(paste('Outliers found:', length(outliers)), sep=' ')
    print('Data rows:')
    print(outliers)
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
