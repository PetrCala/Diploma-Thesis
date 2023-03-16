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
#' @param win_int [float] Interval for winsirization. If 0.01, winsorize at 1%.
#'    Defaults to 0.01.
#' @return [data.frame] The preprocessed data
preprocessData <- function(input_data, win_level = 0.01){
  # Remove redundant rows
  while(is.na(input_data[nrow(input_data), "study_name"])) {
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
  
  # Other useful columns
input_data$study_size <- ave(input_data$study_id, input_data$study_id, FUN = length) # Size of each study
  
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
#' @param data [data.frame] - The input data frame
#' @param sum_stats [vector] - A vector in the form of a dictionary, such as
#'    c("var_name1" = "value1",
#'    "var_name2" = "value2")
#'    The values can also be prepended with "lt" (lower than) or
#'    "gt" (greater than), such as "var1" = "gt0.5".
#' @param conf.level [float] - Confidence level for the confidence interval.
#'    Defaults to 0.95
#' @param weight [bool] - If True, return weighted mean instead of a usual mean.
#'    Defaults to FALSE.
#' @return [data.frame] - A data frame with summary statistic for all the specified
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
  if (!all(expected_cols %in% colnames(input_data))) {
    stop('Missing columns in the data set when trying to plot the Box-Plot.')
  }
  
  # Plot variable preparation
  factor_levels <- rev(sort(unique(input_data[[factor_by]]))) # Dark magic - tells plot how to group y-axis
  factor_by_verbose <- gsub("_", " ", factor_by)
  factor_by_verbose <- paste0(tolower(substr(factor_by_verbose,1,1)), substr(factor_by_verbose,2,nchar(factor_by_verbose))) # All to lower
  
  # Construct the plot - use !!sym(factor_by) to cast some more dark magic - makes plot recognize function input
  box_plot <- ggplot(data = input_data, aes(x = pcc_w, y=factor(!!sym(factor_by), levels = factor_levels))) +
      geom_boxplot(outlier.colour = "#005CAB", outlier.shape = 21, outlier.fill = "#005CAB", fill="#e6f3ff", color = "#0d4ed1") +
      labs(title = NULL,x="Estimate of the PCC between years of schooling and wage", y = "Grouped by " %>% paste0(factor_by_verbose)) +
      main_theme()
  
  # Plot the plot
  print(paste0('Printing a box plot for the factor: ', factor_by_verbose))
  suppressWarnings(print(box_plot))
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
      study <- as.character(input_data[outlier, 'study_name'])
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

#' Input the main data frame, several specifications, and create a funnel plot
#' 
#' @param input_data [data.frame] Main data frame. Must contain cols 'pcc_w', 'se_precision_w'
#' @param pcc_cutoff [float] Cutoff point for PCC.
#' @param precision_cutoff [float] Cutoff point for precision.
#' @param verbose [bool] If T, print out outlier information. Defaults to T.
getFunnelPlot <- function(input_data, pcc_cutoff=0.2, precision_cutoff=0.2, verbose = T){
  # Check column validity
expected_cols <- c('pcc_w', 'se_precision_w')
  if (!all(expected_cols %in% colnames(input_data))) {
    stop('Missing columns in the data set when trying to plot the Funnel plot.')
  }
  
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
  t_hist_filter <- (data$t_w > lower_cutoff & data$t_w < upper_cutoff ) #removing the outliers from the graph
  
  # Construct the histogram
  t_hist_plot <- ggplot(data = input_data[t_hist_filter,], aes(x = t_w[t_hist_filter], y = after_stat(density))) +
    geom_histogram(color = "black", fill = "#1261ff", bins = "80") +
    geom_vline(aes(xintercept = mean(t_w)), color = "dark orange", linetype = "dashed", size = 0.7) + 
    geom_vline(aes(xintercept = -1.96), color = "red", size = 0.5) +
    geom_vline(aes(xintercept = 1.96), color = "red", size = 0.5) +
    labs(x = "T-statistic", y = "Density") +
    #scale_x_continuous(breaks = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20), label = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20)) +
    main_theme()
  
  suppressWarnings(print(t_hist_plot)) # Print out the plot
}

######################### LINEAR TESTS ######################### 

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
  OLS <- lm(formula = pcc_w ~ se_pcc_w, data = data)
  OLS_id <- coeftest(OLS, vcov = vcovHC(OLS, type = "HC0", cluster = c(data$study_id)))
  
  # FE
  FE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = data, method = "FE")
  FE_c <- coeftest(FE, vcov = vcov(FE, type = "fixed", cluster = c(data$study_id)))
  
  # RE
  RE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = data, method = "REML")
  RE_c <- coeftest(RE, vcov = vcov(RE, type = "fixed", cluster = c(data$study_id)))
  
  # Weighted by number of observations per study
  OLS_w_study <- lm(formula = pcc_w ~ se_pcc_w, data = data, weight = (data$study_size*data$study_size))
  OLS_w_study_c <- coeftest(OLS_w_study, vcov = vcovHC(OLS_w_study, type = "HC0", cluster = c(data$study_id)))
  
  # Weighted by precision
  OLS_w_precision <- lm(formula = pcc_w ~ se_pcc_w, data = data, weight = c(data$se_precision_w*data$se_precision_w))
  OLS_w_precision_c <- coeftest(OLS_w_precision, vcov = vcovHC(OLS_w_precision, type = "HC0", cluster = c(data$study_id)))
  
  # Combine the results into a data frame
  results <- data.frame(
    OLS_clustered = OLS_id[, "Estimate"],
    FE_clustered = FE_c[, "Estimate"],
    RE_clustered = RE_c[, "Estimate"],
    OLS_weighted_study_clustered = OLS_w_study_c[, "Estimate"],
    OLS_weighted_precision_clustered = OLS_w_precision_c[, "Estimate"]
  )
  
  rownames(results) <- c("Intercept", "se_pcc_w")
  results <- t(results)
  
  # Print the results into the console
  print("Results of the linear tests:")
  print(results)
  
  # Return silently
  invisible(results) 
}


######################### NON-LINEAR TESTS ######################### 


###### PUBLICATION BIAS - WAAP (Ioannidis et al., 2017) ######

getWaapResults <- function(data, verbose = T){
  WLS_FE_avg <- sum(data$pcc_w/data$se_pcc_w)/sum(1/data$se_pcc_w)
  WAAP_bound <- abs(WLS_FE_avg)/2.8
  WAAP_reg <- lm(formula = pcc_w ~ -se_precision_w, data = data[data$se_pcc_w<WAAP_bound,])
  WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(data$study_id)))
  if (verbose){
    print(WAAP_reg_cluster)
  }
  WAAP_res <- WAAP_reg_cluster[1:2] # Manual coef extraction
  invisible(WAAP_res)
}

###### PUBLICATION BIAS - TOP10 method (Stanley et al., 2010) ######

getTop10Results <- function(data, verbose = T){
  T10_bound <- quantile(data$se_precision_w, probs = 0.9) #Setting the 90th quantile bound
  T10_reg <- lm(formula = pcc_w ~ -se_precision_w, data = data[data$se_precision_w>T10_bound,]) #Regression using the filtered data
  T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(data$study_id)))
  if (verbose){
    print(T10_reg_cluster)
  }
  T10_res <- T10_reg_cluster[1:2] # Manual coef extraction
  invisible(T10_res)
}


###### PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019) #####

getStemResults <- function(data, verbose = T){
  source("stem_method_ext.R") #github.com/Chishio318/stem-based_method
  
  est_stem <- stem(data$pcc_w, data$se_pcc_w, param) # Actual esimation
  
  # Save results
  stem_coefs <- est_stem$estimates[1:2] # Manual coef extraction
  stem_res <- matrix(NA, nrow = 2, ncol = 1)
  stem_res[,1] <- stem_coefs
  rownames(stem_res) <- c("Estimate", "Std. Error")
  colnames(stem_res) <- c("hier")
  if (verbose){
    print(stem_res)
  }
  invisible(stem_res)
}


###### PUBLICATION BIAS - FAT-PET hierarchical in R ######

getHierResults <- function(data, verbose = T){
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
    hier_coefs <- summary(out_h$Deltadraw)[1:2] # Quiet coef extraction
  )
  hier_res <- matrix(NA, nrow = 2, ncol = 1)
  hier_res[,1] <- hier_coefs
  rownames(hier_res) <- c("Estimate", "Std. Error")
  colnames(hier_res) <- c("hier")
  if (verbose){
    print(hier_res)
  }
  invisible(hier_res)
}

###### NON-LINEAR MODELS RESULTS ######

getNonlinearResults <- function(data) {
  
  # Validate that the necessary columns are present
  required_cols <- c("pcc_w", "se_pcc_w", "study_id", "study_size", "se_precision_w")
  if(!all(required_cols %in% names(data))) {
    stop("Input data frame is missing necessary columns")
  }
  
  waap_res <- getWaapResults(data, verbose = F)
  top10_res <- getTop10Results(data, verbose = F)
  stem_res <- getStemResults(data, verbose = F)
  hier_res <- getHierResults(data, verbose = F)
  
  # Combine the results into a data frame
  results <- data.frame(
    waap_df = waap_res,
    top10_df = top10_res,
    stem_df = stem_res,
    hier_df = hier_res)
 
  rownames(results) <- c("Intercept", "se_pcc_w")
  colnames(results) <- c("WAAP", "Top10", "Stem", "Hierarch")
  results <- t(results)
  
  # Print the results into the console
  print("Results of the non-linear tests:")
  print(results)
  
  # Return silently
  invisible(results) 
}


######################### GRAPHICS #########################

#' Custom ggplot theme
main_theme <- function(){
  theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
        plot.background = element_rect(fill = "#DCEEF3"))
}
