# Install the necessary packages
if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi') # Working directory
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2') # Working directory

# Automatically set the working directory to the script's folder
if (! getwd() == dirname(getActiveDocumentContext()$path)){
  setwd(dirname(getActiveDocumentContext()$path)) # Set WD to the current file location
  print(paste0('Setting the working directory to: ', getwd()))
}

# Load the source scripts from within the same folder
source("new_weighted_mean.R")
source("new_weighted_mean_squared.R")

# TESTING AND COMPARING BOTH FUNCTIONS

# Define a function for timing a function call run
run_with_timer <- function(func, beta, se, sigma) {
  # Call a function with a timer
  start_time <- Sys.time()
  result <- func(beta, se, sigma)
  end_time <- Sys.time()
  # Calculate and print the elapsed time
  elapsed_time <- end_time - start_time
  print(paste("Time elapsed for", deparse(substitute(func)), ":", round(elapsed_time,4)))
  # Return the elapsed_time
  return(elapsed_time)
}

get_difference_factor <- function(old_function_time, new_function_time, function_name){
  difference <- as.numeric(old_function_time, units = "secs") / as.numeric(new_function_time, units = "secs")
  difference <- round(difference, 2)
  f_s_indicator <- ifelse(difference > 1, "faster", "slower")
  verbose_difference <- ifelse(difference > 1, difference, 1/difference)
  print(paste("The new",function_name,"function runs",verbose_difference, f_s_indicator, "than its source function."))
  return(difference)
}

test_a_function <- function(old_function, new_function, function_name, data_points){
  time_differences <- c()
  # Iterate over different sized data
  for (nobs in data_points){
    beta <- runif(nobs, -1, 1)  # a numeric vector of length 1000
    se <- runif(nobs, 0.1, 2)   # a numeric vector of length 1000
    sigma <- runif(1, 0, 5)     # a single random number
    
    # Call the Weighted Mean functions - no difference
    print(paste("Testing the function",function_name))
    cat('\n')
    print(paste("Data size:", nobs))
    time_source <- run_with_timer(old_function, beta, se, sigma) # Expected runtime with 1000obs: 0.1-0.5s
    time_new <- run_with_timer(new_function, beta, se, sigma) # Expected runtime with 1000obs: 0.1-0.5s
    time_difference <- get_difference_factor(time_source, time_new, function_name)
    cat('\n')
    
    # Save the time difference
    time_differences <- append(time_differences, time_difference)
  }
  return(time_differences)
}

get_a_frame <- function(time_differences, data_points, function_name){
  df <- data.frame(x = data_points, y = time_differences)
  colnames(df) <- c("Data points", "Improvement factor")
  print(paste("Runtime values of function",function_name,"for different data sizes:"))
  return(df)
}
  
get_a_plot <- function(time_differences, data_points, function_name){
  df <- data.frame(x = data_points, y = time_differences)
  plot <- ggplot(df, aes(x = data_points, y = time_differences)) +
    geom_point(color = "blue", size = 3) +
    labs(title = function_name,
         x = "Data points",
         y = "Function improvement factor") +
    theme_minimal()
  print(plot)
}

# Test both functions on different sbusets of data
data_points <- c(10,100,200, 300, 400, 500, 700, 800, 900, 1000, 1200, 1500)
wm_diffs <- test_a_function(weighted_mean_source, weighted_mean_new, "weighted mean",data_points)
wms_diffs <- test_a_function(weighted_mean_squared_source, weighted_mean_squared_new, "weighted mean squared", data_points)

# Print out a data frame with the function run times
wm_df <- get_a_frame(wm_diffs, data_points, "Weighted mean")
wms_df <- get_a_frame(wms_diffs, data_points, "Weighted mean squared")

# Print out graphs indicating the function run
get_a_plot(wm_diffs, data_points, "Weighted mean")
get_a_plot(wms_diffs, data_points, "Weighted mean squared")
