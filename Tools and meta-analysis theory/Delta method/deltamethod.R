# Packages
if (!require('car')) install.packages('car'); library('car')
if (!require('writexl')) install.packages('writexl'); library('writexl')

# Clear the environment
rm(list = ls())

# Static
excel_path <- "deltamethod_input.xlsx"
output_path <- "deltamethod_output.xlsx"
expected_cols <- c("study", "type", "years", "beta_higher",
                   "se_higher", "beta_lower","se_lower")


# Methods
readData <- function(path){
  if (!file.exists(path)){
    stop("Source file missing")
  }
  data <- readxl::read_xlsx(path)
  return(data)
}

validateData <- function(input_data){
  if (!all(expected_cols %in% colnames(input_data))){
    stop("Incorrect column names in the source file.")
  }
  if (!all(input_data$type %in% c(1,2))){
    stop("Data type must be either 1 or 2")
  }
  for (i in 1:nrow(input_data)){
    row <- input_data[i,]
    type <- row$type
    beta_lower <- row$beta_lower
    if (type == 2 & beta_lower == 999){
      message(paste("Incorrect type for row",i + 1))
      stop("Incorrect input type")
    }
  }
}

#' Input a row of the data frame and return the deltamethod SE for that row
getDeltaSE <- function(row){
  # First type of estimation
  if (row$type == 1){
    beta_est <- row$beta_higher
    y <- as.numeric(row$years)
    # The covariance matrix
    var_beta_est <- row$se_higher^2
    # Use the delta method 
    delta_out <- car::deltaMethod(c(beta_ = beta_est),
                            g. = "(1 + beta_)^(1 / y) - 1",
                            var_beta_est
    )
  } else {
    beta_higher_est <- row$beta_higher
    beta_lower_est <- row$beta_lower
    y <- as.numeric(row$years)
    # The covariance matrix
    cov_matrix <- diag(c(row$se_higher^2, row$se_lower^2))
    # Use the delta method
    delta_out <- car::deltaMethod(c(beta_higher = beta_higher_est, beta_lower = beta_lower_est),
                       g. = "(1 + beta_higher - beta_lower)^(1 / y) - 1",
                       cov_matrix)
  }
  # Handle missing values
  if (is.na(delta_out$Estimate)){
    browser()
  }
  out_list <- list(est = delta_out$Estimate, se = delta_out$SE)
  return(out_list)
}


# Main
data <- readData(excel_path)
validateData(data)
estimates <- c()
std_errors <- c()
for (i in 1:nrow(data)){
  row <- data[i,]
  delta_list <- getDeltaSE(row)
  estimates <- append(estimates, delta_list$est)
  std_errors <- append(std_errors, delta_list$se)
}

# Add calculated columns and columns in percentages
output_data <- cbind(data, estimates, std_errors)
output_data$estimates_perc <- output_data$estimates * 100
output_data$std_errors_perc <- output_data$std_errors * 100
output_data$tstat <- output_data$estimates_perc / output_data$std_errors_perc
# Print and save the output
print(head(output_data))
writexl::write_xlsx(output_data, output_path) # To excel