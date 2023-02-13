##################### ENVIRONMENT PREPARATION ########################

#Clean the environment
rm(list = ls()) 

# Load the source script
source("diploma_thesis_source.R")

# Static
master_data_set_path = "../Data/data_set_master_thesis_cala.xlsx" # Master data set in folder Data


####################### PACKAGE HANDLING ########################

# Required packages
packages <- c("readr", "tidyverse", "ggplot2", "readxl", "stats", "DescTools", "sandwich", "lmtest", "multiwayvcov",
              "metafor", "bayesm", "puniform", "haven", "meta", "AER", "BMS", "corrplot", "foreign", "xtable",
              "LowRankQP", "foreign", "multcomp", "data.table")

# Load packages
loadPackages(packages)


######################### DATA PREPROCESSING #########################

# Read the data set into the environment
data_raw <- read_xlsx(master_data_set_path, sheet = 'main')

# Data transformation
data <- copy(data_raw) # Make a deep copy
data <- preprocessData(data) # Validate, preprocess, and winsorize data


######################### DATA EXPLORATION #########################

#Summary statistics and the funnel plot for the winsorized data
#summary(data$pcc_w)
#summary(data$se_precision_w)

#Funnel plot

#filter_pcc_w <- (input_data$pcc_w> -0.38851 & input_data$pcc_w<0.525172) #Excluding the outliers from the graph
filter_pcc_w <- (data$pcc_w > -1 & data$pcc_w < 1) # Exclude outliers - automatize later

(
funnel_win <- ggplot(data = data[filter_pcc_w,], aes(x = pcc_w[filter_pcc_w], y = se_precision_w[filter_pcc_w])) + 
  geom_point(color = "#0d4ed1") + 
  labs(title = NULL, x = "Partial correlation coefficient", y = "Precision of the estimate (1/SE)") +
  main_theme()
)

