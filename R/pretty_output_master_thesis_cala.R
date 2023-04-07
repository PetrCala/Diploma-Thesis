# Clean the environment
rm(list = ls()) 

# Working directory
if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi')
if (! getwd() == dirname(getActiveDocumentContext()$path)){
  setwd(dirname(getActiveDocumentContext()$path)) # Set WD to the current file location
  print(paste0('Setting the working directory to: ', getwd()))
}

# Run the main code
source("main_master_thesis_cala.R")
