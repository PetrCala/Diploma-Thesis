#' |--------------------------|
#' Script name: script_runner_master_thesis_cala.R
#' 
#' The script runner for running the analysis for my Master Thesis on the topic
#' of 'Ability bias in returns to schooling: how large it is and why it matters?'
#' 
#' For detailed explanation, see the README file distributed with this script.
#' 
#' Author: Petr Čala
#' Year created: 2023
#' GitHub: github.com/PetrCala/
#' |--------------------------|
 
# Clean the environment
rm(list = ls()) 

# Load packages
if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi') # Working directory

# Working directory
if (! getwd() == dirname(getActiveDocumentContext()$path)){
  setwd(dirname(getActiveDocumentContext()$path)) # Set WD to the current file location
  print(paste0('Setting the working directory to: ', getwd()))
}

# Time the script run
# system.time(source("main_master_thesis_cala.R"))
 
# Run the main file
source("main_master_thesis_cala.R")

