# R code for MAIVE
#
# 1. Input as excel file:
#
#       estimates: bs
#       standard errors: sebs
#       number of observations: Ns
#       (optional: study_id)
#
# 2. Default option for MAIVE: MAIVE-PET-PEESE, unweighted, with instrumented SEs
#
#  Other available options to the user:
#       method= 1 FAT-PET, 2 PEESE, 3 PET-PEESE, 4 EK
#       weighting = 0 no weights, 1 standard weights, 2 adjusted weights  
#       instrumenting = 1 yes, 0 no 
#       correlation at study level: 0 none, 1 fixed effect dummies, 2 clusters
#
# 3. Output:
#
#       MAIVE meta-estimate and standard error
#       Hausman type test: comparison between MAIVE and standard version
#       (When instrumenting: heteroskedastic robust F-test of the first step)

rm(list = ls())

# OPTIONS: 
# method: PET:1, PEESE:2, PET-PEESE:3, EK:4 (default 3)
  method <- 3
# weighting: default no weight: 0 ; weights: 1, adjusted weights: 2 (default 0)
  weight <- 0
# instrumenting (default 1)
  instrument <- 1 
# correlation at study level: none: 0 (default), fixed effects: 1, cluster: 2
  studylevel <-0
# default options are method=3; weight=0; instrument=1; studylevel=0 


# CHOOSE DATASET
# This code, the function maivefunction.R and the excel file inputdata.xlsx must be in the same directory
  if (!require('rstudioapi')) install.packages('rstudioapi'); library('rstudioapi')
  if (!require('readxl')) install.packages('readxl'); library('readxl')
  setwd(dirname(getActiveDocumentContext()$path))
  dat <- read_excel("inputdata.xlsx")
  
  source("maivefunction.R")

# This is the actual execution of the code:
MAIVE=maive(dat=dat,method=method,weight=weight,instrument=instrument,studylevel=studylevel)
cat("\f")

object<-c("MAIVE coefficient","MAIVE standard error","F-test of first step in IV","Hausman-type test (to be used with caution)","Critical Value of Chi2(1)")
value<-c(MAIVE$beta,MAIVE$SE,MAIVE$`F-test`,MAIVE$Hausman,MAIVE$Chi2)
MAIVEresults<-data.frame(object,value)
cat("\f")
MAIVEresults
