rm(list=ls())

#Load libraries
library(multiwayvcov)
library(foreign)
library(multcomp)
library(ggplot2)
library(dplyr)
library(forcats)
library(AER)
library(puniform)
library(Hmisc)
library(DescTools)
library(plm)
library(fwildclusterboot)
library(fixest)
library(readxl)



#Set Seed
set.seed(2281)



# Import the data 
DFR <- read_excel("zzz All my folders here/CUNI/IES/2. Paper 1/0. Article/DFR.xlsx", sheet = "Data")
View(DFR)




#Winsorization set-up
estimate_w <- Winsorize(DFR$estimate, type = 2, probs = c(0.01, 0.99))
se_estimate_w <- Winsorize(DFR$se_estimate, type = 2, probs = c(0.01, 0.99))

DFR$estimate_w <- estimate_w
DFR$se_estimate_w <- se_estimate_w




#OLS Section
OLS <- feols(estimate_w ~ se_estimate_w, cluster = "Study_ID", panel.id = "Study_ID", data = DFR)
summary(OLS)

#OLS Wild Bootstrap intervals
OLS.B1<-boottest(OLS,clustid = "study_id", param = c("se_estimate_w"), B=9999)
OLS.B0<-boottest(OLS,clustid = "study_id", param = c("(Intercept)"), B=9999)
summary(OLS.B1)
summary(OLS.B0)




#Fixed Effects Model Section    
FE_lm <- plm(estimate_w ~ se_estimate_w, model = "within", index = "Study_ID", data = DFR)
summary(BE_plm) 

#Clustered at study level setup
CL <- length(unique(DFR$study_id)) #compute Stata like df-adjustment
dfa <- (CL/(CL - 1))
study_c_vcov <- dfa * vcovHC(FE_lm, type = "HC1", cluster = "group", adjust = T)
coeftest(FE_lm, vcov = study_c_vcov)

within_intercept(FE_lm, vcov=function(BE_plm) vcovHC(BE_plm, cluster = "group", adjust = T, type="sss"))




#Between Effects Section
BE_plm <- plm(estimate_w ~ se_estimate_w, model = "between", index = "Study_ID", data = DFR)
summary(BE_plm)




#Random Effects Section
RE_plm <- plm(estimate_w ~ se_estimate_w, model = "random", index = "Study_ID", data = DFR)
summary(RE_plm)




#Weighted by inverse SE
Inv_SEE <- feols(estimate_w ~ se_estimate_w, cluster = "Study_ID", panel.id = "Study_ID", weights = ~Inv_SE,  data = DFR)
summary(Inv_SEE)

#Inv_SEE Wild Bootstrap intervals
Inv_SEE.B1<-boottest(Inv_SEE,clustid = "study_id", param = c("se_estimate_w"), B=9999)
Inv_SEE.B0<-boottest(Inv_SEE,clustid = "study_id", param = c("(Intercept)"), B=9999)
summary(Inv_SEE.B1)
summary(Inv_SEE.B0)




#Weighted by Number of Observations
DFR$Inv_Obs <- 1/DFR$Obs
OBS_weighted <- feols(estimate_w ~ se_estimate_w, cluster = "Study_ID", panel.id = "Study_ID", weights = ~Inv_Obs,  data = DFR)
summary(OBS_weighted)

#OBS_weighted Wild Bootstrap intervals
OBS_weighted.B1<-boottest(OBS_weighted,clustid = "study_id", param = c("se_estimate_w"), B=9999)
OBS_weighted.B0<-boottest(OBS_weighted,clustid = "study_id", param = c("(Intercept)"), B=9999)
summary(OBS_weighted.B1)
summary(OBS_weighted.B0)




#IV Model Section
DFR$Lobs_Instrument <- 1/sqrt(DFR$Lobs)
IV_Reg <- feols(estimate_w~1|se_estimate_w~Lobs_Instrument, cluster = "Study_ID", ssc=ssc(adj = FALSE, cluster.adj=FALSE), se =  "cluster", data = DFR)
summary(IV_Reg)

