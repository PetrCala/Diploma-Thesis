#####################################################################
# Application 1 -- P-hacking in economics journals
# Paper: Detecting p-hacking
# Authors: G. Elliott, N. Kudrin, K. Wuthrich
#####################################################################
#The following packages need to be installed!
# install.packages("NlcOptim")
# install.packages("fdrtool")
# install.packages("pracma")
# install.packages("gdata")
# install.packages("spatstat")
# install.packages("rddensity")
# install.packages("grDevices")
# install.packages("ggplot2")

library(haven)
library(tidyverse)
library(ggplot2)


#(the data used in this code was produced by Data_preparation.do)
rm(list = ls())
p_max=0.15
p0 = 0

CS_bins_all = 15 #number of bins for all data
sep = "015_30bins."
seprnd = "015_15bins."
Interv = "\u2264 0.15: "

# Set your working directory (it has to be the folder where you save 'Replication_package')
wd = "G:/My Drive/Risk Aversion/Code/elliot_phacking/"

setwd(wd)
RNGkind(sample.kind = "default")
source("Replication_package/Application 1/LCM_cdf.R")
source("Replication_package/Application 1/Tests.R")

#rawdata = 0/1 for derounded/raw data

#UPLOAD CSV FILES
#csv files are the results of data_preparation.do do-file
data <- read.csv(file="RRA_Elliot.csv", header=TRUE, sep=",")

attach(data)

Output = matrix(0, 8, 3)
# Output = matrix(0, 8, 3)

ptop = ptop
# Name = c("All","Economics","Finance")
Name = c("All","Top","GMM")
title_all = "All papers, rounded"

ptop = abs(ptop)
ptop[ptop>1]=1

print(max(id))


P_all = ptop	
P_econ = ptop[econ==1]	 
P_fin = ptop[fin==1]
id_econ = id[econ==1]
id_fin = id[fin==1]


samples = c("P_all", "P_econ","P_fin")
indices = c("id", "id_econ", "id_fin")


CS_bins = CS_bins_all
leftb1 = 1.1*p_max/CS_bins
psiz1 = 1.5
scaling = 2

for (r in 1:3){

  
  if (r==2){
    CS_bins = 10 #number of bins for random draws
    leftb1 = 1.1*p_max/(CS_bins)
  }
  
  if (r==3){
    CS_bins = 5 #number of bins for random draws
    leftb1 = 1.1*p_max/(CS_bins)
  }

  P = eval(parse(text = samples[r]))
  ind = eval(parse(text = indices[r]))
  p_min = min(P[P>0])
  
  # Figures for All papers
  if (r==1){
  grDevices::cairo_pdf(file = paste(Name[r], "pdf", sep=sep))
  a<-hist(P[P<=p_max&P>=p0], breaks = linspace(p0,p_max,(CS_bins+1)), main=title_all, xlab="P-value", cex.main = 2, cex.lab=1.5, cex.axis=1.5)
  }
  
  N_B = length(P[P<=0.05&P>=0.04])
  Nl = length(P[P<=p_max&P>=0])
  Bin5 = specify_decimal(Binomial(P, 0.04,0.05, "c"),3)
  Discontinuity = specify_decimal(Discontinuity_test(P,0.05, 0.01),3)
  LCM_sup = specify_decimal(LCM(P, p_min,p_max,8),3)
  CS_1 = specify_decimal(CoxShi(P,ind,  0, p_max, CS_bins, 1, 0),3)
  CS_2B = specify_decimal(CoxShi(P,ind,  0, p_max, CS_bins, 2, 1),3)
  FM = specify_decimal(Fisher(P, 0, p_max),3)
  
  col_out = r
  Output[1, col_out] = Bin5
  Output[2, col_out] = FM
  Output[3, col_out] = Discontinuity
  Output[4, col_out] = CS_1
  Output[5, col_out] = CS_2B
  Output[6, col_out] = LCM_sup
  Output[7, col_out] = N_B
  Output[8, col_out] = Nl
  
  leftb = leftb1
  psiz = psiz1
  multipl = 1
  if (r==1){
    text(leftb,max(a$counts)*multipl, font=2,cex = psiz, adj = 0, "Test: p-value")
    multipl = multipl - 0.05
    text(leftb,max(a$counts)*multipl,cex = psiz, adj = 0, paste0(('Binomial: '), Bin5))
    multipl = multipl - 0.05
    text(leftb,max(a$counts)*multipl,cex = psiz, adj = 0, paste0(("Fisher's Test: "), FM))
    multipl = multipl - 0.05
    
    text(leftb,max(a$counts)*multipl,cex = psiz, adj = 0, paste0(('Discontinuity: '), Discontinuity))
    multipl = multipl - 0.05
    
    text(leftb,max(a$counts)*multipl,cex = psiz, adj = 0, paste0(('CS1: '), CS_1))
    multipl = multipl - 0.05
    text(leftb,max(a$counts)*multipl, adj = 0,cex = psiz,paste0("CS2B: ", CS_2B))
    multipl = multipl - 0.05
    text(leftb,max(a$counts)*multipl, adj = 0,cex = psiz,paste0("LCM: ", LCM_sup))
    
    
    leftb = leftb1+p_max/scaling
    multipl = 1
    text(leftb,max(a$counts), font=2,cex = psiz, adj = 0, "Interval: # of obs")
    multipl = multipl - 0.05
    text(leftb,max(a$counts)*multipl,adj = 0, cex = psiz,paste0("[0.04, 0.05]: ", N_B))
    multipl = multipl - 0.05
    text(leftb,max(a$counts)*multipl,adj = 0,cex = psiz, paste0(Interv, Nl))
    dev.off()
  }
}


ColumnNamesAll = c("All papers/rounded/all", "Econ/rounded/all","Fin/rounded/all")

ColumnNames = c(ColumnNamesAll)
RowNames = c("Binomial","Fischer","Discontinuity","CS_1","CS_2B","LCM","Obs in [0.04, 0.05]","Obs<=0.15")

write.table(Output, sep = ',', file = "Table_1.csv", row.names = RowNames, col.names = ColumnNames)

detach(data)
