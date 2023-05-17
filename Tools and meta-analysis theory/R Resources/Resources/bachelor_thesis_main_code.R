#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("stats")
#install.packages("DescTools") #Winsorization
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("multiwayvcov") # Multi-way clustering
#install.packages("metafor") #FE/RE
#install.packages("bayesm") #Hierarchical FAT-PET in R
#install.packages("puniform")
#install.packages("haven")
#install.packages("meta")
#install.packages("AER")
#install.packages("BMS")
#install.packages("corrplot") #Correlation plot
#install.packages("foreign") #FMA packages
#install.packages("xtable")
#install.packages("LowRankQP")
#install.packages("foreign") #BPE
#install.packages("multcomp")

library(readr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(stats)
library(DescTools)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(metafor)
library(bayesm)
library(puniform)
library(haven)
library(meta)
library(AER)
library(BMS)
library(corrplot)
library(foreign)
library(xtable)
library(LowRankQP)
library(foreign)
library(multcomp)

######################### Start of code #########################


#################
#Original dataset
#################

#original data
dataset_orig <- read.csv("data_set.csv", sep=";", dec = ",", nrows = 1655)


##################################
#Identifying the treatment groups
##################################

#Only those observations which correspond to the reward groups (control groups taken out)

dataset <- dataset_orig[dataset_orig$grp_reward == 1,]

#############################
#Winsorization and groundwork
#############################
dataset$pcc_w <- Winsorize(x = dataset$pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
dataset$se_pcc_w <- Winsorize(x = dataset$se_pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
dataset$se_precision_w <- 1/dataset$se_pcc_w
dataset$t_adjusted_w <- Winsorize(x = dataset$t_adjusted, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
dataset$significant_w <- c(rep(0,nrow(dataset)))
dataset$significant_w[dataset$t_adjusted_w>1.96] <- 1



#############################
#Summary statistics & Plots
#############################

#Summary statistics and the funnel plot for the winsorized data
summary(dataset$pcc_w)
summary(dataset$se_precision_w)

filter_pcc_w <- (dataset$pcc_w> -0.38851 & dataset$pcc_w<0.525172) #Excluding the outliers from the graph

#Funnel plot
(funnel_win <- ggplot(data = dataset[filter_pcc_w,], aes(x = pcc_w[filter_pcc_w], y = se_precision_w[filter_pcc_w])) + 
  geom_point(colour = "#0d4ed1") + 
  labs(title = NULL, x = "Partial correlation coefficient", y = "Precision of the estimate (1/SE)") +
  theme(axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
        axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
        plot.background = element_rect(fill = "#DCEEF3")))
  
      
#Calculating the Confidence intervals for non-weighted means
CI <- function(data, conf.level = 0.95) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(dataset$pcc_w[data])
  print(paste("Mean and CI"))
  print(xbar, digits = 2)
  sdx = sqrt(var(dataset$pcc[data])/length(dataset$pcc[data]))
  conf.int <- c(xbar - z * sdx, xbar + z * sdx)
  print(conf.int, digits = 2)
  print(length(dataset$pcc_w[data]))
}

#Summary statistics for PCC_w given various conditions
CI()
CI(dataset$effect_gpa==1)
CI(dataset$effect_charity==1)
CI(dataset$effect_game==1)
CI(dataset$effect_work==1)
CI(dataset$effect_positive==1)
CI(dataset$effect_positive==0)
CI(dataset$ols_method==1)
CI(dataset$logit_method==1)
CI(dataset$probit_method==1)
CI(dataset$tobit_method==1)
CI(dataset$fe_method==1)
CI(dataset$re_method==1)
CI(dataset$diff_method==1)
CI(dataset$other_method==1)
CI(dataset$data_cross==1)
CI(dataset$data_panel==1)
CI(dataset$location_field==1)
CI(dataset$location_lab==1)
CI(dataset$crowding_out==1)
CI(dataset$framing_pos==1)
CI(dataset$framing_neg==1)
CI(dataset$reward_scaled>=0.2)
CI(dataset$reward_scaled<0.2)
CI(dataset$all_paid==1)
CI(dataset$reward_own==1)
CI(dataset$reward_else==1)
CI(dataset$perf_quan==1)
CI(dataset$perf_qual==1)
CI(dataset$task_cog==1)
CI(dataset$task_man==1)
CI(dataset$task_app==1)
CI(dataset$task_napp==1)
CI(dataset$mot_alt==1)
CI(dataset$mot_tru==1)
CI(dataset$mot_rec==1)
CI(dataset$mot_fai==1)
CI(dataset$mot_mon==1)
CI(dataset$subject_st==1)
CI(dataset$subject_emp==1)
CI(dataset$subject_mix==1)
CI(dataset$male>0.5)
CI(dataset$male<0.5)
CI(dataset$developed_country==1)
CI(dataset$developed_country==0)


#Calculating the Confidence intervals for weighted means
CI_w <- function(data, conf.level = 0.95) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = weighted.mean(dataset$pcc_w[data], w = c(dataset$study_size*dataset$study_size)[data])
  print(paste("Weighted mean and CI"))
  print(xbar, digits = 2)
  sdx = sqrt(var(dataset$pcc[data])/length(dataset$pcc[data]))
  conf.int <- c(xbar - z * sdx, xbar + z * sdx)
  print(conf.int, digits = 2)
}

#Means weighted by the number of observations per study
CI_w()
CI_w(dataset$effect_gpa==1)
CI_w(dataset$effect_charity==1)
CI_w(dataset$effect_game==1)
CI_w(dataset$effect_work==1)
CI_w(dataset$effect_positive==1)
CI_w(dataset$effect_positive==0)
CI_w(dataset$ols_method==1)
CI_w(dataset$logit_method==1)
CI_w(dataset$probit_method==1)
CI_w(dataset$tobit_method==1)
CI_w(dataset$fe_method==1)
CI_w(dataset$re_method==1)
CI_w(dataset$diff_method==1)
CI_w(dataset$other_method==1)
CI_w(dataset$data_cross==1)
CI_w(dataset$data_panel==1)
CI_w(dataset$location_field==1)
CI_w(dataset$location_lab==1)
CI_w(dataset$crowding_out==1)
CI_w(dataset$framing_pos==1)
CI_w(dataset$framing_neg==1)
CI_w(dataset$reward_scaled>=0.2)
CI_w(dataset$reward_scaled<0.2)
CI_w(dataset$all_paid==1)
CI_w(dataset$reward_own==1)
CI_w(dataset$reward_else==1)
CI_w(dataset$perf_quan==1)
CI_w(dataset$perf_qual==1)
CI_w(dataset$task_cog==1)
CI_w(dataset$task_man==1)
CI_w(dataset$task_app==1)
CI_w(dataset$task_napp==1)
CI_w(dataset$mot_alt==1)
CI_w(dataset$mot_tru==1)
CI_w(dataset$mot_rec==1)
CI_w(dataset$mot_fai==1)
CI_w(dataset$mot_mon==1)
CI_w(dataset$subject_st==1)
CI_w(dataset$subject_emp==1)
CI_w(dataset$subject_mix==1)
CI_w(dataset$male>0.5)
CI_w(dataset$male<0.5)
CI_w(dataset$developed_country==1)
CI_w(dataset$developed_country==0)



###Box plot PCC###
(study_PCC <- ggplot(data = dataset, aes(x = pcc_w, y=factor(study, levels = rev(levels(factor(study)))))) +
    geom_boxplot(outlier.colour = "#005CAB", outlier.shape = 21, outlier.fill = "#005CAB", fill="#e6f3ff", color = "#0d4ed1") +
    labs(title = NULL,x="Estimate of the PCC between rewards and performance", y = NULL) +
    theme(axis.line = element_line(color = "black", size = 0.75, linetype = "solid"), axis.text.x = element_text(colour = "black"),
          axis.ticks.y = element_blank(), axis.text.y = element_text(colour = "black"),
          panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
          plot.background = element_rect(fill = "#DCEEF3"))
)


#############################################################
#############################################################
#PUBLICATION BIAS - FAT-PET (Stanley, 2005)
#############################################################
#############################################################

#OLS

OLS <- lm(formula = pcc_w ~ se_pcc_w, data = dataset)
OLS_id <- coeftest(OLS, vcov = vcovHC(OLS, type = "HC0", cluster = c(dataset$study_id))) #clustering by study_id
print(OLS_id) #OLS, clustered

#FE
FE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = dataset, method = "FE")
FE_c <- coeftest(FE, vcov = vcov(FE, type = "fixed", cluster = c(dataset$study_id))) 
print(FE_c)

#Between
RE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = dataset, method = "REML")
RE_c <- coeftest(RE, vcov = vcov(RE, type = "fixed", cluster = c(dataset$study_id))) 
print(RE_c)

#Weighted by number of observations per study
OLS_w_study <- lm(formula = pcc_w ~ se_pcc_w, data = dataset, weight = (dataset$study_size*dataset$study_size))
OLS_w_study_c <- coeftest(OLS_w_study, vcov = vcovHC(OLS_w_study, type = "HC0", cluster = c(dataset$study_id))) 
print(OLS_w_study_c) #OLS weighted by study, clustered

#Weighted by precision
OLS_w_precision <- lm(formula = pcc_w ~ se_pcc_w, data = dataset, weight = c(dataset$se_precision_w*dataset$se_precision_w))
OLS_w_precision_c <- coeftest(OLS_w_precision, vcov = vcovHC(OLS_w_precision, type = "HC0", cluster = c(dataset$study_id))) 
print(OLS_w_precision_c) #OLS weighted by precision, clustered


#############################################################
#############################################################
#PUBLICATION BIAS - WAAP (Ioannidis et al., 2017)
#############################################################
#############################################################


#A weighted average of the adequately powered

WLS_FE_avg <- sum(dataset$pcc_w/dataset$se_pcc_w)/sum(1/dataset$se_pcc_w) #sum of weighted effects divided by the sum of weights
WAAP_bound <- abs(WLS_FE_avg)/2.8
WAAP_reg <- lm(formula = pcc_w ~ -se_precision_w, data = dataset[dataset$se_pcc_w<WAAP_bound,])
WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(dataset$study_id)))
print(WAAP_reg_cluster)

#############################################################
#############################################################
#PUBLICATION BIAS - TOP10 method (Stanley et al., 2010)
#############################################################
#############################################################


T10_bound <- quantile(dataset$se_precision_w, probs = 0.9) #setting the 90th quantile bound
T10_reg <- lm(formula = pcc_w ~ -se_precision_w, data = dataset[dataset$se_precision_w>T10_bound,]) #regression using the filtered data
T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(dataset$study_id)))
print(T10_reg_cluster)


#############################################################
#############################################################
#PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019)
#############################################################
#############################################################


source("stem_method.R") #github.com/Chishio318/stem-based_method

est_stem <- stem(dataset$pcc_w, dataset$se_pcc_w, param)
est_stem$estimates
funnels_stem <- stem_funnel(dataset$pcc_w, dataset$se_pcc_w, est_stem$estimates) #For more detail see link above


#############################################################
#############################################################
#PUBLICATION BIAS - FAT-PET hierarchical in R
#############################################################
#############################################################

study_levels_h <- levels(as.factor(dataset$study))
nreg_h <- length(study_levels_h)
regdata_h <- NULL
for (i in 1:nreg_h) {
  filter <- dataset$study==study_levels_h[i] #T/F vector identifying if the observation is from the i-th study
  y <- dataset$pcc_w[filter] #PCCs from the i-th study
  X <- cbind(1,
             dataset$se_pcc_w[filter])
  regdata_h[[i]] <- list(y=y, X=X)
}
Data_h <- list(regdata=regdata_h)
Mcmc_h <- list(R=6000)
out_h <- bayesm::rhierLinearModel(
  Data=Data_h,
  Mcmc=Mcmc_h)
cat("Summary of Delta Draws", fill=TRUE)
summary(out_h$Deltadraw) 



#############################################################
#############################################################
#PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019)
#############################################################
#############################################################

#Use
#https://maxkasy.github.io/home/metastudy/
#with the sheet "selection_model" in the appended data file, 1% winsorization

#Manually extracted coefficients for student-t distribution, 1% winsorization
(matrix_selection <- matrix(c(0, 0.003), byrow = TRUE, nrow = 1, ncol = 2,
                            dimnames = list(c("estimate"),c("mean", "standard error"))))


#############################################################
#############################################################
#PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020)
#############################################################
#############################################################

#The results for this model were obtained with a kind help of the supervisor,
#as R code does not exist yet for this model (to our awareness).


###### Relaxing the exogeneity assumption between ESTIMATE and SE (ESTIMATE) ######


#########################################################################
#########################################################################
#PUBLICATION BIAS - FAT-PET with IV
#########################################################################
#########################################################################

instrument <- 1/sqrt(dataset$n_obs) #instrument to be used as weight
IV_reg1 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg1, vcov = vcovHC(IV_reg1, cluster = c(dataset$study_id)), diagnostics = TRUE) #Diagnostic tests included in the summary


instrument <- 1/(dataset$n_obs)
IV_reg2 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg2, vcov = vcovHC(IV_reg2, cluster = c(dataset$study_id)), diagnostics = TRUE)

instrument <- 1/(dataset$n_obs^2)
IV_reg3 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg3, vcov = vcovHC(IV_reg3, cluster = c(dataset$study_id)), diagnostics = TRUE)

#The test with best performing instruments
instrument <- log(dataset$n_obs)
IV_reg4 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg4, vcov = vcovHC(IV_reg4, cluster = c(dataset$study_id)), diagnostics = TRUE)



#########################################################################
#########################################################################
#PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) - code for R
#########################################################################
#########################################################################

#Groundwork
study_levels_h <- levels(as.factor(dataset$study))
nreg_h <- length(study_levels_h)

#Vector of medians for pcc_w
med_pcc_w <- c(rep(NA,nreg_h))
for (i in 1:nreg_h) {
  y <- median(dataset$pcc_w[dataset$study == levels(as.factor(dataset$study))[i]])
  med_pcc_w[i] <- y
}

#Vector of medians for se_pcc_w
med_se_pcc_w <- c(rep(NA,nreg_h))
for (i in 1:nreg_h) {
  y <- median(dataset$se_pcc_w[dataset$study == levels(as.factor(dataset$study))[i]])
  med_se_pcc_w[i] <- y
}


#Maximum likelihood
p_uni_est <- puniform(yi = med_pcc_w, vi = med_se_pcc_w^2, side = "right", method = "ML",
                       alpha = 0.05)
print(p_uni_est)

#A different approach using moments method estimation
#p_uni_est2 <- puni_star(yi = med_pcc_w, vi = med_se_pcc_w^2, side = "right", method = "P",
#                        alpha = 0.05,control=list(max.iter=1000, tol=0.05, reps=10000, int=c(-1,1), verbose=TRUE))
#print(p_uni_est2)




#########################################################################
#########################################################################
#PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008)
#########################################################################
#########################################################################


filter2 <- (dataset$t_adjusted_w>-4 & dataset$t_adjusted_w<19.692) #removing the outliers from the graph

#histogram of t-statistic distribution
(t_hist <- ggplot(data = dataset[filter2,], aes(x = t_adjusted_w[filter2], y = ..density..)) +
  geom_histogram(color = "black", fill = "#1261ff", bins = "80") +
  geom_vline(aes(xintercept = mean(t_adjusted_w)), color = "dark orange", linetype = "dashed", size = 0.7) + 
  geom_vline(aes(xintercept = -1.96), color = "red", size = 0.5) +
  geom_vline(aes(xintercept = 1.96), color = "red", size = 0.5) +
  labs(x = "T-statistic", y = "Density")) +
  scale_x_continuous(breaks = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20), label = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20)) +
  theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),
        axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
        panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
        plot.background = element_rect(fill = "#DCEEF3")) 


#1.96 bound Caliper tests

dataset$significant_w <- c(rep(0,nrow(dataset)))
dataset$significant_w[dataset$t_adjusted_w> 1.96] <- 1 #laying the groundwork for the regression

Cal_1_nc <- lm(formula = significant_w ~ t_adjusted_w - 1, data = dataset[dataset$t_adjusted_w>1.91 & dataset$t_adjusted_w<2.01,])
Cal_1 <- coeftest(Cal_1_nc, vcov = vcovHC(Cal_1_nc, type = "const", cluster = c(dataset$study_id)))
print(Cal_1)

Cal_2_nc <- lm(formula = significant_w ~ t_adjusted_w - 1, data = dataset[dataset$t_adjusted_w>1.86 & dataset$t_adjusted_w<2.06,])
Cal_2 <- coeftest(Cal_2_nc, vcov = vcovHC(Cal_2_nc, type = "const", cluster = c(dataset$study_id)))
print(Cal_2)

Cal_3_nc <- lm(formula = significant_w ~ t_adjusted_w - 1, data = dataset[dataset$t_adjusted_w>1.76 & dataset$t_adjusted_w<2.16,])
Cal_3 <- coeftest(Cal_3_nc, vcov = vcovHC(Cal_3_nc, type = "const", cluster = c(dataset$study_id)))
print(Cal_3)


#-1.96 bound Caliper tests

dataset$significant_w <- c(rep(0,nrow(dataset)))
dataset$significant_w[dataset$t_adjusted_w< -1.96] <- 1 #laying the groundwork for the regression


Cal_1_nc <- lm(formula = significant_w ~ t_adjusted_w - 1, data = dataset[dataset$t_adjusted_w>-2.01 & dataset$t_adjusted_w< -1.91,])
Cal_1 <- coeftest(Cal_1_nc, vcov = vcovHC(Cal_1_nc, type = "const", cluster = c(dataset$study_id)))
print(Cal_1)

Cal_2_nc <- lm(formula = significant_w ~ t_adjusted_w - 1, data = dataset[dataset$t_adjusted_w>-2.06 & dataset$t_adjusted_w< -1.86,])
Cal_2 <- coeftest(Cal_2_nc, vcov = vcovHC(Cal_2_nc, type = "const", cluster = c(dataset$study_id)))
print(Cal_2)

Cal_3_nc <- lm(formula = significant_w ~ t_adjusted_w - 1, data = dataset[dataset$t_adjusted_w>-2.16 & dataset$t_adjusted_w< -1.76,])
Cal_3 <- coeftest(Cal_3_nc, vcov = vcovHC(Cal_3_nc, type = "const", cluster = c(dataset$study_id)))
print(Cal_3)


#########################################################################
#########################################################################
#HETEROGENEITY - Bayesian Model Averaging in R
#########################################################################
#########################################################################


#We get rid of variables, which provide no useful information when used in model averaging (such as the name of the observed group)

BMA_data_orig <- read.csv("bma_data.csv", sep=";", dec = ",", nrows = 1655)

#Filtering the reward groups
BMA_data <- (BMA_data_orig[BMA_data_orig$grp_reward == 1,2:35])
#The control column is included due to bad legibility of the .csv file, as the first box gets added the ";" separator, which is undesirable
#The BMA_data data frame accounts for this and includes only the desired columns


###################################
#Winsorization for the new data set
###################################
BMA_data$pcc <- Winsorize(x = BMA_data$pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
BMA_data$se_pcc <- Winsorize(x = BMA_data$se_pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))

BMA_num <- data.frame(BMA_data, stringsAsFactors = TRUE) #converting to a data frame
summary(BMA_num)


#Testing for VIF
BMA_formula <- as.formula(paste("pcc",paste(colnames(BMA_num)[-1],sep="", collapse = "+"), sep="~",collapse = NULL))
BMA_reg_test <- lm(formula = BMA_formula, data = BMA_num)
car::vif(BMA_reg_test) #VIF coefficients


#Adding names for clarity of the BMA figure
names(BMA_num) <- c("PCC", "Standard error", "Effect GPA", "Effect Charity", "Effect Game",
                    "Effect Positive", "OLS", "Logit", "Probit", "Tobit", "Fixed-effects",
                    "Random-effects", "Diff-in-diff", "Cross-section data",
                    "Average Year", "Lab study", "Crowding-out", "Journal impact",
                    "Study citations", "Positive framing", "Reward scaled", "All paid",
                    "Reward own", "Quan. performance", "Cognitive task",
                    "Appealing task", "Altruism", "Reciprocity", "Fairness",
                    "Students", "Employees", "Gender", "Mid age", "Developed country")
head(BMA_num)

###############
#### BMA1 #### #robustness check
###############

BMA1 = bms(BMA_num, burn=1e5,iter=3e5, g="UIP", mprior="uniform", nmodel=50000,mcmc="bd", user.int =FALSE) 
print(BMA1)

#Extracting the coefficients and plotting the results
coef(BMA1,order.by.pip= F, exact=T, include.constant=T)
image(BMA1, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA1)
plot(BMA1)
print(BMA1$topmod[1])


###############
#### BMA2 #### #main model with dilution prior
###############

BMA2= bms(BMA_num, burn=1e5,iter=3e5, g="UIP", mprior="dilut", nmodel=50000, mcmc="bd", user.int=FALSE) 
print(BMA2)

#Extracting the coefficients and plotting the results
coef(BMA2,order.by.pip= F, exact=T, include.constant=T)
image(BMA2, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA2)
plot(BMA2)
print(BMA2$topmod[1])


###############
#### BMA3 #### #robustness check
###############

BMA3 = bms(BMA_num, burn=1e5,iter=3e5, g="BRIC", mprior="random", nmodel=50000,mcmc="bd", user.int =FALSE) 
print(BMA3)

#Extracting the coefficients and plotting the results
coef(BMA3,order.by.pip= F, exact=T, include.constant=T)
image(BMA3, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA3)
plot(BMA3)
print(BMA3$topmod[1])


###############
#### BMA4 #### #robustness check
###############

#careful, takes about 3-4 mins
BMA4 = bms(BMA_num, burn=1e5,iter=3e5, g="HQ", mprior="random", nmodel=50000,mcmc="bd", user.int =FALSE) 
print(BMA4)

#Extracting the coefficients and plotting the results
coef(BMA4,order.by.pip= F, exact=T, include.constant=T)
image(BMA4, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA4)
plot(BMA4)
print(BMA4$topmod[1])


#plotting variables x PIP for each of the models
plotComp("UIP and Dilut"=BMA1, "UIP and Uniform"=BMA2,"BRIC and Random"=BMA3,"HQ and Random"=BMA4, add.grid=F,cex.xaxis=0.7)


#Plotting the correlation
col<- colorRampPalette(c("red", "white", "blue"))
M <- cor(BMA_num)
corrplot.mixed(M, lower = "number", upper = "circle", lower.col=col(200), upper.col=col(200),tl.pos = c("lt"),
               diag = c("u"), tl.col="black", tl.srt=70, tl.cex=0.85, number.cex = 0.5,cl.cex=0.8, cl.ratio=0.1)


#########################################################################
#########################################################################
#HETEROGENEITY - Frequentist model averaging code for R (Hansen)
#########################################################################
#########################################################################

#The code was obtained from the supervisor, for detailed explanation on the theory see the original paper (cited in the thesis)
#One must have the objects BMA_num and BMA2 from the previous section loaded in order to be able to run this code


#Using BMA_num from the previous section
x.data <- BMA_num[,-1]

#Reordering the columns in accordance to BMA2
BMA2_c <- coef(BMA2,order.by.pip= T, exact=T, include.constant=T) #loading the matrix sorted by PIP
FMA_order <- c(0)
for (i in 1:nrow(BMA2_c)-1){
  FMA_order[i] <- BMA2_c[i,5]
}
x.data <- x.data[,c(FMA_order)] #ordering the data

const_<-c(1)
x.data <-cbind(const_,x.data) #This gives us the dataset in the desired form


#Groundwork
x <- sapply(1:ncol(x.data),function(i){x.data[,i]/max(x.data[,i])})
scale.vector <- as.matrix(sapply(1:ncol(x.data),function(i){max(x.data[,i])}))
Y <- as.matrix(BMA_num[,1]) #The effect (PCC)
output.colnames <- colnames(x.data)
full.fit <- lm(Y~x-1)
beta.full <- as.matrix(coef(full.fit))
M <- k <- ncol(x)
n <- nrow(x)
beta <- matrix(0,k,M)
e <- matrix(0,n,M)
K_vector <- matrix(c(1:M))
var.matrix <- matrix(0,k,M)
bias.sq <- matrix(0,k,M)


#Calculations
for(i in 1:M)
{
  X <- as.matrix(x[,1:i])
  ortho <- eigen(t(X)%*%X)
  Q <- ortho$vectors ; lambda <- ortho$values
  x.tilda <- X%*%Q%*%(diag(lambda^-0.5,i,i))
  beta.star <- t(x.tilda)%*%Y
  beta.hat <- Q%*%diag(lambda^-0.5,i,i)%*%beta.star
  beta[1:i,i] <- beta.hat
  e[,i] <- Y-x.tilda%*%as.matrix(beta.star)
  bias.sq[,i] <- (beta[,i]-beta.full)^2
  var.matrix.star <- diag(as.numeric(((t(e[,i])%*%e[,i])/(n-i))),i,i)
  var.matrix.hat <- var.matrix.star%*%(Q%*%diag(lambda^-1,i,i)%*%t(Q))
  var.matrix[1:i,i] <- diag(var.matrix.hat)
  var.matrix[,i] <- var.matrix[,i]+ bias.sq[,i]
}

e_k <- e[,M]
sigma_hat <- as.numeric((t(e_k)%*%e_k)/(n-M))
G <- t(e)%*%e
a <- ((sigma_hat)^2)*K_vector
A <- matrix(1,1,M)
b <- matrix(1,1,1)
u <- matrix(1,M,1)
optim <- LowRankQP(Vmat=G,dvec=a,Amat=A,bvec=b,uvec=u,method="LU",verbose=FALSE)
weights <- as.matrix(optim$alpha)
beta.scaled <- beta%*%weights
final.beta <- beta.scaled/scale.vector
std.scaled <- sqrt(var.matrix)%*%weights
final.std <- std.scaled/scale.vector
results.reduced <- as.matrix(cbind(final.beta,final.std))
rownames(results.reduced) <- output.colnames; colnames(results.reduced) <- c("Coefficient","Sd. Err")
MMA.fls <- round(results.reduced,4)
MMA.fls <- data.frame(MMA.fls)
t <- as.data.frame(MMA.fls$Coefficient/MMA.fls$Sd..Err)
t[MMA.fls$Coefficient == 0,] <- 0 #added by the author
MMA.fls$pv <-round((1-apply(as.data.frame(apply(t,1,abs)), 1, pnorm))*2,3)
MMA.fls$pv[MMA.fls$pv == 1] <- 0 #added by the author
MMA.fls$names <- rownames(MMA.fls)
names <- c(colnames(BMA_num))
names <- c(names,"const_")
MMA.fls <- MMA.fls[match(names, MMA.fls$names),]
MMA.fls$names <- NULL
MMA.fls[-1,]



####################################
###### Best-practice estimate ######
####################################


#Loading the data into the desired form <- same procedure as in the BMA approach

BMA_data_orig <- read.csv("bma_data.csv", sep=";", dec = ",", nrows = 1655)
BMA_data <- (BMA_data_orig[BMA_data_orig$grp_reward == 1,2:35])
BMA_data$pcc <- Winsorize(x = BMA_data$pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
BMA_data$se_pcc <- Winsorize(x = BMA_data$se_pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))

BMA_num <- data.frame(BMA_data, stringsAsFactors = TRUE)
BMA_formula <- as.formula(paste("pcc",paste(colnames(BMA_num)[-1],sep="", collapse = "+"), sep="~",collapse = NULL))

#The actual estimation

bpe <- lm(formula = BMA_formula, data = BMA_num) #constructing an OLS model
names(coef(bpe))


### Author ###
form_author <- "(Intercept) + 0.348*effect_gpa + 0.276*effect_charity + 0.272*effect_game + 0.865*effect_positive + 
  0.558*ols_method + 0.062*logit_method + 0.063*probit_method + 0.033*tobit_method + 
  0.037*fe_method + 0.026*re_method + 0.030*diff_method + 7.610*data_avgyear + 
  0.224*lab_control + 0.476*crowding_out + 14.975*journal_impact + 8.084*study_citations + 
  0.826*pos_framing_control + 0.609*reward_scaled + 0.735*all_paid + 0.811*reward_own_control + 
  0.694*perf_quan_control + 0.701*task_cog_control + 0.479*task_app + 0.279*mot_alt + 
  0.098*mot_rec + 0.155*mot_fai + 0.607*subject_st + 0.078*subject_emp + 0.528*gender_control + 
  2.932*mid_age + 0.833*developed_country = 0"
summary(glht(bpe, linfct = c(form_author), vcov = vcovHC(bpe, type = "HC0", cluster = c(BMA_data_orig$study_id))))


### Takahashi et al. (2016) ###
form_taka <- "(Intercept) + effect_gpa + effect_positive + tobit_method + cross_control +
  7.607*data_avgyear + lab_control + crowding_out + 0.028*journal_impact + 2.773*study_citations + 
  pos_framing_control + 0.43*reward_scaled + all_paid + reward_own_control + 
  perf_quan_control + task_cog_control + subject_st + 0.5*gender_control + 2.996*mid_age + developed_country = 0"
summary(glht(bpe, linfct = c(form_taka), vcov = vcovHC(bpe, type = "HC0", cluster = c(BMA_data_orig$study_id))))


### Lazear (2000a) ###
form_laz <- "(Intercept) + effect_positive + 0.38*ols_method + 0.46*logit_method + 7.598*data_avgyear + 
  6.985*journal_impact + 8.084*study_citations + 0.568*reward_scaled + all_paid + reward_own_control + 
  perf_quan_control + subject_emp + 0.5*gender_control + 3.178*mid_age + developed_country = 0"
summary(glht(bpe, linfct = c(form_laz), vcov = vcovHC(bpe, type = "HC0", cluster = c(BMA_data_orig$study_id))))


### Angrist et al. (2009) ###
form_ang <- "(Intercept) + effect_gpa + effect_positive + ols_method + cross_control + 7.601*data_avgyear + 
  6.985*journal_impact + 6.178*study_citations + pos_framing_control + 1.201*reward_scaled + reward_own_control + 
  task_cog_control + subject_st + 0.5*gender_control + 2.803*mid_age + developed_country = 0"
summary(glht(bpe, linfct = c(form_ang), vcov = vcovHC(bpe, type = "HC0", cluster = c(BMA_data_orig$study_id))))


######################### End of code #########################

