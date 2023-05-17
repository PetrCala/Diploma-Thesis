####################### Package handling ########################

# Required packages
packages <- c("readr", "tidyverse", "ggplot2", "readxl", "stats", "DescTools", "sandwich", "lmtest", "multiwayvcov",
              "metafor", "bayesm", "puniform", "haven", "meta", "AER", "BMS", "corrplot", "foreign", "xtable",
              "LowRankQP", "foreign", "multcomp", "prob")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
  print(paste("Installing package ", packages[!installed_packages],"...", sep = ""))
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
rm(list = ls()) #Clean environment

######################### Start of code #########################

###################
# Original data set
###################

data_raw <- read_xlsx("data_cala_et_al.xlsx", sheet = 'main', n_max = 1655)

#####################
# Data transformation
#####################

# Tools
win <- function(vector) {
  vector_w <- Winsorize(x = vector, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
  return(vector_w)
}

# Actual transformation
dataset <- data_raw[data_raw$grp_reward == 1,] #Treatment groups
dataset$pcc_w <- win(dataset$pcc)
dataset$se_pcc_w <- win(dataset$se_pcc)
dataset$se_precision_w <- 1/dataset$se_pcc_w
dataset$t_adjusted_w <- win(dataset$t_adjusted)
dataset$significant_w <- c(rep(0,nrow(dataset))) #Marking significant observation
dataset$significant_w[dataset$t_adjusted_w>1.96] <- 1

####################
# Summary statistics
####################

# Tools
summary_stats <- c(
  "effect_gpa" = 1,
  "effect_charity" = 1,
  "effect_game" = 1,
  "effect_work" = 1,
  "effect_positive1" = 1,
  "effect_positive2" = 0,
  "ols_method" = 1,
  "logit_method" = 1,
  "probit_method" = 1,
  "tobit_method" = 1,
  "fe_method" = 1,
  "re_method" = 1,
  "diff_method" = 1,
  "other_method" = 1,
  "data_cross" = 1,
  "data_panel" = 1,
  "location_field" = 1,
  "location_lab" = 1,
  "crowding_out" = 1,
  "framing_pos" = 1,
  "framing_neg" = 1,
  "reward_scaled1" = "gt0.2",
  "reward_scaled2" = "lt0.2",
  "all_paid" = 1,
  "reward_own" = 1,
  "reward_else" = 1,
  "perf_quan" = 1,
  "perf_qual" = 1,
  "task_cog" = 1,
  "task_man" = 1,
  "task_app" = 1,
  "task_napp" = 1,
  "mot_alt" = 1,
  "mot_tru" = 1,
  "mot_rec" = 1,
  "mot_fai" = 1,
  "mot_mon" = 1,
  "subject_st" = 1,
  "subject_emp" = 1,
  "subject_mix" = 1,
  "male1" = "gt0.5",
  "male2" = "lt0.5",
  "developed_country1" = 1,
  "developed_country2" = 0
)

CI <- function (data, sum_stats, conf.level = 0.95, weight = FALSE) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  df_rows <- length(sum_stats)
  df_cols <- 6
  df_matrix <- matrix(nrow=df_rows, ncol=df_cols) #Temporary matrix
  
  for (i in names(sum_stats)) {
    name <- i
    value <- sum_stats[name]
    last_char <- substr(name, nchar(name), nchar(name))
    if (isin(c(1,2,3,4,5), last_char)) {
      name <- substring(name, 1, nchar(i)-1)
    }
    
    if (grepl("lt", value, fixed=TRUE)) {
      value <- substr(value, 3, nchar(value))
      filter <- data[,name]<as.numeric(value)
      disp_name <- paste(name, "<", as.character(value))
    } else if (grepl("gt", value, fixed=TRUE)) {
      value <- substr(value, 3, nchar(value))
      filter <- data[,name]>as.numeric(value)
      disp_name <- paste(name, ">", as.character(value))
    } else {
      filter <- data[,name]==as.numeric(value)
      disp_name <- name
    }
    
    if (weight != TRUE) {
      xbar <- mean(data$pcc_w[filter]) #Simple mean
    } else {
      xbar <- weighted.mean(data$pcc_w[filter], #Weighted mean
                            w = c(data$study_size*data$study_size)[filter])
    }
    sdx <- sd(data$pcc_w[filter])   #SD
    conf_l <- xbar - z * sdx        #Confidence interval lower bound
    conf_u <- xbar + z * sdx        #Confidence interval upper bound
    obs <- sum(filter)              #Number of observations
    out <- c(disp_name, round(xbar, 3), round(sdx, 3),
             round(conf_l, 3), round(conf_u, 3), obs)
    row_idx <- match(i, names(summary_stats))
    df_matrix[row_idx,] <- out
  }
  df <- data.frame(df_matrix)
  colnames(df) <- c("Variable", "Mean", "SD",
                    "CI lower", "CI upper", "Obs")
  return(df)
}

# Effect and precision mean
summary(dataset$pcc_w)
summary(dataset$se_precision_w)

# Calculate the confidence intervals for both weighted and non-weighted means
(CI_summary <- CI(dataset, summary_stats))
(CI_w_summary <- CI(dataset, summary_stats, weight = TRUE))

# Tools for other descriptive statistics
desc_cols <- c("pcc_w", "se_pcc_w", "effect_gpa", "effect_charity", "effect_game", "effect_work", 
               "effect_positive", "ols_method", "logit_method",
               "probit_method", "tobit_method", "fe_method", "re_method", "diff_method", "other_method", "data_cross",
               "data_panel", "time_horizon", "data_avgyear", "n_obs", "location_lab", "location_field", "journal_impact",
               "study_citations", "crowding_out", "framing_pos", "framing_neg", "reward_scaled", "all_paid", "reward_own",
               "reward_else", "perf_quan", "perf_qual", "task_cog", "task_man", "task_app", "task_napp", "mot_alt",
               "mot_tru", "mot_rec", "mot_fai", "mot_mon", "subject_st", "subject_emp", "subject_mix", "male", "mid_age",
               "developed_country")

get_desc <- function(data, cols, expl) {
  means <- sapply(dataset[cols], mean)
  sds <- sapply(dataset[cols], sd)
  df <- data.frame(means, sds)
  colnames(df) <- c('Mean', 'SD')
  return(df)
}

# A data frame of these descriptive statistics (mean+SD) for the desired columns
(desc_stats <- get_desc(dataset, desc_cols, not_explicit))

########
# Plots
########

# Defining a custom plot theme

plot_theme <- function() {
  theme(
    axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
    axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),
    panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
    plot.background = element_rect(fill = "#DCEEF3", colour = "#DCEEF3")
  )
}
theme_set(plot_theme())


### Funnel plot ###

outlier_lower_bound <- -0.38851
outlier_upper_bound <- 0.525172
filter_pcc_w <- (dataset$pcc_w > outlier_lower_bound & dataset$pcc_w < outlier_upper_bound) #Excluding outliers from the graph

(funnel_win <- ggplot(data = dataset[filter_pcc_w,], aes(x = pcc_w[filter_pcc_w], y = se_precision_w[filter_pcc_w])) + 
    geom_point(colour = "#0d4ed1") + 
    geom_vline(aes(xintercept = mean(pcc_w)), color = "red", size = 0.5) +
    labs(title = NULL, x = "Partial correlation coefficient", y = "Precision of the estimate (1/SE)"))

### Box plot PCC ###

(study_PCC <- ggplot(data = dataset, aes(x = pcc_w, y=factor(study, levels = rev(levels(factor(study)))))) +
    geom_boxplot(outlier.colour = "#005CAB", outlier.shape = 21, outlier.fill = "#005CAB", fill="#e6f3ff", color = "#0d4ed1") +
    geom_vline(aes(xintercept = mean(pcc_w)), color = "red", size = 0.5) +
    labs(title = NULL,x="Estimate of the PCC between rewards and performance", y = NULL))


#############################################################
#############################################################
# PUBLICATION BIAS - FAT-PET (Stanley, 2005)
#############################################################
#############################################################

# OLS

OLS <- lm(formula = pcc_w ~ se_pcc_w, data = dataset)
OLS_id <- coeftest(OLS, vcov = vcovHC(OLS, type = "HC0", cluster = c(dataset$study_id))) #Clustering by study_id
print(OLS_id) #OLS, clustered

# FE
FE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = dataset, method = "FE")
FE_c <- coeftest(FE, vcov = vcov(FE, type = "fixed", cluster = c(dataset$study_id))) 
print(FE_c)

# Between
RE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = dataset, method = "REML")
RE_c <- coeftest(RE, vcov = vcov(RE, type = "fixed", cluster = c(dataset$study_id))) 
print(RE_c)

# Weighted by number of observations per study
OLS_w_study <- lm(formula = pcc_w ~ se_pcc_w, data = dataset, weight = (dataset$study_size*dataset$study_size))
OLS_w_study_c <- coeftest(OLS_w_study, vcov = vcovHC(OLS_w_study, type = "HC0", cluster = c(dataset$study_id))) 
print(OLS_w_study_c) #OLS weighted by study, clustered

# Weighted by precision
OLS_w_precision <- lm(formula = pcc_w ~ se_pcc_w, data = dataset, weight = c(dataset$se_precision_w*dataset$se_precision_w))
OLS_w_precision_c <- coeftest(OLS_w_precision, vcov = vcovHC(OLS_w_precision, type = "HC0", cluster = c(dataset$study_id))) 
print(OLS_w_precision_c) #OLS weighted by precision, clustered


#############################################################
#############################################################
# PUBLICATION BIAS - WAAP (Ioannidis et al., 2017)
#############################################################
#############################################################

WLS_FE_avg <- sum(dataset$pcc_w/dataset$se_pcc_w)/sum(1/dataset$se_pcc_w)
WAAP_bound <- abs(WLS_FE_avg)/2.8
WAAP_reg <- lm(formula = pcc_w ~ -se_precision_w, data = dataset[dataset$se_pcc_w<WAAP_bound,])
WAAP_reg_cluster <- coeftest(WAAP_reg, vcov = vcovHC(WAAP_reg, type = "HC0", cluster = c(dataset$study_id)))
print(WAAP_reg_cluster)

#############################################################
#############################################################
# PUBLICATION BIAS - TOP10 method (Stanley et al., 2010)
#############################################################
#############################################################


T10_bound <- quantile(dataset$se_precision_w, probs = 0.9) #Setting the 90th quantile bound
T10_reg <- lm(formula = pcc_w ~ -se_precision_w, data = dataset[dataset$se_precision_w>T10_bound,]) #Regression using the filtered data
T10_reg_cluster <- coeftest(T10_reg, vcov = vcovHC(T10_reg, type = "HC0", cluster = c(dataset$study_id)))
print(T10_reg_cluster)


#############################################################
#############################################################
# PUBLICATION BIAS - Stem-based method in R (Furukawa, 2019)
#############################################################
#############################################################


source("stem_method.R") #github.com/Chishio318/stem-based_method

est_stem <- stem(dataset$pcc_w, dataset$se_pcc_w, param)
est_stem$estimates
funnels_stem <- stem_funnel(dataset$pcc_w, dataset$se_pcc_w, est_stem$estimates) #For more detail see link above


#############################################################
#############################################################
# PUBLICATION BIAS - FAT-PET hierarchical in R
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
# PUBLICATION BIAS - Selection model (Andrews & Kasy, 2019)
#############################################################
#############################################################

# Source:
# https://maxkasy.github.io/home/metastudy/
# We used data found on the sheet "selection_model" in the appended data set (winsorization 1%)


#############################################################
#############################################################
# PUBLICATION BIAS - Endogenous kink (Bom & Rachinger, 2020)
#############################################################
#############################################################

# Stata code appended below



###### Relaxing the exogeneity assumption between ESTIMATE and SE (ESTIMATE) ######


#########################################################################
#########################################################################
# PUBLICATION BIAS - FAT-PET with IV
#########################################################################
#########################################################################

instrument <- 1/sqrt(dataset$n_obs)
IV_reg1 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg1, vcov = vcovHC(IV_reg1, cluster = c(dataset$study_id)), diagnostics = TRUE)

instrument <- 1/(dataset$n_obs)
IV_reg2 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg2, vcov = vcovHC(IV_reg2, cluster = c(dataset$study_id)), diagnostics = TRUE)

instrument <- 1/(dataset$n_obs^2)
IV_reg3 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg3, vcov = vcovHC(IV_reg3, cluster = c(dataset$study_id)), diagnostics = TRUE)

instrument <- log(dataset$n_obs)
IV_reg4 <- ivreg(formula = pcc_w ~ se_pcc_w | instrument, data = dataset)
summary(IV_reg4, vcov = vcovHC(IV_reg4, cluster = c(dataset$study_id)), diagnostics = TRUE)



#########################################################################
#########################################################################
# PUBLICATION BIAS - p-uniform* (van Aert & van Assen, 2019) - code for R
#########################################################################
#########################################################################

# Groundwork
study_levels_h <- levels(as.factor(dataset$study))
nreg_h <- length(study_levels_h)

# Vector of medians for pcc_w
med_pcc_w <- c(rep(NA,nreg_h))
for (i in 1:nreg_h) {
  y <- median(dataset$pcc_w[dataset$study == levels(as.factor(dataset$study))[i]])
  med_pcc_w[i] <- y
}

# Vector of medians for se_pcc_w
med_se_pcc_w <- c(rep(NA,nreg_h))
for (i in 1:nreg_h) {
  y <- median(dataset$se_pcc_w[dataset$study == levels(as.factor(dataset$study))[i]])
  med_se_pcc_w[i] <- y
}


# Maximum likelihood
p_uni_est <- puniform(yi = med_pcc_w, vi = med_se_pcc_w^2, side = "right", method = "ML",
                      alpha = 0.05)
print(p_uni_est)



#########################################################################
#########################################################################
# PUBLICATION BIAS - Caliper test (Gerber & Malhotra, 2008)
#########################################################################
#########################################################################


filter2 <- (dataset$t_adjusted_w>-4 & dataset$t_adjusted_w<19.692) #Removing the outliers from the graph

# histogram of t-statistic distribution
(t_hist <- ggplot(data = dataset[filter2,], aes(x = t_adjusted_w[filter2], y = ..density..)) +
    geom_histogram(color = "black", fill = "#1261ff", bins = "80") +
    geom_vline(aes(xintercept = mean(t_adjusted_w)), color = "dark orange", linetype = "dashed", size = 0.7) + 
    geom_vline(aes(xintercept = -1.96), color = "red", size = 0.5) +
    geom_vline(aes(xintercept = 1.96), color = "red", size = 0.5) +
    labs(x = "T-statistic", y = "Density")) +
  scale_x_continuous(breaks = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20), label = c(-4, -1.96, 0, 1.96, 4, 8, 12, 16, 20)) 


# 1.96 bound Caliper tests

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


# -1.96 bound Caliper tests

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
# HETEROGENEITY - Bayesian Model Averaging in R
#########################################################################
#########################################################################

BMA_data <- read_xlsx("data_cala_et_al.xlsx", sheet = 'bma')

####################################
# Winsorization for the new data set
####################################
BMA_data$pcc <- Winsorize(x = BMA_data$pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
BMA_data$se_pcc <- Winsorize(x = BMA_data$se_pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))

BMA_num <- data.frame(BMA_data, stringsAsFactors = TRUE) #Converting to a data frame
summary(BMA_num)


# Testing for VIF
BMA_formula <- as.formula(paste("pcc",paste(colnames(BMA_num)[-1],sep="", collapse = "+"), sep="~",collapse = NULL))
BMA_reg_test <- lm(formula = BMA_formula, data = BMA_num)
car::vif(BMA_reg_test) #VIF coefficients


# Adding names for clarity of the BMA figure
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
#### BMA1 ####
###############

BMA1 = bms(BMA_num, burn=1e5,iter=3e5, g="UIP", mprior="uniform", nmodel=50000,mcmc="bd", user.int =FALSE) 
print(BMA1)

# Extracting the coefficients and plotting the results
coef(BMA1,order.by.pip= F, exact=T, include.constant=T)
image(BMA1, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #Time consuming

summary(BMA1)
plot(BMA1)
print(BMA1$topmod[1])


###############
#### BMA2 ####
###############

BMA2= bms(BMA_num, burn=1e5,iter=3e5, g="UIP", mprior="dilut", nmodel=50000, mcmc="bd", user.int=FALSE) 
print(BMA2)

# Extracting the coefficients and plotting the results
coef(BMA2,order.by.pip= F, exact=T, include.constant=T)
image(BMA2, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA2)
plot(BMA2)
print(BMA2$topmod[1])


###############
#### BMA3 ####
###############

BMA3 = bms(BMA_num, burn=1e5,iter=3e5, g="BRIC", mprior="random", nmodel=50000,mcmc="bd", user.int =FALSE) 
print(BMA3)

# Extracting the coefficients and plotting the results
coef(BMA3,order.by.pip= F, exact=T, include.constant=T)
image(BMA3, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA3)
plot(BMA3)
print(BMA3$topmod[1])


###############
#### BMA4 ####
###############

# Time consuming
BMA4 = bms(BMA_num, burn=1e5,iter=3e5, g="HQ", mprior="random", nmodel=50000,mcmc="bd", user.int =FALSE) 
print(BMA4)

# Extracting the coefficients and plotting the results
coef(BMA4,order.by.pip= F, exact=T, include.constant=T)
image(BMA4, yprop2pip=FALSE,order.by.pip=TRUE, do.par=TRUE, do.grid=TRUE, do.axis=TRUE, xlab = "", main = "") #takes time, beware

summary(BMA4)
plot(BMA4)
print(BMA4$topmod[1])


# Plotting variables x PIP for each of the models
plotComp("UIP and Dilut"=BMA1, "UIP and Uniform"=BMA2,"BRIC and Random"=BMA3,"HQ and Random"=BMA4, add.grid=F,cex.xaxis=0.7)


# Plotting the correlation
col<- colorRampPalette(c("red", "white", "blue"))
M <- cor(BMA_num)
corrplot.mixed(M, lower = "number", upper = "circle", lower.col=col(200), upper.col=col(200),tl.pos = c("lt"),
               diag = c("u"), tl.col="black", tl.srt=70, tl.cex=0.85, number.cex = 0.5,cl.cex=0.8, cl.ratio=0.1)


#########################################################################
#########################################################################
# HETEROGENEITY - Frequentist model averaging code for R (Hansen)
#########################################################################
#########################################################################

#Requires the second BMA model (BMA2) to be loaded in the environment

# Using BMA_num from the previous section
x.data <- BMA_num[,-1]

# Reordering the columns in accordance to BMA2
BMA2_c <- coef(BMA2,order.by.pip= T, exact=T, include.constant=T) #Loading the matrix sorted by PIP
FMA_order <- c(0)
for (i in 1:nrow(BMA2_c)-1){
  FMA_order[i] <- BMA2_c[i,5]
}
x.data <- x.data[,c(FMA_order)] #Ordering the data

const_<-c(1)
x.data <-cbind(const_,x.data) #This gives us the data set in the desired form


# Groundwork
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


# Calculations
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
t[MMA.fls$Coefficient == 0,] <- 0 #Fixing value types
MMA.fls$pv <-round((1-apply(as.data.frame(apply(t,1,abs)), 1, pnorm))*2,3)
MMA.fls$pv[MMA.fls$pv == 1] <- 0 #Fixing value types
MMA.fls$names <- rownames(MMA.fls)
names <- c(colnames(BMA_num))
names <- c(names,"const_")
MMA.fls <- MMA.fls[match(names, MMA.fls$names),]
MMA.fls$names <- NULL
MMA.fls[-1,]


####################################
###### Best-practice estimate ######
####################################


# Loading the data into the desired form <- same procedure as in the BMA approach

BMA_data <- read_xlsx("data_cala_et_al.xlsx", sheet = 'bma')
BMA_data$pcc <- Winsorize(x = BMA_data$pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
BMA_data$se_pcc <- Winsorize(x = BMA_data$se_pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))

BMA_num <- data.frame(BMA_data, stringsAsFactors = TRUE)
BMA_formula <- as.formula(paste("pcc",paste(colnames(BMA_num)[-1],sep="", collapse = "+"), sep="~",collapse = NULL))

# Calculate BMA descriptive stats for BPE

# Calculate a single stat
calcStat <- function(data, ftype){
  temp_list <- lapply(data, ftype)
  temp_num <- unlist(temp_list)
  return(data.frame(unname(temp_num)))
}

# Calculate all stats and return them as a data frame
calcStats <- function(data){
  varnames <- colnames(data)
  temp <- list()
  desc_stats <- c('mean', 'min', 'max', 'sd')
  for(i in desc_stats){
    stat <- calcStat(data, i)
    temp <- append(temp, stat)
  }
  df <- data.frame(temp)
  colnames(df) <- c('Mean', 'Min', 'Max', 'SD')
  rownames(df) <- varnames
  return(df)
}

BMA_desc <- calcStats(BMA_data)
BMA_desc


# The actual estimation

bpe <- lm(formula = BMA_formula, data = BMA_num) #Constructing an OLS model
names(coef(bpe))


### Author ###
form_author <- "(Intercept) + 0.333*effect_gpa + 0.281*effect_charity + 0.277*effect_game + 0.868*effect_positive + 
  0.570*ols_method + 0.047*logit_method + 0.090*probit_method + 0.030*tobit_method + 
  0.039*fe_method + 0.028*re_method + 0.027*diff_method + 7.610*data_avgyear + 
  0.223*lab_control + 0.487*crowding_out + 14.975*journal_impact + 8.084*study_citations + 
  0.831*pos_framing_control + 0.599*reward_scaled + 0.741*all_paid + 0.808*reward_own_control + 
  0.702*perf_quan_control + 0.705*task_cog_control + 0.491*task_app + 0.291*mot_alt + 
  0.103*mot_rec + 0.151*mot_fai + 0.610*subject_st + 0.072*subject_emp + 0.528*gender_control + 
  2.931*mid_age + 0.835*developed_country = 0"
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

