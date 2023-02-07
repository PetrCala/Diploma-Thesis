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

data_raw <- read_xlsx("data_set_master_thesis_cala.xlsx", sheet = 'main', n_max = 67)

#####################
# Data transformation
#####################

data <- data_raw


#############################
#Winsorization and groundwork
#############################
data$pcc_w <- Winsorize(x = data$pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
data$se_pcc_w <- Winsorize(x = data$se_pcc, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
data$se_precision_w <- 1/data$se_pcc_w
data$t_w <- Winsorize(x = data$t_stat, minval = NULL, maxval = NULL, probs = c(0.01,0.99))
data$significant_w <- c(rep(0,nrow(data)))
data$significant_w[(data$t_w > 1.96) | (data$t_w < -1.96)] <- 1


#############################
#Summary statistics & Plots
#############################

#Summary statistics and the funnel plot for the winsorized data
summary(data$pcc_w)
summary(data$se_precision_w)

#filter_pcc_w <- (data$pcc_w> -0.38851 & data$pcc_w<0.525172) #Excluding the outliers from the graph
filter_pcc_w <- (data$pcc_w > -1 & data$pcc_w < 1)

#Funnel plot
(funnel_win <- ggplot(data = data[filter_pcc_w,], aes(x = pcc_w[filter_pcc_w], y = se_precision_w[filter_pcc_w])) + 
    geom_point(colour = "#0d4ed1") + 
    labs(title = NULL, x = "Partial correlation coefficient", y = "Precision of the estimate (1/SE)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
          axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"),
          panel.background = element_rect(fill = "white"), panel.grid.major.x = element_line(color = "#DCEEF3"),
          plot.background = element_rect(fill = "#DCEEF3")))

