########################################################
##### Psacharopoulos & Patrinos (2018) exploration #####
########################################################

### Packages ###
library(readxl)
library(lmtest) #coeftest
library(metafor) #rma - FE/RE
library(plm) # FE/RE

#Adjust this later
library(readr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(stats)
library(DescTools)
library(sandwich)
library(multiwayvcov)
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


### Data preparation ###
#setwd("C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis/P&P (2018) analysis")
setwd("C:/Users/AU451FE/IES/Diploma-Thesis/P&P (2018) analysis")

data_raw <- read_excel('Data transformed for R.xlsx', sheet = 'Data')
data <- data.frame(data_raw, stringsAsFactors = TRUE) #converting to a data frame
#View(data)


### MP ###

#Remove missing values
relevant_columns <- c('mp_overall', 'schooling_years', 'country',
                      'year', 'region', 'inc_level')
mp_clean <- data[!is.na(data$mp_overall) & !is.na(data$schooling_years), 
                 relevant_columns]
gen_formula <- 'mp_overall ~ schooling_years'


### Replication of methods from the primary study ###

#Figure 1 - rate of return ~ country's year of schooling
#The basic regression should have coefficients 49.611 (intercept) and -0.020 (year)
#Pretty much the same regression as in fig 2, only with data from Annex 1 - not sure if want to use

#Figure 2 - rate of return ~ years of schooling (individual observations)
fig2_reg <- lm(formula = gen_formula, data = mp_clean)


### New methodology ###

#OLS where the standard errors are clustered on country level


OLS_country_se <- lm(formula = gen_formula, data = mp_clean)
OLS_country_se_c <- coeftest(OLS_country_se, vcov = vcovHC(OLS_country_se,
                            type = "HC0", cluster = c(mp_clean$country)))
print(OLS_country_se_c)



#Try running the between/within methods possibly

#within - fixed effects, random - randomeffects, between - between
#Read on the "effect = "twoways""

pdf <- pdata.frame(mp_clean, index = c("mp_overall", "schooling"))

fe <- plm(formula = gen_formula, data = mp_clean, model = "within")
fe_c <- coeftest(fe, vcov = vcov(fe, type = "fixed", cluster = c(mp_clean$country)))
#table(index(your_pdataframe), useNA = "ifany")

sum(is.na(mp_clean$mp_overall))


re <- 0
re_c <- coeftest(re, vcov = vcov(re, type = "fixed", cluster = c(mp_clean$country)))

we <- 0
we_c <- coeftest(we, vcov = vcov(we, type = "fixed", cluster = c(mp_clean$country)))
##Bachelor thesis code


fe <- rma(mp_overall, sei = schooling_years, mods = ~schooling_years,
          data = mp_clean, method = "FE")


#FE
FE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = dataset, method = "FE")
FE_c <- coeftest(FE, vcov = vcov(FE, type = "fixed", cluster = c(dataset$study_id))) 
print(FE_c)

#Between
RE <- rma(pcc_w, sei = se_pcc_w, mods = ~se_pcc_w, data = dataset, method = "REML")
RE_c <- coeftest(RE, vcov = vcov(RE, type = "fixed", cluster = c(dataset$study_id))) 
print(RE_c)


#Quadratic term formula #reconsider procedure

qdr_formula <- 'mp_overall ~ schooling_years + schooling_years^2'
test <- gmm(g = qdr_formula, x = mp_clean)
test





