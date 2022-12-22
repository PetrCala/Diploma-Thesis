########################################################
####### Advanced Econometrics 21/22 WS - project #######
########################################################

### Packages ###
library(readxl)
library(lmtest) #coeftest
library(metafor) #FE/RE
library(AER) #IV regression
library(stargazer) #LaTeX output

#If any of the packages above are not installed on your device,
#simply install them with "install.packages('package_name')"

###########################
### Data pre-processing ###
###########################

setwd('wd_path') #The working directory should contain the source data set
setwd("C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis/P&P (2018) analysis")

#Read the data set and convert it to an R data frame
data_raw <- read_excel('Data transformed for R.xlsx', sheet = 'Data')
data <- data.frame(data_raw, stringsAsFactors = TRUE)

#Remove missing values and include only relevant columns
relevant_columns <- c('mp_overall', 'schooling_years', 'country',
                      'year', 'region', 'inc_level')
mp_clean <- data[!is.na(data$mp_overall) & !is.na(data$schooling_years), 
                 relevant_columns]

#Create a column 'obs' which denotes the number of occurrences in
# the data set for each country 
mp_clean$obs <- NA #Create an empty column
countries <- table(mp_clean$country) #Get the information
for (name in names(countries)) {
  value <- as.numeric(countries[name]) #Number of occurrences
  mp_clean[mp_clean$country == name,'obs'] <- value #Assign these
}

#Useful formulas
gen_formula <- 'mp_overall ~ schooling_years'
qdr_formula <- 'mp_overall ~ schooling_years + schooling_years^2'

##################
##### Models #####
##################

### Unweighted models ###

#A simple OLS
ols <- lm(formula = gen_formula, data = mp_clean)
ols_c <- coeftest(ols, vcov = vcov(ols, cluster = c(mp_clean$country)))

#Simple Fixed-effects
fe <- rma(mp_overall, sei = schooling_years, mods = ~schooling_years,
          data = mp_clean, method = "FE")
fe_c <- coeftest(fe,
                 vcov = vcov(fe, type = "fixed", cluster = c(mp_clean$country)))

#A simple regression with the quadratic term
nls <- nls(formula = qdr_formula, data = mp_clean,
           start = list(schooling_years = 0))
nls_c <- coeftest(nls, vcov = vcov(nls, cluster = c(mp_clean$country)))

stargazer(ols_c, fe_c, nls_c) #LaTeX table for the simple models


### Weighted models ###

#Weighted OLS
ols_w <- lm(formula = gen_formula, data = mp_clean,
            weight = (mp_clean$obs*mp_clean$obs))
ols_w_c <- coeftest(ols_w, vcov = vcov(ols_w, cluster = c(mp_clean$country)))

#Weighted Fixed-effects
fe_w <- rma(mp_overall, sei = schooling_years, mods = ~schooling_years,
        weights = (mp_clean$obs*mp_clean$obs), data = mp_clean, method = "FE")
fe_w_c <- coeftest(fe_w,
          vcov = vcov(fe_w, type = "fixed", cluster = c(mp_clean$country)))

#Weighted regression with the quadratic term
nls_w <- nls(formula = qdr_formula, data = mp_clean,
      start = list(schooling_years = 0), weight = (mp_clean$obs*mp_clean$obs))
nls_w_c <- coeftest(nls_w, vcov = vcov(nls_w, cluster = c(mp_clean$country)))

stargazer(ols_w_c, fe_w_c, nls_w_c) #LaTeX table for the weighted models






### IV regression ###

instrument <- 1/sqrt(mp_clean$obs) #instrument to be used as weight
iv_formula <- 'mp_overall ~ schooling_years | instrument'
IV_reg <- ivreg(formula = iv_formula, data = mp_clean)
summary(IV_reg, vcov = vcov(IV_reg, cluster = c(mp_clean$country)),
        diagnostics = TRUE) #Diagnostic tests included in the summary
