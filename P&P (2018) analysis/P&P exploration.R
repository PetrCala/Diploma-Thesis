########################################################
##### Psacharopoulos & Patrinos (2018) exploration #####
########################################################

### Packages ###
library('readxl')


### Data preparation ###
setwd("C:/Users/hso20/OneDrive/Plocha/IES/Diploma-Thesis/P&P (2018) analysis")

data <- read_excel('Data transformed for R.xlsx', sheet = 'Data')
#View(data)


### Data exploration ###
mp_overall_data <- data[!is.na(data$mp_overall),]
test_formula <- 'mp_overall ~ schooling_years'
test_reg <- lm(formula = test_formula, data = mp_overall_data)
