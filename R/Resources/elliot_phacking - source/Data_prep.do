clear all
*Set your working directory below (it has to be the folder where you save 'Replication_package')


import excel "G:\My Drive\Risk Aversion\Code\elliot_phacking\RRA_Studies.xlsx", sheet("data") firstrow clear

//gen pvalue
drop if estimated==0
replace tstat = rra/se if tstat ==.
gen t_abs = abs(tstat)
replace t_abs = 0 if t_abs==.
gen ptop = 2*(1 - normal(t_abs))
drop if se==.
drop if ptop > 1

gen id = idstudy
gen econ =  econjournal
gen fin = finjournal

export delimited using "G:\My Drive\Risk Aversion\Code\elliot_phacking\RRA_Elliot.csv", replace
