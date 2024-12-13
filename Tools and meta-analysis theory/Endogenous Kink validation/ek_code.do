/* "A Kinked Meta-Regression Model for Publication Bias Correction" (Research Synthesis Methods, Vol.10, Issue 4, pp. 497-514, 2019)
    Pedro Bom and Heiko Rachinger
	STATA code for implementing the EK meta-estimator
    This version: May 2020
*/

* A. INSTRUCTIONS:

* 0. This code requires inputting a sample of estimates along with their standard errors. 
* 1. Create an Excel file with the estimates in the first column and the standard errors in the second column.
* 2. Label this data file data.xlsx and place it in the working directory specified in Stata (or change the working directory to where your file is stored). Very important: the data must be stored in the first sheet of the Excel file. If so, the code should run OK without any other changes.
* 3. If you want to give your data file any generic label ([your_file_name].xlsx), you must replace the default label (data.xlsx) by your new label ([your_file_name].xlsx) in line 38 below.
* 4. If you want to save your data using the extension .xls, you must replace the default extension .xlsx by the extension .xls in line 38 below.

* Note: This code is designed to deal with publication bias only on the positive side. Publication bias on the negative side can easily be accommodated by first reversing the sign of the estimates.

* B. OUTPUT:

* This code estimates the EK meta-regression:
* 	
*			bs = alpha1 + delta*(sebs-a)*I(sebs-a) + error, 
*
*	where 	bs = estimates
*			alpha1 = mean true effect
*			sebs = standard errors
*			a = estimated kink
*			I(sebs-a) = dummy variable that is 1 when sebs>a and 0 otherwise
*			delta = publication bias parameter
* 
* There are three possible output scenarios:
*	1. If a < min(sebs) ==> EK reduces to FAT-PET (Egger's regression). Output: meta-estimate of alpha1 and delta
*	2. If min(sebs) < a < max(sebs) ==> EK is a kinked meta-regression with kink at a. Output: meta-estimate of alpha1 and delta.
*	3. If a > max(sebs) ==> EK is a WLS regression of the estimates on a constant. Output: meta-estimate of alpha1.

quietly {
clear 

import excel "ek_data_cala.xlsx"
rename A bs
rename B sebs


log using quiet_noise.log, text replace
 
gen ones=1
   
sum
local M=r(N)
sum sebs
local sebs_min=r(min)
local sebs_max=r(max)
  
gen sebs2=sebs^2
gen wis=ones/sebs2
gen bs_sebs=bs/sebs
gen ones_sebs=ones/sebs

gen bswis=bs*wis
sum wis
local wis_sum=r(sum)


* FAT-PET
regress bs_sebs ones_sebs ones,noc
local pet=_b[ones_sebs]
local t1_linreg = (_b[ones_sebs]/_se[ones_sebs])
local b_lin=_b[ones_sebs]
local Q1_lin = e(rss)
di `t1_linreg'
local abs_t1_linreg = abs(`t1_linreg')
di `abs_t1_linreg'


* PEESE
regress bs_sebs ones_sebs sebs,noc
local peese=_b[ones_sebs]
local b_sq=_b[ones_sebs]
local Q1_sq = e(rss)
di `Q1_sq'
 

* FAT-PET-PEESE

if `abs_t1_linreg' > invt(`M-2', 0.975) {
    local combreg=`b_sq'
	local Q1=`Q1_sq'
	}
else {
    local combreg=`b_lin'
	local Q1=`Q1_lin'
}

* estimation of random effects variance component
local sigh2hat=max(0,`M'*((`Q1'/(`M'-e(df_m)-1))-1)/`wis_sum') 
local sighhat=sqrt(`sigh2hat') 


* Cutoff value for EK
if `combreg'>1.96*`sighhat' {
    local a1=(`combreg'-1.96*`sighhat')*(`combreg'+1.96*`sighhat')/(2*1.96*`combreg')
}
else {
	local a1=0
	}
    
	
	rename bs bs_original
	rename bs_sebs bs
	rename ones_sebs constant
	rename ones pub_bias
	
	
    noisily: display "EK regression: "

if `a1'>`sebs_min' & `a1'<`sebs_max' {
    gen sebs_a1=sebs-`a1' if sebs>`a1'
	replace sebs_a1=0 if sebs<=`a1'
	gen pubbias=sebs_a1/sebs
	noisily regress bs constant pubbias, noc
	local b0_ek=_b[constant]
	local b1_ek=_b[pubbias]
	local sd0_ek=_se[constant]
	local sd1_ek=_se[pubbias]
}
else if `a1'<`sebs_min' {
    noisily regress bs constant pub_bias, noc
	local b0_ek=_b[constant]
	local b1_ek=_b[pub_bias]
	local sd0_ek=_se[constant]
	local sd1_ek=_se[pub_bias]
}
else if `a1'>`sebs_max' {
    noisily regress bs constant, noc
	local b0_ek=_b[constant]
	local sd0_ek=_se[constant]	
}
noisily: display "EK's mean effect estimate (alpha1) and standard error:"
noisily: di `b0_ek' 
noisily: di `sd0_ek' 
noisily: display "EK's publication bias estimate (delta) and standard error:"
noisily: di `b1_ek' 
noisily: di `sd1_ek' 

log close
}
