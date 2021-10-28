clear all
prog drop _all
capture log close
set more off

global datadir "/Users/victorporcelli/Desktop/PADM.2902 work/Project/Dataset"
global logdir "/Users/victorporcelli/Desktop/PADM.2902 work/Project/LogFile"

log using "$logdir/ProjectLog log_new.smcl", replace
use "$datadir/hrsdata_mr.dta", clear

*drop observations of individuals younger than 50
drop if agey <50

*drop observations without the dependent variable
drop if cesd == .

*adjust cesd to be an indicator of depressed vs not depressed, using 6 as the
*cutoff
gen depressed = cesd
replace depressed = 0 if depressed < 6
replace depressed = 1 if depressed >= 6

*convert gender into a 0-1 dummy
replace gender =0 if gender ==1
replace gender =1 if gender ==2

*create a variable for living alone using hhres
gen livalone = 0
replace livalone = 1 if hhres ==1

*adjust race to be white vs non-white
replace race =0 if race ==1
replace race =1 if race ==2 | race ==3

*replace the missing values in incomplete variables
gen hispan_mis = missing(hispan)
gen a = runiform()
replace hispan = 1 if a >= 0.8993 & missing(hispan)
replace hispan = 0 if a < 0.8993 & missing(hispan)

gen vetrn_mis = (vetrn==.)
gen b = runiform()
replace vetrn = 1 if b >= 0.8146 & missing(vetrn)
replace vetrn = 0 if b < 0.8146 & missing(vetrn)

gen smokev_mis = (smokev==.)
gen c = runiform()
replace smokev = 1 if c >= 0.4558 & missing(smokev)
replace smokev = 0 if c < 0.4558 & missing(smokev)

gen work_mis = (work==.)
gen e = runiform()
replace work = 1 if e >= 0.6714 & missing(work)
replace work = 0 if e < 0.6714 & missing(work)

gen peninc_mis = (peninc==.)
gen f = runiform()
gen pension = peninc
replace pension = 1 if f >= 0.7097 & missing(pension)
replace pension = 0 if f < 0.7097 & missing(pension)

gen educ_mis = (educ==.)
egen educMean = mean(educ)
replace educ = educMean if missing(educ)

gen hchild_mis = (hchild==.)
gen numchildren = hchild
egen hchildMean = mean(numchildren) 
replace numchildren=hchildMean if missing(numchildren)

gen bmi_mis = (bmi==.)
egen bmiMean = mean(bmi)
replace bmi = bmiMean if missing(bmi)

gen diab_mis = (diab_e==.)
gen diabetes = diab_e
gen i = runiform()
replace diabetes = 1 if i >= 0.7655 & missing(diabetes)
replace diabetes = 0 if i < 0.7655 & missing(diabetes)

gen cancr_mis = (cancr_e==.)
gen cancer = cancr_e
gen j = runiform()
replace cancer = 1 if j >= 0.8365 & missing(cancer)
replace cancer = 0 if j < 0.8365 & missing(cancer)

gen heart_mis = (heart_e==.)
gen k = runiform()
gen heartprob = heart_e
replace heartprob = 1 if k >= 0.741 & missing(heartprob)
replace heartprob = 0 if k < 0.741 & missing(heartprob)

gen livsib_mis = (livsib==.)
gen numsiblings = livsib
egen livsibMean = mean(numsiblings)
replace numsiblings = livsibMean if missing(numsiblings)

gen arthr_mis = (arthr_e==.)
gen arthritis = arthr_e
gen p = runiform()
replace arthritis = 1 if p >= 0.3496 & missing(arthritis)
replace arthritis = 0 if p < 0.3496 & missing(arthritis)

gen shlt_mis = (shlt==.)
bysort hhidpn: egen avg_shlt=mean(shlt)  
replace shlt=avg_shlt if missing(shlt)

*convert shlt so 1 is poor and 5 is excellent
gen genhealth = 6-shlt

*replace missing values in mstat, and make it a 0-1 dummy where 1 is a person
*who is married or with a partner
gen mstat_mis = (mstat==.)
gen partnered = mstat
replace partnered = 1 if partnered ==3
replace partnered =0 if partnered !=1

*make iearn variable name more understandable, and convert its unit to thousands
*of dollars
gen income = iearn/1000

*make income .001 if 0 to be able to log it
replace income = 0.001 if income == 0

*export summary statistics
outreg2 using sumstats.doc, replace sum(log) keep(depressed gender educ vetrn ///
partnered hispan race income pension work genhealth smokev heartprob arthritis ///
diabetes cancer bmi livalone numchildren numsiblings i.year) ///
title("Descriptive statistics on health and demographic characteristics of individuals surveyed by the RAND Health and Retirement Study from 2008-2016.") ///
noobs addnote("N=47,554 \n Note: There were missing values for the following variables:  hispan, vetrn, smokev, work, pension, educ, genhealth, numchildren, bmi, diabetes, cancer, heartprob, numsiblings, arthritis, partnered. A detailed explanation of the methods used to complete the dataset for this study is located in Appendix A.") ///

*first regression (Bivariate)
reg depressed gender i.year

*perform the white test for heteroskedasticity
estat imtest, white

*second regression (Controls) with missing values
reg depressed gender educ vetrn partnered hispan race income pension work ///
genhealth smokev heartprob arthritis diabetes cancer bmi livalone numchildren ///
numsiblings i.year hispan_mis vetrn_mis educ_mis smokev_mis hchild_mis ///
shlt_mis peninc_mis work_mis bmi_mis diab_mis cancr_mis heart_mis livsib_mis

*second regression (Controls) without missing values
reg depressed gender educ vetrn partnered hispan race income pension work ///
genhealth smokev heartprob arthritis diabetes cancer bmi livalone numchildren ///
numsiblings i.year

*perform the white test for heteroskedasticity
estat imtest, white

*add in incsq to account for diminishing returns
gen incsq = income*income

*generate interactions
gen geninc = gender*income
gen genincsq = gender*incsq
gen nwhispan = hispan*race
gen support = partnered*numchildren
gen workeduc = work*educ

*third regression (Functional Form)
reg depressed gender geninc genincsq educ vetrn partnered support hispan race ///
nwhispan income incsq pension work workeduc genhealth smokev heartprob ///
arthritis diabetes cancer bmi livalone numchildren numsiblings i.year

*perform the white test for heteroskedasticity
estat imtest, white

*conduct F-tests to check for joint significance among interactions
test gender geninc
test geninc income
test gender genincsq 
test genincsq incsq

test hispan nwhispan
test nwhispan race

test numchildren support
test support partnered

test work workeduc
test workeduc educ

*add robust SEs to adjust for heteroskedasticity
reg depressed gender i.year, r

outreg2 using myreg.doc, replace title("Regression models predicting mild to severe depression by gender in individuals 50 years of age and older.") ///
ctitle(Bivariate) addtext(Year FE, YES, Individual FE, NO) nor2 drop(i.year)

reg depressed gender educ vetrn partnered hispan race income pension work ///
genhealth smokev heartprob arthritis diabetes cancer bmi livalone numchildren ///
numsiblings i.year, r

outreg2 using myreg.doc, append ctitle(Controls) drop(i.year) ///
addtext(Year FE, YES, Individual FE, NO) nor2 

reg depressed gender geninc genincsq educ vetrn partnered support hispan race ///
nwhispan income incsq pension work workeduc genhealth smokev heartprob ///
arthritis diabetes cancer bmi livalone numchildren numsiblings i.year, r

*look at vif scores to look for multicollineaarity
vif

*conduct F-tests to check for joint significance among interactions
test gender geninc
test geninc income
test gender genincsq 
test genincsq incsq

test hispan nwhispan
test nwhispan race

test numchildren support
test support partnered

test work workeduc
test workeduc educ

outreg2 using myreg.doc, append ctitle(Functional Form) nor2 drop(i.year) ///
addtext(Year FE, YES, Individual FE, NO)

*fourth regression  (Fixed effects) without time invariant variables
xtset hhidpn year
xtreg depressed geninc genincsq partnered support income incsq pension work ///
workeduc genhealth bmi livalone numsiblings i.year, fe i (hhidpn) r

*conduct F-tests to check for joint significance among interactions
test geninc income
test genincsq incsq
test geninc genincsq

test support partnered

test work workeduc

outreg2 using myreg.doc, append ctitle(Fixed Effects) nor2 drop(i.year) ///
addtext(Year FE, YES, Individual FE, YES)

log close 

translate "$logdir/ProjectLog log_new.smcl" "$logdir/ProjectLog.pdf", replace ///
fontsize(9) lmargin(.5) rmargin(.5) tmargin(.75) bmargin(.75)
