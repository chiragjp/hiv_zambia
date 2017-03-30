
********************************************************************************
  
clear all
set more off
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
local files_directory = "~/Dropbox/HIV xwas work/data/"

**********************************************************
import delimited "`project_directory'/univariate_07.csv", asdouble clear
rename replicated_univariate_07 replicated
rename conf_int_string_univariate_07 or_ci
rename variable_univariate_07 variable
rename pvalue_univariate_07 pval
keep variable or_ci replicated pval var_label value_label
gen var07=1

 tempfile uni07
 save `uni07', replace
 
import delimited "`project_directory'/univariate_13.csv", asdouble clear
rename replicated_univariate_13 replicated
rename conf_int_string_univariate_13 or_ci
rename variable_univariate_13 variable
rename pvalue_univariate_13 pval
keep variable or_ci replicated pval var_label value_label
gen var07=0
 
append using `uni07'

gen parse1=strpos(or_ci,"[")
gen or=substr(or_ci,1,parse1-1)
destring or, replace
gen logor=log(or)
gen logpval=-log(pval)

keep if replicated==1

keep variable var_label value_label var07 or logor logpval
reshape wide or logor logpval, i(variable var_label value_label) j(var07)
gen inboth=(or0!=. & or1!=.)
gen p_tot= (logpval0+ logpval1)/2
replace p_tot= logpval0 if p_tot==.
replace p_tot= logpval1 if p_tot==.
gsort -inboth -p_tot

keep if inboth==1
gen analysis_num=1
tempfile inall
save `inall', replace
*********************************** end of univariate
*********************************** start ex ante


********************************************************************************
  
clear all
set more off
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
local files_directory = "~/Dropbox/HIV xwas work/data/"

**********************************************************
import delimited "`project_directory'/apriori_adj_07.csv", asdouble clear
rename replicated_apriori_adj_07 replicated
rename conf_int_string_apriori_adj_07 or_ci
rename variable_apriori_adj_07 variable
rename pvalue_apriori_adj_07 pval
keep variable or_ci replicated pval var_label value_label
gen var07=1

tempfile exante07
save `exante07', replace
 
import delimited "`project_directory'/aprior_adj_13.csv", asdouble clear
rename replicated_aprior_adj_13 replicated
rename conf_int_string_aprior_adj_13 or_ci
rename variable_aprior_adj_13 variable
rename pvalue_aprior_adj_13 pval
keep variable or_ci replicated pval var_label value_label
gen var07=0
 
append using `exante07'

gen parse1=strpos(or_ci,"[")
gen or=substr(or_ci,1,parse1-1)
destring or, replace
gen logor=log(or)
gen logpval=-log(pval)

keep if replicated==1

keep variable var_label value_label var07 or logor logpval
reshape wide or logor logpval, i(variable var_label value_label) j(var07)
gen inboth=(or0!=. & or1!=.)
gen p_tot= (logpval0+ logpval1)/2
replace p_tot= logpval0 if p_tot==.
replace p_tot= logpval1 if p_tot==.
gsort -inboth -p_tot

keep if inboth==1
gen analysis_num=2
append using `inall'
save `inall', replace


*********************************** end of ex ante
*********************************** start ex post

clear all
set more off
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
local files_directory = "~/Dropbox/HIV xwas work/data/"

**********************************************************
import delimited "`project_directory'/super_07.csv", asdouble clear
rename replicated_super_07 replicated
rename conf_int_string_super_07 or_ci
rename variable_super_07 variable
rename pvalue_super_07 pval
keep variable or_ci replicated pval var_label value_label
gen var07=1

tempfile expost07
save `expost07', replace
 
import delimited "`project_directory'/super_13.csv", asdouble clear
rename replicated_super_13 replicated
rename conf_int_string_super_13 or_ci
rename variable_super_13 variable
rename pvalue_super_13 pval
keep variable or_ci replicated pval var_label value_label
gen var07=0
 
append using `expost07'

gen parse1=strpos(or_ci,"[")
gen or=substr(or_ci,1,parse1-1)
destring or, replace
gen logor=log(or)
gen logpval=-log(pval)

keep if replicated==1

keep variable var_label value_label var07 or logor logpval
reshape wide or logor logpval, i(variable var_label value_label) j(var07)
gen inboth=(or0!=. & or1!=.)
gen p_tot=(logpval0+ logpval1)/2
replace p_tot= logpval0 if p_tot==.
replace p_tot= logpval1 if p_tot==.
gsort -inboth -p_tot

keep if inboth==1
gen analysis_num=3
append using `inall'
save `inall', replace

duplicates tag variable, gen(num_of_analyses)
replace num_of_analyses=num_of_analyses+1
bys variable: egen p_tot_tot=total(p_tot)
gsort -num_of -p_tot_tot analysis_num

*collapse (sum) analysis_num (mean) num_of_analyses or0 or1 p_tot, by(variable var_label value_label)
collapse (sum) analysis_num (mean) num_of_analyses or0 or1 logpval0 logpval1, by(variable var_label value_label)

gsort -num_of_analyses -analysis_num -logpval0

gen x1=length(variable)
gen x2=substr(variable,x1,1)
gen x3=strpos(variable,"_")>0
replace value_label=x2 if value_label=="" & x3==1
drop x*

gen Univariate=""
gen Ex_ante=""
gen Ex_post=""

replace Univariate="Y" if analysis_num==6
replace Ex_ante="Y" if analysis_num==6
replace Ex_post="Y" if analysis_num==6

replace Univariate="N"  if analysis_num==5
replace Ex_ante="Y" if analysis_num==5
replace Ex_post="Y" if analysis_num==5

replace Univariate="Y" if analysis_num==4
replace Ex_ante="N" if analysis_num==4
replace Ex_post="Y" if analysis_num==4

replace Univariate="N" if analysis_num==3 & num_of_analyses==1
replace Ex_ante="N" if analysis_num==3 & num_of_analyses==1
replace Ex_post="Y" if analysis_num==3 & num_of_analyses==1

replace Univariate="Y" if analysis_num==3 & num_of_analyses==2
replace Ex_ante="Y" if analysis_num==3 & num_of_analyses==2
replace Ex_post="N" if analysis_num==3 & num_of_analyses==2

replace Univariate="N" if analysis_num==2 
replace Ex_ante="Y" if analysis_num==2 
replace Ex_post="N" if analysis_num==2 

replace Univariate="Y"  if analysis_num==1
replace Ex_ante="N" if analysis_num==1
replace Ex_post="N" if analysis_num==1

gen or7=round(or1,0.1)
gen or13=round(or0,0.1)
gen p7=round(logpval1,0.1)
gen p13=round(logpval0,0.1)

gen mean07=string(or7, "%9.1f")+" ("+string(p7)+")"
gen mean13=string(or13, "%9.1f")+" ("+string(p13)+")"
gsort -analysis_num -num_of_analyses variable

order var_label value_label Univariate Ex_ante Ex_post mean07 mean13
keep var_label value_label Univariate Ex_ante Ex_post mean07 mean13
