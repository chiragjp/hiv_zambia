
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
replace var_label="number of >5yo children" if var_label=="index last child prior to maternity-health (calendar)"
replace var_label=subinstr(var_label,"year","yr",.)
replace var_label=subinstr(var_label,"number","#",.)
replace var_label=subinstr(var_label,"respondent's","woman's",.)
keep variable or_ci replicated pval var_label
gen var07=1

 tempfile uni07
 save `uni07', replace
 
import delimited "`project_directory'/univariate_13.csv", asdouble clear
rename replicated_univariate_13 replicated
rename conf_int_string_univariate_13 or_ci
rename variable_univariate_13 variable
rename pvalue_univariate_13 pval
replace var_label="number of >5yo children" if var_label=="index last child prior to maternity-health (calendar)"
replace var_label=subinstr(var_label,"year","yr",.)
replace var_label=subinstr(var_label,"number","#",.)
replace var_label=subinstr(var_label,"respondent's","woman's",.)
keep variable or_ci replicated pval var_label
gen var07=0
 
append using `uni07'

gen parse1=strpos(or_ci,"[")
gen or=substr(or_ci,1,parse1-1)
destring or, replace
gen logor=log(or)
gen logpval=-log(pval)

gsort var07 -logpval
gen label=1 in 1/10
gsort -var07 -logpval
replace label=1 in 1/10

gen varlab_short=substr(var_label,1,23)

twoway (scatter logpval logor if var07==1 & replicated==0, mcolor(navy) ///
msize(vsmall) msymbol(circle_hollow) mlwidth(vthin)) ///
(scatter logpval logor if var07==1 & replicated==1, mcolor(navy) msize(tiny)) ///
(scatter logpval logor if var07==0 & replicated==0, mcolor(cranberry) ///
msize(vsmall) msymbol(diamond_hollow) mlwidth(vthin)) ///
(scatter logpval logor if var07==0 & replicated==1, mcolor(cranberry) msize(tiny) ///
msymbol(diamond)) ///
(scatter logpval logor if var07==1 & replicated==1  & label==1, mcolor(navy) ///
msize(tiny) mlabel(varlab_short) mlabsize(tiny) mlabposition(6) mlabcolor(navy)) ///
(scatter logpval logor if var07==0 & replicated==1 & label==1, mcolor(cranberry) ///
msize(tiny) msymbol(diamond) mlabel(varlab_short) mlabsize(tiny) mlabposition(6) mlabcolor(cranberry)), ///
ytitle(- log p-value) xtitle(log overall OR) xlabel(-3(1)3) ///
legend(order(1 "2007 Not replicated" 2 "2007 Replicated" 3 "2013 Not replicated" 4 "2013 Replicated") ///
rows(2)) graphregion(fcolor(white))  ylabel(, nogrid) xsize(9) ysize(9)

