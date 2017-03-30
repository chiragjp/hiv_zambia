
********************************************************************************
  
clear all
set more off
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
local files_directory = "~/Dropbox/HIV xwas work/data/"

**********************************************************

import delimited "`project_directory'/apriori_replicated_07_13.csv", clear

rename variable_apriori_adj_07 variable
gen consistent=1
keep variable consistent

tempfile superset
save "`superset'", replace

import delimited "`project_directory'/differences/age_diffs_apriori.csv", clear
rename variable_univariate_age1_07 variable

merge 1:1 variable using "`superset'"
replace consistent=0 if _merge==3
keep variable consistent
save "`superset'", replace

import delimited "`project_directory'/differences/poor_rich_diffs_apriori.csv", clear
rename variable_univariate_poor_07 variable

merge 1:1 variable using "`superset'"
replace consistent=0 if _merge==3
keep variable consistent
save "`superset'", replace

import delimited "`project_directory'/differences/tested_nevertested_diffs_apriori.csv", clear
rename variable_univariate_nevertested_ variable

merge 1:1 variable using "`superset'"
replace consistent=0 if _merge==3
keep variable consistent
save "`superset'", replace

import delimited "`project_directory'/differences/wealth_diffs_apriori.csv", clear
rename variable_univariate_wealth1_07 variable

merge 1:1 variable using "`superset'"
replace consistent=0 if _merge==3
keep variable consistent
save "`superset'", replace

drop if consistent==0
save "`project_directory'/consistent in all strata.dta", replace 
