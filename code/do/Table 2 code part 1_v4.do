/*
* Goal: create a table with: Var name, value, Univariate (07/13), Ex ante (07/13), and Ex post (07/13)
* for all variables (replicated at least once in both 07 and 13) (is this last piece important?)
Steps:
1. Take Univariate 07 and 13
2. Keep replicated in both
3. Merge with ex post and ex ante to get pvals etc
4. Repeat with ex ante that are not already in
5. Repeat with ex post that are not already in
6. Format

*/
********************************************************************************
  
clear all
set more off
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
local files_directory = "~/Dropbox/HIV xwas work/data/"

**********************************************************
import delimited "`project_directory'/univariate_07.csv", asdouble clear
rename variable_univariate_07 variable
rename conf_int_string_univariate_07 or_ci07
rename pvalue_univariate_07 pval07
rename replicated_univariate_07 replicated07

tempfile uni
save `uni', replace

import delimited "`project_directory'/univariate_13.csv", asdouble clear
rename variable_univariate_13 variable
rename conf_int_string_univariate_13 or_ci13
rename pvalue_univariate_13 pval13
rename replicated_univariate_13 replicated13

merge 1:1 variable using "`uni'"
drop _merge
keep variable or_ci13 pval13 replicated13 var_label value_label or_ci07 pval07 replicated07
order variable  var_label value_label or_ci07 pval07 replicated07 or_ci13 pval13 replicated13
gen dataset=1
save `uni', replace

import delimited "`project_directory'/apriori_adj_07.csv", asdouble clear
rename variable_apriori_adj_07 variable
rename conf_int_string_apriori_adj_07 or_ci07
rename pvalue_apriori_adj_07 pval07
rename replicated_apriori_adj_07 replicated07

tempfile exa
save `exa', replace

import delimited "`project_directory'/aprior_adj_13.csv", asdouble clear
rename variable_aprior_adj_13 variable
rename conf_int_string_aprior_adj_13 or_ci13
rename pvalue_aprior_adj_13 pval13
rename replicated_aprior_adj_13 replicated13


merge 1:1 variable using "`exa'"
drop _merge
keep variable or_ci13 pval13 replicated13 var_label value_label or_ci07 pval07 replicated07
order variable  var_label value_label or_ci07 pval07 replicated07 or_ci13 pval13 replicated13
gen dataset=2
save `exa', replace

import delimited "`project_directory'/super_07.csv", asdouble clear
rename variable_super_07 variable
rename conf_int_string_super_07 or_ci07
rename pvalue_super_07 pval07
rename replicated_super_07 replicated07

tempfile exp
save `exp', replace
 
import delimited "`project_directory'/super_13.csv", asdouble clear
rename variable_super_13 variable
rename conf_int_string_super_13 or_ci13
rename pvalue_super_13 pval13
rename replicated_super_13 replicated13

merge 1:1 variable using "`exp'"
drop _merge
keep variable or_ci13 pval13 replicated13 var_label value_label or_ci07 pval07 replicated07
order variable  var_label value_label or_ci07 pval07 replicated07 or_ci13 pval13 replicated13
gen dataset=3
save `exp', replace

append using `exa'
append using `uni'

gen parse07=strpos(or_ci07,"[")
gen or07=substr(or_ci07,1,parse07-1)
destring or07, replace
gen logpval07=-log10(pval07)

gen parse13=strpos(or_ci13,"[")
gen or13=substr(or_ci13,1,parse13-1)
destring or13, replace
gen logpval13=-log10(pval13)

egen inboth_a=rowtotal(replicated07 replicated13)
bys variable: egen inboth_b=max(inboth_a)

keep if inboth_b==2

replace or13=. if or13>100
replace or07=. if or07>100

preserve
collapse (mean) logpval07, by(variable)
gsort -logpval07

gen rank=_n
tempfile rank
save `rank', replace
restore
merge m:1 variable using "`rank'"
drop _merge

gen or07t=string(or07, "%9.1f")
gen or13t=string(or13, "%9.1f")
gen  logpval07t=string(logpval07, "%9.1f")
gen  logpval13t=string(logpval13, "%9.1f")
replace logpval07t=logpval07t+"*" if replicated07==1
replace logpval13t=logpval13t+"*" if replicated13==1

replace or07t="-" if or07t=="."
replace or13t="-" if or13t=="."
replace logpval07t="-" if logpval07t=="."
replace logpval13t="-" if logpval13t=="."

drop replicated07 replicated13 parse07 parse13 inboth_a
reshape wide or07t or13t logpval07t logpval13t logpval07 logpval13 or07 or13 or_ci07 or_ci13 pval07 pval13, i(variable) j(dataset)

gen uni_or=or07t1+"; "+or13t1
gen uni_p=logpval07t1+"; "+logpval13t1
gen exa_or=or07t2+"; "+or13t2
gen exa_p=logpval07t2+"; "+logpval13t2
gen exp_or=or07t3+"; "+or13t3
gen exp_p=logpval07t3+"; "+logpval13t3

replace exa_or="-; -" if exa_or=="; "
replace exa_p="-; -" if exa_p=="; "
replace exp_or="-; -" if exp_or=="; "
replace exp_p="-; -" if exp_p=="; "
sort rank

local change10 "v201 v202 v203 v204 v206 v207 v218 v219 v238" // variables where suffix 1 is count 0
local change11 "v003 v136 v137" // variables where suffix 1 is count 1

gen maybechange=strpos(variable,"_")
gen suffix=substr(variable,maybechange+1,.) if maybechange>0
gen stem=substr(variable,1,maybechange-1) if maybechange>0
destring suffix, replace

foreach i of local change10 {
replace value_label=string(suffix-1) if stem=="`i'"
}

foreach i of local change11 {
replace value_label=string(suffix) if stem=="`i'"
}

*keep variable var_label value_label uni_or uni_p exa_or exa_p exp_or exp_p
