clear all
set more off
capture log close
log using "~/Dropbox/HIV xwas work/output/significant variables.log", replace 
import excel "~/Dropbox/HIV xwas work/output/significant variables v7.xlsx", sheet("Sheet1") firstrow clear
gen x=strpos(variable_model,"I(")>0
replace variable_model=substr(variable_model,9,.) if x>0
gen y=length(variable_model)
replace variable_model=substr(variable_model,1,y-2) if x>0
tempfile all_vars

quietly save "`all_vars'", replace
keep if OR_overall>1

local quantiles=4

xtile or_quart=OR_overall, n(`quantiles')
replace or_quart=`quantiles'-or_quart+1
forval c=1/`quantiles' {
preserve
quietly keep if or_quart==`c'
quietly levelsof variable_model, local(sigs)
use "/Users/ebd/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6.dta", clear
di "variables significant with OR>1 quantile `c'"
foreach x of local sigs {
di "`x'"
tab `x'
*describe `x'
}
restore
}

use "`all_vars'", clear
keep if OR_overall<1

local quantiles=3
xtile or_quart=OR_overall, n(`quantiles')
forval c=1/`quantiles' {
preserve
quietly keep if or_quart==`c'
quietly levelsof variable_model, local(sigs)
use "/Users/ebd/Dropbox/HIV xwas work/data/hiv xwas toy dataset v7.dta", clear
di "variables significant with OR<1 quantile `c'"
foreach x of local sigs {
di "`x'"
tab `x'
*describe `x'
}
restore
}

log close
