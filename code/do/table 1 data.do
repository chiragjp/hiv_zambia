set more on

file open sumstats using  "~/Dropbox/HIV xwas work/data/sumstats.txt", write replace
file write sumstats "variable" _tab "var_label" _tab "Emptiness" _n
 
use "/Users/ebd/Dropbox/HIV xwas work/data/hiv xwas toy dataset v6c.dta", clear
gen start=1
gen end=1
order start

gen empty=.
quietly {
foreach x of varlist start-end {

file write sumstats "`x'" _tab 

local labelr : var label `x'
file write sumstats "`labelr'" _tab
			

capture replace empty=`x'==.
if _rc!=0 {
replace empty=`x'==""
}
sum empty
file write sumstats %7.3f (r(mean)) _n
}
}

file close sumstats

insheet using "~/Dropbox/HIV xwas work/data/sumstats.txt", clear
tempfile all_vars
save `all_vars', replace

use "~/Dropbox/HIV xwas work/data/sig_vars_unadjusted_031116.dta", clear
rename variable_model variable
gen len=length(variable)
gen change=strpos(variable,"I(")>0
replace variable=substr(variable,1,len-2) if change==1
replace variable=substr(variable,9,.) if change==1
keep variable or_overall logp_overall
merge 1:1 variable using "`all_vars'"
order variable var_label
gsort -logp -or var_l
