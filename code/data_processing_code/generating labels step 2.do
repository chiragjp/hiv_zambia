
********************************************************************************
  
clear all
set more off
set matsize 11000
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
*local project_directory="~/Desktop/desktop junk"
local files_directory = "~/Dropbox/HIV xwas work/data/"

file open myfile using "`project_directory'/variables labels.txt", write replace
file write myfile  "variable" _tab "var_label" _tab "level" _tab "value_label" _n
local year="13"
**********************************************************

use "`files_directory'/label_generation_step1_file_`year'.dta", clear
foreach var of varlist start-end {
capture local labelr : var label `var'
if _rc==0 {
	capture local labelr=subinstr("`labelr'", char(34),"",.)
}
quietly egen `var'_n=group(`var')
quietly sum `var'_n
if r(max)>30 {
file write myfile "`var'" _tab "`labelr'" _tab "continuous" _n

if "`var'"!="start" {
drop `var' 
}
}
else if r(max)==3 {
quietly sum `var'
if ((r(min)==0 & r(max)>2) | (r(min)==1 & r(max)>3)) {
quietly replace `var'=. if `var'==r(max)
}
}
drop *_n

}
gen start2=.
order start2
drop start2-start

foreach var of varlist _all {
capture local labelr : var label `var'
if _rc==0 {
	capture local labelr=subinstr("`labelr'", char(34),"",.)
}
quietly tab `var'
if r(r)==2 {
local lbe : value label `var'
quietly levelsof `var', local(`var'_levels)
foreach val of local `var'_levels {
capture local `var'vl`val' : label `lbe' `val'
quietly sum `var'
if r(min)==`val' {
file write myfile "`var'" _tab "`labelr'" _tab "0" _tab "``var'vl`val''" _n
}
else {
file write myfile "`var'" _tab "`labelr'" _tab "1" _tab "``var'vl`val''" _n
}
}
}
quietly tab `var'
if r(r)>2 {
local lbe : value label `var'
quietly levelsof `var', local(`var'_levels)
foreach val of local `var'_levels {
capture local `var'vl`val' : label `lbe' `val'
file write myfile "`var'" _tab "`labelr'" _tab "`val'" _tab "``var'vl`val''" _n
}
}
}

file close myfile
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
insheet using "`project_directory'/variables labels.txt", clear names
rename variable var_stem
gen variable=var_stem
bys variable: gen levels=_N
destring level, gen(l2) force
sort var_stem l2
bys var_stem: gen new_level=_n
replace level=string(new_level) if levels>2
replace variable=var_stem+"_"+level if levels>2
drop if levels==2 & level=="0"
order var_stem variable var_label value_label level

save "`project_directory'/variable labels", replace
