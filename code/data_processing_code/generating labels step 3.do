
********************************************************************************
  
clear all
set more off
set matsize 11000
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
*local project_directory="~/Desktop/desktop junk"
local files_directory = "~/Dropbox/HIV xwas work/data/"
local year="13"
**********************************************************

use "`project_directory'/variable labels", replace
tempfile varlabels
save "`varlabels'", replace
cd "`project_directory'"

local files: dir . files "*"
	foreach csvfile of local files {
		if ((strpos("`csvfile'",".csv") | strpos("`csvfile'",".CSV")) & strpos("`csvfile'","`year'")) {
import delimited "`csvfile'", clear asdouble
di "`csvfile'"
foreach var of varlist _all {
if (strpos("`var'","variable")) {
rename `var' variable
}
}
quietly merge 1:1 variable using "`varlabels'"
quietly drop if _merge<3
quietly drop _merge
local length=length("`csvfile'")

local handle=substr("`csvfile'",1,`length'-4)
di "`handle'"
order var_label value_label

save "`project_directory'/`handle'_labeled", replace
}
}
