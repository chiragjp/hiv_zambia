
********************************************************************************
  
clear all
set more off
set matsize 11000
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
*local project_directory="~/Desktop/desktop junk"
local files_directory = "~/Dropbox/HIV xwas work/data/"

local completeness_threshold=0.9
**********************************************************

set obs 1
gen z=.
tempfile all1
save "`all1'", replace
*********************

quietly {
cd "`files_directory'"

local directories: dir . dirs "*"
foreach directory of local directories {
	cd "`files_directory'/`directory'"
	local files: dir . files "*"	
	foreach fileyn of local files {
		if ((strpos("`fileyn'","ar") | strpos("`fileyn'","AR")) & (strpos("`fileyn'",".dta") | strpos("`fileyn'",".DTA"))) {
		foreach irfileyn of local files {
		if ((strpos("`irfileyn'","ir") | strpos("`irfileyn'","IR")) & (strpos("`irfileyn'",".dta") | strpos("`irfileyn'",".DTA")) & !strpos("`irfileyn'","pro")){
			*use v* using "`irfileyn'", clear
			use "`irfileyn'", clear
			di "`fileyn'"
			
			gen cname=substr(v000,1,2)
			gen id1=cname+"a"+string(v001)+"a"+string(v002)+"a"+string(v003)
			
			*** Fix idiosyncratic quirks
			if "`fileyn'"=="CIar50fl.dta" {
				* for CI id should be cluster+structure+hh+line
				replace id1=cname+"a"+string(v001)+"a"+string(shstruct)+"a"+string(v002)+"a"+string(vidx)
			}
			if "`fileyn'"=="SNar4Afl.dta" {
				* for SN id should be cluster+structure/concession+hh+line
				replace id1=cname+"a"+string(v001)+"a"+string(shconces)+"a"+string(v002)+"a"+string(vidx)
			}
			replace v007=v007+2000 if v007<10
			replace v007=v007+1900 if v007<100
			replace v007=v007+8 if cname=="ET"
			replace v008=v008-680 if cname=="NP"
			replace v007=int((v008-1)/12)+1900 if cname=="NP"
			**********************
			
			sum v007
			gen svyid=v000+"_"+string(r(min))
			
			tempfile temp1
			save "`temp1'", replace
			}
		}
		use "`fileyn'", clear
		
		gen cname=upper(substr("`fileyn'",1,2))
		gen id1=cname+"a"+string(hivclust)+"a"+string(hivnumb)+"a"+string(hivline)
		if "`fileyn'"=="CIar50fl.dta" {
			* for CI id should be cluster+structure+hh+line
			replace id1=cname+"a"+string(hivclust)+"a"+string(hivstruct)+"a"+string(hivnumb)+"a"+string(hivline)
		}
		if "`fileyn'"=="SNar4Afl.dta" {
			* for SN id should be cluster+structure/concession+hh+line
			replace id1=cname+"a"+string(hivclust)+"a"+string(hivstruct)+"a"+string(hivnumb)+"a"+string(hivline)
		}
		keep id1 hiv03
		merge 1:1 id1 using "`temp1'"
		
		keep if _merge==3
		append using "`all1'", force
		save "`all1'", replace
		}
}

}

sum v007
gen year2=substr(string(r(min)),3,.)
local year=year2 in 1

drop z
drop if cname==""
drop _merge

gen start=runiform()
gen end=runiform()
** protected variables
order hiv03 cname id1 v000 v005 v007 v021 start
order end, last

** omit string variables from processing
foreach var of varlist start-end {
capture confirm string variable `var'
if !_rc {
drop `var'
}
}

foreach var of varlist start-end {

* dropping 1-level variables
capture tab `var'
if _rc==0 {
if r(r)==1 {
	drop `var'
	}
else {
* dropping all empty variables and variables that are above the completeness threshold
gen x1=`var'==.
sum x1
if r(mean)>(1-`completeness_threshold') {
drop `var'
}
drop x1
}
}
}

foreach var of varlist start-end {
** Change names of variables with "_"
if strpos("`var'","_")>0 {
local newname=subinstr("`var'","_","",1)
rename `var' `newname'
local x="`newname'"
}
}

}

save "`files_directory'/label_generation_step1_file_`year'.dta", replace
