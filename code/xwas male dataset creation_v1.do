
********************************************************************************

********************************************************************************
* First stage: merge HIV data with all possible variables from MR surveys
*
********************************************************************************

clear all
set more off
set matsize 11000
pause on

********************* Macros and setup *******************
local project_directory="~/Dropbox/HIV xwas work/data/"
*local project_directory="~/Desktop/desktop junk/"
*local files_directory = "~/Desktop/desktop junk/trial/"
local files_directory = "~/Dropbox/HIV xwas work/data/"
*local files_directory = "~/Dropbox/Mapping DHS/DHS files"

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
		foreach mrfileyn of local files {
		if ((strpos("`mrfileyn'","mr") | strpos("`mrfileyn'","MR")) & (strpos("`mrfileyn'",".dta") | strpos("`mrfileyn'",".DTA")) & !strpos("`mrfileyn'","pro")){
			*use v* using "`mrfileyn'", clear
			use "`mrfileyn'", clear
			noisily di "`fileyn'"
			
			gen cname=substr(mv000,1,2)
			gen id1=cname+"a"+string(mv001)+"a"+string(mv002)+"a"+string(mv003)
			
			*** Fix idiosyncratic quirks
			if "`fileyn'"=="CIar50fl.dta" {
				* for CI id should be cluster+structure+hh+line
				replace id1=cname+"a"+string(mv001)+"a"+string(shstruct)+"a"+string(mv002)+"a"+string(vidx)
			}
			if "`fileyn'"=="SNar4Afl.dta" {
				* for SN id should be cluster+structure/concession+hh+line
				replace id1=cname+"a"+string(mv001)+"a"+string(shconces)+"a"+string(mv002)+"a"+string(vidx)
			}
			replace mv007=mv007+2000 if mv007<10
			replace mv007=mv007+1900 if mv007<100
			replace mv007=mv007+8 if cname=="ET"
			replace mv008=mv008-680 if cname=="NP"
			replace mv007=int((mv008-1)/12)+1900 if cname=="NP"
			**********************
			
			sum mv007
			gen svyid=mv000+"_"+string(r(min))
			
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


drop z
drop if cname==""
drop _merge
}

********************************************************************************
* second stage: process and discretize variables
*
********************************************************************************

quietly {
gen start=runiform()
gen end=runiform()
** protected variables
order hiv03 cname id1 mv000 mv005 mv007 mv021 start
order end, last
_strip_labels _all

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
}

quietly {
foreach var of varlist start-end {

egen `var'_n=group(`var')
sum `var'_n
if r(max)<12000 {
tab `var'
if r(r)==2 {
sum `var', d
gen `var'_temp=0 if `var'==r(min)
replace `var'_temp=1 if `var'==r(max)
replace `var'=`var'_temp
drop `var'_temp
label value `var'
}
else if r(r)==3 {
sum `var'
if ((r(min)==0 & r(max)>2) | (r(min)==1 & r(max)>3)) {
replace `var'=. if `var'==r(max)
}
}
else if r(r)>3  {
local lbe : value label `var'
if "`lbe'"=="" {
continue
levelsof `var', local(`var'_levels)
foreach val of local `var'_levels {
capture local `var'vl`val' : label `lbe' `val'

if ((strpos("``var'vl`val''","not a dejure resident")>0) | ///
(strpos("``var'vl`val''","other")>0) | ///
(strpos("``var'vl`val''","don't know")>0)) ///
& !(strpos("``var'vl`val''","another")>0) ///
& !(strpos("``var'vl`val''","other relative")>0) {
replace `var'=. if `var'==`val'
}
}
}
}
}
}


drop *_n
foreach var of varlist start-end {
** Change names of variables with "_"
if strpos("`var'","_")>0 {
local newname=subinstr("`var'","_","",1)
rename `var' `newname'
local x="`newname'"
}
}

foreach var of varlist start-end {
egen `var'_n=group(`var')
sum `var'_n
if r(max)<12000 {

tab `var'
if r(r)>2 & r(r)<31 {
tabulate `var', gen("`var'_")
drop `var'
}
}
drop *_n
}

order end, last
foreach var of varlist start-end {
egen `var'_n=group(`var')
sum `var'_n
if r(max)==2 {
sum hiv03 if `var'==0
if r(sd)==0 | r(sd)==. {
drop `var'
}
else {
sum hiv03 if `var'==1
if r(sd)==0 | r(sd)==. {
*noisily di "var `var' is fully determined at level 1"
drop `var'
}
}
}
drop `var'_n
}

mata: mata clear
capture order hiv01
capture order caseid
capture order vcal*
capture order uniqueId
order end, last
quietly corr start-end
mat rho = r(C)
local rows_rho `= rowsof(rho)'
local rnames : rownames rho
local cnames : colnames rho
forval r=2/`rows_rho' {
local r2=`r'-1
forval c=1/`r2' {
if abs(rho[`r',`c'])>0.99 & (rho[`r',`c']!=.) {
local hi_corr `"`: word `r' of `rnames''"'
capture drop `hi_corr'
}
}
}
}

** postscripts
replace mv005=mv005/1000000
capture replace mv531=. if mv531>90 // inconsistent age since first sex
capture replace mv104=50 if mv104==95 //always is 50 years
capture replace mv104=. if mv104>95 //visitor

** Make mv010 the start variable for nest stage
order mv010, after(start)
drop start end

save "`project_directory'/hiv xwas Zambia07 male v7.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 male v7.csv", replace

/*
preserve
keep if v025==0 //urban
save "`project_directory'/hiv xwas Zambia07 dataset v7_urban.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_urban.csv", replace
restore

preserve
keep if v025==1 //rural
save "`project_directory'/hiv xwas Zambia07 dataset v7_rural.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_rural.csv", replace
restore

preserve
keep if v781==0 //never tested for HIV
save "`project_directory'/hiv xwas Zambia07 dataset v7_nevertested.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_nevertested.csv", replace
restore

preserve
keep if v781==1 //tested for HIV previously
save "`project_directory'/hiv xwas Zambia07 dataset v7_tested.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_tested.csv", replace
restore

preserve
keep if v190_1==1 //poorest (wealth1)
save "`project_directory'/hiv xwas Zambia07 dataset v7_wealth1.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_wealth1.csv", replace
restore

preserve
keep if v190_2==1 //less poor (wealth2)
save "`project_directory'/hiv xwas Zambia07 dataset v7_wealth2.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_wealth2.csv", replace
restore

preserve
keep if v190_3==1 //middle (wealth3)
save "`project_directory'/hiv xwas Zambia07 dataset v7_wealth3.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_wealth3.csv", replace
restore

preserve
keep if v190_4==1 //less wealthy (wealth4)
save "`project_directory'/hiv xwas Zambia07 dataset v7_wealth4.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_wealth4.csv", replace
restore

preserve
keep if v190_5==1 //wealthiest (wealth5)
save "`project_directory'/hiv xwas Zambia07 dataset v7_wealth5.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_wealth5.csv", replace
restore

preserve
keep if v190_1==1 | v190_2==1 //poor (wealth binary)
save "`project_directory'/hiv xwas Zambia07 dataset v7_poor.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_poor.csv", replace
restore

preserve
keep if v190_3==1 | v190_4==1 | v190_5==1 //rich (wealth binary)
save "`project_directory'/hiv xwas Zambia07 dataset v7_rich.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_rich.csv", replace
restore

gen age=v007-v010
gen age_cat=age<25
replace age_cat=2 if age>24
replace age_cat=3 if age>34

preserve
keep if age_cat==1 //youngest 15-24
save "`project_directory'/hiv xwas Zambia07 dataset v7_age1.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_age1.csv", replace
restore

preserve
keep if age_cat==2 //middle 25-34
save "`project_directory'/hiv xwas Zambia07 dataset v7_age2.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_age2.csv", replace
restore

preserve
keep if age_cat==3 //oldest 35-50
save "`project_directory'/hiv xwas Zambia07 dataset v7_age3.dta", replace
export delimited using "`project_directory'/hiv xwas Zambia07 dataset v7_age3.csv", replace


** There's a bug where a few variables with 2 levels do not end up getting converted to 0/1 binary
** and then get dropped because of collienarity with HIV status at the variable==0 level.
