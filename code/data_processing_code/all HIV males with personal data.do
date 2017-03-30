
********************************************************************************

********************************************************************************
* File for merging HIV data with all possible variables from other surveys.
*
********************************************************************************

* What I need to do:
* Separate categorical from ordinal variables
* Change categorical variables to a series of dummies
* Leave ordinal variables as is for the most part
* See if any low-frequency levels remain

clear all
set more off
pause on

********************* Macros and setup
local project_directory="/Users/ebd/Dropbox/HIV xwas work/data/"
local filesDirectory = "/Users/ebd/Dropbox/Mapping DHS/DHS files"
*local filesDirectory = "/Users/ebd/Dropbox/HIV xwas work/data/"


set obs 1
gen z=.
tempfile all1
quietly save "`all1'", replace

*********************

cd "`filesDirectory'"
quietly {
local directories: dir . dirs "*"
foreach directory of local directories {
	quietly cd "`filesDirectory'/`directory'"
	local files: dir . files "*"	
	foreach fileyn of local files {
		if ((strpos("`fileyn'","ar") | strpos("`fileyn'","AR")) & (strpos("`fileyn'",".dta") | strpos("`fileyn'",".DTA"))) {
		foreach mrfileyn of local files {
		if ((strpos("`mrfileyn'","mr") | strpos("`mrfileyn'","MR")) & (strpos("`mrfileyn'",".dta") | strpos("`mrfileyn'",".DTA")) & !strpos("`mrfileyn'","pro")){
			quietly use "`mrfileyn'", clear
			noisily di "`fileyn'"
			gen x=.
			gen y=.
			order mv000 mv001 mv002 mv003 mv007 mv008 mv012 x
			capture order sstruct 
			capture order smstruct 
			drop x-y
			gen cname=substr(mv000,1,2)
			gen id1=cname+"a"+string(mv001)+"a"+string(mv002)+"a"+string(mv003)
			
			*** Fix idiosyncratic quirks
			if "`fileyn'"=="CIar50fl.dta" {
				* for CI id should be cluster+structure+hh+line
				quietly replace id1=cname+"a"+string(mv001)+"a"+string(sstruct)+"a"+string(mv002)+"a"+string(mv003)
			}
			if "`fileyn'"=="SNar4Afl.dta" {
				* for SN id should be cluster+structure/concession+hh+line
				quietly replace id1=cname+"a"+string(mv001)+"a"+string(smstruct)+"a"+string(mv002)+"a"+string(mv003)
			}
			quietly replace mv007=mv007+2000 if mv007<10
			quietly replace mv007=mv007+1900 if mv007<100
			quietly replace mv007=mv007+8 if cname=="ET"
			quietly replace mv008=mv008-680 if cname=="NP"
			quietly replace mv007=int((mv008-1)/12)+1900 if cname=="NP"
			**********************
			
			quietly sum mv007
			quietly gen svyid=mv000+"_"+string(r(min))
			
			tempfile temp1
			quietly save "`temp1'", replace
			}
		}
		quietly use "`fileyn'", clear
		
		gen cname=upper(substr("`fileyn'",1,2))
		gen id1=cname+"a"+string(hivclust)+"a"+string(hivnumb)+"a"+string(hivline)
		if "`fileyn'"=="CIar50fl.dta" {
			* for CI id should be cluster+structure+hh+line
			quietly replace id1=cname+"a"+string(hivclust)+"a"+string(hivstruct)+"a"+string(hivnumb)+"a"+string(hivline)
		}
		if "`fileyn'"=="SNar4Afl.dta" {
			* for SN id should be cluster+structure/concession+hh+line
			quietly replace id1=cname+"a"+string(hivclust)+"a"+string(hivstruct)+"a"+string(hivnumb)+"a"+string(hivline)
		}
		
		quietly merge 1:1 id1 using "`temp1'"
		
		quietly keep if _merge==3
		quietly append using "`all1'", force
		quietly save "`all1'", replace
		}
}
}
}
drop z
drop if cname==""
drop _merge
drop if cname=="VN"
drop if cname=="HT"
drop if cname=="DR"
table cname hiv03 if hiv03<2, contents(count mv001 ) row column
