
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
		foreach irfileyn of local files {
		if ((strpos("`irfileyn'","ir") | strpos("`irfileyn'","IR")) & (strpos("`irfileyn'",".dta") | strpos("`irfileyn'",".DTA")) & !strpos("`irfileyn'","pro")){
			quietly use "`irfileyn'", clear
			noisily di "`fileyn'"
			gen x=.
			gen y=.
			order v000 v001 v002 v003 v007 v008 v012 x
			capture order sstruct 
			capture order sconces 
			drop x-y
			gen cname=substr(v000,1,2)
			gen id1=cname+"a"+string(v001)+"a"+string(v002)+"a"+string(v003)
			
			*** Fix idiosyncratic quirks
			if "`fileyn'"=="CIar50fl.dta" {
				* for CI id should be cluster+structure+hh+line
				quietly replace id1=cname+"a"+string(v001)+"a"+string(sstruct)+"a"+string(v002)+"a"+string(v003)
			}
			if "`fileyn'"=="SNar4Afl.dta" {
				* for SN id should be cluster+structure/concession+hh+line
				quietly replace id1=cname+"a"+string(v001)+"a"+string(sconces)+"a"+string(v002)+"a"+string(v003)
			}
			quietly replace v007=v007+2000 if v007<10
			quietly replace v007=v007+1900 if v007<100
			quietly replace v007=v007+8 if cname=="ET"
			quietly replace v008=v008-680 if cname=="NP"
			quietly replace v007=int((v008-1)/12)+1900 if cname=="NP"
			**********************
			
			quietly sum v007
			quietly gen svyid=v000+"_"+string(r(min))
			
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
table cname hiv03 if hiv03<2, contents(count v001 ) row column
