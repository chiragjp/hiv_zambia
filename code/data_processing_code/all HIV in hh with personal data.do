
********************************************************************************

********************************************************************************
* File for merging HIV data with variables from other surveys.
*
********************************************************************************


clear all
set more off
pause on

********************* Macros and setup
*local project_directory="~/Dropbox/HIV xwas work/data/"
local filesDirectory = "~/Dropbox/Mapping DHS/DHS files"
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
		foreach prfileyn of local files {
		if ((strpos("`prfileyn'","pr") | strpos("`prfileyn'","PR")) & (strpos("`prfileyn'",".dta") | strpos("`prfileyn'",".DTA")) & !strpos("`prfileyn'","pro")){
			quietly use "`prfileyn'", clear
			noisily di "`fileyn'"
			gen x=.
			gen y=.
			order hv000 hv001 hv002 hv003 hvidx hv007 hv008 hv104 x
			capture order shstruct 
			capture order shconces 
			drop x-y
			gen cname=substr(hv000,1,2)
			gen id1=cname+"a"+string(hv001)+"a"+string(hv002)+"a"+string(hvidx)
			
			*** Fix idiosyncratic quirks
			if "`fileyn'"=="CIar50fl.dta" {
				* for CI id should be cluster+structure+hh+line
				quietly replace id1=cname+"a"+string(hv001)+"a"+string(shstruct)+"a"+string(hv002)+"a"+string(hvidx)
			}
			if "`fileyn'"=="SNar4Afl.dta" {
				* for SN id should be cluster+structure/concession+hh+line
				quietly replace id1=cname+"a"+string(hv001)+"a"+string(shconces)+"a"+string(hv002)+"a"+string(hvidx)
			}

			quietly replace hv007=hv007+2000 if hv007<10
			quietly replace hv007=hv007+1900 if hv007<100
			quietly replace hv007=hv007+8 if cname=="ET"
			quietly replace hv008=hv008-680 if cname=="NP"
			quietly replace hv007=int((hv008-1)/12)+1900 if cname=="NP"
			**********************
			
			quietly sum hv007
			quietly gen svyid=hv000+"_"+string(r(min))
			
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
drop if cname=="VN"  // not SSA
drop if cname=="HT"  // not SSA
drop if cname=="DR"  // not SSA
table cname hiv03 if hiv03<2, contents(count hv001 ) row column
