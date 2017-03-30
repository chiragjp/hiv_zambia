/*
1. Create/recreate dataset with the completion rate for every survey
2. Choose all variables with >90% non-missing rate
3. Indicate which variables have >10 levels
4. Those will need to be binned manually
5. For variables with <10 levels but some levels having <1% of 
6. Examine variables with shared stems/variable labels
7. Strip male variables and match with women variables based on var name and label

*/

********************************************************************************

********************************************************************************
* File for cleaning and preparing DHS data for analysis
* Zambia women first, but this in preparation for more extensive analysis
********************************************************************************
 
clear all
set more off
pause on

********************* Local macros
local data_directory="/Users/ebd/Dropbox/HIV xwas work/output"
local files_directory = "/Users/ebd/Dropbox/HIV xwas work/data/"
*local filesDirectory = "/Users/ebd/Dropbox/Mapping DHS/DHS files"
*local filesDirectory = "/Users/ebd/Desktop/desktop junk/trial"

*********************

insheet using "`data_directory'/completeness ir files.txt", clear
drop if variable=="start" | variable=="end"
keep if filename=="ZMIR61FL.DTA"
keep if completeness<0.9

quietly levelsof variable, local(dropvars)

clear all
set obs 1
gen z=.
tempfile all1
quietly save "`all1'", replace

*********************

cd "`files_directory'"

local directories: dir . dirs "*"
foreach directory of local directories {
	quietly cd "`files_directory'/`directory'"
	local files: dir . files "*"	
	foreach fileyn of local files {
		if ((strpos("`fileyn'","ar") | strpos("`fileyn'","AR")) & (strpos("`fileyn'",".dta") | strpos("`fileyn'",".DTA"))) {
		foreach irfileyn of local files {
		if ((strpos("`irfileyn'","ir") | strpos("`irfileyn'","IR")) & (strpos("`irfileyn'",".dta") | strpos("`irfileyn'",".DTA")) & !strpos("`irfileyn'","pro")){
			quietly use "`irfileyn'", clear
			di "`fileyn'"
			
			gen cname=substr(v000,1,2)
			gen id1=cname+"a"+string(v001)+"a"+string(v002)+"a"+string(v003)
			
			*** Fix idiosyncratic quirks
			if "`fileyn'"=="CIar50fl.dta" {
				* for CI id should be cluster+structure+hh+line
				quietly replace id1=cname+"a"+string(v001)+"a"+string(shstruct)+"a"+string(v002)+"a"+string(vidx)
			}
			if "`fileyn'"=="SNar4Afl.dta" {
				* for SN id should be cluster+structure/concession+hh+line
				quietly replace id1=cname+"a"+string(v001)+"a"+string(shconces)+"a"+string(v002)+"a"+string(vidx)
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

drop z
drop if cname==""
drop _merge

* drop variables with less than 90% completion
foreach x in `dropvars' {
drop `x'
}

* drop variables with only one value
gen start=1
gen end=1
order start
foreach x of varlist start-end {
quietly sum `x'
if r(sd)==0 {
drop `x'
}
}


* replace variables with 1-digit NA value
foreach x of varlist v119
v120
v121
v122
v123
v124
v125
v140
v153
v476
v633a
v633b
v633d
v744a
v744b
v744c
v744d
v744e
v754bp
v754cp
v754dp
v754jp
v754wp
v756
v763a
v763b
v763c
v769
v774a
v774b
v774c
v777
v778
v779
v780
v822
v823
v824
v825
v837
v848
v849
mm2_01
mm2_02
s110*
s118*
s123
s910b {;
replace `x'=. if `x'>5;
};

foreach x of varlist v116
v113
v127
v128
v129
v130
v139
v161
v525
v613
v627
v628
v717 {;
replace `x'=. if `x'>90;
* v104 removed from list bc >90 is meaningful;
};

foreach x of varlist v226 v115 {;
replace `x'=. if `x'>900;
};

foreach x of varlist v437
v438
v440
v444a
v445
v446 {;
replace `x'=. if `x'>9000;
};
};


#delimit cr
