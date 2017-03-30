
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
*local project_directory="/Users/ebd/Documents/GMD/R21 Behavioral Explorations in DHS/data"
local project_directory="/Users/ebd/Dropbox/HIV xwas work/data/"
*local filesDirectory = "/Users/ebd/Dropbox/Mapping DHS/DHS files"
local filesDirectory = "/Users/ebd/Dropbox/HIV xwas work/data/"


set obs 1
gen z=.
tempfile all1
quietly save "`all1'", replace

*********************

cd "`filesDirectory'"

local directories: dir . dirs "*"
foreach directory of local directories {
	quietly cd "`filesDirectory'/`directory'"
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
stop
quietly {
#delimit ;

keep 
hiv03
v000
v005
v007
ml101
mm1_01
mm1_02
mm2_01
mm2_02
mmc1
mmc2
s1000e
s1007a
s110d
s110g
s110h
s110i
s110j
s110k
s110l
s110m
s110n
s110o
s110p
s110q
s110r
s110s
s110t
s110u
s110v
s110w
s118a
s118a
s118d
s118f
s118g
s123
s461a
s461a
s461b
s715a
s715b
s910b
s936ba
s936bb
s936bc
s936bd
slocal
v009
v010
v011
v012
v016
v021
v024
v025
v104
v113
v115
v115
v116
v116
v119
v119
v120
v121
v122
v122
v123
v124
v125
v125
v127
v128
v129
v130
v131
v133
v133
v136
v137
v138
v139
v140
v149
v150
v151
v152
v153
v155
v157
v158
v158
v159
v159
v161
v167
v190
v201
v202
v203
v204
v205
v206
v207
v208
v209
v210
v213
v216
v217
v218
v219
v226
v228
v238
v301
v302a
v304_*
v307*
v312
v313
v364
v384a
v384b
v384c
v393
v394
v394
v404
v405
v406
v416
v437
v438
v440
v444a
v445
v446
v459
v461
v463a
v463b
v463c
v463d
v463z
v467b
v467b
v467c
v467c
v467d
v467d
v467e
v467e
v467f
v467f
v467h
v467h
v467i
v467i
v474
v474a
v474b
v474c
v474d
v474e
v474f
v475
v476
v477
v481
v481
v481a
v481b
v481c
v481d
v481e
v481f
v482b
v501
v502
v513
v525
v525
v536
v605
v613
v623
v624
v624
v627
v628
v628
v633a
v633b
v633d
v714
v716
v717
v731
v744a
v744a
v744b
v744c
v744c
v744d
v744d
v744e
v745a
v745b
v750
v751
v754bp
v754cp
v754dp
v754jp
v754wp
v756
v762aa
v762ab
v762ac
v762ad
v762ae
v762af
v762aj
v762ak
v762al
v762am
v762an
v762ao
v762ap
v762as
v762at
v762au
v762az
v762ba
v762bb
v762bc
v762bd
v762be
v762bf
v762bj
v762bk
v762bl
v762bm
v762bn
v762bo
v762bp
v762bs
v762bt
v762bu
v763a
v763b
v763c
v766a
v766b
v769
v774a
v774b
v774c
v775
v777
v778
v779
v780
v781
v783
v785
v822
v823
v824
v825
v837
v848
v849;

drop v304_04 v304_12 v304_15 v304_17 v304_19 v304_20 v307_04 v307_15
v307_18 v307_14 v307_13 v307_12 v307_11 v307_10 v307_09 v307_08 v307_07 v307_06 v307_02
v307_16 v307_19 v307_20;

foreach x of varlist v104
v116
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

# delimit cr

* Taking care of variables that have duplicates in the dataset with nearly 
* identical meaning, e.g. number of sexual partners including or excluding 
* husband.  Less informative variables dropped.

preserve
use "`project_directory'/vartypes.dta", clear
gen dups=substr(notes,1,1)=="D"
keep if dups==1
levelsof varname, local(dupls)
restore
foreach x of local dupls {
drop `x'
}

* Taking care of binary variables; replacing other than 0/1 with missing
preserve
use "`project_directory'/vartypes.dta", clear
keep if vartype=="B"
qui levelsof varname, local(bins)
restore
foreach x of local bins {

** Change names of variables with "_"
if strpos("`x'","_")>0 {
local newname=subinstr("`x'","_","",1)
rename `x' `newname' 
local x="`newname'"
}
**

quietly replace `x'=. if `x'>1
}

* Taking care of categorical variables
preserve
use "`project_directory'/vartypes.dta", clear
keep if vartype=="C" & substr(notes,1,1)!="D"
qui levelsof varname, local(cats)
restore
foreach x of local cats {

** Change names of variables with "_"
if strpos("`x'","_")>0 {
local newname=subinstr("`x'","_","",1)
rename `x' `newname' 
local x="`newname'"
}
**

quietly tab `x'
if r(r)==2 {
egen new`x'=max(`x')
replace `x'=(`x'==new`x')
drop new`x'
}
else {
quietly tabulate `x', gen("`x'_")
local lab: variable label `x'
foreach v of varlist `x'_* {
label var `v' "`lab'"
}
drop `x'
}

}

* taking care of low-freq variables
foreach x of varlist _all {
quietly tab `x'
if r(r)==2 {
quietly sum `x'
if r(max)==1 & (r(mean)<0.02 | r(mean)>0.98) {
if "`x'"!="v481" & "`x'"!="v750" & "`x'"!="v751" & "`x'"!="v763a" & "`x'"!="v763c" & "`x'"!="v131_8" {
drop `x'
}
}
}
}
replace v005=v005/1000000

foreach var of varlist _all {
	capture _strip_labels `var'
}
order hiv03 v000 v007 v021

}
save "`project_directory'/hiv xwas toy dataset v6.dta", replace
export delimited using "`project_directory'/hiv xwas toy dataset v6.csv", replace

