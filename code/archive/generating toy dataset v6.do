
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
quietly {
#delimit ;

keep 
hiv03
v000
v005
v007
ml101
mmc1
s1007a
s110d
s110g
s110i
s110k
s110l
s110n
s110p
s110r
s110u
s110v
s110w
s118a
s461a
s715a
s715b
slocal
v009
v010
v011
v012
v016
v021
v024
v025
v115
v116
v119
v120
v121
v122
v125
v129
v133
v136
v137
v138
v139
v140
v150
v151
v152
v158
v159
v161
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
v218
v219
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
v394
v404
v405
v406
v459
v461
v463a
v463z
v467b
v467c
v467d
v467e
v467f
v467h
v467i
v474
v477
v481
v501
v502
v513
v525
v613
v623
v624
v628
v633a
v731
v744a
v744b
v744c
v744d
v744e
v750
v751
v766a
s110h
s110j
s110m
s110o
s110q
s110s
s110t
s118a
s118d
s118f
s118g
s461a
s461b
v113
v115
v116
v119
v122
v123
v124
v125
v127
v128
v133
v149
v158
v159
v167
v393
v394
v463b
v463c
v463d
v467b
v467c
v467d
v467e
v467f
v467h
v467i
v481
v536
v627
v628
v633d
v744a
v744c
v744d
v745a
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
v766b
v822
s123
s936ba
s936bb
s936bc
s936bd
v104
v130
v384c
v437
v438
v482b
v525
v745b
v763c
v131
v157
v217
v481a
v481b
v481c
v481d
v481e
v481f
v605
v624
v716
v717
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
v763b
v781
v155
v633b
v714
v785
v153
v226
v416
v763a
v754jp
v774a
v774b
v774c
v777
v778
v825
v837
v779
v780
v823
v849
v754dp
v754wp
v783
v848
v440
v444a
v445
v446
v754cp
v756
v754bp
v775
mmc2
mm1_01
mm2_01
v475
v476
v474a
v474b
v474c
v474d
v474e
v474f
s1000e
s910b
v824
mm1_02
mm2_02
v769;

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

* recast variables with low-frequency values;
*  needs more thinking;
foreach x of varlist v113
v116
v127
v128
v129
v137
v138
v150
v161
v202
v203
v204
v205
v206
v207
v208
v209
v218
v219
v301
v501
v627
v628
v766a
v766b
v167
v477 {;
gen `x'n=`x';
local lab: variable label `x';
label var `x'n "`lab'";
preserve;
collapse (count) v005, by(`x');
drop if `x'==.;
egen tot=total(v005);
gen freq=v005/tot;
keep if freq<=0.03;
levelsof `x', local(groupvars);
restore;
foreach y in `groupvars' {;
replace `x'n=. if `x'n==`y';
};
drop `x';
rename `x'n `x';
};


# delimit cr

* Taking care of variables that have duplicates in the dataset with nearly 
* identical meaning, e.g. number of sexual partners including or excluding 
* husband.  Less informative variables dropped.

preserve
use "/Users/ebd/Desktop/desktop junk/vartypes.dta", clear
gen dups=substr(notes,1,1)=="D"
keep if dups==1
levelsof varname, local(dupls)
restore
foreach x of local dupls {
drop `x'
}

* Taking care of binary variables; replacing other than 0/1 with missing
preserve
use "/Users/ebd/Desktop/desktop junk/vartypes.dta", clear
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

* Takig care of categorical variables
preserve
use "/Users/ebd/Desktop/desktop junk/vartypes.dta", clear
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

replace v005=v005/1000000

foreach var of varlist _all {
	capture _strip_labels `var'
}
order hiv03 v000 v007 v021

}
save "`project_directory'/hiv xwas toy dataset v6.dta", replace
export delimited using "`project_directory'/hiv xwas toy dataset v6.csv", replace

