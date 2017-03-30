********************************************************************************

********************************************************************************
* File cyclying through all DHS surveys and checking for completeness
*
********************************************************************************
/*
******** Among all DHS files; only those with HIV test below *********
clear all
set more off
pause on

********************* Local macros

local project_directory="~/Dropbox/HIV xwas work/data"
local filesDirectory = "~/Dropbox/Mapping DHS/DHS files"
*local filesDirectory = "/Users/ebd/Desktop/desktop junk/trial"
file open myfile using "`project_directory'/completeness hr files.txt", write replace
file write myfile  "variable" _tab "filename" _tab "var label" _tab "observations" _tab "completeness" _n

*********************

cd "`filesDirectory'"

local directories: dir . dirs "*"
foreach directory of local directories {
	cd "`filesDirectory'/`directory'"
	local hrfiles: dir . files "*"	
	foreach hrfileyn of local hrfiles {
		if ((strpos("`hrfileyn'","hr") | strpos("`hrfileyn'","HR")) & (strpos("`hrfileyn'",".dta") | strpos("`hrfileyn'",".DTA")) & !strpos("`hrfileyn'", "_") ///
		& "`directory'"!="09PeruContinuous"){
			use "`hrfileyn'", clear
			
			gen start=1
			gen end=1
			order start
			order end, last
			quietly gen xx=.
			foreach x of varlist start-end {
			local x=subinstr("`x'", char(34),"",.)
			file write myfile "`x'" _tab "`hrfileyn'"
			capture local labelr : var label `x'
			if _rc==0 {
			capture local labelr=subinstr("`labelr'", char(34),"",.)
			capture file write myfile _tab "`labelr'"
			capture replace xx=(`x'!=.)
			if _rc==0 {
			quietly sum xx
			}
			else {
			replace xx=(`x'!="")
			quietly sum xx
			}
			file write myfile _tab %7.3f (r(N)) _tab %7.3f (r(mean))
			}
			file write myfile _n
			
		}
		}
	}
}

file close myfile
local project_directory="~/Dropbox/HIV xwas work/data"
insheet using "`project_directory'/completeness hr files.txt", clear
duplicates tag variable, gen (n_svys)
quietly replace n_svys=n_svys+1
drop if variable=="start" | variable=="end"
gsort -n_svys -comple variable filename
replace  varlabel="." if varlabel==""
gen filetype=substr(filename,3,2)
egen var_label_grp=group( variable varlabel)
collapse (mean) completeness n_svys (count) n_in_group=var_label_grp, by(variable varlabel filetype)
bys variable: egen max_n=max(n_svys)
gsort -comple varlabel
save "`project_directory'/completeness hr files", replace
 
 
*/
****************** Below only for those files with an HIV test

********************************************************************************

********************************************************************************
* File cyclying through all DHS surveys and checking for completeness
*
********************************************************************************

clear all
set more off
pause on

********************* Local macros

local project_directory="~/Dropbox/HIV xwas work/data"
local filesDirectory = "~/Dropbox/Mapping DHS/DHS files"
*local filesDirectory = "/Users/ebd/Desktop/desktop junk/trial"
file open myfile using "`project_directory'/completeness hr_hiv files.txt", write replace
file write myfile  "variable" _tab "filename" _tab "var label" _tab "observations" _tab "completeness" _n

*********************

cd "`filesDirectory'"

local directories: dir . dirs "*"
quietly {
foreach directory of local directories {
	quietly cd "`filesDirectory'/`directory'"
	local files: dir . files "*"	
	foreach fileyn of local files {
	if ((strpos("`fileyn'","ar") | strpos("`fileyn'","AR")) & (strpos("`fileyn'",".dta") | strpos("`fileyn'",".DTA"))) {
	local hrfiles: dir . files "*"	
	foreach hrfileyn of local hrfiles {
		if ((strpos("`hrfileyn'","hr") | strpos("`hrfileyn'","HR")) & (strpos("`hrfileyn'",".dta") | strpos("`hrfileyn'",".DTA")) & !strpos("`hrfileyn'", "_") ///
		& "`directory'"!="09PeruContinuous"){
			use "`hrfileyn'", clear
			noisily di "`hrfileyn'"
			gen start=1
			gen end=1
			order start
			order end, last
			gen xx=.
			foreach x of varlist start-end {
			local x=subinstr("`x'", char(34),"",.)
			file write myfile "`x'" _tab "`hrfileyn'"
			capture local labelr : var label `x'
			if _rc==0 {
			capture local labelr=subinstr("`labelr'", char(34),"",.)
			capture file write myfile _tab "`labelr'"
			capture replace xx=(`x'!=.)
			if _rc==0 {
			quietly sum xx
			}
			else {
			replace xx=(`x'!="")
			quietly sum xx
			}
			file write myfile _tab %7.3g (r(N)) _tab %7.3f (r(mean))
			}
			file write myfile _n
			
		}
		}
	}
}
}
}
}
file close myfile
local project_directory="~/Dropbox/HIV xwas work/data"
insheet using "`project_directory'/completeness hr_hiv files.txt", clear
gen cname=substr(filename,1,2)
drop if cname=="VN" | cname=="vn"
drop if cname=="HT" | cname=="ht"
drop if cname=="DR" | cname=="dr"
replace cname=upper(cname)

duplicates tag variable, gen (n_svys)
quietly replace n_svys=n_svys+1
drop if variable=="start" | variable=="end"
gsort -n_svys -comple variable filename
replace  varlabel="." if varlabel==""
gen filetype=substr(filename,3,2)
egen var_label_grp=group( variable varlabel)
collapse (mean) completeness n_svys (count) n_in_group=var_label_grp, by(variable varlabel filetype)
bys variable: egen max_n=max(n_svys)
gsort -comple varlabel
save "`project_directory'/completeness hr_hiv files", replace

local project_directory="~/Dropbox/HIV xwas work/data"
insheet using "`project_directory'/completeness hr_hiv files.txt", clear 
gen cname=substr(filename,1,2)
replace cname=upper(cname)
drop if cname=="VN" | cname=="vn"
drop if cname=="HT" | cname=="ht"
drop if cname=="DR" | cname=="dr"
drop if variable=="start" | variable=="end"
gen obs_w_res= observations*completeness
bys  variable: egen tot_obs=total(observations)
bys variable: egen tot_res=total(obs_w_res)
gen x=1
bys variable: egen n_svys=total(x)
drop x
duplicates drop variable, force
gen res_rate= tot_res/tot_obs
gsort -n_ -res_r variable
